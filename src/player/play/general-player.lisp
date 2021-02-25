;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
;============================================================================
;
;   This program is free software. For information on usage
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
;============================================================================
; File author: J. Bresson
;============================================================================

(in-package :om)

;;;=================================
;;; MAIN PLAYER
;;;=================================
;;; METHODS TO REDEFINE

(defgeneric make-player (id &key run-callback stop-callback callback-tick time-window))
(defmethod destroy-player (player) nil)
(defmethod player-set-time-interval (player from to))
(defmethod player-get-time (player) 0)
(defmethod player-get-state (player) t)
(defmethod player-get-object-state (player object) nil)
(defmethod player-idle-p (player) t)
(defmethod player-stop-object (player object) t)
(defmethod player-add-callback (player) t)
(defmethod player-remove-callback (player) t)
(defmethod player-reset (player) t)


(defmethod player-start (player &key start-t end-t) (declare (ignore start-t end-t)))
(defmethod player-stop (player))
(defmethod player-pause (player))
(defmethod player-continue (player))
(defmethod player-loop (player))
(defmethod player-start-record (player))
(defmethod player-stop-record (player))

;;; RETURN A LIST OF ACTIONS + TIME FOR AN OBJECT TO BE PLAYED
(defmethod get-action-list-for-play (object interval &optional parent) nil)

(defmethod player-play-object (engine object caller &key parent interval)
  (declare (ignore parent interval))
  (om-print (format nil "NO RENDERER FOR ~A" object)))

;(defmethod player-stop-object ((engine t) &optional objects) nil)


;;;=================================
;;; DEFAULT PLAYER
;;; (simple loop on a list of events)
;;; This player is actually not used anymore
;;;=================================
(defclass omplayer ()
  ((state :accessor state :initform :stop)    ; :play :pause :stop :record
   (loop-play :accessor loop-play :initform nil)
   (start-time :accessor start-time :initform 0)
   (stop-time :accessor stop-time :initform 0)
   (play-interval  :accessor play-interval :initform nil) ;;; check if this is necessary or if we can do everything with start-time and end-time....
   (player-offset :accessor player-offset :initform 0)
   (ref-clock-time :accessor ref-clock-time :initform 0)
   ;;; CALLBACKS
   (callback-tick :initform 0.1 :accessor callback-tick :initarg :callback-tick)
   (caller :initform nil :accessor caller :initarg :caller)
   (callback-fun :initform nil :accessor callback-fun :initarg :callback-fun)
   (callback-process :initform nil :accessor callback-process)
   (stop-fun :initform nil :accessor stop-fun :initarg :stop-fun)
   ;;; SCHEDULING TASKS
   (events :initform nil :accessor events :initarg :events)
   (scheduling-process :initform nil :accessor scheduling-process)
   (scheduler-tick :initform 0.01 :accessor scheduler-tick :initarg :scheduler-tick)
   ;;; OBJECTS
   (play-list :initform nil :accessor play-list :initarg :play-list)
   ))

(defmethod sort-events ((self omplayer))
  (setf (events self) (sort (events self) '< :key 'car)))

(defmethod schedule-task ((player omplayer) task at &optional (sort t))
  (push (cons at task) (events player))
  (when sort (sort-events player)))

(defmethod unschedule-all ((player omplayer))
  (setf (events player) nil))

(defun get-my-play-list (engine play-list)
  (mapcar 'cadr (remove-if-not #'(lambda (x) (equal x engine)) play-list :key 'car)))


(defmethod make-player ((id t) &key run-callback stop-callback (callback-tick 0.05) time-window)
  (declare (ignore time-window))
  (make-instance 'omplayer
                 :callback-fun run-callback
                 :callback-tick callback-tick
                 :stop-fun stop-callback))

(defun clock-time () (om-get-internal-time))

(defmethod player-get-time ((player omplayer))
  (cond ((equal (state player) :play)
         (+ (player-offset player) (start-time player) (- (clock-time) (ref-clock-time player))))
        ((equal (state player) :pause)
         (+ (player-offset player) (start-time player)))
        (t 0)))

(defmethod player-get-object-time ((player omplayer) object)
  (player-get-time player))

(defmethod player-get-state ((player omplayer)) (state player))

(defmethod player-set-time-interval ((player omplayer) from to)
  (setf (play-interval player) (list from to)))

(defmethod player-idle-p ((self omplayer))
  (not (member (state self) '(:play :record))))

(defmethod player-schedule-tasks ((player omplayer) object tasklist)
  (loop for task in tasklist do (schedule-task player (cadr task) (car task))))

;;; CALLED WHEN THE PLAYER HAS TO PLAY SEVERAL THINGS OR PREPARE THEM IN ADVANCE
(defmethod player-play-object ((player omplayer) obj caller &key parent interval)
  (setf (caller player) caller)
  (player-schedule-tasks player obj (get-action-list-for-play obj interval parent)))

(defmethod player-get-object-state ((player omplayer) object) (state player))
(defmethod player-stop-object ((player omplayer) object) (player-stop player))
(defmethod player-pause-object ((player omplayer) object) (player-pause player))
(defmethod player-continue-object ((player omplayer) object) (player-continue player))
(defmethod player-loop-object ((player omplayer) object loop?) (player-loop player))

;;; CALLED TO START PLAYER
(defmethod player-start ((player omplayer) &key (start-t 0) (end-t 3600000))

  (cond ((equal (state player) :play)
         (setf (stop-time player) (max (stop-time player) (or end-t 0))))

        (t
         (when end-t (setf (stop-time player) end-t))
         (when (callback-process player)
           (om-kill-process (callback-process player)))
         (when (scheduling-process player)
           (om-kill-process (scheduling-process player)))

         (setf (scheduling-process player)
               (om-run-process "player scheduling"
                               #'(lambda ()
                                   (loop
                                    (loop while (and (events player) (>= (player-get-time player) (car (car (events player))))) do
                                          (funcall (cdr (pop (events player)))))
                                    (when (and (stop-time player) (> (player-get-time player) (stop-time player)))
                                      (if (loop-play player) (player-loop player) (player-stop player)))
                                    (sleep (scheduler-tick player))
                                    ))
                               :priority 80000000))

         (when (callback-fun player)
           (setf (callback-process player)
                 (om-run-process "player caller callback"
                                 #'(lambda ()
                                     (loop
                                      (funcall (callback-fun player) (caller player) (player-get-time player))
                                      (sleep (callback-tick player))
                                      ))
                                 :priority 10)
                 ))

         (setf (state player) :play
               (start-time player) start-t
               (ref-clock-time player) (clock-time))

           ;(om-delayed-funcall stop-time #'player-stop player obj)
         )
        ))


;;; CALLED TO PAUSE PLAYER
(defmethod player-pause ((player omplayer))
  (when (equal (state player) :play)
    (setf (start-time player) (player-get-time player)
          (state player) :pause)
    (om-stop-process (scheduling-process player))
    (om-stop-process (callback-process player))
    ))

;;; CALLED TO CONTINUE PLAYER
(defmethod player-continue ((player omplayer))
  (om-resume-process (scheduling-process player))
  (om-resume-process (callback-process player))
  (setf (ref-clock-time player) (clock-time)
        (state player) :play))

;;; CALLED TO LOOP PLAYER
(defmethod player-loop ((player omplayer))
  ;(setf (stop-time player) (cadr (play-interval player)))
  (setf (start-time player) (or (car (play-interval player)) 0)
        (ref-clock-time player) (clock-time)))

;;; CALLED TO STOP PLAYER
(defmethod player-stop ((player omplayer))
  (unschedule-all player)
  (setf (play-list player) nil)
  (when (and (stop-fun player) (caller player))
    (funcall (stop-fun player) (caller player)))
  (when (callback-process player)
    (om-kill-process (callback-process player))
    (setf (callback-process player) nil))
  (setf (state player) :stop
        (ref-clock-time player) (clock-time)
        (start-time player) 0)
  (when (scheduling-process player)
    (om-kill-process (scheduling-process player))
    (setf (scheduling-process player) nil))
  )


;;; CALLED TO START RECORD WITH PLAYER
(defmethod player-start-record ((player omplayer))
  (if (equal (state player) :stop)
      (progn
        (setf (state player) :record))
    (om-beep)))

;;; CALLED TO STOP RECORD WITH PLAYER
(defmethod player-stop-record ((player omplayer))
  (when (callback-process player)
    (om-kill-process (callback-process player))
    (setf (callback-process player) nil))
  (setf (state player) :stop
        (ref-clock-time player) (clock-time)
        (start-time player) 0)
  (when (scheduling-process player)
    (om-kill-process (scheduling-process player))
    (setf (scheduling-process player) nil)))




#|

;;; SPECIFIES SOMETHING TO BE PLAYED ATHER A GIVEN DELAY (<at>) PAST THE CALL TO PLAYER-START
;;; THE DEFAULT BEHAVIOUR IS TO SCHEDULE 'player-play-object' AT DELAY
(defmethod prepare-to-play ((engine t) (player omplayer) object at interval params)
  (schedule-task player
                 #'(lambda ()
                     (player-play-object engine object :interval interval :params params))
                 at))

;;; PLAY (NOW)
;;; IF THE RENDERER RELIES ON THE PLAYER SCHEDULING, THIS IS THE ONLY METHOD TO IMPLEMENT
(defmethod player-play-object ((engine t) object &key interval params)
  (declare (ignore interval))
  ;(print (format nil "~A : play ~A - ~A" engine object interval))
  t)


;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine t) &optional play-list)
  ;(print (format nil "~A : start" engine))
  t)

;;; PAUSE (all)
(defmethod player-pause ((engine t) &optional play-list)
  ;(print (format nil "~A : pause" engine))
  t)

;;; CONTINUE (all)
(defmethod player-continue ((engine t) &optional play-list)
  ;(print (format nil "~A : continue" engine))
  t)

;;; STOP (all)
(defmethod player-stop ((engine t) &optional play-list)
  ;(print (format nil "~A : stop" engine))
  t)

;;; SET LOOP (called before play)
(defmethod player-set-loop ((engine t) &optional start end)
  ;(print (format nil "~A : set loop" engine))
  t)

;;; an engine must choose a strategy to reschedule it's contents on loops
(defmethod player-loop ((engine t) player &optional play-list)
  ;(print (format nil "~A : loop" engine))
  t)

(defmethod player-record-start ((engine t))
  ;(print (format nil "~A : record" engine))
  t)

;;; must return the recorded object
(defmethod player-record-stop ((engine t))
  ;(print (format nil "~A : record stop" engine))
  nil)

|#




