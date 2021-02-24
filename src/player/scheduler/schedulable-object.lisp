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
; File author: D. Bouche
;============================================================================


;;===========================================================================
;  Schedulable Object
;;===========================================================================

(declaim (optimize (speed 3) (safety 0) (debug 1)))

(in-package :om)

;;===========================================================================
;;;Schedulable-Object = Object to be played by a Dynamic-Scheduler
;;===========================================================================

;;;Structure
(defclass schedulable-object ()
  ((scheduler-settings :initform (list :autostop t) :accessor scheduler-settings :type (or null cons))
   (scheduler-info :initform (list :state :stop :loop-count 0) :accessor scheduler-info :type (or null cons))
   (scheduler-data :initform (list :plan-lock (mp:make-lock) :task-plan-lock (mp:make-lock)) :accessor scheduler-data :type (or null cons)))
  (:documentation "
SCHEDULABLE OBJECT: an object to be played through a SCHEDULER.
=======================================================================

Any subtype of schedulable-object is playable.
For a proper use, some methods have to be redefined:

1) The method (get-action-list-for-play ((self OBJECT-CLASS) time-interval)).
This method will ask the object what has to be played in [(car time-interval);(cadr time-interval)[.
The output has to be:
.((Date1 Function1 Data1) ... (DateN FunctionN DataN)) where DateN is the date in ms when FunctionN has to be called with DataN as a list of arguments (NIL if no arguments needed),
.NIL if nothing has to be played (no item to play found in time-interval).

2) The method (get-computation-list-for-play ((self OBJECT-CLASS) time-interval)).
This method will ask the object what has to be computed in [(car time-interval);(cadr time-interval)[.
The output has to be:
.((Date1 Deadline1 Function1 Data1) ... (DateN DeadlineN FunctionN DataN)) where DateN is the date in ms when FunctionN is allowed to be performed and Deadline1 is the date in ms when the result has to be produced,
.NIL if nothing has to be computed.

3) The method

3) Every modification on the data used by the previous methods has to be wrapped in the macro (with-schedulable-object object &rest body).
If the use of a macro is not convenient, you can simple call (notify-scheduler object) each time you think rescheduling might be useful."))

;;===========================================================================
;; API
;;===========================================================================

;; TO REDEFINE FOR YOUR SUBCLASS
;; It should return a list of lists containing a date, a function and its arguments
(defmethod get-action-list-for-play ((self schedulable-object) time-interval &optional parent)
  (append
   (if (not (or (eq (state self) :play) (play-planned? self)))
       (progn
         (setf (play-planned? self) t)
         (list
          (list
           (max 0 (car time-interval))
           #'(lambda ()
               (print (format nil "No redefinition of get-action-list-for-play found => starting ~A" self))
               (player-play-object *scheduler* self nil
                                   :interval (list (max 0 (car time-interval)) (cadr (interval self)))
                                   :parent parent)
               (setf (play-planned? self) nil))
           nil)))
     '())
   (if (>= (cadr time-interval) (get-obj-dur self))
       (list
        (list
         (get-obj-dur self)
         #'(lambda ()
             (player-stop-object *scheduler* self))
         nil)))))

;;; some objects can call this instead of returning a list of action (e.g. sound, etc.)
;;; this will call the specific play/stop actions for these objects
(defmethod external-player-actions ((self schedulable-object) time-interval &optional parent)
  (append
   (if (not (or (eq (state self) :play) (play-planned? self)))
       (progn
         (setf (play-planned? self) t)
         (list
          (list
           (max 0 (car time-interval))
           #'(lambda ()
               (player-play-object *scheduler* self nil
                                   :interval (list (max 0 (car (interval self)))
                                                   (cadr (interval self)))
                                   :parent parent)
               (setf (play-planned? self) nil))
           nil)))
     '())
   (if (>= (cadr time-interval) (get-obj-dur self))
       (list
        (list
         (get-obj-dur self)
         #'(lambda ()
             (player-stop-object *scheduler* self))
         nil)))))

;; TO REDEFINE FOR YOUR SUBCLASS
;; It should return a list of lists containing a date when a computation can start, its deadline, a function and its arguments
(defmethod get-computation-list-for-play ((self schedulable-object) &optional time-interval)
  nil)

;; TO REDEFINE FOR YOUR SUBCLASS
;; It should return an absolute duration
(defmethod get-obj-dur ((self schedulable-object)) *positive-infinity*)


;; TO USE TO EDIT DATA SLOTS OF YOUR OBJECT USED BY GET-ACTION-LIST-FOR-PLAY
;; (if not used, concurrent play and edit of the object is not ensured to be safe)
(defmacro with-schedulable-object (object &rest body)
  `(let (res)
     (setq res (progn ,@body))
     (when (eq (state ,object) :play)
       (setf (time-window ,object) (or (user-time-window ,object) *Lmin*))
       (reschedule ,object *scheduler* (get-obj-time ,object)))
     res))

;; ALTERNATIVE TO THE MACRO ABOVE
;; (notifies the scheduler of potentially needed replanning)
(defmethod notify-scheduler ((object schedulable-object))
  (when (and (eq (state object) :play) (> (time-window object) *Lmin*))
    (setf (time-window object) *Lmin*)
    (reschedule object *scheduler* (get-obj-time object))))

;; TO SET IF THE OBJECT HAS TO AUTOMATICALLY STOP WHEN GET-OBJ-DUR IS REACHED BY THE SCHEDULER
;; (t by default, set to nil is useful for objects that are filled while being played)
(defmethod set-object-autostop ((self schedulable-object) t-or-nil)
  (setf (autostop self) t-or-nil))

;; SET THE OBJECT TIME WHILE PLAYING
;; Sets the time and replans from this date
(defmethod set-object-current-time ((self schedulable-object) time)
  (setf (ref-time self) (- (om-get-internal-time) time))
  (if (eq (state self) :play)
      (reschedule self *scheduler* time nil)))

(defmethod set-object-current-time ((self t) time) nil)

;; CALLBACK USED WHEN THE SYSTEM SWITCHES THE OBJECT TIME AUTOMATICALLY
;; Happens when the object loops.
(defmethod set-time-callback ((self schedulable-object) time) nil)

;; SET THE OBJECT SCHEDULER TIME WINDOW
(defmethod set-object-time-window ((self schedulable-object) window)
  (setf (user-time-window self) window)) ;(max window *Lmin*)

;; LOOPS AN OBJECT
;; The object will loop at the end of its interval of at its end (if it exists)
(defmethod loop-object ((self schedulable-object))
  (setf (looper self) t)
  (notify-scheduler self))

;; UNLOOPS AN OBJECT
;; The object will not loop
(defmethod unloop-object ((self schedulable-object))
  (setf (looper self) nil)
  (notify-scheduler self))

(defmethod is-looping ((self schedulable-object))
  (looper self))

;; SET AN OBJECT'S INTERVAL
(defmethod set-object-interval ((self schedulable-object) interval)
  (setf (interval self) (if (or (null (cadr interval))
                                (= (car interval) (cadr interval)))
                            (list (car interval) *positive-infinity*)
                          interval)))

(defmethod set-object-interval ((self t) interval) interval)


;; GET AN OBJECT'S STATE
(defmethod get-object-state ((self schedulable-object)) (state self))
(defmethod get-object-state ((self t)) nil)

;; AUGMENTED TRANSPORT
;; Automatically play/continue or pause object according to its current state
(defmethod player-play/pause-object ((self scheduler) (object schedulable-object) caller &key parent (at 0) interval params)
  (declare (ignore at params parent))
  (if (eq (state object) :pause)
      (player-continue-object self object)
    (if (eq (state object) :play)
        (player-pause-object self object)
      (player-play-object self object caller :interval interval))))

(defmethod player-play/pause-object ((self t) (object t) caller &key parent (at 0) interval params)
  (declare (ignore self object caller parent at interval params))
  nil)

;; AUGMENTED TRANSPORT
;; Automatically play or stop object according to its current state
(defmethod player-play/stop-object ((self scheduler) (object schedulable-object) caller &key parent (at 0) interval params)
  (declare (ignore at params parent))
  (if (or (not (state object)) (eq (state object) :stop))
      (player-play-object self object caller :interval interval)
    (player-stop-object self object))
  ;;; return play-state
  (state object))

(defmethod player-play/stop-object ((self t) (object t) caller &key parent (at 0) interval params)
  (declare (ignore self object caller parent at interval params))
  nil)


(defmethod player-loop-object ((player scheduler) (object schedulable-object) loop?)
  (declare (ignore player))
  (if loop? (loop-object object)
    (unloop-object object)))


;;===========================================================================
;; GETTERS & SETTERS
;;===========================================================================

(defmethod looper ((self schedulable-object))
  (getf (scheduler-settings self) :looper))
(defmethod (setf looper) (t-or-nil (self schedulable-object))
  (setf (getf (scheduler-settings self) :looper) t-or-nil))

(defmethod interval ((self schedulable-object))
  (getf (scheduler-settings self) :interval))
(defmethod (setf interval) (new-interval (self schedulable-object))
  (setf (getf (scheduler-settings self) :interval) new-interval))

(defmethod autostop ((self schedulable-object))
  (getf (scheduler-settings self) :autostop))
(defmethod (setf autostop) (t-or-nil (self schedulable-object))
  (setf (getf (scheduler-settings self) :autostop) t-or-nil))

(defmethod user-time-window ((self schedulable-object))
  (getf (scheduler-settings self) :user-time-window))
(defmethod (setf user-time-window) (new-window (self schedulable-object))
  (setf (getf (scheduler-settings self) :user-time-window) new-window))

(defmethod state ((self schedulable-object))
  (getf (scheduler-info self) :state))
(defmethod (setf state) (new-state (self schedulable-object))
  (setf (getf (scheduler-info self) :state) new-state))

(defmethod play-planned? ((self schedulable-object))
  (getf (scheduler-info self) :play-planned?))
(defmethod (setf play-planned?) (t-or-nil (self schedulable-object))
  (setf (getf (scheduler-info self) :play-planned?) t-or-nil))

(defmethod loop-count ((self schedulable-object))
  (getf (scheduler-info self) :loop-count))
(defmethod (setf loop-count) (count (self schedulable-object))
  (setf (getf (scheduler-info self) :loop-count) count))

(defmethod auto-adapt-time-window ((self schedulable-object))
  (getf (scheduler-info self) :auto-adapt-time-window))
(defmethod (setf auto-adapt-time-window) (t-or-nil (self schedulable-object))
  (setf (getf (scheduler-info self) :auto-adapt-time-window) t-or-nil))

(defmethod update-object-time-window ((self schedulable-object))
  (if (and (auto-adapt-time-window self) (not (user-time-window self)))
      (setf (time-window self) (min *Lmax* (* 2 (time-window self))))))

;; Data
(defmethod destroy-data ((self schedulable-object))
  (setf (scheduler-data self) (list :plan-lock (plan-lock self)
                                    :task-plan-lock (task-plan-lock self)))
  (setf (loop-count self) 0))

(defmethod children ((self schedulable-object))
  (getf (scheduler-data self) :children))
(defmethod (setf children) (children (self schedulable-object))
  (setf (getf (scheduler-data self) :children) children))

(defmethod time-window ((self schedulable-object))
  (or (getf (scheduler-data self) :time-window) (user-time-window self) *Ldefault*))
(defmethod (setf time-window) (new-window (self schedulable-object))
  (setf (getf (scheduler-data self) :time-window) new-window))

(defmethod ref-time ((self schedulable-object))
  (or (getf (scheduler-data self) :ref-time) 0))
(defmethod (setf ref-time) (new-time (self schedulable-object))
  (setf (getf (scheduler-data self) :ref-time) new-time))

(defmethod pause-time ((self schedulable-object))
  (or (getf (scheduler-data self) :pause-time) 0))
(defmethod (setf pause-time) (new-time (self schedulable-object))
  (setf (getf (scheduler-data self) :pause-time) new-time))

(defmethod current-local-time ((self schedulable-object))
  (or (getf (scheduler-data self) :current-local-time) 0))
(defmethod (setf current-local-time) (new-time (self schedulable-object))
  (setf (getf (scheduler-data self) :current-local-time) new-time))

(defmethod plan ((self schedulable-object))
  (or (getf (scheduler-data self) :plan) (list)))
(defmethod (setf plan) (new-plan (self schedulable-object))
  (setf (getf (scheduler-data self) :plan) new-plan))

(defmethod plan-lock ((self schedulable-object))
  (getf (scheduler-data self) :plan-lock))
(defmethod (setf plan-lock) (new-lock (self schedulable-object))
  (setf (getf (scheduler-data self) :plan-lock) new-lock))

(defmethod task-plan ((self schedulable-object))
  (or (getf (scheduler-data self) :task-plan) (list)))
(defmethod (setf task-plan) (new-plan (self schedulable-object))
  (setf (getf (scheduler-data self) :task-plan) new-plan))

(defmethod task-plan-lock ((self schedulable-object))
  (getf (scheduler-data self) :task-plan-lock))
(defmethod (setf task-plan-lock) (new-lock (self schedulable-object))
  (setf (getf (scheduler-data self) :task-plan-lock) new-lock))

(defmethod computation-plan ((self schedulable-object))
  (or (getf (scheduler-data self) :computation-plan) (list)))
(defmethod (setf computation-plan) (new-plan (self schedulable-object))
  (setf (getf (scheduler-data self) :computation-plan) new-plan))


(defmethod actionlist ((self schedulable-object))
  (getf (scheduler-data self) :actionlist))
(defmethod (setf actionlist) (actionlist (self schedulable-object))
  (setf (getf (scheduler-data self) :actionlist) actionlist))

(defmethod last-action-pos ((self schedulable-object))
  (getf (scheduler-data self) :last-action-pos))
(defmethod (setf last-action-pos) (new-pos (self schedulable-object))
  (setf (getf (scheduler-data self) :last-action-pos) new-pos))


;;===========================================================================
;; Plan related methods
;;===========================================================================

;; !! TIME-WINDOW must be adapted depending on the scheduling duration
;(t1 (om-get-internal-time)) ;;start date of scheduling operation
;         t2 ;;end date of scheduling operation
;         dt ;;duration of the scheduling operation

;; SCHEDULE : produces the next plan for an object
(defmethod schedule ((sched scheduler) (obj schedulable-object))

  (let ((scheduling

         #'(lambda ()
             (let ((I (get-next-I obj))
                   (start-t (or (car (interval obj)) 0))
                   bundles actlist)
               ;; Get actions as raw data
               (om-with-timeout (timeout sched)
                                #'(lambda ()
                                    (print (format nil "Schedule operation timed out (> ~As) : ~A stopped" (timeout sched) obj))
                                    (stop-schedulable-object obj sched))
                                #'(lambda () (setq bundles (get-action-list-for-play obj (subseq I 0 2)))))

               ;; Wrap actions in the appropriate data structure
               (setq actlist (loop for bundle in bundles
                                   collect
                                   (act-alloc :timestamp (car bundle)
                                              :fun (cadr bundle)
                                              :data (caddr bundle))))

               ;; Update the scheduler next action date (used in timer execution mode only)
               (when (car actlist)
                 (setf (next-date sched) (min (next-date sched) (act-timestamp (car actlist)))))

               ;; Add new actions to the object plan
               (mp:with-lock ((plan-lock obj))
                 (setf (plan obj)
                       (sort (append (plan obj)
                                     (if (nth 2 I)
                                         (if (eq (nth 2 I) :loop)
                                             ;; If the object has to loop, reset its time at the end
                                             (append actlist
                                                     (list (act-alloc :timestamp (1- (cadr I))
                                                                      :fun #'(lambda ()
                                                                               (incf (loop-count obj))
                                                                               (setf (ref-time obj) (- (om-get-internal-time) start-t)
                                                                                     (play-planned? obj) nil)
                                                                               (setf (current-local-time obj) start-t)
                                                                               (schedule sched obj)
                                                                               (interleave-tasks obj
                                                                                                 (list start-t
                                                                                                       (+ start-t (time-window obj))))
                                                                               (set-time-callback obj (car (interval obj)))))))
                                           ;; If the object has to stop, stop the object at the end of interval
                                           (append actlist
                                                   (list (act-alloc :timestamp (1- (cadr I))
                                                                    :fun #'(lambda ()
                                                                             (funcall (run-callback sched)
                                                                                      (get-caller obj sched)
                                                                                      (1- (cadr I)))
                                                                             (player-stop-object sched obj)
                                                                             )))))
                                       ;; If the object continues, keep scheduling
                                       (append (list (act-alloc :timestamp (car I)
                                                                :fun #'(lambda ()
                                                                         (schedule sched obj)
                                                                         (interleave-tasks obj
                                                                                           (list (cadr I)
                                                                                                 (+ (cadr I) (time-window obj)))))))
                                               actlist)))
                             '< :key 'act-timestamp))
                 )))))

    (if (scheduler-multi-thread sched)
        (mp:process-send (process sched) scheduling)
      (funcall scheduling))
    ))


;; move forward the intervals in the object.
;; returns the next interval to schedule
;; returns (t1 t2+1 :stop) when reaches the end (if looper is on, returns :loop and restarts from the beginning)
;; !!! get-obj-dur must be defined for the object (and > 0, otherwise will never start)
(defmethod get-next-I ((self schedulable-object))
  (let* ((tmax (if (cadr (interval self))
                   (if (= (cadr (interval self)) *positive-infinity*)
                       (if (autostop self)
                           (get-obj-dur self)
                         *positive-infinity*)
                     (cadr (interval self)))
                 (if (autostop self)
                     (get-obj-dur self)
                   *positive-infinity*)))
         (t1 (current-local-time self))
         (t2 (min (+ t1 (time-window self)) (or tmax *positive-infinity*))))
    (if (= t2 tmax) ;; the interval touches the end of the object/interval
        (if (looper self)
            (progn
              (reset-I self)
              (list t1 (1+ t2) :loop))
          (list t1 (1+ t2) :stop))
      (progn
        (incf (current-local-time self) (time-window self))
        (when (not (user-time-window self))
          (setf (time-window self) (min *Lmax* (* 2 (time-window self)))))
        (list t1 t2)))))


(defmethod reset-I ((self schedulable-object) &optional date)
  (setf (current-local-time self) (or date (car (interval self)) 0)
        (time-window self) (or (user-time-window self) *Lmin*)))


;; RESCHEDULING OF AN OBJECT
;; From 'time if provided, instantaneous otherwise
(defmethod reschedule ((self schedulable-object) (sched scheduler) &optional time (preserve t))
  (let ((switch-date (if time (+ time *Lmin*)
                       (get-obj-time self))))

    (setf (play-planned? self) nil
          (time-window self) (or (user-time-window self) *lmin*)
          (current-local-time self) switch-date)

    (mp:with-lock ((plan-lock self))
      (setf (plan self)
            (when preserve
              (subseq (plan self)
                      0 (position switch-date (plan self) :test '< :key 'act-timestamp)))))

    (interleave-tasks self (list switch-date (time-window self)))

    (schedule sched self)))


(defmethod interleave-tasks ((self schedulable-object) interval)
  (mp:with-lock ((plan-lock self))
    (setf (plan self)
          (sort
           (append (plan self)
                   (cast-computation-list
                    (get-computation-list-for-play
                     self
                     (list (or (car interval) 0) (or (cadr interval) (get-obj-dur self))))))
           '< :key 'act-timestamp))))


(defun cast-computation-list (plan)
  (loop for task in plan
        collect
        (let ((fun (nth 2 task))
              (data (nth 3 task)))
          (act-alloc :timestamp (nth 0 task)
                     :fun #'(lambda () (if data
                                           (compute (apply fun data))
                                         (compute (funcall fun))))
                     :marker t))))


(defmethod get-pre-computation-plan ((self schedulable-object) interval)
  (loop for task in (sort (get-computation-list-for-play
                           self
                           (list (or (car interval) 0)
                                 (or (cadr interval) (get-obj-dur self)))) '< :key 'cadr)
        collect
        (let ((fun (nth 2 task))
              (data (nth 3 task)))
          (act-alloc :timestamp (nth 0 task)
                     :fun #'(lambda () (if data
                                           (apply fun data)
                                         (funcall fun)))))))


;;===========================================================================
;;;Object informations & Scheduler redefinitions
;;===========================================================================

;; SETS THE ID OF AN OBJECT
;(defmethod set-schedulable-object-id ((self schedulable-object) (number integer))
;  (loop for char across (number-to-string number) do
;        (vector-push-extend char (identifier self)))
;  (vector-push-extend #\. (identifier self)))

;; GET THE OBJECT TIME
(defmethod get-obj-time ((self schedulable-object) &key internal-time)
  (cond ((eq (state self) :pause)
         (pause-time self))
        ((eq (state self) :stop) 0)
        (t (- (or internal-time (om-get-internal-time)) (ref-time self)))))

;; inlined version
(declaim (inline %get-obj-time))
(defun %get-obj-time (obj)
  (cond ((eq (state obj) :pause)
         (pause-time obj))
        ((eq (state obj) :stop) 0)
        (t (- (om-get-internal-time) (ref-time obj)))))

;; GET THE TIMESTAMP OF AN OBJECT
(defmethod item-timestamp ((self schedulable-object))
  0)

;; PLAY AN OBJECT
(defmethod play-item ((self schedulable-object) (sched scheduler) &key caller parent)
  (declare (ignore parent))
  (player-play-object sched self caller))


;;===========================================================================
;;;Player methods
;;===========================================================================

;; PLAYS AN OBJECT
;; internal-time can be used to fix the start reference time (for sync between multiple objects)
;; interval sets the playing interval of the object
;; caller is the object to receive graphic updates
(defmethod play-schedulable-object ((self schedulable-object) (sched scheduler)
                                    &key internal-time interval parent caller)
  (when (or (eq (state self) :stop)
            (not (state self)))
    (init-schedulable-object self :interval interval :parent parent)
    (pre-schedule sched self)
    (setf (ref-time self) (- (or internal-time (om-get-internal-time))
                             (or (car (interval self)) 0)
                             ;(act-timestamp (car (plan self)))
                             ))
    ;(pre-compute-plan self)
    (register-object sched self caller)
    (poke-scheduling-system)))

(defmethod init-schedulable-object ((self schedulable-object) &key interval parent)
  (setf (interval self) interval
        (plan self) nil
        (current-local-time self) (or (car interval) 0)
        (state self) :play)
  (if parent
      (push self (children parent))))

(defmethod pre-schedule ((self scheduler) (obj schedulable-object))
  (schedule self obj)
  (loop for act in
        (get-pre-computation-plan obj (list (or (car (interval obj)) 0)
                                            (+ (or (car (interval obj)) 0) (time-window obj))))
        do
        (if (< (act-timestamp act) 0)
            (%play-action act)
          (mp:with-lock ((plan-lock obj))
            (push act (plan obj)))))
  (mp:with-lock ((plan-lock obj))
    (sort (plan obj) '< :key 'act-timestamp)))

(defmethod register-object ((self scheduler) object caller)
  (mp:with-lock ((lock self))
    (push (list object caller) (register self))))

(defmethod unregister-object ((self scheduler) object)
  (mp:with-lock ((lock self))
    (setf (register self) (remove object (register self) :key 'car))))

(defmethod pre-compute-plan ((self schedulable-object))
  (mp:with-lock ((plan-lock self))
    (loop while (and (plan self) (< (act-timestamp (car (plan self))) 0))
          do
          (om-ignore&print-error (%play-action (pop (plan self)))))))

(defmethod play-schedulable-object ((self t) (sched scheduler) &key internal-time interval parent caller)
  (declare (ignore self sched internal-time interval parent caller))
  nil)

;; PAUSES AN OBJECT
;; internal-time can be used to fix the pause reference time (for sync between multiple objects)
(defmethod pause-schedulable-object ((self schedulable-object) (sched scheduler) &key internal-time)
  (when (eq (state self) :play)
    (let ((internal-time (or internal-time (om-get-internal-time))))
      (loop for child in (children self)
            do
            (player-pause-object sched child))
      (setf (pause-time self) (get-obj-time self :internal-time internal-time)
            (current-local-time self) (pause-time self)
            (state self) :pause))))

(defmethod pause-schedulable-object ((self t) (sched scheduler) &key internal-time)
  (declare (ignore self sched internal-time))
  nil)

;; CONTINUES AN OBJECT
;; internal-time can be used to fix the start reference time (for sync between multiple objects)
(defmethod continue-schedulable-object ((self schedulable-object) (sched scheduler) &key internal-time)
  (when (eq (state self) :pause)
    (let ((internal-time (or internal-time (om-get-internal-time))))

      (loop for child in (children self) do
            (player-continue-object sched child))

      (setf (ref-time self) (round (- internal-time (pause-time self))))
      (setf (state self) :play)
      (poke-scheduling-system))))

(defmethod continue-schedulable-object ((self t) (sched scheduler) &key internal-time)
  (declare (ignore self sched internal-time))
  nil)

;; STOPS AN OBJECT
(defmethod stop-schedulable-object ((self t) (sched scheduler))
  (declare (ignore self sched))
  nil)

(defmethod stop-schedulable-object ((self schedulable-object) (sched scheduler))
  (let ((caller (get-caller self sched)))
    (loop for child in (children self)
          do
          (player-stop-object sched child))
    (unregister-object sched self)
    (when caller (funcall (stop-callback sched) caller))
    (abort-schedulable-object self)))

(defmethod abort-schedulable-object ((self schedulable-object))
  (setf (state self) :stop
        (current-local-time self) (or (car (interval self)) 0)
        (play-planned? self) nil)
  ;(mp:with-lock ((plan-lock self)) (setf (plan self) nil))
  (setf (plan self) nil)
  (destroy-data self))

(defmethod get-caller ((self schedulable-object) (sched scheduler))
  (cadr (find self (register sched) :key 'car)))

