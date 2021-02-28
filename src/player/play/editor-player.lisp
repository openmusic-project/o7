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
;;; AN EDITOR ASSOCIATED WITH A PLAYER
;;;=================================

(defclass play-editor-mixin ()
  ((player :initform nil :accessor player)
   (loop-play :initform nil :accessor loop-play)
   (play-interval :initform nil :accessor play-interval)
   (end-callback :initform nil :accessor end-callback)

   (play-button :initform nil :accessor play-button)
   (pause-button :initform nil :accessor pause-button)
   (stop-button :initform nil :accessor stop-button)
   (time-monitor :initform nil :accessor time-monitor)
   (rec-button :initform nil :accessor rec-button)
   (repeat-button :initform nil :accessor repeat-button)
   (next-button :initform nil :accessor next-button)
   (prev-button :initform nil :accessor prev-button)

   (metronome :initform nil :initarg :metronome :accessor metronome) ;; a metronome object is defined in the score package
   ))

(defmethod editor-make-player ((self play-editor-mixin))
  (make-player :reactive-player
               :run-callback 'play-editor-callback
               :stop-callback 'stop-editor-callback))

(defmethod initialize-instance :after ((self play-editor-mixin) &rest initargs)
  (setf (player self) (editor-make-player self)))

(defmethod editor-close ((self play-editor-mixin))
  (editor-stop self)
  (call-next-method))


;;; TO BE OVERRIDEN BY SUBCLASSES
(defmethod cursor-panes ((self play-editor-mixin)) nil)


;;;=====================================
;;; HELPERS TO SEND DATA TO THE PLAYER:
;;;=====================================

(defmethod get-player-engine ((self play-editor-mixin)) nil)

(defmethod get-obj-to-play ((self play-editor-mixin)) nil)

(defmethod get-play-duration ((self play-editor-mixin))
  (+ (get-obj-dur (get-obj-to-play self)) 100))  ;;; = 0 if obj = NIL

(defmethod play-selection-first ((self t)) t)

(defmethod additional-player-params ((self t)) nil)

;;; return the views to update
(defmethod play-editor-get-ruler-views ((self play-editor-mixin)) nil)

(defmethod default-editor-min-x-range ((self play-editor-mixin)) 1000)

(defmethod default-editor-x-range ((self play-editor-mixin))
  (let ((play-obj (get-obj-to-play self)))
    (list 0
          (+ (or (get-obj-dur play-obj) 0) (default-editor-min-x-range self)))))


(defmethod reinit-x-ranges ((self play-editor-mixin))
  (let ((def-range (default-editor-x-range self)))
    (mapcar #'(lambda (ruler-view)
                (set-ruler-range ruler-view (car def-range) (cadr def-range)))
            (list! (play-editor-get-ruler-views self)))
    ))

(defmethod reinit-x-ranges-from-ruler ((self play-editor-mixin) ruler)
  (reinit-x-ranges self))

;;;=====================================
;;; INTERVAL
;;;=====================================

(defmethod play-editor-default-interval ((self t)) '(0 0))

(defmethod get-interval-to-play ((self play-editor-mixin))
  (when (and (play-selection-first self)
             (play-interval self)
             (get-obj-to-play self))
    (if (= (car (play-interval self)) (cadr (play-interval self)))
        (when (plusp (car (play-interval self)))
          (list (car (play-interval self)) most-positive-fixnum ))
      (play-interval self))))


(defmethod start-interval-selection ((self play-editor-mixin) view pos)
  (let ((t1 (round (pix-to-x view (om-point-x pos)))))
    (om-init-temp-graphics-motion
     view pos nil
     :motion #'(lambda (view p2)
                 (let ((t2 (round (pixel-to-time view (om-point-x p2)))))
                   (editor-set-interval self (list (min t1 t2) (max t1 t2)))))
     :release #'(lambda (view pos)
                  (declare (ignore view pos))
                  (om-invalidate-view (window self)))
     :min-move 10)))

(defmethod change-interval-begin ((self play-editor-mixin) view pos)
  (om-init-temp-graphics-motion
   view pos nil
   :motion #'(lambda (view p)
               (let ((t1 (round (pixel-to-time view (om-point-x p)))))
                 (editor-set-interval self (list t1 (cadr (play-interval self))))))
   :release #'(lambda (view pos)
                (declare (ignore view pos))
                (editor-set-interval self (list (apply 'min (play-interval self))
                                                (apply 'max (play-interval self))))
                (om-invalidate-view (window self)))
   :min-move 4))

(defmethod change-interval-end ((self play-editor-mixin) view pos)
  (om-init-temp-graphics-motion
   view pos nil
   :motion #'(lambda (view p)
               (let ((t2 (round (pixel-to-time view (om-point-x p)))))
                 (editor-set-interval self (list (car (play-interval self)) t2))))
   :release #'(lambda (view pos)
                (declare (ignore view pos))
                (editor-set-interval self (list (apply 'min (play-interval self))
                                                (apply 'max (play-interval self))))
                (om-invalidate-view (window self)))
   :min-move 4))

(defmethod editor-fix-interval ((self play-editor-mixin) interval &key (high-bound nil))
  (list (if (car interval) (max 0 (car interval)) 0)
        (if high-bound
            (min (get-obj-dur (get-obj-to-play self)) (cadr interval))
          (cadr interval))))

(defmethod editor-set-interval ((self t) interval) nil)

(defmethod update-cursor-pane-intervals ((self play-editor-mixin))
  (let ((inter (play-interval self)))
    (when inter
      (mapcar #'(lambda (p)
                  (setf (cursor-interval p) inter)
                  (om-invalidate-view p))
              (cursor-panes self)))))

(defmethod editor-set-interval ((self play-editor-mixin) interval)
  (let ((inter (editor-fix-interval self interval)))
    (setf (play-interval self) inter)
    (set-object-interval (get-obj-to-play self) inter)
    (when (is-looping (get-obj-to-play self))
      (notify-scheduler (get-obj-to-play self)))
    (update-cursor-pane-intervals self)
    ))

(defmethod set-cursor-time ((self play-editor-mixin) time)
  (mapcar #'(lambda (pane) (update-cursor pane time)) (cursor-panes self))
  (editor-invalidate-views self))


;;;=================================
;;; PLAYER CALLS
;;;=================================

(defmethod play-obj? ((self t)) nil)

(defmethod editor-play-state ((self play-editor-mixin))
  (player-get-object-state (player self) (get-obj-to-play self)))


(defmethod start-box-callback ((self t)) nil)
(defmethod play-box-callback ((self t) time) nil)
(defmethod stop-box-callback ((self t)) nil)

(defmethod start-editor-callback ((self t)) nil)

(defmethod start-editor-callback ((self play-editor-mixin))
  (when (pause-button self) (button-unselect (pause-button self)))
  (when (play-button self) (button-select (play-button self)))
  (mapcar #'start-cursor (cursor-panes self))
  (when (object self) (start-box-callback (object self))))

(defmethod play-editor-callback ((self play-editor-mixin) time)
  (set-time-display self time)
  (mapcar #'(lambda (view) (when view (update-cursor view time))) (cursor-panes self))
  (when (object self) (play-box-callback (object self) time)))

(defmethod stop-editor-callback ((self t)) nil)

(defmethod stop-editor-callback ((self play-editor-mixin))
  (when (play-button self) (button-unselect (play-button self)))
  (when (pause-button self) (button-unselect (pause-button self)))
  (mapcar 'stop-cursor (remove nil (cursor-panes self)))
  (when (object self) (stop-box-callback (object self))))

;;; never used..
(defmethod editor-callback-fun ((self play-editor-mixin))
  #'(lambda (editor time)
      (handler-bind ((error #'(lambda (e)
                                (print e)
                                (om-kill-process (callback-process (player self)))
                                (abort e))))
        (play-editor-callback editor time))))

(defmethod editor-play ((self play-editor-mixin))

  (when (play-obj? (get-obj-to-play self))

    (start-editor-callback self)

    (if (equal (player-get-object-state (player self) (get-obj-to-play self)) :pause)

        (progn
          (player-continue-object (player self) (get-obj-to-play self))
          (when (metronome self)
            (player-continue-object (player self) (metronome self))))

      (let ((interval (get-interval-to-play self)))
        (when (metronome self)
          (player-play-object (player self) (metronome self) nil :interval interval))
        (player-play-object (player self) (get-obj-to-play self) self :interval interval)
        (player-start (player self) :start-t (or (car interval) 0) :end-t (cadr interval)))
      )))


(defmethod editor-pause ((self play-editor-mixin))
  (when (play-button self) (button-unselect (play-button self)))
  (when (pause-button self) (button-select (pause-button self)))
  (when (metronome self)
    (player-pause-object (player self) (metronome self)))
  (player-pause-object (player self) (get-obj-to-play self)))


;;; this is called sometimes to reset by security..
(defmethod editor-stop ((self t)) nil)

(defmethod editor-stop ((self play-editor-mixin))

  (stop-editor-callback self)

  (when (not (equal :stop (player-get-object-state (player self) (get-obj-to-play self))))
    (player-stop-object (player self) (get-obj-to-play self)))

  (when (and (metronome self)
             (not (equal :stop (player-get-object-state (player self) (metronome self)))))
    (player-stop-object (player self) (metronome self)))

  (let ((start-time (or (car (play-interval self)) (car (play-editor-default-interval self)))))
    (set-time-display self start-time)
    (mapcar #'(lambda (view) (update-cursor view start-time)) (cursor-panes self)))
  )

(defmethod editor-play/stop ((self play-editor-mixin))
  (if (not (eq (player-get-object-state (player self) (get-obj-to-play self)) :stop))
      (editor-stop self)
    (editor-play self)))

(defmethod editor-play/pause ((self play-editor-mixin))
  (if (not (eq (player-get-object-state (player self) (get-obj-to-play self)) :play))
      (editor-play self)
    (editor-pause self)))


(defmethod editor-record ((self play-editor-mixin))
  ;;;(setf (engines (player self)) (list (get-player-engine self)))
  ;(player-record (player self))
  nil)

;;; FUNCTIONS TO DEFINE BY THE EDITORS
(defmethod editor-next-step ((self play-editor-mixin)) nil)
(defmethod editor-previous-step ((self play-editor-mixin)) nil)


(defmethod editor-repeat ((self play-editor-mixin) t-or-nil)
  (player-loop-object (player self) (get-obj-to-play self) t-or-nil))


(defmethod editor-reset-interval ((self play-editor-mixin))
  (editor-set-interval self (play-editor-default-interval self))
  (mapcar 'reset-cursor (cursor-panes self)))

(defmethod editor-reset-cursor ((self play-editor-mixin))
  (mapcar 'stop-cursor (cursor-panes self)))

(defmethod editor-key-action :around ((self play-editor-mixin) key)
  (case key
    (#\Space (editor-play/pause self) t)
    (#\p (editor-play/pause self) t)
    (#\s (editor-stop self) t)
    (:om-key-esc
     (when (eq (player-get-object-state (player self) (get-obj-to-play self)) :stop)
       (if (equal '(0 0) (play-interval self))
           (call-next-method) ;; if the interval is already reset: check if there is another 'escape' to do
         (editor-reset-interval self)))
     (editor-stop self)
     (call-next-method)
     t)
    (otherwise (call-next-method))
    ))


;;;===================================
; VIEW WITH CURSOR
;;;===================================

(defclass x-cursor-graduated-view (x-graduated-view om-transient-drawing-view)
  ((cursor-mode  :initform :normal :accessor cursor-mode :initarg :cursor-mode)   ;; :normal ou :interval
   (cursor-interval :initform '(0 0) :accessor cursor-interval)
   (cursor-interval-fill-color :accessor cursor-interval-fill-color :initarg :cursor-interval-fill-color
                               :initform (om-make-color-alpha (om-def-color :gray) 0.2))
   (cursor-interval-lines-color :accessor cursor-interval-lines-color :initarg :cursor-interval-lines-color
                                :initform (om-make-color 0.4 0.2 0.2))
   (cursor-pos :initform 0 :accessor cursor-pos))
  (:default-initargs :fit-size-to-children nil))

(defmethod time-to-pixel ((self x-cursor-graduated-view) time)
  (x-to-pix self time))

;++++++++++++++++++++++++++++++++
; RULES FOR THE CURSOR:
; - Set cursot at a position = click on the view
; - Move cursor = click and drag on it
; - Set a new interval/start point = double-click on the view
; - Change existing interval : click and drag on begin or end
; - Reset cursor = escape
; - Reset interval = 2nd escape
;++++++++++++++++++++++++++++++++

(defmethod draw-cursor-line ((self x-cursor-graduated-view) position size)
  (om-draw-line (om-point-x position) (om-point-y position)
                (om-point-x position)
                (+ (om-point-y position) (om-point-y size))
                :color (om-make-color 0.8 0.5 0.5)))

(defmethod drag-move-cursor ((self x-cursor-graduated-view) position)
  (om-init-temp-graphics-motion
   self position nil
   :motion #'(lambda (view p)
               (declare (ignore view))
               (let ((t1 (round (pix-to-x self (om-point-x p)))))
                 (set-cursor-time (editor self) t1)))
   :release #'(lambda (view pos)
                (declare (ignore view pos))
                (om-invalidate-view (window (editor self))))
   :min-move 4))

(defmethod start-cursor ((self x-cursor-graduated-view))
  (om-stop-transient-drawing self)
  (om-start-transient-drawing
   self #'draw-cursor-line
   (omp (time-to-pixel self (car (cursor-interval self))) 0)
   (omp 2 (h self))))

(defmethod stop-cursor ((self x-cursor-graduated-view))
  (om-stop-transient-drawing self)
  (om-invalidate-view self))

(defmethod reset-cursor ((self x-cursor-graduated-view))
  (update-cursor self 0))

(defmethod om-view-resized :after ((self x-cursor-graduated-view) size)
  (om-update-transient-drawing
   self
   :x (time-to-pixel self (cursor-pos self))
   :h (om-point-y size)))

(defmethod update-cursor ((self x-cursor-graduated-view) time &optional y1 y2)
  (unless (= (cursor-pos self) time)
    (setf (cursor-pos self) time))
  (om-update-transient-drawing self :x (time-to-pixel self (cursor-pos self))))


(defmethod update-view-from-ruler ((self x-ruler-view) (view x-cursor-graduated-view))
  (om-update-transient-drawing view :x (time-to-pixel view (cursor-pos view)))
  (call-next-method))


(defmethod pixel-to-time ((self x-cursor-graduated-view) x)
  (round (pix-to-x self x)))

(defmethod om-draw-contents :after ((self x-cursor-graduated-view))
  (when (and (play-obj? (get-obj-to-play (editor (om-view-window self))))
             (cursor-interval self))
    (let ((t1 (car (cursor-interval self)))
          (t2 (cadr (cursor-interval self))))
      (unless (= t1 t2 0)
        (let ((i1 (time-to-pixel self (car (cursor-interval self))))
              (i2 (time-to-pixel self (cadr (cursor-interval self)))))
          (om-with-fg-color (cursor-interval-lines-color self)
            (om-with-line '(3 3)
              (om-with-line-size 1
                (om-draw-line i1 0 i1 (h self))
                (om-draw-line i2 0 i2 (h self)))))
          (om-draw-rect i1 0 (- i2 i1) (h self) :fill t :color (cursor-interval-fill-color self))
          (unless t ;(equal (editor-play-state (editor self)) :play)
            ;; => never do that, when play is stopped
            (draw-cursor-line self
                              (omp (time-to-pixel self (cursor-pos self)) 0)
                              (omp 4 (h self)))
            )
          )))))

;;; return T if detected/did something
(defmethod handle-selection-extent ((self x-cursor-graduated-view) position)
  (let ((tpl-editor (editor (om-view-window self)))
        (bx (time-to-pixel self (car (cursor-interval self))))
        (ex (time-to-pixel self (cadr (cursor-interval self)))))
    (cond ((om-point-in-line-p position (omp ex 0) (omp ex (h self)) 4)
           (change-interval-end tpl-editor self position)
           t)
          ((om-point-in-line-p position (omp bx 0) (omp bx (h self)) 4)
           (change-interval-begin tpl-editor self position)
           t)
          (t
           ;(set-cursor-time tpl-editor (pixel-to-time self (om-point-x position)))
           ;(start-interval-selection tpl-editor self position)
           nil
           ))))

(defmethod om-view-click-handler ((self x-cursor-graduated-view) position)
  (handle-selection-extent self position))


(defmethod om-transient-drawing-item-clicked ((self x-cursor-graduated-view) clicked-pos-in-view)
  (drag-move-cursor self clicked-pos-in-view))


(defmethod om-view-doubleclick-handler ((self x-cursor-graduated-view) position)
  (let ((time (max 0 (pixel-to-time self (om-point-x position))))
        (tpl-editor (editor self)) ;; we assume it is an OMEditorView...
        )
    (editor-set-interval tpl-editor (list time time))
    (set-cursor-time tpl-editor time)
    (editor-stop tpl-editor)
    (call-next-method)))


(defmethod om-view-mouse-motion-handler :around ((self x-cursor-graduated-view) position)
  (let ((bx (time-to-pixel self (car (cursor-interval self))))
        (ex (time-to-pixel self (cadr (cursor-interval self)))))
    (cond ((or (om-point-in-line-p position (omp bx 0) (omp bx (h self)) 4)
               (om-point-in-line-p position (omp ex 0) (omp ex (h self)) 4))
           (om-set-view-cursor self (om-get-cursor :h-size)))
          (t (om-set-view-cursor self (om-view-cursor self))
             (call-next-method)))))


;;;=================================
;;; STANDARDIZED PLAY CONTROLS
;;;=================================

;;; inserts teh button in a view, so it can be in a simple layout
(defun make-button-view (button)
  (om-make-view 'om-view
                :size (om-view-size button)
                :subviews (list button)))


(defmethod make-play-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (play-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-play-black
                                 :icon-pushed :icon-play-green
                                 :icon-disabled :icon-play-gray
                                 :lock-push t :enabled enable
                                 :pushed (equal (player-get-object-state (player editor) (get-obj-to-play editor)) :play)
                                 :action #'(lambda (b)
                                             (declare (ignore b))
                                             (editor-play editor))))))


(defmethod make-pause-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (pause-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-pause-black :icon-pushed :icon-pause-orange :icon-disabled :icon-pause-gray
                                 :lock-push t :enabled enable
                                 :pushed (equal (player-get-object-state (player editor) (get-obj-to-play editor)) :pause)
                                 :action #'(lambda (b)
                                             (declare (ignore b))
                                             (editor-pause editor))))))

(defmethod make-stop-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (stop-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-stop-black :icon-pushed :icon-stop-white :icon-disabled :icon-stop-gray
                                 :lock-push nil :enabled enable
                                 :action #'(lambda (b)
                                             (declare (ignore b))
                                             (when (pause-button editor) (button-unselect (pause-button editor)))
                                             (when (play-button editor) (button-unselect (play-button editor)))
                                             (editor-stop editor))))))

(defmethod make-previous-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (prev-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-previous-black :icon-pushed :icon-previous-white :icon-disabled :icon-previous-gray
                                 :lock-push nil :enabled enable
                                 :action #'(lambda (b)
                                             (declare (ignore b))
                                             (editor-previous-step editor))))))

(defmethod make-next-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (next-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-next-black :icon-pushed :icon-next-white :icon-disabled :icon-next-gray
                                 :lock-push nil :enabled enable
                                 :action #'(lambda (b)
                                             (declare (ignore b))
                                             (editor-next-step editor))))))

(defmethod make-rec-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (rec-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-record-black :icon-pushed :icon-record-red :icon-disabled :icon-record-gray
                                 :lock-push t :enabled enable
                                 :action #'(lambda (b)
                                             (declare (ignore b))
                                             (editor-record editor))))))

(defmethod make-repeat-button ((editor play-editor-mixin) &key size enable)
  (make-button-view
   (setf (repeat-button editor)
         (om-make-graphic-object 'om-icon-button :size (or size (omp 16 16))
                                 :icon :icon-repeat-black :icon-pushed :icon-repeat-orange :icon-disabled :icon-repeat-gray
                                 :lock-push t :enabled enable
                                 :pushed (is-looping (get-obj-to-play editor))
                                 :action #'(lambda (b)
                                             (editor-repeat editor (print (pushed b))))))))


(defun time-display (time_ms &optional format)
  (multiple-value-bind (time_s ms)
      (floor (round time_ms) 1000)
    (multiple-value-bind (time_m s)
        (floor time_s 60)
      (multiple-value-bind (h m)
          (floor time_m 60)
        (list h m s ms)
        (if format
            (format nil "~2,'0dh~2,'0dm~2,'0ds~3,'0d" h m s ms)
          (if (= h 0)
              (format nil "~2,'0d:~2,'0d:~3,'0d" m s ms)
            (format nil "~2,'0d:~2,'0d:~2,'0d:~3,'0d" h m s ms)))))))


(defmethod make-time-monitor ((editor play-editor-mixin) &key time color background font)
  (setf (time-monitor editor)
        (om-make-di 'om-simple-text :size (omp 100 20)
                    :text (if time (time-display time) "")
                    :font (or font (om-def-font :font2))
                    :bg-color background
                    :fg-color (or color (om-def-color :black)))))


(defmethod set-time-display ((self play-editor-mixin) time)
  (when (time-monitor self)
    (om-set-dialog-item-text (time-monitor self) (if time (time-display time) ""))))


(defmethod enable-play-controls ((self play-editor-mixin) t-or-nil)
  (mapc
   #'(lambda (b) (when b (setf (enabled b) t-or-nil) (om-invalidate-view b)))
   (list (play-button self) (pause-button self) (stop-button self) (rec-button self) (repeat-button self) (prev-button self) (next-button self)))
  (when (time-monitor self) (set-time-display self (if t-or-nil 0 nil))))


;;;==========================
;;; A RULER WITH TIME GRADUATION + MOVING CURSOR
;;;==========================

(defclass time-ruler (x-ruler-view x-cursor-graduated-view)
  ((unit :accessor unit :initform :ms :initarg :unit)
   (bottom-p :accessor bottom-p :initform t :initarg :bottom-p) ;bottom-p indicates if the arrow need to be on the top or the bottom (default is on the top)
   (markers-p :accessor markers-p :initform t :initarg :markers-p) ;use or not markers
   (markers-count-object-onset-p
    :accessor markers-count-object-onset-p :initform t :initarg :markers-count-object-onset-p
    :documentation "if T, markers will consider the objet onset for placement (e.g. in a sequencer)")
   (snap-to-grid :accessor snap-to-grid :initform t :initarg :snap-to-grid)
   (selected-time-markers :accessor selected-time-markers :initform nil))
  (:default-initargs :vmin 0))

(defmethod start-cursor ((self time-ruler)) nil)

(defmethod pixel-to-time ((self time-ruler) x)
  (round (pix-to-x self x)))

(defmethod unit-value-str ((self time-ruler) value &optional (unit-dur 0))
  (if (equal (unit self) :ms) (call-next-method)
    (if (> unit-dur 100) (format nil "~d" (round (/ value 1000.0)))
      (format nil
              (cond ((= unit-dur 100) "~1$")
                    ((= unit-dur 10) "~2$")
                    ((= unit-dur 1) "~3$")
                    (t "~0$"))
              (/ value 1000.0)))))

(defmethod om-draw-contents ((self time-ruler))
  ;draw the markers
  (when (markers-p self)
    (loop for marker in (get-all-time-markers self)
          do
          (let ((pos (time-to-pixel self marker)))
            (om-with-fg-color (om-make-color 0.9 0.7 0 (if (find marker (selected-time-markers self)) 1 0.45))
              (om-draw-line pos 0 pos (h self))
              (if (bottom-p self)
                  (om-draw-polygon (list (omp (- pos 4) 0)
                                         (omp (- pos 4) (- (h self) 5))
                                         (omp pos (h self))
                                         (omp (+ pos 4) (- (h self) 5))
                                         (omp (+ pos 4) 0) )
                                   :fill t)
                (om-draw-polygon (list (omp (- pos 4) (h self))
                                       (omp (- pos 4) 5)
                                       (omp pos  0)
                                       (omp (+ pos 4) 5)
                                       (omp (+ pos 4) (h self)))
                                 :fill t)
                )))))

  ;draw the play head
  ;(let ((pos (x-to-pix self (cursor-pos self))))
  ;  (om-with-fg-color (om-make-color 1 1 1 0.5)
  ;    (if (bottom-p self)
  ;        (om-draw-polygon (list (omp (- pos 5) (- (h self) 5))
  ;                               (omp (+ pos 5) (- (h self) 5))
  ;                               (omp pos (h self)))
  ;                         :fill t)
  ;      (om-draw-polygon (list (omp (- pos 5)  5)
  ;                             (omp (+ pos 5) 5)
  ;                              (omp pos  0))
  ;                       :fill t)))
  ;  (call-next-method))

  (call-next-method)
  )

(defmethod draw-grid-from-ruler ((self om-view) (ruler time-ruler))
  (when (markers-p ruler)
    (loop for marker in (get-all-time-markers ruler)
          do
          (let ((pos (time-to-pixel ruler marker)))
            (om-with-fg-color (om-make-color  0.9 0.7 0 (if (find marker (selected-time-markers ruler)) 1 0.45))
              (om-draw-line pos 0 pos (h self))))))
  (om-with-line '(2 2)
    (call-next-method)))


;;;=================================
; SNAP TO GRID FUNCITONNALITIES
;;;=================================

(defmethod snap-time-to-grid  ((ruler time-ruler) time &optional (snap-delta nil))
  ;returns a time value corresponding to the given time aligned on the grid with a delta (in ms) treshold.
  ;default treshold is a tenth of the grid
  (let* ((unit-dur (get-units ruler))
         (delta (if snap-delta (min snap-delta (/ unit-dur 2)) (/ unit-dur 8)))
         (offset  (mod time unit-dur)))
    (if (> offset (- unit-dur delta))
        (- (+ time unit-dur) offset)
      (if (< offset delta)
          (- time offset)
        time))))


(defmethod snap-time-to-markers  ((ruler time-ruler) time &optional (snap-delta nil) (source-time nil))
  (let* ((unit-dur (get-units ruler))
         (delta  (if snap-delta (min snap-delta (/ unit-dur 2)) (/ unit-dur 8)))
         (markers (get-all-time-markers ruler))
         (pos nil)
         (pre-marker nil)
         (post-marker nil))
    (when source-time
      (let ((src-pos (position source-time markers)))
        (when src-pos
          (setf markers (remove-nth src-pos markers)))))
    (setf pos (or (position time markers  :test '<= ) (length markers)))
    (when (> pos 0)
      (setf pre-marker (nth (1- pos) markers)))
    (when (< pos (1- (length markers)))
      (setf post-marker (nth pos markers)))
    (if (and pre-marker (< (- time pre-marker) delta))
        pre-marker
      (if (and post-marker (< (- post-marker time) delta))
          post-marker
        time))))

(defmethod snap-time-to-grid-and-markers ((ruler time-ruler) time &optional (snap-delta nil) (source-time nil))
  (let* ((time-grid (snap-time-to-grid ruler time snap-delta))
         (time-marker (snap-time-to-markers ruler time snap-delta source-time))
         (d1 (if (equal time time-grid) most-positive-fixnum (abs (- time time-grid))))
         (d2 (if (equal time time-marker) most-positive-fixnum (abs (- time time-marker)))))
    (if (<= d2 d1) time-marker time-grid)))

;utilities to adapt dt for snap to grid functionnalities
(defmethod adapt-dt-for-grid ((ruler time-ruler) selected-point-time dt)
  (let* ((newt (+ selected-point-time dt))
         (newt-grid (snap-time-to-grid ruler newt nil))
         (offset (- newt-grid newt)))
    (+ dt offset)))

(defmethod adapt-dt-for-markers ((ruler time-ruler) selected-point-time dt)
  (let* ((newt (+ selected-point-time dt))
         (newt-grid (snap-time-to-markers ruler newt nil selected-point-time))
         (offset (- newt-grid newt)))
    (+ dt offset)))

(defmethod adapt-dt-for-grid-and-markers ((ruler time-ruler) selected-point-time dt &optional snap-delta)
  (let* ((newt (+ selected-point-time dt))
         (newt-grid (snap-time-to-grid-and-markers ruler newt snap-delta selected-point-time))
         (offset (- newt-grid newt)))
    (+ dt offset)))


;;; CALLED BY PAN/ZOOM GESTURES

(defmethod shift-time-ruler ((self time-ruler) dx)
  (let ((dxx (* (/ dx (w self)) (- (v2 self) (v1 self)))))
    (unless (or (and (plusp dxx) (vmin self) (= (vmin self) (v1 self)))
                (and (minusp dxx) (vmax self) (= (vmax self) (v2 self))))
      (set-ruler-range self
                       (if (vmin self) (max (vmin self) (- (v1 self) dxx)) (- (v1 self) dxx))
                       (if (vmax self) (min (vmax self) (- (v2 self) dxx)) (- (v2 self) dxx))))
    ))

(defmethod zoom-time-ruler ((self time-ruler) dx center view)
  (let* ((position (or center (omp (* (w view) .5) (* (h view) .5))))
         (x-pos (pix-to-x view (om-point-x position)))
         (curr-w (- (x2 view) (x1 view)))
         (new-w (round (* curr-w (1+ dx))))
         (new-x1 (round (- x-pos (/ (* (- x-pos (x1 view)) new-w) curr-w)))))

    (set-ruler-range self new-x1 (+ new-x1 new-w))
    ))


;;;=================================
;;; TIME MARKERS API
;;;=================================

;TO USE MARQUERS :
;1) Have a child class of timed-objects and overload the get-time-markers method
;2) Have a child class of x-graduated view and overload the following methods

(defmethod get-time-markers ((self t))
  "returns the list of time markers in an object"
  nil)

(defmethod get-elements-for-marker ((self timed-object) marker)
  "returns a list of one or more elements in the object that correspond to the marker"
  nil)

(defmethod translate-elements-from-time-marker ((self timed-object) elems dt)
  "translates elements from a time marker with dt"
  (declare (ignore elems))
  (setf (onset self) (max 0 (+ (onset self) dt))))


(defmethod get-timed-objects-with-markers ((self x-graduated-view))
  "returns a list of timed-object containing markers"
  nil)

(defmethod select-elements-at-time ((self x-cursor-graduated-view) marker-time)
  "selects the elements with same time than the marker-time"
  nil)

;;; optional
(defmethod clear-editor-selection ((self OMEditor))
  (set-selection self nil))

(defmethod get-editor-for-graduated-view ((self x-graduated-view))
  "returns the editor handling the graduated view/selection and translation for timed-objects"
  (editor self))

;;;=================================
;;; ACTIONS ands Utilities
;;;=================================

(defmethod get-related-views-editors ((self time-ruler))
  (remove-duplicates (remove nil (loop for rv in (related-views self) collect (get-editor-for-graduated-view rv)))))

(defmethod select-elements-from-marker ((self time-ruler) marker-time)
  (loop for ed in (get-related-views-editors self) do
        (clear-editor-selection ed))
  (loop for rv in (related-views self) do
        (select-elements-at-time rv marker-time)))

(defmethod get-onset ((self timed-object)) (onset self))
(defmethod get-onset ((self OMBox)) (get-box-onset self))

(defmethod get-all-time-markers ((self time-ruler))
  (sort (remove nil (flat (loop for view in (related-views self)
                                collect
                                (loop for timed-obj in (get-timed-objects-with-markers view)
                                      collect
                                      (if (markers-count-object-onset-p self)
                                          (om+ (get-time-markers timed-obj) (get-onset timed-obj))
                                        (get-time-markers timed-obj))
                                      )))) '<))


(defmethod find-marker-at-time ((self time-ruler) time)
  ;gets the first marker for each related views that is close of 5pix to the position of the mouse.
  (let ((delta-t (dpix-to-dx self 5)))
    (loop for marker-time in (get-all-time-markers self)
          when (and  (<= time  (+ delta-t marker-time))  (>= time (- marker-time delta-t)))
          return marker-time
          )))


(defmethod translate-from-marker-action ((self time-ruler) marker position)

  (let* ((ref-time (pix-to-x self (om-point-x position)))
         (objs (remove nil (flat (loop for rv in (related-views self)
                                       collect
                                       (get-timed-objects-with-markers rv)))))
         (obj-elem-list
          (remove nil
                  (loop for obj in objs collect
                        (let ((matching-elements
                               (get-elements-for-marker
                                obj
                                (if (markers-count-object-onset-p self)
                                    (om- marker (get-onset obj)) marker))))
                          (when matching-elements
                            (list obj matching-elements)))))))

    (om-init-temp-graphics-motion
     self position nil
     :min-move 4
     :motion #'(lambda (view pos)
                 (let* ((tmp_time (pixel-to-time view (om-point-x pos)))
                        (dt (round (- tmp_time ref-time)))
                        (new-dt (if (snap-to-grid self) (adapt-dt-for-grid-and-markers self marker dt) dt)))
                   (when (not (zerop new-dt))
                     (loop for item in obj-elem-list do
                           (let ((obj (car item))
                                 (elem (cadr item)))
                             (translate-elements-from-time-marker obj elem new-dt)))
                     (loop for ed in (get-related-views-editors self)
                           do (update-to-editor ed self))
                     (setf (selected-time-markers self)
                           (replace-in-list (selected-time-markers self)
                                            (+ marker new-dt)
                                            (position marker (selected-time-markers self))))
                     (setf marker (+ marker new-dt))
                     (setf ref-time (+ ref-time new-dt))
                     (mapcar 'om-invalidate-view (related-views self))
                     (om-invalidate-view self)))))))

;=========
; EVENTS
;=========

(defmethod om-view-click-handler ((self time-ruler) position)
  (if (markers-p self)
      (let* ((time (pix-to-x self (om-point-x position)))
             (marker (find-marker-at-time self time)))
        (if marker
            (progn
              (setf (selected-time-markers self) (list marker))
              (select-elements-from-marker self marker)
              (translate-from-marker-action self marker position))
          (call-next-method)))
    (call-next-method)))

(defmethod om-view-mouse-leave-handler ((self time-ruler))
  (om-set-view-cursor self nil))

(defmethod om-view-mouse-motion-handler ((self time-ruler) pos)
  (when (markers-p self)
    (if (find-marker-at-time self (pix-to-x self (om-point-x pos)))
        (om-set-view-cursor self nil)
      (om-set-view-cursor self (om-view-cursor self)))))
