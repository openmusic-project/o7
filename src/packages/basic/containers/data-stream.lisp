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

;;;======================================
;;; DIFFERENT KIND OF DATA (FRAMES/BUNDLES)
;;;======================================
(defclass* data-frame (timed-item timed-object)
  ((attributes :accessor attributes :initarg :attributes :initform nil :documentation "some additional attributes for drawing etc.")))

;;; COMPAT: REPLACE THE CALLS WITH ONSET
(defmethod date ((self data-frame)) (onset self))
(defmethod (setf date) (time (self data-frame)) (setf (onset self) time))

;;; TIME-SEQUENCE API
(defmethod item-get-time ((self data-frame)) (date self))
(defmethod item-set-time ((self data-frame) time) (setf (date self) time))

(defmethod data-size ((self data-frame)) 1)
(defmethod data-frame-text-description ((self data-frame)) '("DATA FRAME"))

(defmethod get-frame-action ((self data-frame))
  #'(lambda () (print "EMPTY ACTION")))

(defmethod get-obj-dur ((self data-frame)) (item-get-duration self))

;;; SIMPLEST DATA FRAME
(defclass act-bundle (data-frame)
  (;; (date :accessor date :initarg :dateg :initform 0 :documentation "date/time of the frame")
   (actions :accessor actions :initarg :actions :initform nil)))

(defmethod get-frame-action ((self act-bundle))
  #'(lambda () (mapcar 'funcall (actions self))))

(defun make-act-bundle (date actions)
  (make-instance 'act-bundle
                 :date date
                 :actions actions))

;;;======================================
;;; INTERNAL CLASS
;;;======================================
(defclass internal-data-stream (named-object time-sequence schedulable-object)
  ((default-frame-type :accessor default-frame-type :initform 'act-bundle)
   (frames :initform nil :documentation "a list of timed data chunks")
   ))


;; redefine for other slots
(defmethod data-stream-frames-slot ((self internal-data-stream)) 'frames)

(defmethod frames ((self internal-data-stream)) (slot-value self (data-stream-frames-slot self)))
(defmethod (setf frames) (frames (self internal-data-stream)) (setf (slot-value self (data-stream-frames-slot self)) frames))

(defmethod data-stream-get-frames ((self internal-data-stream)) (frames self))
(defmethod data-stream-set-frames ((self internal-data-stream) frames)
  (setf (frames self) frames)
  (time-sequence-update-internal-times self)
  (time-sequence-update-obj-dur self))

;;; TIME-SEQUENCE API (called by timeline editor etc.)
(defmethod time-sequence-get-timed-item-list ((self internal-data-stream))
  (data-stream-get-frames self))

(defmethod time-sequence-set-timed-item-list ((self internal-data-stream) list)
  (data-stream-set-frames self list))

(defmethod time-sequence-make-timed-item-at ((self internal-data-stream) at)
  (make-instance (default-frame-type self) :onset at))


;;;======================================
;;; MAIN CLASS
;;;======================================

;;; redefines the slots as :initargs
(defclass* data-stream (internal-data-stream named-object time-sequence schedulable-object)
  ((default-frame-type :accessor default-frame-type :initarg :default-frame-type :initform 'act-bundle)
   (frames :initarg :frames :initform nil :documentation "a list of timed data chunks")
   )
  (:documentation "A general container to organize data in time."))


;;; called after initialize-instance in OM-context
(defmethod om-init-instance ((self data-stream) &optional initargs)

  (let ((frames (find-value-in-kv-list initargs :frames)))

    (when frames
      (setf (default-frame-type self) (type-of (car frames)))
      ;;; => makes copies of the frames if provided as initargs
      (setf (frames self) (om-copy (frames self))))

    (setf (frames self) (sort (remove nil (frames self)) '< :key 'item-get-time))
    (mapc #'(lambda (f) (setf (attributes f) nil)) frames))

  (call-next-method))


(defmethod display-modes-for-object ((self data-stream))
  '(:mini-view :text :hidden))

(defmethod draw-mini-view ((self data-stream) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (om-with-fg-color (om-make-color-alpha (om-def-color :dark-blue) 0.5)
      (multiple-value-bind (fx ox)
          (conversion-factor-and-offset 0 (get-obj-dur self) w x)
        (loop for frame in (data-stream-get-frames self) do
              (om-draw-rect (+ ox (* fx (or (date frame) 0)))
                            y 4 h
                            :fill t)
              )))))


;;;======================================
;;; OBJECT PROPERTIES
;;;======================================
(defmethod play-obj? ((self internal-data-stream)) t)

(defmethod get-action-list-for-play ((object internal-data-stream) interval &optional parent)
  (mapcar
   #'(lambda (frame)
       (list (date frame)
             #'(lambda () (funcall (get-frame-action frame)))))
   (remove-if #'(lambda (date) (not (in-interval date interval :exclude-high-bound t)))
              (data-stream-get-frames object)
              :key #'onset)))

;;;======================================
;;; OMMETHOD FOR PATCHES
;;;======================================

(defmethod* add-frame-in-data-stream ((self internal-data-stream) frame)
  (time-sequence-insert-timed-item-and-update self frame)
  frame)

(defmethod* add-frame-in-data-stream ((self t) frame)
  (om-beep-msg "ERROR: ~A is not a valid DATA-STREAM" self))

;;; when editing in mode "box" => allows to update editor
(defmethod* add-frame-in-data-stream ((self omboxeditcall) frame)
  (time-sequence-insert-timed-item-and-update (get-box-value self) frame)
  (update-after-eval self)
  frame)

(defmethod* clear-data-stream ((self internal-data-stream))
  (data-stream-set-frames self nil))

(defmethod* clear-data-stream ((self t))
  (om-beep-msg "ERROR: ~A is not a valid DATA-STREAM" self))

;;; when editing in mode "box" => allows to update editor
(defmethod* clear-data-stream ((self omboxeditcall))
  (clear-data-stream (get-box-value self))
  (update-after-eval self))






