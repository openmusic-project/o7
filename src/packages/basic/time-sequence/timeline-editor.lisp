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
; File author: J. Garcia
;============================================================================

(in-package :om)

;graphical vars
;this define the view height and the items size as well (70% height)
(defparameter *timeline-view-height* 20)
(defparameter *timeline-item-height* (* *timeline-view-height* 0.7))


;;; TIMELINE EDITOR
;;; IS GENERALLY ATTACHED TO ANOTHER EDITOR

(defclass timeline-editor (OMEditor play-editor-mixin)
  ((time-ruler :accessor time-ruler :initform nil :initarg :time-ruler)
   (timeline-views :accessor timeline-views :initform nil :initarg :timeline-views)
   (snap-to-grid :accessor snap-to-grid :initform t :initarg :snap-to-grid)))


(defmethod editor-get-time-sequence ((self OMEditor) id)
  (declare (ignore id))
  (object-value self))

(defmethod editor-get-time-sequence ((self timeline-editor) id)
  (editor-get-time-sequence (container-editor self) id))

(defmethod editor-get-all-time-sequences ((self OMEditor))
  (list (object-value self)))


(defmethod delete-editor-selection ((self t)) nil)

(defmethod editor-delete-contents-from-timeline ((self OMEditor) timeline-id objects-id)
  ;;; we suppose that the editor's selection is in sync with the timeline
  (delete-editor-selection self))

(defmethod cursor-panes ((self timeline-editor))
  (when (and (timeline-views self) (time-ruler self))
    (append (list (time-ruler self)) (timeline-views self))))

(defmethod set-cursor-time ((self timeline-editor) time)
  (mapcar #'(lambda (pane) (update-cursor pane time)) (cursor-panes self))
  (set-cursor-time (container-editor self) time)
  (set-time-display self time)
  (om-invalidate-view (time-ruler self)))

(defmethod get-cursor-time ((self timeline-editor))
  (if (time-ruler self) (cursor-pos (time-ruler self)) 0))

(defmethod update-to-editor ((self timeline-editor) (from omeditor))
  (let ((obj (editor-get-time-sequence from nil))
        (sel (selection from)))
    (when obj
      (let ((sel-points (if (find T sel)
                            (time-sequence-get-timed-item-list obj)
                          (get-points-from-indices obj sel))))
        (setf (selection self) sel-points))
      (editor-invalidate-views self))))

;; called from the time ruler
(defmethod update-to-editor ((self timeline-editor) (from time-ruler))
  (update-to-editor (container-editor self) self))

(defmethod update-to-editor ((self timeline-editor) (from t))
  (editor-invalidate-views self))


; used to update selection and value in the timeline editor
; !! works only if the editor has a slot 'timeline-editor !!
(defmethod update-timeline-editor ((self OMeditor))
  (when (and (timeline-editor self) (window self))
    (update-to-editor (timeline-editor self) self)
    t))

(defmethod update-to-editor ((editor OMEditor) (from timeline-editor))
  (let ((time-sequence (editor-get-time-sequence editor nil)))
    (when time-sequence
      (setf (selection editor) (get-indices-from-points time-sequence (selection from)))
      (time-sequence-update-internal-times time-sequence))

    ;;; we should do this only when the time-sequence is modified...
    ;;; (report-modifications editor)
    ))


(defmethod editor-invalidate-views ((self timeline-editor))
  (om-invalidate-view (get-g-component self :main-panel)))


(defmethod snap-all-points-to-grid ((self timeline-editor) id &optional (snap-delta nil))
  (let* ((obj (editor-get-time-sequence self id)))
    (loop for point in (time-sequence-get-timed-item-list obj)
          for time in (time-sequence-get-internal-times obj)
          do
          (item-set-time point (snap-time-to-grid (time-ruler self) time))
          )))

(defmethod snap-point-to-grid ((self timeline-editor) id pos &optional (snap-delta nil))
  (let* ((unit-dur (get-units (time-ruler self)))
         (delta (if snap-delta (min snap-delta (/ unit-dur 2)) (/ unit-dur 2)))
         (point (nth pos (time-sequence-get-timed-item-list (editor-get-time-sequence self id)))))
    (item-set-time point (snap-time-to-grid (time-ruler self) (item-get-time point) delta))
    ))


(defmethod play-editor-get-ruler-views ((self timeline-editor)) (time-ruler self))

(defmethod reinit-ranges ((self timeline-editor))
  (reinit-x-ranges self))


;;;==========================
;;; TIMELINE VIEW
;;;==========================

(defclass om-timeline-view (x-cursor-graduated-view OMEditorView)
  ((selected-p :accessor selected-p :initform nil)
   (label :accessor label :initform nil :initarg :label)
   (id :accessor id :initarg :id :initform -1))
  (:default-initargs :visible-min-height *timeline-view-height*))

;;;==========================
;;; selection
;;;==========================

(defmethod select-timeline ((self om-timeline-view) t-or-nil)
  (setf (selected-p self) t-or-nil)
  (when (> (length (timeline-views (editor self))) 1)
    (om-set-bg-color self
                     (if t-or-nil
                         (om-get-light-offset-color (get-color (editor-get-time-sequence (editor self) (id self))) 0.8)
                       (om-def-color :white)))
    (om-invalidate-view self))
  t-or-nil)

(defmethod get-selected-timelines ((self timeline-editor))
  (loop for tlv in (timeline-views self)
        for i = 0 then (+ i 1)
        when (selected-p tlv)
        collect i))

(defmethod set-selected-timelines ((self timeline-editor) list-of-i)
  (loop for tlv in (timeline-views self)
        for i = 0 then (+ i 1) do
        (select-timeline
         tlv
         (if (find i list-of-i :test '=) t nil))))

;;;==========================

(defmethod update-view-from-ruler ((rv x-ruler-view) (view om-timeline-view))
  (setf (x1 view) (/ (v1 rv) (expt 10 (decimals rv)))
        (x2 view) (/ (v2 rv) (expt 10 (decimals rv))))
  (set-shift-and-factor view)
  (call-next-method)
  (om-invalidate-view view))

(defmethod initialize-instance :after ((self om-timeline-view) &rest args)
  (om-set-bg-color self (om-def-color :transparent))
  (unless (label self) (setf (label self) ""))
  (start-cursor self)  ;add a cursor directly
  )

;do not remove the cursor from a timeline view
(defmethod stop-cursor ((self om-timeline-view)) nil)

(defmethod get-obj-to-play ((self timeline-editor))
  (get-obj-to-play (container-editor self)))

(defmethod get-color (self) (om-def-color :dark-gray))


(defmethod clear-timeline ((self timeline-editor))
  (let ((container-layout (get-g-component self :main-panel)))
    (when (car (om-subviews container-layout)) ;;I need this to remove the button icon. It should be removed from the layout that contains it. Ratios need to be adapted before..
      (om-set-layout-ratios (car (om-subviews (car (om-subviews container-layout)))) nil)
      (om-remove-all-subviews (car (om-subviews (car (om-subviews container-layout))))))
    (om-remove-all-subviews container-layout)
    (setf (time-ruler self) nil)))

; a view at the left of the timeline.
; should be defined and sized depending on the container editor's layout
(defmethod make-timeline-left-item ((self t) id) (om-make-view 'om-view :size (omp 15 15)))

(defmethod build-transport-view ((self OMEditor))
  (om-make-layout 'om-row-layout :subviews (list (make-time-monitor (timeline-editor self) :time 0))))

(defmethod build-options-view ((self timeline-editor))
  (let ((snap-to-grid-chk (om-make-di 'om-check-box :text "Snap to Grid" :size (omp 100 24) :font (om-def-font :font1)
                                      :checked-p (snap-to-grid self)
                                      :di-action #'(lambda (item)
                                                     (setf (snap-to-grid self) (om-checked-p item)
                                                           (snap-to-grid (time-ruler self)) (om-checked-p item))
                                                     (editor-invalidate-views self)))))
    (om-make-layout 'om-row-layout :subviews (list snap-to-grid-chk))))

(defmethod build-transport-and-options-layout ((self timeline-editor))
  (let* ((transport-layout (build-transport-view (container-editor self)))
         (options-layout (build-options-view self)))
    (om-make-layout 'om-row-layout
                    :subviews
                    (list
                     (om-make-view 'om-view :direct-draw nil
                                   :subviews (list transport-layout))
                     nil
                     (om-make-view 'om-view :direct-draw nil
                                   :subviews (list options-layout )))
                    :ratios '(0.1 100 0.1)
                    )))

(defmethod make-timeline-view ((self timeline-editor))
  (let* ((container-editor (container-editor self))
         (main-panel (get-g-component self :main-panel))
         (time-ruler (om-make-view 'time-ruler  :size (omp nil 20)
                                   :unit :ms :bg-color (om-def-color :white) :bottom-p nil
                                   :snap-to-grid (snap-to-grid self)
                                   :markers-count-object-onset-p nil))
         (timeline-views nil)
         (left-item-w 0)
         (foldable-containers nil))

    (loop for obj in (editor-get-all-time-sequences container-editor)
          for i = 0 then (+ i 1) do
          (let* ((timeline-view (om-make-view 'om-timeline-view :id i :editor self :bg-color (om-def-color :white)))
                 (foldable-container (om-make-layout 'om-column-layout))
                 (timeline-item (make-timeline-left-item container-editor (id timeline-view)))

                 ;(fold-icon (om-make-graphic-object
                 ;            'om-icon-button :size (omp 10 10)
                 ;            :icon :arrow-drop-right :icon-pushed :arrow-drop-up
                 ;            :lock-push t
                 ;            :action #'(lambda (b)
                 ;                        (if (pushed b)
                 ;                            (let ((extra-views (get-timeline-foldable-views container-editor :obj obj :time-ruler time-ruler)))
                 ;                              (apply 'om-add-subviews (cons foldable-container extra-views))
                 ;                              (reinit-ranges self))
                 ;                          (progn
                 ;                            (om-remove-all-subviews foldable-container)
                 ;                            (setf (related-views time-ruler) (timeline-views self))))
                 ;                        (om-update-layout (main-view (container-editor self)))
                 ;                        (om-invalidate-view main-panel))))

                 (fold-group (om-make-layout 'om-column-layout
                                             :ratios '(1.0 0.001)
                                             :subviews
                                             (list
                                              (om-make-layout
                                               'om-row-layout
                                               :ratios '(0.001 1)
                                               :subviews
                                               (list timeline-item timeline-view)) ;; fold-icon
                                              foldable-container))))
            (setq left-item-w (om-width timeline-item))

            (setf (cursor-interval timeline-view) (play-interval (print container-editor)))

            (pushr timeline-view timeline-views)
            (pushr fold-group foldable-containers)))

    (setf (related-views time-ruler) (append timeline-views (related-views time-ruler)))

    (om-remove-all-subviews main-panel)

    (when main-panel
      (om-add-subviews
       main-panel
       (om-make-layout
        'om-column-layout
        :ratios (append '(1) (make-list (length foldable-containers) :initial-element 100) '(1))
        :subviews
        (append
         (list (build-transport-and-options-layout self))
         (append
          foldable-containers
          (list ;;; ruler
                (om-make-layout
                 'om-row-layout
                 :subviews
                 (list
                  ;;; a dummy view to take the same size as the timeline-item
                  (om-make-view 'om-view :size (omp left-item-w nil))
                  time-ruler)
                 :ratios '(0.001 1))
                )))))
      (setf (time-ruler self) time-ruler)
      (setf (timeline-views self) timeline-views)
      (reinit-ranges self)
      (om-invalidate-view main-panel)
      )))


(defclass om-timeline-subview (om-view x-graduated-view) ())

;to redefine by each subclass
(defmethod get-timeline-foldable-views ((self omeditor) &key obj time-ruler)
  (declare (ignore obj time-ruler))
  (list (om-make-view 'om-timeline-subview :bg-color (om-def-color :gold) :size (omp nil 40))))

;;;==========================
;;; Drawing
;;;==========================

(defmethod draw-timeline-background ((self OMEditor) view id) nil)

(defmethod om-draw-contents ((self om-timeline-view))
  (let* ((editor (editor self))
         (obj (editor-get-time-sequence (container-editor editor) (id self))))

    (draw-timeline-background (container-editor editor) self (id self))
    (om-with-fg-color (om-make-color 0.4 0.4 0.7 0.6)
      (when (time-ruler editor)
        (draw-grid-from-ruler self (time-ruler editor))))

    ;; x-cursor-graduate-view : draw interval etc.
    (call-next-method)

    (when obj
      (let* ((color (get-color obj))
             (x1 0)
             (x2 (om-point-x (om-view-size self)))
             (y_max (om-point-y (om-view-size self)))
             (y (/ y_max 2))
             (name (format nil "~{~a~}" (list (label self))))
             (active_pos (find-active-position-at-time obj (or (cursor-pos self) 0))))
        ;draw scale
        (om-with-fg-color (om-get-darker-color color 0.7)
          (om-draw-line x1 y x2 y)
          (om-draw-string (- x2  (+ (om-string-size name (om-get-font self)) 1)) (- y 1) name))
        ;draw children (maybe in the other order to have the correct display)
        (let ((prev-point nil))
          (loop for p in (time-sequence-get-timed-item-list obj)
                for i = 0 then (1+ i)
                do
                (let* ((cx (x-to-pix self (item-get-internal-time p))))
                  (when (and prev-point (items-merged-p prev-point p))
                    (let ((prev_x (x-to-pix self (item-get-internal-time prev-point))))
                      (om-with-alpha 0.5
                        (om-with-fg-color (om-get-lighter-color color 0.2)
                          (om-draw-rect prev_x (- y 4) (- cx prev_x ) 8 :fill t :angles :round))))
                    )
                  (om-draw-timeline-point cx y (/ *timeline-item-height* 3) color
                                          :active-p (= i active_pos)
                                          :selected-p (and (selected-p self)
                                                           (find p (selection editor)))
                                          :type (item-get-type p)
                                          :time (item-get-time p)))
                (setf prev-point p)
                ))
        ;draw ticks if interpolation selected
        (when (and (number-? (interpol obj)) (time-ruler editor))
          ;;; sometimes this method is called before the time-ruler is even created (not good)
          (loop for val from (max (v1 (time-ruler editor)) (get-first-time obj))
                to (min (get-obj-dur obj) (v2 (time-ruler editor)))
                by (number-number (interpol obj))
                do
                (let ((x-val (x-to-pix self val)))
                  (om-with-fg-color (om-get-darker-color color 0.1)
                    (om-draw-line x-val (- y 2) x-val (+ y 2))))))
        )
      )))


(defmethod om-draw-timeline-point (cx cy radius col &key active-p selected-p type time)
  (let ((light-col (om-get-lighter-color col 0.2)))
    (when (equal type :master)
      (om-with-fg-color (om-make-color 0.3 0.3 1 0.9)
        (om-draw-line cx 0 cx 50)
        (om-draw-circle cx cy (+ 2 radius) :fill nil)))
    ;draw circle
    (when active-p
        ;selected highlight
      (om-with-fg-color col
        (om-draw-circle cx cy (+ radius 1) :fill t)))
    (om-with-fg-color (if selected-p (om-def-color :dark-red) light-col)
      (om-draw-circle cx cy (if selected-p (1+ radius) radius) :fill (if time t nil)))))


;;;==========================
;;; Time-marker methods
;;; to redefine by subclasses
;;;==========================

(defmethod get-timed-objects-with-markers ((self om-timeline-view))
  (list (editor-get-time-sequence (container-editor (editor self)) (id self))))

(defmethod select-elements-at-time ((self om-timeline-view) marker-time)
  ;selects the elements with same time than the marker-time
  (let* ((editor (editor self))
         (obj (editor-get-time-sequence editor (id self))))
    (let ((p (point-exists-at-time obj marker-time)))
      (when p
        (select-timeline self t)
        (if (not (find p (selection editor)))
            (setf (selection editor) (append (list p) (selection editor)))
          )))
    (update-to-editor editor self)))

(defmethod clear-editor-selection ((self timeline-editor))
  (set-selection self nil)
  (set-selected-timelines self nil))

;;;==========================
;;; Events methods
;;;==========================

(defmethod timed-item-index-at-time ((self timeline-editor) (panel om-timeline-view) time)
  (let* ((obj (editor-get-time-sequence self (id panel)))
         (times (time-sequence-get-internal-times obj)))
    (if (> (length times) 0)
        (let* ((delta (/ *timeline-item-height* 2))
               (timepos (x-to-pix panel time))
               (pos (find-position-at-time obj time))
               (t1 (nth (if (<= pos 0) 0 (1- pos)) times))
               (pos1 (x-to-pix panel t1))
               (t2 (or (nth pos times) t1))
               (pos2 (x-to-pix panel t2)))
          (if (and (> pos 0) (< (- timepos pos1) delta))
              (1- pos)
            (if (and (< pos (length times)) (< (- pos2 timepos) delta))
                pos
              nil)))
      nil)))

(defmethod timed-item-at-time ((self timeline-editor) (panel om-timeline-view) time)
  (let* ((obj (editor-get-time-sequence self (id panel)))
         (pos (timed-item-index-at-time self panel time)))
    (if pos
        (nth pos (time-sequence-get-timed-item-list obj))
      nil)))

(defmethod order-points-by-time ((self timeline-editor))
  (loop for tlv in (timeline-views self) do
        (let ((obj (editor-get-time-sequence self (id tlv))))
          (time-sequence-reorder-timed-item-list obj))))

(defmethod add-point-at-time ((self timeline-editor) time id)
  (let* ((obj (editor-get-time-sequence self id))
         (point (time-sequence-make-timed-item-at obj (round time))))
    (time-sequence-insert-timed-item-and-update obj point)))

(defmethod translate-selection ((self timeline-editor) dt)
  (when (selection self)
    (loop for tlv in (timeline-views self) do
          (let* ((obj (editor-get-time-sequence self (id tlv)))
                 (points (filter-points-for-obj obj (selection self))))
            (when points
              (temporal-translate-points obj points dt))))
    (order-points-by-time self)))

(defmethod set-selection-as-master ((self timeline-editor) id)
  (when (selection self)
    (let ((obj (editor-get-time-sequence self id)))
      (cond ((find T (selection self))
             (set-all-points-as-master obj))
            (t
             (loop for point in (selection self) do
                   (item-set-type point (if (eql :master (item-get-type point)) (item-get-time point) :master)))
             (update-time-types-from-tpoint-list obj))))))

(defmethod get-selected-indices-for-view ((self om-timeline-view))
  (let ((obj (editor-get-time-sequence (editor self) (id self))))
    (get-indices-from-points obj (selection (editor self)))))

(defmethod get-selected-points-for-view ((self om-timeline-view))
  (let ((obj (editor-get-time-sequence (editor self) (id self))))
    (filter-points-for-obj obj (selection (editor self)))))

(defmethod get-selection-as-views-indices-list ((self timeline-editor))
  ;return a list with nb of views elements each as follows: (view-id (indices0 indices1 ... indicesn))
  (loop for tlv in (timeline-views self)
        collect
        (list (id tlv) (get-selected-indices-for-view tlv))))

(defmethod get-obj-for-point ((self timeline-editor) point)
  ;returns the object containg the point
  (loop for tlv in (timeline-views self)
        when (position point (time-sequence-get-timed-item-list (editor-get-time-sequence self (id tlv))))
        return (editor-get-time-sequence self (id tlv))))

(defmethod move-time-point-action ((view om-timeline-view) editor orig-point position)
  (let* ((time (pix-to-x view (om-point-x position))))
    (om-init-temp-graphics-motion
     view position nil :min-move 4
     :motion #'(lambda (view pos)
                 (let* ((tmp_time (pixel-to-time view (om-point-x pos)))
                        (dt (round (- tmp_time time)))
                        (selected-point-time (item-get-internal-time orig-point)))
                   (set-time-display editor tmp_time)
                   (when (selection editor)
                     (let* ((new-dt  (if (snap-to-grid editor)
                                         (adapt-dt-for-grid-and-markers (time-ruler editor) selected-point-time dt) dt)))
                       (when (not (equal new-dt 0))
                         (setf time (+ time new-dt))
                         (translate-selection editor new-dt)
                         (set-cursor-time editor  (item-get-internal-time orig-point))
                         ))
                     (om-invalidate-view view)
                     (when (equal :master (item-get-type orig-point))
                       (om-invalidate-view (time-ruler editor)))
                     (update-to-editor (container-editor editor) editor)
                     (report-modifications (container-editor editor))
                     ))))))


;;;==========================
;;; Events
;;;==========================

(defmethod alllow-insert-point-from-timeline ((self OMEditor)) t)

(defmethod om-view-click-handler ((self om-timeline-view) position)

  (let* ((timeline-editor (editor self))
         (time (pix-to-x self (om-point-x position)))
         (point (timed-item-at-time timeline-editor self time)))
    ;add a point if add key down and point not existing
    (when (and (om-add-key-down) (alllow-insert-point-from-timeline (container-editor timeline-editor)) (not point))
      (let ((pos (add-point-at-time timeline-editor time (id self)))
            (obj (editor-get-time-sequence timeline-editor (id self))))
        (setf point (get-nth-point obj pos))))
    ;timelines views selection
    (cond
     ((om-shift-key-p)
      (let ((sel-for-view (get-selected-indices-for-view self)))
        (if (and (selected-p self) (not sel-for-view) (not point))
            (select-timeline self nil)
          (select-timeline self t))))
     (t
      (set-selected-timelines timeline-editor (list (id self)))))

    (set-selection timeline-editor point)

     ;point selection
    (if point
        (progn
          (move-time-point-action self timeline-editor point position)
          (set-cursor-time timeline-editor (or (and point (item-get-time point)) time))
          (update-to-editor (container-editor timeline-editor) timeline-editor))
      (or (call-next-method)
          (progn
            (set-cursor-time timeline-editor (or (and point (item-get-time point)) time))
            (drag-move-cursor self position))))
    (set-time-display timeline-editor (if point (item-get-time point) time))
    point))


(defmethod editor-key-action ((editor timeline-editor) key)
  (case key
    (:om-key-delete
     (mapcar
      #'(lambda (timeline-id)
          (editor-delete-contents-from-timeline (container-editor editor) timeline-id (selection editor))
          (om-invalidate-view (nth timeline-id (timeline-views editor))))
      (get-selected-timelines editor))
     (setf (selection editor) nil)
     (update-to-editor (container-editor editor) editor)
     t)
    (#\u (mapcar #'(lambda (timeline-id)
                     (snap-all-points-to-grid editor timeline-id))
                 (get-selected-timelines editor))
         (update-to-editor (container-editor editor) editor)
         t)
    (#\m  (mapcar #'(lambda (timeline-id)
                      (set-selection-as-master editor timeline-id)
                      (om-invalidate-view (nth timeline-id (timeline-views editor)))
                      )
                  (get-selected-timelines editor))
          (update-to-editor (container-editor editor) editor)
          (om-invalidate-view (time-ruler editor))
          t)
    (otherwise nil)
    ))

(defmethod om-view-key-handler ((self om-timeline-view) key)
  (or (editor-key-action (editor self) key)
      (call-next-method)) ;;; => to window and play-editor-mixin
  )

(defmethod editor-play ((self timeline-editor))
  (editor-play (container-editor self)))

(defmethod editor-pause ((self timeline-editor))
  (editor-pause (container-editor self)))

(defmethod editor-stop ((self timeline-editor))
  (editor-stop (container-editor self)))

(defmethod editor-set-interval ((self timeline-editor) interval)
  (editor-set-interval (container-editor self) interval))
