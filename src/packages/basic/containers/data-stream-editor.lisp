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
;;; EDITOR
;;;======================================

(defclass data-stream-editor (OMEditor play-editor-mixin undoable-editor-mixin multi-display-editor-mixin)
  ((timeline-editor :accessor timeline-editor :initform nil)))

(defmethod object-default-edition-params ((self data-stream))
  '((:display-mode :blocks)
    (:grid t)))

(defclass stream-panel (x-cursor-graduated-view y-graduated-view OMEditorView om-tt-view)
  ((stream-id :accessor stream-id :initform 0 :initarg :stream-id)
   (left-view :accessor left-view :initform nil :initarg :left-view)))

(defmethod editor-view-class ((self data-stream-editor)) 'stream-panel)
(defmethod object-has-editor ((self internal-data-stream)) t)
(defmethod get-editor-class ((self internal-data-stream)) 'data-stream-editor)
(defmethod get-obj-to-play ((self data-stream-editor)) (object-value self))

(defmethod get-object-slots-for-undo ((self internal-data-stream)) '(frames))

(defmethod alllow-insert-point-from-timeline ((self data-stream-editor)) nil)


;;; from play-editor-mixin
(defmethod cursor-panes ((self data-stream-editor))
  ;(append (get-g-component self :data-panel-list)
  (cons (active-panel self)
        (and (timeline-editor self)
             (cursor-panes (timeline-editor self)))))

(defmethod active-panel ((editor data-stream-editor))
  (let ((n (or (and (multi-display-p editor)
                    (position (object-value editor) (multi-obj-list editor)))
               0)))
    (nth n (get-g-component editor :data-panel-list))))

(defmethod editor-window-init-size ((self data-stream-editor)) (om-make-point 650 200))

(defmethod frame-display-modes-for-object ((self data-stream-editor) (object t)) '(:blocks :bubbles))

(defmethod make-editor-controls ((editor data-stream-editor))
  (let ((object (object-value editor)))
    (when (> (length (frame-display-modes-for-object editor object)) 1)
      (om-make-di 'om-popup-list :size (omp 80 24) :font (om-def-font :font1)
                  :items (frame-display-modes-for-object editor object)
                  :di-action #'(lambda (item)
                                 (editor-set-edit-param editor :display-mode (om-get-selected-item item))
                                 (mapc 'om-invalidate-view (get-g-component editor :data-panel-list)))
                  :value (editor-get-edit-param editor :display-mode)
                  ))))

(defmethod editor-with-timeline ((self data-stream-editor)) t)

(defun make-timeline-check-box (editor)
  (om-make-di 'om-check-box :text "timeline" :size (omp 65 24) :font (om-def-font :font1)
              :checked-p (editor-get-edit-param editor :show-timeline)
              :enabled (timeline-editor editor)
              :di-action #'(lambda (item)
                             (let ((timeline-ed (timeline-editor editor)))
                               (editor-set-edit-param editor :show-timeline (om-checked-p item))
                               (clear-timeline timeline-ed)
                               (when (om-checked-p item) (make-timeline-view timeline-ed))
                               (om-update-layout (main-view editor))))
              ))


(defun make-control-bar (editor)

  (let ((mousepostext (om-make-graphic-object 'om-item-text :size (omp 60 16))))

    (set-g-component editor :mousepos-txt mousepostext)

    (om-make-layout
     'om-row-layout
     :ratios '(1 100 1 1 1 1 1)
     :subviews (list mousepostext
                     nil
                     (make-time-monitor editor)
                     (make-play-button editor :enable t)
                     (make-repeat-button editor :enable t)
                     (make-pause-button editor :enable t)
                     (make-stop-button editor :enable t)
                     ))
    ))


(defmethod init-editor ((editor data-stream-editor))
  (call-next-method)
  (when (editor-with-timeline editor)
    (setf (timeline-editor editor)
          (make-instance 'timeline-editor
                         :object (object editor)
                         :container-editor editor))
    ))

;;; sets the editor slightly longer that the actual object length
(defmethod editor-view-after-init-space ((self t)) 1000)

;;; the small view at the left of teh timeline should be sized according to the editor's layout
(defmethod make-timeline-left-item ((self data-stream-editor) id)
  (om-make-view 'om-view :size (omp 28 15)))

(defmethod make-left-panel-for-object ((editor data-stream-editor) (object t))
  (om-make-view 'om-view :size (omp 28 nil)))



(defmethod data-stream-get-x-ruler-vmin ((self data-stream-editor)) 0)


;;; voice editor has a different ruler
(defmethod make-time-ruler ((editor data-stream-editor) dur)

  (om-make-view 'time-ruler
                :related-views (get-g-component editor :data-panel-list)
                :size (omp nil 20)
                :bg-color (om-def-color :white)
                :vmin (data-stream-get-x-ruler-vmin editor)
                :x1 (data-stream-get-x-ruler-vmin editor)
                :x2 dur)
  )


(defmethod editor-scroll-v ((self data-stream-editor)) nil)

(defmethod make-editor-window-contents ((editor data-stream-editor))

  (let* ((data-stream (object-value editor))
         (object-s (if (multi-display-p editor)
                       (multi-obj-list editor)
                     (list data-stream)))
         (n-objs (length object-s))
         (max-dur (loop for obj in object-s maximize (or (get-obj-dur obj) 0)))
         (ed-dur (if (zerop max-dur)
                     10000
                   (+ max-dur (editor-view-after-init-space data-stream)))))

    (set-g-component editor :data-panel-list
                     (loop for d-s in object-s
                           for i = 0 then (+ i 1) collect
                           (om-make-view (editor-view-class editor) :stream-id i
                                         :editor editor
                                         :size (omp 50 nil)
                                         :direct-draw t
                                         :bg-color (om-def-color :white)
                                         ;; internal vertical scroller
                                         :scrollbars (editor-scroll-v editor)
                                         :left-view (make-left-panel-for-object editor d-s)
                                         )))

    (set-g-component editor :x-ruler (make-time-ruler editor ed-dur))
    (set-g-component editor :main-panel (car (get-g-component editor :data-panel-list)))

    (when (timeline-editor editor)
      (set-g-component (timeline-editor editor) :main-panel (om-make-layout 'om-row-layout))

      (when (editor-get-edit-param editor :show-timeline)
        (make-timeline-view (timeline-editor editor))))

    (om-make-layout
     'om-column-layout
     :ratios '(96 2 2)
     :subviews (list
                ;;; first group with the 'main' editor:
                (om-make-layout
                 'om-simple-layout
                 ;;; if this is a multi-object editor with 3+ objects, add vertical scroller
                 :scrollbars (if (> (length (get-g-component editor :data-panel-list)) 2) :v nil)
                 :subviews
                 (list
                  (om-make-layout
                   'om-grid-layout
                   :delta 0
                   :ratios `((nil 100)
                             ,(append '(0.01)
                                      (make-list n-objs :initial-element (/ 0.98 n-objs))
                                      '(0.01)))
                   :subviews
                   (append (list nil (make-control-bar editor))
                           (loop for view in (get-g-component editor :data-panel-list)
                                 append (list (left-view view) view))
                           (list nil (get-g-component editor :x-ruler)))
                   )
                  ))

                ;;; the timeline editor:
                (when (timeline-editor editor)
                  (get-g-component (timeline-editor editor) :main-panel))

                ;;; the bottom control bar:
                (om-make-layout 'om-row-layout
                                :size (omp nil 40)
                                :subviews (list (make-editor-controls editor) nil
                                                (and (editor-with-timeline editor)
                                                     (make-timeline-check-box editor))))
                ))
    ))


(defmethod om-view-scrolled ((self stream-panel) pos)
  (om-set-scroll-position (left-view self) (omp 0 (cadr pos))))


;===== MultiDisplay API

(defmethod enable-multi-display ((editor data-stream-editor) obj-list)
  (call-next-method)
  (when (container-editor editor)
    (om-substitute-subviews
     (main-view (container-editor editor))
     (main-view editor)
     (setf (main-view editor)
           (make-editor-window-contents editor)))
    (init-editor-window editor)
    ))

(defmethod disable-multi-display ((editor data-stream-editor))
  (call-next-method)
  (when (container-editor editor)
    (om-substitute-subviews
     (main-view (container-editor editor))
     (main-view editor)
     (setf (main-view editor)
           (make-editor-window-contents editor)))
    (init-editor-window editor)
    ))

;======================

(defmethod init-editor-window ((editor data-stream-editor))
  (call-next-method)
  (when (get-g-component editor :x-ruler)
    (update-views-from-ruler (get-g-component editor :x-ruler)))
  (when (object-value editor)
    (loop for view in (get-g-component editor :data-panel-list) do
          (setf (y2 view) (car (y-range-for-object (object-value editor)))
                (y1 view) (cadr (y-range-for-object (object-value editor))))
          (set-shift-and-factor view))
    ))

(defmethod update-to-editor ((editor data-stream-editor) (from ombox))
  (let* ((data-stream (object-value editor)))
    (when data-stream
      (let ((new-max-dur (if (zerop (get-obj-dur data-stream))
                             10000
                           (+ (get-obj-dur data-stream) (editor-view-after-init-space data-stream)))))

        (when (get-g-component editor :x-ruler)
          (setf (vmax (get-g-component editor :x-ruler)) new-max-dur)
          (set-ruler-range
           (get-g-component editor :x-ruler)
           (v1 (get-g-component editor :x-ruler))
           new-max-dur))
        (mapc 'om-invalidate-view (get-g-component editor :data-panel-list))

        (when (and (timeline-editor editor)
                   (editor-get-edit-param editor :show-timeline))
          (update-to-editor (timeline-editor editor) editor))

        (call-next-method)
        ))))

(defmethod update-to-editor ((editor data-stream-editor) (from t))
  (call-next-method)
  (mapc 'om-invalidate-view (get-g-component editor :data-panel-list)))


(defmethod editor-invalidate-views ((self data-stream-editor))
  (call-next-method)
  (mapc 'om-invalidate-view (get-g-component self :data-panel-list))
  (when (timeline-editor self)
    (editor-invalidate-views (timeline-editor self))))


;;; todo : factorize this in different editors..
(defmethod editor-delete-contents-from-timeline ((self data-stream-editor) timeline-id sel)
  (let ((data-stream (object-value self)))
    (mapcar #'(lambda (point) (remove-timed-point-from-time-sequence data-stream point)) sel)
    (time-sequence-update-internal-times data-stream))
  (editor-invalidate-views self)
  (report-modifications self))


;;; called when resetting the x-rulers
(defmethod play-editor-get-ruler-views ((self data-stream-editor))
  (get-g-component self :x-ruler))


;; lesser value and greater values in the ruler (bottom to top)
(defmethod y-range-for-object ((self internal-data-stream)) '(0 100))


;;;===========================================
;;; FRAMES DRAW & SELECTION
;;;===========================================
;;; attributes are no intrinsec data of the frame
;;; they should calculated depending on the frame data and according to a given mode of display

(defmethod get-frame-graphic-duration ((self data-frame))
  (if (zerop (item-get-duration self)) 200 (item-get-duration self)))

(defmethod get-frame-color ((self data-frame))
  (or (getf (attributes self) :color)
      (setf (getf (attributes self) :color) (om-random-color 0.4))))

;; random !
;; compare values to y-range-for-object
(defmethod get-frame-posy ((self data-frame))
  (or (getf (attributes self) :posy)
      (setf (getf (attributes self) :posy) (om-random 30 90))))

;; arbitrary !
(defmethod get-frame-sizey ((self data-frame))
  (max 10 (* 1.2 (data-size self))))

(defmethod finalize-data-frame ((f data-frame) &rest args) nil)

;; returns (x y w h)
(defmethod get-frame-area ((frame data-frame) editor)
  (let ((panel (active-panel editor))
        (sizey (get-frame-sizey frame))
        (posy (get-frame-posy frame)))
    (case (editor-get-edit-param editor :display-mode)
      (:bubbles (values
                 (- (x-to-pix panel (or (date frame) 0)) (dy-to-dpix panel (/ sizey 2)))
                 (- (h panel) (y-to-pix panel (- posy (/ sizey 2))))
                 (dy-to-dpix panel sizey)
                 (dy-to-dpix panel sizey)
                 ))
      (otherwise (values (x-to-pix panel (date frame))
                         (- (h panel) (y-to-pix panel posy))
                         (max 3 (dx-to-dpix panel (get-frame-graphic-duration frame)))
                         (max 3 (dy-to-dpix panel sizey))  ;; !! downwards
                         )))))

(defmethod draw ((frame data-frame) x y w h selected) nil)

(defmethod draw-data-frame ((frame data-frame) editor i &optional (active t))
  (multiple-value-bind (x y w h)
      (get-frame-area frame editor)
    (om-with-fg-color (get-frame-color frame)
      (or (draw frame x y w h (and active (find i (selection editor))))
          (case (editor-get-edit-param editor :display-mode)
            (:bubbles
             ;(om-draw-rect x y w h :fill nil)
             (let ((r (abs (min w h))))
               (om-draw-circle (+ x (round r 2))
                               (- y (round r 2))
                               (round r 2) :fill t)
               (when (and active (find i (selection editor)))
                 (om-draw-circle (+ x (round r 2))
                                 (- y (round r 2))
                                 (round r 2) :fill t
                                 :color (om-make-color .5 .5 .5 .5)))
               ))
            (otherwise
             (om-draw-rect x y w h :fill t)
             (when (and active (find i (selection editor)))
               (om-draw-rect x y w h :fill t
                             :color (om-make-color .5 .5 .5 .5))))
            ))
      )))

(defmethod frame-at-pos ((editor data-stream-editor) position)
  (let ((frames (data-stream-get-frames (object-value editor))))
    (when frames
      (position-if #'(lambda (f)
                       (multiple-value-bind (x y w h)
                           (get-frame-area f editor)
                         (om-point-in-rect-p position x y w h)))
                   frames))))


(defmethod frames-in-area ((editor data-stream-editor) p1 p2)
  (loop for f in (data-stream-get-frames (object-value editor))
        for i = 0 then (1+ i)
        when (multiple-value-bind (x y w h)
                 (get-frame-area f editor)
               (rect-intersection
                (om-point-x p1) (om-point-y p1) (om-point-x p2) (om-point-y p2)
                x y (+ x w) (+ y h)))
        collect i))

;;;=======================
;;; EDITOR FUNCTIONS
;;;=======================

(defmethod draw-background ((editor data-stream-editor) (view stream-panel)) nil)

(defmethod om-draw-contents ((self stream-panel))
  (let* ((editor (editor self))
         (stream (if (multi-display-p editor)
                     (nth (stream-id self) (multi-obj-list editor))
                   (object-value editor)))
         (active (if (multi-display-p editor)
                     (equal self (active-panel editor))
                   t)))

    (when active (draw-background editor self))

    (when (editor-get-edit-param editor :grid)
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-line '(2 2)
          (draw-grid-from-ruler self (get-g-component editor :x-ruler)))))

    (when stream
      (om-with-fg-color (om-def-color :dark-gray)
        (loop for frame in (data-stream-get-frames stream)
              for i = 0 then (1+ i) do
              (draw-data-frame frame editor i active)))
      )
    ))

(defmethod om-draw-contents :after ((self stream-panel))
  (when (and (multi-display-p (editor self))
             (equal self (active-panel (editor self))))
    (om-draw-rect 0 0 (w self) (h self) :fill nil
                  :line 4 :color (om-make-color .8 .4 .4))
    ))


(defmethod position-display ((editor data-stream-editor) pos-pix)
  (when (active-panel editor)
    (let* ((time (round (pix-to-x (active-panel editor) (om-point-x pos-pix)))))
      (om-set-text (get-g-component editor :mousepos-txt) (format nil "~Dms" time))))
  )


(defmethod move-editor-selection ((self data-stream-editor) &key (dx 0) (dy 0))
  (declare (ignore dy)) ;; in a basic data-frame -- subclasses can do it !
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-stream-get-frames (object-value self)))))
          (item-set-time frame (max 0 (round (+ (item-get-time frame) dx))))
          )))

(defmethod resize-editor-selection ((self data-stream-editor) &key (dx 0) (dy 0))
  (declare (ignore dy)) ;; in a basic data-frame -- subclasses can do it !
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-stream-get-frames (object-value self)))))
          (item-set-duration frame (max 0 (round (+ (item-get-duration frame) dx))))
          )))


;;; sort the frames and reset the selection indices correctly
(defmethod editor-sort-frames ((self data-stream-editor))
  (let* ((stream (object-value self))
         (selected-objects (loop for pos in (selection self) collect
                                 (nth pos (data-stream-get-frames stream)))))

    (time-sequence-reorder-timed-item-list stream)

    (setf (selection self)
          (loop for selected in selected-objects
                collect (position selected (data-stream-get-frames stream))))))


(defmethod resizable-frame ((self data-frame)) nil)

(defmethod om-view-mouse-motion-handler ((self stream-panel) position)
  (let ((editor (editor self)))

    (when (object-value editor)

      ;;; show the mouse position on screen
      (position-display editor position)

      (unless (or (equal (editor-play-state editor) :play)
                  (and (multi-display-p editor) (not (equal self (active-panel editor)))))

        (om-hide-tooltip self)

        (let ((frames (data-stream-get-frames (object-value editor)))
              (fp (frame-at-pos editor position)))
          (when fp
            (if (om-command-key-p)
                ;;; show tooltip for the frame under the  mouse cursor
                (om-show-tooltip self (data-frame-text-description (nth fp frames))
                                 (omp (- (om-point-x position) 60) 20)
                                 0)

              ;;; show reisize cursor if by the end of a resizable-frame
              (let ((mouse-x (om-point-x position))
                    (frame-end-time-x (time-to-pixel self (item-end-time (nth fp frames)))))
                (if (and (<= mouse-x frame-end-time-x) (>= mouse-x (- frame-end-time-x 5)))
                    (om-set-view-cursor self (om-get-cursor :h-size))
                  (om-set-view-cursor self nil)))
              ))
          )))))


(defmethod om-view-click-handler :around ((self stream-panel) position)
  (let ((editor (editor self)))
    (when (and (container-editor editor)
               (not (equal self (active-panel editor))))
      (set-current-nth (container-editor editor) (stream-id self)))
    (call-next-method)
    ))


(defmethod om-view-click-handler ((self stream-panel) position)

  (let ((editor (editor self)))

    (unless (handle-selection-extent self position) ;; => play-editor-mixin handles cursor etc.

      (when (object-value editor)

        (let ((p0 position)
              (selection (frame-at-pos editor position)))

          (set-selection editor selection)
          (update-timeline-editor editor)
          (om-invalidate-view self)

          (cond

           ((and (null selection) (om-add-key-down))
            (let ((frame (time-sequence-make-timed-item-at (object-value editor) (pixel-to-time self (om-point-x p0)))))
              (finalize-data-frame frame :posy (pix-to-y self (- (h self) (om-point-y p0))))
              (time-sequence-insert-timed-item-and-update (object-value editor) frame)
              (report-modifications editor)
              (update-timeline-editor editor)
              (om-invalidate-view self)))

           (selection
            (let* ((selected-frame (nth selection (data-stream-get-frames (object-value editor))))
                   (selected-frame-end-t (time-to-pixel self (item-end-time selected-frame))))

              ;;; resize the selected frame ?
              (if (and (resizable-frame selected-frame)
                       (<= (om-point-x position) selected-frame-end-t) (>= (om-point-x position) (- selected-frame-end-t 5)))
                  (om-init-temp-graphics-motion
                   self position nil
                   :motion #'(lambda (view pos)
                               (declare (ignore view))
                               (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0)))))
                                 (when (> (- (om-point-x pos) (x-to-pix self (item-get-time selected-frame))) 10)
                                   (resize-editor-selection editor :dx (round dx))
                                   (setf p0 pos)
                                   (om-invalidate-view self))))
                   :release #'(lambda (view pos)
                                (declare (ignore view pos))
                                (report-modifications editor)
                                (om-invalidate-view self))
                   :min-move 4)

                ;;; move the selection
                (om-init-temp-graphics-motion
                 self position nil
                 :motion #'(lambda (view pos)
                             (declare (ignore view))
                             (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                                   (dy (dpix-to-dy self (- (om-point-y pos) (om-point-y p0)))))
                               (move-editor-selection editor :dx dx :dy dy)
                               (setf p0 pos)
                               (position-display editor pos)
                               (update-timeline-editor editor)
                               (om-invalidate-view self)
                               ))
                 :release #'(lambda (view pos)
                              (declare (ignore view pos))
                              (let ((selected-frames (posn-match (data-stream-get-frames (object-value editor)) (selection editor))))
                                (editor-sort-frames editor)
                                (move-editor-selection editor :dy :round)
                                (time-sequence-reorder-timed-item-list (object-value editor))
                                (update-timeline-editor editor)
                                ;;; reset the selection:
                                (set-selection editor
                                               (loop for f in selected-frames collect
                                                     (position f (data-stream-get-frames (object-value editor)))))
                                )
                              (report-modifications editor)
                              (om-invalidate-view self))
                 :min-move 4)
                ))
            )
           (t
            ;; no selection: start selection lasso
            (om-init-temp-graphics-motion
             self position
             (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
             :min-move 10
             :release #'(lambda (view position)
                          (declare (ignore view))
                          (setf (selection editor) (frames-in-area editor p0 position))
                          (update-timeline-editor editor)
                          (om-invalidate-view self))
             )
            ))
          )))))


(defmethod editor-key-action ((editor data-stream-editor) key)
  (let* ((panel (active-panel editor))
         (stream (object-value editor)))
    (case key
      (:om-key-delete
       (when (selection editor)
         (store-current-state-for-undo editor)
         (loop for pos in (sort (selection editor) '>) do
               (remove-nth-timed-point-from-time-sequence stream pos))
         (setf (selection editor) nil)
         (om-invalidate-view panel)
         (update-timeline-editor editor)
         ;; (time-sequence-update-obj-dur stream)
         (report-modifications editor)))
      (:om-key-esc
       ;; maybe not needed ? we already have the db-click on ruler for that...
       ;; (reinit-x-ranges editor)
       )
      (:om-key-left
       (store-current-state-for-undo editor)
       (move-editor-selection editor :dx (- (get-units (get-g-component editor :x-ruler) (if (om-shift-key-p) 100 10))))
       (editor-sort-frames editor)
       (time-sequence-update-internal-times stream)
       (om-invalidate-view panel)
       (update-timeline-editor editor)
       (report-modifications editor)
       )
      (:om-key-right
       (store-current-state-for-undo editor)
       (move-editor-selection editor :dx (get-units (get-g-component editor :x-ruler) (if (om-shift-key-p) 100 10)))
       (editor-sort-frames editor)
       (time-sequence-update-internal-times stream)
       (om-invalidate-view panel)
       (update-timeline-editor editor)
       (report-modifications editor))
      (:om-key-tab
       (setf (selection editor)
             (if (selection editor)
                 (list (next-element-in-editor editor (car (selection editor))))
               (list (first-element-in-editor editor))))
       (editor-invalidate-views editor))
      (otherwise (call-next-method))
      )))


;;; in data-strea-editor the selection is an index to elements in the frame sequence
;;; this is not necessarilythe case of editor subclasses
(defmethod first-element-in-editor ((editor data-stream-editor))
  (and (data-stream-get-frames (object-value editor)) 0))

(defmethod next-element-in-editor ((editor data-stream-editor) (element number))
  (let ((seq (object-value editor)))
    (and (data-stream-get-frames seq)
         (mod (1+ element) (length (data-stream-get-frames seq))))))

(defmethod next-element-in-editor ((editor data-stream-editor) (element t)) nil)


(defmethod select-all-command ((self data-stream-editor))
  #'(lambda ()
      (set-selection self
                     (loop for i from 0 to (1- (length (data-stream-get-frames (object-value self))))
                           collect i))
      (update-timeline-editor self)
      (editor-invalidate-views self)
      ))


;;;==================================
;;; TURN PAGES / FOLLOW PLAY POSITION
;;;==================================

(defmethod play-editor-callback ((editor data-stream-editor) time)
  (call-next-method)
  (let ((panel (get-g-component editor :main-panel))
        (x-ruler (get-g-component editor :x-ruler)))

    (when (and panel x-ruler)
      (let ((x-range (round (- (x2 panel) (x1 panel)))))

        (cond ((> time (x2 panel))
               (set-ruler-range x-ruler (+ (v1 x-ruler) x-range) (+ (v2 x-ruler) x-range)))
              ((< time (x1 panel))
               (set-ruler-range x-ruler time (+ time x-range)))
              (t nil))))
    ))


;;;=========================
;;; TOUCH GESTURES
;;;=========================

(defmethod om-view-pan-handler ((self stream-panel) position dx dy)
  (let ((fact 10))
    (move-rulers self :dx (* fact dx) :dy (* fact dy))))


(defmethod om-view-zoom-handler ((self stream-panel) position zoom)
  (zoom-rulers self :dx (- 1 zoom) :dy 0 :center position))


(defmethod move-rulers ((self stream-panel) &key (dx 0) (dy 0))
  (declare (ignore dy)) ;; no y-ruler
  (shift-time-ruler (get-g-component (editor self) :x-ruler) dx))

;;; no y-ruler : zoom just in x
(defmethod zoom-rulers ((panel stream-panel) &key (dx 0.1) (dy 0.1) center)
  (declare (ignore dy)) ;; no y-ruler
  (let ((x-ruler (get-g-component (editor panel) :x-ruler)))
    (when (and x-ruler (ruler-zoom-? x-ruler))
      (zoom-time-ruler x-ruler dx center panel))))

