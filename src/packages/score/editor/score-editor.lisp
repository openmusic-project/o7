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

;;;===========================================
;;; SCORE EDITORS (GENERAL/SHARED FEATURES)
;;;===========================================

(defclass score-editor (OMEditor undoable-editor-mixin)
  ((editor-window-config :accessor editor-window-config :initarg :editor-window-config :initform nil)))

;;; these params are shared between the editor and the box
(defmethod object-default-edition-params ((self score-element))
  '((:font-size 24)
    (:staff :g)
    (:scale :scale-1/2)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    (:h-stretch 1)
    (:y-shift 4)))


;;; Note: y-shift is a value or a list containing the space (in units) above the staff. (see get-total-y-shift)
;;; It is useful to control the spacing between lines (by mouse/key actions) but needs to be maintained up-to-date
;;; when voices are added/removed etc.

;;; only chord-seq-editor allows to edit time
(defmethod edit-time-? ((self score-editor)) nil)

;;;============
;;; SCORE VIEW
;;;============

;;; A specialized view for the display of SCORE-OBJECTs
(defclass score-view (OMEditorView)
  ((margin-l :accessor margin-l :initarg :margin-l :initform 1)
   (margin-r :accessor margin-r :initarg :margin-r :initform 1)
   (keys :accessor keys :initarg :keys :initform t)
   (contents :accessor contents :initarg :contents :initform t)))


;;;========================
;;; TIME/SPACE CONVERSIONS
;;;========================

(defmethod time-to-pixel ((self score-view) time)
  (let* ((ed (editor self))
         (stretch (editor-get-edit-param ed :h-stretch))
         (time-map (editor-get-edit-param ed :time-map)))

    ;(print (list ed time-map stretch))
    (if (and time-map (numberp stretch)) ;;; in principle only VOICE/POLY have a time-map

        (let ((adjusted-stretch-factor (* stretch (font-size-to-unit (editor-get-edit-param ed :font-size)))))

          (* adjusted-stretch-factor
             (-
              (score-time-to-units time-map time)
              (score-time-to-units time-map (x1 self)))))

      (let ((tt (if (listp time) (car time) time))) ;;; can happen that time is a list (see draw-measure)
        (call-next-method self tt)))))


;;; used (only?) when we click on the view to re-position the play cursor
(defmethod pixel-to-time ((self score-view) pixel)
  (let* ((ed (editor self))
         (stretch (editor-get-edit-param ed :h-stretch))
         (time-map (editor-get-edit-param ed :time-map)))

    (if (and time-map (numberp stretch))

        (let* ((unit (font-size-to-unit (editor-get-edit-param ed :font-size)))
               (v0 (round (x1 self)))
               (v0-in-units (score-time-to-units time-map v0))
               (pos-in-units (+ v0-in-units (/ pixel (* stretch unit)))))

          (score-units-to-time time-map pos-in-units))

      (call-next-method))))


;;; for simple views that are not x-graduated (e.g. chord)
(defmethod pixel-to-time ((self om-view) x) 0)


;;;========================
;;; DISPLAY
;;;========================

;;; redefined for editors with several staves...
(defmethod draw-staff-in-editor-view ((editor score-editor) (self score-view))
  (draw-staff 0 0
              (editor-get-edit-param editor :y-shift)
              (w self) (h self)
              (editor-get-edit-param editor :font-size)
              (editor-get-edit-param editor :staff)
              :margin-l (margin-l self)
              :margin-r (margin-r self)
              :keys (keys self))
  )


;;; redefined by VOICE/POLY
(defmethod draw-tempo-in-editor-view ((editor score-editor) (self score-view)) nil)


(defmethod om-draw-contents ((self score-view))

  (let* ((editor (editor self))
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size))))

    (om-trap-errors

     (draw-staff-in-editor-view editor self)

     (when (contents self)
       (draw-score-object-in-editor-view editor self unit))

     )))


;;;============
;;; INTERACTION
;;;============

(defun point-in-bbox (p bbox)
  (and (>= (om-point-x p) (b-box-x1 bbox))
       (<= (om-point-x p) (b-box-x2 bbox))
       (>= (om-point-y p) (b-box-y1 bbox))
       (<= (om-point-y p) (b-box-y2 bbox))))

(defun bbox-in-rect (bbox x1 y1 x2 y2)
  (and (<= x1 (b-box-x1 bbox))
       (>= x2 (b-box-x2 bbox))
       (<= y1 (b-box-y1 bbox))
       (>= y2 (b-box-y2 bbox))))


;;; supposing that sub-bounding-boxes are always included
(defmethod find-score-element-at-pos ((object note) pos)
  (and (b-box object)
       (point-in-bbox pos (b-box object))
       object))

;;; in measure the "selectable" bounding box does not contain the internal element's bounding boxes
(defmethod find-score-element-at-pos ((object measure) pos)
  (if (and (b-box object) (point-in-bbox pos (b-box object)))
      object
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      found)))

(defmethod find-score-element-at-pos ((object score-element) pos)
  (cond

   ((null (b-box object)) ;;; the object itself has no bounding box (yet?) or, we have clicked outside
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      found))

   ((point-in-bbox pos (b-box object)) ;; the object has one, and we're inside
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      (or found object)))

   (t NIL) ;;; not here...
   ))


(defmethod find-score-elements-in-area ((object score-element) x1 y1 x2 y2)

  (if (and (b-box object)
           (bbox-in-rect (b-box object) x1 y1 x2 y2)) ;; the object has one, and it's inside

      object ;;; the whole object is selected

    ;;; else: go check in the children
    (remove nil
            (flat
             (loop for elem in (inside object) ;; check its children..
                   collect (find-score-elements-in-area elem x1 y1 x2 y2))))
    ))


;;; measure is special: it is selected only by clickking on the bar/signature
(defmethod find-score-elements-in-area ((object measure) x1 y1 x2 y2)
  (flat
   (loop for elem in (inside object) ;; check its children..
         collect (find-score-elements-in-area elem x1 y1 x2 y2))))


;;; note: same with _all_ (not only top-level) could be useful for groups... ?

(defmethod get-tpl-elements-of-type ((self score-element) type)

  (if (find (type-of self) (list! type))

      (list self)

    (loop for element in (inside self) append
          (get-tpl-elements-of-type element type))
    ))

(defmethod get-tpl-elements-of-type ((self t) type) nil)


;;;======================
;;; MOUSE ACTIONS
;;;======================

(defmethod score-object-update ((self t)) nil)

(defmethod score-object-update ((self time-sequence))
  (time-sequence-reorder-timed-item-list self)
  (time-sequence-update-obj-dur self))


;;; overrides data-stream-panel in chord-seq-editor
(defmethod om-view-mouse-motion-handler ((self score-view) position)
  (position-display (editor self) position))


;;; redefined with objects of several voices...
(defmethod get-voice-at-pos ((self score-editor) position)
  (values (object-value self) 0))
(defmethod get-all-voices ((self score-editor))
  (list (object-value self)))

(defmethod get-selected-chords ((editor score-editor))
  (remove-duplicates
   (remove-if-not #'(lambda (elt) (typep elt 'chord))
                  (selection editor))))

;;; redefined with objects of several voices...
(defmethod get-total-y-shift ((editor score-editor) voice-num)
  (editor-get-edit-param editor :y-shift))


(defmethod om-view-click-handler ((self score-view) position)

  (let* ((editor (editor self))
         (obj (object-value editor))
         (ed-staff (editor-get-edit-param editor :staff))
         (scale (get-the-scale (editor-get-edit-param editor :scale)))
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size))))

    (set-paste-position position self) ;;; for copy/paste

    (multiple-value-bind (voice pos)
        (get-voice-at-pos editor position)

      (declare (ignore pos))

      (when voice

        (let* ((voice-pos (and (get-voices obj) (position voice (get-voices obj))))
               (staff (if (listp ed-staff) (or (and voice-pos (nth voice-pos ed-staff)) (car ed-staff)) ed-staff))
               (shift (+ (calculate-staff-line-shift staff) (get-total-y-shift editor voice-pos)))
               (clicked-pos position)
               (click-y-in-units (- shift (/ (om-point-y position) unit)))
               (clicked-pitch (line-to-pitch click-y-in-units scale)) ;;; <= scale here ??? )
               (clicked-time (pixel-to-time self (om-point-x position))))

          (cond
           ((om-add-key-down)  ;;; add a note
            (store-current-state-for-undo editor)

            (let ((container-chord (get-chord-from-editor-click editor position)))

              (when container-chord
                (let ((new-note (make-instance 'note :midic clicked-pitch)))

                  ;;; set to the same dur as others (important in voices)
                  (when (notes container-chord)
                    (setf (dur new-note) (list-max (ldur container-chord))))

                  (setf (notes container-chord)
                        (sort (cons new-note
                                    (notes container-chord))
                              '< :key 'midic))

                  ;;; some updates of the time-sequence required here
                  (score-object-update voice)
                  (report-modifications editor)
                  (editor-invalidate-views editor)

                  (if (or (= 1 (length (notes container-chord)))
                          (find container-chord (selection editor)))
                      (setf (selection editor) (list container-chord))
                    (setf (selection editor) (list new-note)))

                  (om-init-temp-graphics-motion
                   self position nil :min-move 1
                   :motion #'(lambda (view pos)
                               (declare (ignore view))
                               (let* ((new-y-in-units (- shift (/ (om-point-y pos) unit)))
                                      (new-pitch (line-to-pitch new-y-in-units scale))
                                      (diff (- new-pitch clicked-pitch)))
                                 (unless (zerop diff)
                                   (store-current-state-for-undo editor :action :move :item (selection editor))
                                   (loop for n in (get-notes (selection editor)) do
                                         (setf (midic n) (+ (midic n) diff)))
                                   (setf clicked-pitch new-pitch)
                                   (om-invalidate-view self))
                                 ))
                   :release #'(lambda (view pos)
                                (declare (ignore view pos))
                                (reset-undoable-editor-action editor)
                                (report-modifications editor))
                   )
                  ))))

           ;; select
           (t (let ((selection (find-score-element-at-pos obj position)))
                (set-selection editor selection)
                (editor-invalidate-views editor)

                ;;; move the selection or select rectangle...

                (if selection

                    ;;; move it
                    (let ((modif nil))
                      (om-init-temp-graphics-motion
                       self position nil
                       :min-move 1
                       :motion #'(lambda (view pos)
                                   (declare (ignore view))

                                   (let ((x-move (- (om-point-x pos) (om-point-x clicked-pos)))
                                         (y-move (- (om-point-y pos) (om-point-y clicked-pos))))

                                     (if (and (edit-time-? editor)
                                              (> (abs x-move) (abs y-move)))

                                         (let* ((new-time (pixel-to-time self (om-point-x pos)))
                                                (diff (- new-time clicked-time)))
                                           (unless (zerop diff)
                                             (setf modif t)
                                             (store-current-state-for-undo editor :action :move :item (selection editor))
                                             ;;; remove-duplicates: continuation chords refer to existing notes !
                                             (loop for c in (get-selected-chords editor)
                                                   do (when (>= (+ (item-get-time c) diff) 0)
                                                        (item-set-time c (+ (item-get-time c) diff))))
                                             (setf clicked-time new-time)
                                             (editor-invalidate-views editor)
                                             )
                                           )

                                       (let* ((new-y-in-units (- shift (/ (om-point-y pos) unit)))
                                              (new-pitch (line-to-pitch new-y-in-units scale))
                                              (diff (- new-pitch clicked-pitch)))
                                         (unless (zerop diff)
                                           (setf modif t)
                                           (store-current-state-for-undo editor :action :move :item (selection editor))
                                           ;;; remove-duplicates: continuation chords refer to existing notes !
                                           (loop for n in (remove-duplicates (get-notes (selection editor)))
                                                 do (setf (midic n) (+ (midic n) diff)))
                                           (setf clicked-pitch new-pitch)
                                           (editor-invalidate-views editor)
                                           )))
                                     (setf clicked-pos pos)
                                     ))

                       :release #'(lambda (view pos)
                                    (declare (ignore view pos))
                                    (when modif
                                      (score-object-update voice)
                                      (reset-undoable-editor-action editor)
                                      (report-modifications editor)))
                       ))

                  ;;; no selection: start selection-rectangle
                  (om-init-temp-graphics-motion
                   self position
                   (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
                   :min-move 10
                   :release #'(lambda (view end-pos)
                                (let ((x1 (min (om-point-x position) (om-point-x end-pos)))
                                      (x2 (max (om-point-x position) (om-point-x end-pos)))
                                      (y1 (min (om-point-y position) (om-point-y end-pos)))
                                      (y2 (max (om-point-y position) (om-point-y end-pos))))

                                  (set-selection editor (find-score-elements-in-area obj x1 y1 x2 y2))
                                  (om-invalidate-view view)
                                  ))
                   )
                  )
                ))
           )
          )))))


(defmethod om-view-zoom-handler ((self score-view) position zoom)
  (let ((editor (editor self))
        (d-size (if (> zoom 1) 1 -1)))
    (set-font-size editor (+ d-size (editor-get-edit-param editor :font-size)))
    ))

;;;======================
;;; KEYBOARD ACTIONS
;;;======================

;;; to be specialized for specific types of editor/objects
(defmethod score-editor-delete ((self score-editor) element) nil)
(defmethod score-editor-change-selection-durs ((self score-editor) delta) nil)

(defmethod score-editor-handle-voice-selection ((self score-editor) direction) nil)
(defmethod score-editor-update-params-before-remove ((self score-editor) removed) nil)


(defmethod editor-key-action ((editor score-editor) key)

  (case key

    (:om-key-left
     (if (om-option-key-p)
         (progn
           (store-current-state-for-undo editor)
           (score-editor-change-selection-durs editor (if (om-shift-key-p) -1000 -100))
           (editor-invalidate-views editor)
           (report-modifications editor))
       (call-next-method)))

    (:om-key-right
     (if (om-option-key-p)
         (progn
           (store-current-state-for-undo editor)
           (score-editor-change-selection-durs editor (if (om-shift-key-p) 1000 100))
           (editor-invalidate-views editor)
           (report-modifications editor))
       (call-next-method)))

    ;;; if not with "option", left/right for chord-seq are handled in data-stream-editor only

    (:om-key-up
     (store-current-state-for-undo editor)
     (score-editor-handle-voice-selection editor -1)
     (move-editor-selection editor :dy (if (om-shift-key-p) 12 1))
     (editor-invalidate-views editor)
     (report-modifications editor))

    (:om-key-down
     (store-current-state-for-undo editor)
     (score-editor-handle-voice-selection editor 1)
     (move-editor-selection editor :dy (if (om-shift-key-p) -12 -1))
     (editor-invalidate-views editor)
     (report-modifications editor))

    (:om-key-delete
     (delete-selection editor))

    (otherwise
     (call-next-method))
    ))


;;; also called bu cut-command:
(defmethod delete-selection ((editor score-editor))
  (when (selection editor)
    (store-current-state-for-undo editor)
    (loop for element in (selection editor) do
          (score-editor-update-params-before-remove editor element)
          (score-editor-delete editor element)
          )
    (setf (selection editor) nil)
    (editor-invalidate-views editor)
    (report-modifications editor)))


;;;======================
;;; CONTROL PANEL
;;;======================

(defmethod set-font-size ((self score-editor) size)
  (let ((v (min 120 (max 8 size))))
    (editor-set-edit-param self :font-size v)
    (when (get-g-component self :font-size-box)
      (om-set-selected-item (get-g-component self :font-size-box) v))))


;;; required for after some changes or edit-param modifs
;;; (add/remove voice, change staff, etc..)
;;; redefined in multi-editors
(defmethod set-interior-size-from-contents ((self score-editor)) nil)

(defmethod make-score-display-params-controls ((editor score-editor))

  (let* ((text-h 16)
         (control-h 24)
         (title (om-make-di 'om-simple-text :text "Editor params"
                            :size (omp 100 22) ;; :bg-color (om-def-color :red)
                            :font (om-def-font :font2b)))

         (size-item
          (om-make-layout
           'om-row-layout
           :align :center
           :subviews (list
                      (om-make-di 'om-simple-text :text "size"
                                  :font (om-def-font :font1)
                                  :size (omp 30 text-h))

                      (set-g-component
                       editor :font-size-box
                       (om-make-di 'om-popup-list :items *score-fontsize-options*
                                   :size (omp 55 control-h) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :font-size)
                                   :di-action #'(lambda (list)
                                                  (set-font-size editor (om-get-selected-item list))
                                                  ))))
           ))

         (staff-item
          (om-make-layout
           'om-row-layout
           :align :center
           :subviews (list
                      (om-make-di 'om-simple-text :text "staff"
                                  :size (omp 35 text-h)
                                  :font (om-def-font :font1))
                      (set-g-component
                       editor :staff-menu  ;; we need to change it sometimes...
                       (om-make-di 'om-popup-list :items *score-staff-options*
                                   :size (omp 100 control-h) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :staff)
                                   :di-action #'(lambda (list)
                                                  (editor-set-edit-param editor :staff (om-get-selected-item list))
                                                  (set-interior-size-from-contents editor)
                                                  (report-modifications editor) ;; update the box display
                                                  ))))))

         (scale-item
          (om-make-layout
           'om-row-layout
           :align :center
           :subviews (list
                      (om-make-di 'om-simple-text :text "scale"
                                  :size (omp 35 text-h)
                                  :font (om-def-font :font1))
                      (om-make-di 'om-popup-list :items (mapcar #'car *all-scales*)
                                  :size (omp 100 control-h) :font (om-def-font :font1)
                                  :value (editor-get-edit-param editor :scale)
                                  :di-action #'(lambda (list)
                                                 (editor-set-edit-param editor :scale (om-get-selected-item list))
                                                 (report-modifications editor) ;; update the box display
                                                 )))))

         (duration-item
          (om-make-layout
           'om-row-layout
           :align :center
           :subviews (list
                      (om-make-di 'om-simple-text :text "durations"
                                  :size (omp 56 text-h)
                                  :font (om-def-font :font1))
                      (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                                  :size (omp 20 control-h)
                                  :checked-p (editor-get-edit-param editor :duration-display)
                                  :di-action #'(lambda (item)
                                                 (editor-set-edit-param editor :duration-display (om-checked-p item))))
                      )))

         (grid-numbox
          (om-make-graphic-object
           'numbox
           :position (omp 0 0)
           :value (editor-get-edit-param editor :grid-step)
           :enabled (editor-get-edit-param editor :grid)
           :bg-color (om-def-color :white)
           :db-click t
           :decimals 0
           :size (om-make-point 40 text-h)
           :font (om-def-font :font1)
           :min-val 10
           :max-val 10000
           :after-fun #'(lambda (item)
                          (editor-set-edit-param editor :grid-step (value item))
                          (editor-invalidate-views editor))))

         (grid-item
          (om-make-layout
           'om-row-layout
           :align :center
           :delta nil
           :subviews (list (om-make-di 'om-simple-text :text "grid"
                                       :font (om-def-font :font1)
                                       :size (omp 56 text-h))
                           (om-make-di 'om-check-box
                                       :checked-p (editor-get-edit-param editor :grid)
                                       :text ""
                                       :size (om-make-point 16 control-h)
                                       :di-action #'(lambda (item)
                                                      (enable-numbox grid-numbox (om-checked-p item))
                                                      (editor-set-edit-param editor :grid (om-checked-p item))
                                                      (editor-invalidate-views editor)
                                                      ))
                           (om-make-layout 'om-simple-layout
                                           :align :bottom
                                           :subviews (list grid-numbox)))
           ))

         ;;; end LET declarations
         )

    ;;; all in a simple layout fixes refresh/redisplay
    ;;; problems on windows...
    (om-make-layout
     'om-simple-layout
     :subviews (list
                (om-make-layout
                 'om-grid-layout
                 :dimensions '(3 2)
                 :delta '(10 0)
                 :subviews (list
                            title
                            scale-item
                            duration-item
                            size-item
                            staff-item
                            (when (find :grid (object-default-edition-params (object-value editor)) :key #'car)
                              ;;; not all objects have a grid...
                              grid-item)
                            ))
                ))
    ))


(defmethod score-editor-set-window-config ((self score-editor) mode)
  (unless (equal (editor-window-config self) mode)
    (setf (editor-window-config self) mode)
    (let (x1 x2)
      (when (get-g-component self :x-ruler)
        (setf x1 (v1 (get-g-component self :x-ruler))
              x2 (v2 (get-g-component self :x-ruler))))
      (build-editor-window self)
      (init-editor-window self)
      (when (get-g-component self :x-ruler)
        (set-ruler-range (get-g-component self :x-ruler) x1 x2))
      (update-score-inspector self t)
      )))


;;;======================
;;; MENUS
;;;======================

(defun score-edit-menu-items (self)
  (list (om-make-menu-comp
         (list (om-make-menu-item "Undo" #'(lambda () (when (undo-command self) (funcall (undo-command self))))
                                  :key "z" :enabled #'(lambda () (and (undo-command self) t)))
               (om-make-menu-item "Redo" #'(lambda () (when (redo-command self) (funcall (redo-command self))))
                                  :key "Z" :enabled #'(lambda () (and (redo-command self) t)))))
        (om-make-menu-comp
         (list (om-make-menu-item "Copy" #'(lambda () (funcall (copy-command self)))
                                  :key "c" :enabled #'(lambda () (and (copy-command self) t)))
               (om-make-menu-item "Cut" #'(lambda () (funcall (cut-command self)))
                                  :key "x" :enabled #'(lambda () (and (cut-command self) t)))
               (om-make-menu-item "Paste"#'(lambda () (funcall (paste-command self)))
                                  :key "v" :enabled #'(lambda () (and (paste-command self) t)))
               (om-make-menu-item "Delete" #'(lambda () (funcall (clear-command self)))
                                  :enabled (and (clear-command self) t))))
        (om-make-menu-comp
         (list (om-make-menu-item "Select All" #'(lambda () (funcall (select-all-command self)))
                                  :key "a" :enabled #'(lambda () (and (select-all-command self) t)))))

        (om-make-menu-comp
         (list (om-make-menu-item "Align Chords [Shift+A]" #'(lambda () (funcall (align-command self)))
                                  :enabled #'(lambda () (and (align-command self) t)))))

        (om-make-menu-item
         "Show Inspector"
         #'(lambda () (score-editor-set-window-config
                       self
                       (if (equal (editor-window-config self) :inspector) nil :inspector)))
         :selected #'(lambda () (equal (editor-window-config self) :inspector))
         :key "i"
         )
        ))


(defmethod import-menus ((self score-editor)) nil)
(defmethod export-menus ((self score-editor)) nil)

(defmethod import-export-menu-items ((self score-editor))
  (list
   (om-make-menu-comp
    (list (om-make-menu "Import..." (import-menus self) :enabled (import-menus self))
          (om-make-menu "Export..." (export-menus self) :enabled (export-menus self)))
    )))


(defmethod om-menu-items ((self score-editor))
  (remove nil
          (list
           (main-app-menu-item)
           (om-make-menu "File" (append (default-file-menu-items self) (import-export-menu-items self)))
           (om-make-menu "Edit" (score-edit-menu-items self))
           (om-make-menu "Windows" (default-windows-menu-items self))
           (om-make-menu "Help" (default-help-menu-items self))
           )))


(defmethod select-all-command ((self score-editor))
  #'(lambda ()
      (set-selection self (get-tpl-elements-of-type (object-value self) '(chord r-rest)))
      (editor-invalidate-views self)
      ))


(defmethod copy-command ((self score-editor))
  (when (selection self)
    #'(lambda ()
        (set-om-clipboard (mapcar #'om-copy (selection self))))))


(defmethod cut-command ((self score-editor))
  (when (selection self)
    #'(lambda ()
        (set-om-clipboard (mapcar #'om-copy (selection self)))
        (delete-selection self))))


(defmethod paste-command ((self score-editor))
  (when (get-om-clipboard)
    #'(lambda ()
        (score-editor-paste self (get-om-clipboard))
        (editor-invalidate-views self)
        )))


;;; different behaviours for the different editors...
(defmethod score-editor-paste ((self t) elements) nil)


;;; PASTE FROM SCORE IN PATCH

(defmethod paste-command-for-view :around ((editor patch-editor) (view patch-editor-view))

  (let ((clipboard (get-om-clipboard)))

    (if (find-if #'(lambda (elt) (subtypep (type-of elt) 'score-element)) clipboard)

        (let ((object
               (cond

                ((list-subtypep clipboard 'measure)
                 ;;; make a voice boix with selected measures
                 (let ((obj (make-instance 'voice :tree nil)))
                   (insert-in-voice obj (reverse clipboard) 0)
                   obj))

                ((= 1 (length clipboard))
                 ;;; make a box with the only object in the clipboard
                 (om-copy (car clipboard)))

                ((list-subtypep clipboard 'voice)
                 ;;; make a multi-seq with the objects in the clipboard
                 (make-instance 'poly :obj-list (mapcar #'om-copy clipboard)))

                ((list-subtypep clipboard 'chord-seq)
                 ;;; make a multi-seq with the objects in the clipboard
                 (make-instance 'multi-seq :obj-list (mapcar #'om-copy clipboard)))

                (t
                 ;;; make a chord-seq from chords
                 (let ((chords (sort (loop for item in clipboard append (get-chords item)) #'< :key #'onset)))
                   (when chords
                     (let ((t0 (onset (car chords))))
                       (loop for c in chords do
                             (item-set-time c (- (item-get-time c) t0)))
                       (let ((obj (make-instance 'chord-seq)))
                         (set-chords obj chords)
                         obj)))))
                )))

          (if object

              (let ((paste-pos (or (get-paste-position view)
                                   (om-make-point (round (w view) 2) (- (h view) 60)))))
                (output-value-as-new-box object view paste-pos)
                (set-paste-position (om-add-points paste-pos (omp 20 20)) view))

            (om-beep))
          )

      (call-next-method))
    ))


;;; only works in chord-seq-editor
(defmethod align-command ((self score-editor)) nil)


;;;=====================================
;;; INSPECTOR / EDIT VALUES IN SCORE
;;;=====================================

(defclass score-inspector-view (inspector-view) ())

(defmethod make-editor-window-contents ((editor score-editor))

  (set-g-component editor :inspector nil)

  (let ((main-editor-view (call-next-method)))

    (if (editor-window-config editor)

        (let ((inspector-panel (make-instance 'score-inspector-view)))
          (set-g-component editor :inspector inspector-panel)
          (om-make-layout
           'om-row-layout
           :ratios '(9 nil 1)
           :subviews (list main-editor-view
                           :separator
                           inspector-panel
                           )))

      (om-make-layout
       'om-row-layout
       :ratios '(1 nil)
       :subviews (list main-editor-view
                       (om-make-layout
                        'om-column-layout
                        :subviews (list
                                   nil ;;; space at the top
                                   (om-make-view 'om-view :size (omp 16 16)
                                                 :subviews (list
                                                            (om-make-graphic-object
                                                             'om-icon-button :size (omp 16 16) :position (omp 0 0)
                                                             :icon :info-gray :icon-disabled :info
                                                             :lock-push nil :enabled (not (equal (editor-window-config editor) :inspector))
                                                             :action #'(lambda (b)
                                                                         (declare (ignore b))
                                                                         (score-editor-set-window-config
                                                                          editor
                                                                          (if (equal (editor-window-config editor) :inspector) nil :inspector)))
                                                             )
                                                            ))
                                   nil ;;; space at the bottom
                                   ))
                       ))
      )))


(defmethod update-score-inspector ((editor score-editor) &optional force)
  (let ((inspector (get-g-component editor :inspector)))
    (when (and inspector
               (or force (not (equal (selection editor) (object inspector)))))  ;;; this equal test shoudl work if these are two lists with the same thing
      (set-inspector-contents inspector (selection editor))
      )))


(defmethod report-modifications ((self score-editor))
  (call-next-method)
  (update-score-inspector self t))

(defmethod set-selection ((editor score-editor) (new-selection t))
  (call-next-method)
  (update-score-inspector editor))


;;; forbidden in voicee/poly editors
(defmethod note-dur-edit-allowed ((self score-editor)) t)


;;; in principle elt is always a list (the editor selection)
(defmethod set-inspector-contents ((self score-inspector-view) object)

  (setf (object self) object)
  (om-remove-all-subviews self)

  (let* ((editor (editor self))
         (notes (loop for elt in object append (get-tpl-elements-of-type elt 'note)))
         (measures (loop for elt in object append (get-tpl-elements-of-type elt 'measure)))
         (voices (loop for elt in object append (get-tpl-elements-of-type elt 'voice)))
         (numbox-h 16) (text-h 16)

         (close-button
          ;;; "close" button at the top-right...
          (om-make-layout
           'om-row-layout
           :ratios '(100 1)
           :subviews
           (list nil
                 (om-make-graphic-object
                  'om-icon-button :icon :xx :icon-pushed :xx-pushed
                  :size (omp 12 12)
                  :action #'(lambda (b)
                              (declare (ignore b))
                              (score-editor-set-window-config editor nil))
                  )
                 )))

         (notes-layout
          (om-make-layout
           'om-column-layout
           :subviews
           (list
            (om-make-di 'om-simple-text :size (om-make-point 120 20)
                        :text "Selected note(s)"
                        :font (om-def-font :font1b))
            (if notes

                (om-make-layout
                 'om-grid-layout
                 :dimensions '(2 6)
                 :delta '(4 0)
                 :subviews
                 (list

                  (om-make-di 'om-simple-text :size (om-make-point 50 text-h)
                              :text "midic"
                              :font (om-def-font :font1))

                  (om-make-view
                   'om-view :size (omp 40 numbox-h)
                   :subviews (list
                              (om-make-graphic-object
                               'numbox
                               :size (omp 40 numbox-h) :position (omp 0 0)
                               :bg-color (om-def-color :white)
                               :fg-color (if (all-equal (mapcar #'midic notes)) (om-def-color :black) (om-def-color :gray))
                               :db-click t
                               :min-val 0 :max-val 20000
                               :value (slot-value (car notes) 'midic)
                               :after-fun #'(lambda (item)
                                              (store-current-state-for-undo editor)
                                              (loop for n in notes do
                                                    (setf (slot-value n 'midic) (value item)))
                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor))
                               )))

                  (om-make-di 'om-simple-text :size (om-make-point 50 text-h)
                              :text "vel"
                              :font (om-def-font :font1))

                  (om-make-view
                   'om-view :size (omp 40 numbox-h)
                   :subviews (list
                              (om-make-graphic-object
                               'numbox
                               :size (omp 40 numbox-h) :position (omp 0 0)
                               :bg-color (om-def-color :white)
                               :fg-color (if (all-equal (mapcar #'vel notes)) (om-def-color :black) (om-def-color :gray))
                               :db-click t
                               :min-val 0 :max-val 127
                               :value (slot-value (car notes) 'vel)
                               :after-fun #'(lambda (item)
                                              (store-current-state-for-undo editor)
                                              (loop for n in notes do
                                                    (setf (slot-value n 'vel) (value item)))
                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor))
                               )))

                  (om-make-di 'om-simple-text :size (om-make-point 50 text-h)
                              :text "dur"
                              :font (om-def-font :font1))

                  (om-make-view
                   'om-view :size (omp 40 numbox-h)
                   :subviews (list
                              (om-make-graphic-object
                               'numbox
                               :size (omp 40 numbox-h) :position (omp 0 0)
                               :bg-color (om-def-color :white)
                               :db-click t
                               :enabled (note-dur-edit-allowed editor)
                               :fg-color (if (and (all-equal (mapcar #'dur notes))
                                                  (note-dur-edit-allowed editor))
                                             (om-def-color :black) (om-def-color :gray))
                               :min-val 0 :max-val 10000
                               :value (slot-value (car notes) 'dur)
                               :after-fun #'(lambda (item)
                                              (store-current-state-for-undo editor)
                                              (loop for n in notes do
                                                    (setf (slot-value n 'dur) (value item)))
                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor))
                               )))

                  (om-make-di 'om-simple-text :size (om-make-point 50 text-h)
                              :text "offset"
                              :font (om-def-font :font1))

                  (om-make-view
                   'om-view :size (omp 40 numbox-h)
                   :subviews (list
                              (om-make-graphic-object
                               'numbox
                               :size (omp 40 numbox-h) :position (omp 0 0)
                               :bg-color (om-def-color :white)
                               :enabled (note-dur-edit-allowed editor)
                               :fg-color (if (and (all-equal (mapcar #'dur notes))
                                                  (note-dur-edit-allowed editor))
                                             (om-def-color :black) (om-def-color :gray))
                               :db-click t
                               :min-val -20000 :max-val 20000
                               :value (slot-value (car notes) 'offset)
                               :after-fun #'(lambda (item)
                                              (store-current-state-for-undo editor)
                                              (loop for n in notes do
                                                    (setf (slot-value n 'offset) (value item)))
                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor))
                               )))


                  (om-make-di 'om-simple-text :size (om-make-point 50 text-h)
                              :text "chan"
                              :font (om-def-font :font1))

                  (om-make-view
                   'om-view :size (omp 40 numbox-h)
                   :subviews (list
                              (om-make-graphic-object
                               'numbox
                               :size (omp 40 numbox-h) :position (omp 0 0)
                               :bg-color (om-def-color :white)
                               :fg-color (if (all-equal (mapcar #'chan notes)) (om-def-color :black) (om-def-color :gray))
                               :db-click t
                               :min-val 1 :max-val 16
                               :value (slot-value (car notes) 'chan)
                               :after-fun #'(lambda (item)
                                              (store-current-state-for-undo editor)
                                              (loop for n in notes do
                                                    (setf (slot-value n 'chan) (value item)))
                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor))
                               )))

                  (om-make-di 'om-simple-text :size (om-make-point 50 text-h)
                              :text "port"
                              :font (om-def-font :font1))

                  (om-make-view
                   'om-view :size (omp 40 numbox-h)
                   :subviews (list
                              (om-make-graphic-object
                               'numbox
                               :size (omp 40 numbox-h) :position (omp 0 0)
                               :bg-color (om-def-color :white)
                               :fg-color (if (all-equal (mapcar #'port notes)) (om-def-color :black) (om-def-color :gray))
                               :db-click t
                               :allow-nil -1
                               :min-val -1 :max-val 1000
                               :value (slot-value (car notes) 'port)
                               :after-fun #'(lambda (item)
                                              (store-current-state-for-undo editor)
                                              (loop for n in notes do
                                                    (setf (slot-value n 'port) (value item)))
                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor))
                               )))
                  ))

              ;;; else (no notes)
              (om-make-di 'om-simple-text :size (om-make-point 100 text-h)
                          :text "--"
                          :fg-color (om-def-color :dark-gray)
                          :focus t  ;; prevents focus on other items :)
                          :font (om-def-font :font3)))
            ))
          )

         (measures-layout
          (when measures
            (om-make-layout
             'om-column-layout
             :subviews
             (list
              (om-make-di 'om-simple-text :size (om-make-point 120 20)
                          :text "Selected measures(s)"
                          :font (om-def-font :font1b))

              (om-make-layout
               'om-row-layout
               :subviews
               (list
                (om-make-di 'om-simple-text :size (om-make-point 80 text-h)
                            :text "time signature:"
                            :font (om-def-font :font1))

                (om-make-view
                 'om-view :size (omp 25 numbox-h)
                 :subviews (list
                            (om-make-graphic-object
                             'numbox
                             :size (omp 25 numbox-h)
                             :bg-color (om-def-color :white)
                             :fg-color (if (all-equal (mapcar #'(lambda (m) (car (car (tree m)))) measures))
                                           (om-def-color :black) (om-def-color :gray))
                             :db-click t
                             :min-val 1 :max-val 120
                             :value (car (car (tree (car measures))))
                             :after-fun #'(lambda (item)
                                            (let ((temp-selection nil))

                                              (store-current-state-for-undo editor)

                                              (loop for m in measures do
                                                    (let ((container-voice (find-if
                                                                            #'(lambda (v) (find m (inside v)))
                                                                            (get-all-voices editor))))
                                                      (when container-voice
                                                        (let ((pos (position m (inside container-voice)))
                                                              (new-tree (list (list (value item) (cadr (car (tree m))))
                                                                              (cadr (tree m)))))
                                                          (push (list container-voice pos) temp-selection)
                                                          (setf (nth pos (cadr (tree container-voice))) new-tree)
                                                          ))))

                                              (loop for v in (remove-duplicates (mapcar #'car temp-selection)) do
                                                    (build-voice-from-tree v))

                                              ;;; restore selected measures (they have be rebuilt)
                                              (setf (selection editor)
                                                    (loop for elt in temp-selection
                                                          collect (nth (cadr elt) (inside (car elt)))))

                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor)))
                             )))

                (om-make-view
                 'om-view :size (omp 25 numbox-h)
                 :subviews (list
                            (om-make-graphic-object
                             'numbox
                             :size (omp 25 numbox-h)
                             :bg-color (om-def-color :white)
                             :fg-color (if (all-equal (mapcar #'(lambda (m) (cadr (car (tree m)))) measures))
                                           (om-def-color :black) (om-def-color :gray))
                             :db-click t
                             :min-val 1 :max-val 120
                             :value (cadr (car (tree (car measures))))
                             :after-fun #'(lambda (item)
                                            (let ((temp-selection nil))

                                              (store-current-state-for-undo editor)

                                              (loop for m in measures do
                                                    (let ((container-voice (find-if
                                                                            #'(lambda (v) (find m (inside v)))
                                                                            (get-all-voices editor))))
                                                      (when container-voice
                                                        (let ((pos (position m (inside container-voice)))
                                                              (new-tree (list (list (car (car (tree m))) (value item))
                                                                              (cadr (tree m)))))
                                                          (push (list container-voice pos) temp-selection)
                                                          (setf (nth pos (cadr (tree container-voice))) new-tree)
                                                          ))))

                                              (loop for v in (remove-duplicates (mapcar #'car temp-selection)) do
                                                    (build-voice-from-tree v))

                                              ;;; restore selected measures (they have be rebuilt)
                                              (setf (selection editor)
                                                    (loop for elt in temp-selection collect (nth (cadr elt) (inside (car elt)))))

                                              (update-from-editor (object editor))
                                              (editor-invalidate-views editor)))
                             )))
                ))
              )))
          )

         (voices-layout
          (when voices
            (om-make-layout
             'om-column-layout
             :subviews
             (list
              (om-make-di 'om-simple-text :size (om-make-point 120 20)
                          :text "Selected voice(s)"
                          :font (om-def-font :font1b))

              (om-make-layout
               'om-row-layout
               :subviews
               (list
                (om-make-di 'om-simple-text :size (om-make-point 80 text-h)
                            :text "tempo:"
                            :font (om-def-font :font1))

                (om-make-view
                 'om-view :size (omp 30 numbox-h)
                 :subviews (list
                            (om-make-graphic-object
                             'numbox
                             :size (omp 30 numbox-h)
                             :bg-color (om-def-color :white)
                             :fg-color (if (all-equal (mapcar #'tempo voices))
                                           (om-def-color :black) (om-def-color :gray))
                             :db-click t
                             :min-val 1 :max-val 600
                             :value (tempo (car voices))
                             :after-fun #'(lambda (item)
                                            (store-current-state-for-undo editor)
                                            (loop for v in voices do
                                                  (setf (tempo v) (value item))
                                                  (set-timing-from-tempo (chords v) (tempo v))
                                                  )
                                            (update-from-editor (object editor))
                                            (editor-invalidate-views editor))
                             )))
                )))
             )))

         (display-aparams-text-w 72)
         (display-aparams-menu-w 76)
         (display-params-layout
          (om-make-layout
           'om-column-layout
           :delta 1
           :subviews
           (list
            (om-make-di 'om-simple-text :size (om-make-point 120 20)
                        :text "Display params"
                        :font (om-def-font :font1b))

            (om-make-layout
             'om-row-layout
             :align :center
             :subviews
             (list
              (om-make-di 'om-simple-text :text "offsets"
                          :font (om-def-font :font1)
                          :size (omp display-aparams-text-w text-h))

              (om-make-di 'om-popup-list :items '(:hidden :shift :grace-note)
                          :size (omp display-aparams-menu-w 22) :font (om-def-font :font1)
                          :value (editor-get-edit-param editor :offsets)
                          :di-action #'(lambda (list)
                                         (editor-set-edit-param editor :offsets (om-get-selected-item list)))
                          )
              ))

            (om-make-layout
             'om-row-layout
             :align :center
             :subviews
             (list
              (om-make-di 'om-simple-text :text "velocity"
                          :size (omp display-aparams-text-w text-h)
                          :font (om-def-font :font1))
              (om-make-di 'om-popup-list :items '(:hidden :value :symbol :size :alpha)
                          :size (omp display-aparams-menu-w 22) :font (om-def-font :font1)
                          :value (editor-get-edit-param editor :velocity-display)
                          :di-action #'(lambda (list)
                                         (editor-set-edit-param editor :velocity-display (om-get-selected-item list))))
              ))

            (om-make-layout
             'om-row-layout
             :align :center
             :subviews
             (list
              (om-make-di 'om-simple-text :text "MIDI channel"
                          :size (omp display-aparams-text-w text-h)
                          :font (om-def-font :font1))
              (om-make-di 'om-popup-list :items '(:hidden :number :color :color-and-number)
                          :size (omp display-aparams-menu-w 22) :font (om-def-font :font1)
                          :value (editor-get-edit-param editor :channel-display)
                          :di-action #'(lambda (list)
                                         (editor-set-edit-param editor :channel-display (om-get-selected-item list))))
              ))

            (om-make-layout
             'om-row-layout
             :align :center
             :subviews
             (list
              (om-make-di 'om-simple-text :text "MIDI port"
                          :size (omp display-aparams-text-w text-h)
                          :font (om-def-font :font1))
              (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                          :size (omp 28 20)
                          :checked-p (editor-get-edit-param editor :port-display)
                          :di-action #'(lambda (item)
                                         (editor-set-edit-param editor :port-display (om-checked-p item))))))
            )))

         ;;; end LET
         )

    (om-add-subviews self
                     (om-make-layout
                      'om-simple-layout
                      :subviews (list
                                 (om-make-layout
                                  'om-column-layout
                                  :delta 10
                                  :subviews
                                  (remove
                                   nil
                                   (list
                                    close-button
                                    notes-layout
                                    measures-layout
                                    voices-layout
                                    display-params-layout
                                    ))))))

    (when editor (om-update-layout (window editor)))

    ))
