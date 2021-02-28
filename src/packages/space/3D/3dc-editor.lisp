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
; File authors: J. Bresson, J. Garcia
;============================================================================


(in-package :om)


(defclass 3DC-editor (multi-display-editor-mixin OMEditor multi-view-editor play-editor-mixin)
  ((top-bpc-editor :accessor top-bpc-editor :initform nil)
   (front-bpc-editor :accessor front-bpc-editor :initform nil)
   (3Dp :accessor 3Dp :initform nil)
   (timeline-editor :accessor timeline-editor :initform nil)
   (timeline-enabled :accessor timeline-enabled :initarg :timeline-enabled :initform t)
   (osc-manager :accessor osc-manager :initform nil)
   (color-options :accessor color-options :initarg :color-options :initform '(:single :indices :length :speed))
   ))

(defmethod object-default-edition-params ((self 3DC))
  (append (call-next-method)
          `((:line-width 1)
            (:3D-bg-color ,(om-def-color :white)))))

(defmethod object-has-editor ((self 3dc)) t)
(defmethod get-editor-class ((self 3DC)) '3DC-editor)

(defmethod init-editor ((self 3dc-editor))
  (when (timeline-enabled self)
    (setf (timeline-editor self)
          (make-instance 'timeline-editor :object (object self) :container-editor self))
    ))

(defmethod make-3D-background-element ((self background-element)) nil)

;===== MultiDisplay API
(defmethod enable-multi-display ((self 3dc-editor) obj-list)
  (call-next-method)
  (enable-multi-display (top-bpc-editor self) obj-list)
  (enable-multi-display (front-bpc-editor self) obj-list)
  (set-3D-objects self))

(defmethod disable-multi-display ((self 3dc-editor))
  (call-next-method)
  (disable-multi-display (top-bpc-editor self))
  (disable-multi-display (front-bpc-editor self))
  (set-3D-objects self))

;=====
;;; from play-editor-mixin
(defmethod get-obj-to-play ((self 3dc-editor))
  (object-value self))

(defmethod play-editor-callback ((self 3dc-editor) time)
  (om-invalidate-view (3dp self))
  (when (top-bpc-editor self)
    (om-invalidate-view (get-g-component (top-bpc-editor self) :main-panel)))
  (when (front-bpc-editor self)
    (om-invalidate-view (get-g-component (front-bpc-editor self) :main-panel)))
  (call-next-method))

(defmethod cursor-panes ((self 3dc-editor))
  (when (timeline-editor self)
    (cursor-panes (timeline-editor self))))

(defmethod set-time-display ((self 3dc-editor) time)
  (when (timeline-editor self)
    (set-time-display (timeline-editor self) time))
  (call-next-method))

;;;==========================
;;; SYSTEM CALLBACKS
;;;==========================

(defclass 3D-axis-view (om-view) ())

(defmethod om-draw-contents ((self 3d-axis-view))
  (call-next-method)
  (om-with-fg-color (om-make-color 0.8 0.3 0.3)
    (om-draw-line 10 5 40 5)
    (om-draw-string 1 7 "x"))
  (om-with-fg-color (om-make-color 0.3 0.6 0.3)
    (om-draw-line 10 12 40 12)
    (om-draw-string 1 14 "y"))
  (om-with-fg-color (om-make-color 0.3 0.3 0.6)
    (om-draw-line 10 19 40 19)
    (om-draw-string 1 21 "z")))

(defmethod make-timeline-left-item ((self 3DC-editor) id)
  (om-make-view 'om-view :size (omp 0 15)))

(defmethod make-editor-window-contents ((editor 3DC-editor))
  (let* ((object (or (object-value editor) (make-instance '3DC))) ;;; dummy-object just in case...
         (decimals (decimals object))

         (3dPanel (om-make-view '3dpanel
                                :editor editor
                                :bg-color (editor-get-edit-param editor :3D-bg-color)
                                :g-objects (create-GL-objects editor)
                                ))

         (top-editor (make-instance 'bpc-editor :object (object editor)  :container-editor editor :decimals decimals
                                    :x-axis-key :x :y-axis-key :y
                                    :make-point-function #'(lambda (x y) (make-3Dpoint :x x :y y))))
         (front-editor (make-instance 'bpc-editor :object (object editor) :container-editor editor :decimals decimals
                                      :x-axis-key :x :y-axis-key :z
                                      :make-point-function #'(lambda (x y) (make-3Dpoint :x x :z y))))

         (top-panel (om-make-view 'BPC-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 100)
                                  :scale-fact (expt 10 decimals) :editor top-editor))
         (front-panel (om-make-view 'BPC-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 100)
                                    :scale-fact (expt 10 decimals) :editor front-editor))

         (top-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 60 16)))
         (front-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 60 16)))

         (timeline-editor (timeline-editor editor))

         (transport-area nil)
         (bottom-area nil)

         (top-layout
          (om-make-layout
           'om-grid-layout :dimensions '(2 3) :ratios '((0.01 0.99) (0.01 0.98 0.01))
           :subviews
           (list nil
                 (om-make-layout
                  'om-row-layout
                  :subviews (list (om-make-graphic-object 'om-item-text :text "Top view" :size (omp 60 18))
                                  top-mousepos-txt))
                 (setf (y-ruler top-panel)
                       (om-make-view 'y-ruler-view :related-views (list top-panel)
                                     :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
                 top-panel
                 nil
                 (setf (x-ruler top-panel)
                       (om-make-view 'x-ruler-view :related-views (list top-panel)
                                     :size (omp nil 30) :bg-color (om-def-color :white) :decimals decimals)))))
         (front-layout
          (om-make-layout
           'om-grid-layout :dimensions '(2 3)
           :ratios '((0.01 0.99) (0.01 0.98 0.01))
           :subviews
           (list
            nil
            (om-make-layout
             'om-row-layout
             :subviews (list (om-make-graphic-object 'om-item-text :text "Front view" :size (omp 60 18))
                             front-mousepos-txt))
            (setf (y-ruler front-panel)
                  (om-make-view 'y-ruler-view :related-views (list front-panel)
                                :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
            front-panel nil
            (setf (x-ruler front-panel)
                  (om-make-view 'x-ruler-view :related-views (list front-panel)
                                :size (omp nil 30) :bg-color (om-def-color :white) :decimals decimals)))))

         )

    ;bottom area
    (setf bottom-area
          (om-make-layout
           'om-row-layout :size (omp nil 40)
           :ratios '(1 1 1 10 1)
           :subviews
           (list (om-make-layout
                  'om-column-layout
                  :subviews
                  (list
                   (om-make-di 'om-simple-text :text "Options (general)"
                               :size (omp 200 22)
                               :font (om-def-font :font1b))
                   (om-make-layout
                    'om-row-layout
                    :subviews
                    (list
                     (om-make-di 'om-simple-text :text "draw mode:"
                                 :size (omp 80 22)
                                 :font (om-def-font :font1))
                     (om-make-di 'om-popup-list :items '(:default :points :lines)
                                 :size (omp 80 24) :font (om-def-font :font1)
                                 :value (editor-get-edit-param editor :draw-style)
                                 :di-action #'(lambda (list)
                                                (editor-set-edit-param editor :draw-style (om-get-selected-item list))
                                                ))))
                   (om-make-layout
                    'om-row-layout
                    :subviews
                    (list
                     (om-make-di 'om-simple-text :text "line size:"
                                 :size (omp 80 20)
                                 :font (om-def-font :font1))
                     (om-make-graphic-object 'numbox
                                             :value (editor-get-edit-param editor :line-width)
                                             :bg-color (om-def-color :white)
                                             :border t
                                             :size (om-make-point 28 18)
                                             :font (om-def-font :font1)
                                             :min-val 1 :max-val 10
                                             :after-fun #'(lambda (item)
                                                            (editor-set-edit-param editor :line-width (value item))
                                                            (set-3D-objects editor)))
                     ))
                   (when (color-options editor)
                     (om-make-layout
                      'om-row-layout
                      :subviews
                      (list
                       (om-make-di 'om-simple-text :text "color mapping:" :size (omp 80 30)
                                   :font (om-def-font :font1))
                       (om-make-di 'om-popup-list
                                   :items (color-options editor)
                                   :size (omp 80 22) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :color-style)
                                   :di-action #'(lambda (list)
                                                  (editor-set-edit-param editor :color-style
                                                                         (om-get-selected-item list))
                                                  (update-3d-curve-vertices-colors editor)
                                                  (update-3D-view editor))))
                      ))
                   ))
                 (om-make-layout
                  'om-column-layout
                  :subviews
                  (list
                   (om-make-di 'om-simple-text :text "3D view"
                               :size (omp 100 22)
                               :font (om-def-font :font1b))
                   (om-make-layout
                    'om-row-layout
                    :subviews
                    (list (om-make-di 'om-check-box :text "axes" :size (omp 46 24) :font (om-def-font :font1)
                                      :checked-p (editor-get-edit-param editor :show-axes)
                                      :di-action #'(lambda (item)
                                                     (editor-set-edit-param editor :show-axes (om-checked-p item))
                                                     (update-3D-view editor)))
                          (om-make-view '3D-axis-view :size (omp 30 24))
                          )
                    )

                   (om-make-layout
                    'om-row-layout
                    :subviews
                    (list
                     (om-make-di 'om-simple-text :text "background color"
                                 :size (omp 100 20)
                                 :font (om-def-font :font1))
                     (om-make-view 'color-view
                                   :size (om-make-point 35 16) :resizable nil
                                   :color (editor-get-edit-param editor :3D-bg-color)
                                   :after-fun #'(lambda (item)
                                                  (editor-set-edit-param editor :3D-bg-color (color item))
                                                  (om-set-bg-color (get-g-component editor :main-panel) (color item))
                                                  (update-3D-view editor)
                                                  ))
                     ))

                   (om-make-di 'om-check-box :text "background elements" :size (omp 160 24) :font (om-def-font :font1)
                               :checked-p (editor-get-edit-param editor :show-background)
                               :di-action #'(lambda (item)
                                              (editor-set-edit-param editor :show-background (om-checked-p item))
                                              (set-3D-objects editor)))

                   ; anaglyph works biut not really useful for the moment
                   ;(om-make-di 'om-check-box :text "anaglyph" :size (omp 75 24) :font (om-def-font :font1)
                   ;            :checked-p gl-user::*om-3d-anaglyph*
                   ;            :di-action #'(lambda (item)
                   ;                           (gl-user::opengl-enable-or-disable-anaglyph (om-checked-p item))
                   ;                           (update-3D-view editor)))

                   nil))

                 (om-make-layout
                  'om-column-layout
                  :subviews
                  (list
                   (om-make-di 'om-simple-text :text "2D views"
                               :size (omp 100 22)
                               :font (om-def-font :font1b))
                   (om-make-di 'om-check-box :text "indices" :size (omp 60 24) :font (om-def-font :font1)
                               :checked-p (editor-get-edit-param editor :show-indices)
                               :di-action #'(lambda (item)
                                              (editor-set-edit-param editor :show-indices (om-checked-p item))))
                   (om-make-di 'om-check-box :text "times" :size (omp 60 24) :font (om-def-font :font1)
                               :checked-p (editor-get-edit-param editor :show-times)
                               :di-action #'(lambda (item)
                                              (editor-set-edit-param editor :show-times (om-checked-p item))))
                   ))

                 nil ;; fill whitespace in the middle

                 (om-make-layout
                  'om-column-layout
                  :subviews
                  (list
                   (when timeline-editor
                     (om-make-di 'om-check-box
                                 :text "show timeline" :size (omp 100 24)
                                 :font (om-def-font :font1)
                                 :checked-p (editor-get-edit-param editor :show-timeline)
                                 :di-action #'(lambda (item)
                                                (editor-set-edit-param editor :show-timeline (om-checked-p item))
                                                (clear-timeline timeline-editor)
                                                (om-invalidate-view (get-g-component timeline-editor :main-panel))
                                                (when (om-checked-p item)
                                                  (make-timeline-view timeline-editor))
                                                (om-update-layout (main-view editor))))
                     )
                   #|
                   (om-make-layout
                    'om-row-layout
                    :subviews
                    (list
                     (om-make-di 'om-simple-text :text "Gesture offset (ms):"
                                 :font (om-def-font :font1)
                                 :size (omp 120 30))
                     (om-make-graphic-object 'numbox
                                             :value (gesture-interval-time top-editor)
                                             :min-val 0 :size (omp 40 18)
                                             :font (om-def-font :font1)
                                             :bg-color (om-def-color :white)
                                             :after-fun #'(lambda (numbox)
                                                            (setf (gesture-interval-time top-editor) (value numbox)
                                                                  (gesture-interval-time front-editor) (value numbox)))))
                    )
|#
                   ))

                 nil
                 )))


    (when (timeline-enabled editor)
      (setf transport-area (om-make-layout 'om-row-layout ;:align :center
                                           :ratios '(0.8 0.5 0.05)
                                           :subviews (list
                                                      nil
                                                      (make-time-monitor editor :time (if (action object) 0 nil))
                                                      (om-make-layout
                                                       'om-row-layout ;:size (omp 60 20)
                                                       :subviews (list (make-play-button editor :enable t)
                                                                       (make-repeat-button editor :enable t)
                                                                       (make-pause-button editor :enable t)
                                                                       (make-stop-button editor :enable t))))
                                           )))

    (set-g-component editor :main-panel 3dpanel)
    (set-g-component top-editor :main-panel top-panel)
    (set-g-component front-editor :main-panel front-panel)
    (set-g-component top-editor :mousepos-txt top-mousepos-txt)
    (set-g-component front-editor :mousepos-txt front-mousepos-txt)
    (setf (3Dp editor) 3dPanel
          (top-bpc-editor editor) top-editor
          (front-bpc-editor editor) front-editor)

    (update-3d-curve-vertices-colors editor)

    (when timeline-editor
      (set-g-component timeline-editor :main-panel (om-make-layout 'om-row-layout))
      (when (editor-get-edit-param editor :show-timeline)
        (make-timeline-view timeline-editor)))

    (om-make-layout
     'om-row-layout
     :ratios '(9.9 0.1)
     :subviews (list
                (om-make-layout
                 'om-column-layout
                 :subviews (list
                            transport-area
                            (om-make-layout
                             'om-row-layout
                             :subviews (list
                                        3dpanel
                                        :divider
                                        (om-make-layout
                                         'om-column-layout
                                         :subviews  (list
                                                     top-layout
                                                     :divider
                                                     front-layout)))
                             :ratios '(0.6 nil 0.4)
                             )
                            (when timeline-editor (get-g-component timeline-editor :main-panel))
                            bottom-area)
                 :delta 2
                 :ratios '(0.01 1 0.01 0.01))
                (call-next-method)
                )) ;side panel
    ))

(defmethod editor-close ((self 3dc-editor))
  (when (osc-manager self)
    (close-osc-manager self)
    (setf (osc-manager self) nil))
  (call-next-method))

(defmethod editor-invalidate-views ((self 3Dc-editor))

  (when (top-bpc-editor self)
    (editor-invalidate-views (top-bpc-editor self)))
  (when (front-bpc-editor self)
    (editor-invalidate-views (front-bpc-editor self)))

  (when (timeline-editor self)
    (editor-invalidate-views (timeline-editor self)))

  (update-editor-3d-object self)
  (update-3d-view self))

(defmethod editor-window-init-size ((self 3DC-editor)) (om-make-point 800 600))

(defmethod init-editor-window ((editor 3DC-editor))
  (call-next-method)
  (reinit-ranges (top-bpc-editor editor))
  (reinit-ranges (front-bpc-editor editor))
  (update-editor-3d-object editor)
  (om-init-3D-view (3Dp editor)))

; when the object is modified
(defmethod update-to-editor ((self 3DC-editor) (from t))
  (setf (selection self) nil)
  (when (window self)
    (set-3d-objects self)
    (time-sequence-update-internal-times (object-value self))
    ; (enable-play-controls self (action (object-value self))) ;;; leave T
    (update-sub-editors self)
    (update-default-view self)

    ;;; will reset the repeat-state of the new object according to the current
    ;;; repeat button state -- loop isn't stored as a proper editor property :-/
    (editor-repeat self (pushed (repeat-button self)))
    ))

; when the timeline is edited
(defmethod update-to-editor ((self 3DC-editor) (from timeline-editor))
  (setf (selection self) (get-indices-from-points (object-value self) (selection from)))
  (when (window self)
    (with-schedulable-object (object-value self)
                             (update-editor-3d-object self)
                             (time-sequence-update-internal-times (object-value self))
                             (update-3d-view self)
                             (update-sub-editors self)
                             )
    )
  (report-modifications self))


; when internal editors are edited
(defmethod update-to-editor ((self 3DC-editor) (from bpf-editor))
  (setf (selection self) (selection from))
  (update-sub-editors self)
  (when (window self)
    (with-schedulable-object (object-value self)
                             (update-editor-3d-object self)
                             (update-3d-view self)))
  (report-modifications self))


(defmethod format-3D-points  ((self 3DC))
  (mat-trans (list (x-points self) (y-points self) (z-points self) (times self))))

(defmethod create-GL-objects ((self 3DC-editor))
  (let ((obj-list (if (object-value self)
                      (cons (object-value self) (remove (object-value self) (multi-obj-list self)))
                    (multi-obj-list self))))
    (remove
     nil
     (append

      (mapcar #'(lambda (obj)
                  (make-instance
                   '3D-lines
                   :points (format-3d-points obj) :color (color obj)
                   :draw-style :default :line-width (editor-get-edit-param self :line-width))
                  )
              obj-list)

      (when (editor-get-edit-param self :show-background)
        (mapcar 'make-3D-background-element
                (editor-get-edit-param self :background)))
      ))
    ))


;;; will redraw the existing objects
(defmethod update-3D-view ((self 3DC-editor))
  (when (3DP self)
    (gl-user::clear-gl-display-list (3Dp self))
    (om-invalidate-view (3Dp self))))

;;; will rebuild the objects then redraw
;;; the gl-objets are drawn in an optimized OpenGL call-list
(defmethod set-3D-objects ((self 3dc-editor))
  (when (3Dp self)
    (om-set-gl-objects (3Dp self) (create-GL-objects self))
    (update-3d-curve-vertices-colors self)
    (update-3D-view self)))


(defmethod update-editor-3d-object ((self 3dc-editor))
  (when (3DP self)
    (let ((3d-objs (if (multi-obj-list self)
                       (first-n (om-get-gl-objects (3DP self)) (length (multi-obj-list self)))
                     (list (car (om-get-gl-objects (3DP self))))))
          (obj (object-value self)))
      (when obj
        (setf (selected-points (car 3d-objs)) (selection self))
        (loop for 3d-obj in 3d-objs
              for o in (cons obj (remove obj (multi-obj-list self))) do
              (setf (draw-style 3d-obj) (editor-get-edit-param self :draw-style))
              (om-set-3Dobj-points 3d-obj (format-3d-points o))))
      (update-3d-curve-vertices-colors self))))



(defmethod update-sub-editors ((self 3DC-editor))
  (when (window self)
    (setf (selection (top-bpc-editor self)) (selection self)
          (selection (front-bpc-editor self)) (selection self))
    (editor-invalidate-views (top-bpc-editor self))
    (editor-invalidate-views (front-bpc-editor self))
    (update-timeline-editor self)))

;;;==========================
;;; MENUS
;;;==========================

(defmethod om-menu-items ((self 3dc-editor))
  (remove nil
          (list
           (main-app-menu-item)
           (om-make-menu "File" (default-file-menu-items self))
           (om-make-menu "Edit" (bpf-edit-menu-items self))
           (om-make-menu "Windows" (default-windows-menu-items self))
           (om-make-menu "Help" (default-help-menu-items self))
           )))

(defmethod reverse-points ((self 3dc-editor))
  (time-sequence-reverse (object-value self))
  (editor-invalidate-views self)
  (update-to-editor (timeline-editor self) self)
  (report-modifications self))

(defmethod select-all-command ((self 3dc-editor))
  #'(lambda ()
      (setf (selection self) (list T))
      (update-editor-3d-object self)
      (update-sub-editors self)
      (update-3d-view self)
      ))

(defmethod open-osc-manager-command ((self 3dc-editor))
  #'(lambda () (editor-open-osc-manager self)))

;;;==========================
;;; KEY EVENTS
;;;==========================

(defmethod editor-key-action ((editor 3DC-editor) key)
  (let ((top-editor (top-bpc-editor editor))
        (front-editor (front-bpc-editor editor)))
    (when (and top-editor front-editor)
      (let ((top-panel (get-g-component top-editor :main-panel))
            (front-panel (get-g-component front-editor :main-panel))
            (selected-editor (and (selected-view editor) (editor (selected-view editor))))
            (3dpanel (3dp editor)))
        (case key
          (#\- (zoom-rulers top-panel :dx -0.1 :dy -0.1)
               (zoom-rulers front-panel :dx -0.1 :dy -0.1)
               (zoom-view 3dpanel 0.8))
          (#\+ (zoom-rulers top-panel :dx 0.1 :dy 0.1)
               (zoom-rulers front-panel :dx 0.1 :dy 0.1)
               (zoom-view 3dpanel 1.2))
          (:om-key-delete
           (delete-editor-selection editor)
           (report-modifications top-editor) ;; why ?
           (update-editor-3d-object editor)
           (update-sub-editors editor))
          (:om-key-esc
           (set-selection editor nil)
           (call-next-method) ;;; will also reset the cursor interval
           (update-editor-3d-object editor)
           (update-sub-editors editor))
          ;;; we use the internal editors to make the moves because they have ruler hints
          (:om-key-left
           (let* ((ed (or selected-editor top-editor))
                  (panel (get-g-component ed :main-panel)))
             (with-schedulable-object (object-value editor)
                                      (move-editor-selection ed :dx (/ (- (get-units (x-ruler panel) (if (om-shift-key-p) 400 40))) (scale-fact panel)))
                                      (time-sequence-update-internal-times (object-value editor)))
             (report-modifications top-editor)
             ))
          (:om-key-right
           (let* ((ed (or selected-editor top-editor))
                  (panel (get-g-component ed :main-panel)))
             (with-schedulable-object (object-value editor)
                                      (move-editor-selection ed :dx (/ (get-units (x-ruler panel) (if (om-shift-key-p) 400 40)) (scale-fact panel)))
                                      (time-sequence-update-internal-times (object-value editor)))
             (report-modifications top-editor)))
          (:om-key-up
           (let* ((ed (or selected-editor top-editor))
                  (panel (get-g-component ed :main-panel)))
             (with-schedulable-object (object-value editor)
                                      (move-editor-selection ed :dy (/ (get-units (y-ruler panel) (if (om-shift-key-p) 400 40)) (scale-fact panel)))
                                      (time-sequence-update-internal-times (object-value editor)))
             (report-modifications ed)))
          (:om-key-down
           (let* ((ed (or selected-editor top-editor))
                  (panel (get-g-component ed :main-panel)))
             (with-schedulable-object (object-value editor)
                                      (move-editor-selection ed :dy (/ (- (get-units (y-ruler panel) (if (om-shift-key-p) 400 40))) (scale-fact panel)))
                                      (time-sequence-update-internal-times (object-value editor)))
             (report-modifications ed)))
          (:om-key-pageup
           (with-schedulable-object (object-value editor)
                                    (move-editor-selection front-editor :dy (/ (get-units (y-ruler front-panel) (if (om-shift-key-p) 400 40)) (scale-fact front-panel)))
                                    (time-sequence-update-internal-times (object-value editor)))
           (report-modifications front-editor))
          (:om-key-pagedown
           (with-schedulable-object (object-value editor)
                                    (move-editor-selection front-editor :dy (/ (- (get-units (y-ruler front-panel) (if (om-shift-key-p) 400 40))) (scale-fact front-panel)))
                                    (time-sequence-update-internal-times (object-value editor)))
           (report-modifications front-editor))
          (#\Space
           (editor-stop editor)
           (player-play-object *general-player* (get-obj-to-play editor) editor))
          (otherwise (editor-key-action (top-bpc-editor editor) key)
                     (editor-key-action (front-bpc-editor editor) key))
          )))))


;;;==========================
;;; some functions from BPF/BPC-editor
;;;==========================

(defmethod delete-editor-selection ((self 3dc-editor))
  (if (find T (selection self))
      (setf (point-list (object-value self)) nil)
    (mapcar
     #'(lambda (i) (remove-nth-timed-point-from-time-sequence (object-value self) i))
     (sort (selection self) '>)
     ))
  (setf (selection self) nil)
  (update-sub-editors self))


;;;==========================
;;; 3D PANEL
;;;==========================

(defclass 3DPanel (OMEditorView om-opengl-view) ())

(defmethod om-get-bg-color ((self 3DPanel))
  (editor-get-edit-param (editor self) :3d-bg-color))

(defmethod om-get-default-extents ((self 3DPanel))
  (values -0.5 0.5 -0.5 0.5 -0.5 0.5))

(defmethod om-draw-contents ((self 3DPanel))
  (let ((editor (editor self)))

    ;(opengl:rendering-on (self)
    ;  (gl-user::polar-rotate (gl-user::icotransform self) :dz -90 :dx 90))

    (when (editor-get-edit-param editor :show-axes)
      (opengl:gl-push-matrix)
      (draw-3D-axes editor)
      (opengl:gl-pop-matrix))

    (when (and (osc-manager editor) (active-p (osc-manager editor)))
      (opengl:gl-push-matrix)
      (draw-3D-cursor-position editor)
      (opengl:gl-pop-matrix))

    (when (equal (player-get-object-state (player editor) (object-value editor)) :play)
      (opengl:gl-push-matrix)
      (draw-3D-player-cursor-position editor (player-get-object-time (player editor) (object-value editor)))
      (opengl:gl-pop-matrix))
    )
  )

(defmethod draw-3D-axes ((self 3DC-Editor))

  (let* ((l 1.0)
         (arrow-size (/ l 20.0)))
    ;X axis
    (opengl:gl-color3-f 0.8 0.3 0.3)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f -0.3 0.0 0.0)
    (opengl:gl-vertex3-f l 0.0 0.0)
    (opengl:gl-end)
    (draw-cone (list l 0.0 0.0) arrow-size 90.0 (list 0.0 1.0 0.0))

    ;Y axis
    (opengl:gl-color3-f 0.3 0.6 0.3)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f 0.0 -0.3 0.0)
    (opengl:gl-vertex3-f 0.0 l 0.0)
    (opengl:gl-end)
    (draw-cone (list 0.0 l 0.0) arrow-size -90.0 (list 1.0 0.0 0.0))

    ;Z axis
    (opengl:gl-color3-f 0.3 0.3 0.6)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f 0.0 0.0 -0.3)
    (opengl:gl-vertex3-f 0.0 0.0 l)
    (opengl:gl-end)
    (draw-cone (list 0.0 0.0 l) arrow-size 0.0 (list 0.0 1.0 0.0))
    )
  (restore-om-gl-colors-and-attributes))


(defmethod  draw-3D-cursor-position ((self 3DC-Editor))
  "Draw the OSC cursor"
  (when (osc-manager self)
    (if (equal (cursor-status (osc-manager self)) :draw)
        (opengl:gl-color4-f 1.0 0.1 0.1 1.0)
      (opengl:gl-color4-f 0.1 1.0 0.1 1.0))
    (draw-sphere (cursor-position (osc-manager self))
                 (* (editor-get-edit-param self :line-width) 0.02))
    (restore-om-gl-colors-and-attributes)))


(defmethod  draw-3D-player-cursor-position ((self 3DC-Editor) time)
  "Draw the player cursor(s)"
  (loop for obj in (or (multi-obj-list self) (list (object-value self))) do
        (let ((point (time-sequence-get-active-timed-item-at obj time)))
          (when point
            ; (opengl:gl-color4-f 0.9 0.3 0.1 1.0)
            (let ((c (or (color obj) (om-make-color .9 .3 .1))))
              (opengl:gl-color3-f (coerce (om-color-r c) 'single-float)
                                  (coerce (om-color-g c) 'single-float)
                                  (coerce (om-color-b c) 'single-float)))
            (draw-sphere (point-to-list point) (* (editor-get-edit-param self :line-width) 0.03))
            )))
  (restore-om-gl-colors-and-attributes))

(defmethod point-to-list ((point 3dpoint))
  (list (3dpoint-x point) (3dpoint-y point) (3dpoint-z point) (3dpoint-time point)))


(defmethod update-3d-curve-vertices-colors ((self 3dc-editor))
  (let ((g-objects (om-get-gl-objects (3Dp self)))
        (om-objects (cons (object-value self) (remove (object-value self) (multi-obj-list self)))))
    (loop for 3dobj in g-objects
          for 3dc in om-objects do
          (setf (vertices-colors-interpol 3dobj)
                (not (eql (editor-get-edit-param self :color-style) :speed))
                (vertices-colors 3dobj)
                (get-vertices-colors self 3DC)))))

(defmethod update-curve-min-max-values ((self 3dc-editor) min max) nil)

(defmethod get-vertices-colors ((self 3dc-editor) (obj 3dc))
  "create a vector of colors for a 3D-timed-curve depending on the mode selected"
  (let* ((size (max (length (point-list obj)) 1))
         (min_h 0.65)
         (max_h 0.0)
         (range_h (- max_h min_h))
         (h_step (/ (- max_h min_h) (if (= size 1) 1 (1- size)))))
    (if (not (point-list obj))
        (default-color-vertices self obj)
      (case (editor-get-edit-param self :color-style)
        ((equal :indices)
         (update-curve-min-max-values self 0 (1- size))
         (loop for i from 0 to (1- size)
               collect
               (let ((h (+ min_h (* i h_step))))
                 (om-make-color-hsv h 0.8 0.8)))
         )
        ((equal :length)
         (let* ((profile (give-normalized-cumulative-length-profile obj)))
           (update-curve-min-max-values self 0 (give-length obj))
           (loop for val in profile
                 collect
                 (om-make-color-hsv (+ min_h (* val range_h)) 0.8 0.8)))
         )
        ((equal :speed)
         (let* ((speeds (give-speed-profile obj))
                (max_speed nil)
                (min_speed nil))
           (if (< (length speeds) 2)
               (progn
                 (setf max_speed (or (car speeds) 0))
                 (setf min_speed (or (car speeds) 0)))
             (progn
             ;removing -1 and replacing by max speed
               (setf max_speed (reduce 'max speeds))
               (setf speeds (loop for speed in speeds
                                  collect (if (= speed -1) max_speed speed)))
               (setf min_speed (reduce 'min speeds))
               (setf speeds (normalize-speeds-list speeds min_speed max_speed))))
           (setf speeds (cons (car speeds) speeds))
           (update-curve-min-max-values self min_speed max_speed)
           (loop for speed in speeds
                 collect  (om-make-color-hsv (+ min_h (* range_h (or speed 0.5))) 0.8 0.8)
                 )))
        (otherwise
         (update-curve-min-max-values self nil nil)
         (default-color-vertices self obj))))))


(defun normalize-speeds-list (speeds min_speed max_speed)
  (let ((range_speed (- max_speed min_speed)))
    (setf range_speed (- max_speed min_speed))
    (when (zerop range_speed)
      (setf range_speed 1))
    (setf speeds (om/ (om- speeds min_speed) range_speed))))

(defmethod default-color-vertices ((self 3dc-editor) (obj 3dc))
  (make-list (length (point-list obj)) :initial-element (or (color obj) (om-def-color :black))))


(defmethod om-view-key-handler ((self 3DPanel) key)
  (case key
    (:om-key-esc
     (om-init-3D-view self))
    (otherwise (call-next-method)))
  )

;;;===================
;;; OSC INPUT
;;;===================

(defun editor-open-osc-manager (editor)
  (unless (osc-manager editor)
    (setf (osc-manager editor) (make-instance 'osc-curvce-input-manager :editor editor)))
  (open-osc-manager (osc-manager editor)))

(defmethod osc-manager-add-callback ((editor 3DC-editor) point)
  (let ((obj (object-value editor)))
    (time-sequence-insert-timed-item-and-update obj point (length (point-list obj)))
    (report-modifications editor)
    (if (= (mod (length (point-list obj)) 10) 9)
        (editor-invalidate-views editor))
    (om-invalidate-view (3Dp editor))))

(defmethod osc-manager-move-callback ((editor 3DC-editor))
  (update-sub-editors editor)
  (om-invalidate-view (3dp editor)))

(defmethod osc-manager-clear-callback ((editor 3DC-editor))
  (setf (point-list (object-value editor)) nil)
  (report-modifications editor)
  (update-sub-editors editor))



#|
(defclass color-scale-view (om-view) ())
(defmethod om-draw-contents ((self color-scale-view))
  (let* ((size (om-view-size self))
        (width (om-point-x size))
        (steps 20)
        (w_step (round (/ width steps)))
        (height (om-point-y size))
        (min_h 0.65)
        (max_h 0.0)
        (h_step (/ (- max_h min_h) steps)))
    (loop for i from 0 to steps do
          (let ((h (+ min_h (* i h_step))))
            (om-draw-rect
             (* i w_step)  0 (+ (* (+ 1 i) w_step) 1) height
             :color (om-make-color-hsv h 0.8 0.8) :fill t)))))
|#


#|
;;; Timeline specific code not used anymore...
(defmethod get-timeline-foldable-views ((self 3dc-editor) &key obj time-ruler)
  (let* ((decimals (decimals obj))
         (x-label (om-make-graphic-object 'om-item-text :text " x/t" :size (omp 30 15) :font (om-def-font :font1)))
         (x-editor (make-instance 'bpf-editor :object (object self)  :container-editor self :decimals decimals
                                  :x-axis-key :time :y-axis-key :x))
         (x-panel (om-make-view 'bpf-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 80) ;:scale-fact (expt 10 decimals)
                                :editor x-editor))
         (x-y-ruler (om-make-view 'y-ruler-view :related-views (list x-panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
         (x-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1)))
         (y-label (om-make-graphic-object 'om-item-text :text " y/t" :size (omp 30 15) :font (om-def-font :font1)))
         (y-editor (make-instance 'bpf-editor :object (object self)  :container-editor self :decimals decimals
                                  :x-axis-key :time :y-axis-key :y))
         (y-panel (om-make-view 'bpf-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 80) ;:scale-fact (expt 10 decimals)
                                :editor y-editor))
         (y-y-ruler (om-make-view 'y-ruler-view :related-views (list y-panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
         (y-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1)))
         (z-label (om-make-graphic-object 'om-item-text :text " z/t" :size (omp 30 15) :font (om-def-font :font1)))
         (z-editor (make-instance 'bpf-editor :object (object self)  :container-editor self :decimals decimals
                                  :x-axis-key :time :y-axis-key :z))
         (z-panel (om-make-view 'bpf-panel :direct-draw t :bg-color (om-def-color :white) :scrollbars nil :size (omp 50 80) ;:scale-fact (expt 10 decimals)
                                :editor z-editor))
         (z-y-ruler (om-make-view 'y-ruler-view :related-views (list z-panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals decimals))
         (z-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1))))

    (set-g-component x-editor :main-panel x-panel)
    ;(setf (grid x-editor) t)
    (set-g-component x-editor :mousepos-txt x-mousepos-txt)
    (setf (y-ruler x-panel) x-y-ruler)
    (setf (related-views time-ruler) (append (list x-panel) (related-views time-ruler)))
    (reinit-ranges x-editor)

    (set-g-component y-editor :main-panel y-panel)
    ;(setf (grid y-editor) t)
    (set-g-component y-editor :mousepos-txt y-mousepos-txt)
    (setf (y-ruler y-panel) y-y-ruler)
    (setf (related-views time-ruler) (append (list y-panel) (related-views time-ruler)))
    (reinit-ranges y-editor)

    (set-g-component z-editor :main-panel z-panel)
    ;(setf (grid z-editor) t)
    (set-g-component z-editor :mousepos-txt z-mousepos-txt)
    (setf (y-ruler z-panel) z-y-ruler)
    (setf (related-views time-ruler) (append (list z-panel) (related-views time-ruler)))
    (reinit-ranges z-editor)

    (list
     (om-make-layout 'om-column-layout
                     :ratios '(0.01 1)
                     :subviews
                     (list
                      (om-make-layout 'om-row-layout :subviews (list x-label x-mousepos-txt) :align :bottom)
                      (om-make-layout 'om-row-layout :subviews (list x-y-ruler x-panel) :ratios '(0.001 1))))
      (om-make-layout 'om-column-layout
                     :ratios '(0.01 1)
                     :subviews
                     (list
                      (om-make-layout 'om-row-layout :subviews (list y-label y-mousepos-txt) :align :bottom)
                      (om-make-layout 'om-row-layout :subviews (list y-y-ruler y-panel) :ratios '(0.001 1))))
      (om-make-layout 'om-column-layout
                     :ratios '(0.01 1)
                     :subviews
                     (list
                      (om-make-layout 'om-row-layout :subviews (list z-label z-mousepos-txt) :align :bottom)
                      (om-make-layout 'om-row-layout :subviews (list z-y-ruler z-panel) :ratios '(0.001 1)))))))
|#


