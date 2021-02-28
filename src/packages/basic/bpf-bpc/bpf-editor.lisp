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

;distance treshold to add a new point in pen mode
(defvar *add-point-distance-treshold* 5)

(defclass bpf-editor (multi-display-editor-mixin OMEditor play-editor-mixin undoable-editor-mixin)
  ((edit-mode :accessor edit-mode :initform :mouse)
   (decimals :accessor decimals :initform 0 :initarg :decimals)
   ;;; multi-view
   (x-axis-key :accessor x-axis-key :initarg :x-axis-key :initform :x)
   (y-axis-key :accessor y-axis-key :initarg :y-axis-key :initform :y)
   (make-point-function :accessor make-point-function :initarg :make-point-function :initform 'om-make-bpfpoint)  ;;; BPC uses om-make-tpoint
   ;;; mini-dialog
   (point-editor :initform nil :accessor point-editor)
   ;;; timeline
   (timeline-editor :accessor timeline-editor :initform nil)
   ))

;;; background elements are just visible in the BPF/BPC/3DC editors
;;; can be pictures, speakers, etc.
(defclass background-element () ())
(defmethod draw-background-element ((self background-element) view editor &optional x1 y1 x2 y2))

(defmethod object-default-edition-params ((self BPF))
  '((:draw-style :default)
    (:background nil)
    (:display-min nil)
    (:display-max nil)))

(defun x-axis-accessor (editor) (case (x-axis-key editor) (:x 'om-point-x) (:y 'om-point-y) (:z 'om-point-z) (:time 'tpoint-time)))
(defun y-axis-accessor (editor) (case (y-axis-key editor) (:x 'om-point-x) (:y 'om-point-y) (:z 'om-point-z)))
(defun editor-make-point (editor x y) (funcall (make-point-function editor) x y))
(defun editor-point-x (editor point) (funcall (x-axis-accessor editor) point))
(defun editor-point-y (editor point) (funcall (y-axis-accessor editor) point))
(defun editor-point-set-x (editor point x) (funcall 'om-point-set point (x-axis-key editor) x))
(defun editor-point-set-y (editor point y) (funcall 'om-point-set point (y-axis-key editor) y))

(defmethod additional-box-attributes ((self bpf))
  '((:background "sets one or more background-element(s) (picture, etc.) in the editor" nil)
    (:display-min "sets a min y-value for display" nil)
    (:display-max "sets a max y-value for display" nil)))

(defmethod editor-window-init-size ((self bpf-editor)) (om-make-point 550 400))

(defmethod object-has-editor ((self bpf)) t)
(defmethod get-editor-class ((self bpf)) 'bpf-editor)

;;; from play-editor-mixin
(defmethod get-obj-to-play ((self bpf-editor)) (object-value self))

(defmethod handle-multi-display ((self bpf-editor)) t)

;;; UNDO SYSTEM
;; (defmethod undoable-object ((self bpf-editor)) (object-value self))
(defmethod get-object-slots-for-undo ((self bpf)) (append (call-next-method) '(point-list)))


(defparameter +bpf-editor-modes+ '(:mouse :pen :hand)) ; :zoomin :zoomout))

(defclass bpf-bpc-panel (OMEditorView multi-view-editor-view)
  ((scale-fact :accessor scale-fact :initarg :scale-fact :initform 1)
   (x-ruler :accessor x-ruler :initform nil)
   (y-ruler :accessor y-ruler :initform nil)
   ))

(defmethod set-x-ruler-range ((self bpf-bpc-panel) x1 x2)
  (when (x-ruler self)
    (set-ruler-range (x-ruler self) x1 x2)))

(defmethod set-y-ruler-range ((self bpf-bpc-panel) y1 y2)
  (when (y-ruler self)
    (set-ruler-range (y-ruler self) y1 y2)))

(defclass bpf-panel (bpf-bpc-panel x-cursor-graduated-view y-graduated-view) ())

(defmethod get-curve-panel-class ((self bpf-editor)) 'bpf-panel)

(defmethod pix-to-x ((self bpf-bpc-panel) pix) (/ (call-next-method) (scale-fact self)))
(defmethod dpix-to-dx ((self bpf-bpc-panel) dpix) (/ (call-next-method) (scale-fact self)))
(defmethod x-to-pix ((self bpf-bpc-panel) x) (call-next-method self (* x (scale-fact self))))
(defmethod dx-to-dpix ((self bpf-bpc-panel) dx) (call-next-method self (* dx (scale-fact self))))

(defmethod pix-to-y ((self bpf-bpc-panel) pix) (/ (call-next-method) (scale-fact self)))
(defmethod dpix-to-dy ((self bpf-bpc-panel) dpix) (/ (call-next-method) (scale-fact self)))
(defmethod y-to-pix ((self bpf-bpc-panel) y) (call-next-method self (* y (scale-fact self))))
(defmethod dy-to-dpix ((self bpf-bpc-panel) dy) (call-next-method self (* dy (scale-fact self))))


;;;==========================
;;; Special BPC
;;;==========================

(defclass bpc-editor (bpf-editor)
  ;;;reference time for gesture capture while drawing (BPC only)
  ((gesture-time-ref :accessor gesture-time-ref :initform 0)
   (gesture-interval-time :initform 500 :accessor gesture-interval-time :initarg :gesture-interval-time))
  (:default-initargs :make-point-function 'om-make-tpoint))

(defmethod get-editor-class ((self bpc)) 'bpc-editor)

(defclass bpc-panel (bpf-bpc-panel x-graduated-view y-graduated-view) ())
(defmethod get-curve-panel-class ((self bpc-editor)) 'bpc-panel)


;;;==========================
;;; play-editor-mixin methods
;;;==========================

(defmethod cursor-panes ((self bpf-editor))
  (cons (get-g-component self :main-panel)
        (if (timeline-editor self)
            (cursor-panes (timeline-editor self)))))

(defmethod cursor-panes ((self bpc-editor))
  (when (timeline-editor self)
    (cursor-panes (timeline-editor self))))

(defmethod play-editor-callback ((self bpc-editor) time)
  (call-next-method)
  ;;; no very good :)
  (om-invalidate-view (get-g-component self :main-panel)))

(defmethod set-time-display ((self bpc-editor) time)
  (set-time-display (timeline-editor self) time)
  (call-next-method))

;;;==========================
;;; SYSTEM CALLBACKS
;;;==========================

(defmethod init-editor ((self bpf-editor))
  (setf (decimals self) (decimals (object-value self)))
  (setf (timeline-editor self) (make-instance 'timeline-editor :object (object self) :container-editor self)))

(defmethod init-editor ((self bpc-editor))
  (call-next-method)
  (time-sequence-update-internal-times (object-value self) self))

(defmethod make-timeline-left-item ((self bpf-editor) id)
  (om-make-view 'om-view :size (omp 30 15)))

(defmethod draw-modes-for-object ((self bpf-editor)) '(:default :points :lines :histogram))

;;; for a BPF editor the 'main-view' is the whole layout
(defmethod make-editor-window-contents ((editor bpf-editor))
  (let* ((object (object-value editor))
         (panel (om-make-view (get-curve-panel-class editor) :size (omp 50 100) :direct-draw t :bg-color (om-def-color :white) :scrollbars nil
                              :scale-fact (expt 10 (decimals editor))
                              :editor editor))
         (rx (om-make-view 'x-ruler-view :related-views (list panel) :size (omp nil 20) :bg-color (om-def-color :white) :decimals (decimals editor)))
         (ry (om-make-view 'y-ruler-view :related-views (list panel) :size (omp 30 nil) :bg-color (om-def-color :white) :decimals (decimals editor)))
         (mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 200 16)))
         ;(name-txt (om-make-graphic-object 'om-item-text :size (omp 60 16) :text (name object)))

         (timeline-editor (timeline-editor editor))
         (timeline (om-make-layout 'om-row-layout))
         (top-area (om-make-layout 'om-row-layout ;:align :center
                                   :ratios '(nil nil 0.5 0.05 0.05)
                                   :subviews (list nil mousepos-txt ;; name-txt
                                                   nil
                                                   (make-time-monitor editor :time (if (action object) 0 nil))
                                                   ;(om-make-layout 'om-column-layout :align :right :subviews (list
                                                   (and t ;(action object)
                                                        (om-make-layout
                                                         'om-row-layout ;:size (omp 60 20)
                                                         :subviews (list (make-play-button editor :enable (action object))
                                                                         (make-repeat-button editor :enable (action object))
                                                                         (make-pause-button editor :enable (action object))
                                                                         (make-stop-button editor :enable (action object)))))
                                                   )))
         (bottom-area nil)
         )
    ;create the bottom area (needed to add remove the timeline-time-monitor
    (setf bottom-area (om-make-layout 'om-row-layout
                                      :align :bottom
                                      :size (omp nil 40)
                                      :subviews
                                      (list
                                       (om-make-di 'om-simple-text :text "Display:"
                                                   :size (omp 40 20)
                                                   :font (om-def-font :font1))
                                       (om-make-di 'om-popup-list :items (draw-modes-for-object editor)
                                                   :size (omp 80 24) :font (om-def-font :font1)
                                                   :value (editor-get-edit-param editor :draw-style)
                                                   :di-action #'(lambda (list)
                                                                  (editor-set-edit-param editor :draw-style (om-get-selected-item list))))
                                       nil
                                       (om-make-di 'om-check-box :text "indices" :size (omp 60 24) :font (om-def-font :font1)
                                                   :checked-p (editor-get-edit-param editor :show-indices)
                                                   :di-action #'(lambda (item)
                                                                  (editor-set-edit-param editor :show-indices (om-checked-p item))))
                                       (om-make-di 'om-check-box :text "times" :size (omp 60 24) :font (om-def-font :font1)
                                                   :checked-p (editor-get-edit-param editor :show-times)
                                                   :di-action #'(lambda (item)
                                                                  (editor-set-edit-param editor :show-times (om-checked-p item))))
                                       (om-make-di 'om-check-box :text "grid" :size (omp 45 24) :font (om-def-font :font1)
                                                   :checked-p (editor-get-edit-param editor :grid)
                                                   :di-action #'(lambda (item)
                                                                  (editor-set-edit-param editor :grid (om-checked-p item))))

                                       ;;; this should be only for BPC
                                       ;;; (already set in 3DC as well)
                                       ;(om-make-di 'om-simple-text :text "Offset:"
                                       ;            :size (omp 38 20)
                                       ;            :font (om-def-font :font1))
                                       ;(om-make-graphic-object 'numbox
                                       ;                        :value (if (eq (type-of editor) 'bpc-editor)
                                       ;                                   (gesture-interval-time editor)
                                       ;                                 500)
                                       ;                        :bg-color (om-def-color :white)
                                       ;                        :border t
                                       ;                        :size (om-make-point 40 18)
                                       ;                        :font (om-def-font :font1)
                                       ;                        :min-val 0
                                       ;                        :after-fun #'(lambda (item)
                                       ;                                       (setf (gesture-interval-time editor) (value item))
                                       ;                                       ))

                                       (when timeline-editor
                                         (om-make-di 'om-check-box :text "timeline" :size (omp 120 24) :font (om-def-font :font1)
                                                     :checked-p (editor-get-edit-param editor :show-timeline)
                                                     :di-action #'(lambda (item)
                                                                    (editor-set-edit-param editor :show-timeline (om-checked-p item))
                                                                    (clear-timeline timeline-editor)
                                                                    (om-invalidate-view timeline)
                                                                    (when (om-checked-p item)
                                                                      (make-timeline-view timeline-editor))
                                                                    (om-update-layout (main-view editor)))
                                                     ))
                                       nil
                                       )))

    (set-g-component editor :x-ruler rx)
    (set-g-component editor :y-ruler ry)
    (set-g-component editor :mousepos-txt mousepos-txt)
    (set-g-component editor :main-panel panel)

    (setf (x-ruler panel) rx (y-ruler panel) ry)

    ;timeline
    (when timeline-editor
      (set-g-component timeline-editor :main-panel timeline)
      (when (editor-get-edit-param editor :show-timeline)
        (make-timeline-view timeline-editor)))

    (om-make-layout 'om-row-layout :ratios '(9.9 0.1)
                    :subviews
                    (list
                     (om-make-layout 'om-column-layout
                                     :ratios '(0.96 nil 0.02)
                                     :subviews
                                     (list
                                      (om-make-layout 'om-grid-layout :align :right
                                                      :dimensions '(2 3)
                                                      :delta 2
                                                      :ratios '((0.01 0.99) (0.01 0.98 0.01))
                                                      :subviews (list nil top-area
                                                                      ry panel
                                                                      nil rx)
                                                      )
                                      (when timeline timeline)
                                      bottom-area))
                     (call-next-method)))
    ))


(defmethod initialize-instance :after ((self bpf-bpc-panel) &rest args)
  (let* ((editor (editor self))
         (mode-buttons
          (list
           (om-make-graphic-object 'om-icon-button :position (omp 10 5) :size (omp 20 20)
                                   :icon :mouse :icon-pushed :mouse-pushed
                                   :lock-push t :pushed (equal (edit-mode editor) :mouse) :id :mouse
                                   :action #'(lambda (b) (declare (ignore b)) (editor-set-edit-mode editor :mouse)))
           (om-make-graphic-object 'om-icon-button :position (omp 30 5) :size (omp 20 20)
                                   :icon :pen :icon-pushed :pen-pushed
                                   :lock-push t :pushed (equal (edit-mode editor) :pen) :id :pen
                                   :action #'(lambda (b) (declare (ignore b)) (editor-set-edit-mode editor :pen)))
           (om-make-graphic-object 'om-icon-button :position (omp 50 5) :size (omp 20 20)
                                   :icon :hand :icon-pushed :hand-pushed
                                   :lock-push t :pushed (equal (edit-mode editor) :hand) :id :hand
                                   :action #'(lambda (b) (declare (ignore b)) (editor-set-edit-mode editor :hand))))))
    (set-g-component editor :mode-buttons mode-buttons)
    (apply 'om-add-subviews (cons self mode-buttons))
    ))

;;; happens when the window is already built
(defmethod init-editor-window ((editor bpf-editor))
  (call-next-method)
  (reinit-ranges editor)
  )


(defmethod om-view-cursor ((self bpf-bpc-panel))
  (let ((editor (or (editor self) (editor (om-view-window self)))))
    (case (edit-mode editor)
      (:pen (if (om-add-key-down) nil (om-get-cursor :pen)))
      (:hand (om-get-cursor :hand))
      ;(:zoomin (om-get-cursor :loupe))
      (otherwise (if (om-add-key-down) (om-get-cursor :add) nil)))))

(defmethod set-decimals-in-editor ((self bpf-editor) val)
  (when (get-g-component self :x-ruler)
    (setf (decimals (get-g-component self :x-ruler)) val)
    (scale-ruler (get-g-component self :y-ruler) (expt 10 (- val (decimals self)))))
  (when (get-g-component self :y-ruler)
    (setf (decimals (get-g-component self :y-ruler)) val)
    (scale-ruler (get-g-component self :x-ruler) (expt 10 (- val (decimals self)))))
  (when (get-g-component self :main-panel)
    (setf (scale-fact (get-g-component self :main-panel)) (expt 10 val)))
  (setf (decimals self) val))

;;; called at at eval
(defmethod update-to-editor ((editor bpf-editor) (from t))
  (call-next-method)
  (let ((object (object-value editor)))

    (when (g-components editor)
      (when object
        (set-decimals-in-editor editor (decimals object))
        (enable-play-controls editor (action object)))
      (om-invalidate-view (get-g-component editor :main-panel))
      )
    (when (timeline-editor editor)
      (update-to-editor (timeline-editor editor) editor))))

(defmethod update-to-editor ((editor bpf-editor) (from timeline-editor))
  (call-next-method)
  (om-invalidate-view (get-g-component editor :main-panel))
  (update-timeline-editor editor)
  (report-modifications editor)
  )

(defmethod update-to-editor ((self bpc-editor) (from t))
  (call-next-method)
  (time-sequence-update-internal-times (object-value self)))


;;; called from the default-editor part
;;; update: I think this is never called: the arguments are (view object)
(defmethod update-after-prop-edit ((view OMEditorWindow) (editor bpf-editor))
  (let ((value (object-value editor))
        (box (object editor)))
    (editor-invalidate-views editor)
    (when box (setf (name box) (name value)))
    (enable-play-controls editor (action value))
    (report-modifications editor)))

(defmethod editor-invalidate-views ((self bpf-editor))
  (call-next-method)
  (om-invalidate-view (get-g-component self :main-panel))
  (when (timeline-editor self)
    (editor-invalidate-views (timeline-editor self))))

;;;==========================
;;; DRAW
;;;==========================

(defmethod time-to-draw ((self bpf) (editor bpf-editor) pt i)
  (om-point-x pt))

;times are negatives values if they are not user defined (for display differenciation)
(defmethod time-to-draw ((self bpc) editor pt i)
  (or (tpoint-time pt)
      (let ((ti (nth i (time-sequence-get-internal-times self))))
        (and ti (- ti)))))


(defun draw-bpf-point (p editor &key index time selected)

  (cond
   ((and time
         (equal (player-get-object-state (player editor) (object-value editor)) :play)
         (< (abs time) (player-get-object-time (player editor) (object-value editor)))
         )
    (om-with-fg-color (om-def-color :dark-red)
      (om-draw-circle (car p) (cadr p) 4 :fill t)))

   (selected
    (om-with-fg-color (om-def-color :dark-red)
      (om-draw-circle (car p) (cadr p) 4 :fill t)))

   ;;; draw normal except if lines only
   ((not (equal (editor-get-edit-param editor :draw-style) :lines))
    (om-draw-circle (car p) (cadr p) 3 :fill t))
   (t nil))


  (when index
    (om-draw-string (car p) (+ (cadr p) 15) (number-to-string index)))
  (when (and time (editor-get-edit-param editor :show-times))
    (om-with-fg-color (if (minusp time) (om-def-color :gray) (om-def-color :dark-blue) )
      (om-draw-string (car p) (- (cadr p) 15) (number-to-string (abs time) 0))))
  )

(defun draw-interpol-point (p editor &key time)
  (if (and time (equal (player-get-object-state (player editor) (object-value editor)) :play)
           (>= (player-get-object-time (player editor) (object-value editor)) time))
      (om-with-fg-color (om-def-color :dark-red)
        (om-draw-circle (car p) (cadr p) 1 :fill nil))
    (om-draw-circle (car p) (cadr p) 1 :fill nil)
    ))

(defun point-visible-p (pt x1 x2 y1 y2)
  (and (> (car pt) x1) (< (car pt) x2)
       (> (cadr pt) y1) (< (cadr pt) y2)))

(defmethod draw-one-bpf ((bpf bpf) view editor foreground? &optional x1 x2 y1 y2)

  (om-trap-errors

   (let ((pts (point-list bpf)))
     (when pts
       (let ((first-pt (list (x-to-pix view (editor-point-x editor (car pts)))
                             (y-to-pix view (editor-point-y editor (car pts)))))
             (selection (and foreground? (selection editor)))
             (show-indice (editor-get-edit-param editor :show-indices)))

         (cond
          ;;; draw only points
          ((equal (editor-get-edit-param editor :draw-style) :points)

           (draw-bpf-point first-pt editor
                           :selected (and (consp selection) (find 0 selection))
                           :index (and foreground? show-indice 0)
                           :time (and foreground? (time-to-draw bpf editor (car pts) 0))) ;TODO Change drawing args here !

           (loop for pt in (cdr pts)
                 for i = 1 then (1+ i) do
                 (let ((p (list (x-to-pix view (editor-point-x editor pt))
                                (y-to-pix view (editor-point-y editor pt)))))
                   (when (point-visible-p p x1 x2 y1 y2)
                     (draw-bpf-point p editor
                                     :selected (and (consp selection) (find i selection))
                                     :index (and foreground? show-indice i)
                                     :time (and foreground? (time-to-draw bpf editor pt i)))
                     ))))

          ;;; histogram
          ((equal (editor-get-edit-param editor :draw-style) :histogram)

           (let ((origin-y (y-to-pix view (editor-point-y editor (omp 0 0)))))

             (loop for i from 0 to (1- (length pts))
                   do
                   (let* ((pt (nth i pts))
                          (x (x-to-pix view (editor-point-x editor pt)))
                          (y (y-to-pix view (editor-point-y editor pt)))
                          (next-pt (nth (1+ i) pts))
                          (next-x (if next-pt (x-to-pix view (editor-point-x editor next-pt))
                                    (if (plusp i)
                                        ;;; repeat last width
                                        (let ((prev-pt (nth (1- i) pts)))
                                          (+ x (- x (x-to-pix view (editor-point-x editor prev-pt)))))
                                      ;;; last case: only one point
                                      (+ x 100)))))

                     (om-draw-rect x origin-y (- next-x x) (- y origin-y)
                                   :fill nil)
                     (om-draw-rect x origin-y (- next-x x) (- y origin-y)
                                   :fill t :color (om-def-color :gray))

                     (draw-bpf-point (list x y) editor
                                     :selected (and (consp selection) (find i selection))
                                     :index (and foreground? show-indice i)
                                     :time (and foreground? (time-to-draw bpf editor pt i)))

                     (om-draw-string (1+ x) (- origin-y 4) (format nil "~D" (om-point-y pt))
                                     :font (om-def-font :font1 :size 9)
                                     :color (om-def-color :white))
                     ))
             ))

          ;;; draw lines (with/without points)
          (t
           (draw-bpf-point first-pt editor
                           :selected (and (consp selection) (find 0 selection))
                           :index (and foreground? show-indice 0)
                           :time (and foreground? (time-to-draw bpf editor (car pts) 0))) ;TODO Change drawing args here !

           (let ((pt-list (loop for rest on pts
                                for i = 1 then (1+ i)
                                while (cadr rest)
                                append
                                (let* ((p1 (car rest))
                                       (p2 (cadr rest))
                                       (pp1 (list (x-to-pix view (editor-point-x editor p1))
                                                  (y-to-pix view (editor-point-y editor p1))))
                                       (pp2 (list (x-to-pix view (editor-point-x editor p2))
                                                  (y-to-pix view (editor-point-y editor p2)))))
                                               ;(when ;(or (point-visible-p pp1 x xmax y ymax)
                                                     ;    (point-visible-p pp2 x xmax y ymax)
                                                     ;    (and (> x (car pp1)) (< x (car pp2))))
                                  (unless (or (and (< (car pp1) x1) (< (car pp2) x1))
                                              (and (> (car pp1) x2) (> (car pp2) x2)))

                                    ;;; will not draw the point if line-only, except if selected etc.
                                    (draw-bpf-point pp2 editor
                                                    :selected (and (consp selection) (find i selection))
                                                    :index (and foreground? show-indice i)
                                                    :time (and foreground? (time-to-draw bpf editor p2 i)))
                                    (append pp1 pp2)
                                    )
                                  ))))
             (om-with-line-size (if (find T selection) 2 1)
               (om-draw-lines pt-list))
             )))

         (when (number-? (interpol bpf))
           (let ((interpol-times (arithm-ser (get-first-time bpf) (get-obj-dur bpf) (number-number (interpol bpf)))))
             (loop for time in interpol-times
                   do (let ((new-p (time-sequence-make-timed-item-at bpf time)))
                        (draw-interpol-point (list (x-to-pix view (editor-point-x editor new-p))
                                                   (y-to-pix view (editor-point-y editor new-p)))
                                             editor
                                             :time (and foreground? time))))))

         )))))


(defmethod om-draw-contents-area ((self bpf-bpc-panel) x y w h)
  (let* ((editor (editor self))
         (obj (object-value editor))
         (xmax (+ x w))
         (ymax (+ y h)))

    (when obj

      (om-with-font
       (om-def-font :font1)

       ;;; GRID
       (when (editor-get-edit-param editor :grid)
         ;(om-with-line '(2 2)  ;; seems tpo cost a lot in drawing...
         (om-with-fg-color (om-make-color 0.95 0.95 0.95)
           (draw-grid-from-ruler self (x-ruler self))
           (draw-grid-from-ruler self (y-ruler self))
           ))

       ;;; AXES
       (om-with-fg-color (om-def-color :gray)
         (let ((center (list (x-to-pix self 0) (y-to-pix self 0))))
           (when (and (> (cadr center) 0) (< (cadr center) (h self)))
             (om-draw-line 0 (cadr center) (w self) (cadr center)))
           (when (and (> (car center) 0) (< (car center) (w self)))
             (om-draw-line (car center) 0 (car center) (h self)))
           ))

       (mapc #'(lambda (elt) (draw-background-element elt self editor x y xmax ymax))
             (list! (get-edit-param (object editor) :background)))

       ;;; draw multi ?
       (when (multi-display-p editor)
         (loop for bg-bpf in (remove obj (multi-obj-list editor))
               do (om-with-fg-color (om-make-color-alpha (or (color bg-bpf) (om-def-color :dark-gray)) 0.4)
                    (draw-one-bpf bg-bpf self editor nil x xmax y ymax))))

       (when (point-list obj)

         (om-with-fg-color (if (find T (selection editor))
                               (om-def-color :dark-red)
                             (or (color obj)
                                 (om-def-color :dark-gray)))

           (draw-one-bpf obj self editor t x xmax y ymax)

           ))
       ))
    ))



;;;==========================
;;; MENUS
;;;==========================

(defun bpf-edit-menu-items (self)
  (list (om-make-menu-comp
         (list (om-make-menu-item "Undo" #'(lambda () (funcall (undo-command self))) :key "z" :enabled #'(lambda () (and (undo-command self) t)))
               (om-make-menu-item "Redo" #'(lambda () (funcall (redo-command self))) :key "Z" :enabled #'(lambda () (and (redo-command self) t)))))
        (om-make-menu-comp
         (list
          (om-make-menu-item "Delete selection" #'(lambda () (funcall (clear-command self))) :enabled (and (clear-command self) t))))
        (om-make-menu-comp
         (list (om-make-menu-item "Select All" #'(lambda () (funcall (select-all-command self))) :key "a" :enabled (and (select-all-command self) t))))
        (om-make-menu-comp
         (list
          (om-make-menu-item "Reverse Points" #'(lambda () (reverse-points self)) :key "r" )))
        (om-make-menu-comp
         (list
          (om-make-menu-item "OSC Input Manager" #'(lambda () (funcall (open-osc-manager-command self)))
                             :enabled (and (open-osc-manager-command self) t))))
        ))

(defmethod om-menu-items ((self bpf-editor))
  (remove nil
          (list
           (main-app-menu-item)
           (om-make-menu "File" (default-file-menu-items self))
           (om-make-menu "Edit" (bpf-edit-menu-items self))
           (om-make-menu "Windows" (default-windows-menu-items self))
           (om-make-menu "Help" (default-help-menu-items self))
           )))

(defmethod select-all-command ((self bpf-editor))
  #'(lambda ()
      (setf (selection self) (list T))
      (update-timeline-editor self)
      (editor-invalidate-views self)
      (select-bpf self)))

(defmethod open-osc-manager-command ((self bpf-editor)) nil)

;;;==========================
;;; ACTIONS
;;;==========================

(defmethod editor-set-edit-mode ((self bpf-editor) mode)
  (mapcar #'(lambda (b)
              (if (equal mode (id b)) (button-select b) (button-unselect b)))
          (get-g-component self :mode-buttons))
  (setf (edit-mode self) mode)
  (om-set-view-cursor (get-g-component self :main-panel)
                      (om-view-cursor (get-g-component self :main-panel))))

;;; depending on where is the panel...
(defmethod position-display ((editor t) pos-pix) nil)

(defmethod position-display ((editor bpf-editor) pos-pix)
  (when (get-g-component editor :mousepos-txt)
    (let* ((decimals (decimals editor))
           (point (omp (pix-to-x (get-g-component editor :main-panel) (om-point-x pos-pix))
                       (pix-to-y (get-g-component editor :main-panel) (om-point-y pos-pix)))))
      (om-set-text (get-g-component editor :mousepos-txt)
                   (if (zerop decimals)
                       (format nil "[~D,~D]" (round (om-point-x point)) (round (om-point-y point)))
                     (format nil (format nil "[~~,~DF,~~,~DF]" decimals decimals) (om-point-x point) (om-point-y point))))
      )
    ))


(defmethod reinit-ranges ((self bpf-editor))
  ;;; ignore bg-pict for the moment (bpf-picture-ranges (pict (editor self)))
  (let ((bpf (object-value self))
        (scaler (expt 10 (decimals self))))
    (when bpf
      (let ((ranges (space-ranges (loop for x in (mapcar (x-axis-accessor self) (point-list bpf))
                                        for y in (mapcar (y-axis-accessor self) (point-list bpf))
                                        minimize x into x1 maximize x into x2
                                        minimize y into y1 maximize y into y2
                                        finally (return (list x1 x2 y1 y2)))
                                  0.05 (/ 100 (expt 10 (decimals self))))))
        (set-x-ruler-range (get-g-component self :main-panel) (* (nth 0 ranges) scaler) (* (nth 1 ranges) scaler))
        (set-y-ruler-range (get-g-component self :main-panel) (* (nth 2 ranges) scaler) (* (nth 3 ranges) scaler))
        ))
    ))



(defmethod set-rulers-from-selection ((self bpf-editor) x1 x2 y1 y2)
  (let ((panel (get-g-component self :main-panel)))
    (set-ruler-range (x-ruler panel)
                     (pix-to-x panel x1)
                     (pix-to-x panel x2))
    (set-ruler-range (y-ruler panel)
                     (pix-to-y panel y1)
                     (pix-to-y panel y2))))


(defmethod find-clicked-point-or-curve ((editor bpf-editor) (object bpf) position)
  (let ((points (point-list object))
        (panel (get-g-component editor :main-panel)))
    (when points
      (let* ((xx (pix-to-x panel (om-point-x position)))
             (pos (or (position xx points :key #'(lambda (p) (editor-point-x editor p)) :test '>= :from-end t) 0))
             (p1 (nth pos points))
             (p2 (when (< (1+ pos) (length points)) (nth (1+ pos) points)))
             (delta 4))
        (or (and p1 (om-point-in-rect-p position
                                        (- (round (x-to-pix panel (editor-point-x editor p1))) delta)
                                        (- (round (y-to-pix panel (editor-point-y editor p1))) delta)
                                        (* delta 2) (* delta 2))
                 pos)
            (and p2
                 (om-point-in-rect-p position
                                     (- (x-to-pix panel (editor-point-x editor p2)) delta)
                                     (- (y-to-pix panel (editor-point-y editor p2)) delta)
                                     (* delta 2) (* delta 2))
                 (1+ pos))
            (and p1 p2 (not (find (editor-get-edit-param editor :draw-style) '(:points :histogram)))
                 (om-point-in-line-p position
                                     (om-make-point (x-to-pix panel (editor-point-x editor p1))
                                                    (y-to-pix panel (editor-point-y editor p1)))
                                     (om-make-point (x-to-pix panel (editor-point-x editor p2))
                                                    (y-to-pix panel (editor-point-y editor p2)))
                                     delta)
                 t)
            )
        ))))

(defmethod find-clicked-point-or-curve ((editor bpc-editor) (object bpc) position)
  (let ((points (point-list object))
        (panel (get-g-component editor :main-panel)))
    (when points
      (let* ((delta 4) (rep nil))
        (if (om-point-in-rect-p position
                                (- (x-to-pix panel (editor-point-x editor (car points))) delta)
                                (- (y-to-pix panel (editor-point-y editor (car points))) delta)
                                (* delta 2) (* delta 2))
            (setf rep 0)

          (loop for restpoints on points
                for i = 1 then (1+ i)
                while (and (cdr restpoints) (not rep))
                do (let ((p (cadr restpoints)))
                     (if (om-point-in-rect-p position
                                             (- (x-to-pix panel (editor-point-x editor p)) delta)
                                             (- (y-to-pix panel (editor-point-y editor p)) delta)
                                             (* delta 2) (* delta 2))
                         (setf rep i)
                       (if (not (equal (editor-get-edit-param editor :draw-style) :points))
                           (setf rep (om-point-in-line-p position
                                                         (om-make-point (x-to-pix panel (editor-point-x editor p))
                                                                        (y-to-pix panel (editor-point-y editor p)))
                                                         (om-make-point (x-to-pix panel (editor-point-x editor (car restpoints)))
                                                                        (y-to-pix panel (editor-point-y editor (car restpoints))))
                                                         delta)))
                       ))))
        rep))))



;;; do nothing...
(defmethod select-bpf ((editor bpf-editor) &optional n))

(defmethod points-in-area ((editor bpf-editor) p1 p2)
  (let* ((panel (get-g-component editor :main-panel))
         (x1 (pix-to-x panel (om-point-x p1)))
         (x2 (pix-to-x panel (om-point-x p2)))
         (y1 (pix-to-y panel (om-point-y p1)))
         (y2 (pix-to-y panel (om-point-y p2)))
         (xmin (min x1 x2)) (xmax (max x1 x2))
         (ymin (min y1 y2)) (ymax (max y1 y2))
         (points (point-list (object-value editor))))
    (loop for p in points
          for i = 0 then (1+ i)
          when (and (>= (editor-point-x editor p) xmin)
                    (<= (editor-point-x editor p) xmax)
                    (>= (editor-point-y editor p) ymin)
                    (<= (editor-point-y editor p) ymax))
          collect i)
    ))


;;;
;;; return the position of the inserted point
(defmethod add-point-at-pix ((editor bpf-editor) (object bpf) position &optional (time nil))
  (let* ((panel (get-g-component editor :main-panel))
         (new-point (editor-make-point editor
                                       (pix-to-x panel (om-point-x position))
                                       (pix-to-y panel (om-point-y position)))))

    (insert-point object new-point)))

;;; systematically at the end of the point list (e.g. called when drawing)
;;; return the position of the inserted point
(defmethod add-point-at-pix ((editor bpc-editor) (object bpc) position &optional (time nil))
  (let* ((panel (get-g-component editor :main-panel))
         (new-point (editor-make-point editor
                                       (pix-to-x panel (om-point-x position))
                                       (pix-to-y panel (om-point-y position)))))
    (when time
      (setf (tpoint-time new-point) time))
    (time-sequence-insert-timed-item-and-update object new-point)))


;;; insert at the correct place in BPF
;;; if the point exist : move it
;;; draw or not = the same
;;; return the position of the inserted point
(defmethod insert-point-at-pix ((editor bpf-editor) (object bpf) position &optional (time nil))
  (add-point-at-pix editor object position time))

;;; insert at the correct place in BPC
;;; if in a segment, insert, else add at the end
;;; => don't call this in 'draw' mode
;;; return the position of the inserted point
(defmethod insert-point-at-pix ((editor bpf-editor) (object bpc) position &optional (time nil))

  (let* ((panel (get-g-component editor :main-panel))
         (new-point (editor-make-point editor
                                       (pix-to-x panel (om-point-x position))
                                       (pix-to-y panel (om-point-y position))))
         (segment nil) (delta 4))

    (loop for restpoints on (point-list object)
          for i = 1 then (1+ i)
          while (and (cdr restpoints) (not segment))
          when (om-point-in-line-p position
                                   (om-make-point (x-to-pix panel (editor-point-x editor (car restpoints)))
                                                  (y-to-pix panel (editor-point-y editor (car restpoints))))
                                   (om-make-point (x-to-pix panel (editor-point-x editor (cadr restpoints)))
                                                  (y-to-pix panel (editor-point-y editor (cadr restpoints))))
                                   delta)
          do (setf segment i))
    (when (and time (not segment))
      (setf (tpoint-time new-point) time))
    (time-sequence-insert-timed-item-and-update object new-point (or segment (length (point-list object))))
    ))

(defmethod move-editor-selection ((self bpf-editor) &key (dx 0) (dy 0))
  (when (selection self)
    (let ((bpf (object-value self)))
      (cond ((find T (selection self))
             (loop for point in (point-list bpf) do
                   (editor-point-set-x self point (+ (editor-point-x self point) dx))
                   (editor-point-set-y self point (+ (editor-point-y self point) dy))))
            (t
             (let ((points (loop for pos in (selection self) collect (nth pos (point-list bpf)))))
               (funcall 'move-points-in-bpf bpf points dx dy (x-axis-key self) (y-axis-key self))))
            ))))

(defmethod delete-editor-selection ((self bpf-editor))
  (if (find T (selection self))
      (setf (point-list (object-value self)) nil)
    (mapcar
     #'(lambda (i) (remove-nth-point (object-value self) i))
     (sort (selection self) '>)
     ))
  (setf (selection self) nil)
  (when (timeline-editor self)
    (update-to-editor (timeline-editor self) self)))

(defmethod delete-editor-selection ((self bpc-editor))
  (if (find T (selection self))
      (setf (point-list (object-value self)) nil)
    (mapcar
     #'(lambda (p) (remove-nth-timed-point-from-time-sequence (object-value self) p))
     (sort (selection self) '>)))
  (setf (selection self) nil)
  (update-to-editor (timeline-editor self) self))

(defmethod round-point-values ((editor bpf-editor))
  (set-bpf-point-values (object-value editor)))


(defmethod set-point-in-obj ((self bpf-editor) point values)

  (set-point-in-bpf (object-value self) point
                    (car values)
                    (cadr values))

  (setf (point-list (object-value self))
        (sort (point-list (object-value self)) '< :key 'om-point-x))

  (when (container-editor self)
    (update-to-editor (container-editor self) self))

  (editor-invalidate-views self))

(defmethod set-point-in-obj ((self bpc-editor) point values)

  (set-point-in-bpc (object-value self) point
                    (car values)
                    (cadr values)
                    (cadddr values))

  (setf (point-list (object-value self))
        (sort (point-list (object-value self)) '< :key 'tpoint-internal-time))

  (time-sequence-update-internal-times (object-value self))

  (when (container-editor self)
    (update-to-editor (container-editor self) self))
  (editor-invalidate-views self))


(defmethod close-point-editor ((self bpf-editor))
  (when (point-editor self)
    (om-close-window (point-editor self))
    (setf (point-editor self) nil)))

(defmethod open-point-editor ((self bpf-editor) p &key (z nil z-supplied-p) (time nil time-supplied-p))
  (let* ((x (om-point-x p)) (y (om-point-y p))
         xt yt zt tt cb ob win)
    (let ((return-from-point-editor
           #'(lambda (item)
               (declare (ignore item))
               (set-point-in-obj self p
                                 (list (read-number (om-dialog-item-text xt))
                                       (read-number (om-dialog-item-text yt))
                                       (and zt (read-number (om-dialog-item-text zt)))
                                       (and tt (read-number (om-dialog-item-text tt)))))
               (close-point-editor self))))

      (setf xt (om-make-di 'om-editable-text :text (number-to-string x)
                           :border t
                           :bg-color (om-def-color :white) :size (omp 80 32)) ; :di-action return-from-point-editor)
            yt (om-make-di 'om-editable-text :text (number-to-string y)
                           :border t
                           :bg-color (om-def-color :white) :size (omp 80 32)) ; :di-action return-from-point-editor)
            zt (when z-supplied-p
                 (om-make-di 'om-editable-text :text (number-to-string z)
                             :border t
                             :bg-color (om-def-color :white) :size (omp 80 32))) ; :di-action return-from-point-editor))
            tt (when time-supplied-p
                 (om-make-di 'om-editable-text :text (number-to-string time)
                             :border t
                             :bg-color (om-def-color :white) :size (omp 80 32))) ; :di-action return-from-point-editor))
            cb (om-make-di 'om-button :text "Cancel" :size (omp 80 25)
                           :di-action #'(lambda (b) (declare (ignore b)) (close-point-editor self)))
            ob (om-make-di 'om-button :text "OK" :size (omp 80 25) :default t :focus t
                           :di-action return-from-point-editor))

      (setf win (om-make-window
                 'om-no-border-win :resizable nil

                 :position (om-add-points (om-view-position (window self)) (om-mouse-position (window self)))
                 :win-layout (om-make-layout
                              'om-column-layout :ratios '(1 1 2) :align :left
                              :subviews (list
                                         (om-make-layout
                                          'om-row-layout :align :center
                                          :subviews (list (om-make-di 'om-simple-text :text "X") xt))
                                         (om-make-layout
                                          'om-row-layout :align :center
                                          :subviews (list (om-make-di 'om-simple-text :text "Y") yt))

                                         (when zt
                                           (om-make-layout
                                            'om-row-layout :align :center
                                            :subviews (list (om-make-di 'om-simple-text :text "Z") zt)))

                                         (when tt
                                           (om-make-layout
                                            'om-row-layout :align :center
                                            :subviews (list (om-make-di 'om-simple-text :text "Time" :size (omp 40 nil)) tt)))

                                         (om-make-view 'om-view :size (omp nil 20))
                                         (om-make-layout 'om-row-layout :subviews (list cb ob))
                                         )))))
    (setf (point-editor self) win)
    (om-show-window win)))

(defmethod edit-editor-point ((self bpf-editor) i)
  (let ((p (nth i (point-list (object-value self)))))
    (close-point-editor self)
    (open-point-editor self p)))

(defmethod edit-editor-point ((self bpc-editor) i)
  (let* ((object (object-value self))
         (p (nth i (point-list object))))
    (close-point-editor self)
    (open-point-editor self p :time (nth i (times object)))))


(defmethod reverse-points ((self bpf-editor))
  (time-sequence-reverse (object-value self))
  (editor-invalidate-views self)
  (update-to-editor (timeline-editor self) self)
  (report-modifications self))

;;;==========================
;;; USER
;;;==========================

(defmethod om-view-mouse-motion-handler ((self bpf-bpc-panel) position)
  (let ((editor (editor self)))
    (position-display editor position)))

(defmethod om-view-scrolled ((self bpf-bpc-panel) xy) nil)

(defmethod om-view-doubleclick-handler ((self bpf-bpc-panel) position)
  (let ((editor (editor self)))
    (cond ((equal (edit-mode editor) :hand)
           (reinit-ranges editor))
          (t (let ((selection (find-clicked-point-or-curve editor (object-value editor) position)))
               (set-selection editor selection)
               (om-invalidate-view self)
               (update-timeline-editor editor)
               (if (numberp selection)
                   (edit-editor-point editor selection)
                 (call-next-method))
               )))
    ))

(defmethod om-view-doubleclick-handler ((self bpc-panel) position)
  (let* ((editor (editor self))
         (obj (object-value editor)))
    (when obj
      (if (find-clicked-point-or-curve editor obj position)
          (call-next-method)
        (reinit-ranges editor))
      )))

(defmethod om-view-click-handler ((self bpf-bpc-panel) position)
  (let* ((editor (editor self))
         (p0 position)
         (t0 (get-internal-real-time))
         (obj (object-value editor)))
    (close-point-editor editor)

    (when obj
      (case (edit-mode editor)
        (:mouse
         (cond ((om-add-key-down)
                (store-current-state-for-undo editor)
                (let ((p (insert-point-at-pix editor obj position)))
                  (when p
                    (setf (selection editor) (list p))   ; (position p (point-list obj))
                    (report-modifications editor)
                    (om-invalidate-view self)
                    (update-timeline-editor editor)
                    ;;; move the new point
                    (om-init-temp-graphics-motion
                     self position nil :min-move 10
                     :motion #'(lambda (view pos)
                                 (declare (ignore view))
                                 (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                                       (dy (dpix-to-dy self (- (om-point-y p0) (om-point-y pos)))))
                                   (store-current-state-for-undo editor :action :move :item (selection editor))
                                   (move-editor-selection editor :dx dx :dy dy)
                                   (setf p0 pos)
                                   (editor-invalidate-views editor)
                                   (position-display editor pos)))
                     :release #'(lambda (view pos)
                                  (declare (ignore pos))
                                  (reset-undoable-editor-action editor)
                                  (round-point-values editor)
                                  (time-sequence-update-internal-times obj)
                                  (report-modifications editor)
                                  (om-invalidate-view view)
                                  (update-timeline-editor editor))
                     )
                    )))
               (t (let ((selection (find-clicked-point-or-curve editor obj position)))
                    (set-selection editor selection)
                    (om-invalidate-view self)
                    (update-timeline-editor editor)
                    ;;; move the selection or select rectangle
                    (if selection
                        (let ()
                          (om-init-temp-graphics-motion
                           self position nil
                           :motion #'(lambda (view pos)
                                       (declare (ignore view))
                                       (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                                             (dy (dpix-to-dy self (- (om-point-y p0) (om-point-y pos)))))
                                         (store-current-state-for-undo editor :action :move :item (selection editor))
                                         (move-editor-selection editor :dx dx :dy dy)
                                         (setf p0 pos)
                                         (position-display editor pos)
                                         (editor-invalidate-views editor)
                                         ))
                           :release #'(lambda (view pos)
                                        (declare (ignore pos))
                                        (reset-undoable-editor-action editor)
                                        (round-point-values editor)
                                        (time-sequence-update-internal-times obj)
                                        (report-modifications editor)
                                        (om-invalidate-view view)
                                        (update-timeline-editor editor))
                           :min-move 4)
                          )
                      (om-init-temp-graphics-motion
                       self position
                       (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
                       :min-move 10
                       :release #'(lambda (view position)
                                    (set-selection editor (points-in-area editor p0 position))
                                    (om-invalidate-view view)
                                    (update-timeline-editor editor)
                                    )
                       )
                      )))
               ))
        (:pen
         (cond ((om-add-key-down)
                (om-init-temp-graphics-motion
                 self position
                 (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
                 :min-move 10
                 :release #'(lambda (view position)
                              (set-selection editor (points-in-area editor p0 position))
                              (om-invalidate-view view)
                              (update-timeline-editor editor))
                 ))
               (t
                (set-selection editor nil)
                (when (equal (class-of editor) 'bpc-editor)
                  (setf (gesture-time-ref editor) t0))
                (let ((time-offset 0))
                  (when (and (bpc-p obj) (> (length (point-list obj)) 0))
                    ;find last time and add the default-offset
                    (setf time-offset (+ (get-obj-dur obj)
                                         ;;; (gesture-interval-time editor)
                                         (let ((tlist (time-sequence-get-internal-times obj)))
                                           (cond ((> (length tlist) 1) (- (car (last tlist)) (car (last tlist 2))))
                                                 ((= (length tlist) 1) (if (plusp (car tlist))
                                                                           (car tlist)
                                                                         (gesture-interval-time editor)))
                                                 (t 0)))
                                         )))
                  (store-current-state-for-undo editor)
                  (insert-point-at-pix editor obj position time-offset)

                  (om-init-temp-graphics-motion self position nil
                                                :motion #'(lambda (view pos)
                                                            (when (> (om-points-distance p0 pos) *add-point-distance-treshold*)
                                                              (add-point-at-pix  editor obj pos (+ (- (get-internal-real-time) t0) time-offset))
                                                              (setf p0 pos)
                                                              (om-invalidate-view view)
                                                              ))
                                                :release #'(lambda (view pos)
                                                             (declare (ignore pos))
                                                             (time-sequence-update-internal-times obj)
                                                             (report-modifications editor)
                                                             (om-invalidate-view view)
                                                             (update-timeline-editor editor)))

                  (report-modifications editor)
                  (om-invalidate-view self)
                  (update-timeline-editor editor)

                  )))
         )
        (:hand (let ((curr-pos position))
                 (om-init-temp-graphics-motion self position nil
                                               :motion #'(lambda (view pos)
                                                           (declare (ignore view))
                                                           (move-rulers self
                                                                        :dx (- (om-point-x pos) (om-point-x curr-pos))
                                                                        :dy (- (om-point-y pos) (om-point-y curr-pos)))
                                                           (setf curr-pos pos)
                                                           ))))
        ))))



(defmethod om-view-pan-handler ((self bpf-bpc-panel) position dx dy)
  (let ((fact 10))
    (move-rulers self :dx (* fact dx) :dy (* fact dy))))

(defmethod om-view-zoom-handler ((self bpf-bpc-panel) position zoom)
  (zoom-rulers self :dx (- 1 zoom) :dy (- 1 zoom) :center position))


(defmethod move-rulers ((self bpf-bpc-panel) &key (dx 0) (dy 0))
  (let* ((rx (x-ruler self))
         (ry (y-ruler self))
         (dxx (* (/ dx (w rx)) (- (v2 rx) (v1 rx))))
         (dyy (* (/ dy (h ry)) (- (v2 ry) (v1 ry)))))
    (unless (or (and (plusp dxx) (vmin rx) (= (vmin rx) (v1 rx)))
                (and (minusp dxx) (vmax rx) (= (vmax rx) (v2 rx))))
      (set-ruler-range rx
                       (if (vmin rx) (max (vmin rx) (- (v1 rx) dxx)) (- (v1 rx) dxx))
                       (if (vmax rx) (min (vmax rx) (- (v2 rx) dxx)) (- (v2 rx) dxx))))
    (unless (or (and (plusp dyy) (vmin ry) (= (vmin ry) (v1 ry)))
                (and (minusp dyy) (vmax ry) (= (vmax ry) (v2 ry))))
      (set-ruler-range ry
                       (if (vmin ry) (max (vmin ry) (- (v1 ry) dyy)) (+ (v1 ry) dyy))
                       (if (vmax ry) (min (vmax ry) (- (v2 ry) dyy)) (+ (v2 ry) dyy))))
    ))


(defmethod zoom-rulers ((panel bpf-bpc-panel) &key (dx 0.1) (dy 0.1) center)

  (let* ((position (or center (omp (* (w panel) .5) (* (h panel) .5))))
         (x-pos (* (pix-to-x panel (om-point-x position)) (scale-fact panel)))
         (y-pos (* (pix-to-y panel (om-point-y position)) (scale-fact panel)))
         (curr-w (- (x2 panel) (x1 panel)))
         (curr-h (- (y2 panel) (y1 panel)))
         (new-w (round (* curr-w (1+ dx))))
         (new-h (round (* curr-h (1+ dy))))

         (new-x1 (round (- x-pos (/ (* (- x-pos (x1 panel)) new-w) curr-w))))
         (new-y1 (round (- y-pos (/ (* (- y-pos (y1 panel)) new-h) curr-h)))))
    (set-x-ruler-range panel new-x1 (+ new-x1 new-w))
    (set-y-ruler-range panel new-y1 (+ new-y1 new-h))
    ))


(defmethod editor-key-action ((editor bpf-editor) key)
  (let ((panel (get-g-component editor :main-panel)))
    (case key
      (#\- (zoom-rulers panel :dx -0.1 :dy -0.1)) ;;; zoom out : the ruler gets bigger
      (#\+ (zoom-rulers panel :dx 0.1 :dy 0.1)) ;;; zoom in : the ruler gets smaller
      (:om-key-delete
       (store-current-state-for-undo editor)
       (delete-editor-selection editor)
       (time-sequence-update-internal-times (object-value editor))
       (report-modifications editor)
       (editor-invalidate-views editor)
       )
      (:om-key-esc
       (setf (selection editor) nil)
       (call-next-method) ;;; will also reset the cursor interval
       (editor-invalidate-views editor))
      (:om-key-left
       (store-current-state-for-undo editor :action :move :item (selection editor))
       (move-editor-selection editor :dx (/ (- (get-units (x-ruler panel) (if (om-shift-key-p) 400 40))) (scale-fact panel)))
       (time-sequence-update-internal-times (object-value editor))
       (update-timeline-editor editor)
       (editor-invalidate-views editor)
       (report-modifications editor)
       )
      (:om-key-right
       (store-current-state-for-undo editor :action :move :item (selection editor))
       (move-editor-selection editor :dx (/ (get-units (x-ruler panel) (if (om-shift-key-p) 400 40)) (scale-fact panel)))
       (time-sequence-update-internal-times (object-value editor))
       (update-timeline-editor editor)
       (editor-invalidate-views editor)
       (report-modifications editor))
      (:om-key-up
       (store-current-state-for-undo editor :action :move :item (selection editor))
       (move-editor-selection editor :dy (/ (get-units (y-ruler panel) (if (om-shift-key-p) 400 40)) (scale-fact panel)))
       (time-sequence-update-internal-times (object-value editor))
       (update-timeline-editor editor)
       (editor-invalidate-views editor)
       (report-modifications editor))
      (:om-key-down
       (store-current-state-for-undo editor :action :move :item (selection editor))
       (move-editor-selection editor :dy (/ (- (get-units (y-ruler panel) (if (om-shift-key-p) 400 40))) (scale-fact panel)))
       (time-sequence-update-internal-times (object-value editor))
       (update-timeline-editor editor)
       (editor-invalidate-views editor)
       (report-modifications editor))
      (:om-key-tab
       (let* ((p (position (edit-mode editor) +bpf-editor-modes+)))
         (when p
           (editor-set-edit-mode editor (nth (mod (1+ p) (length +bpf-editor-modes+)) +bpf-editor-modes+)))))
      (otherwise
       (when (timeline-editor editor)
         (editor-key-action (timeline-editor editor) key)
         ))
      )))



;;; Timeline specific code

(defmethod get-timeline-foldable-views ((self bpc-editor) &key obj time-ruler)
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
         (y-mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 120 15) :font (om-def-font :font1))))

    (set-g-component x-editor :main-panel x-panel)
    ;(setf (grid x-editor) t)
    (set-g-component x-editor :mousepos-txt x-mousepos-txt)
    (setf (y-ruler x-panel) x-y-ruler)
    (setf (related-views time-ruler) (append (list x-panel) (related-views time-ruler)))
   ; (reinit-ranges x-editor)

    (set-g-component y-editor :main-panel y-panel)
    ;(setf (grid y-editor) t)
    (set-g-component y-editor :mousepos-txt y-mousepos-txt)
    (setf (y-ruler y-panel) y-y-ruler)
    (setf (related-views time-ruler) (append (list y-panel) (related-views time-ruler)))
   ; (reinit-ranges y-editor)

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
                      (om-make-layout 'om-row-layout :subviews (list y-y-ruler y-panel) :ratios '(0.001 1)))))))



(defmethod get-color ((self bpf))
  (or (color self) (om-def-color :dark-gray)))


