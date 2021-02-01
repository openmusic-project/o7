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


;;;========================================================================
;;; MULTI-SEQ/POLY EDITOR
;;;========================================================================

(defclass poly-editor-mixin () ())

;;; The editor will inherit from data-stream-editor, althouhg MULTI-SEQ in not a data-stream...
(defclass multi-seq-editor (poly-editor-mixin chord-seq-editor) ())
(defmethod get-editor-class ((self multi-seq)) 'multi-seq-editor)

(defclass poly-editor (poly-editor-mixin voice-editor) ())
(defmethod get-editor-class ((self poly)) 'poly-editor)


;;;========================================================================
;;; COMMON FEATURES
;;;========================================================================

;;; From DATA-STREAM-EDITOR...
(defmethod y-range-for-object ((self multi-seq)) '(-100 100))   ;;; not used anyway

;;;=========================
;;; Multi-STAFF
;;;=========================

(defvar *default-inter-staff* 8)


(defmethod update-edit-params ((editor poly-editor-mixin))
  (let* ((n-voices (length (obj-list (object-value editor))))
         (new-list (make-list n-voices))
         (previous-y-list (list! (editor-get-edit-param editor :y-shift))))

    (loop for i from 0 to (1- n-voices)
          do (setf (nth i new-list) (or (nth i previous-y-list)
                                        *default-inter-staff*)))

    (editor-set-edit-param editor :y-shift new-list)
    ))


(defmethod accum-y-shift-list ((editor poly-editor-mixin))
  (let* ((y-shift (editor-get-edit-param editor :y-shift))
         (y-shift-list (list! y-shift))
         (ed-staff (editor-get-edit-param editor :staff))
         (accum-y-list ()))

    (loop with tmp-y = 0
          for i from 0 to (1- (length (obj-list (object-value editor))))
          do
          (let ((staff (if (listp ed-staff) (or (nth i ed-staff) (car ed-staff)) ed-staff)))
            (setf tmp-y (+ tmp-y (or (nth i y-shift-list) *default-inter-staff*)))
            (push tmp-y accum-y-list)
            (setf tmp-y (+ tmp-y (- (staff-higher-line staff) (staff-lower-line staff))))
            ))
    (reverse accum-y-list)
    ))

;; returns the y1-y2 pairs for all staffs
(defun make-staff-y-map (editor)

  (let* ((unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
         (ed-staff (editor-get-edit-param editor :staff)))

    (loop for ys in (accum-y-shift-list editor)
          for i from 0
          collect (let ((staff (if (listp ed-staff) (or (nth i ed-staff) (car ed-staff)) ed-staff)))
                    (staff-y-range staff ys unit)))
    ))


;;;---------------------------------------------
;;; SCORE-EDITOR REDEFINITIONS
(defmethod get-total-y-shift ((editor multi-seq-editor) voice-num)
  (nth voice-num (accum-y-shift-list editor)))
(defmethod get-total-y-shift ((editor poly-editor) voice-num)
  (nth voice-num (accum-y-shift-list editor)))
;;;---------------------------------------------


(defmethod editor-scroll-v ((self multi-seq-editor)) :v)
(defmethod editor-scroll-v ((self poly-editor)) :v)

(defmethod set-interior-size-from-contents ((self poly-editor-mixin))
  (let* ((fontsize (editor-get-edit-param self :font-size))
         (unit (font-size-to-unit fontsize))
         (score-view (get-g-component self :main-panel))
         (shift (car (last (accum-y-shift-list self)))))

    (when shift

      (oa::om-set-interior-size score-view
                                (omp (om-point-x (om-view-size score-view))
                                     (round (* shift unit))))

      (oa::om-set-interior-size (left-view score-view)
                                (omp (om-point-x (om-view-size score-view))
                                     (round (* shift unit))))

      )))

(defmethod init-editor-window :after ((self poly-editor-mixin))
  (set-interior-size-from-contents self))


;;;=========================
;;; LEFT VIEW (KEYS etc.)
;;;=========================

(defclass poly-left-score-view (left-score-view) ())

(defmethod initialize-instance ((self poly-left-score-view) &rest args)
  (call-next-method)
  (om-add-subviews
   self
   (om-make-graphic-object 'om-icon-button
                           :position (omp 4 4)
                           :size (omp 12 12)
                           :icon :edit+ :icon-pushed :edit+-pushed
                           :action #'(lambda (item)
                                       (declare (ignore item))
                                       (poly-editor-add-voice (editor self)))
                           ))
  )

;;;---------------------------------------------
;;; SCORE-EDITOR REDEFINITIONS
(defmethod left-score-view-class ((self multi-seq-editor)) 'poly-left-score-view)
(defmethod left-score-view-class ((self poly-editor)) 'poly-left-score-view)
;;;---------------------------------------------

;;; hack / for some reason the initerior size initialization doesn't work on windows...
#+windows
(defmethod om-set-scroll-position :before ((self poly-left-score-view) pos)
  (set-interior-size-from-contents (editor self)))


(defmethod om-view-click-handler ((self poly-left-score-view) position)

  (let* ((editor (editor self))
         (staff-y-map (make-staff-y-map editor))
         (selected-staff (position-if #'(lambda (range)
                                          (and (>= (om-point-y position) (car range))
                                               (<= (om-point-y position) (cadr range))))
                                      staff-y-map)))

    ;;; in calse the y-shift / staff list are not up-to-date with the list of voices
    (update-edit-params editor)

    (if selected-staff

        (let* ((unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
               (previous-y-list (editor-get-edit-param editor :y-shift)))

          (set-selection editor (nth selected-staff (obj-list (object-value editor))))

          (om-init-temp-graphics-motion
           self position nil :min-move 1
           :motion #'(lambda (view pos)
                       (declare (ignore view))

                       (when (and (> (om-point-y pos) 10)
                                  (< (om-point-y pos) (+ (om-v-scroll-position self) (h self) -10)))

                         (let ((y-diff-in-units (/ (- (om-point-y pos) (om-point-y position)) unit))
                               (new-list (copy-list previous-y-list)))

                           (setf (nth selected-staff new-list)
                                 (max (round *default-inter-staff* 2) (+ (nth selected-staff new-list) y-diff-in-units)))

                           (editor-set-edit-param editor :y-shift new-list)
                           (om-invalidate-view self)
                           (om-invalidate-view (main-view editor)))
                         )))
          )

      (set-selection editor nil))

    (om-invalidate-view self)
    (om-invalidate-view (main-view editor))
    ))


;;;=========================
;;; DISPLAY
;;;=========================

(defmethod poly-editor-draw-staff-in-editor-view ((editor poly-editor-mixin) (self score-view))
  (let* ((fontsize (editor-get-edit-param editor :font-size))
         (ed-staff (editor-get-edit-param editor :staff)))

    (loop for shift in (accum-y-shift-list editor)
          for i from 0
          do
          (let ((staff (if (listp ed-staff) (or (nth i ed-staff) (car ed-staff)) ed-staff)))

            (om-with-fg-color (when (find (nth i (obj-list (object-value editor)))
                                          (selection editor))
                                (om-def-color :selection))
              (draw-staff 0 0
                          shift
                          (w self) (h self)
                          fontsize
                          staff
                          :margin-l (margin-l self)
                          :margin-r (margin-r self)
                          :keys (keys self))
              )))))


;;;---------------------------------------------
;;; SCORE-EDITOR REDEFINITIONS

(defmethod draw-staff-in-editor-view ((editor multi-seq-editor) (self score-view))
  (poly-editor-draw-staff-in-editor-view editor self))
(defmethod draw-staff-in-editor-view ((editor poly-editor) (self score-view))
  (poly-editor-draw-staff-in-editor-view editor self))

(defmethod draw-tempo-in-editor-view ((editor poly-editor) (self score-view))
  (let* ((fontsize (editor-get-edit-param editor :font-size))
         (unit (font-size-to-unit fontsize)))

    (loop for voice in (obj-list (object-value editor))
          for shift in (accum-y-shift-list editor)
          do
          (draw-tempo voice (* 2 unit) (* shift unit) fontsize))
    ))

(defmethod draw-sequence ((object multi-seq) editor view unit &optional (voice-num 0))
  (loop for voice in (obj-list object)
        for shift in (accum-y-shift-list editor)
        do (draw-sequence voice editor view unit shift)))
;;;---------------------------------------------


;;;=========================
;;; ACTIONS
;;;=========================

(defmethod first-element-in-editor ((editor poly-editor-mixin))
  (car (chords (car (obj-list (object-value editor))))))


(defmethod next-element-in-editor ((editor poly-editor-mixin) (element chord-seq))
  (let* ((obj (object-value editor))
         (pos (position element (obj-list obj))))
    (or (nth (1+ pos) (obj-list obj))
        (car (obj-list obj)))))

(defmethod next-element-in-editor ((editor poly-editor-mixin) (element chord))
  (let* ((seq (find-if #'(lambda (v) (find element (chords v))) (obj-list (object-value editor))))
         (pos (position element (chords seq))))
    (or (nth (1+ pos) (chords seq))
        (car (chords seq)))))

(defmethod next-element-in-editor ((editor poly-editor-mixin) (element note))
  (let* ((seq (find-if #'(lambda (v) (find-if #'(lambda (c) (find element (notes c))) (chords v)))
                       (obj-list (object-value editor))))
         (chord (find-if #'(lambda (c) (find element (notes c))) (chords seq))))
    (when chord
      (let ((pos (position element (notes chord))))
        (or (nth (1+ pos) (notes chord))
            (car (notes chord)))))))




;;; REMOVE METHODS
(defmethod remove-from-obj ((self multi-seq) (item chord))
  (let ((cseq (find-if #'(lambda (cseq)
                           (find item (data-stream-get-frames cseq)))
                       (obj-list self)
                       )))
    (when cseq
      (remove-from-obj cseq item))
    ))

(defmethod remove-from-obj ((self multi-seq) (item note))
  (loop for cseq in (obj-list self)
        while (not (remove-from-obj cseq item))))

(defmethod remove-from-obj ((self multi-seq) (item chord-seq))
  (setf (obj-list self) (remove item (obj-list self))))

(defmethod remove-from-obj ((self poly) (item measure))
  (let ((voice (find-if #'(lambda (v) (find item (inside v))) (obj-list self))))
    (when voice
      (remove-from-obj voice item))
    ))



(defmethod score-editor-handle-voice-selection ((self poly-editor-mixin) direction)

  (if (om-option-key-p)

      ;;; change order: put selected voice(s) above the previous one
      (let ((obj (object-value self))
            (selected-voices  (loop for item in (selection self)
                                    when (subtypep (type-of item) 'chord-seq)
                                    collect item)))
        (when selected-voices

          (let ((posi (position (car selected-voices) (obj-list obj))))

            (when posi
              ;;; the group of selected voices will move to posi+1 or posi-1
              (loop for voice in selected-voices do
                    (score-editor-update-params-before-remove self voice)
                    (remove-from-obj obj voice))

              (let ((newpos (max 0
                                 (min (length (obj-list obj))
                                      (+ posi direction)))))

                (setf (obj-list obj)
                      (flat (insert-in-list (obj-list obj) selected-voices newpos)))

                (editor-set-edit-param self :y-shift
                                       (flat
                                        (insert-in-list (editor-get-edit-param self :y-shift)
                                                        (make-list (length selected-voices)
                                                                   :initial-element *default-inter-staff*)
                                                        newpos)))

                )))))

    ;;; change shift for selected voices
    (loop for obj in (selection self) do
          (when (subtypep (type-of obj) 'chord-seq)
            (let ((pos (position obj (obj-list (object-value self)))))
              (when pos
                (let ((curr (nth pos (editor-get-edit-param self :y-shift))))
                  (editor-set-edit-param self :y-shift
                                         (subs-posn (editor-get-edit-param self :y-shift)
                                                    pos
                                                    (max 2 (+ curr direction)))))
                ))))
    ))




(defmethod score-editor-update-params-before-remove ((self poly-editor-mixin) removed)
  (let ((pos (position removed (obj-list (object-value self)))))
    (when pos
      (editor-set-edit-param self :y-shift
                             (remove-nth pos (editor-get-edit-param self :y-shift)))
      )
    (set-interior-size-from-contents self)
    ))

;;; add chord/notes
(defmethod poly-editor-get-voice-at-pos ((self poly-editor-mixin) position)
  (when (obj-list (object-value self))
    (let* ((staff-y-map (make-staff-y-map self))
           (y (om-point-y position))
           (min-dist (min (abs (- y (car (car staff-y-map))))
                          (abs (- y (cadr (car staff-y-map))))))
           (pos 0))

      (loop for staff-range in (cdr staff-y-map)
            for p from 1
            do (let ((dist (min (abs (- y (car staff-range)))
                                (abs (- y (cadr staff-range))))))
                 (when (< dist min-dist)
                   (setf pos p
                         min-dist dist)))
            )
      (values (nth pos (obj-list (object-value self))) pos)
      )))


;;; voice can also be a list
(defmethod poly-editor-add-voice ((self poly-editor-mixin) &optional voice)
  (let* ((obj (object-value self))
         (new-voice (or voice (make-instance (voice-type obj)))))
    (store-current-state-for-undo self)
    (setf (obj-list obj) (append (obj-list obj) (list! new-voice))))
  (editor-invalidate-views self)
  (report-modifications self)
  (set-interior-size-from-contents self))

;;;---------------------------------------------
;;; SCORE-EDITOR REDEFINITIONS
(defmethod get-voice-at-pos ((self multi-seq-editor) position)
  (poly-editor-get-voice-at-pos self position))
(defmethod get-voice-at-pos ((self poly-editor) position)
  (poly-editor-get-voice-at-pos self position))

(defmethod get-all-voices ((self multi-seq-editor))
  (obj-list (object-value self)))
(defmethod get-all-voices ((self poly-editor))
  (obj-list (object-value self)))
;;;---------------------------------------------


;;; multi-staff: indicate selected-voice's staff in menu (if any)
(defmethod set-selection ((editor multi-seq-editor) (new-selection t))

  (call-next-method)

  (let ((selected-cseq (find 'chord-seq (selection editor) :key #'type-of :test #'subtypep)))
    ;;; the first found :)
    (when (and selected-cseq (get-g-component editor :staff-menu))
      (let ((ed-staff (editor-get-edit-param editor :staff)))
        (when (listp ed-staff)
          (om-set-selected-item
           (get-g-component editor :staff-menu)
           (or (nth (position selected-cseq (obj-list (object-value editor))) ed-staff)
               (car ed-staff)))
          ))))

  (update-score-inspector editor))


;;; add voices
(defmethod score-editor-paste ((self poly-editor-mixin) elements)

  (if (list-subtypep elements (voice-type (object-value self)))
      ;;; add voice(s)
      (poly-editor-add-voice self (mapcar #'om-copy elements))
    ;;; add chords
    (call-next-method)))

