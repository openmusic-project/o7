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
;;; CHORD-SEQ EDITOR / GENERAL SCORE EDITOR
;;;========================================================================

(defclass chord-seq-editor (score-editor data-stream-editor) ())
(defmethod get-editor-class ((self chord-seq)) 'chord-seq-editor)

(defclass chord-seq-panel (score-view stream-panel) ()
  (:default-initargs :keys nil :margin-l nil :margin-r 1))

(defmethod editor-view-class ((self chord-seq-editor)) 'chord-seq-panel)

(defmethod edit-time-? ((self chord-seq-editor)) t)

(defmethod editor-window-init-size ((self chord-seq-editor)) (om-make-point 650 300))

;;; this will just disable the display-mode menu
(defmethod frame-display-modes-for-object ((self data-stream-editor) (object score-element)) '(:chords))

(defmethod object-default-edition-params ((self chord-seq))
  (append (call-next-method)
          '((:grid nil) (:grid-step 1000)
            (:stems t)
            (:offsets :grace-note))))

(defmethod object-default-edition-params ((self multi-seq))
  (append (call-next-method)
          '((:grid nil) (:grid-step 1000)
            (:stems t)
            (:offsets :small-notes))))

;;; offset can be :shift, :small-notes, or :hidden

(defmethod editor-with-timeline ((self chord-seq-editor)) nil)

;;;=========================
;;; LEFT-VIEW
;;;=========================

(defclass left-score-view (score-view) ())

(defmethod left-score-view-class ((self chord-seq-editor)) 'left-score-view)

(defmethod om-draw-contents ((self left-score-view))

  (let* ((editor (editor self))
         (scrolled (> (x1 (get-g-component editor :main-panel)) 0)))

    (draw-staff-in-editor-view editor self)

    (draw-tempo-in-editor-view editor self)

    (when scrolled
      (om-draw-rect (- (w self) 20) 0 20 (h self)
                    :fill t :color (om-make-color .9 .9 .9 .5))
      (om-draw-string (- (w self) 15) 10 "...")
      (om-draw-string (- (w self) 15) (- (h self) 10) "..."))
    ))

(defmethod editor-scroll-v ((self chord-seq-editor)) nil)

(defmethod make-left-panel-for-object ((editor chord-seq-editor) (object score-element))
  (om-make-view (left-score-view-class editor) :size (omp (* 2 (editor-get-edit-param editor :font-size)) nil)
                :direct-draw t
                :bg-color (om-def-color :white)
                :editor editor
                :scrollbars (editor-scroll-v editor)
                :margin-l 1 :margin-r nil :keys t :contents nil
                ))

(defmethod om-view-scrolled ((self left-score-view) pos)
  ;;; for some reason the initerior size initialization doesn't work on windows...
  #+windows(set-interior-size-from-contents (editor self))

  (om-set-scroll-position
   (get-g-component (editor self) :main-panel)
   (omp 0 (cadr pos))))


(defmethod om-view-click-handler ((self left-score-view) position)

  ;;; is the staff-line selected ?
  (let* ((editor (editor self))
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
         (y-shift (editor-get-edit-param editor :y-shift))
         (staff (editor-get-edit-param editor :staff))
         (staff-y-minmax (staff-y-range staff y-shift unit)))

    (if (and (>= (om-point-y position) (car staff-y-minmax))
             (<= (om-point-y position) (cadr staff-y-minmax)))

        (progn
          (set-selection editor (object-value editor))
          (om-init-temp-graphics-motion
           self position nil :min-move 1
           :motion #'(lambda (view pos)
                       (declare (ignore view))
                       (when (and (> (om-point-y pos) 10)
                                  (< (om-point-y pos) (- (h self) 10)))

                         (let ((y-diff-in-units (/ (- (om-point-y pos) (om-point-y position)) unit)))

                           (editor-set-edit-param editor :y-shift (max 0 (+ y-shift y-diff-in-units)))
                           (om-invalidate-view self)
                           (om-invalidate-view (main-view editor)))
                         ))))
      (set-selection editor nil))

    (om-invalidate-view self)
    (om-invalidate-view (main-view editor))
    ))


;;;=========================
;;; EDITOR
;;;=========================

;;; add a "grid" and an offset-display option
(defmethod make-editor-controls ((editor chord-seq-editor))
  (make-score-display-params-controls editor))


(defmethod update-view-from-ruler ((self x-ruler-view) (view chord-seq-panel))
  (call-next-method)
  (om-invalidate-view (left-view view)))


;;; just like stream-panel (not like score-view)
(defmethod om-view-zoom-handler ((self chord-seq-panel) position zoom)
  (zoom-rulers self :dx (- 1 zoom) :dy 0 :center position))


;;; leave some space (-200) for the first note...
(defmethod default-editor-x-range ((self chord-seq-editor))
  (let ((play-obj (get-obj-to-play self)))
    (if play-obj
        (list -200 (+ (get-obj-dur play-obj) (default-editor-min-x-range self)))
      (list (vmin self) (or (vmax self) (default-editor-min-x-range self))))))


(defmethod update-to-editor ((editor chord-seq-editor) (from t))
  (call-next-method)
  (editor-invalidate-views editor))

(defmethod editor-invalidate-views ((self chord-seq-editor))
  (om-invalidate-view (main-view self))) ;; mmm..


;;; rebuild when font-size has changed, in order to adjust the left-view...
(defmethod set-font-size ((self chord-seq-editor) size)
  (call-next-method)
  (build-editor-window self)
  (init-editor-window self))


(defmethod add-chords-allowed ((self chord-seq-editor)) t)


(defmethod om-view-click-handler ((self chord-seq-panel) position)
  (or (handle-selection-extent self position)
      (call-next-method))) ;;; => score-view


;;; in data-stream-editor the selection is an index to elements in the frame sequence
;;; here in chord-seq editor the selection s a score-element
(defmethod first-element-in-editor ((editor chord-seq-editor))
  (car (chords (object-value editor))))

;;; called by the "tab" action
(defmethod next-element-in-editor ((editor chord-seq-editor) (element t))
  (first-element-in-editor editor))

(defmethod next-element-in-editor ((editor chord-seq-editor) (element chord))
  (let* ((seq (object-value editor))
         (pos (position element (chords seq))))
    (or (nth (1+ pos) (chords seq))
        (car (chords seq)))))

(defmethod next-element-in-editor ((editor chord-seq-editor) (element note))
  (let* ((seq (object-value editor))
         (chord (find-if #'(lambda (c) (find element (notes c))) (chords seq))))
    (when chord
      (let ((pos (position element (notes chord))))
        (or (nth (1+ pos) (notes chord))
            (car (notes chord)))))))


(defmethod stems-on-off ((self chord-seq-editor))
  (editor-set-edit-param self :stems (not (editor-get-edit-param self :stems)))
  (editor-invalidate-views self))


(defmethod editor-key-action ((editor chord-seq-editor) key)
  (case key

    (#\A (align-chords-in-editor editor))

    (#\S (stems-on-off editor))

    (otherwise (call-next-method)) ;;; => score-editor
    ))


;;; called at add-click
(defmethod get-chord-from-editor-click ((self chord-seq-editor) position)

  (let ((time-seq (get-voice-at-pos self position))
        (time-pos (pixel-to-time (get-g-component self :main-panel) (om-point-x position))))

    (or
     ;;; there's a selected chord near the click
     (find-if #'(lambda (element)
                  (and (typep element '(or chord r-rest))
                       (b-box element)
                       (>= (om-point-x position) (b-box-x1 (b-box element)))
                       (<= (om-point-x position) (b-box-x2 (b-box element)))))
              (selection self))

     ;;; else, make a new chord
     (when (add-chords-allowed self)
       (let ((new-chord (time-sequence-make-timed-item-at time-seq time-pos)))
         (setf (notes new-chord) nil)
         ;;; notes = NIL here so the duration will be 0 at updating the time-sequence
         (time-sequence-insert-timed-item-and-update time-seq new-chord (find-position-at-time time-seq time-pos))
         new-chord))

     )))


;;; redefines from data-stream-editor
(defmethod move-editor-selection ((self chord-seq-editor) &key (dx 0) (dy 0))

  (unless (equal (editor-play-state self) :stop)
    (close-open-chords-at-time (get-selected-chords self)
                               (get-obj-time (object-value self))
                               (object-value self)))

  (unless (zerop dx)
    (loop for item in (selection self)
          when (typep item 'chord)
          do (item-set-time item (max 0 (round (+ (item-get-time item) dx))))))

  (unless (zerop dy)
    (let ((step (or (step-from-scale (editor-get-edit-param self :scale)) 100))
          (notes (loop for item in (selection self) append (get-notes item))))
      (loop for n in notes do
            (setf (midic n) (+ (midic n) (* dy step))))))
  )


(defmethod score-editor-change-selection-durs ((self chord-seq-editor) delta)
  (when (editor-get-edit-param self :duration-display)

    (unless (equal (editor-play-state self) :stop)
      (close-open-chords-at-time (get-selected-chords self)
                                 (get-obj-time (object-value self))
                                 (object-value self)))

    (let ((notes (loop for item in (selection self) append (get-notes item))))
      (loop for n in notes
            do (setf (dur n) (max (abs delta) (round (+ (dur n) delta)))))
      (time-sequence-update-obj-dur (object-value self))
      )))


(defmethod score-editor-delete ((self chord-seq-editor) element)
  (remove-from-obj (object-value self) element))

(defmethod editor-sort-frames ((self chord-seq-editor))
  (time-sequence-reorder-timed-item-list (object-value self)))


;;; paste command
;;; also works for multi-seq
(defmethod score-editor-paste ((self chord-seq-editor) elements)

  (let ((chords (mapcar #'om-copy
                        (sort
                         (loop for item in elements append (get-tpl-elements-of-type item '(chord)))
                         #'< :key #'onset)))
        (view (get-g-component self :main-panel)))

    (if chords

        (let* ((t0 (onset (car chords)))
               (paste-pos (get-paste-position view))
               (p0 (if paste-pos
                       (pixel-to-time view (om-point-x paste-pos))
                     (+ t0 200)))
               (cs (get-voice-at-pos self (or paste-pos (omp 0 0)))))

          (loop for c in chords do
                (item-set-time c (+ p0 (- (item-get-time c) t0))))

          (set-paste-position (omp (time-to-pixel view (+ p0 200))
                                   (if paste-pos (om-point-y paste-pos) 0))
                              view)

          (store-current-state-for-undo self)
          (set-chords cs (sort (append (chords cs) chords) #'< :key #'onset))
          (report-modifications self)
          t)

      (om-beep))
    ))


(defmethod align-chords-in-editor ((self chord-seq-editor))
  (when (selection self)
    (store-current-state-for-undo self)
    (let ((selected-chords
           (loop for elt in (selection self)
                 append (get-tpl-elements-of-type elt 'chord))))
      (align-chords-in-sequence
       (object-value self)
       (or (editor-get-edit-param self :grid-step) 100)
       selected-chords)
      (report-modifications self)
      (editor-invalidate-views self))
    ))

(defmethod align-command ((self chord-seq-editor))
  (when (selection self)
    #'(lambda ()
        (align-chords-in-editor self))
    ))

;;;=========================
;;; IMPORT/EXPORT
;;;=========================

(defmethod import-menus ((self chord-seq-editor))
  (list
   (om-make-menu-item "MIDI" #'(lambda () (editor-import-midi self)))
   (om-make-menu-item "MusicXML" #'(lambda () (editor-import-musicxml self)))
   ))

(defmethod export-menus ((self chord-seq-editor))
  (list
   (om-make-menu-item "MIDI" #'(lambda () (editor-export-midi self)))
   (om-make-menu-item "MusicXML" #'(lambda () (editor-export-musicxml self)))
   ))


(defmethod editor-import-midi ((self chord-seq-editor))
  (objfromobjs
   (import-midi) ;; => MIDI-TRACK
   (object-value self))
  (report-modifications self)
  (editor-invalidate-views self))

(defmethod editor-import-musicxml ((self chord-seq-editor))
  (objfromobjs
   (import-musicxml) ;; => MIDI-TRACK
   (object-value self))
  (report-modifications self)
  (editor-invalidate-views self))

(defmethod editor-export-midi ((self chord-seq-editor))
  (save-as-midi (object-value self) nil))

(defmethod editor-export-musicxml ((self chord-seq-editor))
  (export-musicxml (object-value self)))


;;;=========================
;;; DISPLAY
;;;=========================

(defmethod data-stream-get-x-ruler-vmin ((self chord-seq-editor)) -200)

(defmethod draw-score-object-in-editor-view ((editor chord-seq-editor) view unit)
  (let ((obj (if (multi-display-p editor)
                 (nth (stream-id view) (multi-obj-list editor))
               (object-value editor))))

    ;;; grid
    (when (editor-get-edit-param editor :grid)
      (draw-grid-on-score-editor editor view))

    (draw-sequence obj editor view unit)))


(defmethod draw-grid-on-score-editor ((editor chord-seq-editor) view)
  (let ((grid-step (or (editor-get-edit-param editor :grid-step) 100))) ;; just in case..
    (loop for x from (* (ceiling (x1 view) grid-step) grid-step)
          to (* (floor (x2 view) grid-step) grid-step)
          by grid-step
          do (let ((x-pix (time-to-pixel view x)))
               (om-draw-line x-pix 0 x-pix (h view) :color (om-def-color :gray) :style '(2 2))
               )
          )))


;;; redefined for other objects
(defmethod draw-sequence ((object chord-seq) editor view unit &optional (force-y-shift nil))

  (let ((font-size (editor-get-edit-param editor :font-size))
        (staff (editor-get-edit-param editor :staff))
        (scale (editor-get-edit-param editor :scale))
        (chan (editor-get-edit-param editor :channel-display))
        (vel (editor-get-edit-param editor :velocity-display))
        (port (editor-get-edit-param editor :port-display))
        (dur (editor-get-edit-param editor :duration-display))
        (stems (editor-get-edit-param editor :stems))
        (offsets (editor-get-edit-param editor :offsets))
        (y-u (or force-y-shift (editor-get-edit-param editor :y-shift))))

    (when (listp staff)
      (setf staff (or (nth (position object (get-voices (object-value editor))) staff)
                      (car staff))))

    ;;; NOTE: so far we don't build/update a bounding-box for the chord-seq itself (might be useful in POLY)..
    (loop for chord in (chords object) do
          (setf
           (b-box chord)
           (draw-chord chord
                       (date chord)
                       0 y-u
                       0 0
                       (w view) (h view)
                       font-size
                       :staff staff :scale scale
                       :draw-chans chan
                       :draw-vels vel
                       :draw-ports port
                       :draw-durs dur
                       :stem stems
                       :selection (if (find chord (selection editor)) T
                                    (selection editor))
                       :time-function #'(lambda (time) (time-to-pixel view time))
                       :offsets offsets
                       :build-b-boxes t
                       ))
          ;(draw-b-box chord)
          ;(mapcar 'draw-b-box (inside chord))

          )))

