;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
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
;;; CHORD EDITOR
;;;========================================================================


(defclass chord-editor (score-editor play-editor-mixin) 
  ((temp-arp-chord :accessor temp-arp-chord :initform nil)))


(defmethod object-has-editor ((self chord)) t)
(defmethod get-editor-class ((self chord)) 'chord-editor)

(defmethod object-default-edition-params ((self chord))
  (append (call-next-method)
          '((:chord-mode :chord))))

;;; chord-mode:
;;; - chord = play the chord (with offsets if there are offsets)
;;; - arp-up = play arpegio up
;;; - arp-up = play arpegio down
;;; - arp-order = play arpegio in order


(defmethod make-editor-window-contents ((editor chord-editor))
  
  (let* ((panel (om-make-view 'score-view 
                              :size (omp 50 100) 
                              :direct-draw t 
                              :bg-color (om-def-color :white) 
                              :scrollbars nil
                              :editor editor))
         
         (bottom-area (make-score-display-params-controls editor)))

    (set-g-component editor :main-panel panel)
    (om-make-layout 'om-row-layout :ratios '(99 1) 
                    :subviews 
                    (list 
                     (om-make-layout 'om-column-layout 
                                     :ratios '(99 1)
                                     :subviews (list panel bottom-area))
                     (call-next-method)))
    ))


(defmethod make-score-display-params-controls ((editor chord-editor))
  (om-make-layout 
   'om-row-layout
   :subviews 
   (list 
    (call-next-method)
    :separator
    (om-make-layout 
     'om-row-layout 
     :subviews (list 
                (om-make-di 'om-simple-text :text "Chord mode" 
                            :size (omp 70 20)
                            :font (om-def-font :font1))
                (om-make-di 'om-popup-list :items '(:chord :arp-up :arp-down :arp-order) 
                            :size (omp 80 24) :font (om-def-font :font1)
                            :value (editor-get-edit-param editor :chord-mode)
                            :di-action #'(lambda (list) 
                                           (editor-set-edit-param editor :chord-mode (om-get-selected-item list))
                                           ;;; update / redisplay something ?? 
                                           ))))
    nil
    )))

(defmethod update-to-editor ((editor chord-editor) (from t))
  (call-next-method)
  (editor-invalidate-views editor))

(defmethod editor-invalidate-views ((self chord-editor))
  (call-next-method)
  (om-invalidate-view (get-g-component self :main-panel)))


;;; called at add-click
(defmethod get-chord-from-editor-click ((self chord-editor) position) 
  (declare (ignore position))
  (object-value self))


(defmethod score-editor-delete ((self chord-editor) element) 
  (let ((c (object-value self)))
    (if (equal element c)
        (setf (notes c) nil)
      (setf (notes c) (remove element (notes c))))))

 
(defmethod move-editor-selection ((self chord-editor) &key (dx 0) (dy 0))
  
  (declare (ignore dx))

  (let* ((chord (object-value self))
         (step (or (step-from-scale (editor-get-edit-param self :scale)) 100))
         (notes (if (find chord (selection self))
                    (notes chord)
                  (selection self))))

    (unless (zerop dy)
      (loop for n in notes do
            (setf (midic n) (+ (midic n) (* dy step)))))
    ))


(defmethod score-editor-change-selection-durs ((self chord-editor) delta) 
  (let* ((chord (object-value self))
         (notes (if (find chord (selection self))
                    (notes chord)
                  (selection self))))
    
    (loop for n in notes
          do (setf (dur n) (max (abs delta) (round (+ (dur n) delta)))))
    ))


;;; SPECIAL/SIMPLE CASE FOR CHORD-EDITOR
(defmethod draw-score-object-in-editor-view ((editor chord-editor) view unit)

  (let ((chord (object-value editor)))
    
    (setf 
     (b-box chord)
     (draw-chord chord
                 0 
                 0 (editor-get-edit-param editor :y-shift)
                 0 0 
                 (w view) (h view) 
                 (editor-get-edit-param editor :font-size) 
                 :staff (editor-get-edit-param editor :staff)
                 :scale (editor-get-edit-param editor :scale)
                 :draw-chans (editor-get-edit-param editor :channel-display)
                 :draw-vels (editor-get-edit-param editor :velocity-display)
                 :draw-ports (editor-get-edit-param editor :port-display)
                 :draw-durs (editor-get-edit-param editor :duration-display)
                 :draw-dur-ruler t
                 :selection (if (find chord (selection editor)) T 
                              (selection editor))
                 :offsets (editor-get-edit-param editor :offsets)
                 :time-function #'(lambda (time) 
                                    (+ (/ (w view) 2) 
                                       (if (notes chord)
                                           (* (/ (- (w view) 80) 2) 
                                              (/ time (list-max (mapcar 
                                                                 #'(lambda (n) (+ (dur n) (offset n)))
                                                                 (notes chord)))))
                                         0))
                                    )
                 :build-b-boxes t
                 ))
    
    ; (draw-b-box chord)
    ))


;;;====================================
;;; PLAY
;;;====================================

(defmethod get-obj-to-play ((self chord-editor)) 
  (or (temp-arp-chord self) (object-value self)))

(defmethod start-editor-callback ((self chord-editor))
  (let ((chord (object-value self)))
    (setf (temp-arp-chord self)
          (case (editor-get-edit-param self :chord-mode)
            (:arp-order
             (make-instance 'chord-seq 
                            :lmidic (lmidic chord) 
                            :lchan (lchan chord) :lport (lport chord)
                            :lvel (lvel chord)
                            :lonset '(0 200) :ldur 200))
            (:arp-up
             (make-instance 'chord-seq 
                            :lmidic (sort (lmidic chord) #'<)
                            :lchan (lchan chord) :lport (lport chord)
                            :lvel (lvel chord)
                            :lonset '(0 200) :ldur 200))
            (:arp-down
             (make-instance 'chord-seq 
                            :lmidic (sort (lmidic chord) #'>) 
                            :lchan (lchan chord) :lport (lport chord)
                            :lvel (lvel chord)
                            :lonset '(0 200) :ldur 200))
            (t chord)
            )))
  (call-next-method))


(defmethod editor-pause ((self chord-editor))
  (editor-stop self))


