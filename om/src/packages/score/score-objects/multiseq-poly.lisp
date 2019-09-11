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

;;;===================================================
;;; SUPERIMPOSITION OF VOICES 
;;; POLY / MULTI-SEQ + MIXED-TYPES
;;;===================================================

(defclass* multi-seq (score-object collection) 
  ((obj-list :initarg :obj-list :initarg :voices
             :accessor obj-list :initform nil)))

(defclass* poly  (multi-seq) 
  ((obj-list :initarg :obj-list :initarg :chord-seqs
             :accessor obj-list :initform nil)))

(defmethod chord-seqs ((self multi-seq)) (obj-list self))
(defmethod voices ((self poly)) (obj-list self))

(defmethod voice-type ((self multi-seq)) 'chord-seq)
(defmethod voice-type ((self poly)) 'voice)

(defmethod inside ((self multi-seq)) (obj-list self))
(defmethod num-voices ((self multi-seq)) (length (obj-list self)))
(defmethod get-obj-dur ((self multi-seq)) (apply 'max (mapcar 'get-obj-dur (obj-list self))))

(defmethod objfromobjs ((model multi-seq) (target multi-seq))
  (setf (obj-list target)
        (loop for obj in (obj-list model) collect
              (objfromobjs obj (make-instance (voice-type target)))))
  target)

;;;===================================================
;;; PLAY
;;;===================================================
(defmethod play-obj? ((self multi-seq)) t)

(defmethod get-action-list-for-play ((object multi-seq) interval &optional parent)
  (loop for voice in (obj-list object)
        append (get-action-list-for-play voice interval parent)))

;;;===================================================
;;; TIME-SEQUENCE METHODS APPLIED TO POLYPHONIES
;;;===================================================

(defmethod time-sequence-update-obj-dur ((self multi-seq))
  (loop for voice in (obj-list self) do
        (time-sequence-update-obj-dur voice)))

(defmethod time-sequence-reorder-timed-item-list ((self multi-seq))
  (loop for voice in (obj-list self) do
        (time-sequence-reorder-timed-item-list voice)))

(defmethod time-sequence-update-internal-times ((self multi-seq) &optional (interpol-mode :constant-speed) (duration 10000) (modif-time nil))
  (loop for voice in (obj-list self) do
        (time-sequence-update-internal-times voice interpol-mode duration modif-time)))