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

;(defclass container ()
;  ((inside :accessor inside :initarg :inside :initform nil :documentation "the contents of the container")))

(defclass score-element (schedulable-object)
  (
   ;;; symbolic date and symbolic-dur make sense only if the object is in a context with tempo
   (symbolic-date :accessor symbolic-date :initarg :symbolic-date :initform nil
                  :documentation "date in symbolic musical time (ratio of beat)")
   (symbolic-dur :accessor symbolic-dur :initarg :symbolic-dur :initform nil
                 :documentation "duration in symbolic musical time (ratio of beat)")
   (symbolic-dur-extent :accessor symbolic-dur-extent :initarg :symbolic-dur-extent :initform 0
                        :documentation "an extension of the duration (used for tied chords)")

   ;;; parent is not :initarg so that it is not included in om-copy (cyclic references)
   (parent :accessor parent :initform nil :documentation "the container group, measure or voice of a score element")

   ;;; approximation of pitch (in division f a tone) used essentially for MIDI playback
   ;;; is modified by modification of the scale parameter
   (pitch-approx :accessor pitch-approx :initform 2)

   (extras :initarg :extras :initform nil :accessor extras :type list :documentation "some 'extra' score-elements attached to this element")

   ;;; bounding-box is a cached graphic information for score display
   (b-box :accessor b-box :initform nil)
   ))


(defmethod omng-delete ((box score-element))
  (om-midi::midi-all-keys-off)
  (call-next-method))

(defmethod release-previous-value ((self score-element))
  (om-midi::midi-all-keys-off)
  (call-next-method))


;;; this method to be defined according to the different objects' slot names etc.
;;; also allows compat with OM6 naming
(defmethod inside ((self score-element)) nil)

;;; only poly has more than 1 voice
(defmethod num-voices ((self score-element)) 1)

(defstruct b-box (x1) (x2) (y1) (y2))
(defmethod b-box-w (b) (- (b-box-x2 b) (b-box-x1 b)))
(defmethod b-box-h (b) (- (b-box-y2 b) (b-box-y1 b)))




