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

;;;===================================================
;;; CHORD-SEQ IS JUST A SPECIAL KIND OF DATA-STREAM
;;;===================================================

;;; some of the slots :initargs of INTERNAL-CHORD-SEQ are hidden in the graphical interface

;;; (almost) all slot accessors are redefined below in this file


(defclass internal-chord-seq (score-element internal-data-stream)
  ((Lmidic :initform '((6000)) :initarg :Lmidic :type list :documentation "pitches (mc): list or list of lists")
   (Lonset :initform '(0 1000) :initarg :Lonset :type list :documentation "onsets (ms): list")
   (Ldur :initform '((1000)) :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (Lvel :initform 100 :initarg :Lvel :type list :documentation "velocities (0-127): list or list of lists")
   (Loffset :initform 0 :initarg :Loffset :type list :documentation "offsets (ms): list or list of lists")
   (Lchan :initform 1 :initarg :Lchan :type list :documentation "MIDI channels (1-16): list or list of lists")
   (Lport :initform nil :initarg :Lport :type list :documentation "MIDI ports: list or list of lists")
   (Llegato :accessor Llegato :initform nil :initarg :Llegato :initarg :legato :documentation "relative chords duration (0.0-... or NIL)")   ;;; this one has no redefined accessor (do it?)
   (name :accessor name :initform nil :initarg :name :documentation "the name of this voice")
   )
  ;; (:default-initargs :default-frame-type 'chord)
  )

(defmethod initialize-instance ((self internal-chord-seq) &rest args)
  (setf (default-frame-type self) 'chord)
  (call-next-method))

;;; redefines only visible :initargs
(defclass* chord-seq (internal-chord-seq)

  ((Lmidic :initform '((6000)) :initarg :LMidic :type list :documentation "pitches (mc): list or list of lists")
   (Lonset :initform '(0 1000) :initarg :LOnset :type list :documentation "onsets (ms): list")
   (Ldur :initform '((1000)) :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (Lvel :initform 100 :initarg :LVel :type list :documentation "velocities (0-127): list or list of lists"))

  (:documentation "
A sequence of chords.

Time is expressed in absolute dates and durations. (For rhytmic structures see the VOICE object.)

CHORD-SEQ is defined with:

- <lmidic>: a list of list of pitches (midicents: 100 = 1 half-tone - 6000 = C3); e.g. '((6000 6500) (6100 6700 7100)). Each pitch list is considered as a chord in teh sequence. Single-note sequences can be specified with simple list of pitches, e.g. (6000 61000 6800) ; in this case each pitch will be considered as a single-note chord.
- <lonsets>: a list of onsets in milliseconds (one for each chord). If the list is shorter than the list of pitch, the last interval is repeated.
- <ldur>: a list or list of lists values (in milliseconds) expressing chords durations or note durations inside each chord.
- <lvel>: a list or list of lists of values (MIDI velocity from 0 to 127) expressing chords velocities or note durations inside each chord.
- <loffset>: a list or list of lists of values (milliseconds) expressing note offsets for the notes inside each chord.
- <lchan>: a list or list of lists of values (1-16) expressing MIDI channels for each chord or notes inside each chord.
- <lport>: a list or list of lists of values expressing MIDI ports for each chord or notes inside each chord.
- <llegato>: list of float numbers > 0.0 (or NIL), indicating the duration of chords as a ratio of inter-onsets time intervals. When applied, the ldur and loffset inputs are ignored, and note durations are set with regard to the legato value.
Note: for compatibility, legato can also be specified as an integer [0-100], and is then considered a percentage value.

All values (excepted lonsets and legato) are returned (in the box outputs) as list of lists (one value for each note, missing values computed from previous notes or chords).

Note: the last value of <lonset> is the end of the sequence (end-time of the longest note).

Internally most of these values are just used to build a list of CHORD objects, accessible with GET-CHORDS.

"))

(defmethod chords ((self chord-seq)) (time-sequence-get-timed-item-list self))

(defmethod set-chords ((self chord-seq) (chords list))
  (data-stream-set-frames self chords))

(defmethod inside ((self chord-seq)) (chords self))


(defmethod additional-class-attributes ((self chord-seq)) '(Loffset Lchan Lport Llegato extras))

;; (defmethod data-stream-frames-slot ((self chord-seq)) 'inside)

;;; redefined from time-sequence
(defmethod time-sequence-default-duration ((self chord-seq)) 1000)

(defmethod save-slot-value ((self chord-seq) slot-name val)
  (if (and (member slot-name '(lmidic lvel ldur lchan lport loffset llegato extras))
           (all-equal val))
      (omng-save (car val))
    (call-next-method)))


(defmethod do-initialize ((self chord-seq) &key Lmidic Lvel Loffset Ldur Lonset Lchan Lport Llegato)

  (let ((midics (list! Lmidic))
        (vels (list! Lvel))
        (durs (list! Ldur))
        (offsets (list! Loffset))
        (chans (list! Lchan))
        (ports (list! Lport))
        (legatos (list! Llegato)))

    (let ((chord-list
           (cond

            ;;; special cases..
            ((list-subtypep Lmidic '(chord)) ;;; this is probably a mistake but we can deal with it
             ;(om-print "<lmidic> slot initialized with a list of chords." "Warning")
             (om-copy Lmidic))

            ((list-subtypep LMidic '(note))
             ;(om-print "<lmidic> slot initialized with a list of notes." "Warning")
             (mapcar #'(lambda (n) (ObjfromObjs n (make-instance 'chord))) Lmidic))

            (t
             (loop while (or midics vels durs offsets ports)
                   for midic = (or (pop midics) midic)
                   for vel = (or (pop vels) vel)
                   for dur = (or (pop durs) dur)
                   for offset = (or (pop offsets) offset)
                   for chan = (or (pop chans) chan)
                   for port = (or (pop ports) port)
                   collect (make-instance
                            'chord
                            :Lmidic (list! midic)
                            :Lvel (list! vel)
                            :Ldur (list! dur)
                            :Loffset (list! offset)
                            :Lchan (list! chan)
                            :Lport (list! port)
                            )))
            ))) ;;; end chord-list definition


      (let* ((sorted-list (sort (mat-trans (list (copy-list (first-n Lonset (length chord-list)))
                                                 (first-n chord-list (length Lonset))))
                                #'< :key 'car))
             (unsorted-chords (nthcdr (length Lonset)  chord-list))
             (sorted-chords (append (mapcar #'second sorted-list) unsorted-chords))
             (sorted-onsets (mapcar #'first sorted-list)))

        (loop with defdelay = (if (>= (length sorted-onsets) 2)
                                  (- (car (last sorted-onsets)) (car (last sorted-onsets 2)))
                                1000) ;;; the default delay between two chords if not specified otherwise
              with defstart = (or (pop sorted-onsets) 0)
              for chord in sorted-chords
              for onset = defstart then (or (pop sorted-onsets)  (+ onset defdelay))
              for next-ontset = (or (first sorted-onsets) (+ onset defdelay))
              for legato = (or (pop legatos) legato)
              do (setf (date chord) onset)
              do (when (and legato (> legato 0))
                   (if (integerp legato) (setf legato (/ legato 100.0)))
                   (let ((dur (round (* (- next-ontset onset) legato))))
                     (loop for note in (notes chord)
                           do (setf (offset note) 0
                                    (dur note) dur))))
              )

        (set-chords self sorted-chords)

        self))))


(defmethod initialize-instance ((self chord-seq) &rest initargs)

  (call-next-method)

  (when t ; initargs
    (do-initialize self
                   :Lmidic (slot-value self 'Lmidic)
                   :Lvel (slot-value self 'Lvel)
                   :Ldur (slot-value self 'Ldur)
                   :Lonset (slot-value self 'Lonset)
                   :Loffset (slot-value self 'Loffset)
                   :Lchan (slot-value self 'Lchan)
                   :Lport (slot-value self 'Lport)
                   :Llegato (slot-value self 'Llegato)
                   ))

  (setf (slot-value self 'Lmidic) nil  (slot-value self 'Lvel) nil
        (slot-value self 'Loffset) nil  (slot-value self 'Ldur) nil
        (slot-value self 'Lonset) nil (slot-value self 'Lchan) nil
        (slot-value self 'Lport) nil (slot-value self 'Llegato) nil)
  self)


;========================
; GET/SET ACCESSORS
;========================

(defmethod Lmidic ((self chord-seq))
  (loop for chord in (chords self)
        collect (Lmidic chord)))

(defmethod Lvel ((self chord-seq))
  (loop for chord in (chords self)
        collect (Lvel chord)))

(defmethod Ldur ((self chord-seq))
  (loop for chord in (chords self)
        collect (Ldur chord)))

(defmethod Loffset ((self chord-seq))
  (loop for chord in (chords self)
        collect (Loffset chord)))

(defmethod Lchan ((self chord-seq))
  (loop for chord in (chords self)
        collect (Lchan chord)))

(defmethod Lport ((self chord-seq))
  (loop for chord in (chords self)
        collect (Lport chord)))

(defmethod Lonset ((self chord-seq))
  (nconc (loop for chord in (chords self)
               collect (item-get-time chord))
         (list (get-obj-dur self))))



(defmethod (setf Lmidic) ((Lmidic list) (self chord-seq))
  (do-initialize self
                 :Lmidic Lmidic
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Lvel) ((Lvel list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel Lvel
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Loffset) ((Loffset list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset Loffset
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Ldur) ((Ldur list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur ldur
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Lonset) ((Lonset list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset Lonset
                 ))

(defmethod (setf Lchan) ((Lchan list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan Lchan
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Llegato) ((Llegato list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato Llegato
                 :Lonset (Lonset self)
                 ))

(defmethod (setf LPort) ((LPort list) (self chord-seq))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport Lport
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))


;;;======================================
;;; EDITION
;;;======================================

(defmethod remove-from-obj ((self chord-seq) (item t)) nil)

(defmethod remove-from-obj ((self chord-seq) (item chord))
  (time-sequence-remove-timed-item self item))

(defmethod remove-from-obj ((self chord-seq) (item note))
  (loop for c in (chords self)
        do (when (find item (notes c))
             (setf (notes c) (remove item (notes c)))
             (when (null (notes c))
               (remove-from-obj self c))
             (return t))
        ))

