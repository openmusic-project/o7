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
;============================================================================
;
; BASIC SCORE FUNCTIONS
;
;============================================================================

(in-package :om)


;--------------------
;  DURATION
;--------------------

(defmethod! object-dur ((self score-object))
  :initvals '(nil)
  :indoc '("a musical object")
  :outdoc '("duration (ms)") 
  :icon :duration
  :doc "Returns the total duration of a musical object (ms)"
  (get-obj-dur self))


;--------------------
;  GET-CHORDS
;--------------------

(defmethod* get-chords ((self chord-seq))
   :initvals '(nil) 
   :indoc '("a music sequence (voice, chord-seq, poly, multi-seq...)")
   :icon :score 
   :doc "Extracts an ordered list (or list of list) of chords from a music sequence or superposition.
Returned chords are copies of original internal chords. Time information (onset) is removed."
   (loop for chord in (chords self) collect
         (om-copy chord)))


(defmethod* get-chords ((self poly))
  (loop for voice in (obj-list self) collect (get-chords voice)))
  

;--------------------
;  CONCAT
;--------------------

(defmethod* concat ((s1 chord-seq) (s2 chord-seq) &optional (s2-offset nil))
  :initvals '(nil nil nil) 
  :indoc '("a musical object" "a musical object" "concat offset") 
  :icon :score
  :doc "Concatenates two musical objects/sequences into a new one.

Optional input <s2-offset> may be used to pass an offset value (in ms) for <s2>.
Otherwise the <s2> is concatenated at the end of <s1>.
<s2-offset> is ignored in the case where VOICE objects.

MULTI-SEQ: global concatenation takes into account the duration of the object.  
POLY: each voice is concatenated, regardless of the global duration.
"
  
  (let ((cs (make-instance 'chord-seq)))

    (time-sequence-set-timed-item-list cs (append (get-chords s1) (get-chords s2)))
    (time-sequence-set-times cs (append (time-sequence-get-times s1)
                                        (om+ (time-sequence-get-times s2)
                                             (or s2-offset (object-dur s1)))))
    (time-sequence-update-obj-dur cs)
    cs))

;;; TODO: concatenate the tempo list (see below)
(defmethod* concat ((s1 voice) (s2 voice) &optional s2-offset)

  (when (and s2-offset (not (zerop s2-offset)))
    (om-print "CONCAT sequences of type VOICE: s2-offset value is ignored." "Warning"))
  
  (make-instance 'voice
                 :chords (append (get-chords s1) (get-chords s2))
                 :tree (append (tree s1) (tree s2)))
  ;;; (setf (tempo rep) (concat-tempi s1 s2))
  )


#|
(defun concat-tempi (voice1 voice2)

  (flet ((equal-tempi (t1 t2)
           (and (= (car t1) (car t2))
                (= (second t1) (second t2))
                (equal (third t1) (third t2)))))
    
    (let* ((tempo1 (tempo voice1))
           (tempo2 (tempo voice2))
           (newtempo (copy-list tempo1))
           lasttempo)
      
      (if (second tempo1)
          (setf lasttempo (second (car (last (second tempo1)))))
        (setf lasttempo (car tempo1)))
      
      (unless (equal-tempi (car tempo2) lasttempo)
        (setf (nth 1 newtempo) (append (nth 1 newtempo) 
                                       (list (list (list (length (inside voice1)) 0) (car tempo2))))
              ))
      
      (loop for item in (second tempo2) do
            (setf (nth 1 newtempo) (append (nth 1 newtempo) 
                                           (list (list (list (+ (length (inside voice1)) (caar item))
                                                             (second (car item)))
                                                       (second item))))
                  ))
    newtempo)))
|#


(defmethod* concat ((s1 multi-seq) (s2 multi-seq) &optional (s2-offset nil))

  (let ((offset (or s2-offset (get-obj-dur s1))))
    
    (make-instance 
     'multi-seq 
     :chord-seqs (loop with chs1 = (inside s1)
                       with chs2 = (inside s2)
                       while (or chs1 chs2)
                       collect (concat (pop chs1) (pop chs2) offset)))
    ))

(defmethod* concat ((s1 poly ) (s2 poly) &optional (s2-offset nil))

  (when (and s2-offset (not (zerop s2-offset)))
    (om-print "CONCAT sequences of type POLY: s2-offset value is ignored." "Warning"))

  (make-instance 'poly :voices (mapcar #'concat (inside s1) (inside s2))))


(defmethod* concat ((s1 score-object) (s2 null) &optional (s2-offset nil))
  (declare (ignore s2 s2-offset))
  (clone s1))

(defmethod* concat ((s1 null) (s2 score-object) &optional (s2-offset nil))
  (declare (ignore s1))
  (concat (make-instance (type-of s2)) s2 s2-offset))




;--------------------
;  MERGE
;--------------------




;--------------------
;  SELECT
;--------------------


(defmethod* select ((self chord-seq) (start number) (end number))
  :initvals '(nil 0 1000) 
  :indoc '("a sequence" "an integer" "an integer")
  :doc "
Extracts a subseqence :

when :
<self> is a chord-seq, <start> and <end> are absolute positions in ms, result is a chord-seq.
<self> is a voice, <start> and <end> are measure numbers, result is a voice.
<self> is a multi-seq, <start> and <end> are absolute positions in ms, result is a multi-seq.
"
  (if (or (< start 0)
          (>= start end))
       
     (om-beep-msg "select : Bad start/end values")
     
    (let ((rep (make-instance (type-of self))))
      
      (data-stream-set-frames 
       rep 
       (loop for chord in (data-stream-get-frames self)
             when (and (>= (item-get-time chord) start)
                       (< (item-get-time chord) end))
             collect (let ((c (om-copy chord)))
                       (setf (onset c) (- (onset chord) start))
                       c)))
      
      (time-sequence-update-obj-dur rep)

      rep)
    ))

(defmethod* select ((self multi-seq) (start number) (end number))
   (make-instance 'multi-seq
                  :obj-list (loop for cs in (obj-list self)
                                  collect (select cs start end))))


;--------------------
;  MASK
;--------------------







;--------------------
;  SPLIT
;--------------------

;;; by Gilbert Nouno
(defmethod* split-voices ((self chord-seq) &optional (random nil))
  :indoc '("a 'polyphonic' chord-seq" "random distribution strategy?")
  :initvals '(nil nil)
  :doc "Separates a CHORD-SEQ with overlapping notes into a list of monoponic CHORD-SEQs

If <random> = T, voice distribution is chosen randomly. Otherwise the first available voice is selected."
  :icon :score
  
  (let ((chords-lists nil))
    
    (loop for chord in (get-chords self)
          for time in (lonset self)
          for i = 0 then (1+ i) do
          
          (let ((position nil) 
                (list-indices (arithm-ser 0 (1- (length chords-lists)) 1)))
            (if random (setf list-indices (permut-random list-indices)))
            (loop for n in list-indices
                  while (not position) do 
                  (let ((voice? (nth n chords-lists)))
                    (when (> time (+ (car (car voice?)) (list-max (ldur (cadr (car voice?))))))
                      (setf (nth n chords-lists) (cons (list time chord) (nth n chords-lists)))
                      ;(setf voice? (cons (list time chord) voice?))
                      (setf position t))))
            
            (unless position
              (setf chords-lists
                    (append chords-lists (list (list (list time chord))))))
              
            ))

    ;;; chords-list =  
    ;;; (((onset1 chord1) (onset2 chord2) ...)
    ;;;  ((onset1 chord1) (onset2 chord2) ...)
    ;;;  ...[n times]... )

    (loop for list in chords-lists collect
          (let ((chords (reverse (mapcar 'cadr list)))
                (onsets (reverse (mapcar 'car list))))
            (make-instance 'chord-seq
                           :lonset onsets
                           :lmidic (mapcar 'lmidic chords)
                           :ldur (mapcar 'ldur chords)
                           :lvel (mapcar 'lvel chords)
                           :lchan (mapcar 'lchan chords))))
    )
 
)


