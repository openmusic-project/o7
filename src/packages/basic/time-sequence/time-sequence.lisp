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
;=========================================================================
; Author: J. Garcia, J. Bresson
;=========================================================================

(in-package :om)

;;;========================================
;;; TIME SEQUENCE
;;; an object made of a set of timed items
;;; the timed items can actually have no time
;;; TIME-SEQUENCE handles this using hidden, interpolated 'internal-time'
;;;========================================

;;; objects in a time-sequence should be subclasses of timed-item
;;; OR provide the same accessors
(defclass timed-item ()
  ((item-time :initform nil :accessor item-time :initarg :item-time)
   (item-internal-time :initform nil :accessor item-internal-time :initarg :item-internal-time)
   (item-type :initform nil :accessor item-type :initarg :item-type)
   ))


(defmethod item-get-time ((self timed-item)) (item-time self))
(defmethod item-set-time ((self timed-item) time) (setf (item-time self) time))
(defmethod item-get-internal-time ((self timed-item)) (item-internal-time self))
(defmethod item-set-internal-time ((self timed-item) time) (setf (item-internal-time self) time))
(defmethod item-get-type ((self timed-item)) (item-type self))
(defmethod item-set-type ((self timed-item) type) (setf (item-type self) type))
(defmethod items-merged-p ((i1 timed-item) (i2 timed-item)) (equal i1 i2))
(defmethod items-distance ((i1 timed-item) (i2 timed-item))
  (if (and (item-time i2) (item-time i1))
      (abs (- (item-time i2) (item-time i1)))
    1))

;;; time-sequence does not deal with durations but this can be useful, e.g to determine the end
(defmethod item-get-duration ((self t)) 0)
(defmethod item-set-duration ((self t) dur) nil)
(defmethod item-end-time ((self timed-item)) (+ (item-get-time self) (item-get-duration self)))


;;; e.g. for get-obj-dur
(defun item-real-time (timed-item) (or (item-get-time timed-item) (item-get-internal-time timed-item)))


;TODO:
; - MOVE gesture time in editor
; - Interpolate output values if interpol is on ?

(defclass time-sequence (timed-object)
  ((time-types :initform nil :accessor time-types)
   (duration :initform 0 :accessor duration :initarg :duration)
   (interpol :initform nil :accessor interpol :initarg :interpol)))


(defmethod initialize-instance :after ((self time-sequence) &rest args)

  (setf (interpol self)
        (make-number-or-nil :number (or (and (number-? (interpol self))
                                             (number-number (interpol self)))
                                        50)
                            :t-or-nil (number-? (interpol self))))

  self)


(defmethod om-init-instance :after ((self time-sequence) &optional initargs)
  (time-sequence-update-internal-times self)
  (time-sequence-update-obj-dur self) ;;; is this necessary ?
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;to be redefined by subclasses of TIME-SEQUENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod time-sequence-get-timed-item-list ((self time-sequence)) nil)
(defmethod time-sequence-set-timed-item-list ((self time-sequence) items)
  (time-sequence-update-obj-dur self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CAN be redefined by subclasses of TIME-SEQUENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod time-sequence-get-times ((self time-sequence))
  (loop for item in (time-sequence-get-timed-item-list self)
        collect (item-get-time item)))

(defmethod time-sequence-set-times ((self time-sequence) times)
  (loop for point in (time-sequence-get-timed-item-list self)
        for time in times do (item-set-time point time))
  (time-sequence-update-internal-times self))

(defmethod time-sequence-get-internal-times ((self time-sequence))
  (loop for point in (time-sequence-get-timed-item-list self) collect (item-get-internal-time point)))

;;; when there is no items insiode...
(defmethod time-sequence-default-duration ((self time-sequence)) 0)


(defmethod time-sequence-make-timed-item-at ((self time-sequence) at)
  (make-instance 'timed-item :item-time at))

;;; REDEFINE THIS METHOD IF NEEDED
; but should not called directly
; => USE time-sequence-insert-timed-item-and-update
(defmethod time-sequence-insert-timed-item ((self time-sequence) item &optional position)
  "Insert a timed-item into the item-list at pos position"
  (let* ((list (time-sequence-get-timed-item-list self))
         (p (or position (length list))))

    (time-sequence-set-timed-item-list self
                                       (append (and list (first-n list p))
                                               (list item)
                                               (nthcdr p list)))
    ))


; USE THIS METHOD TO UPDATE THE TIME PROPERTIES WHEN ADDING A POINT
(defmethod time-sequence-insert-timed-item-and-update ((self time-sequence) item &optional position)
  (let ((pos (or position (find-position-at-time self (item-get-time item)))))

    (clean-master-points-type self)

    (time-sequence-insert-timed-item self item pos)

    (time-sequence-update-internal-times self)

    pos))


;;; REDEFINE THESE METHODS IF NEEDED
; but should not called directly
; => USE remove-timed-point-from-time-sequence and remove-nth-timed-point-from-time-sequence
(defmethod time-sequence-remove-timed-item ((self time-sequence) item)
  "Remove a timed point at pos from the list"
  (time-sequence-set-timed-item-list self (remove item (time-sequence-get-timed-item-list self))))
(defmethod time-sequence-remove-nth-timed-item ((self time-sequence) pos)
  "Remove NTH timed point at pos from the list"
  (time-sequence-set-timed-item-list self (remove-nth pos (time-sequence-get-timed-item-list self))))


; USE THESE METHODS TO UPDATE THE TIME PROPERTIES WHEN REMOVING A POINT
(defmethod remove-timed-point-from-time-sequence ((self time-sequence) item)
  (time-sequence-remove-timed-item self item)
  (time-sequence-update-internal-times self))

(defmethod remove-nth-timed-point-from-time-sequence ((self time-sequence) pos)
  (time-sequence-set-timed-item-list self (remove-nth pos (time-sequence-get-timed-item-list self)))
  (time-sequence-update-internal-times self))


; REVERSE THE TIME SEQUENCE
(defmethod time-sequence-reverse ((self time-sequence))
  (let* ((t0 (get-first-time self))
         (times (time-sequence-get-times self))
         (durations (reverse (x->dx (remove nil times))))
         reversed-times)
    (time-sequence-set-timed-item-list self
                                       (reverse (time-sequence-get-timed-item-list self)))
    (setf reversed-times
          (loop for item in times collect
                (if item
                    (let ((val t0))
                      (setf t0 (+ t0 (or (pop durations) 0)))
                      val)
                  item)))
    (time-sequence-set-times self reversed-times)
    self))


;;;======================================
;;; UTILS (internal)
;;;======================================

(defmethod get-nth-point ((self time-sequence) n)
  (nth n (time-sequence-get-timed-item-list self)))

(defmethod get-first-time ((self time-sequence))
  (if (time-sequence-get-timed-item-list self)
      (item-get-internal-time (nth 0 (time-sequence-get-timed-item-list self)))
    0))

(defmethod get-obj-dur ((self time-sequence))
  (duration self))


;;; !!! assume the timed-items are time-sorted
(defmethod time-sequence-update-obj-dur ((self time-sequence))
  (setf (duration self)
        (if (remove nil (time-sequence-get-timed-item-list self))
            (let ((last-frame (last-elem (time-sequence-get-timed-item-list self))))
              (+ (item-real-time last-frame) (item-get-duration last-frame)))
          (time-sequence-default-duration self))
        ))


(defmethod set-internal-times ((self time-sequence) internal-times)
  (loop for point in (time-sequence-get-timed-item-list self)
        for time in internal-times
        do (item-set-internal-time point time)))


(defmethod update-time-types-from-tpoint-list ((self time-sequence))
  (setf (time-types self)
        (loop for item in (time-sequence-get-timed-item-list self)
              collect (item-get-type item))))

(defmethod update-tpoint-list-from-time-types ((self time-sequence))
  (set-tpoint-list-time-types self (time-types self)))

(defmethod set-tpoint-list-time-types ((self time-sequence) types)
  (loop for point in (time-sequence-get-timed-item-list self)
        for type in types do
        (item-set-type point type)))

; Active interpolated time for a time value
(defmethod get-active-interpol-time ((self time-sequence) time)
  (let* ((interpol-time (number-number (interpol self)))
         (delta (- time (get-first-time self)))
         (rest (mod delta interpol-time))
         (mod (floor (/ delta interpol-time))))
    (if (< mod 0)
        (get-first-time self)
      (+ (* (if (= 0 rest) mod (1+ mod)) interpol-time) (get-first-time self)))
    ))


(defmethod get-points-from-indices ((self time-sequence) indices)
  (let ((point-list (time-sequence-get-timed-item-list self)))
    (if (find T indices)
        point-list
      (loop for n in indices collect (nth n point-list)))))

(defmethod get-indices-from-points ((self time-sequence) points)
  (let ((point-list (time-sequence-get-timed-item-list self)))
    (remove nil (loop for p in points
                      collect (position p point-list)))))

(defmethod filter-points-for-obj ((self time-sequence) points)
  (loop for p in points
        when (member p (time-sequence-get-timed-item-list self))
        collect p))

(defmethod point-exists-at-time ((self time-sequence) time)
  (loop for point in (time-sequence-get-timed-item-list self)
        when (and ;; (not (= (floor (item-get-internal-time point)) 0))
                  ;; (= (floor (item-get-internal-time (print point))) (floor time))
                  (= (item-get-internal-time point) time))
        return point))

(defmethod get-point-at-time ((self time-sequence) time)
  (let ((pos (find-position-at-time self time)))
    (nth pos (time-sequence-get-timed-item-list self))))

; Last active position
(defmethod find-active-position-at-time ((self time-sequence) time)
  (or
   (position time (time-sequence-get-timed-item-list self) :key 'item-get-internal-time :test '>= :from-end t)
   0))

; Position in the element list.
; If in between two elements, returns the position of the last one
(defmethod find-position-at-time ((self time-sequence) time)
  (or
   (position time (time-sequence-get-timed-item-list self) :key 'item-get-internal-time :test '<=)
   (length (time-sequence-get-timed-item-list self))))


(defmethod time-sequence-make-interpolated-timed-item-at ((self time-sequence) time)
  (time-sequence-make-timed-item-at self time))

(defmethod time-sequence-get-active-timed-item-at ((self time-sequence) time)
  (if (number-? (interpol self))
      (let ((interpol-time (get-active-interpol-time self time)))
        (time-sequence-make-interpolated-timed-item-at self interpol-time))
    (let ((pos (find-active-position-at-time self time)))
      (nth pos (time-sequence-get-timed-item-list self)))))


; Successive points sets of which the first and last times are known.
; (If possible avoid copy of the points...)
#|
(defun get-time-segments (points)
  (let ((rep (list (list (om-copy (car points))))))
    (loop for pt in (cdr points) do
          (push (om-copy pt) (car rep))
          (when (item-get-time pt)
            (setf (car rep) (reverse (car rep)))
            (push (list (om-copy pt)) rep)))
    (if (= 1 (length (car rep))) ;; last point had a time
        (setf rep (cdr rep))
      (setf (car rep) (reverse (car rep))))
    (reverse rep)))
|#

(defun get-time-segments (points)
  (let ((rep (list (list (car points)))))
    (loop for pt in (cdr points) do
          (push pt (car rep))
          (when (item-get-time pt)
            (setf (car rep) (reverse (car rep)))
            (push (list pt) rep)))
    (if (= 1 (length (car rep))) ;; last point had a time
        (setf rep (cdr rep))
      (setf (car rep) (reverse (car rep))))
    (reverse rep)))


(defun calc-constant-time-intermediate-values (begin end n)
  (if (= begin end)
      (make-list n :initial-element begin)
    (arithm-ser begin end (/ (- end begin) (1- (float n))))))

(defun calc-constant-speed-intermediate-values (seg)
  (let* ((distances (loop for i from 0 to (- (length seg) 2) collect
                          (items-distance (nth i seg) (nth (1+ i) seg))))
         (total-length (reduce '+ distances :initial-value 0))
         (ratios (mapcar #'(lambda (l) (if (zerop total-length) 1 (/ l total-length))) distances)))
    (om-round (om+ (om* (dx->x 0 ratios)
                        (- (item-get-time (last-elem seg)) (item-get-time (car seg))))
                   (item-get-time (car seg))))
    ))


; todo:
; - clean and avoid calls to om-copy
; - simplify when all points have a time !!
(defmethod time-sequence-update-internal-times ((self time-sequence) &optional (interpol-mode :constant-speed) (duration 10000) (modif-time nil))
  ;this function creates a list of times for all timed points even if they are nil.
  ;It can do with constant time or constant speed interpolations (interpol-mode :constant-speed :constant-time "default")
  ;If the time of the first point is not specified, it will use zero
  ;If the time of the last point is not specified, it will use the default duration optional arg or the length of the previous timed segment
  ;(order-points-by-time self)

  ;; reset the internal times
  (set-internal-times self (copy-list (time-sequence-get-times self)))

  (let ((points (time-sequence-get-timed-item-list self)))

    (when points

      (let ((tmp-points
             (if (= 1 (length points))

                 ;;the list contains only one point then return0 or the existing time
                  ;(let ((pt (om-copy (car points))))
                  ;  (item-set-time pt (or (item-get-time (car points)) 0))
                  ;  (list pt))
                 (list (or (item-get-time (car points)) 0))

               ;;the list contains more than one point
               (let ((seg-list (get-time-segments points)))

                 ;; first and time is NIL
                 (unless (item-get-time (car (car seg-list)))
                   (item-set-time (car (car seg-list)) 0.0))

                 ;; last time is NIL
                 (unless (item-get-time (last-elem (last-elem seg-list)))
                   ;;; only 1 segment : arbirary end time at 'duration'
                   (if (= 1 (length seg-list))
                       (item-set-time (last-elem (last-elem seg-list)) (+ (item-get-time (car (last-elem seg-list))) duration))
                     ;; set the same duration as the previous segment
                     (let ((prev-segment (car (last seg-list 2))))
                       (item-set-time (last-elem (last-elem seg-list))
                                      (+ (item-get-time (car (last-elem seg-list)))
                                         (- (item-get-time (last-elem prev-segment))
                                            (item-get-time (car prev-segment))))))))

                 ;; fill between segments
                  ;(let ((replist (list (om-copy (car (car seg-list))))))
                  ;  (loop for seg in seg-list do
                  ;        (let ((timestamps (if (equal interpol-mode :constant-time)
                  ;                              ;;; constant duration between points
                  ;                              (calc-constant-time-intermediate-values (item-get-time (car seg))
                  ;                                                                      (item-get-time (last-elem seg))
                  ;                                                                      (length seg))
                  ;                            ;;; constant speed between points
                  ;                            (calc-constant-speed-intermediate-values seg)
                  ;                            )))
                  ;          (loop for pt in (butlast (cdr seg))
                  ;                for time in (butlast (cdr timestamps))
                  ;                do (let ((newpt (om-copy pt)))
                  ;                     (item-set-time newpt time)
                  ;                     (push newpt replist)
                  ;                     ))
                  ;          (push (om-copy (last-elem seg)) replist)
                  ;          ))
                  ;  (reverse replist))

                 (cons (item-get-time (car (car seg-list))) ;; we ensured it has a time...
                       (loop for seg in seg-list append
                             (let ((timestamps (if (equal interpol-mode :constant-time)
                                                   ;;; constant duration between points
                                                   (calc-constant-time-intermediate-values (item-get-time (car seg))
                                                                                           (item-get-time (last-elem seg))
                                                                                           (length seg))
                                                 ;;; constant speed between points
                                                 (calc-constant-speed-intermediate-values seg)
                                                 )))
                               (cdr timestamps))))

                 ))))

        ;;; set the new list of times as internal times
         ;(set-internal-times self (mapcar #'item-get-time tmp-points))
        (set-internal-times self tmp-points)

        )))

  (update-time-types self)
  )


(defmethod time-sequence-reorder-timed-item-list ((self time-sequence))
  (let ((points (time-sequence-get-timed-item-list self)))
    (when points
      (clean-master-points-type self)
      ;order point list
      (time-sequence-set-timed-item-list self (sort points '< :key 'item-get-internal-time))
      ;update internal-times
      (time-sequence-update-internal-times self))))


(defmethod get-all-master-points-positions ((self time-sequence))
  (om-all-positions :master (time-types self)))

(defmethod get-all-master-points ((self time-sequence))
  (loop for p in (time-sequence-get-timed-item-list self)
        when (equal (item-get-type p) :master)
        collect p))

(defmethod get-all-master-points-times ((self time-sequence))
  (loop for point in (get-all-master-points self)
        collect
        (item-get-internal-time point)))

(defmethod set-all-points-as-master ((self time-sequence))
  (loop for point in (time-sequence-get-timed-item-list self)
        do (item-set-type point :master))
  (update-time-types-from-tpoint-list self))

(defmethod get-first-and-last-time ((self time-sequence))
  (let ((points (time-sequence-get-timed-item-list self)))
    (when points
      (if (> (length points) 1)
          (list (item-get-internal-time (car points)) (item-get-internal-time (last-elem points)))
        (list (item-get-internal-time (car points)))))))

(defmethod clean-master-points-type ((self time-sequence))
  (let ((points (time-sequence-get-timed-item-list self)))
    (when points
       ;remove-first and last master-points
      (when (> (length points) 0)
        (item-set-type (car points) (item-get-time (car points))))
      (when (> (length points) 1)
        (item-set-type (last-elem points) (item-get-time (last-elem points))))))
  (update-time-types-from-tpoint-list self))

(defmethod update-time-types ((self time-sequence))
  (let ((points (time-sequence-get-timed-item-list self)))
    (loop for point in points
          do
          (unless (eql (item-get-type point) :master)
            (item-set-type point (item-get-time point))))
    (when (> (length points) 0)
      (item-set-type (car points) :master))
    (when (> (length points) 1)
      (item-set-type (last-elem points) :master))
    ;update internal time types slot
    (update-time-types-from-tpoint-list self)
    ))


(defmethod temporal-translate-all ((self time-sequence) dt)
  (loop for point in (time-sequence-get-timed-item-list self) do
        (item-set-time point (+ (item-get-time point) dt))))


(defmethod temporal-translate-points ((self time-sequence) points dt)
  ;if only one point and master then translate as master point
  (if (and (eql (length points) 1) (eql (item-get-type (car points)) :master))
      (time-stretch-from-master-point self (car points) dt)
     ;otherwise translate normally if possible
    (when (possible-time-translation self points dt)
      (loop for point in points do
            (item-set-time point (max 0 (+ (item-get-internal-time point) dt))))))
  (time-sequence-update-internal-times self))


(defmethod possible-time-translation ((self time-sequence) points dt)
  (when points
    (setf points (sort points '< :key 'item-get-internal-time))
    (let ((timed-items (time-sequence-get-timed-item-list self)))
      (if (eql (length timed-items) 1)
          t
        (let ((point-before (find (item-get-internal-time (car points)) timed-items :key 'item-get-internal-time :test '> :from-end t))
              (point-after (find (item-get-internal-time  (car (last points))) timed-items :key 'item-get-internal-time :test '<)))
          (and (or (null point-before)
                   (> (+ (item-get-internal-time (car points)) dt) (item-get-internal-time point-before)))
               (or (null point-after)
                   (< (+ (item-get-internal-time (car (last points))) dt) (item-get-internal-time point-after)))
               )
          )))))


(defmethod possible-time-translation-from-indices ((self time-sequence) indices dt)
  (let ((all-times (time-sequence-get-internal-times self)))
    (and indices
         (or (eql (length all-times) 1)
             (let ((min-ind (car indices))
                   (max-ind (car (last indices)))
                   (prev-t 0)
                   (post-t (get-obj-dur self)))
               (when (> min-ind 0)
                 (setf prev-t (nth (1- min-ind) all-times)))
               (when (< max-ind (1- (length all-times)))
                 (setf post-t (nth (1+ max-ind) all-times)))
               (and (< (+ dt (nth max-ind all-times)) post-t)
                    (> (+ (nth min-ind all-times) dt) prev-t)))))
    ))


(defmethod temporal-translate-points-from-indices ((self time-sequence) indices dt)
  ;if only one point and master then translate as master point
  (if (and (eql (length indices) 1) (eql (nth (car indices) (time-types self)) :master))
      (time-stretch-from-master-point self (car indices)  dt)
  ;otherwise translate normally if possible
    (when (possible-time-translation self indices dt)
      (loop for point_index in indices do
            (item-set-time (nth point_index (time-sequence-get-timed-item-list self))
                           (max 0  (+ (nth point_index (time-sequence-get-internal-times self)) dt))))))
  (time-sequence-update-internal-times self))


;;; return T if the right-move is ok
(defmethod move-the-right-side ((self time-sequence) master-point dt)

  (let* ((points (time-sequence-get-timed-item-list self))
         (times (time-sequence-get-internal-times self))
         (master-time (item-get-internal-time master-point))
         (new-t (max 0 (+ dt master-time)))
         (master-pos (find-position-at-time self master-time))
         (all-master-pos (get-all-master-points-positions self))
         (pos-in-masters (position master-pos all-master-pos))
         (next-master-pos (nth (1+ pos-in-masters) all-master-pos))
         (next-master-time (nth next-master-pos times))
         (next-master-ratio (if (= master-time next-master-time) 0 ;;; in principle we want to avoid this
                              (/ (- new-t next-master-time) (- master-time next-master-time)))))

    ;;; process all points between the two adjacent master-poinyts
    (loop for idx = (1- next-master-pos) then (- idx 1)
          while (> idx master-pos)
          do
          (let ((p (nth idx points)))
            (when (item-get-time p)
              (let ((new-p-time (round (+ (* next-master-ratio (- (nth idx times) master-time)) new-t)))
                    (next-p-time (nth (1+ idx) times)))
                (when (< new-p-time next-p-time)
                  (item-set-time p new-p-time))
                )
              )))
    (< (round new-t) (nth (1+ master-pos) times))
    ))


;;; return T if the left-move is ok
(defmethod move-the-left-side ((self time-sequence) master-point dt)
  (let* ((points (time-sequence-get-timed-item-list self))
         (times (time-sequence-get-internal-times self))
         (master-time (item-get-internal-time master-point))
         (new-t (max 0 (+ dt master-time)))
         (master-pos (find-position-at-time self master-time))
         (all-master-pos (get-all-master-points-positions self))
         (pos-in-masters (position master-pos all-master-pos))
         (prev-master-pos (nth (1- pos-in-masters) all-master-pos))
         (prev-master-time (nth prev-master-pos times))
         (prev-master-ratio (/ (- new-t prev-master-time) (- master-time prev-master-time))))

    ;;; process all points between the two adjacent master-poinyts
    (loop for idx from (1+ prev-master-pos) to (1- master-pos)
          do
          (let ((p (nth idx points)))
            (when (item-get-time p)
              (let ((new-p-time (round (+ (* prev-master-ratio (- (nth idx times) prev-master-time)) prev-master-time)))
                    (prev-p-time (nth (1- idx) times)))
                (when (> new-p-time prev-p-time)
                  (item-set-time p new-p-time))
                )
              )))

    (> (round new-t) (nth (1- master-pos) times))
    ))



(defmethod time-stretch-from-master-point ((self time-sequence) master-point dt)

  (let* ((points (time-sequence-get-timed-item-list self))
         (master-time (item-get-internal-time master-point))
         (new-t (max 0 (+ dt master-time)))
         (pos (find-position-at-time self master-time))
         (all-master-pos (get-all-master-points-positions self))
         (pos-index-in-masters (position pos all-master-pos))
         (master-point-before (find master-point points :test #'(lambda (p1 p2)
                                                                  (and (equal (item-get-type p2) :master)
                                                                       (> (item-get-internal-time p1) (item-get-internal-time p2))))
                                    :from-end t))
         (master-point-after (find master-point points :test #'(lambda (p1 p2)
                                                                 (and (equal (item-get-type p2) :master)
                                                                      (< (item-get-internal-time p1) (item-get-internal-time p2)))))))

    (when (and pos-index-in-masters
               (or (eql (length points) 1)
                   (and (or (null master-point-before)
                            (> (round new-t) (item-get-internal-time master-point-before)))
                        (or (null master-point-after)
                            (< (round new-t) (item-get-internal-time master-point-after))))))

      ; => The move is OK wrt. other master-points

      (cond ((> dt 0) ;;; MOVE-RIGHT
             ;;; check right-compression first
             (let ((do-it (or (null master-point-after)
                              (move-the-right-side self master-point dt))))
               (when do-it
                 (item-set-time master-point (round new-t))
                 (when master-point-before
                   (move-the-left-side self master-point dt)))
               ))

            ((< dt 0) ;;; MOVE LEFT
             ;;; check left-compression first
             (let ((do-it (or (null master-point-before)
                              (move-the-left-side self master-point dt))))
               (when do-it
                 (item-set-time master-point (round new-t))
                 (when master-point-after
                   (move-the-right-side self master-point dt)))
               ))

            (t nil))
      )))


;;;=========================================
;;; TIME MARKERS METHODS
;;;=========================================

(defmethod get-time-markers ((self time-sequence))
  (cons 0 (get-all-master-points-times self)))

(defmethod get-elements-for-marker ((self time-sequence) marker)
  (list (point-exists-at-time self marker)))

;;; jb: not sure it is ever needed to redefine it
(defmethod translate-elements-from-time-marker ((self time-sequence) elems dt)
  (when (not (member nil elems))
    (temporal-translate-points self elems dt)))

;;;=========================================
;;; LENGTH AND SPEED METHODS
;;;=========================================

(defmethod give-speed-profile ((self time-sequence))
;compute speed profile (all speeds bewteen points) for the curve. When div by zero should occur and infite speed it replace with -1. Returns nil if no points
  (let ((points (time-sequence-get-timed-item-list self))
        (times (time-sequence-get-internal-times self)))
    (if (not points)
        nil
      (loop for i from 0 to (1- (1- (length points)))
            collect
            (let* ((p1 (nth i points))
                   (t1 (nth i times))
                   (p2 (nth (1+ i) points))
                   (t2 (nth (1+ i) times))
                   (dist (items-distance p1 p2))
                   (dur (- t2 t1)))
              (if (= dur 0) -1 (/ dist dur)))))))


(defmethod give-length-profile ((self time-sequence))
;give length profile (all segment legnthes) for the curve. Returns nil if no points.
  (let ((points (time-sequence-get-timed-item-list self)))
    (if (not points)
        nil
      (loop for i from 0 to (1- (1- (length points)))
            collect
            (let* ((p1 (nth i points))
                   (p2 (nth (1+ i) points)))
              (items-distance p1 p2))))))


(defmethod give-length ((self time-sequence))
;give the length of the curve. return 0 if no points
  (if (not (time-sequence-get-timed-item-list self))
      0
    (reduce '+ (give-length-profile self))))


(defmethod give-normalized-cumulative-length-profile ((self time-sequence))
;give length profile cumulated and normalized between 0 and 1. Returns nil if no or 1 points.
  (let ((pl (time-sequence-get-timed-item-list self)))
    (when (>= (length pl) 1)
      (let ((length-profile (give-length-profile self)))
        (when length-profile
          (let ((length (reduce '+ length-profile))
                (val (dx->x 0 length-profile)))
            (if (zerop length) (list 0)
              (om/ val length))))))))

