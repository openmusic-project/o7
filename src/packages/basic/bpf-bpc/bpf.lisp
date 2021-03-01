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

;;;===========================================
;;; BPF OBJECT
;;;===========================================

(in-package :om)

(defclass internalBPF (named-object schedulable-object object-with-action)
  ((point-list :initform nil :accessor point-list)
   (color :initform (om-def-color :black) :accessor color :initarg :color)
   (decimals :initform 2 :accessor decimals :initarg :decimals :documentation "precision (integer) [0-10]")))

(defmethod additional-slots-to-copy ((self internalBPF)) '(point-list action-fun))

;;; POINTS IN BPF
;;; VIRTUALLY IMPLEMENTS TIMED-ITEM INTERFACE
;;; - adds the 'type' slot
;;; - time is X
(defstruct (bpfpoint (:include oa::ompoint)) type)

(defun om-make-bpfpoint (x y &optional type)
  (make-bpfpoint :x x :y y :type type))

;;; necessary ?
(defmethod make-load-form ((self bpfpoint) &optional env)
  (declare (ignore env))
  `(make-bpfpoint :x ,(oa::ompoint-x self) :y ,(oa::ompoint-y self) :type ,(bpfpoint-type self)))

(defmethod om-copy ((self bpfpoint))
  (make-bpfpoint :x (bpfpoint-x self) :y (bpfpoint-y self) :type (bpfpoint-type self)))

(defmethod om-point-set ((point bpfpoint) &key x y type)
  (if x (setf (bpfpoint-x point) x))
  (if y (setf (bpfpoint-y point) y))
  (if type (setf (bpfpoint-type point) type))
  point)

(defmethod om-point-set-values-from-point ((point bpfpoint) (from bpfpoint))
  (setf (bpfpoint-x point) (bpfpoint-x from))
  (setf (bpfpoint-y point) (bpfpoint-y from))
  (setf (bpfpoint-type point) (bpfpoint-type from))
  point)


(defmethod item-get-time ((self bpfpoint)) (om-point-x self))
(defmethod item-set-time ((self bpfpoint) time) (om-point-set self :x time))
(defmethod item-get-internal-time ((self bpfpoint)) (om-point-x self))
(defmethod item-set-internal-time ((self bpfpoint) time) nil)
(defmethod item-get-type ((self bpfpoint)) (bpfpoint-type self))
(defmethod item-set-type ((self bpfpoint) type) (om-point-set self :type type))
(defmethod items-merged-p ((i1 bpfpoint) (i2 bpfpoint)) (om-points-at-same-position i1 i2))
(defmethod items-distance ((i1 bpfpoint) (i2 bpfpoint)) (om-points-distance i1 i2))

(defclass* BPF (internalbpf time-sequence)
  ((x-points :initform '(0 2000) :initarg :x-points :documentation "X coordinates (list)")
   (y-points :initform '(0 100) :initarg :y-points :documentation "Y coordinates (list)")
   (gain :initform 1 :accessor gain :documentation "A gain factor for Y values"))
  (:icon 'bpf)
  (:documentation "BREAK-POINTS FUNCTION: a 2D function defined as y=f(x) by a list of [x,y] coordinates (<x-points> / <y-points>).

- <x-point> must be stricly increasing or will be sorted at initialization.

- If <x-list> and <y-list> are not of the same length, the last step in the shorter one is repeated."
   ))

(defmethod bpf-p ((self bpf)) t)
(defmethod bpf-p ((self t)) nil)

(defmethod additional-class-attributes ((self BPF)) '(decimals color name action interpol))

(defmethod additional-slots-to-copy ((self BPF))
  (append (call-next-method) '(time-types)))

;;; decimals will be set because it is initarg
(defmethod initialize-instance ((self bpf) &rest args)
  (call-next-method)
  (check-decimals self)
  (init-bpf-points self)
  self)


(defun om-make-bpf (type xpts ypts dec)
  (make-instance type :x-points xpts :y-points ypts :decimals dec))

;;;===============================

(defmethod get-properties-list ((self bpf))
  `((""
     (:decimals "Precision (decimals)" :number decimals (0 10))
     (:color "Color" :color color)
     (:name "Name" :text name)
     (:action "Action" :action action-accessor)
     (:interpol "Interpolation" ,(make-number-or-nil :min 20 :max 1000) interpol)
     )))

(defun send-as-osc (bpf-point &optional (address "/bpf-point") (host "localhost") (port 3000))
  (osc-send (cons address bpf-point) host port))

(defmethod arguments-for-action ((fun (eql 'send-as-osc)))
  '((:string address "/bpf-point")
    (:string host "localhost")
    (:int port 3000)))

(defmethod get-def-action-list ((object BPF))
  '(print send-as-osc midi-controller))

(defmethod action-accessor ((self bpf) &optional (value nil value-supplied-p))
  (if value-supplied-p
      (set-action self value)
    (action self)))


;;;===============================

(defmethod check-decimals ((self bpf))
  (unless (and (integerp (decimals self))
               (> (decimals self) 0)
               (<= (decimals self) 10))
    (cond ((not (integerp (decimals self)))
           (om-beep-msg "BPF decimals must be an integer value! (def = 2)")
           (setf (slot-value self 'decimals) 2))
          ((minusp (decimals self))
           (om-beep-msg "BPF decimals must be a positive integer! (set to 0)")
           (setf (slot-value self 'decimals) 0))
          ((> (decimals self) 10)
           (om-beep-msg "BPF only support up to 10 decimals!")
           (setf (slot-value self 'decimals) 10))
          )))

(defmethod (setf decimals) ((decimals t) (self bpf))
  (let ((x (x-values-from-points self))
        (y (y-values-from-points self)))
    (setf (slot-value self 'decimals) decimals)
    (check-decimals self)
    (set-bpf-points self :x x :y y)
    (decimals self)))

;;; depending on decimals, the BPF will truncate float numbers
(defun truncate-function (decimals)
  (if (zerop decimals) #'round
    (let ((factor (expt 10 decimals)))
      #'(lambda (n) (/ (round (* n factor)) (float factor))))))

(defmethod x-values-from-points ((self bpf)) (mapcar #'om-point-x (point-list self)))
;   (let ((fun (if (zerop (decimals self)) #'om-point-x #'(lambda (p) (/ (om-point-x p) (expt 10.0 (decimals self)))))))
;     (mapcar fun (point-list self))))

(defmethod y-values-from-points ((self bpf)) (mapcar #'om-point-y (point-list self)))
;   (let ((fun (if (zerop (decimals self)) #'om-point-y #'(lambda (p) (/ (om-point-y p) (expt 10.0 (decimals self)))))))
;     (mapcar fun (point-list self))))

(defmethod xy-values-from-points ((self bpf) &optional from to)
  (mapcar #'(lambda (p) (list (om-point-x p) (om-point-y p)))
          (filter-list (point-list self) from to :key 'om-point-x)))


;(if (zerop (decimals self))
;    (mapcar #'(lambda (p) (list (om-point-x p) (om-point-y p))) (point-list self))
;  (let ((fact (expt 10.0 (decimals self))))
;    (mapcar #'(lambda (p) (list (/ (om-point-x p) fact) (/ (om-point-y p) fact))) (point-list self)))
;  ))

(defmethod set-bpf-points ((self bpf) &key x y z time time-types)
  (declare (ignore time z))
  (setf (point-list self) (sort
                           (make-points-from-lists (or x (x-values-from-points self)) ;  (slot-value self 'x-points))
                                                   (or y (y-values-from-points self)) ;  (slot-value self 'y-points))
                                                   (decimals self)
                                                   'om-make-bpfpoint)
                           '< :key 'om-point-x))
  ;;; todo?: check here if there is not duplicate X points and send a warning...

  (when time-types
    (loop for p in (point-list self)
          for type in time-types do (om-point-set p :type type)))

  (setf (slot-value self 'x-points) NIL)
  (setf (slot-value self 'y-points) NIL))


(defmethod (setf x-points) ((x-points t) (self bpf))
  (set-bpf-points self :x x-points)
  x-points)

(defmethod (setf y-points) ((y-points t) (self bpf))
  (set-bpf-points self :y y-points)
  y-points)

(defmethod x-points ((self bpf)) (x-values-from-points self))
(defmethod y-points ((self bpf)) (y-values-from-points self))

(defmethod init-bpf-points  ((self bpf))
  (set-bpf-points self
                  :x (slot-value self 'x-points)
                  :y (slot-value self 'y-points)
                  :time-types (slot-value self 'time-types))
  )

(defmethod change-precision ((self bpf) decimals)
  (setf (decimals self) decimals)
  (decimals self))

(defmethod make-points-from-lists ((listx list) (listy list) &optional (decimals 0) (mkpoint 'om-make-point))
  (when (or listx listy)
    (if (and (list-subtypep listx 'number) (list-subtypep listy 'number))
        (let* ((listx (mapcar (truncate-function decimals) (or listx '(0 1))))
               (listy (mapcar (truncate-function decimals) (or listy '(0 1))))
               (defx (if (= 1 (length listx)) (car listx) (- (car (last listx)) (car (last listx 2)))))
               (defy (if (= 1 (length listy)) (car listy) (- (car (last listy)) (car (last listy 2))))))
          (loop for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) (+ ypoin defy))
                for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) (+ xpoin defx))
                while (or listy listx)
                collect (funcall mkpoint xpoin ypoin)
                into rep
                finally (return (append rep (list (funcall mkpoint xpoin ypoin)))))
          )
      (om-beep-msg "BUILD BPF POINTS: input coordinates are not (all) numbers!")
      )))

(defmethod make-points-from-lists ((pointx number) (listy list) &optional (decimals 0) (mkpoint 'om-make-point))
  (if (list-subtypep listy 'number)
      (let* ((pointx (funcall (truncate-function decimals) pointx))
             (listy (mapcar (truncate-function decimals) listy)))
        (loop for ypoin in listy
              for x = 0 then (+ x pointx)
              collect (funcall mkpoint x ypoin)))
    (om-beep-msg "BUILD BPF POINTS: Y-coordinates are not (all) numbers!")
    ))

(defmethod make-points-from-lists ((listx list) (pointy number) &optional (decimals 0) (mkpoint 'om-make-point))
  (if (list-subtypep listx 'number)
      (let* ((pointy (funcall (truncate-function decimals) pointy))
             (listx (mapcar (truncate-function decimals) listx)))
        (loop for xpoin in listx
              collect (funcall mkpoint xpoin pointy)))
    (om-beep-msg "BUILD BPF POINTS: X-coordinates are not (all) numbers!")
    ))


(defmethod make-points-from-lists ((pointx t) (pointy t) &optional (decimals 0) (mkpoint 'om-make-point))
  (om-beep-msg "CAN'T BUILD BPF POINTS from [~A ~A]" pointx pointy))


;;;=====================================
;;; TIME-SEQUENCE API
;=======================================

(defmethod time-sequence-get-timed-item-list ((self bpf)) (point-list self))
(defmethod time-sequence-set-timed-item-list ((self bpf) points)
  (setf (point-list self) points)
  (call-next-method))

(defmethod time-sequence-get-times ((self bpf)) (x-points self))
(defmethod time-sequence-insert-timed-item ((self bpf) point &optional position)
  (insert-point self point position))
(defmethod time-sequence-make-timed-item-at ((self bpf) at)
  (om-make-bpfpoint at (x-transfer self at (decimals self))))


;;; redefine this to be a little bit safer wrt floating point errors
;;; (since BPF can have sub-millisecond time-point values...)
(defmethod point-exists-at-time ((self bpf) time)
  (let* ((fact (expt 10 (decimals self)))
         (rounded-time (round (* time fact))))
    (loop for point in (time-sequence-get-timed-item-list self)
          when (and (= (round (* (item-get-internal-time point) fact)) rounded-time))
          return point)))


;=======================================
;WHEN IN A COLLECTION...
;=======================================

(defmethod homogenize-collection ((self bpf) list)
  (let* ((maxdecimals (loop for item in list maximize (decimals item))))
    ;;; newlist all at the same (max) precision
    (loop for bpf in list
          unless (= (decimals bpf) maxdecimals)
          do
          (change-precision bpf maxdecimals))
    list))

;=======================================
;OPERATIONS
;=======================================

(defmethod set-bpf-point-values ((self bpf))
  (let ((tf (truncate-function (decimals self))))
    (loop for p in (point-list self) do
          (om-point-set p :x (funcall tf (om-point-x p)) :y (funcall tf (om-point-y p))))))

;=============================
; Insert a point at the right position (x ordered) returns the position
;==============================

(defmethod adapt-point ((self bpf) point)
  (om-point-set point
                :x (funcall (truncate-function (decimals self)) (om-point-x point))
                :y (funcall (truncate-function (decimals self)) (om-point-y point)))
  point)

;;; MUST RETURN THE POSITION
(defmethod insert-point ((self bpf) point &optional position)
  (let* ((new-point (adapt-point self point))
         (pos (or position  ;;; in principle there is no need to specify the position with BPFs
                  (position (om-point-x new-point) (point-list self) :key 'om-point-x :test '<=)
                   ;(length (point-list self))
                  ))
         (new-point-list (copy-tree (point-list self))))
    (if new-point-list
        (if pos
            (if (= (om-point-x (nth pos new-point-list)) (om-point-x new-point))
                (setf (nth pos new-point-list) new-point)
              (if (= pos 0)
                  (push new-point new-point-list)
                (push new-point (nthcdr pos new-point-list))))
          ;;; pos = NIL : insert at the end
          (nconc new-point-list (list new-point))
          )
      (setf new-point-list (list new-point)))
    (setf (point-list self) new-point-list)

    ;;; return the position
    (or pos (1- (length new-point-list)))))


;=============================
; Delete a point
;=============================

(defmethod remove-point ((self bpf) point)
  (setf (point-list self)
        (remove point (point-list self) :test 'om-points-equal-p)))

(defmethod remove-nth-point ((self bpf) n)
  (setf (point-list self)
        (append (subseq (point-list self) 0 n)
                (subseq (point-list self) (1+ n)))))

;=============================
; Move the point in x and y
;=============================

(defmethod possible-move ((self bpf) points x-key deltax y-key deltay)
  (let ((point-before (find (om-point-x (car points)) (point-list self)
                            :key 'om-point-x :test '> :from-end t))
        (point-after (find (om-point-x (car (last points))) (point-list self)
                           :key 'om-point-x :test '<)))
    (and (or (null point-before)
             (> (+ (om-point-x (car points)) deltax) (om-point-x point-before)))
         (or (null point-after)
             (< (+ (om-point-x (car (last points))) deltax) (om-point-x point-after)))
         )
    ))

(defmethod possible-set ((self bpf) point x y)
  (let ((point-before (find (om-point-x point) (point-list self)
                            :key 'om-point-x :test '> :from-end t))
        (point-after (find (om-point-x point) (point-list self)
                           :key 'om-point-x :test '<)))
    (and (or (null point-before)
             (> x (om-point-x point-before)))
         (or (null point-after)
             (< x (om-point-x point-after))))))

;;; move-plist = '(:x dx :y dy ...)

;TODO Add axis keys to check and move in correct dimension if internal bpf
(defmethod move-points-in-bpf ((self bpf) points dx dy &optional (x-key :x) (y-key :y))
  (with-schedulable-object self
                           (when (possible-move self points x-key dx y-key dy)
                             (loop for p in points do (funcall 'om-point-mv p x-key dx y-key dy))
                             points)))


(defmethod set-point-in-bpf ((self bpf) point x y)
  (let ((xx (funcall (truncate-function (decimals self)) (or x (om-point-x point))))
        (yy (funcall (truncate-function (decimals self)) (or y (om-point-y point)))))
    (when (possible-set self point xx yy)
      (om-point-set point :x xx :y yy))
    point))


;=======================================
;ACCESSORS
;=======================================

; Get the x values of prev et next points of point
(defmethod give-prev+next-x ((self bpf) point)
  (let ((pos (position point (point-list self) :test #'eql)))
    (when pos
      (list (and (plusp pos) (nth (1- pos) (point-list self)))
            (nth (1+ pos) (point-list self))))))


; Get the prev and next points for a point not in the bpf
(defmethod give-closest-points ((self bpf) point)
  (let ((pos (position (om-point-x point) (point-list self) :test '< :key 'om-point-x)))
    (cond ((zerop pos)
           (list nil (car (point-list self))))
          (pos
           (list (nth (1- pos) (point-list self)) (nth pos (point-list self))))
          (t
           (append (last (point-list self)) '(nil))))
    ))

; Get the points that fall in the interval (x1 x2)
(defmethod give-points-in-x-range ((self bpf) x1 x2)
  (loop for x in (point-list self)
        while (<= (om-point-x x) x2)  ;;; because points are ordered
        when (>= (om-point-x x) x1)
        collect x))

; Get the points that fall in the interval (y1 y2)
(defmethod give-points-in-y-range ((self bpf) y1 y2)
  (loop for point in (point-list self)
        when (and (>= y2 (om-point-y point)) (<= y1 (om-point-y point)))
        collect point))

; Get the points that fall in the given rect (tl br)
(defmethod give-points-in-rect ((self bpf) tl br)
  (let* ((x (om-point-x tl)) (y (om-point-y tl))
         (w (- (om-point-x br) x)) (h (- (om-point-y br) y)))
    (loop for p in (point-list self)
          when (om-point-in-rect-p p x y w h)
          collect p)))

;=======================================
;BOX
;=======================================
(defmethod display-modes-for-object ((self bpf))
  '(:mini-view :text :hidden))


; Get the min - max points in x and y axis
; using reduce 'mix/max is fatser when interpreted but not when compiled
(defmethod nice-bpf-range ((self bpf))
  (multiple-value-bind (x1 x2 y1 y2)
      (loop for x in (x-values-from-points self)
            for y in (y-values-from-points self)
            minimize x into x1 maximize x into x2
            minimize y into y1 maximize y into y2
            finally (return (values x1 x2 y1 y2)))
    (append (list x1 x2) (list y1 y2))
    ; (if (< (- x2 x1) 10) (list (- x1 5) (+ x2 5)) (list x1 x2))
    ; (if (< (- y2 y1) 10) (list (- y1 5) (+ y2 5)) (list y1 y2)))
    ))

(defmethod nice-bpf-range ((self list))
  (loop for item in self
        for range = (nice-bpf-range item)
        minimize (first range) into x1
        maximize (second range) into x2
        minimize (third range) into y1
        maximize (fourth range) into y2
        finally (return (list x1 x2 y1 y2))))


(defmethod get-cache-display-for-draw ((self bpf) box)
  (list
   (nice-bpf-range self)
   (if (<= (length (point-pairs self)) 500)
       (point-pairs self)
     ;(reduce-n-points (point-pairs self) 1000 100)
     ;(min-max-points (point-pairs self) 1000)
     (point-pairs (om-sample self 500))
     )
   ))


(defmethod draw-mini-view ((self bpf) (box t) x y w h &optional time)
  (let* ((display-cache (ensure-cache-display-draw box self))
         (ranges (car display-cache))
         (x-range (list (nth 0 ranges) (nth 1 ranges)))
         (y-range (list (or (get-edit-param box :display-min) (nth 2 ranges))
                        (or (get-edit-param box :display-max) (nth 3 ranges))))
         (font (om-def-font :font1 :size 8)))

    (draw-bpf-points-in-rect (cadr display-cache)
                             (color self)
                             (append x-range y-range)
                             (+ x 2) (+ y 10) (- w 4) (- h 20)
                             ;x (+ y 10) w (- h 20)
                             (get-edit-param box :draw-style))

    (om-with-font font
                  (om-draw-string (+ x 10) (+ y (- h 4)) (number-to-string (nth 0 x-range)))
                  (om-draw-string (+ x (- w (om-string-size (number-to-string (nth 1 x-range)) font) 4))
                                  (+ y (- h 4))
                                  (number-to-string (nth 1 ranges)))
                  (om-draw-string x (+ y (- h 14)) (number-to-string (nth 0 y-range)))
                  (om-draw-string x (+ y 10) (number-to-string (nth 1 y-range)))
                  )

    t))


(defmethod draw-sequencer-mini-view ((self bpf) (box t) x y w h &optional time)
  (let* ((display-cache (ensure-cache-display-draw box self))
         (ranges (car display-cache))
         (x-range (list 0 (nth 1 ranges)))
         (y-range (list (or (get-edit-param box :display-min) (nth 2 ranges))
                        (or (get-edit-param box :display-max) (nth 3 ranges)))))

    (draw-bpf-points-in-rect (cadr display-cache)
                             (color self)
                             (append x-range y-range)
                             (+ x 2) (+ y 10) (- w 4) (- h 20)
                             (get-edit-param box :draw-style))

    (om-with-fg-color (om-def-color :gray)
      (om-with-font (om-def-font :font1 :size 8)
                    (om-draw-string x (+ y (- h 14)) (number-to-string (nth 0 y-range)))
                    (om-draw-string x (+ y 10) (number-to-string (nth 1 y-range)))
                    ))
    t))


(defun conversion-factor-and-offset (min max w delta)
  (let* ((range (- max min))
         (factor (if (zerop range) 1 (/ w range))))
    (values factor (- delta (* min factor)))))

(defun draw-bpf-points-in-rect (points color ranges x y w h &optional style)

  (multiple-value-bind (fx ox)
      (conversion-factor-and-offset (car ranges) (cadr ranges) w x)
    (multiple-value-bind (fy oy)
        ;;; Y ranges are reversed !!
        (conversion-factor-and-offset (cadddr ranges) (caddr ranges) h y)

      (when points

        (flet ((px (p) (if (om-point-p p) (om-point-x p) (car p)))
               (py (p) (if (om-point-p p) (om-point-y p) (cadr p))))

          (cond

           ((equal style :points)

            (om-with-fg-color (om-def-color :dark-gray)

              (loop for pt in points do
                    (om-draw-circle (+ ox (* fx (px pt)))
                                    (+ oy (* fy (py pt)))
                                    3 :fill t))
              ))

           ((or (equal style :lines)
                (null style))

            (let ((lines (loop for pts on points
                               while (cadr pts)
                               append
                               (let ((p1 (car pts))
                                     (p2 (cadr pts)))
                                 (om+ 0.5
                                      (list (+ ox (* fx (px p1)))
                                            (+ oy (* fy (py p1)))
                                            (+ ox (* fx (px p2)))
                                            (+ oy (* fy (py p2)))))
                                 ))))
              (om-with-fg-color (or color (om-def-color :dark-gray))
                (om-draw-lines lines))
              ))

           ((equal style :histogram)

            (loop for i from 0 to (1- (length points))
                  do
                  (let* ((p (nth i points))
                         (x (+ ox (* fx (px p))))
                         (prev-p (if (plusp i) (nth (1- i) points)))
                         (next-p (nth (1+ i) points))
                         (prev-px (if prev-p (+ ox (* fx (px prev-p)))))
                         (next-px (if next-p (+ ox (* fx (px next-p)))))

                         (x1 (if prev-px
                                 (/ (+ x prev-px) 2)
                               (if next-px
                                   (- x (- next-px x))
                                 (- x 1))))

                         (x2 (if next-px
                                 (/ (+ x next-px) 2)
                               (if prev-px
                                   (+ x (- x prev-px))
                                 (+ x 1)))))

                    (om-draw-rect x1 oy (- x2 x1) (* fy (py p))
                                  :fill nil)
                    (om-draw-rect x1 oy (- x2 x1) (* fy (py p))
                                  :fill t :color (om-def-color :gray))

                    (om-draw-string (- x 4) (- oy 4) (format nil "~D" (py p))
                                    :font (om-def-font :font1 :size 9)
                                    :color (om-def-color :white))
                    ))
            )

           (t

            (om-with-fg-color (om-def-color :gray)

              ; first point
              (om-draw-circle (+ ox (* fx (px (car points))))
                              (+ oy (* fy (py (car points))))
                              3 :fill t)
              (let ((lines (loop for pts on points
                                 while (cadr pts)
                                 append
                                 (let ((p1 (car pts))
                                       (p2 (cadr pts)))
                                   (om-draw-circle (+ ox (* fx (px p2)))
                                                   (+ oy (* fy (py p2)))
                                                   3 :fill t)
                                   ;;; collect for lines
                                   (om+ 0.5
                                        (list (+ ox (* fx (px p1)))
                                              (+ oy (* fy (py p1)))
                                              (+ ox (* fx (px p2)))
                                              (+ oy (* fy (py p2)))))
                                   ))))
                (om-with-fg-color (or color (om-def-color :dark-gray))
                  (om-draw-lines lines))
                )))

           ))
        )
      )))


;;;=============================
;;; FOR DRAW ON COLLECTION BOXES
;;;=============================

(defmethod collection-draw-mini-view ((type BPF) list box x y w h time)

  (if (list-subtypep list 'BPF) ;;; works only is all objects are BPFs

      ;;; with BPFs we wan to gather info (ranges) from all different caches of teh collection-box multi-cache list
      (multiple-value-bind (ranges bpf-points-list)
          (loop with temp-cache = nil
                for o in list
                do (setf (cache-display box) nil)
                do (setf temp-cache (ensure-cache-display-draw box o))
                collect (cadr temp-cache) into bpf-points-list
                minimize (first (car temp-cache)) into x1
                maximize (second (car temp-cache)) into x2
                minimize (third (car temp-cache)) into y1
                maximize (fourth (car temp-cache)) into y2
                finally (return (values (list x1 x2 y1 y2) bpf-points-list)))

        (let* ((n-bpfs (length list))
               (max-n 40) ;;; LIMIT TO .... ?
               (step (max 1 (floor n-bpfs max-n))))

          (loop for i from 0 to (1- n-bpfs) by step
                do (draw-bpf-points-in-rect
                    (nth i bpf-points-list)
                    (color (nth i list))
                    ranges
                    x (+ y 10) w (- h 20)
                    :lines)
                ))

        (let ((font (om-def-font :font1 :size 8)))
          (om-with-font font
                        (om-draw-string (+ x 10) (+ y (- h 4)) (number-to-string (nth 0 ranges)))
                        (om-draw-string (+ x (- w (om-string-size (number-to-string (nth 1 ranges)) font) 4))
                                        (+ y (- h 4))
                                        (number-to-string (nth 1 ranges)))
                        (om-draw-string x (+ y (- h 14)) (number-to-string (nth 2 ranges)))
                        (om-draw-string x (+ y 10) (number-to-string (nth 3 ranges)))
                        )
          )
        )

    (call-next-method))
  )


;;;=============================
;;; BPF PLAY
;;;=============================

(defmethod play-obj? ((self bpf)) t) ;(action self)
(defmethod get-obj-dur ((self bpf)) (or (car (last (x-points self))) 0))
(defmethod point-time ((self bpf) p) (om-point-x p))

;(defmethod get-all-times ((self bpf)) (x-points self))

;;; RETURNS A LIST OF (TIME ACTION) TO PERFORM IN TIME-INTERVAL
(defmethod get-action-list-for-play ((object BPF) time-interval &optional parent)

  (when (action object)

    (if (number-? (interpol object))

        (let* ((t1 (max 0 (car time-interval)))
               (t2 (min (get-obj-dur object) (cadr time-interval)))
               (time-list (arithm-ser (get-active-interpol-time object t1) t2 (number-number (interpol object)))))
          (loop for interpolated-time in time-list
                for val in (x-transfer object time-list)
                collect (let ((v val)
                              (ti interpolated-time))
                          (list
                           ti
                           #'(lambda ()
                               (funcall (action-fun object) (list ti (float (* (gain object) v)))))))))
      ;;; no interpolation
      (mapcar
       #'(lambda (xy)
           (list (car xy)
                 #'(lambda ()
                     (funcall (action-fun object) (list (car xy) (* (gain object) (cadr xy)))))))
       (xy-values-from-points object (car time-interval) (cadr time-interval))))))


(defmethod set-object-gain ((self bpf) gain)
  (setf (gain self) gain))

(defmethod set-object-pan ((self bpf) pan)
  nil)


;;; DB:
;;; in order to limit replanning operations, it is preferrable to
;;; call (setf point-list) only once all modifications are performed
;;; For example, when drawing a curve, don't call (setf point-list) on
;;; each insert-point but only once the mouse is released
(defmethod (setf point-list) ((point-list t) (self bpf))
  (with-schedulable-object
   self
   (setf (slot-value self 'point-list) point-list)))


;;;===============================================
;;; SVG export
;;;===============================================

(defmethod export-svg ((self bpf) file-path &key with-points (w 300) (h 300) (margins 20) (line-size 1))
  :icon 908
  :indoc '("a BPF object" "a pathname" "draw-points" "image width" "image height" "margins size" "line-size")
  :initvals '(nil nil nil 300 300 20 1)
  :doc "
Exports <self> to SVG format.
"
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory)
                                                            :prompt "New SVG file"
                                                            :types '("SVG Files" "*.svg")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((bpf-points (point-pairs (bpf-scale self :x1 margins :x2 (- w margins) :y2 margins :y1 (- h margins)))) ; y2 and y1 switched to have the correct orientation
             (scene (svg::make-svg-toplevel 'svg::svg-1.1-toplevel :height h :width w))
             (prev_p nil)
             (path (svg::make-path))
             (color (or (color self) (om-def-color :black)))
             (bpfcolorstr (format nil "rgb(~D, ~D, ~D)"
                                  (round (* 255 (om-color-r color)))
                                  (round (* 255 (om-color-g color)))
                                  (round (* 255 (om-color-b color))))))
        ;draw line
        (loop for pt in bpf-points do
              (svg::with-path path
                (if prev_p
                    (svg::line-to (car pt) (cadr pt))
                  (svg::move-to (car pt) (cadr pt))))
              (setf prev_p pt))
        (svg::draw scene (:path :d path)
                   :fill "none" :stroke bpfcolorstr :stroke-width line-size)

        ;if points, draw points
        (when with-points
          (loop for pt in bpf-points do
                (svg::draw scene (:circle :cx (car pt) :cy (cadr pt) :r (if (numberp with-points) with-points 2))
                           :stroke "rgb(0, 0, 0)"
                           :fill bpfcolorstr)))

        (with-open-file (s pathname :direction :output :if-exists :supersede)
          (svg::stream-out s scene)))
      pathname
      )))
