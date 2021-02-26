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

;;;====================================================
;;; OBJECT/DATA BOXES:  FACTORY, SLOTS, INSTANCE
;;;====================================================

;;; Rules for object boxes:
;;; - The main slots of the box are the direct slots that have declared initargs
;;; - Additional (keyword) slots are declared with 'additional-class-attributes'
;;; - They are matched to a slot accessor if this slot is declared with initarg (in one of the class' superclasses)


(in-package :om)

;;===========================================
;  ABSTRACT SUPERCLASS
;;===========================================
;;; for boxeditcall ans slotsbox
(defclass OMBoxRelatedWClass (OMBoxCall) ()
  (:documentation "Boxes with a class as reference")
  (:metaclass omstandardclass))

(defmethod get-icon-id ((self OMBoxRelatedWClass)) 'icon-class)


;;; for the moment we do not allow object boxes in lambda mode
(defmethod valid-property-p ((self OMBoxRelatedWClass) (prop-id (eql :lambda))) t)
(defmethod eval-modes-for-box ((self OMBoxRelatedWClass)) '(nil :reference :box))

(defmethod box-def-self-in ((self t)) NIL)

(defmethod create-box-inputs ((self OMBoxRelatedWClass))
  (let ((class (find-class (reference self) nil)))
    (when class
      (cons
       (make-instance 'box-input
                      :name "SELF"
                      :box self :reference :self
                      :value (box-def-self-in (reference self))
                      :doc-string "Connect here to create a new instance by copy")
       (mapcar #'(lambda (slot)
                   (make-instance 'box-input
                                  :name (string (slot-name slot)) :reference (slot-name slot)
                                  :box self
                                  :value (valued-val (slot-initform slot))
                                  :doc-string (apply 'concatenate (cons 'string
                                                                        (append
                                                                         (unless (equal t (slot-type slot)) (list "[" (string-downcase (slot-type slot)) "] "))
                                                                         (list (slot-doc slot)))
                                                                        )
                                                     )))
               (remove-if-not 'slot-initargs (class-direct-instance-slots class)))   ;;; direct ?
       )
      )))


(defmethod create-box-outputs ((self OMBoxRelatedWClass))
  (let ((class (find-class (reference self) nil)))
    (when class
      (cons
       (make-instance 'box-output
                      :name "SELF" :reference :self
                      :box self
                      :doc-string (format nil "Instance of class ~A" (class-name class)))
       (mapcar #'(lambda (slot)
                   (make-instance 'box-output
                                  :name (string (slot-name slot))
                                  :reference (slot-name slot)
                                  :box self
                                  :doc-string  (or (slot-doc slot) nil)))
               (remove-if-not 'slot-initargs (class-direct-instance-slots class)))    ;;; direct ?
       ))))


;;; ADDITIONAL CLASS AND BOX/EDITOR ATTRIBUTES CAN BE ADDED AS OPTIONAL/KEYWORDS
;; will appear as a keyword input (must be a valid slot)
(defmethod additional-class-attributes ((self t)) nil)

(defmethod allow-more-optionals ((self OMBoxRelatedWClass)) t)

;;; box attributes can be just a name, or
;;; (name doc menu)
(defmethod additional-box-attributes ((self t)) nil)
(defmethod box-attributes-names ((attributes list))
  (mapcar #'(lambda (attr) (if (listp attr) (car attr) attr)) attributes))

(defmethod additional-box-attributes-names ((self OMBoxRelatedWClass))
  (box-attributes-names (additional-box-attributes (get-box-value self))))

(defmethod get-all-keywords ((self OMBoxRelatedWClass))
  ;; this function can be called when the value is not yet initialised in the box
  (let ((val (or (and (null (lambda-state self)) (get-box-value self))
                 (make-instance (reference self)))))
    (remove nil
            (list (additional-class-attributes val)
                  (box-attributes-names (additional-box-attributes val))
                  ))
    ))


(defun make-unique-name (symbol-base name-list)
  (let ((pack (symbol-package symbol-base))
        (name (symbol-name symbol-base)))
    (if (find name name-list :test 'string-equal)
        (let ()
          (loop while (find name name-list :test 'string-equal)
                for i = 2 then (+ i 1) do
                (setf name (string+ (symbol-name symbol-base) "_" (number-to-string i))))
          (intern name pack))
      symbol-base)
    ))

(defmethod next-keyword-input ((self OMBoxRelatedWClass))

  (let ((keywordlist (apply 'append (get-all-keywords self)))
        (usedkeywords (mapcar #'(lambda (in) (name in)) (get-keyword-inputs self))))

    (if keywordlist

        (or (find-if-not #'(lambda (elt) (member elt usedkeywords :test 'string-equal)) keywordlist :key 'string)
            (and (find (box-free-keyword-name self) keywordlist)
                 (make-unique-name (box-free-keyword-name self) usedkeywords))
            (values nil "All keywords are already used.."))

      (values nil (string+ "No keyword for box '" (name self) "'.")))
    ))


(defmethod add-keyword-input ((self OMBoxRelatedWClass) &key key value doc reactive)

  (declare (ignore value doc reactive))

  (call-next-method)
  (let ((name (string-downcase key)))
    (set-box-outputs self (append (outputs self)
                                  (list (make-instance
                                         'box-keyword-output
                                         :name name
                                         :box self
                                         :doc-string (get-input-doc self name)))))
    ))


(defmethod update-output-from-new-in ((box OMBoxRelatedWClass) name in)
  (let ((out (find name (outputs box) :key 'name :test 'string-equal)))
    (when out
      (setf (name out) (name in)
            (doc-string out) (get-input-doc box (name in)))
      )))


(defmethod remove-one-keyword-input ((self OMBoxRelatedWClass))
  (when (call-next-method)
    (remove-one-output self (car (last (outputs self))))))


;;;===============================
;;; RELATION TO REFERENCE CLASS
;;; (WHEN THE CLASS IS AN OMCLASS)
;;;===============================
(defmethod om-copy ((self OMBoxRelatedWClass))
  (let* ((newbox (call-next-method))
         (class (find-class (reference newbox) nil)))
    (when (omclass-p class)
      (retain-reference class newbox))
    newbox))

(defmethod omng-delete ((box OMBoxRelatedWClass))
  (call-next-method)
  (let ((class (find-class (reference box) nil)))
    (when (omclass-p class)
      (release-reference class box)))
  t)


;;;===============================
;;; EVALUATION
;;;===============================

(defmethod objFromObjs ((model t) (target t))
  (if (or (subtypep (type-of target) (type-of model))
          (subtypep (type-of model) (type-of target)))
      (clone-object model target)
    (om-beep-msg "No conversion method was found from ~A to ~A" (type-of model) (type-of target))
    ))


; don't mess with packages: send interned symbols to these functions

;(defun set-slot-val (obj slot-name value)
;  (eval `(setf (,(intern-pack slot-name (symbol-package (type-of obj))) ,obj) ',value)))

(defmethod set-slot-val (obj slot-name value)
  (eval `(setf (,slot-name ,obj) ',value)))

(defmethod set-slot-val (obj (slot-name string) value)
  (let ((slot (find slot-name (class-slots (class-of obj))
                    :key #'(lambda (slot) (string (slot-name slot)))
                    :test 'string-equal)))
    (when slot
      (set-slot-val obj (slot-name slot) value))))

;(defmethod get-slot-val (obj slot-name)
;  (eval `(,(intern-pack slot-name (symbol-package (type-of obj))) ,obj)))

(defmethod get-slot-val (obj slot-name)
  (eval `(,slot-name ,obj)))

(defmethod get-slot-val (obj (slot-name string))
  (let ((slot (find slot-name (class-slots (class-of obj))
                    :key #'(lambda (slot) (string (slot-name slot)))
                    :test 'string-equal)))
    (when slot
      (get-slot-val obj (slot-name slot)))))

(defun set-value-slots (value args)
  (mapcar #'(lambda (item) (set-slot-val value (car item) (cadr item))) args))

;;; This is redefined by the graphical initialization
;;; method of visual classes
(defmethod v-oop-init ((self t) &rest args) args)

;;; called after slots are set (including in OM)
;;; or when a new slot is set (e.g. slot box, property, etc.)
;;; <initargs> = NIL for save/load/copy OR in the init value of a box
;;; <initargs> = all initargs in MAKE-VALUE / list incl. connected initargs MAKE-VALUE-FROM-MODEL
;;; ////// <initargs> = only the specific initarg e.g. in set-property ///// NOT ANYMORE !!
(defmethod om-init-instance ((self t) &optional initargs) self)


(defmethod update-after-eval ((self OMBoxRelatedWClass)) nil)

(defmethod get-connected-args ((self OMBoxRelatedWClass) &optional (accessor #'omng-box-value))
  (remove nil
          (mapcar #'(lambda (input)
                      (when (connections input)
                        (list (intern-k (name input))
                              (funcall accessor input))))
                  (cdr (inputs self)))))

(defmethod get-all-args ((self OMBoxRelatedWClass) &optional (accessor #'omng-box-value))
  (mapcar #'(lambda (input)
              (list (intern-k (name input))
                    (funcall accessor input)))
          (cdr (inputs self))))

;;; Called by the main box evaluation procedure
;;; <initargs> are all the input values (connected or not)
(defun make-value (classname initargs)
  (let* ((class-slots (class-instance-slots (find-class classname)))
         (class-initargs (remove nil (mapcar 'slot-initargs class-slots)))
         (class-slots-names (mapcar 'slot-name class-slots))
         ;;; the regular class initargs
         (supplied-initargs (remove-if #'(lambda (item) (not (find item class-initargs :test 'find))) initargs :key 'car))
         ;;; not initargs but valid slots
         (supplied-other-args (loop for arg in initargs
                                    when (and (find (symbol-name (car arg)) class-slots-names :key 'symbol-name :test 'string-equal)
                                              (not (member (car arg) supplied-initargs :key 'car)))
                                    collect (list (symbol-name (car arg)) (cadr arg)))))

    (om-init-instance
     (let ((obj (apply 'make-instance (cons classname (reduce 'append supplied-initargs)))))
       (set-value-slots obj supplied-other-args)
       obj)
     initargs)
    ))


;;; SPECIAL FOR BOXEDITCALL:
;;; If the first input is connected the value is built by copying a prototype instance (<model>)
;;; <args> are only the connected input values
(defun make-value-from-model (type model initargs)

  (let* ((target (make-instance type))
         (rep (if (equal (type-of model) (type-of target))
                  (om-copy model)
                (objFromObjs model target)))
         (class-slots-names (mapcar 'slot-name (class-instance-slots (find-class type))))
         (set-slot-args (loop for initarg in initargs
                              when (find (symbol-name (car initarg)) class-slots-names :key 'symbol-name :test 'string-equal)
                              collect (list (symbol-name (car initarg)) (cadr initarg)))))

    (if rep
        (progn
          (set-value-slots rep set-slot-args)
          (om-init-instance rep (or initargs '(nil)))
          ;;; we want to pass a non-nil ['(NIL) and not NIL] value in order to signal evaluation mode to om-init-instance
          )
      (progn (om-beep-msg "Error creating a ~A from ~A" type model)
        (om-abort)))
    ))



(defmethod prepare-obj-for-request ((object t) (box OMBoxRelatedWClass)) object)


;;; NEEDS TO BE REWORKED/SIMPLIFIED
;;; IN PRINCIPLE THE CASE LAMBDA NEVER HAPPENS
(defmethod rep-editor ((box OMBoxRelatedWClass) num)
  (if (= num 0) (car (value box))
    (cond
     ;;; GENERAL CASE
     ((null (lambda-state box))
      (let* ((obj (prepare-obj-for-request (get-box-value box) box))
             (slot (name (nth num (outputs box)))))
        (cond ((find slot (class-instance-slots (find-class (type-of obj)))
                     :test 'string-equal
                     :key #'(lambda (slot) (symbol-name (slot-name slot))))
               (get-slot-val (car (value box)) slot))
              ((find (intern-k slot) (additional-box-attributes-names box))
               (get-edit-param box (intern-k slot)))
              (t nil))))
     ;;; LAMBDA
     ((equal (lambda-state box) :lambda)
      (let ((new-arg-list (function-arg-list (car (value box)))))
        ; (loop for n from 1 to (length (function-lambda-list (car (value box)))) collect (gensym))))
        (eval `#'(lambda ,new-arg-list
                   (let ((value (funcall ,(car (value box)) ,.new-arg-list)))
                     (get-slot-val value ,(name (nth num (outputs box)))))))))
     ;;; OTHER CASES
     (t (car (value box))))
    ))


;;===========================================
;; THE MAIN OBJECT BOX (FACTORY)
;;===========================================

(defclass OMBoxEditCall (OMBoxRelatedWClass ObjectWithEditor object-with-edit-params)
  ((play-state :initform nil :accessor play-state))
  (:metaclass omstandardclass))

(defmethod special-box-type ((class-name t)) nil)

(defmethod default-size ((self OMBoxEditCall)) (om-make-point 80 50))

(defmethod maximum-size ((self OMBoxEditCall)) nil)
(defmethod minimum-size ((self OMBoxEditCall))
  (om-make-point (+ 10
                    (max 60
                         (* (length (inputs self)) 10)
                         (* (box-n-outs self) 10)))
                 40))

(defmethod get-box-class ((self standard-class))
  (let ((classes (clos::class-precedence-list self)))
    (or
     (loop for c in classes
           when (special-box-type (class-name c))
           return (special-box-type (class-name c)))
     'OMBoxEditCall)))

(defmethod window-title-for-object ((self t))
  (get-object-type-name self))

(defmethod get-window-title ((self OMBoxEditCall))
  (window-title-for-object (car (value self))))

(defmethod def-reactive ((self OMBoxEditCall) key)
  (if (find key (additional-box-attributes-names self)) T NIL))

;;; NOT ! called when properties are changed in the inspector
(defmethod om-init-instance ((self omboxeditcall) &optional initargs)
  (let ((name (find-value-in-kv-list initargs :name)))
    (when (and name (editor self))
      (report-modifications (editor self)))
    self))

(defmethod omng-delete ((box OMBoxEditCall))
  (call-next-method)
  (when (editor box)
    (om-close-window (editor-window box))
    (when (editor box)
      ;;; in principle the window-close callback will have closed the editor and set it to NIL
      ;;; but for instance not if the window is not a special om# window (e.g. external app or library...)
      (editor-close (editor box))))
  (omng-delete (get-box-value box))
  t)


(defmethod get-input-def-value ((self OMBoxEditCall) name)
  (let ((slot (find name (class-instance-slots (find-class (reference self) nil)) :key 'slot-name)))
    (if slot
        (eval (slot-initform slot))
      ;;; maybe it's the edit-params.. ?
      (get-default-edit-param self name))))

(defmethod get-input-doc ((self OMBoxEditCall) name)
  (let ((slot (find (intern name (symbol-package (reference self)))
                    (class-instance-slots (find-class (reference self) nil)) :key 'slot-name)))
    (if slot (slot-doc slot)
      ;;; maybe it's the edit-params.. ?
      (let* ((val (or (and (null (lambda-state self)) (car (value self)))
                      (make-instance (reference self))))
             (pos (position (intern-k name) (additional-box-attributes val) :key 'car)))
        (when pos (nth 1 (nth pos (additional-box-attributes val))))))))


(defmethod class-attributes-menus ((self t)) nil)

(defmethod get-input-menu ((self OMBoxEditCall) name)
  (let* ((val (or (and (null (lambda-state self)) (car (value self)))
                  (make-instance (reference self))))
         (found-in-class-attr (find name (class-attributes-menus val) :key #'(lambda (entry) (string (car entry))) :test 'string-equal))
         (found-in-box-attr (find (intern-k name) (additional-box-attributes val) :key 'car)))
    (or (nth 1 found-in-class-attr)
        (nth 2 found-in-box-attr))))


(defmethod object-has-editor ((self t)) nil)

(defmethod get-value-for-editor ((self OMBoxEditCall)) (get-box-value self))

(defmethod open-editor ((self OMBoxEditCall))
  (when (object-has-editor (car (value self)))
    (unless (editor self)
      (setf (editor self) (make-instance (get-editor-class (car (value self)))
                                         :object self))
      (init-editor (editor self)))
    (open-editor-window (editor self))))

;;;=============================
;;; EDIT-PARAMS
;;;=============================

(defmethod object-default-edition-params ((self t)) nil)


(defmethod get-default-edit-param ((self OMBoxEditCall) param)
  (let ((val (or (car (value self)) (make-instance (reference self)))))
    (find-value-in-kv-list (object-default-edition-params val) param)))

;;;=============================
;;; FRAME
;;;=============================
(defclass OMObjectBoxFrame (OMBoxFrame)
  ((box-play-time :initform nil :accessor box-play-time)))

(defmethod get-box-frame-class ((self OMBoxEditCall)) 'OMObjectBoxFrame)

(defmethod get-box-help ((self OMBoxEditCall)) (format nil "Object of type ~A" (string-upcase (reference self))))

(defmethod box-ed-params-properties ((self t)) nil)
(defmethod editor-ed-params-properties ((self t)) nil)

(defmethod get-properties-list ((self OMBoxEditCall))

  (let ((properties (add-properties
                     (call-next-method)
                     "Appearance"
                     (append
                      `((:name "Name" :string name)
                        (:display "View (m)" ,(display-modes-for-object (get-box-value self)) display)
                        (:showname "Show name (n)" :bool show-name))
                      ))))

    (if (play-obj? (get-box-value self))
        (add-properties
         properties
         "Sequencer"
         '((:show-markers "Show markers" :bool show-markers)))
      properties)

    ))


(defmethod display-modes-for-object ((self t)) '(:text :hidden))

(defmethod set-display ((self OMBox) val)
  (setf (display self) val)
  (update-inspector-for-object self)
  (when (frame self) (om-invalidate-view (frame self))))

(defmethod change-display ((self OMBox))
  (when (visible-property (get-properties-list self) :display)
    (let ((next-mode (next-in-list (display-modes-for-object (car (value self)))
                                   (display self))))
      (set-display self next-mode))))

(defmethod set-box-play-time ((self OMObjectBoxFrame) time) (setf (box-play-time self) time))
(defmethod set-box-play-time ((self t) time) nil)


;;; redefine when default init value is different
(defmethod initialize-box-value ((self OMBoxeditCall) &optional value)
  (let ((val (or value (om-init-instance (make-instance (reference self))))))
    (set-name val (string-upcase (reference self)))
    (setf (value self) (list val))
    (reset-cache-display self)
    (when (frame self)
      (om-invalidate-view (frame self)))
    (when (editor self)
      (update-to-editor (editor self) self))))


(defmethod omNG-make-new-boxcall ((reference standard-class) pos &optional init-args)
  (let* ((box (make-instance (get-box-class reference)
                             ;;; when typed from the pach editor, the passed ibit-args is just a name (string)
                             :name (if (stringp init-args) init-args nil)  ; (format nil "~A" init-args)
                             :reference (class-name reference)
                             :icon-pos :noicon :show-name nil
                             :display :mini-view
                             :text-align :center))
         (size (default-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))

    ;;; if init-args is not a string, it is considered a value for init...
    ;;; should actually test that this is an instance of reference
    (initialize-box-value box (if (not (stringp init-args)) init-args))
    (setf (display box) (car (display-modes-for-object (get-box-value box))))
    box))


(defmethod omNG-make-new-boxcall ((reference OMClass) pos &optional init-args)
  (let ((box (call-next-method)))
    (retain-reference reference box)
    box))

(defmethod om-copy ((self OMBoxEditCall))
  (let ((newbox (call-next-method)))
    (setf (value newbox) (om-copy (value self)))
    newbox))

(defmethod after-copy-action ((self OMBoxEditCall))
  (unless (find-if #'connections (inputs self))
    (set-lock-state self :locked)))

(defmethod om-view-doubleclick-handler ((self OMObjectBoxFrame) pos)
  (or (apply-in-area self 'click-in-area pos)
      (open-editor (object self))))

(defmethod update-after-prop-edit ((self OMBoxFrame) (object OMBoxEditCall))
  (call-next-method)
  (when (editor object) (update-to-editor (editor object) object)))


;;;=======================
;;; DRAW
;;;=======================

(defmethod box-draw ((self OMBoxEditCall) (frame OMBoxFrame))
  (draw-value-in-frame (get-box-value self) frame)
  t)

;;; to be redefined by objects if they have a specific miniview
(defmethod draw-mini-view ((object t) (box OMBox) x y w h &optional time) nil)

;; the bold text that is written on the object box
(defmethod object-box-label ((object t)) (string-upcase (type-of object)))
(defmethod object-box-label ((object null)) "NIL")

(defmethod draw-label ((box OMBox) object &key color)
  (let ((frame (frame box))
        (str (if (eval-flag box)
                 ".oO__.."
               (object-box-label object)))
        (font (om-def-font :font1 :face "arial" :size 18 :style '(:bold))))
    (multiple-value-bind (sw sh) (om-string-size str font)
      (declare (ignore sw))
      (let* ((lines (om-string-wrap str (- (w frame) 18) font))
             (y0 (max 24 (- (+ 6 (/ (h frame) 2))
                            (round (* (1- (length lines)) sh) 2)))))
        (om-with-font font
                      (om-with-fg-color (or color (om-make-color 0.6 0.6 0.6 0.5))
                        (loop for line in lines for y = y0 then (+ y sh) do
                              (om-draw-string 10 y line)
                              ))
                      )
        ))))


(defmethod draw-mini-text ((object t) (box OMBox) x y w h &optional time)


  (let ((type-str (string-upcase (type-of object)))
        (type-font (om-def-font :font1b :size 10)))

    (om-draw-string (+ x (- w (om-string-size type-str type-font) 12))
                    (+ y 18)
                    type-str
                    :font (om-def-font :font1b :size 10)
                    :color (om-def-color :gray)))

  (om-with-font
   (om-def-font :font1 :size 10)

   (loop for i = (+ y 30) then (+ i 10)
         for sl in (ensure-cache-display-text box object)
         while (< i (- h 6)) do
         (let ((str (format nil "~A: ~A" (car sl) (cadr sl))))
           (if (> (om-string-size str (om-def-font :font1 :size 8)) (- w 10))
               (om-draw-string (+ x 4) i (concatenate 'string (subseq str 0 (min (length str) (1- (round w 5)))) "..."))
             (om-draw-string (+ x 4) i str))
           ))
   )
  )

(defmethod draw-type-of-object ((object t))
  (string-upcase (type-of object)))

(defparameter *miniview-x-margin* 4)
(defparameter *miniview-y-margin* 4)

(defmethod draw-value-in-frame ((object t) (frame OMObjectBoxFrame))
  (let ((box (object frame)))
    (case (display box)
      (:text
       (draw-label box object :color (om-make-color 0.6 0.6 0.6 0.2))
       (draw-mini-text object box 0 0 (w frame) (h frame) (box-play-time frame)))
      (:mini-view
       (om-with-clip-rect frame
           *miniview-x-margin* *miniview-y-margin*
           (- (w frame) (* 2 *miniview-x-margin*)) (- (h frame) (* 2 *miniview-y-margin*))
         (draw-label box object :color (om-make-color 0.6 0.6 0.6 0.2))
         (ensure-cache-display-draw box object)
         (draw-mini-view object box
                         *miniview-x-margin* *miniview-y-margin*
                         (- (w frame) (* 2 *miniview-x-margin*)) (- (h frame) (* 2 *miniview-y-margin*))
                         (box-play-time frame))
         ))
      (:hidden
       (om-with-clip-rect frame  0 *miniview-y-margin* (w frame) (- (h frame) (* 2 *miniview-y-margin*))
         (draw-label box object)))
      (otherwise nil)
      )

    (when (play-state box)
      (draw-cursor-on-box (get-obj-to-play box) frame (box-play-time frame)))
    ))


(defmethod time-to-pixel ((self omobjectboxframe) x)
  (miniview-time-to-pixel (get-box-value (object self)) (object self) self x))

(defmethod miniview-time-to-pixel (object box view time)
  (* (w view)
     (/ time (if (plusp (get-obj-dur object)) (get-obj-dur object) 1000))))

(defmethod draw-cursor-on-box (object frame time)
  (when time
    (om-with-fg-color (om-make-color 0.73 0.37 0.42)
      (let ((x (miniview-time-to-pixel object (object frame) frame time)))
        ; (print (list "cursor" time x))
        (om-draw-polygon (list (- x 5) 4 x 9 (+ x 5) 4) :fill t)
        (om-with-line '(2 2)
          (om-draw-line x 4 x (- (h frame) 6))
          )))
    t))

;;; specific update depending on context
(defmethod contextual-update ((self OMBox) (container t)) nil)


(defmethod update-after-eval ((self OMBoxEditCall))
  (when (frame self)
    (reset-cache-display self)
    (let ((val (car (value self))))
      ;;; if the object has a name, the box takes the object name
      ;;; if not, the object takes the box name...
      (if (get-name val)
          (setf (name self) (get-name val))
        (set-name val (name self))))
    (om-invalidate-view (frame self)))
  (contextual-update self (container self))
  (when (editor self)
    (update-to-editor (editor self) self)))


(defmethod update-from-editor ((self OMBoxEditCall) &key (value-changed t) (reactive t))

  (declare (ignore reactive)) ;;; reactive is handled in a :around method

  (when value-changed
    (setf (lock-state self) :locked)
    (contextual-update self (container self))
    (when (frame self)
      ;(update-inspector-for-object self) ;; ? sure about this ?
      ; => removed: caused problems with BPF editor draw when inspector is open :(
      (reset-cache-display self)
      (om-invalidate-view (frame self)))
    )
  (report-modifications (editor (container self))))


;;===========================================
;; THE SLOTS BOX
;;===========================================
(defclass OMSlotsBox (OMBoxRelatedWClass) ()
  (:metaclass omstandardclass)
  (:default-initargs :border nil))

(defmethod default-size ((self OMSlotsBox))
  (minimum-size self))

(defmethod v-resizable ((self OMSlotsBox)) nil)

(defmethod get-box-help ((self OMSlotsBox)) (format nil "Get/set values for the slots of a ~A" (string-upcase (reference self))))

;;; init-args MUST be a class
(defmethod omNG-make-new-boxcall ((reference (eql 'slots)) pos &optional init-args)
  (make-slots-boxcall init-args pos))

(defmethod make-slots-boxcall ((reference standard-class) pos)
  (let* ((box (make-instance 'OMSlotsBox
                             :name (string+ (string-upcase (class-name reference)) " SLOTS")
                             :reference (class-name reference)
                             :icon-pos :left
                            ;:text-font (om-def-font :font1 :style '(:italic))
                             :color (om-make-color 0.9 0.88 0.81)
                             :text-align :left))
         (size (default-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    box))

(defmethod make-slots-boxcall ((reference OMClass) pos)
  (let ((box (call-next-method)))
    (retain-reference reference box)
    box))

(defmethod make-slots-boxcall ((reference t) pos)
  (om-beep-msg "Error: can not create a SLOTS box from ~A" reference))



;;===========================================
;; THE 'INSTANCE' BOX
;;===========================================

;;; not necessary ?
;;; instances can be contained in existing value/factory box
;;; maybe add an attribuite just to remove the inputs ?
;;; (defclass OMInstanceBox (OMBoxRelatedWClass) ())


(defmethod make-new-box-with-instance ((instance standard-object) pos)
  (let ((box (omng-make-new-boxcall (class-of instance) pos instance)))
    (when box
      (setf (display box) :mini-view))
    box))


(defmethod make-new-box-with-instance ((instance cons) pos)

  (if (and
       (list-subtypep instance 'standard-object)
       (list-typep instance (type-of (car instance))))

      (let ((box (omng-make-new-boxcall (find-class 'collection) pos)))
        (when box
          (setf (obj-list (get-box-value box)) instance)
          (om-init-instance (get-box-value box))
          (setf (display box) :text)
          )
        box)

    (call-next-method)))


(defmethod make-new-box-with-instance ((instance t) pos)
  (omng-make-new-boxcall 'value pos instance))


