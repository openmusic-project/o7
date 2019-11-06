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

;;; A simplified version of the "global variable" system
;;; Global variables are identified by a unique symbol/name (just like in Lisp!) 

(in-package :om)

;;; OMGlobalVar is actually not really used: 
;;; the name of the OMGlobalBox refers to a Lisp global. 
(defclass OMGlobalVar (OMPatchComponent) ())
(defclass OMGlobalBox (OMPatchComponentBox) ())

(defmethod special-box-p ((ref (eql 'global))) t) 
(defmethod get-box-class ((ref OMGlobalVar)) 'OMGlobalBox)
(defmethod box-symbol ((self OMGlobalVar)) 'global)
(defmethod object-box-label ((self OMGlobalVar)) (string+ "GLOBAL " (name self)))


(defmethod omNG-make-special-box ((reference (eql 'global)) pos &optional init-args)
  (let* ((name (car (list! init-args)))
         (var (make-instance 'OMGlobalVar :name (if name (string name) (string (gensym "VAR"))))))
    (omNG-make-new-boxcall var pos)))


(defmethod box-draw ((self OMGlobalBox) frame)
  (om-with-clip-rect frame 0 0 (- (w frame) 4) (- (h frame) 8)
    (draw-label self (reference self))))

(defmethod create-box-outputs ((self OMGlobalBox)) 
  (list 
   (make-instance 'box-output :box self :value nil :name "value")))

(defmethod next-optional-input ((self OMGlobalBox)) 
  (zerop (length (inputs self))))

(defmethod more-optional-input ((self OMGlobalBox) &key name value doc reactive)
  (declare (ignore name doc))
  (add-optional-input self :name "value"
                      :value value :reactive reactive)
  t)

(defmethod allow-rename ((self OMGlobalBox)) t)

(defmethod allow-text-input ((self OMGlobalBox)) 
  (unless (is-persistant (reference self))
    (values (name self)
            #'(lambda (box text)
                ;;; the box name shall be updated as well
                (set-name box text)
                ))))


;;;========================
;;; GLOBAL VAR BEHAVIOUR
;;;========================

(defmethod initialize-instance :after ((self OMGlobalBox) &rest args)
  (when (name self)
    (let ((sym (intern (string-upcase (name self)))))
      (unless (boundp sym)
        (om-print-format "Defining global variable: ~A" (list sym))
        (eval `(defvar ,sym ,(car (value self))))
        ))))

(defmethod set-name :after ((self OMGlobalBox) new-name)
  (let ((sym (intern (string-upcase (name self)))))
   (unless (boundp sym)
     (om-print-format "Defining global variable: ~A" (list sym))
     (eval `(defvar ,sym ,(car (value self))))
     )))

(defmethod set-value :after ((self OMGlobalBox) new-val)
  (when (name self) ;;; not always the case at this moment (e.g. at loading the box) 
    (let ((sym (intern (string-upcase (name self)))))
      (eval `(setf ,sym ,(car new-val)))
      )))


(defmethod read-value ((self OMGlobalBox))
  (let ((sym (intern (string-upcase (name self)))))
    (setf (value self) (list (eval sym)))
    (car (value self))))

(defmethod omng-save ((self OMGlobalBox))  
  (read-value self)
  (append (call-next-method)
          (list (save-value self))))
    

(defmethod boxcall-value ((self OMGlobalBox)) 
  
  (when (and (car (inputs self)) (connections (car (inputs self))))
    (let ((val (omNG-box-value (car (inputs self)))))
      (set-value self (list val))
      ))

  (read-value self))


(defmethod gen-code-for-call ((self OMGlobalBox) &optional args)
  
  (let ((sym (intern (string-upcase (name self)))))
    
    (if (and (car (inputs self)) (connections (car (inputs self))))
        
        `(setf ,sym ,(gen-code (car (inputs self))))

      sym)
    ))
    


    
    