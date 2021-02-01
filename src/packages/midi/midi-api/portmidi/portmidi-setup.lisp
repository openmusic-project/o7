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

;;;==============================
;;; PORTMIDI PORTS SETUP TOOL
;;;==============================

;;; SETTINGS =
;;; (((IN1 ("IN-DEVICE-NAME" ...))
;;;  ...)
;;; ((OUT1 ("OUT-DEVICE-NAME" ...))
;;;  ...))

;;; (portmidi-connect-ports (portmidi-setup nil nil))

(in-package :om-midi)

;;; tables ((port name stream) ...)
(defvar *portmidi-in-ports-table* nil)
(defvar *portmidi-out-ports-table* nil)
(defparameter *portmidi-def-buffer-size* 1024)

(defun portmidi-close-all-midi-ports ()
  (mapcar #'pm::pm-close (mapcar #'third (remove-duplicates *portmidi-in-ports-table* :key 'cadr :test 'string-equal)))
  (mapcar #'pm::pm-close (mapcar #'third (remove-duplicates *portmidi-out-ports-table* :key 'cadr :test 'string-equal)))
  (setf *portmidi-in-ports-table* nil *portmidi-out-ports-table* nil))

(defun portmidi-connect-ports (settings)
  (portmidi-restart)
  (unless (pm-time-started) (pm-time-start))
  (let ((pm-devices (list-devices)))
    ;;; IN
    (loop for item in (car settings) do
          (loop for indevice in (cadr item) do
                (let ((stream (nth 2 (find indevice *portmidi-in-ports-table* :key 'cadr :test 'string-equal))))
                  (unless stream  ;;; no stream is open for this device : create stream and open it
                    (let ((device-id (car (find-if #'(lambda (device) (and (string-equal (getf (cdr device) :name) indevice) (getf (cdr device) :input))) pm-devices))))
                      (when device-id
                        (setf stream (pm::pm-open-input device-id *portmidi-def-buffer-size*))
                        )))
                  (om-lisp::om-print-format "IN ~D => ~A" (list (car item) indevice) "MIDI-CONNECT")
                  (push (list (car item) indevice stream) *portmidi-in-ports-table*) ;;; add this port/stream pair in the table
                  )))

    ;;; OUT
    (loop for item in (cadr settings) do
          (loop for outdevice in (cadr item) do
                (let ((stream (nth 2 (find outdevice *portmidi-out-ports-table* :key 'cadr :test 'string-equal))))
                  (unless stream ;;; no stream is open for this device : create stream and open it
                    (let ((device-id (car (find-if #'(lambda (device) (and (string-equal (getf (cdr device) :name) outdevice) (getf (cdr device) :output))) pm-devices))))
                      (when device-id
                        (setf stream (pm::pm-open-output device-id *portmidi-def-buffer-size* 0)))))
                  (om-lisp::om-print-format "OUT ~D => ~A" (list (car item) outdevice) "MIDI-CONNECT")
                  (push (list (car item) outdevice stream) *portmidi-out-ports-table*)   ;;; add this port/stream pair in the table
                  ))
          )
    t))


(defun get-input-stream-from-port (port)
  (when port
    (let ((device (find port *portmidi-in-ports-table* :key 'car :test '=)))
      (values (nth 2 device) (nth 1 device)))))

(defun get-output-stream-from-port (port)
  (when port
    (let ((device (find port *portmidi-out-ports-table* :key 'car :test '=)))
      (values (nth 2 device) (nth 1 device)))))


(defmethod portmidi-setup (settings)
  (show-portmidi-dialog settings))

(defclass portmidi-ports-dialog (oa::om-dialog)
  ((portviews :accessor portviews :initform nil :initarg :portviews)
   (settings :accessor settings :initform nil :initarg :settings)))

(defclass portmidi-ports-view (oa::om-column-layout)
  ((portlines :accessor portlines :initform nil :initarg :portlines)
   (direction :accessor direction :initform nil :initarg :direction)))


(defmethod set-portmidi-connection-view ((self portmidi-ports-view) dialog)
  (let ((devices (remove nil (loop for ref in (list-devices)
                                   when (nth (if (equal (direction self) :in) 6 8) ref)
                                   collect (nth 4 ref))))
        (pos-in-settings (if (equal (direction self) :in) 0 1)))
    (oa::om-with-delayed-update self

      (apply 'oa::om-remove-subviews (cons self (portlines self)))

      (when (nth pos-in-settings (settings dialog))
        (apply 'oa::om-add-subviews
               (cons self
                     (append
                      (setf (portlines self)
                            (loop for portsetting in (sort (nth pos-in-settings (settings dialog)) '< :key 'car)
                                  collect
                                  (oa::om-make-layout
                                   'oa::om-row-layout
                                   :subviews
                                   (list
                                    (oa::om-make-di 'oa::om-simple-text
                                                    :size (oa::om-make-point 40 24)
                                                    :text (format nil "~D" (car portsetting))
                                                    :font (oa::om-def-font :font2b))

                                    (oa::om-make-di 'oa::om-button
                                                    :size (oa::om-make-point 40 24)
                                                    :text "-"
                                                    :di-action (let ((n (car portsetting)))
                                                                 #'(lambda (button)
                                                                     (declare (ignore button))
                                                                     (setf (nth pos-in-settings (settings dialog))
                                                                           (remove n (nth pos-in-settings (settings dialog)) :key 'car :test '=))
                                                                     (set-portmidi-connection-view self dialog)
                                                                     )))

                                    (oa::om-make-di 'oa:om-popup-list
                                                    :size (oa::om-make-point 200 24)
                                                    :items (cons "[disconnected]" devices)
                                                    :value (car (cadr portsetting)) ;; device for this port
                                                    :di-action (let ((p (position (car portsetting) (nth pos-in-settings (settings dialog))
                                                                                  :test '= :key 'car))
                                                                     (port (car portsetting)))
                                                                 #'(lambda (list)
                                                                     (setf (nth p (nth pos-in-settings (settings dialog)))
                                                                           (list port
                                                                                 (if (= 0 (oa::om-get-selected-item-index list)) nil
                                                                                   (list (oa::om-get-selected-item list))))))))
                                    nil))))
                      nil)))))))



(defun show-portmidi-dialog (settings)

  (let ((dd (oa::om-make-window 'portmidi-ports-dialog
                                :window-title "PortMIDI Setup"
                                :size (oa::om-make-point 600 310)
                                :resizable t
                                :settings settings))
        (inv (oa::om-make-layout 'portmidi-ports-view
                                 :size (oa::om-make-point 300 210)
                                 :direction :in))
        (outv (oa::om-make-layout 'portmidi-ports-view
                                  :size (oa::om-make-point 300 210)
                                  :direction :out)))

    (oa::om-add-subviews
     inv
     (oa::om-make-layout
      'oa::om-row-layout
      :ratios '(1 1 1 100)
      :subviews
      (list
       (oa::om-make-di 'oa::om-simple-text
                       :size (oa::om-make-point 40 24)
                       :text "In"
                       :font (oa::om-def-font :font2b))

       (oa::om-make-di 'oa::om-button
                       :size (oa::om-make-point 40 24)
                       :text "+"
                       :di-action #'(lambda (item)
                                      (declare (ignore item))
                                      (let ((newport 0))
                                        (loop while (find newport (car (settings dd)) :test '= :key 'car)
                                              do (setf newport (1+ newport)))
                                        (setf (settings dd) (list
                                                             (append (car (settings dd)) (list (list newport nil)))
                                                             (cadr (settings dd))))
                                        (set-portmidi-connection-view inv dd)
                                        )))

       (oa::om-make-di 'oa::om-simple-text
                       :size (oa::om-make-point 120 24)
                       :text "Input Devices"
                       :font (oa::om-def-font :font2b))
       NIL
       ))
     )

    (oa::om-add-subviews
     outv
     (oa::om-make-layout
      'oa::om-row-layout
      :ratios '(1 1 1 100)
      :subviews
      (list

       (oa::om-make-di 'oa::om-simple-text
                       :size (oa::om-make-point 40 24)
                       :text "Out"
                       :font (oa::om-def-font :font2b))

       (oa::om-make-di 'oa::om-button
                       :size (oa::om-make-point 40 24)
                       :text "+"
                       :di-action #'(lambda (item)
                                      (declare (ignore item))
                                      (let ((newport 0))
                                        (loop while (find newport (cadr (settings dd)) :test '= :key 'car)
                                              do (setf newport (1+ newport)))
                                        (setf (settings dd) (list
                                                             (car (settings dd))
                                                             (append (cadr (settings dd)) (list (list newport nil)))
                                                             ))
                                        (set-portmidi-connection-view outv dd)
                                        )))

       (oa::om-make-di 'oa::om-simple-text
                       :size (oa::om-make-point 120 24)
                       :text "Output Devices"
                       :font (oa::om-def-font :font2b))
       NIL
       ))
     )

    (setf (portviews dd) (list inv outv))

    (oa::om-add-subviews
     dd
     (oa::om-make-layout
      'oa:om-column-layout
      :ratios '(1 nil)
      :delta 10
      :subviews (list
                 (oa::om-make-layout 'oa:om-row-layout :subviews (list inv outv))
                 (oa::om-make-layout 'oa:om-row-layout :align :bottom
                                     :subviews
                                     (list
                                      (oa::om-make-di 'oa::om-multi-text
                                                      :size (oa::om-make-point 400 32)
                                                      :fg-color (oa:om-def-color :dark-gray)
                                                      :text "This software detects MIDI devices at startup. If some active MIDI devices do not appear in the lists, you might need to restart."
                                                      :font (oa::om-def-font :font1))
                                     ;(oa::om-make-di 'oa::om-button :position (oa::om-make-point 20 265) :size (oa::om-make-point 130 20) :text "Refresh Devices"
                                     ;                :di-action #'(lambda (item)
                                     ;                               (portmidi-connect-ports (settings dd))
                                     ;                               (set-portmidi-connection-view inv dd)
                                     ;                               (set-portmidi-connection-view outv dd)
                                     ;                               ))
                                      NIL
                                      (oa::om-make-di 'oa::om-button
                                                      :size (oa::om-make-point 80 24) :text "Cancel"
                                                      :di-action #'(lambda (item) (declare (ignore item))
                                                                     (oa::om-return-from-modal-dialog dd nil)))
                                      (oa::om-make-di 'oa::om-button
                                                      :size (oa::om-make-point 80 24) :text "OK"
                                                      :di-action #'(lambda (item) (declare (ignore item))
                                                                     (oa::om-return-from-modal-dialog dd (settings dd))))
                                      ))
                 )))

    (set-portmidi-connection-view inv dd)
    (set-portmidi-connection-view outv dd)

    (oa::om-modal-dialog dd)
    ))






