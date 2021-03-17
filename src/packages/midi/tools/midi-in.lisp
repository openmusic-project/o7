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



(defmethod* midi-in (port msg-processing &optional thru)
  :icon 'midi
  :indoc '("port number" "incoming message processing patch" "thru port number")
  :initvals '(nil nil nil)
  :doc "A local MIDI input receiver.

Use 'R' to set the box reactive and activate/deactivate the server.

When the MICI receiver is on, MIDI-IN waits for MIDI messages on port <port> and calls <msg-processing> with the decoded message as parameter.

If <port> is NIL, default MIDI In preference applies.

<msg-processing> (if not NIL) must be a patch in mode 'lambda' with 1 input corresponding to a MIDI message.
This patch should handle and process the incoming messages.

If <thru> is non-NIL, this should be a port number designating an output MIDI port to redirect incoming messages (e.g. to a MIDI synthesizers).
Otherwise, default MIDI Thru preferences apply.
"
  t)


(defmethod boxclass-from-function-name ((self (eql 'midi-in))) 'OMReceiveBox)

(defmethod start-receive-process ((self (eql 'midi-in))) 'start-midi-in)
(defmethod stop-receive-process ((self (eql 'midi-in))) 'stop-midi-in)


(defun msg-to-midievent (event &optional p)
  (make-instance 'midievent
                 :ev-type (om-midi::midi-evt-type event)
                 :ev-fields (om-midi::midi-evt-fields event)
                 :ev-chan (om-midi::midi-evt-chan event)
                 :ev-port p))


(defmethod start-midi-in ((box OMReceiveBox) args)
  (let ((port (or (car args) (get-pref-value :midi :in-port)))
        (fun (cadr args))
        (thru (or (caddr args) (and (get-pref-value :midi :thru)
                                    (get-pref-value :midi :thru-port)))))

    (let ((process (om-midi::portmidi-in-start
                    port
                    #'(lambda (message time)
                        (declare (ignore time))
                        (let* ((me (msg-to-midievent message port))
                               (delivered (if fun (funcall fun me) me)))
                          (set-delivered-value box delivered))
                        )
                    1
                    thru)))
      (when process
        (om-print-format "MIDI-IN start recording on port ~D" (list port) "MIDI")
        (push box *running-midi-boxes*)
        process))
    ))


(defmethod stop-midi-in ((box OMReceiveBox) process)
  (when process
    (om-midi::portmidi-in-stop process)
    (om-print "MIDI-IN stop recording" "MIDI"))
  (setf *running-midi-boxes* (remove box *running-midi-boxes*))
  t)
