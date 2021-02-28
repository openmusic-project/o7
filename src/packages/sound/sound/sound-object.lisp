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


;;;=================
;;; SOUND OBJECT
;;;=================

(in-package :om)



;;; use as :ptr for OM-SOUND-BUFFER
(defun make-audio-buffer (nch size &optional (type :float))
  (fli::allocate-foreign-object
   :nelems nch
   :type :pointer
   :initial-contents (loop for c from 0 to (1- nch)
                           collect (fli::allocate-foreign-object
                                    :type type :nelems size
                                    :initial-element 0.0))))

;;==============================
;; SOUND BUFFER
;;==============================

(defparameter *default-internal-sample-size* :float)

(defstruct (om-sound-buffer (:include oa::om-pointer))
  (nch 1 :type integer))

(defmethod om-pointer-release-function ((self om-sound-buffer)) 'om-cleanup)

;;; if the om-sound-buffer is created like this, it will be garbaged automatically
(defun make-om-sound-buffer-GC (&key ptr (count 1) (nch 1))
  (om-print-dbg "Initializing audio buffer (~A channels)..." (list nch))
  (om-create-with-gc (make-om-sound-buffer :ptr ptr :count count :nch nch)))

;;; this is the garbage action
(defmethod om-cleanup ((self om-sound-buffer))
  (om-print-dbg "Free Audio buffer: ~A" (list self))
  (when (and (oa::om-pointer-ptr self) (not (om-null-pointer-p (oa::om-pointer-ptr self))))
    (audio-io::om-free-audio-buffer (oa::om-pointer-ptr self) (om-sound-buffer-nch self))))

;;; not useful if cleanup buffer works
;(defmethod oa::om-release ((ptr om-sound-buffer))
  ;(om-print (format nil "Release audio buffer ~A in ~A" (oa::om-pointer-ptr ptr) ptr) "SOUND_DEBUG")
;  (when (<= (decf (oa::om-pointer-count ptr)) 0)
    ;(om-print (format nil "CAN FREE Audio buffer ~A in ~A !" (oa::om-pointer-ptr ptr) ptr) "SOUND_DEBUG")
    ;(unless (om-null-pointer-p (oa::om-pointer-ptr ptr))
    ;  (audio-io::om-free-audio-buffer (oa::om-pointer-ptr ptr) (om-sound-buffer-nch ptr)))
;    ))


;;; we never save it on disk
(defmethod omng-save ((self om-sound-buffer)) nil)

;;==============================
;; SOUND
;;==============================

;; schedulable-object
(defclass om-internal-sound (om-cleanup-mixin schedulable-object timed-object)
  ((buffer :accessor buffer :initform nil :initarg :buffer :documentation "the buffer of audio samples")
   (n-samples :accessor n-samples :initform nil :initarg :n-samples :type integer)
   (n-channels :accessor n-channels :initform nil :initarg :n-channels :type integer)
   (sample-rate :accessor sample-rate :initform nil :initarg :sample-rate :type integer)
   (smpl-type :accessor smpl-type :initform :float :initarg :smpl-type)
   (sample-size :accessor sample-size :initform 16 :initarg :sample-size :type integer)
   (mute :accessor mute :initform nil)
   (buffer-player :accessor buffer-player :initform nil :documentation "pointer to a buffer player")))

;; om-internal-sound never explicitly frees its buffer but just releases it.
;; => buffer must be created 'with-GC'
(defmethod om-cleanup ((self om-internal-sound))
  (om-print-dbg "SOUND cleanup: ~A (~A)" (list self (buffer self)))
  (when (buffer self) (oa::om-release (buffer self))))

(defmethod additional-slots-to-copy ((from om-internal-sound))
  (append (call-next-method) '(mute)))

(defmethod excluded-slots-from-copy ((from om-internal-sound))  '(buffer))



;;; CLONE A SOUND
(defmethod clone-object ((self om-internal-sound) &optional clone)

  (let ((snd (call-next-method)))

    (when (buffer self)
      (let ((new-ptr (make-audio-buffer (n-channels self) (n-samples self)))
            (self-ptr (oa::om-pointer-ptr (buffer self))))
        (om-print-dbg "Copying sound buffer (~D x ~D channels)..." (list (n-samples self) (n-channels self)))
        (dotimes (ch (n-channels self))
          (dotimes (smp (n-samples self))
            (setf (cffi::mem-aref (cffi::mem-aref new-ptr :pointer ch) :float smp)
                  (cffi::mem-aref (cffi::mem-aref self-ptr :pointer ch) :float smp))))
        (setf (buffer snd) (make-om-sound-buffer-gc :ptr new-ptr :nch (n-channels self)))
        ))

    snd))


(defmethod release-sound-buffer ((self om-internal-sound))
  (when (buffer self)
    (oa::om-release (buffer self))
    (setf (buffer self) nil)))

;;; called by the box at eval
(defmethod release-previous-value ((self om-internal-sound))
  (release-sound-buffer self))


;;; mostly for compatibility...
(defmethod get-om-sound-data ((self om-internal-sound))
  (buffer self))


(defclass* sound (om-internal-sound data-stream named-object)
  ((markers :initform nil :documentation "a list of markers (ms)")  ;; :accessor markers ;; => accessor is redefined below
   (file-pathname  :initform nil :documentation "a pathname")      ;; :accessor file-pathname ;; => accessor is redefined below
   (gain :accessor gain :initform 1.0 :documentation "gain controller [0.0-1.0]")
   (access-from-file :accessor access-from-file :initform nil :documentation "read from file or allocate buffer"))
  (:icon 'sound)
  (:default-initargs :default-frame-type 'marker-frame)
  (:documentation "Sound object.

A sound can be initialized/attached to a pathname (<file-pathname>) corresponding to an audio file on the disk.
If it is unlocked and unconnected, evaluating the box will open a file chooser dialog and allow the selection of a sound file to load.

The other inputs/outputs :
- <gain> = a gain applied to the audio buffer for playback. If a list is supplied, it will be considered as a set of gains to apply to the different channels of the sound.
- <markers> = a list of markers (time in milliseconds). The markers can also be added/moved/removed from the sound editor.
- <access-from-file> = if this parameter is non-NIL, then this sound will work without an internal audio buffer, that is, referring to a file on disk.

Press 'space' to play/stop the sound file.
"))

;(references-to (find-class 'sound))
;(setf (references-to (find-class 'sound)) nil)

(defmethod additional-class-attributes ((self sound))
  '(markers
    file-pathname
    sample-rate
    n-channels
    n-samples
    gain
    access-from-file))

(defmethod play-obj? ((self sound)) t)

(defmethod get-obj-dur ((self sound))
  (if (and (sample-rate self) (n-samples self)) (* 1000.0 (/ (n-samples self) (sample-rate self))) 0))


(defmethod box-def-self-in ((self (eql 'sound))) :choose-file)

(defmethod default-name ((self sound))
  (when (file-pathname self)
    (if (stringp (pathname-type (file-pathname self)))
        (string+ (pathname-name (file-pathname self)) "." (pathname-type (file-pathname self)))
      (pathname-name (file-pathname self)))))


;;; specialized to save sound pathname relative
;;; + prevent saving unnecessary slots
(defmethod omng-save ((self sound))
  `(:object
    (:class ,(type-of self))
    (:slots
     ((:onset 0)
      (:duration ,(get-obj-dur self))
      (:n-samples ,(n-samples self))
      (:n-channels ,(n-channels self))
      (:sample-rate ,(sample-rate self))
      (:smpl-type ,(smpl-type self))
      (:sample-size ,(sample-size self))))
    (:add-slots
     ((:markers ,(omng-save (markers self)))
      (:gain ,(gain self))
      (:access-from-file ,(access-from-file self))
      (:file-pathname ,(and (file-pathname self)
                            (omng-save (relative-pathname
                                        (file-pathname self)
                                        *relative-path-reference*))))
      ))
    ))


;;;===========================
;;; UTILS
;;;===========================

;;; GET A SOUND OBJECT FROM ...
; used in :
; spat-object (sources)
; merge-sounds
; with-sound-buffer

;;; called when, for some reason, we want a full-featured sound (with info etc.)
(defmethod get-sound ((self sound)) self)
(defmethod get-sound ((self t)) nil)

(defmethod get-sound ((self om-internal-sound))
  (om-init-instance (clone-object self (make-instance 'sound)) nil))


;; `((:file-pathname ,self) (:access-from-file t))
(defmethod get-sound ((self pathname))
  (when (probe-file self)
    (let ((snd (make-instance 'sound)))
      (setf (file-pathname snd) self)
      (om-init-instance snd)
      )))

(defmethod get-sound ((self string)) (get-sound (pathname self)))

(defmethod get-sound-name ((self pathname)) (pathname-name self))
(defmethod get-sound-name ((self string)) self)
(defmethod get-sound-name ((self sound)) (if (file-pathname self) (pathname-name (file-pathname self)) "sound"))
(defmethod get-sound-name ((self t)) "invalid sound")

(defmethod get-sound-file ((self pathname)) self)
(defmethod get-sound-file ((self string)) (pathname self))
(defmethod get-sound-file ((self sound)) (get-sound-file (file-pathname self)))
(defmethod get-sound-file ((self t)) nil)


;;; cross-version API used in libs etc.
(defmethod om-sound-file-name ((self sound)) (file-pathname self))
(defmethod om-sound-sample-rate ((self sound)) (sample-rate self))
(defmethod om-sound-n-samples ((self sound)) (n-samples self))
(defmethod om-sound-n-channels ((self sound)) (n-channels self))


;;; UTILS FOR SYNTHESIS AND PROCESSING

(defmacro with-sound-output ((snd &key (size 0) (nch 1) (sr 44100) (type :float)) &body body)
  `(let ((,snd (make-instance 'sound :n-samples ,size :n-channels ,nch :sample-rate ,sr :smpl-type ,type
                              :buffer (make-om-sound-buffer-GC :ptr (make-audio-buffer ,nch ,size ,type) :nch ,nch))))
     ,@body

     ,snd))

(defmacro write-in-sound (snd chan pos value)
  `(setf (fli:dereference
          (fli:dereference (oa::om-pointer-ptr (buffer ,snd)) :index ,chan :type :pointer)
          :index ,pos :type (smpl-type ,snd))
         ,value)
  )

(defmacro read-in-sound (snd chan pos)
  `(fli:dereference
    (fli:dereference (oa::om-pointer-ptr (buffer ,snd)) :index ,chan :type :pointer)
    :index ,pos :type (smpl-type ,snd)))


#|
;;; example of use:

(defun synth (dur freq gain envelope)

  (let* ((sr 44100)
         (nbsamples (round (* dur sr)))
         (freqs (list! freq))
         (steps (loop for f in freqs collect (/ f sr)))
         (sampled-envelope (om-scale (nth 2 (multiple-value-list (om-sample envelope nbsamples))) 0.0 1.0)))

    (with-sound-output (mysound :nch 2 :size nbsamples :sr 44100 :type :float)

      (loop for x from 0 to (1- nbsamples)
            for y-list = (make-list (length steps) :initial-element 0) then (om+ y-list steps)
            for amp in sampled-envelope
            do
            (write-in-sound mysound 1 x
                            (* gain amp
                               (apply '+
                                      (loop for y in y-list collect
                                            (sin (* 2 (coerce pi 'single-float) (cadr (multiple-value-list (floor y))))))

                                      ))
                            )
            )
      )))
|#

;;;===========================
;;; TIME MARKERS
;;;===========================

(defclass marker-frame (data-frame)
  ((label :accessor label :initarg :label :initform nil)))

(defmethod print-object ((self marker-frame) out)
  (format out "#<marker-at ~D: ~A>" (date self) (label self)))

(defmethod data-frame-text-description ((self marker-frame))
  (list (format nil "t=~A" (date self))
        (or (label self) "")))

(defmethod get-frame-action ((self marker-frame)) nil)

(defmethod get-frame-graphic-duration ((self marker-frame)) 10)
(defmethod get-frame-color ((self marker-frame)) (om-make-color .5 .3 .3))
(defmethod get-frame-posy ((self marker-frame)) 100)
(defmethod get-frame-sizey ((self marker-frame)) 200)

(defmethod draw ((frame marker-frame) x y w h selected)
  (om-draw-line x y x h :line (if selected 3 2) :style '(4 4) :color (if selected (om-def-color :dark-red) nil))
  (when (label frame)
    (om-draw-string (+ x 4) (+ y 12) (format nil "~A" (label frame)) :color (if selected (om-def-color :dark-red) nil)))
  t)



(defmethod markers-time ((self sound))
  (loop for frame in (data-stream-get-frames self)
        collect (date frame)))

(defmethod get-time-markers ((self sound)) (markers-time self))


;;; (RE)DEFINE MARKERS ACCESSOR FOR SOUND

;;; for the user markers are just numbers
;;; (in fact they are data-frames)
(defmethod markers ((self sound))
  (loop for frame in (data-stream-get-frames self)
        collect (if (label frame)
                    (list (date frame) (label frame))
                  (date frame))
        ))

(defmethod (setf markers) (markers (self sound))

  ;;; markers used to be in seconds in OM6... :-s
  (when (find-if #'floatp markers)
    (om-print "Warning: float-formatted markers will be converted to milliseconds" "SOUND-BOX")
    (setf markers (sec->ms markers)))

  (data-stream-set-frames
   self
   (loop for m in markers collect
         (cond ((numberp m)
                (make-instance 'marker-frame :date m))
               ((consp m)
                (make-instance 'marker-frame :date (car m) :label (cadr m)))
               ((typep m 'marker-frame)
                (om-copy m)))
         ))
  markers)


;;;===========================
;;; INITIALIZATION...
;;;===========================

#|
;; NOT USED FOR THE MOMENT
(defmacro om-sound-protect (sound &body body)
  `(if (equal (loaded ,sound) :error)
       (progn
         (print (format nil "sound ~s is disabled because of previous errors" (namestring (filename ,sound))))
         nil)
     (or (ignore-errors ,@body)
         (progn
           (print (format nil "error in sound ~s" (namestring (filename ,sound))))
           (setf (loaded ,sound) :error)
           nil))))
|#

(defmethod objFromObjs ((model om-internal-sound) (target sound))
  (clone-object model target))

(defmethod objFromObjs ((model sound) (target sound))
  (clone-object model target))

(defmethod objFromObjs ((model t) (target sound))
  (when (get-sound-file model)
    (objFromObjs (get-sound-file model) target)))

(defmethod objFromObjs ((model pathname) (target sound))
  (setf (file-pathname target) model)
  target)

(defmethod objFromObjs ((model (eql :choose-file)) (target sound))
  (let ((file (om-choose-file-dialog :prompt "Choose an audio file AIFF/WAV..."
                                     :types '("Audio files" "*.aiff;*.aif;*.wav"
                                              "AIFF files" "*.aiff;*.aif" "WAV files" "*.wav"))))
    (if file (objFromObjs file target)
      (om-abort))))

;;;=================================
;;; SOUND INIT RULES:
;;;=================================

;;; (RE)DEFINE file-pathname ACCESSORS

(defmethod (setf file-pathname) (new-path (self sound))
  (when new-path
    ;;; we're assigning a new file-pathname
    (let* ((old-path (slot-value self 'file-pathname))
           (final-new-path (if (buffer self)
                               ;;; a buffer was already in: save the buffer in the new file
                               (save-sound-data self new-path)
                             ;;; a path alreadt existed: copy in new
                             (if old-path (om-copy-file old-path new-path)))))
      (setf (slot-value self 'file-pathname) new-path)
      final-new-path)))

(defmethod file-pathname ((self sound))
  (slot-value self 'file-pathname))

(defmethod set-play-buffer ((self sound))

  (if (and (file-pathname self) (access-from-file self))
      (progn
        (om-print-dbg "Initializing FILE player for sound ~A (~D channels)"
                      (list self (n-channels self)))
        (setf (buffer-player self) (make-player-from-file (namestring (file-pathname self)))))

    (when (buffer self) ;;; in principle at that point there should be a buffer..
      (if (and (n-samples self) (n-channels self) (sample-rate self))
          (progn
            (om-print-dbg "Initializing BUFFER player for sound ~A (~D channels)"
                          (list self (n-channels self)))
            (setf (buffer-player self) (make-player-from-buffer
                                        (oa::om-pointer-ptr (buffer self))
                                        (n-samples self) (n-channels self) (sample-rate self))))
        (om-beep-msg "Incomplete info in SOUND object. Could not instanciate the player !!")
        ))
    ))


(defmethod om-init-instance ((self sound) &optional initargs)

  (call-next-method)

  (if (access-from-file self)

      (if (and (valid-pathname-p (file-pathname self))
               (file-exists-p (file-pathname self)))

          ;; We want to keep working with this file (and no buffer)
          (if (buffer self)

              (release-sound-buffer self)

            (set-sound-info self (file-pathname self)))

        (om-beep-msg "Cannot use ACCESS-FROM-FILE without a valid file !!"))

    ;;; else we set the buffer (if needed)
    (when (and (file-pathname self)
               (not (buffer self)))
      (set-sound-data self (file-pathname self)))

    )

  ;;; SET A PLAYER IN ANY CASE !
  ; (set-play-buffer self)  ;; be lazy => do it later!

  self)

#|

;;; IF <SELF> + NO <FILE-PATHNAME>
;;; - access-from-file = T => SET FILE-PATHNAME and NO BUFFER
;;; - access-from-file = NIL => SET BUFFER and FILE-PATHNAME = NIL
;;; IF <SELF> + <FILE-PATHNAME> (same)
;;; - access-from-file = T => NO BUFFER, JUST SET SOUND INFO
;;; - access-from-file = NIL => SET BUFFER
;;; IF <SELF> + <FILE-PATHNAME> (different)
;;; - access-from-file = T => SAVE INCOMING DATA TO FILE-PATHNAME, NO BUFFER
;;; - access-from-file = NIL => SAVE INCOMING DATA TO FILE-PATHNAME, KEEP BUFFER
;;; IF NO <SELF> AND <FILE-PATHNAME> ONLY
;;; - access-from-file = T => NO BUFFER - JUST SET SOUND INFO
;;; - access-from-file = NIL => SET BUFFER


(cond ((and FILE-IN
                (file-pathname self)
                (string-equal (namestring (file-pathname self)) (namestring FILE-IN)))
           ;;; FILE-IN maches the current file
           (if access-from-file
               ;; We want to keep working with this file (and no buffer)
               (if (buffer self)
                   (release-sound-buffer self)
                 (set-sound-info self FILE-IN))
             ;;; else we set the buffer (if needed)
             (unless (buffer self)
               (set-sound-data self FILE-IN))))

          ((and FILE-IN (file-pathname self))
           ;;; there was already a file but different
           (om-copy-file (file-pathname self) FILE-IN)
           (setf (file-pathname self) FILE-IN)
           (if access-from-file
               ;; We want to keep working with this file (and no buffer)
               (if (buffer self)
                   (release-sound-buffer self)
                 (set-sound-info self FILE-IN))
             ;;; create new buffer
             (set-sound-data self FILE-IN)))

          ((and FILE-IN (buffer self))
           ;;; There was no file in the sound but already a buffer
           (save-sound-data self FILE-IN)
           (setf (file-pathname self) FILE-IN)
           (if access-from-file
               (release-sound-buffer self)))

          (FILE-IN
           ;;; there was nothing before...
           (if access-from-file
               (progn (setf (file-pathname self) FILE-IN)
                 (set-sound-info self FILE-IN))
             (set-sound-data self FILE-IN)))

          ((file-pathname self)
           ;;; No FILE-IN given but a file was in
           (if access-from-file
               (if (buffer self)
                   (release-sound-buffer self)
                 (set-sound-info self (file-pathname self)))
             (unless (buffer self) ;;; create the buffer if needed
               (set-sound-data self (file-pathname self)))))

          (t
           ;;; there was no file and no in-file is given
           (if access-from-file
               (om-beep-msg "Cannot use ACCESS-FROM-FILE without a file !!"))
           ;;; => if there was a buffer we just keep it
           )
          )
|#

;;; executes its body with buffer-name bound to a valid audio buffer
;;; this buffer can be found in sound or produced from the filename
;;; in the second case, it is freed at the end
;;; sound must also have a valid n-samples and n-channels
;;; => do it without '-GC' ?
(defmacro with-audio-buffer ((buffer-name sound) &body body)
  `(let ((snd (get-sound ,sound)))
     (if snd
         (let* ((tmp-buffer (unless (buffer snd)
                              (when (and (valid-pathname-p (file-pathname snd))
                                         (probe-file (file-pathname snd))
                                         (n-channels snd) (n-samples snd))
                                (make-om-sound-buffer-GC
                                 :count 1 :nch (n-channels snd)
                                 :ptr (audio-io::om-get-audio-buffer (namestring (file-pathname snd)) *default-internal-sample-size*)))))
                (,buffer-name (or tmp-buffer (buffer snd))))
           (unwind-protect
               (progn
                 (unless ,buffer-name (om-print-format "Warning: no sound buffer allocated for ~A" (list (file-pathname snd))))
                 ,@body)
             (when tmp-buffer (oa::om-release tmp-buffer))
             ))
       (om-beep-msg "Wrong input type for sound: ~A" ,sound))
     ))




(defun set-sound-data (sound path)

  (when (buffer sound) (oa::om-release (buffer sound)))

  (if (probe-file path)
      (progn
        (om-print-dbg "Loading sound from: ~s" (list path))
        (multiple-value-bind (buffer format channels sr ss size)

            (audio-io::om-get-audio-buffer (namestring path) *default-internal-sample-size* nil)
          (declare (ignore format))

          (when buffer
            (unwind-protect
                (progn
                  (setf (buffer sound) (make-om-sound-buffer-GC :ptr buffer :count 1 :nch channels)
                        (smpl-type sound) *default-internal-sample-size*
                        (n-samples sound) size
                        (n-channels sound) channels
                        (sample-rate sound) sr
                        (sample-size sound) ss)
                  sound)))))
    (progn
      (om-beep-msg "Unable to load soudn from: ~s" path)
      (setf (buffer sound) nil)
      nil)
    ))


(defun set-sound-info (sound path)

  (if (probe-file path)

      (multiple-value-bind (format channels sr ss size)
          (audio-io::om-get-sound-info (namestring path))
        (declare (ignore format))

        (setf (n-samples sound) size
              (n-channels sound) channels
              (sample-rate sound) sr
              (sample-size sound) ss)
        )

    (om-beep-msg "Unable to load soudn from: ~s" path))

  sound)


(defun interleave-buffer (in out samples channels)
  (dotimes (smp samples)
    (dotimes (ch channels)
      (setf (cffi::mem-aref out :float (+ (* smp channels) ch))
            (cffi::mem-aref (cffi::mem-aref in :pointer ch) :float smp))))
  out)

(defun split-buffer (in out samples channels)
  (dotimes (smp samples)
    (dotimes (ch channels)
      (setf (cffi::mem-aref (cffi::mem-aref out :pointer ch) :float smp)
            (cffi::mem-aref in :float (+ (* smp channels) ch)))))
  out)


(defun save-sound-data (sound path)
  (when (buffer sound)
    (let* ((nch (n-channels sound))
           (nsmp (n-samples sound)))
      (om-print-format "Writing file to disk: ~S" (list path))
      (audio-io::om-save-buffer-in-file (oa::om-pointer-ptr (buffer sound))
                                        (namestring path)
                                        nsmp nch (sample-rate sound)
                                        (get-pref-value :audio :resolution)
                                        (get-pref-value :audio :format)
                                        )
      (or (probe-file path)
          (om-beep-msg "Error -- no file written")))
    ))



;;;===========================
;;; METHODS
;;;===========================


(defmethod* sound-points ((self sound) (num integer) &optional channel)
  :initvals '(nil 1000 1)
  :indoc '("a sound object" "number of points" "channel number")
  :doc "Reurns <num> sampled points from the audio waveform of channel <channel> in <self>."
  :icon 'sound
  (with-audio-buffer (b self)
    (let ((numdat (n-samples self))
          (numchan (n-channels self))
          (ch (or channel 1)))
      (if (or (> ch numchan) (> num numdat))
          (om-beep-msg "SOUND-POINTS: out-of-range input values !!")
        (let ((channel-ptr (om-read-ptr (om-sound-buffer-ptr b) (1- ch) :pointer)))
          (loop for i from 0 to numdat by (round numdat num) collect
                (om-read-ptr channel-ptr i :float)))
        ))))


(defmethod* sound-dur ((sound sound))
  :icon 'sound
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in seconds."
  (if (and (n-samples sound) (sample-rate sound)
           (> (sample-rate sound) 0))
      (float (/ (n-samples sound) (sample-rate sound)))
    0))

(defmethod* sound-dur ((sound pathname))
  (sound-dur (namestring sound)))

(defmethod* sound-dur ((sound string))
  (if (probe-file sound)
      (multiple-value-bind (format channels sr ss size)
          (audio-io::om-get-sound-info sound)
        (declare (ignore format channels ss))

        (if (and size sr (> sr 0)) (float (/ size sr)) 0))
    (progn (om-beep-msg "File not found: ~s" sound) 0)))

(defmethod* sound-dur-ms ((sound t))
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in milliseconds."
  :icon 'sound
  (round (* 1000 (sound-dur sound))))


;;;====================
;;; PLAYER
;;;====================
;;;Methods redefinitions using the slot "data" of schedulable object to bypass actions rendering and store the las pointer
(defmethod get-action-list-for-play ((self sound) time-interval &optional parent)
  (external-player-actions self time-interval parent))

(defmethod player-play-object ((self scheduler) (object sound) caller &key parent interval)

  (declare (ignore parent))

  (unless (buffer-player object) (set-play-buffer object))

  (when (buffer-player object)
    (start-buffer-player (buffer-player object)
                         :start-frame (if (car interval)
                                          (round (* (car interval) (/ (sample-rate object) 1000.0)))
                                        (or (car interval) 0))))
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object sound))
  (when (buffer-player object)
    (stop-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object sound))
  (when (buffer-player object)
    (pause-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object sound))
  (when (buffer-player object)
    (continue-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod set-object-current-time ((self sound) time)
  (when (buffer-player self)
    (jump-to-time (buffer-player self) time))
  (call-next-method))


;;; UTIL FUNCTION FOR JUST PLAYING A SOUND (NOW) E.G. IN A PATCH
(defmethod play-sound ((sound om-internal-sound))
  (player-play-object *general-player* sound nil))



;;;============================================
;;; DISPLAY ARRAY
;;;============================================

;;;Fill a sound's display array (for waveform)
;;;Inspired from OM6's display-array (to do)
(defun fill-sound-display-array (audio-ptr audio-ptr-size array-ptr array-size n-channels)
  ;(print (list audio-ptr audio-ptr-size (* audio-ptr-size n-channels) array-ptr array-size (* array-size n-channels) n-channels))
  (let* ((sample-ratio (/ audio-ptr-size array-size))
         (window (floor sample-ratio))
         (maxindx (1- (* n-channels audio-ptr-size)))
         (maxi 0.0)
         (pos-in-buffer 0))
    (if (or (fli:null-pointer-p audio-ptr) (fli:null-pointer-p array-ptr))
        (print "ERROR BUILDING DISPLAY ARRAY: NULL POINTER")
        ;(print (list window maxindx))
      (dotimes (array-frame array-size)
          ;(print (list "::array-cell" array-frame))
        (setq pos-in-buffer (floor (* sample-ratio array-frame)))
        (dotimes (chan n-channels)
            ;(print (list "::::ch" chan))
          (dotimes (i window)
              ;(print (list "POS IN SOUND" (+ (* pos-in-buffer n-channels) (+ chan (* n-channels i)))))
            (setq maxi (max (abs
                               ;(fli:dereference audio-ptr :type :float :index (min maxindx (+ (* pos-in-buffer n-channels) (+ chan (* n-channels i)))))
                             (fli:dereference
                              (fli:dereference audio-ptr :type :pointer :index chan)
                              :type :float :index (min maxindx (+ pos-in-buffer i)))
                             ) maxi)))
          (setf (fli:dereference array-ptr :index (+ array-frame (* chan array-size))) maxi)
            ;(print (list "POS IN ARRAY" (+ array-frame (* chan array-size)) maxi))
          (setq maxi 0.0))))))

(defun resample-2D-array (array start-pos end-pos nbpix)
  (let* ((n-channels (car (array-dimensions array)))
         (slice-size (- end-pos start-pos))
         (maxi 0.0)
         result)
    (cond ((= nbpix slice-size)
           (setq result (make-array (list n-channels slice-size) :element-type 'single-float))
           (dotimes (i nbpix)
             (dotimes (n n-channels)
               (setf (aref result n i) (aref array n (+ start-pos i))))))
          ((< nbpix slice-size)
           (let ((step (/ slice-size nbpix 1.0)))
             (setq result (make-array (list n-channels nbpix) :element-type 'single-float))
             (dotimes (c n-channels)
               (dotimes (i nbpix)
                 (dotimes (j (floor step))
                   (setq maxi (max maxi (aref array c (round (+ start-pos j (* i step)))))))
                 (setf (aref result c i) maxi)
                 (setq maxi 0.0)))))
          (t (om-print "ERROR: ARRAY IS SMALLER THAN NBPIX") nil))
    (values result (if result (< (cadr (array-dimensions result)) nbpix) t))))


;;; CREATES An INTERNAL PICTURE FROM MAX DETECTION OVER DOWNSAMPLED BUFFER
(defun create-waveform-pict (array &optional color)
  (when array
    (let ((pict-h 1000)
          (nch (car (array-dimensions array)))
          (array-size (cadr (array-dimensions array))))
      (when (and (> nch 0) (> array-size 0))
        (let* ((channels-h (round pict-h nch))
               (offset-y (round channels-h 2))
               pixpoint pixpointprev)

          (om-record-pict array-size 1000
          ;(om-draw-rect 0 0 array-size 1000 :fill t)
            (om-with-fg-color color
              (dotimes (c nch)
                (let ((ch-y (+ (* c channels-h) offset-y)))
                  (om-draw-line 0 ch-y array-size ch-y)
                  (setq pixpointprev (* offset-y (* 0.99 (aref array c 0))))
                  (loop for i from 1 to (1- array-size) do
                        (setf pixpoint (* offset-y (* 0.99 (aref array c (min i (1- array-size))))))
                        (unless (= pixpointprev pixpoint 0)
                          (om-draw-polygon `(,(om-make-point (1- i) (+ ch-y pixpointprev))
                                             ,(om-make-point i (+ ch-y pixpoint))
                                             ,(om-make-point i (+ ch-y (- pixpoint)))
                                             ,(om-make-point (1- i) (+ ch-y (- pixpointprev))))
                                           :fill t))
                        (setq pixpointprev pixpoint))))
              ))
          )))
    ))


(defun get-pict-from-sound (self)
  (when (and (n-samples self) (> (n-samples self) 0) (n-channels self)
             (or (buffer self) (file-pathname self)))
    (let* ((window 128)
           (pictsize 512)
           (array-size (floor (n-samples self) window))
           (array (make-array (list (n-channels self) array-size)
                              :element-type 'single-float :initial-element 0.0 :allocation :static))
           )
      (with-audio-buffer (b self)
        (if b
            (let ()
              (fli:with-dynamic-lisp-array-pointer
                  (ptr array :type :float)
                (fill-sound-display-array (om-sound-buffer-ptr b)
                                          (n-samples self) ptr array-size (n-channels self)))

              (create-waveform-pict
               (resample-2D-array array 0 array-size (min array-size pictsize))
               (om-make-color 0.41 0.54 0.67)))
          :error
          )))))


(defmethod get-cache-display-for-draw ((self sound) box)
  (declare (ignore box))
  (get-pict-from-sound self))




;;;===========================
;;; BOX
;;;===========================

;(defclass OMBoxSound (omboxeditcall) ())
;(defmethod special-box-type ((self (eql 'sound))) 'OMBoxSound)

(defmethod display-modes-for-object ((self sound))
  '(:mini-view :text :hidden))

(defmethod get-cache-display-for-text ((object sound) box)
  (declare (ignore box))
  (append (call-next-method)
          (list (list :buffer (buffer object)))))

(defmethod draw-mini-view ((self sound) (box t) x y w h &optional time)
  (let ((pict (ensure-cache-display-draw box self)))
    (cond
     ((equal pict :error)
      (om-with-fg-color (om-make-color .8 .4 .4)
        (om-with-font (om-def-font :font1b)
                      (om-draw-string (+ x 6) (+ y 12) "File not loaded:" :wrap (- w 20)))
        (when (file-pathname self)
          (om-with-font (om-def-font :font1 :size 10)
                        (om-draw-string (+ x 6) (+ y 24) (namestring (file-pathname self)) :wrap (- w 20))))
        ))

     (pict
      (om-draw-picture pict :x x :y (+ y 4) :w w :h (- h 8)))

     (t
      (om-draw-string (+ x 6) (+ y 12) "NO SOUND" :color (om-def-color :white) :font (om-def-font :font1b)))
     )

    (when (markers self)
      (let* ((dur (if (plusp (get-obj-dur self))
                      (get-obj-dur self)
                    (+ (last-elem (markers self))
                       (- (last-elem (markers self))
                          (or (last-elem (butlast (markers self))) 0)))))
             (fact (/ w dur)))

        (loop for mrk in (markers-time self) do
              (om-with-fg-color (om-def-color :gray)
                (om-draw-line (+ x (* mrk fact)) 8 (+ x (* mrk fact)) h
                              :style '(2 2)
                              ))
              )))
    ))


;;;====================
;;; EDITOR
;;;====================

(defclass sound-editor (data-stream-editor)
  ((cache-display-list :accessor cache-display-list :initform nil)))

(defclass sound-panel (stream-panel) ())
(defmethod get-editor-class ((self sound)) 'sound-editor)
(defmethod editor-view-class ((self sound-editor)) 'sound-panel)

(defmethod window-title-for-object ((self sound))
  (string+ "SOUND - "
           (if (file-pathname self)
               (namestring (file-pathname self))
             "Temp Buffer")))

(defmethod frame-display-modes-for-object ((self sound-editor) (object sound)) '(:lines))

(defmethod editor-view-after-init-space ((self sound)) 0)


(defmethod default-editor-min-x-range ((self sound-editor)) 0)

(defmethod om-draw-contents ((self sound-panel))
  (call-next-method)
  (let* ((editor (editor self))
         (sound (if (multi-display-p editor)
                    (nth (stream-id self) (multi-obj-list editor))
                  (object-value editor))))
    (if (or (buffer sound) (and (access-from-file sound) (valid-pathname-p (file-pathname sound))))
        ;;; draw the sound..
        (draw-sound-waveform sound editor self (x1 self) (x2 self) (stream-id self))
      ;;; no sound
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-font (om-make-font "Arial" (round (h self) 4) :style '(:bold))
                      (om-draw-string 10 (+ (round (h self) 2) (round (h self) 8)) "No sound loaded..")
                      ))
      )
    ))


(defun draw-sound-waveform (sound editor view from to &optional (sound-id 0))
  (unless (nth sound-id (cache-display-list editor))
    (setf (cache-display-list editor)
          (if (multi-display-p editor)
              (mapcar #'(lambda (o) (get-cache-display-for-draw o nil)) (multi-obj-list editor))
            (list (get-cache-display-for-draw (object-value editor) nil)))))
  (let ((dur (get-obj-dur sound))
        (pict (nth sound-id (cache-display-list editor))))
    (when (and pict (not (equal :error pict)))
      (om-draw-picture pict
                       :w (w view) :h (h view)
                       :src-h (* (om-pict-height pict))
                       :src-x (* (om-pict-width pict) (/ from dur))
                       :src-w (* (om-pict-width pict) (/ (- to from) dur))))
    ))


(defmethod update-to-editor ((editor sound-editor) (from t))
  (unless (or (equal from editor)
              (and (multi-display-p editor)
                   (equal from (container-editor editor))))
    (setf (cache-display-list editor) nil))
  (call-next-method))


(defmethod editor-key-action ((editor sound-editor) key)
  (let* ((panel (active-panel editor))
         (stream (object-value editor)))
    (case key
      (#\l
       (when (selection editor)
         (let ((newlabel (om-get-user-string "enter a new label for selected markers"
                                             :initial-string (or (label (nth (car (selection editor)) (frames stream)))
                                                                 ""))))
           (when newlabel
             (loop for pos in (selection editor) do
                   (setf (label (nth pos (frames stream))) newlabel))
             (om-invalidate-view panel)
             (report-modifications editor)
             )
           )))
      (:om-key-esc NIL) ;;; we don't want to reinit-x-ranges as in the next-method
      (otherwise (call-next-method)))
    ))



#|

(defmethod om-draw-waveform ((self soundPanel))
  (multiple-value-bind (data smplevel)
      (om-get-display-slice self)
    (when (and (> (car (array-dimensions data)) 0) (> (cadr (array-dimensions data)) 0))
      (let* ((thesound (object (om-view-container self)))
           (window-v-size (* 0.99 (om-point-v (om-view-size self))))
           (system-etat (get-system-etat self))
           (xmin (car (rangex self)))
           (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
           (xmax (cadr (rangex self)))
           (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
           (xtime (- xmax xmin))
           (nbpts (cadr (array-dimensions data)))
           (timestep (/ xtime (coerce nbpts 'single-float)))
           (nch (om-sound-n-channels thesound))
           (channels-h (round window-v-size nch))
           (offset-y (round channels-h 2))
           pixpoint
           pixtime
           pixprev pixtprev)
      (when data
        (om-with-fg-color nil *om-steel-blue-color*
          ;(when smplevel    ;;;Use this "when" only when HQ display is used below
          (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 0.5) ;;;=*om-steel-blue-color* with 50% transparency
            (dotimes (i nch)
              (om-draw-line pixmin (- (+ (* i channels-h) offset-y) 10) pixmax (- (+ (* i channels-h) offset-y) 10))));)
          (dotimes (c nch)
            (setq pixprev (round (* offset-y (* 0.99 (aref data c 0)))))
            (setq pixtprev (om-point-h (point2pixel self (om-make-point (round xmin) 0) system-etat)))
            (loop for i from 1 to (1- (cadr (array-dimensions data))) do
                  (let ((val (aref data c i)))
                    (setf pixtime (om-point-h (point2pixel self (om-make-point (+ xmin (* i timestep)) 0) system-etat)))
                    (setf pixpoint (* offset-y (* 0.99 val))) ; scaled 0-1 --> 0 -->256/2
                    (if (not smplevel) ;(= nbpts nbpix)
                        (progn
                          ;;;Use this for HQ display : requires a bit more computation to display middle lines with dynamic intensity
                          ;(when (not smplevel)
                          ;  (let ((colorval (- 1 (log (1+ (* (1- (exp 1)) (expt (abs val) 0.1)))))));(- 1 (expt (abs val) 0.2))));(expt (abs val) 0.2)))
                          ;    (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 colorval)
                          ;      (om-draw-line pixtprev (- (+ (* c channels-h) offset-y) 10) pixtime (- (+ (* c channels-h) offset-y) 10)))))
                          (om-fill-polygon `(,(om-make-point pixtprev (- (+ offset-y (* c channels-h) pixprev) 9))
                                             ,(om-make-point pixtime (- (+ offset-y (* c channels-h) pixpoint) 9))
                                             ,(om-make-point pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 9))
                                             ,(om-make-point pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 9)))))
                      (om-draw-line pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 10) pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 10)))
                    (setq pixprev pixpoint pixtprev pixtime))))))))))



(defmethod om-get-display-slice ((self soundpanel))
  (let* ((snd (object (om-view-container self)))
         (system-etat (get-system-etat self))
         (xmin (car (rangex self)))
         (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
         (xmax (cadr (rangex self)))
         (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
         (nbpix (- pixmax pixmin)))
    (sound-get-display-array-slice snd nbpix xmin xmax)))


;;;===================
;;; DISPLAY-ARRAY
;;;===================

;;; Function used to FILL the display array of a sound (and choosed max window)
;;; (use LIBSNDFILE and FLI)
(defmethod om-fill-sound-display-array ((format t) path ptr channels size &optional (window 128))
  #+libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames) :type :int :index #+powerpc 1 #-powerpc 0))
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           (buffer-size (* window channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))   ;Fenêtrage du son
           ;(MaxArray (make-array (list channels (ceiling size window)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (ceiling size window)))
           (frames-read 0)
           maxi)
      (loop for indx from 0 do ;(print (list indx "/" (ceiling size window)))
            (setq frames-read (sf::sf-readf-float sndfile-handle buffer window))
            (dotimes (n channels)
              (dotimes (i window)
                (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
              ;(setf (aref MaxArray n (min indx indxmax)) maxi)
              (setf (fli:dereference ptr :index (+ (min indx indxmax) (* n (ceiling size window)))) maxi)
              (setq maxi 0.0))
            while (= frames-read window))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle))))


;(ratio (round (om-sound-n-samples self) 2000)))) for variable ratio. 2000 +/- screen size
; ok for small files, not good for big ones


;;; not used for the moment
(defmethod build-display-array-dynamic ((self sound))
  (let* ((ratio 128)
         (size (om-sound-n-samples self))
         (channels (om-sound-n-channels self))
         ;(array-width (ceiling size ratio))
         )
    (setf (display-ratio self) ratio
          (display-builder self) (om-run-process
                                  "DisplayArrayBuilder"
                                  #'(lambda (snd)
                                      (setf (display-array snd)
                                            (make-array (list channels (ceiling size ratio))
                                                        :element-type 'single-float :initial-element 0.0 :allocation :static))
                                      (fli:with-dynamic-lisp-array-pointer
                                          (ptr (display-array snd) :type :float)
                                        (om-fill-sound-display-array (namestring (filename snd)) ptr ratio))
                                      (sound-get-best-pict snd)
                                      (setf (display-builder self) nil)
                                      (print (format nil "~A Loaded..." (filename self)))) self))))


(defmethod build-display-array ((self sound))
  (let ((winsize 128)
        (format (om-sound-format self))
        (channels (om-sound-n-channels self)))
    (when (and format channels)
      (let ((array-width (ceiling (om-sound-n-samples self) winsize)))
        (setf (display-ratio self) winsize)
      ;"DisplayArrayBuilder"
        (funcall
         #'(lambda (snd)
             (setf (display-array snd)
                   (make-array (list channels array-width)
                               :element-type 'single-float :initial-element 0.0 :allocation :static))
             (fli:with-dynamic-lisp-array-pointer
                 (ptr (display-array snd) :type :float)
               (om-fill-sound-display-array format (namestring (filename snd)) ptr channels array-width winsize))
             (sound-get-best-pict snd)
        ;(setf (display-builder self) nil)
             (print (format nil "~A Loaded..." (filename self))))
         self)))))



(defun om-get-sound-display-array-slice (path nsmp-out start-time end-time)
  #+libsndfile
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    ;;;Initialisation du descripteur
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0)
    (let* (;;;Remplissage du descripteur et affectation aux variables temporaires
           (sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (sr (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ratio (* sr 0.001))
           (start-smp (floor (* start-time sr-ratio)))
           (end-smp (ceiling (* end-time sr-ratio)))
           (size (- end-smp start-smp))
           (channels (fli::dereference (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) :type :int :index #+powerpc 1 #-powerpc 0))
           (window (/ size nsmp-out 1.0))
           (window-adaptive (round window))
           ;;;Variables calcul de waveform
           (buffer-size (* (ceiling window) channels))
           (buffer (fli::allocate-foreign-object :type :float :nelems buffer-size))
           (MaxArray (make-array (list channels (min nsmp-out size)) :element-type 'single-float :initial-element 0.0))   ;Tableau pour stocker les max
           (indxmax (1- (min nsmp-out size)))
           (frames-read 0)
           (frames-count 0)
           (winsum 0)
           maxi throw-buffer)
      (when (> start-smp 0)
        (setq throw-buffer (fli::allocate-foreign-object :type :float :nelems (* start-smp channels)))
        (sf::sf-readf-float sndfile-handle throw-buffer start-smp)
        (fli:free-foreign-object throw-buffer))
      (if (> size nsmp-out)
          (loop for indx from 0 do
                (setq winsum (+ winsum window-adaptive))
                (if (> indx 0) (setq window-adaptive (- (round (* (+ 2 indx) window)) (round winsum))))
                (setq frames-read (sf::sf-readf-float sndfile-handle buffer window-adaptive)
                      frames-count (+ frames-count frames-read))
                (dotimes (n channels)
                  (dotimes (i window-adaptive)
                    (setq maxi (max (abs (fli:dereference buffer :type :float :index (+ n (* channels i)))) (or maxi 0.0))))
                  (setf (aref MaxArray n (min indx indxmax)) maxi)
                  (setq maxi 0.0))
                while (and (< frames-count size) (= frames-read window-adaptive)))
        (loop for indx from 0 do
              (setq window-adaptive (max window-adaptive 1)
                    frames-read (sf::sf-readf-float sndfile-handle buffer window-adaptive)
                    frames-count (+ frames-count frames-read))
              (dotimes (n channels)
                (setf (aref MaxArray n (min indx indxmax)) (fli:dereference buffer :type :float :index n)))
              while (and (< frames-count size) (= frames-read window-adaptive))))
      (fli:free-foreign-object buffer)
      (sf::sf_close sndfile-handle)
      MaxArray)))


(defmethod sound-get-display-array-slice ((self sound) nbpix start-time end-time)
  (when (display-array self)
    (let* ((sr (* (om-sound-sample-rate self) 0.001))
           (maxtime (round (om-sound-n-samples self) sr))
           (targettime (- end-time start-time))
           (timeratio (float (/ targettime maxtime 1.0)))
           (win (display-ratio self))
           (maxnbpix (round (* timeratio (cadr (array-dimensions (display-array self))))))
           ;(start-smp (floor (* start-time sr)))
           ;(end-smp (ceiling (* end-time sr)))
           (start (floor (* start-time sr) win))
           (end (ceiling (* end-time sr) win))
           (stoppoint (1- (cadr (array-dimensions (display-array self)))));(+ start (1- maxnbpix)))
           (maxi 0.0)
           result)
      (cond ((= nbpix maxnbpix)
             (setq result (display-array self)))
            ((< nbpix maxnbpix)
             (let* ((step (/ (- end start) nbpix 1.0)))
               (setq result (make-array (list (om-sound-n-channels self) nbpix) :element-type 'single-float :initial-element 0.0))
               (dotimes (c (om-sound-n-channels self))
                 (dotimes (i nbpix)
                   (dotimes (j (round step))
                     (setq maxi (max maxi (aref (display-array self) c (min stoppoint (round (+ start j (* i step))))))))
                   (setf (aref result c i) maxi)
                   (setq maxi 0.0)))
               result))
            ((> nbpix maxnbpix)
             (setq result (om-get-sound-display-array-slice (namestring (filename self)) nbpix start-time end-time))))
      (values result (< (cadr (array-dimensions result)) nbpix)))))




|#

