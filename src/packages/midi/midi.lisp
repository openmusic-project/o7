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

(defpackage :om-midi)

(compile&load (merge-pathnames "midi-api/midi-api" *load-pathname*))
(compile&load (merge-pathnames "midi-setup" *load-pathname*))
(compile&load (merge-pathnames "tools/midi-values" *load-pathname*))
(compile&load (merge-pathnames "tools/midi-import-export" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-event" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-track" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-controllers" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-mix" *load-pathname*))
(compile&load (merge-pathnames "tools/midi-extract" *load-pathname*))
(compile&load (merge-pathnames "tools/midi-out" *load-pathname*))
(compile&load (merge-pathnames "tools/midi-in" *load-pathname*))
(compile&load (merge-pathnames "compatibility" *load-pathname*))


(omNG-make-package
 "MIDI"
 :container-pack *om-package-tree*
 :doc "MIDI tools and objects"
 :classes '(midi-track midi-note midievent midi-mix-console)
 :functions nil
 :subpackages
 (list (omNG-make-package
        "Import/Export/Conversions"
        :doc "MIDI import and conversion utilities"
        :functions '(import-midi get-midievents mf-info save-as-midi))
       (omNG-make-package
        "Filters"
        :doc "Tools to filter/process"
        :functions '(test-midi-type test-date test-midi-track test-midi-channel test-midi-port))
       (omNG-make-package
        "Utils"
        :doc "Other MIDI utilities"
        :functions '(midi-type midi-control-change gm-program gm-drumnote mc-to-pitchwheel))
       (omNG-make-package
        "Out"
        :doc "Send MIDI events out"
        :functions '(pgmout pitchbend pitchwheel ctrlchg volume midi-reset send-midi-note))
       (omNG-make-package
        "In"
        :doc "REceive MIDI events"
        :functions '(midi-in))
       ))
