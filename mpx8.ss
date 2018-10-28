;; -*- Gerbil -*-
package: mpx8
namespace: mpx8
(export main)

(declare (not optimize-dead-definitions))
(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
  :std/format
  :std/iter
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  )

(def program-name "mpx8")

(def *midi-input* #f)
(def *input-buffer* [])
(def file-data [])

(def file-header
  (u8vector
   #x4B #x49 #x54 #x48
   #x80 #x00 #x00 #x00
   #xE4 #x00 #x08 #x08
   #x00 #x20 #x00 #x20
   #x00 #x00 #x00 #x03
   #x00 #x10 #x00 #x20
   #x00 #x00 #x00 #x00
   #x00 #x00 #x0C #x18
   #x00 #x00 #x0C #x18
   #x00 #x00 #x0C #x18
   #x00 #x00 #x0C #x18
   #x00 #x00 #x0C #x18
   #x00 #x00 #x0C #x18
   #x00 #x00 #x0C #x18
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00))

(def *running-status* #x0)
(def *status* #x0)

;; # Kit Header
;; # 50 bytes

;; # 4B 49 54 30 // Kit header
;; # 30 33 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00

;; # Name of the Kit
;; # Eg: KIT001
(def kit-header
  (u8vector
   #x4B #x49 #x54 #x30
   #x30 #x31 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00))

;; # Pad internal
;; # 88 bytes

;; # 4B 49 54 49 // Sample
;; # 50 00 00 00 // Holder
;; # 00 00 00 00 // Sample location (internal)
;; # 00 00 00 08 // Name length
;; # 38 30 38 20 // Name
;; # 4B 69 63 6B
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 15 // Holder
;; # 00 06 00 00 // Holder
;; # 00 01 00 20 // Level     00 XX 00 20 (00 to 20)
;; # 01 05 08 10 // Tune      01 XX 08 10 (00 to 08)
;; # 02 04 08 10 // Panning   02 XX 08 10 (00 to 08)
;; # 03 01 00 0A // Reverb    03 XX 00 20 (00 to 0A)
;; # 08 01 00 7F // Midi      08 XX 00 7F (00 to 7F)
;; # 09 00 00 03 // Trigger   09 XX 00 03 (00 to 03)
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00


(def pad-internal
  (u8vector
   #x4B #x49 #x54 #x49
   #x50 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x08
   #x38 #x30 #x38 #x20
   #x4B #x69 #x63 #x6B
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x15
   #x00 #x06 #x00 #x00
   #x00 #x05 #x00 #x20
   #x01 #x04 #x08 #x10
   #x02 #x04 #x08 #x10
   #x03 #x00 #x00 #x20
   #x08 #x00 #x00 #x7F
   #x09 #x00 #x00 #x03
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00))

;; # Pad card
;; # 88 bytes

;; # 4B 49 54 49 // Sample
;; # 50 00 00 00 // Holder
;; # FF AA 00 00 // Sample location (card)
;; # 00 00 00 08 // Name length
;; # 3C 45 6D 70 // Name
;; # 74 79 3E 20
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 01 00 //Holder card
;; # 00 06 00 00 // Holder
;; # 00 01 00 20 // Level     00 XX 00 20 (00 to 0A)
;; # 01 05 08 10 // Tune      01 XX 08 10 (00 to 08)
;; # 02 04 08 10 // Panning   02 XX 08 10 (00 to 08)
;; # 03 01 00 20 // Reverb    03 XX 00 20 (00 to 0A)
;; # 08 01 00 7F // Midi      08 XX 00 7F (00 to 7F)
;; # 09 00 00 03 // Trigger   09 XX 00 03 (00 to 03)
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00
;; # 00 00 00 00

(def pad-card
  (u8vector
   #x4B #x49 #x54 #x49
   #x50 #x00 #x00 #x00
   #xFF #xAA #x00 #x00
   #x00 #x00 #x00 #x08
   #x3C #x45 #x6D #x70
   #x74 #x79 #x3E #x20
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x01 #x00
   #x00 #x06 #x00 #x00
   #x00 #x05 #x00 #x20
   #x01 #x04 #x00 #x10
   #x02 #x04 #x00 #x10
   #x03 #x00 #x00 #x20
   #x08 #x00 #x00 #x7F
   #x09 #x00 #x00 #x03
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00))

;; # Internal samples
;; # 001: 808 Kick
;; # 002: 808 SNR
;; # 003: Hat
;; # 004: Clap
;; # 005: Pluck8Bt
;; # 006: Bass1 C2
;; # 007: UH
;; # 008: Yea
;; # 009: 909 Kick
;; # 010: S-Ressy
;; # 011: 808 HH O
;; # 012: 808 HH C
;; # 013: 808 Tom
;; # 014: SynthStb
;; # 015: Trans FX
;; # 016: Gunshot
;; # 017: K-Harder
;; # 018: Rimshot
;; # 019: Airhorn
;; # 021: ABass C2
;; # 022: NoSample

(def samples-internal
  [ "808 Kick"
    "808 SNR"
    "Hat"
    "Clap"
    "Pluck8Bt"
    "Bass1 C2"
    "UH"
    "Yea"
    "909 Kick"
    "S-Ressy"
    "808 HH O"
    "808 HH C"
    "808 Tom"
    "SynthStb"
    "Trans FX"
    "Gunshot"
    "K-Harder"
    "Rimshot"
    "Airhorn"
    "ABass C2"
    "NoSample" ])

(def internal-pads [ pad-internal pad-internal pad-internal pad-internal pad-internal pad-internal pad-internal pad-internal ])

(def internal-data-segment [ pad-internal pad-internal pad-internal pad-internal pad-internal pad-internal pad-internal pad-internal ]) ;;[ internalPads[0] internalPads[1] internalPads[2] internalPads[3] internalPads[4] internalPads[5] internalPads[6] internalPads[7] ])

(def card-pads [ pad-card pad-card pad-card pad-card pad-card pad-card pad-card pad-card ])

;;kitFile = fileHeader+kitHeader+internalDataSegment+cardPads

(def kit-parts [ file-header kit-header
			     pad-internal
			     pad-internal
			     pad-internal
			     pad-internal
			     pad-internal
			     pad-internal
			     pad-internal
			     pad-internal
			     pad-card
			     pad-card
			     pad-card
			     pad-card
			     pad-card
			     pad-card
			     pad-card
			     pad-card ])

;; fileData = ''.join(str(e) for e in kitFile)
;; print "File data created"

;; myfile = open("KIT001.KIT", "w")
;; print "Writing file"
;; myfile.write(fileData.decode("hex"))
;; myfile.close()
;; print "File ready"

;; append-u8vectors
;; open file, write bytes with write-subu8vector, close file

;; write-u8vector
;; (def (write-u8vector bytes (port (current-output-port)))
;;   (write-subu8vector bytes 0 (u8vector-length bytes) port))

(def (create kitfile)
  (let* ((path (format "~a.kit" kitfile))
	 (kit-file (open-file '(path: "./test" create: maybe truncate: #t))))
    (for-each
      (lambda (x)
	(write-u8vector x kit-file))
      kit-parts)
    (force-output kit-file)
    (close-port kit-file)
    (displayln "done")))

;;(with-output-to-file "./KIT01.me"
;; (for-each
;;   (lambda (x)
;;     (write-u8vector x (open-file "./KIT01.me")))
;;   kit-file)

;; <Ober> then with-open-file sort of ting?
;; <Ober> thing?
;; <Ober> ok
;; <Ober> ok that makes sense. was not sure if anything special was required for
;;        writing out binary  [09:57]
;; <vyzo> nah
;; <vyzo> just write the bytes with write-subu8vector
;; <Ober> perfect
;; <vyzo> open file, write bytes with write-subu8vector, close file
;; <vyzo> and voilla, binary :)
;; <Ober> cool thanks
;; <Ober> have to attack midi next.
;; <vyzo> you don't have to append the vectors
;; <Ober> will submit the pull requests tomorrow for brew. had both tested and
;;        working fine today.  [09:58]
;; <vyzo> you can just do multiple write-subu8vectors
;; <vyzo> more efficient
;; <Ober> does it take multiple arguments?
;; <vyzo> it takes a few args
;; <vyzo> it's documented in the manual
;; <Ober> perfect. thank you  [09:59]
;; <vyzo> (write-subu8vector u8vector start end [port])
;; <Ober> I can keep appending to the same u8vector until the end then write
;;        it. perfect
;; <vyzo> also  [10:00]
;; <vyzo> we have a utility function
;; <vyzo> in :gerbil/gambit/ports
;; <vyzo> write-u8vector
;; <vyzo> which writes the whole vector
;; <vyzo> without having to pass the start end arguments
;; <Ober> fantastic
;; <vyzo> (def (write-u8vector bytes (port (current-output-port)))
;; <vyzo>   (write-subu8vector bytes 0 (u8vector-length bytes) port))
;; <vyzo> re: appending  [10:01]
;; <vyzo> it's more efficient to accumulate u8vectors in a list
;; <vyzo> with cons
;; <vyzo> and when you are done, reverse it and do (for-each write-u8vector
;;        (reverse vecs))
;; <Ober> reverse due to endianess? or lifo?  [10:02]
;; <vyzo> no, to write then in order  [10:03]
;; <Ober> ahh.
;; <vyzo> because you are stacking them with cons
;; <vyzo> the magic operator :)
;; <Ober> ah right

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
	 (verb (car args))
	 (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
	   (count (hash-get info count:)))
      (unless count
	(set! count 0))
      (unless (= (length args2) count)
	(usage-verb verb))
      (apply (eval (string->symbol (string-append "mpx8#" verb))) args2))))

(def interactives
  (hash
   ("create" (hash (description: "Create new kit") (usage: "create <kit name>") (count: 1)))
   ("read-midi-file" (hash (description: "Read Midi file") (usage: "read-midi-file <midi file>") (count: 1)))
   ))


(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln "Usage: mpx8 <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (read-midi-file filename)
  "read an entire Midifile from the file with name given as argument"
  (let ((offset 0))
    (call-with-input-file filename
      (lambda (p)
	(let* ((type (read-fixed-length-quantity 4 p))
	       (length (read-fixed-length-quantity 4 p))
	       (format (read-fixed-length-quantity 2 p))
	       (nb-tracks (read-fixed-length-quantity 2 p))
	       (division (read-fixed-length-quantity 2 p)))

	  (displayln "type: " type
		     " length: " length
		     " format: " format
		     " nb-tracks: " nb-tracks
		     " division: " division)

	  (for/collect ((track (in-range 1 nb-tracks)))
		       (read-track p))


	  )))))

(def (read-track p)
  (let ((type (read-fixed-length-quantity 4 p))
	(length (read-fixed-length-quantity 4 p)))
    (def message #f)
    (for/collect (t (in-range 1 length))
		 (set! message (read-timed-message p)))))

(def (read-variable-length-quantity p)
  "read a MIDI variable length quantity from *midi-input*"
  (let ((result 0)
	(byte (read-next-byte p)))
    (while (< byte #x80)
      (set! byte (read-next-byte p))
      (set! result (bitwise-ior (arithmetic-shift result 7) (bitwise-and byte #x7f))))
    result))

(def (read-timed-message p)
  "read a message preceded with a delta-time indication"
  (let ((delta-time (read-variable-length-quantity p))
	(status-or-data #f)
	(next (peek-u8 p)))
    (displayln "delta-time: " delta-time " status-or-data: " next)
    (if (>= next #x80)
      (begin
	(set! status-or-data (read-next-byte p))
	(when (<= *status* #xef)
	  (set! *running-status* *status*)))
      (begin
	(set! status-or-data #x0)))
    (let ((message (read-message p)))
      message)))

(def (read-message p)
  (when (>= *status* #x80)
    (cond
     ((between? *status* #x80 #x8f) (note-off-message))
     ((between? *status* #x90 #x9f) (note-on-message))
     ((between? *status* #xa0 #xaf) (polyphonic-key-pressure-message))
     ((between? *status* #xb0 #xbf) (control-change-message))
     ((between? *status* #xc0 #xcf) (program-change-message))
     ((between? *status* #xd0 #xdf) (channel-pressure-message))
     ((between? *status* #xe0 #xef) (pitch-bend-message))
     ((between? *status* #xb0 #xbf) (mode-message))
     ((between? *status* #xb0 #xbf) (all-notes-off-message))
     ((between? *status* #xb0 #xbf) (omni-mode-off-message))
     ((between? *status* #xb0 #xbf) (omni-mode-on-message))
     ((between? *status* #xb0 #xbf) (mono-mode-on-message))
     ((between? *status* #xb0 #xbf) (poly-mode-on-message))
     ((between? *status* #xf2 #xf2) (system-message))
     ((between? *status* #xf3 #xf3) (song-select-message))
     ((between? *status* #xf6 #xf6) (tune-request-message))
     ((between? *status* #xfa #xfa) (start-sequence-message))
     ((between? *status* #xfc #xfc) (stop-sequence-message))
     ((between? *status* #xf6 #xf6) (tune-request-message))
     ((between? *status* #xf0 #xf0) (system-exclusive-message))
     ((between? *status* #xf7 #xf7) (authorization-system-exclusive-message))
     ((between? *status* #xff #xff) (meta-message))
     ((between? *status* #x00 #x00) (sequence-number-message))
     ((between? *status* #x01 #x01) (general-text-message))
     ((between? *status* #x03 #x03) (sequence/track-name-message))
     ((between? *status* #x05 #x05) (lyric-message))
     ((between? *status* #x07 #x07) (cue-point-message))
     ((between? *status* #x09 #x09) (device-name-message))
     ((between? *status* #x21 #x21) (midi-port-message))
     ((between? *status* #x2f #x2f) (end-of-track-message))
     ((between? *status* #x51 #x51) (tempo-message))
     ((between? *status* #x54 #x54) (smpte-offset-message))
     ((between? *status* #x58 #x58) (time-signature-message))
     ((between? *status* #x59 #x59) (key-signature-message))
     ((between? *status* #x7f #x7f) (proprietary-event)))))



(def (note-off-message)
  (displayln "Got note-off-message"))

(def (note-on-message)
  (displayln "Got note-on-message"))

(def (polyphonic-key-pressure-message)
  (displayln "Got polyphonic-key-pressure-message"))

(def (control-change-message)
  (displayln "Got control-change-message"))

(def (program-change-message)
  (displayln "Got program-change-message"))

(def (channel-pressure-message)
  (displayln "Got channel-pressure-message"))

(def (pitch-bend-message)
  (displayln "Got pitch-bend-message"))

(def (mode-message)
  (displayln "Got mode-message"))

(def (all-notes-off-message)
  (displayln "Got all-notes-off-message"))

(def (omni-mode-off-message)
  (displayln "Got omni-mode-off-message"))

(def (omni-mode-on-message)
  (displayln "Got omni-mode-on-message"))

(def (mono-mode-on-message)
  (displayln "Got mono-mode-on-message"))

(def (poly-mode-on-message)
  (displayln "Got poly-mode-on-message"))

(def (system-message)
  (displayln "Got system-message"))

(def (song-select-message)
  (displayln "Got song-select-message"))

(def (tune-request-messag)
  (displayln "Got tune-request-messag"))

(def (start-sequence-message)
  (displayln "Got start-sequence-message"))

(def (stop-sequence-message)
  (displayln "Got stop-sequence-message"))

(def (tune-request-message)
  (displayln "Got tune-request-message"))

(def (system-exclusive-message)
  (displayln "Got system-exclusive-message"))

(def (authorization-system-exclusive-message)
  (displayln "Got authorization-system-exclusive-message"))

(def (meta-message)
  (displayln "Got meta-message"))

(def (sequence-number-message)
  (displayln "Got sequence-number-message"))

(def (general-text-message)
  (displayln "Got general-text-message"))

(def (sequence/track-name-message)
  (displayln "Got sequence/track-name-message"))

(def (lyric-message)
  (displayln "Got lyric-message"))

(def (cue-point-message)
  (displayln "Got cue-point-message"))

(def (device-name-message)
  (displayln "Got device-name-message"))

(def (midi-port-message)
  (displayln "Got midi-port-message"))

(def (end-of-track-message)
  (displayln "Got end-of-track-message"))

(def (tempo-message)
  (displayln "Got tempo-message"))

(def (smpte-offset-message)
  (displayln "Got smpte-offset-message"))

(def (time-signature-message)
  (displayln "Got time-signature-message"))

(def (key-signature-message)
  (displayln "Got key-signature-message"))

(def (proprietary-event)
  (displayln "Got proprietary-event"))


(def (between? v min max)
  (cond
   ((and (<= v max) (>= v min))
    #t)
   (else
    #f)))

(def (le32dec v)
  (bitwise-ior
   (u8vector-ref v 3)
   (arithmetic-shift (u8vector-ref v 1) 2)
   (arithmetic-shift (u8vector-ref v 2) 1)
   (arithmetic-shift (u8vector-ref v 3) 0)))

(def (le16dec v)
  (bitwise-ior
   (u8vector-ref v 1)
   (arithmetic-shift (u8vector-ref v 1) 0)))

(def (read-fixed-length-quantity nb-bytes p)
  "read an unsigned integer of nb-bytes bytes from port p"
  (unless (zero? nb-bytes)
    (let ((results 0))
      (for (byte (in-range 1 nb-bytes))
	   (set! results (bitwise-ior (arithmetic-shift results 8) (read-next-byte p))))
      results)))

(def (read-next-byte p)
  "read an unsigned 8-bit byte from *midi-input* checking for unread bytes"
  (read-u8 p))
