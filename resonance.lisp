;;;;------------------------------------------------------------------------
;;;;Resonance Calculator in Lisp
;;;;------------------------------------------------------------------------
;;;A set of functions for calculating resonance in violin intonation

;;;Goals: lilypond extension (color-coded resonant notes)
	 ;incorporate into realtime tuner
	 ;indicate whether the note should be played high or low
                  ;to accomodate optimal resonance.
	 ;return list of note names with string name instead of
		  ;just frequencies

;;;------------------------------------------------------------------------
;;;Current highest-level functionality:

(open-sympathizer 220 violin-open-strings)
;;;;output: the sympathetic vibrating notes on each string in relation
;;;to the given frequency.

(how-sympathetic 220) ;for the number of shared harmonics with open strings

;;;------------------------------------------------------------------------
;;;Open string frequencies- using standard tuning but can be altered
(defconstant open-strings '(196 293.66 440 659.25))

(defconstant violin-open-strings '((G 196)
		       (D 293.66)
		       (A 440)
		       (E 659.25)))

;;;------------------------------------------------------------------------
;;;Overtone functions

(defun overtone-ladder (fundamental gap n)
  (cond ((< n 1) nil)
	(t (cons (+ fundamental gap)
		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

(defun overtones (fundamental)
  (cons fundamental (overtone-ladder fundamental fundamental 15)))


(defun open-string-set (open-string-list)
  (cond ((null open-string-list) nil)
	(t (cons (overtones (first open-string-list))
		 (open-string-set (rest open-string-list))))))

(defvar open-string-overtones (open-string-set open-strings))

;;;;------------------------------------------------------------------------
;;;Resonance functions

(defun resonance-compare (note open-string)
  (loop for overtone in (overtones note)
	do (loop for ovtone in (overtones open-string)
		 if (< (abs (- overtone  ovtone)) 10)
		   collect ovtone into common-overtones
		 finally (return common-overtones))))

(defun resonance-check (pitch string-overtones)
  (loop for overtone in string-overtones
	if (< (abs (- overtone pitch)) 10)
	  collect overtone into common-overtones
	finally (return common-overtones)))

(defun resonance-compare (note-overtones open-string)
  (cond ((null note-overtones) nil)
	((resonance-check (first note-overtones)
			  (overtones open-string))
	 (cons (resonance-check (first note-overtones)
				  (overtones open-string))
	       (resonance-compare (rest note-overtones) open-string)))
	(t (resonance-compare (rest note-overtones) open-string))))
	
;;;------------------------------------------------------------------------
;;;Sympathetic Vibration functions:

(defun open-sympathizer (note string-list)
  (cond ((null string-list) nil)
	(t (cons (cons (first (first string-list))
		       (resonance-compare (overtones note)
					  (second (first string-list))))
		 (open-sympathizer note (rest string-list))))))

(defun sympathy-calculator (note string-list)
  (cond ((null string-list) nil)
	(t (append (resonance-compare (overtones note)
				    (first string-list))
		 (sympathy-calculator note (rest string-list))))))

(defun how-sympathetic (note)
  (length (sympathy-calculator note open-strings)))

;;;------------------------------------------------------------------------
;;;------------------------------------------------------------------------
