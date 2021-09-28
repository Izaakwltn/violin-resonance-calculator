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

(defconstant violin-open-strings '((G-string 196)
		       (D-string 293.66)
		       (A-string 440)
		       (E-string 659.25)))

;;;------------------------------------------------------------------------
;;;Overtone functions

(defun overtone-ladder (fundamental gap n)
  (cond ((< n 1) nil)
	(t (cons (+ fundamental gap)
		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

(defun overtones (fundamental)
  (cons fundamental
	(overtone-ladder fundamental fundamental 15)))


(defun open-string-set (open-string-list)
  (cond ((null open-string-list) nil)
	(t (cons (overtones (first open-string-list))
		 (open-string-set (rest open-string-list))))))

(defvar open-string-overtones (open-string-set open-strings))

;;;;------------------------------------------------------------------------
;;;Resonance functions

;(defun resonance-compare (note open-string)
 ; (loop for overtone in (overtones note)
;	do (loop for ovtone in (overtones open-string)
;		 if (< (abs (- overtone  ovtone)) 10)
;		   collect ovtone into common-overtones
;		 finally (return common-overtones))))

(defun resonance-check (pitch string-overtones)
  (loop for overtone in string-overtones
	if (< (abs (- overtone pitch)) 10)
	  collect  overtone into common-overtones
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


;;;;Next steps- Given pitch as input, provide output in form

;given note in form (letter octave)
(A 3) ->
;returns:
;1. Sympathetic Rating
;2. A list of resonant frequencies on each string
;3. Ideally whether to play slightly sharper or slightly flatter

(defun resonance-calculator (note-name octave))

(defun note-to-freq (note-name octave);;;has to use quoted note-name
  (freq-finder (second (assoc note-name note-freq-table)) octave))

(defun freq-finder (note-freq octaves-up)
  (cond ((zerop octaves-up) note-freq)
	(t (freq-finder (* 2 note-freq) (- octaves-up 1)))))

(defun minimize-freq (freq)
  (cond ((< freq 31) freq)
	(t (minimize-freq (/ freq 2)))))

(defun freq-compare)

(defun freq-to-note (freq)
  (loop for i in note-freq-table
        collect
	;;;;find the minimum abs difference between freq and i
   
(defvar note-freq-table '((C . 16.35)
			  (C# . 17.32)
			  ;(Db . 17.32)
			  (D . 18.35)
			  (D# . 19.45)
			  ;(Eb . 19.45)
			  (E . 20.6)
			  (F . 21.83)
			  (F# . 23.12)
			  ;(Gb . 23.12)
			  (G . 24.5)
			  (G# . 25.96)
			  ;(Ab . 25.96)
(			  (A . 27.5)
			  (Bb . 29.14)
			  (B . 30.87)))

(defvar freq-key '())

(defun freq-to-note (freq)
  (setq freq-key '(c 20))
  (closest-note (minimize-freq freq) note-freq-table))

(defun closest-note (freq freq-list)
  (cond ((null freq-list) (first freq-key))
	((< (abs (- freq (second (first freq-list))))
	    (second freq-key))
	 (progn (setq freq-key
		      (list (first (first freq-list))
			    (abs (- freq (second (first freq-list))))))
		(closest-note freq (rest freq-list))))
	(t (closest-note freq (rest freq-list)))))


(defun keyed-overtone-ladder (fundamental gap n)
  (cond ((< n 1) nil)
	(t (cons (list (freq-to-note (+ fundamental gap))
		       (+ fundamental gap))
		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

(defun keyed-overtones (fundamental)
  (cons (list (freq-to-note fundamental) fundamental)
	(overtone-ladder fundamental fundamental 15)))
    
(defun keyed-resonance-check (pitch string-overtones)
  (loop for overtone in string-overtones
	if (< (abs (- overtone pitch)) 10)
	  do (return (list (freq-to-note overtone)
			overtone))))

(defun keyed-resonance-compare (note-overtones open-string)
  (cond ((null note-overtones) nil)
	((keyed-resonance-check (first note-overtones)
			  (overtones open-string))
	 (cons (keyed-resonance-check (first note-overtones)
				  (overtones open-string))
	       (keyed-resonance-compare (rest note-overtones) open-string)))
	(t (keyed-resonance-compare (rest note-overtones) open-string))))

(defun keyed-open-sympathizer (note string-list)
  (cond ((null string-list) nil)
	(t (cons (cons (first (first string-list))
		       (keyed-resonance-compare (overtones note)
					  (second (first string-list))))
		 (keyed-open-sympathizer note (rest string-list))))))

(defun keyed-sympathy-calculator (note string-list)
  (cond ((null string-list) nil)
	(t (append (list (first (first string-list))
			 (keyed-resonance-compare (overtones note)
			   (second (first string-list))))
		   (keyed-sympathy-calculator note (rest string-list))))))

(defun violin-resonance-calculator (note-name octave)
  (keyed-sympathy-calculator (note-to-freq note-name octave)
			     violin-open-strings))

(defun violin (note-name octave) ;(violin 'a 3)
  (keyed-sympathy-calculator (note-to-freq note-name octave)
			     violin-open-strings))
;;;;--------------------------------------
;;;;Web app

	  ;;Input: Note/Octave

;;;;output: Note/Octave
	   ;G String: ...
           ;D string: ...

;;;optimal

(defun optimal-resonant-note ())
;;;go through all notes g 3 -


;;;;scheme/lilypond extension that turns notes gray if they're resonant
;-should be pretty friendly 
