;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: morphix-reader.lisp
;;;      module: morphix
;;;     version: 3.0
;;;  written by: Gunter Neumann
;;; last update: 1.2.96
;;;  updated by: Gunter Neumann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MORPHIX")

; ---------------------------------------------------------------------------
;;; MORPHIX: ANALYSIS

;;; THE MORPHIX READER AUTOMATON
;;; used to convert an input string to a list of input words in lowercase

(defvar %special-chars% 
    '(#\. #\, #\; #\! #\? #\: #\( #\) #\{ #\} #\[ #\] #\$ #\' #\ ))

(defvar %delimeter-signs% '(#\- #\_))

(defvar %uml-char-codes% '(228 246 252 223 196 214 220))
;; ae oe ue ss Ae Oe Ue


(defun morphix-read (input-string &optional (split-string NIL))
  (flet ((word (s e umls)
           (let* ((sl (max 1 (- e s)))
		  (c nil)
		  (cc nil)
		  (offset 0)
		  (string (make-string (+ sl umls))))
             (cond ((zerop umls)
		    (dotimes (n sl string)
		      (setf (aref string n)
			    (char-downcase (aref input-string (+ s n))))))
		   (t (dotimes (n sl string)
			(setq c (aref input-string (+ s n)))
			(cond ((member (setq cc (char-code c))
				       %uml-char-codes%)
			       (case cc
				 ((228 196)
				  (setf (aref string (+ n offset)) #\a
					offset (1+ offset)
					(aref string (+ n offset)) #\e))
				 ((246 214)
				  (setf (aref string (+ n offset)) #\o
					offset (1+ offset)
					(aref string (+ n offset)) #\e))
				 ((252 220)
				  (setf (aref string (+ n offset)) #\u
					offset (1+ offset)
					(aref string (+ n offset)) #\e))
				 (223
				   (setf (aref string (+ n offset)) #\s
					offset (1+ offset)
					(aref string (+ n offset)) #\s))))
			      (t (setf (aref string (+ n offset))
				       (char-downcase c))))))))))
    (do ((il (length input-string))
	 (result nil)
	 (state 0)
	 (start 0)
	 (uml-chars 0)
         (delimiter-cnt 0)
	 (end 0 (1+ end))
	 (char nil))
	
	((> end il) (nreverse result))

      (setf char (if (eql end il) nil (char input-string end)))
    
      (case state                       ; 1 is most likely
        ;; one is the character state 
	(1 (cond ((member char '(nil #\Space #\Tab #\Newline))
		  (push (word start end uml-chars) result)
		  (setf state 0 uml-chars 0 start (1+ end)))
                 ;; GN: changed here on 27/1/00
                 ((and split-string
                       (member char  %delimeter-signs%))
                  (setf state 6) (incf delimiter-cnt))
		 ((member char %special-chars%)
		  (push	(word start end uml-chars) result)
		  (push (string char) result)
		  (setf state 0 uml-chars 0 start (1+ end)))
		 ((member (char-code char) %uml-char-codes%)
		  (incf uml-chars))))
        ;; state zero covers:
        ;; space, tab, specials
	(0 (cond ((member char '(nil #\Space #\Tab #\Newline)) (incf start))
		 ((member char %special-chars%) 
		  (push (word start end uml-chars) result)
		  (incf start))
		 ((digit-char-p char) (setf state 2))
		 ((member (char-code char) %uml-char-codes%)
		  (incf uml-chars) (setf state 1))
		 (t (setf state 1))))
        ;; state two:
        ;; integer part of digit
	(2 (cond ((member char '(nil #\Space #\Tab #\Newline))
		  (push	(convert-to-card (word start end uml-chars)) result)
		  (setf state 0 uml-chars 0 start (1+ end)))
		 ((eql char #\.) (setf state 4))
		 ((eql char #\,) (setf state 3))
		 ((member char %special-chars%)
		  (push	(convert-to-card (word start end uml-chars)) result)
		  (push (string char) result)
		  (setf state 0 uml-chars 0 start (1+ end)))
		 ((digit-char-p char))
		 ((member (char-code char) %uml-char-codes%)
		  (incf uml-chars) (setf state 1))
		 (t (setf state 1))))
        ;; state three: floating point designated by #\,
        
	(3 (cond ((member char '(nil #\Space #\Tab #\Newline))
		  (push	(convert-to-card (word start (1- end) uml-chars)) result)
		  (push "," result)
		  (setf state 0 uml-chars 0 start (1+ end)))
		 ((member char %special-chars%)
		  (push	(convert-to-card (word start (1- end) uml-chars)) result)
		  (push "," result)
		  (push (string char) result)
		  (setf state 0 uml-chars 0 start (1+ end)))
		 ((digit-char-p char) (setf state 5))
		 (t
		  (if (member (char-code char) %uml-char-codes%)
		      (incf uml-chars))
		  (push	(convert-to-card (word start (1- end) uml-chars)) result)
		  (push "," result)
		  (setf state 1 uml-chars 0 start end))))
        ;; state three: floating point designated by #\.

	(4  (cond  ((null char)
		   (push (convert-to-card-and-ord
			   (word start end uml-chars)) result)
		   (push "." result)
		   (setf state 0 uml-chars 0 start end))
		  ((member char '(#\Space #\Tab #\Newline))
		   (push (convert-to-ord (word start end uml-chars)) result)
		   (setf state 0 uml-chars 0 start (1+ end)))
		  ((member char %special-chars%)
		   (push (convert-to-ord (word start end uml-chars)) result)
		   (push (string char) result)
		   (setf state 0 uml-chars 0 start (1+ end)))
		  ((digit-char-p char) (setf state 5))
		  (t
		   (if (member (char-code char) %uml-char-codes%)
		      (incf uml-chars))
		   (push (convert-to-ord (word start end uml-chars)) result)
		   (setf state 1 start end))))
        
        ;; state five: digits
        
	(5   (cond ((member char '(nil #\Space #\Tab #\Newline))
		    (push (convert-to-card (word start end uml-chars)) result)
		    (setf state 0 uml-chars 0 start (1+ end)))
		   ((member char %special-chars%)
		    (push (convert-to-card (word start end uml-chars)) result)
		    (push (string char) result) 
		    (setf state 0 uml-chars 0 start (1+ end)))
		   ((digit-char-p char))
		   (t
		    (if (member (char-code char) %uml-char-codes%)
		      (incf uml-chars))
		    (push (convert-to-card (word start end uml-chars)) result)
		    (setf state 1 uml-chars 0 start end))))
        
        ;; state six: handle delimiters like #\-
        
        (6  (cond ((member char '(nil #\Space #\Tab #\Newline))
                   (push (word start (- end delimiter-cnt)
                               uml-chars) result)
                   (setf state 0 uml-chars 0 
                         delimiter-cnt 0 
                         start (1+ end)))
		    
                  ((member char  %delimeter-signs% :test #'char=)
                   (incf delimiter-cnt))
                  
                   ((member char %special-chars%)
                    (push (word start (- end delimiter-cnt)
                                uml-chars) result)
                    (push (string char) result)
                    (setf state 0 uml-chars 0 delimiter-cnt 0 start (1+ end)))
                   
		   ((digit-char-p char) (setf state 0))
                   
                   (t 
                      (push (word start (- end delimiter-cnt)
                                  uml-chars) result)
                      (setf state 1 
                            ;; GN: changed here on 27/1/00
                            uml-chars (if (member (char-code char) %uml-char-codes%)
                                          1 0)
                            delimiter-cnt 0 start end))
		   ))
        
        ))))

(defun convert-to-card (number-as-string)
  `((,number-as-string (wortart kardinalzahl))))

(defun convert-to-ord (ordinal-as-string)
  `((,ordinal-as-string (wortart ordinalzahl))))

(defun convert-to-card-and-ord (number-as-string)
  `((,(subseq number-as-string 0 (1- (length number-as-string)))
     (wortart kardinalzahl))
    (,number-as-string (wortart ordinalzahl))))



(defun insert-umlaut (string position)
  (let* ((int-pos (1- position))
         (char-code (char-code (char string (1- position)))))
    (case char-code
      (97 (setf (char string int-pos) (code-char 228))) ;; a to ae
      (111 (setf (char string int-pos) (code-char 246))) ;; o to oe
      (117 (setf (char string int-pos) (code-char 252))) ;; u to ue
      (115 string)
      (65 (setf (char string int-pos) (code-char 196))) ;; A to Ae
      (79 (setf (char string int-pos) (code-char 214))) ;; O to Oe
      (85 (setf (char string int-pos) (code-char 196)))) ;; U to Ue
    string))

(defun map-to-umlaut (word)
  (let* ((e-position (test-for-umlaut word)))
    (if e-position
        (insert-umlaut (low-concat word word
                                   0 e-position
                                   (1+ e-position) (length word))
                       e-position)
      word)))