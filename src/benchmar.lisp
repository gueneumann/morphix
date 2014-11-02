;;; -*- Syntax: Common-lisp; Base: 10; Mode: LISP; Package: (:morphix :use :scl :colon-mode :external); Default-character-style: (:FIX :ROMAN :NORMAL); -*-
(in-package :morphix)


(defvar *textpath* nil "name of file to analyze with (benchtext)")

#||
; ------------------------------------------------------------------------------

1. Call the function MORPHIX-BENCHMARK
   This will perform the analysis of the 8 sentences below (by 50 repetitions)
   and give an average cpu-time

2. Call the function BENCHTEXT
   This will time the morphological analysis of a whole file.
   The filename must be specified as value of the global variable
   morphix::*textpath*
   The result is as follows:
    a. time to read 200 lines in
    b. time to morphix-read these lines (tokenization of morphix FSA)
    c. time to analyze the resulting words

   Note: Each word is analyzed only one time. A Hasharray is filled with
	 each word as key and its morphological analysis or t - in the
         case of no morphological result - is entered as the value.
	 The first step during analysis of a word is a lookup in this hasharray.
||#

;;--------------------------D A T A --------------------------------------------

;(l-s "1984" 1900000)

(defvar *sentences*
 (make-array
   8 :initial-contents
   '(("wer" "hat" "seinen" "lohnsteuerjahresausgleich" "noch" "nicht" "gemacht" "?")
     ("ich" "mache" "meine" "einkommensteuererklaerung" "jedes" "jahr" "!")
     ("wer" "ist" "der" "geeignetste" "mann" "fuer" "diese" "aufgabe" "?")
     ("die" "kinder" "spielen" "auf" "der" "strasse" ".")
     ("eine" "bessere" "anleitung" "zum" "ausfuellen" "der" "formulare"
	"wurde" "noch" "nicht" "veroeffentlicht" ".")
     ("dieser" "ratgeber" "ist" "fuer" "sie" "ein" "zuverlaessiger" "helfer"
	"beim" "erstellen" "ihrer" "einkommensteuererklaerung" "fuer"
	"das" "kalenderjahr" "1984" ".")
     ("sein" "uebersichtlicher" "aufbau" "und" "zahlreiche" "fallbeispiele"
	"gewaehrleisten" "ihnen" "ein" "schnelles" "zurechtfinden" "in"
	"allen" "einkommensteuerlichen" "fragen" ".")
     ("in" "einem" "gesonderten" "kapitel" "werden" "anleitungen" "zum"
	"ausfuellen" "der" "erklaerungsformulare" "gegeben" "."))))

;; end-of-data ==================================================

(defun morphix-benchmark ()
  (reset-results)
  (setq *property-retrieval* nil)
  (let ((average 0)
	(result nil))
    (dotimes (n 8)
      (let ((sentence (aref *sentences* n)))
	(format t "~% CPU SECONDS per word in the ~d. sentence: ~d"
	      (1+ n)
	      (setq average (/ (average (time-of-sentence-in-time-units
					  sentence))
			       (length sentence))))
	(push average result)))
    (format t "~2% CPU-SECONDS PER WORD: ~d" (/ (apply #'+ result) 8.0))))

(defun time-of-sentence-in-time-units (sentence)
  (let ((times nil))
    (dotimes (n 50) n
      (push (morphix-time sentence) times))
      times))

(defun average (time-list)
  (/ (apply #'+ (nbutlast (nthcdr 20 (sort time-list #'<)) 20))
     10.0 internal-time-units-per-second))

(defun morphix-time (sentence)
  (let* ((t1 (get-internal-run-time))
	 (tmp (morphix sentence))
	 (t2 (get-internal-run-time)))
    tmp
    (- t2 t1)))

(defmacro xtime (n form)
   `(funcall
     (compile nil
	      '(lambda ()
		 (time (dotimes (i ,n) ,form))))))

; ---------------------------------------------------------------------------
;;; BENCHMARK TO ANALYZE A WHOLE FILE!


(defvar *lines-to-analyse* nil)
(defvar *number-of-unknown-words* 0)
(defvar *lines-to-read-in* 500)
(defvar *lines-read-counter* 0)
(defvar *complete* nil)
(defvar *read-time* 0)
(defvar *lines-read* 0)
(defvar *morphix-reader* 0)
(defvar *no-of-words* 0)
(defvar *analysis-time* 0)

#+:SYMBOLICS (setq sys:inhibit-fdefine-warnings t)

(defun non-result-handler (word &optional (fail-val "nicht analysiert"))
  (incf *number-of-unknown-words*)
  (cond (*clarification-dialog-on* (dialog-for word))
	(t `((,word ,fail-val)))))

#+:SYMBOLICS (setq sys:inhibit-fdefine-warnings nil)

(defmacro take-time (form)
  `(let* ((t1 (get-internal-run-time))
	  (tmp ,form)
	  (t2 (get-internal-run-time)))
     tmp
     (/ (- t2 t1) 1.0 internal-time-units-per-second)))

(defun benchtext ()
   (setf *number-of-unknown-words* 0
	 *whole-input* nil
	 *property-retrieval* t
	 *clarification-dialog-on* nil
	 *complete* nil
	 *read-time* 0
	 *lines-read* 0
	 *morphix-reader*  0
	 *no-of-words* 0
	 *analysis-time* 0)
   (reset-results)
   (cond (#+SYMBOLICS(fs:probef *textpath*) #-SYMBOLICS t
	  (with-open-file (stream1 *textpath* :direction :input)
	    (do () (*complete*
		     (setf *whole-input* (nreverse *whole-input*))
		     (format t
			"~4% T O T A L: ~
                     ~% READ DATA FROM FILE: ~d seconds (internal-run-time)~
                              ~% MORPHIX READER:      ~d seconds~
                              ~% MORPHIX ANALYSIS:    ~d seconds~
                              ~% number of words:  ~d ; unknown-words: ~d~
                              ~2% pure analysis time per word in msec: ~d
                              ~% complete time per word in msec: ~d~
                              ~%-------------------------------------
                              ~2% total time: ~D seconds~
                              ~% speed: ~D    words per second"
			     *read-time* *morphix-reader* *analysis-time*
			     *no-of-words* *number-of-unknown-words*
			     (/ *analysis-time* *no-of-words* 0.001)
			     (/ (+ *analysis-time*
				   *morphix-reader*
				   *read-time*)
				*no-of-words* 0.001)
                             (+ *analysis-time*
				   *morphix-reader*
				   *read-time*)
                             (/ *no-of-words* (+ *analysis-time*
				   *morphix-reader*
				   *read-time*))
                             )
		     (return t))
	      (let* ((read-time (take-time (get-data-from-file stream1)))
		     (lines-read *lines-read-counter*)
		     (morphix-read-time (take-time (call-morphix-read)))
		     (hyphen-time (take-time (simple-hyphen--1)))
		     (no-of-words (get-number-of-words))
		     (analysis-time
		       (take-time (mapc #'morphix *lines-to-analyse*))))
		(push *lines-to-analyse* *whole-input*)
		(format t
		    "~%~4$ s to read ~5d lines; morphix-read: ~4$ s; analyzed ~5d words in ~4$ s; unknown: ~d"
		    read-time lines-read (+ morphix-read-time hyphen-time)  no-of-words
		    analysis-time *number-of-unknown-words*)
		(setf *read-time* (+ *read-time* read-time)
		      *lines-read* (+ *lines-read* lines-read)
		      *morphix-reader* (+ *morphix-reader* morphix-read-time hyphen-time)
		      *no-of-words* (+ *no-of-words* no-of-words)
		      *analysis-time*
		      (+ *analysis-time* analysis-time))))))
	 (t (format t "PLEASE SET THE VAR *textpath* to a pathname"))))


(defun get-data-from-file (stream)
  (setq *lines-to-analyse* nil)
  (do ((line (read-line stream nil 'end) (read-line stream nil 'end))
       (lines-read 1 (1+ lines-read)))
      ((or (eql line 'end) (>= lines-read *lines-to-read-in*))
       (setq *lines-read-counter* lines-read)
       (cond ((eql line 'end) (setf *lines-to-analyse*
				    (nreverse *lines-to-analyse*)
				    *complete* t))
	     (t (setf *lines-to-analyse*
		      (nreverse (push line *lines-to-analyse*))))))
    (push line *lines-to-analyse*)))

(defun call-morphix-read ()
  (setq *lines-to-analyse*
	(mapcar #'morphix-read *lines-to-analyse*)))

(defun simple-hyphen--1 ()
  (do* ((rest *lines-to-analyse* (cdr rest))
	(end-line (car rest) (car rest))
	(last-of-end nil)
	(beg-line (cadr rest) (cadr rest)))
       ((null rest) *lines-to-analyse*)
    (cond ((and (stringp (setq last-of-end (car (last end-line))))
		(eql #\- (aref last-of-end (1- (length last-of-end))))
		(stringp (car beg-line)))
	   (rplaca (last end-line) 
		   (low-concat last-of-end
			       (car beg-line)
			       0
			       (- (length last-of-end) 1)
			       0 (length (car beg-line))))
	   (setf (cadr rest) (cdadr rest))))))
    
(defun get-number-of-words ()
	(do ((rest *lines-to-analyse* (cdr rest))
	     (no 0))
	    ((null rest) no)
	  (setq no (+ no (length (car rest))))))



(defun show-results ()
  (dolist (lines *whole-input*)
    (dolist (line lines)
      (dolist (elem line)
	(cond ((stringp elem)
	       (format t "~%~S: ~S" elem (gethash elem %morphix-results%)))
	      (t (format t "~%   ~S" elem)))))))

(defun morphix-go (in-file)
  (setq *textpath* in-file)
  (benchtext))
