#|19. April 2013:

Create NereidJava COCW Lexicon:

only load relevant lexica CCW, verbs

:cd /Users/gune00/dfki/src/Lisp/morphix/pd-vers/
|#

;;; select relevant lexica and load them with (load-mlex)
(in-package :cl-user)

;;; Only closed words
(setf *lex-file-name-list*
  '(
    "01defdet.lisp"
    "02infldet.lisp"
    "10prep.lisp"
    "13-14pron.lisp"
    "15whpron.lisp"
    "16punct.lisp"
    "17whadv.lisp"
    "19ord.lisp"
    "20schotter.lisp"
    "2verbaux.lisp"
    "2verbirr.lisp"
    "2verbmod.lisp"
    "2verbreg.lisp"
    "50vpref.lisp"
    "6adv.lisp"
    "7rest.lisp"
    "8coord.lisp"
    "9conj.lisp"
    "heegpref.lisp"
    "heegverb.lisp"
    ))

#|
;;; Only open words
(setf *lex-file-name-list*
  '(
     "10nounpl.lisp"
    "11nounmas.lisp"
    "12nounfem.lisp"
    "13nounneu.lisp"
    "14nounirrpl.lisp"
    "30adj.lisp"
    "31adj.lisp"
    "32adj.lisp"
    "33adjirr.lisp"
    "paradice.lisp"
    "wip.lisp"
   "adj.lisp"
    "noun1.lisp"
    "noun2.lisp"
    "noun3.lisp"
    "noun4.lisp"
    "noun5.lisp"
    ))

;;; all files
(setf *lex-file-name-list*
  '(
     "01defdet.lisp"
    "02infldet.lisp"
    "10prep.lisp"
    "13-14pron.lisp"
    "15whpron.lisp"
    "16punct.lisp"
    "17whadv.lisp"
    "19ord.lisp"
    "20schotter.lisp"
    "2verbaux.lisp"
    "2verbirr.lisp"
    "2verbmod.lisp"
    "2verbreg.lisp"
    "50vpref.lisp"
    "6adv.lisp"
    "7rest.lisp"
    "8coord.lisp"
    "9conj.lisp"
    "heegpref.lisp"
    "heegverb.lisp"
    "10nounpl.lisp"
    "11nounmas.lisp"
    "12nounfem.lisp"
    "13nounneu.lisp"
    "14nounirrpl.lisp"
    "30adj.lisp"
    "31adj.lisp"
    "32adj.lisp"
    "33adjirr.lisp"
    "paradice.lisp"
    "wip.lisp"
   "adj.lisp"
    "noun1.lisp"
    "noun2.lisp"
    "noun3.lisp"
    "noun4.lisp"
    "noun5.lisp"
    ))

|#

;;; change dir to :cd /Users/gune00/dfki/src/Lisp/morphix/fullform/

;;; load file make-ff-lexicon.lisp

(in-package :mo)

(defun init-nereid ()
  (setq morphix::*relevant-features* 
    '(:TENSE :CASE))
  (set-sym)
  )

(defun init-make-cocw ()
   (setq *reading-hash* (make-hash-table :test #'equal))
   (setq *val-key-map* NIL)
   (setq *reading-cnt* 0)
   (init-nereid)
   (setq *entry-cnt* 0)
   )

(defvar *entry-cnt* 0)

(defun make-nereid-lkb ()
  (init-make-cocw)

  (make-ccw-lex "/Users/gune00/dfki/src/Lisp/morphix/fullform/ccw-nereid-ff.txt")
  (make-ocw-lex "/Users/gune00/dfki/src/Lisp/morphix/fullform/ccw-nereid-sf.txt")
)

#|
Make XML output directly:

deiner: dein POSSPRON 1 2 3 ; deiner PERSPRON 3 ; 

->

First version only with POS and dummy prob
<COCW id="45" ln="0"><WF>af</WF><RS><R stem="xxx" pos="RG" freq="1" ln="0"/><R pos="SP" freq="1" ln="0"/></RS></COCW>

|#

;;; check whether wordform as SS in it then mark it

(defun contains-double-s (string)
  (let ((pos-first-s (position #\s string)))
    (when pos-first-s
      (when (< pos-first-s (1- (length string)))
	(char-equal #\s (aref  string (1+ pos-first-s)))))))

;;; GENERATES FILES FOR WORKING WITH NEMEXR AND NEREID IN JAVA
;;; works fine
;;; checks whether wordform as umlaut and/or double s, and if so, marks them
;;; currently I assume that I have to modify these entries manually
;;; ok, but keep mapping of manual changes, so that I can reuse them later for more complex lexicons
;;; or can use them in automatic way!!
;;; so better change them in generate files first ! before making the complete one
(defun morphix2dnf-interface (word morphix-result 
                              &optional (stream *terminal-io*))
  (let* ((new-reading (loop for x in morphix-result
                          for stem = (if (consp (tripple-stem x))
                                         (string-downcase
                                          (format NIL "~a~{~a~}"
                                                  (first (tripple-stem x)) 
                                                  (rest (tripple-stem x))))
                                       (tripple-stem x))
                          unless (member (tripple-pos x) 
                                         '(:no-res stop-word)
                                         :test #'eq)
                          collect
;; no readings so far
                            (format NIL "<R stem=\"~a\" pos=\"~a\" freq=\"1\" ln=\"0\"/>"
                                      stem 
                                      (tripple-pos x))
                              ))
	 (umlaut (test-for-umlaut word))
	 (umlaut-attr (when umlaut (format NIL "umlaut=\"T\"")))
	 (contains-double-s (contains-double-s word))
	 (umlaut-and-double-s (and umlaut-attr contains-double-s))
	 )

    (when new-reading
      (if umlaut-and-double-s
	  (format stream  "~%<COCW id=\"~a\" ln=\"0\" ~a doubles=\"T\"><WF>~a</WF><RS>~{~a~}</RS></COCW>" 
	       (incf *entry-cnt*)
	       umlaut-attr
	       word new-reading)
	  (if umlaut-attr
	      (format stream  "~%<COCW id=\"~a\" ln=\"0\" ~a><WF>~a</WF><RS>~{~a~}</RS></COCW>" 
		      (incf *entry-cnt*)
		      umlaut-attr
		      word new-reading)
	      (if contains-double-s
		  (format stream  "~%<COCW id=\"~a\" ln=\"0\" doubles=\"T\"><WF>~a</WF><RS>~{~a~}</RS></COCW>" 
			  (incf *entry-cnt*)
			  word new-reading)
		  (format stream  "~%<COCW id=\"~a\" ln=\"0\"><WF>~a</WF><RS>~{~a~}</RS></COCW>" 
			  (incf *entry-cnt*)
			  word new-reading)
		  ))))))
