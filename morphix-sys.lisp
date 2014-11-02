;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: morphix-sys.lisp
;;;      module: morphix
;;;     version:
;;;  written by: Guenter Neumann
;;; last update: 12/12/97
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Very simple straight forward way of compiling and loading morphix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :cl-user)

#| to do:
one interface file
use stuff from oe to write docu

|#

;;; BEGIN USER UPDATE
;;; The morphix source path
;;; You have to CHANGE this

(defvar *morphix-src-path* NIL)
(setf *morphix-src-path* 
      #+:windows(pathname "D:/src/Lisp/morphix/pd-vers/src/*.lisp")
      #+:ccl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/src/*.lisp")
      #+:sbcl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/src/*.lisp")
      #+:abcl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/src/*.lisp")
)

;;; The morphix binary path
;;; You have to CHANGE this
(defvar *morphix-bin-path* NIL)
(setf *morphix-bin-path* 
      #+:windows(pathname "D:/src/Lisp/morphix/pd-vers/bin/*.fasl")
      #+:ccl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/bin/ccl/*.fasl")
      #+:sbcl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/bin/sbcl/*.fasl")
      #+:abcl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/bin/abcl/*.fasl")
      )

;;; The morphix lex path
;;; You have to CHANGE this
(defvar *lex-path* NIL)
(setf *lex-path* 
      #+:windows(pathname "D:/src/Lisp/morphix/pd-vers/lex/mlex/*.lisp")
      #+:ccl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/lex/mlex/*.lisp")
      #+:sbcl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/lex/mlex/*.lisp")
      #+:abcl(pathname "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/lex/mlex/*.lisp")
      )

;;; END USER UPDATE


(defvar *src-file-name-list* NIL)

(setf *src-file-name-list* 
  '(
    "dtree-package"
    "dtree"                             ; abstract datatype for letter tries
                                        ; (used for building up lexicon)
    "dtree-matcher"                     ; a regular matcher for tries
    
    "morphix-package"
    "globals"
    "morphix-reader"                    ; a simple tokenizer
    "ialtree"                           ; a classification scheme of all 
                                        ; German endings and their 
                                        ; corresponding potential
                                        ; morpho-syntactic information
    
    "main"                              ; contains all main functions for
                                        ; performing analysis and generation
    "composita"                         ; contains the code and knowledge 
                                        ; sources

    "morphix-gen"                       ; a generic function for generation
    "add-lex-fct"                       ; additional functions for building a
                                        ; structured lexicon
    
    
    
    "user-interface"
    
    
    "memoization"                       ; adaption of Norvig's code, I use it
                                        ; for on-line computation DNF
    "morphix-to-fset"                   ; a tool for constructing feature 
                                        ; vector representation
    "morphix-unify"                     ; a trivial but fast specialized 
                                        ; unifier for DNF feature vector
    
    "benchmar"                          ; functions for performing benchmarking
    
    "type-interface"
    ))





(defvar *lex-file-name-list* NIL)
(setf *lex-file-name-list*
  '(
    "01defdet.lisp"
    "02infldet.lisp"
    "10nounpl.lisp"
    "10prep.lisp"
    "11nounmas.lisp"
    "12nounfem.lisp"
    "13-14pron.lisp"
    "13nounneu.lisp"
    "14nounirrpl.lisp"
    "15whpron.lisp"
    "16punct.lisp"
    "17whadv.lisp"
    "19ord.lisp"
    "20schotter.lisp"
    "2verbaux.lisp"
    "2verbirr.lisp"
    "2verbmod.lisp"
    "2verbreg.lisp"
    "30adj.lisp"
    "31adj.lisp"
    "32adj.lisp"
    "33adjirr.lisp"
    "50vpref.lisp"
    "6adv.lisp"
    "7rest.lisp"
    "8coord.lisp"
    "9conj.lisp"
    "heegpref.lisp"
    "heegverb.lisp"
    "paradice.lisp"
    "wip.lisp"
    "adj.lisp"
    "noun1.lisp"
    "noun2.lisp"
    "noun3.lisp"
    "noun4.lisp"
    "noun5.lisp"
    ))

(defun compile-morphix ()
  (loop for file in  *src-file-name-list* do
        (compile-file (merge-pathnames (pathname file)
                                       *morphix-src-path*)
                      :output-file
                      (merge-pathnames (pathname file)
                                       *morphix-bin-path*))
        (load (merge-pathnames (pathname file)
                               *morphix-bin-path*))))

(defun load-morphix ()
  (loop for file in  *src-file-name-list* do
        (load (merge-pathnames (pathname file)
                               *morphix-bin-path*))))

(defun load-mlex (&optional (lex-list *lex-file-name-list*))
  (loop for file in lex-list do
        (load (merge-pathnames (pathname file)
                               *lex-path*))))


