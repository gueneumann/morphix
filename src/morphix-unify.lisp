;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: morphix-unify.lisp
;;;      module: morphix
;;;     version: 1.0
;;;  written by: GN
;;; last update: 3/6/97
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MORPHIX")

;;; morphix-unifiier
;;; based on stuff in file morphix-to-fset.lisp

;;; unification will be performed on DNF of morphix-results 
;;; for each version of possible DNF normalizers I have to define
;;; separate unifiers (as long as no compilation into internal format
;;; is considered)
;;;
;;; thus the current unifiers are first TESTS !

;;; ********************************************************************************
;;;
;;;                               UNIFIER FOR FEATURE VECTORS AS COMPUTED
;;;                               BY (setf (get 'mapper  :normalizer) #'feature-vectors)


;;; UTILITIES:

(defun get-fset (word)
  (second (car (outer-rt word))))

#|

- input are: two list of terms, s1, s2
- in general |s1| =/ |s2|
- each term t: same order and equal length
- values: symbol or variable
- variable match either value
- symbols must be equal
- unifier: for each term in s1 and s2 collect matching terms

- some benchmarks (GN 4/6/97)

(setq s1 '(((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :S) (:CASE . :N)) 
           ((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :S) (:CASE . :A))
           ((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :P) (:CASE . :N)) 
           ((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :P) (:CASE . :A))))

(setq s2 '(((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :S) (:CASE . :N)) 
           ((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :S) (:CASE . :G))
           ((:TENSE . :NO) (:FORM . :NO) (:NUMBER . :S) (:CASE . :D)))
      
using (loop for pair in vec1 ... ):

(time (dotimes (x 100000) (unify-feature-sets  s2 s1)))
; cpu time (non-gc) 5,570 msec user, 0 msec system
; cpu time (gc)     170 msec user, 0 msec system
; cpu time (total)  5,740 msec user, 0 msec system
; real time  5,826 msec
; space allocation:
;  5,300,004 cons cells, 0 symbols, 32 other bytes
NIL

using (loop for i from 0 to (1- *feature-length*):
                            
(time (dotimes (x 100000) (unify-feature-sets  s2 s1)))
; cpu time (non-gc) 4,860 msec user, 0 msec system
; cpu time (gc)     120 msec user, 0 msec system
; cpu time (total)  4,980 msec user, 0 msec system
; real time  5,024 msec
; space allocation:
;  4,100,004 cons cells, 0 symbols, 32 other bytes
NIL

using (setq s2 (delete term2 s2 :test #'equal)) inside unify-feature-sets:

(time (dotimes (x 100000) (unify-feature-sets  s2 s1)))
; cpu time (non-gc) 6,900 msec user, 0 msec system
; cpu time (gc)     150 msec user, 0 msec system
; cpu time (total)  7,050 msec user, 0 msec system
; real time  7,129 msec
; space allocation:
;  3,500,004 cons cells, 0 symbols, 32 other bytes
NIL
|#

(defun unify-feature-vector (vec1 vec2 &aux res)
  ;;(format T "~%~a" vec1)
  ;;(format T "~%~a~%" vec2)
  
  ;; since terms are ordered vectors of equal length where
  ;; the length depends on the length of
  ;; *relevant-features*
  ;; a simple integer loop is required
  
  (loop for i from 0 to (1- *feature-length*)
                        ;; select ith elements from vec1 and vec2
                        
      for pair1 = (nth i vec1)
      for pair2 = (nth i vec2)
      do
        (cond
         ;; if pair1 has value *DEFAULT-FEAT-VAL* then simply choose val(pairt2)
         ((eq (rest pair1) *DEFAULT-FEAT-VAL*)
          (push pair2 res))
         ;; the same for pair2
         ((eq (rest pair2) *DEFAULT-FEAT-VAL*)
          (push pair1 res))
         ;; both values are identical
         ((eq (rest pair1) (rest pair2))
          (push pair1 res))
         ;; else a mismatch has been found, so stop immediatly
         ( T (return NIL)))
      finally (return (nreverse res))))

(defun unify-feature-sets (s1 s2 &aux res)
  ;; this is a simple n*m loop because s1 and s2 are disjuncts
  ;; so that each element of s1 has to be checked with each element
  ;; of s2;
  ;; have to check how to make this more efficient, maybe
  ;; by sorting s1 and s2 according to most feature instantiated
  (cond ((null s1) s2)
        ((null s2) s1)
        
        (T (loop for term1 in s1 do
                 (loop for term2 in s2 
                     for term-res = (unify-feature-vector term1 term2)
                     when term-res do
                       ;; check whether this is true
                       ;; seems to be slower but to save space
                       ;;(setq s2 (delete term2 s2 :test #'equal))
                       (setf res (push term-res res))
                     ))
           res)))

