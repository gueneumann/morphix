
;; Software copyrights: Guenter Neumann, Donnerstag, 24. Oktober 1991

;;  FILE: dtree.lisp
;; contains: functions to hold decision tree
;; toplevel-functions to be called elsewhere are: 

;; insert-entry, get-entry, delete-entry, entry-exists-p


(in-package "COMMON-LISP-USER")

(defpackage "DTREE"
  (:use "COMMON-LISP"
        )
  (:export 
           :insert-entry 
           :update-dtree-entry 
           :get-entry
           :delete-entry
           :entry-exists-p
           :get-suffix-dtree
           :get-leafs
           :get-substrings
           :print-res))