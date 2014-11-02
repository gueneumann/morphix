;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: add-lex-fct.lisp
;;;      module: morphix
;;;     version: 4.0
;;;  written by: Guenter Neumann (Copyrights)
;;; last update: 27.1.96
;;;  updated by: Guenter Neumann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This files contains additional lexical functionality for supporting
;;;  the message extraction task. 
;;; The main idea is to support the representation of additional information
;;; to morphix lexica in order to  express, sub-word-categorization,
;;; sorts, semantic and syntactic information.
;;; This will be  done by adding corresponding
;;; leaf-nodes in the morphix-tries;
;;; Currently the leaf-name :info is used to represent morpholgical
;;; information;
;;; now we will provid leaf-names: 
;;; - :mcat (for sub-word-categorization)
;;; - :sort (for sorts)
;;; - :synsem (for syntax and semantics)
;;;
;;; This kind of additional information has to be distinguish
;;; wrt. different word-catgeories.
;;; Therefore, the value of such additional leaf-name
;;; will be of form:
;;; 
;;; <leaf-name-value> ::= value | values
;;; <value> ::= (<cat> . {<cat-value>| <cat-values>})
;;; <values> ::= ( multiple <value>+)
;;; <cat-value> ::= <leaf-value> | <leaf-values>
;;; <leaf-value> ::= "the syntactic form of leaf-name information
;;;                   depends on the specific leaf-name"
;;; <leaf-values> ::= ( multiple <leaf-value>+)
;;;
;;; Notes: the keywrd multiple is used to signal the list of
;;;        values is a list of alternatives. We need this additional
;;;        in order to distinguish this kind of list from
;;;        values which are also represented as lists but which have a 
;;;        different semantics; then more efficient retrieval is possible
;;;
;;; Below some generic functions are defined which are used
;;; - to insert a new value under a given leaf-name and specific cat
;;;   insertion also allows overwriting of old values
;;; - retrieval:
;;;   - using: key and cat as index
;;;   - using: key, value and optional also cat as index
;;;   - deletion
;;;
;;; All these functions are defined as extensions to  the basic
;;; functions defined for the data type trie;
;;;
;;; In the case the new leaf-name values are defining sorts
;;;  and have been defined as a TDL-sort-lattice, retrieval will
;;; also support sort-inference
;;; Notes on the form of new leaf-names:
;;;
;;; :mcat: a lisp-symbol denoting a leaf-tdl-sort
;;; :sort: a two alement list of (<sort-name> <count/mass>)
;;;        <sort-name> will belong to a tdl-sort-lattice
;;; :synsem: a feature structure of form
;;;          [syn: syn-value 
;;;           sem: sem-value]
;;;          currenlty, they will be implemented as assoc lists
;;;
;;; the form of the sem-value will be a list of sem-elements
;;; where each sem-elements will  be of the general
;;; form:
;;; [pred: pred
;;;  inst: sort
;;;  id: id]
;;; where pred will equal to stem value, inst will be equal to sort
;;; and id will be a unique identifier (prob. only used for verbs)
;;; verbs will also contain elements for representing their argument
;;; structure;
;;; The idea behind this representation, is that we will treat semantic
;;; expression computed by the FST as flat lists. Then we hope to be able
;;; to supprot a more simple domain-specific inference/planning
;;;
;;; Next steps:
;;; - support displaying of all leaf-node information
;;; - semi-automatic  definition of leaf-node information
;;; - file-handling
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MORPHIX")

;;; generic function definitions:

(defun get-leaf-entry (stem cat &key (lexicon *stem-lexicon*)
                                     (leaf-name :info)
                                     (test #'equal))
  "get-leaf-entry() retrieves the VALUE of CAT found under LEAF-NAME
found under STEM in LEXICON. The  test function for VALUE is TEST."
  
  (let* ((cat-alist (dtree::get-entry stem lexicon
                                      :leaf-name leaf-name)))
    ;; when stem has an entry for leaf-name
    (when cat-alist
      ;; if the found entry is ambiguous
      (if (multiple-lex-entry-p cat-alist)
          ;; then search for the reading cat
          (assoc cat (rest cat-alist) :test test)
        ;; else, directly compare cat-value of found
        ;; unique value
        (if (funcall test  (first cat-alist) cat)
            ;; and return it
            cat-alist)))))
  
  
(defun find-leaf-entry (stem entry 
                        &key cat
                             (lexicon *stem-lexicon*)
                             (leaf-name :info)
                             (key-test #'equal)
                             (entry-test #'equal))
  "find-leaf-entry() searches for a leaf-name entry under path stem.
If cat is specified only the entry-information of that cat is searched
through.
Otherwise all leaf-name-lists of all  possible cats are checked whether they
contain entry; If so all candidate leaf-name-list are pushed into a stack, 
which is finally returned."
  
  (let* ((leaf-name-alist (if cat
                              ;; if cat is specifed then only consider
                              ;; the value of cat
                              (rest (get-leaf-entry stem cat 
                                              :lexicon lexicon
                                              :leaf-name leaf-name
                                              :test key-test))
                          ;; else consider all cats' information
                          (dtree::get-entry stem 
                                           lexicon
                                           :leaf-name leaf-name))))
    ;; if leaf-name information exists
    (if leaf-name-alist
        ;; if cat is specified
        (if cat
            ;; if found cat information is ambiguous
            (if (multiple-lex-entry-p leaf-name-alist)
                ;; then search entry using member and entry-test
                (member entry (rest leaf-name-alist) :test entry-test)
              ;; else directly compare entry and found value
              (funcall entry-test entry leaf-name-alist))
          
          ;; else consider all cats:
          ;; if ambiguous readings are possible
          (if (multiple-lex-entry-p leaf-name-alist)
              ;; then loop through all readings
              (loop for reading in (rest leaf-name-alist)
                                   ;; extract value of reading
                  for reading-val = (rest reading)
                  when
                    ;; when value is ambiguous
                    (if (multiple-lex-entry-p reading-val)
                        ;; and entry is a member
                        (member entry (rest reading-val) :test entry-test)
                      ;; or reading-val and entry are equal 
                      ;; (using entry-test) 
                      (funcall entry-test entry reading-val))
                    ;; then collect reading in result
                  collect reading into result
                  finally
                    ;; if more readings contain entry then 
                    ;; add dtree::%multi-entry% to resulting list
                    ;; or return the only found element directly
                    (return (if (>  (length result) 1)
                                (cons dtree::%multi-entry% result)
                              (first result))))
            ;; else only one reading
            (let ((reading-val (rest leaf-name-alist)))
              ;; if found cat information is ambiguous
              (if (multiple-lex-entry-p reading-val)
                  ;; then search entry using member and entry-test
                (member entry (rest reading-val) :test entry-test)
              ;; else directly compare entry and found value
              (funcall entry-test entry reading-val))))))))

(defun insert-leaf-entry (stem cat new-entry &key 
                                             (lexicon *stem-lexicon*)
                                             (leaf-name :info)
                                             (test #'equal)
                                             (overwrite-p T))
  "insert-mcat-entry() inserts new-entry information for cat under 
stem in the lexicon. If such an entry already exists and
overwrite-p is true then destructively exchange old and new information. 
If overwrite-p is nil that add the new info to current
information of cat. Otherwise add an element (cat . mcat-info) to
the mcat alist."
  
  (let* ((old-acons (get-leaf-entry stem cat 
                                    :lexicon lexicon 
                                    :leaf-name leaf-name
                                    :test test)))
    ;; if an old entry exists for cat
    (if old-acons 
        ;; if overwrite-p is true
        (if overwrite-p
            (return-from insert-leaf-entry
              ;; then destrucively overwrite it and return
              (setf (rest old-acons) new-entry))
          (return-from insert-leaf-entry
            ;; else augment new information to the old one
            (cond ((multiple-lex-entry-p (rest old-acons))
                   ;; if old entry is ambiguous
                   ;; and new-entry is already there then just return
                   ;; old value
                   (if (member new-entry (cddr old-acons) :test #'equal) 
                       old-acons
                     ;; else desctruviley add the new information
                     (setf (cddr old-acons)
                       (cons new-entry (cddr old-acons)))))
                  ;; else check whether old and new entry is equal
                  ((equal new-entry old-acons) old-acons)
                  ;; and if not add the new entry and mark the resulting
                  ;; list as ambiguous using the value dtree::%multi-entry% 
                  (t (setf (rest old-acons)
                             (list dtree::%multi-entry% 
                                   new-entry 
                                   (rest old-acons)))))))
      ;; else just insert a new cons-pair (cat . <mcat-info)
      ;; under the leaf-name
      (dtree::insert-entry stem 
                          (cons cat new-entry)
                          lexicon
                          :leaf-name leaf-name))))

;;; specific function definition

;;; functions for slot :mcat

(defun get-mcat-entry (stem cat &key (lexicon *stem-lexicon*))
  (get-leaf-entry stem cat :lexicon lexicon :leaf-name :mcat))

(defun find-mcat-entry (stem cat-entry  &rest keywrds)
  (apply #'find-leaf-entry (append (list stem cat-entry :leaf-name :mcat)
                                   keywrds)))

(defun insert-mcat-entry (stem cat new-mcat &rest keywrds)
  (apply #'insert-leaf-entry (append 
                              (list stem cat new-mcat :leaf-name :mcat) 
                              keywrds)))
  
;;; functions for slot :sort

(defun get-sort-entry (stem cat &key (lexicon *stem-lexicon*))
  (get-leaf-entry stem cat :lexicon lexicon :leaf-name :sort))

(defun find-sort-entry (stem sort-entry  &rest keywrds)
  (apply #'find-leaf-entry (append (list stem sort-entry :leaf-name :sort)
                                   keywrds)))

;;; Currently sort information computed by WK is of form
;;; (sort-name count/mass)
;;; I will currently consider sort-name as an entry, and
;;; eventually put the count/mass information under its own
;;; leaf-name;

(defun insert-sort-entry (stem cat new-sort &rest keywrds)
  (apply #'insert-leaf-entry (append 
                              (list stem cat 
                                    (first new-sort) :leaf-name :sort) 
                              keywrds)))

;;; functions for slot :synsem

(defun get-synsem-entry (stem cat &key (lexicon *stem-lexicon*))
  (get-leaf-entry stem cat :lexicon lexicon :leaf-name :synsem))

(defun find-synsem-entry (stem synsem-entry  &rest keywrds)
  (apply #'find-leaf-entry (append (list stem synsem-entry :leaf-name :synsem)
                                   keywrds)))

(defun insert-synsem-entry (stem cat new-synsem &rest keywrds)
  (apply #'insert-leaf-entry (append 
                              (list stem cat new-synsem :leaf-name :synsem) 
                              keywrds)))
 

;;; functions for slot :sc

(defun get-subcat-entry (stem cat &key (lexicon *stem-lexicon*))
  (get-leaf-entry stem cat :lexicon lexicon :leaf-name :sc))

(defun find-subcat-entry (stem synsem-entry  &rest keywrds)
  (apply #'find-leaf-entry (append (list stem synsem-entry :leaf-name :sc)
                                   keywrds)))

(defun insert-subcat-entry (stem cat new-synsem &rest keywrds)
  (apply #'insert-leaf-entry (append 
                              (list stem cat new-synsem :leaf-name :sc)
                              (list :overwrite-p NIL)
                              keywrds)))
 

;;; functions for slot :tdl

(defun get-tdl-entry (stem cat &key (lexicon *stem-lexicon*))
  (get-leaf-entry stem cat :lexicon lexicon :leaf-name :tdl))

(defun find-tdl-entry (stem synsem-entry  &rest keywrds)
  (apply #'find-leaf-entry (append (list stem synsem-entry :leaf-name :tdl)
                                   keywrds)))

(defun insert-tdl-entry (stem cat new-synsem &rest keywrds)
  (apply #'insert-leaf-entry (append 
                              (list stem cat new-synsem :leaf-name :tdl)
                              (list :overwrite-p NIL)
                              keywrds)))