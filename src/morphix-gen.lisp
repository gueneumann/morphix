;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MORPHIX -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: morphix-gen.lisp
;;;      module: morphix
;;;   $Revision: 1.1 $
;;;  written by: scherf, dfki saarbruecken
;;;       $Date: 1995/09/30 22:46:18 $
;;;  updated by: scherf, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Software copywrights: Oliver Scherf, 1995/09/05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :morphix)

(defvar *funcall-table* 
  '((noun . generate-noun)
    (verb . generate-verb)
    (adj . generate-adj)
    (past-participle . generate-past-participle)
    (present-participle . generate-present-participle)
    (possessive . generate-possessive)
    (perspron . generate-perspron)
    (reflexive . generate-reflexive)
    (detdef . generate-detdef)
    (query . generate-query)
    (determiner . generate-determiner)
    (determiner-indef . generate-determiner-indef)
    (ordinal . generate-ordinal)))

(defvar *fun-para-table* '((noun (:case nom)
                                 (:number sg))
                           (verb (:mood indikativ)
                                 (:tense praesens)
                                 (:person 1)
                                 (:voice aktiv)
                                 (:number sg))
                           (adj (:comparation pos)
                                (:attributive-used-p T)
                                (:article ohne)
                                (:gender mas)
                                (:number sg)
                                (:case nom))
                           (past-participle (:participle-selector T)
                                            (:comparation pos)
				            (:attributive-used-p T)
				            (:article ohne)
                                            (:gender mas)
                                            (:number sg)
                                            (:case nom))
                           (present-participle (:additional-zu NIL)
				               (:comparation pos)
				               (:attributive-used-p T)
				               (:article ohne)
                                               (:gender mas)
                                               (:number sg)
                                               (:case nom))
                           (possessive (:gender mas)
                                       (:number sg)
                                       (:case nom)
				       (:type substantivwort)
				       (:article ohne))
                           (perspron (:person 1)
                                     (:case nom)
                                     (:number sg)
                                     (gender mas))
                           (reflexive (:person 1)
                                      (:case dat)
                                      (:number sg))
                           (detdef (:case nom)
                                   (:number sg)
                                   (:gender mas)
                                   (:det 'det))
                           (query (:case nom)
                                  (:number sg)
                                  (:gender mas))
                           (determiner (:case nom)
                                       (:number sg)
                                       (:gender mas)
				       (:category 'determinativ))
                           (determiner-indef (:case nom)
                                             (:number sg)
                                             (:gender mas))
                           (ordinal (:attributive-used-p NIL)
				    (:article ohne)
                                    (:gender mas)
                                    (:number sg)
                                    (:case nom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; get-default ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-default (arg-name cat) 
   (first (cdr (assoc arg-name (cdr (assoc cat *fun-para-table*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test-category ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-category (cat funcall-table)
  (if (not (find cat funcall-table :key #'car :test #'eq))
    (let ((default-cat (caar funcall-table)))
      (format T "~%Arghh, ~S is NOT a defined category. I will try it ~
                 with ~
                 ~A as default categroy. Hope the best. The actual ~
                 permitted categories are:~%" cat default-cat)
      (loop for cat-fun-pair in funcall-table
            do (format T "~A~%" (car cat-fun-pair)))
      default-cat)
    ;; else
    cat))
      
;;;;;;;;;;;;;;;;;;;;;;;;;; test-if-right-opt-args ;;;;;;;;;;;;;;;;;;;;;;;

(defun test-if-right-opt-args (cat opt-args-list funcall-table)
  "Checks whether the optional given arguments are permitted for the ~
   category cat."
  (let ((args-list (loop for para-val in 
                         (cdr (assoc cat *fun-para-table*))
                         collect (first para-val))))
    (loop for i from 0 to (- (length opt-args-list) 1) by 2 
          for test = (find (elt opt-args-list i) args-list :test #'eq)
          with flag = NIL
          when test append (list test (elt opt-args-list (+ 1 i)))
          do 
          (unless test
            (setf flag T))
          finally 
          (when (and flag (find cat funcall-table :key #'car :test #'eq))
            (format T "~%Ughh, this is NOT the correct choice of ~
                       arguments. The following keyword arguments ~
                       are permitted for category ~A:~%~{~S~%~}~% ~
                       The wrong arguments are omitted."
                    cat args-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-ordinal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; gender               '(mas fem ntr)
;;; article              '(ohne unbestimmt bestimmt)
;;; attributive-used-p   '(T NIL)s

(defun generate-ordinal (stem &key
                              (cat 'ordinal)
                              (case (get-default :case cat))
                              (number (get-default :number cat))
                              (gender (get-default :gender cat))
                              (article (get-default :article cat))
                              (attributive-used-p
                               (get-default :attributive-used-p cat)))
  (ordinal-inflection stem 
                      attributive-used-p article gender number case))

;;;;;;;;;;;;;;;;;;;;;;;;; generate-determiner-indef ;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; gender               '(mas fem ntr)

(defun generate-determiner-indef (stem &key
                                  (cat 'determiner-indef)
                                  (case (get-default :case cat))
                                  (number (get-default :number cat))
                                  (gender (get-default :gender cat)))
  (determiner-indef-inflection stem case number gender))
                                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-determiner ;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; gender               '(mas fem ntr)
;;; category             '(determinativ 'relativpronomen)

(defun generate-determiner (stem &key
                            (cat 'determiner)
                            (case (get-default :case cat))
                            (number (get-default :number cat))
                            (gender (get-default :gender cat))
                            (category (get-default :category cat)))
  (determiner-inflection stem case number gender category))

;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; gender               '(mas fem ntr)
;;;
;;; Important notice from "The Morphix-Brothers":
;;; only correct for (and (eq number 'sg)
;;;                       (not (and (eq gender 'ntr)
;;;                                 (eq case 'dat))))

(defun generate-query (&key
                       (cat 'query)
                       (case (get-default :case cat))
                       (number (get-default :number cat))
                       (gender (get-default :gender cat)))
  (query-inflection case number gender))

;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-detdef ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; gender               '(mas fem ntr)
;;; det                  '(det)

(defun generate-detdef (&key
                        (cat 'detdef)
                        (case (get-default :case cat))
                        (number (get-default :number cat))
                        (gender (get-default :gender cat))
                        (det (get-default :det cat)))
  (detdef-inflection case number gender det))
                        
;;;;;;;;;;;;;;;;;;;;;;;; generate-reflexive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; person               '(1 2 3)

(defun generate-reflexive (&key
                           (cat 'reflexive)
                           (person (get-default :person cat))
                           (case (get-default :case cat))
                           (number (get-default :number cat)))
  (reflexive-inflection person case number))

;;;;;;;;;;;;;;;;;;;;;;;; generate-perspron ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; gender               '(mas fem ntr)
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; person               '(1 2 3)

(defun generate-perspron (&key
                          (cat 'perspron)
                          (person (get-default :person cat))
                          (case (get-default :case cat))
                          (number (get-default :number cat))
                          (gender (get-default :gender cat)))
  (perspron-inflection person case number gender))

;;;;;;;;;;;;;;;;;;;;;;;; generate-possessive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; gender               '(mas fem ntr)
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; type                 '(substantivwort)
;;; article              '(ohne unbestimmt bestimmt)

(defun generate-possessive (stem &key
                                 (cat 'possessive)
                                 (gender 
                                  (get-default :gender cat))
                                 (number
                                  (get-default :number cat))
                                 (case
                                   (get-default :case cat))
	                         (type 
                                  (get-default :type cat))
		                 (article
                                  (get-default :article cat)))
  (possessive-inflection stem gender number case type article))

;;;;;;;;;;;;;;;;;;;;;;; generate-present-participle ;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; comparation          '(pos kom sup)
;;; attributive-used-p   '(T NIL) 
;;; article              '(ohne unbestimmt bestimmt)
;;; gender               '(mas fem ntr)
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; additional-zu        '(T NIL) Adds a "zu" string or not.

(defun generate-present-participle (stem &key
                                         (cat 'present-participle)
                                         (additional-zu
                                          (get-default 
                                           :additional-zu cat))
				         (comparation 
                                          (get-default :comparation cat))
				         (attributive-used-p
                                          (get-default 
                                           :attributive-used-p cat))
				         (article (get-default 
                                                   :article cat))
                                         (gender 
                                          (get-default :gender cat))
                                         (number 
                                          (get-default :number cat))
                                         (case (get-default :case cat))
                                         (entry 
                                          (find-associate-entry
                                           stem
                                           '(verb hilfsverb modalverb))))
  (present-participle-inflection stem additional-zu comparation
                                 attributive-used-p article gender
                                 number case entry))
               
;;;;;;;;;;;;;;;;;;;;;;; generate-past-participle ;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; comparation          '(pos kom sup)
;;; attributive-used-p   '(T NIL) 
;;; article              '(ohne unbestimmt bestimmt)
;;; gender               '(mas fem ntr)
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)
;;; participle-selector  <a selector>

(defun generate-past-participle (stem &key
                                      (cat 'past-participle)
				      (participle-selector
                                       (get-default :participle-selector
                                                    cat))
                                      (comparation 
                                       (get-default :comparation cat))
				      (attributive-used-p
                                       (get-default 
                                        :attributive-used-p cat))
				      (article (get-default 
                                                :article cat))
                                      (gender (get-default :gender cat))
                                      (number (get-default :number cat))
                                      (case (get-default :case cat))
                                      (entry 
                                       (find-associate-entry 
                                        stem 
                                        '(verb hilfsverb modalverb))))
  (past-participle-inflection stem entry participle-selector comparation
                              attributive-used-p article gender number 
                              case))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-noun ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg      permitted values
;;; --------------------------------------------------------------------
;;; case              '(nom gen dat akk)
;;; number            '(sg pl)
;;; entry             <entry>

(defun generate-noun (stem &key 
                           (cat 'noun)
                           (case (get-default :case cat))
                           (number (get-default :number cat))
			   (entry (find-associate-entry
                                   (string-downcase stem) 'nomen)))
  (noun-inflection stem case number entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-verb ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg      permitted values
;;; --------------------------------------------------------------------
;;; tense             '(praesens imperfekt perfekt 
;;;                     plusquamperfekt futur-1 futur-2)
;;; mood              '(indikativ konjunktiv imperativ)
;;; voice             '(aktiv passiv)
;;; person            '(1 2 3)
;;; number            '(sg pl)
;;; entry             <an entry>

(defun generate-verb (stem &key
                           (cat 'verb)
                           (number (get-default :number cat))
                           (tense (get-default :tense cat))
                           (mood (get-default :mood cat))
                           (person (get-default :person cat))
                           (voice (get-default :voice cat))
                           (entry (find-associate-entry stem '(verb 
                                                             hilfsverb
                                                             modalverb))))
  (verb-inflection stem tense mood voice person number entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; generate-adj ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optional arg         permitted values
;;; --------------------------------------------------------------------
;;; comparation          '(pos kom sup)
;;; attributive-used-p   '(T NIL) 
;;; article              '(ohne unbestimmt bestimmt)
;;; gender               '(mas fem ntr)
;;; number               '(sg pl)
;;; case                 '(nom gen dat akk)

(defun generate-adj (stem &key
                          (cat 'adj)
                          (comparation (get-default :comparation cat))
                          (attributive-used-p (get-default 
                                               :attributive-used-p cat))
                          (article (get-default :article cat))
                          (gender (get-default :gender cat))
                          (number (get-default :number cat))
                          (case (get-default :case cat))
                          (lex (find-associate-entry stem 'adjektiv)))
  (adjective-inflection stem comparation attributive-used-p 
                        article gender number case :lex lex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; generate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; top level function
;;;
;;; argument             permitted values
;;; --------------------------------------------------------------------
;;; cat                  '(noun verb adj past-participle perspron
;;;                        present-participle possessive reflexive 
;;;                        detdef query determiner determiner-indef 
;;;                        ordinal)

(defun generate (stem cat
                      &rest other-keys
                      &key (funcall-table *funcall-table*) 
                      (fun-para-table NIL)
                      &allow-other-keys)
  (let ((result nil)
        (h-fun-para-table (when fun-para-table
                            *fun-para-table*))
        (stem-arg (string-downcase stem))
        (test-cat (test-category cat funcall-table))
        (test-rest-args (when other-keys
                          (test-if-right-opt-args cat other-keys
                                                  funcall-table))))
    (when fun-para-table
      (setf *fun-para-table* fun-para-table))
    (setf result
          (apply (cdr (assoc test-cat funcall-table)) 
                 (if stem 
                   (cons stem-arg test-rest-args)
                   ;; else
                   test-rest-args)))
    (when fun-para-table
      (setf *fun-para-table* h-fun-para-table))
    result))
