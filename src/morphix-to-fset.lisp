;;; Copyrights by Guenter Neumann, 1997

(in-package "MORPHIX")

#|

the bnf form of the morphix result will be translated into a structure
that contains templates.
these templates can be expanded into appropriate substructures of an feature logic formalism

in the future I will use templates with parameters

the following subtasks must be worked out:

1.  construct a table that maps terminal symbols of the morphix output to names of templates
    this step is not necessary but allows to use own template names

2.  from the bnf form construct an equivalent feature logic term

note: the well-formed structure of morphix-result is not well-formed, because there are some
inconistent cases, for example, preps and sentential signs have no flexion label but special ones
I will therefor change the corresponding lexical items (this is easy to do because the inconsistent form
are all fullform!!!

at this moment my solution is that I mor specify the access function get-flexion-label

When the changes might not be done, then the morpho-syntactic information is lost
 
||#


#||



subtask 2

1. distinguish the following cases during the construction of a feature term:
 
   a.  postfix notation, e.g. (and t1 t2 (or (not t3) (and t4 t5 t6)))

   b.  infix notation, e.g.   (t1 and t2 ( (not t3) or (t4 and t5 and t6)))

   (c.  mixin of these possibilities, e.g  (t1 and t2 (or (not t3) (and t4 t5 t6)))
       but: mixing is only allowed between different operands)

   It is possible that the syntax of the feature terms uses an operator only 
   implictly, e.g  (t1 t2 (or (not t3) (t4 t5 t6))) here *conj* is used implictly


t1 - t7 are templates (and then they are symbols) or they are feature terms

in general a feature term is a set of pairs label value, separated by a special symbol,
label is a symbol, value is feature term or template

|#

#| Things do to!!

1.  in the following function I have to built in that a lexicon entry can have a special
    key which contains additonal information
    I will assume that this information is a list of templates which themself
    are pointers (or names) into possibly large knowledge sources or that describes
    further morphosyntactic information;
    a claraification dialog could be necessary for the update procedure of the lexicon of 
    morphix

2.  I have to remember the input word for the specification of the phon and graphem value
    of the disered feature description

    |#
#|
;;; possible feature/ values pairs

form time number person
x    time number person
form x    number person
form x    x      x
form x    number x
form x    x      person

x    comp det gender number case
x    x    x   gender number case
x    x    det gender number case
x    x    x   x      x      case
mcat x    det gender number case
mcat x    x   gender number case

comp comp-f  (error in (KOM ((PRAEDIKATIV-GEBRAUCHT))) wird zu
                       ((COMP . COM) (COMP . PRED-USED) NIL), 
                       zb edler )

person x  number case   (zb eure, ich) 
person gender  number case  (zB sie)

dummy number case  (error da dummy eg. ((NIL ((PL (NOM GEN AKK)))))) 

cat
|#


;;; ********************************************************************************
;;;
;;;                                        HEADER

(defvar *disjunction-sign* nil)

(defvar *disjunction-type* nil)

 ; type is postfix or infix or nil (means *disjunction-sign* should not be specified)

(setq *disjunction-sign* :or) ;; see Matiasek's FUN

(setq *disjunction-type* 'postfix)

(defvar *conjunction-sign* nil) 

(defvar *conjunction-type* nil)

(setq *conjunction-type* 'postfix)  ;; see Matiasek's FUN

(setq *conjunction-sign* :and)

(defvar *mapping-morphix-id-to-template-id* nil)

(setq *mapping-morphix-id-to-template-id* 
  '(
    ;; wordclasses

    (nomen (:cat . :n))
    (verb (:cat . :v))
    (hilfsverb (:cat . :aux))
    (modalverb (:cat . :modv))
    (adjektiv (:cat . :a))
    (attributiv-gebrauchtes (:cat . :attr-a))
    (determinativ (:cat . :def))
    (determinativ-indef (:cat . :indef))
    (relativpronomen (:cat . :relpron))
    (personalpronomen (:cat . :perspron))
    (reflexivpronomen (:cat . :refpron))
    (possessivpronomen (:cat . :posspron))
    (interrogativpronomen (:cat . :whpron))
    (ordinalzahl (:cat . :ord))
    (praeposition (:cat . :prep))
    (interpunktion (:cat  . :intp))
    (verbzusatz (:cat . :vpref))
    (adverb (:cat . :adv))
    (koord-konjunktion (:cat . :coord))
    (subord-konjunktion (:cat . :subord))
    (frageadverb (:cat . :whadv))
    (kardinalzahl (:cat . :card))
    (partikel (:cat . :part))
    
    ;; special subcats
    
    (substantivwort (:mcat . :n-adj))
    (artikelwort (:mcat . :det-word))

    ;; sentential signs

    (KLAMMER-RUND-AUF (:sym . :OPEN-PARA))
    (KLAMMER-RUND-ZU (:sym . :CLOSED-PARA))
    
    (ausrufezeichen (:sym . :!sign))
    (fragezeichen (:sym . :questsign))
    (bindestrich (:sym . :seperator))
    (punkt (:sym .  :dot))
    (doppelpunkt (:sym . :dotdot))
    (semikolon (:sym . :semikolon))
    (komma (:sym . :comma))
    
    ;; comparation
    (pos (:comp . :p))
    (kom (:comp . :c))
    (sup (:comp . :s))
    
    
    (praedikativ-gebraucht (:comp-f . :pred-used))
    
    ;; degree of comparation
    
    (ohne (:det .  :none))
    (unbestimmt (:det . :indef))
    (bestimmt (:det . :def))
    
    ;; tempus

    (praesens (:tense . :pres))
    (imperfekt (:tense . :past))
    (konjunktiv-1 (:tense . :subjunct-1))
    (konjunktiv-2 (:tense . :subjunct-2))
    
    ;; verb inflection information

    (infinitiv (:form . :infin))
    (erweiterter-infinitiv (:form . :infin-zu))
    (partizip-perfekt (:form . :psp))
    (partizip-praesens (:form . :prp))
    (partizip-praesens-mit-zu (:form . :prp-zu))
    (imperativ  (:form . :imp))
    (anrede (:person . :anrede))
    
    ;; gender, numerus, case, person
    (nfm (:gender . :nfm))
    (mas (:gender . :m))
    (fem (:gender . :f))
    (ntr (:gender . :nt)) 
    
    (sg (:number . :s))
    (pl (:number . :p))
    
    (nom (:case . :nom))
    (gen (:case . :gen))
    (dat (:case . :dat))
    (akk (:case . :akk))
    
    (1 (:person . 1))
    (2 (:person . 2))
    (2A (:person . :2A))
    (3 (:person . 3))
    ))

(defvar *default-feat-val* NIL)
(setq *default-feat-val* :no)

;;; ********************************************************************************
;;;
;;;                                         INITIALIZATION OF FEATURE SET

(defvar *feature-value-domain* NIL)
(defvar *morphix-identifiers-to-template-names-table*
    (make-hash-table :size 50 :rehash-size 1.5))

(defvar *all-features* NIL)

(defvar *relevant-features* NIL)
(setq *relevant-features* '(:TENSE :FORM :PERSON :GENDER :NUMBER :CASE))

(defvar *feature-length* 0)
(setq *feature-length* (length *relevant-features*))

(defun reset-dnf-cache()
  (memo::un-memoize 'morphix::dnf-one-reading)
  (memo::memoize 'morphix::dnf-one-reading ;; the memoized function
                 :key #'identity  ;; memoize all actual parameters from memoized-function
                 :test #'equal))

(defun select-relevant-features (&aux (cnt -1))
    "Select relevant feature set on basis of chosen elements from
*all-features*."
    ;; firstly: reset dnf-cache because old values cannot be used anymore
    (reset-dnf-cache)
      ;; first reset old relevant features
  (setf *relevant-features* NIL)
  (format T "~%Select relevant features (a for all (minus POS), p for all (plus POS), q for exit):~%")
  (terpri)
  ;; then assign an integer to each possible feature
  (loop for feat in *all-features*
      do
        (format T "~a:~a  " (incf cnt) feat))
  (terpri)
  ;; next get the list of possibilities
  ;; if A has been selected then all features will be
  ;; considerd as being relevant
  (loop for i in (read-delimited-list #\q)
      do
        (cond ((eq i 'p)
               (return
                 (setf *relevant-features* *all-features*)))
              ((eq i 'a)
               (return
                 (setf *relevant-features* (delete :cat *all-features*))))
              (T (setf *relevant-features*
                   (append *relevant-features*
                           (list (nth i *all-features*)))))))
  (setf *feature-length* (length *relevant-features*))
  *relevant-features*)

(defun set-feature-value-domain (feat-val-pair)
  "Computes an assoc list of all features and their values. This assoc list is 
stored in variable *feature-value-domain* and computed through the call of
fill-mapping-table(). Simultaneously, the variable *all-features* is initialized, which
is used when creating the output representation of a FSET-DNF."
  (let ((feat (first feat-val-pair))
        (val (rest feat-val-pair)))
    (if (assoc feat *feature-value-domain*)
        (setf (rest (assoc  feat *feature-value-domain*)) 
          (nreverse 
           (adjoin val
                   (rest (assoc feat *feature-value-domain*)))))
      (progn
        (setf *all-features*
          (push feat *all-features*))
        (setf *feature-value-domain*
          (append *feature-value-domain*
                  `((,feat ,val))))))))

(defun fill-mapping-table ()
  "fill-mapping-table() is used for mapping morphix internal terms to
externally defined terms which are enumerate in variable 
*mapping-morphix-id-to-template-id*."
  
  (setq *all-features* NIL)
  (setq *feature-value-domain* NIL)
  
  
  (do* ((rest-table *mapping-morphix-id-to-template-id* (rest rest-table))
	(pair (first rest-table) (first rest-table))
	(key (first pair) (first pair))
	(value (second pair) (second pair)))
       ((null rest-table) nil)
    (setf (gethash key *morphix-identifiers-to-template-names-table*)
      value)
    (set-feature-value-domain value))
    
  (setf *all-features* (nreverse *all-features*)))


(defun get-template-id (morphix-id)
  (or (gethash morphix-id *morphix-identifiers-to-template-names-table* )
      `(:cat . ,morphix-id)))


(fill-mapping-table)



;;; ********************************************************************************
;;;
;;;                                         MAKE-FSET-TERM

(defun map-lemma-value (lemma)
  `(stem . ,lemma))

(defun conjunction-test (bfn-term)
  "conjunction in the BNF of morphix-results have the form:
- (X), where X is a list or a symbol
- (X Y) where X is a symbol and Y is a list
- in all other cases the term is a disjunction
"
  (or (null (rest bfn-term))
      (and (atom (first bfn-term))
	   (consp (second bfn-term)))))

(defmacro atomic-disjunct-p (disjunct)
  `(atom ,disjunct))

(defun insert-logical-sign (term sign type)
  " A term is a list and its semantic (whether its is a conjunction or
disjunction) is determined in a procedural context)"
  (case type
    (postfix (cons sign term))
    (infix (insert-infix-sign term sign))
    (otherwise term)))

(defun insert-infix-sign (term sign)
  (do* ((rest-term (rest term) (rest rest-term))
	(element (first rest-term) (first rest-term))
	(result-term (list (first term))))
       ((null rest-term) (nreverse result-term))
    (setq result-term (nconc (list element sign) result-term))))

(defun get-flexion-label (reading)
  "Get the information that is located under the label flexion in one reading
of the morphix result"
  (let* ((label-value (or (assoc 'flexion (rest  reading))
			  (assoc 'folgekasus (rest reading))
			  (assoc 'satzzeichen (rest reading))))
	 (label-name (first label-value)))
    (case label-name
      (flexion (second label-value))
      (folgekasus (second label-value))
      (satzzeichen (list (rest label-value)))
      (otherwise nil))))
      
(defun make-translation (bnf-term)
  "toplevel function for translating the bnf term of morpho-syntactic information"
  (cond ((null bnf-term) NIL)
        ((not (conjunction-test bnf-term)) 
	 (handle-disjunction bnf-term))
	(t (handle-conjunction (if (atom (first bnf-term))
				   bnf-term
				   (first bnf-term))))))

(defun handle-disjunction (disjunction)
  (do* ((rest-disjunction  disjunction (rest rest-disjunction))
	(first-disjunct (first rest-disjunction) (first rest-disjunction))
	(result-term nil))
       ((null rest-disjunction) (insert-logical-sign (nreverse result-term)
						     *disjunction-sign*
						     *disjunction-type*))
    (push (if (atomic-disjunct-p first-disjunct)
	      (get-template-id first-disjunct)
	      ;; if a disjunct is a atomic one then determine
	      ;; the associate template 
	      ;; otherwise enter recursively the translation
	      ;; process
	      (make-translation first-disjunct))
	  result-term)))

(defun handle-conjunction (conjunction)
  
  (let ((second-value (second conjunction)))
    ;; conjunction is of the form
    ;; either (X Y) (-> complex conjunct) or (X) (-> simple conjunct)

    (cond ((not second-value)
           (get-template-id (car conjunction))
           )
	  ;; conjunction is a simple one of the form (X)

	  (t (insert-logical-sign 
	      (cons (get-template-id (first conjunction))		    
		    (cond ((not (conjunction-test second-value))
			   ;; the complex conjunct has a disjunctive value Y
			   (list (handle-disjunction second-value)))

			  ((or
                            (atom (first second-value))
                            (= 1 (length (first second-value)))) 
                           
			   ;; the second value of the complex conjunct is a simple 
			   ;; conjunct (i.e. a list with one symbol)
                           ;; e.g. SG in (imperativ (SG))
                           ;; or is the form ((PRAEDIKATIV-GEBRAUCHT)) which is of wrong
                           ;; syntax
                           
			   (list (get-template-id (if (atom (first second-value))
                                                      (first second-value)
                                                    (caar second-value)))))
			  ;; the second value of the conjunct is a complex 
			  ;; conjunction (with additonal outer braces)
			  ;; of the form ((X Y)), where Y (denoted as temp-result) 
			  ;; is a conjunction or a disjunction
			  ;; e.g. ((pl (dat akk))) of the conjunction 
			  ;; ((mas ((pl (akk dat)))))
			  ;; or ((sg (gen))) in ((ntr ((sg (gen)))))

			  (t (let ((temp-result 
				    (make-translation 
				     (second (first second-value)))))
			       (cons (get-template-id (first (first second-value)))
				       
				     (if (and (atom (first temp-result))
					      (null (rest temp-result)))

					 ;; temp-result is a conjunction
					 temp-result

					 (list temp-result)
					 ;; temp-result is a disjunction
					 ))))))
	      *conjunction-sign*
	      *conjunction-type*
	      )))))

(defun pre-compile (reading)
  "This will simply add (stem . <stem>) and (cat . <cat>) to a fset-term"
  (list (map-lemma-value
	  (first reading))
	(get-template-id (second (assoc 'wortart (rest reading))))))

(defun special-morphix-case (bnf-term)
  "this is necessary because morphix-result for GELAUFEN
is ((partizip-perfekt)); I think in all other cases the result is only an atom"
  (and (atom (first bnf-term))
       (null (rest bnf-term))
       (first bnf-term)))


(defun handle-one-reading (reading)
  (let ((flexion-translated (make-translation (get-flexion-label reading)))) 
    (insert-logical-sign
      (append (pre-compile reading)
	      (if flexion-translated
		  (if (special-morphix-case flexion-translated)
		      (list (get-template-id (first flexion-translated)))
		      (list-not-nil flexion-translated))))
      *conjunction-sign*
      *conjunction-type*
      )))

(defun compile-morphix-result (morphix-result)
  "the top-level function for compilation of a morphix-analysis result"
  (cond ((stringp (cadar morphix-result))
         (car morphix-result))
        ((conjunction-test morphix-result)

	 (handle-one-reading (first morphix-result)))
	(t (do* ((rest-reading morphix-result (rest rest-reading))
		 (reading (first rest-reading) (first rest-reading))
		 (result nil))
		((null rest-reading) (insert-logical-sign (nreverse result)
							  *disjunction-sign*
							  *disjunction-type*))
	     (push (handle-one-reading reading)
		   result)))))

(defun fset-morphix (input-string)
  (let* ((input-list (morphix-read input-string))
	 (morph-result (morphix input-list))
	 (res (mapcar #'compile-morphix-result morph-result)))
    (format NIL "~&~{~a~%~}" res)))

(defun fset-wa (input-string)
  (let* ((morphix-res (w-a input-string))
         (fst-res (compile-morphix-result morphix-res)))
    (format t "In: ~a~%" morphix-res)
    (format t "Out: ~a~%" fst-res)
    fst-res))

;;; ********************************************************************************
;;;                                         MAKE DNF TERM

;;; make-fset2dnf for postfix notation only !!!

;;; i.e., (:and c (:or a b)) -> (:or (:and c a) (:and c b))
;;; i.e., (:and (:or a b) c) -> (:or (:and a c) (:and b c))

;;; if (:or a b c) then (:or (make-dnf a) (make-dnf b) (make-dnf c))
;;; if (:and a b c) then (apply-distr (:and (make-dnf a) (make-dnf b) (make-dnf c)))

;;; apply-distr:
;;; beachte: immer of form (:and t1 t2 (:or d1 d2 d3))

;;; the code above is prob not the best, but anyway will only be used offline
(defun and-p (fset)
  (and (consp fset)
       (eq (car fset) *conjunction-sign*)))

(defun or-p (fset)
  (and (consp fset)
       (eq (car fset) *disjunction-sign*)))

(defun term-p (fset)
  (and (consp fset)
       (not (or (eq (car fset) *conjunction-sign*)
                (eq (car fset) *disjunction-sign*)))))

;;; gets as input an postfix-fset computed by compile-morphix-resul()
;;; and constructs the DNF form

(defun make-fset-dnf (postfix-fset)
  (cond 
   ;; if terminal (of form (a . b) then do nothing
   ((term-p postfix-fset) postfix-fset)
   
   ;; if :or then call on each disj function
   ;; make-fset-dnf()
   ((or-p postfix-fset)
    (loop for disj in (rest postfix-fset)
        append
          (make-fset-dnf disj)))
   
   ;; if :and form
   ((and-p postfix-fset)
    (cond ((or-p (car (last postfix-fset)))
           ;; last elem is :or form
           (apply-or-distr postfix-fset))
          ((and-p (car (last postfix-fset)))
           ;; last elem is :and form
           (apply-and-distr postfix-fset))
          ;; :and contains only terminals
          (T (cons
              *conjunction-sign*
              (loop for conj in (rest postfix-fset)
                  collect
                    (make-fset-dnf conj))))))))

(defun apply-or-distr (and-fset)
  (let ((terminals  (butlast ;; get only terminals
                     (rest ;; strip off :and
                      and-fset))))  
    ;; assuming form of (:and t1 t2 (:or d1 d2))
    ;; then make (:or (:and t1 t2 d1) (:and t1 t2 d2))
    
    (make-fset-dnf
     (cons *disjunction-sign*
          (loop for disj in (rest (first (last and-fset)))
              collect
                (cond ((and-p disj)
                       ;; disj is a complex :and
                       ;; then splice terminals in (non-destructively)
                       (cons
                        *conjunction-sign*
                        (append
                         terminals 
                         (rest disj))))
                      (T (term-p disj)
                         ;; disj is a terminal
                         ;; then splice terminals in (non-destructively)
                         (cons *conjunction-sign*
                               (append
                                 terminals
                                 (list disj))))))))))

(defun apply-and-distr (and-fset)
  (let ((terminals  (butlast ;; get only terminals
                     (rest ;; strip off :and
                      and-fset))))  
    ;; assuming form of (:and t1 t2 (:and ...)
    ;; flattens :and form like (:and t1 (:and t2 ...))
          (make-fset-dnf
           (cons *conjunction-sign*
                 (append
                  terminals 
                  (rest (first (last and-fset))))))))


;;;  ********************************************************************************
;;;                                         OUTPUT NORMALIZATION FUNCTIONS

;;; ********************************************************************************
;;;                                         THE FIRST JUST CLUSTERS A FLAT LIST
;;;

;; necessary because make-dnf returns flatten list (which is ok for C++ but not for lisp)
;; maps (:and a b c d :and d e f) to ((a b c) (d e f))

(defun collect-ands (fset-dnf)
  (if (term-p fset-dnf) 
      (when (member (first fset-dnf) *relevant-features* :test #'eq) 
        fset-dnf)
    (remove-duplicates
     (collect-ands* fset-dnf nil nil #'nreverse)
     :test #'equal)))

(defun collect-ands* (fset-dnf new akku constr)
  (cond ((null fset-dnf) (nreverse 
                          (if new (append (list (funcall  constr new)) akku)
                            akku)))
        ((eq (first fset-dnf) *conjunction-sign*)
         (collect-ands* (rest fset-dnf)
                        nil 
                        (if new (append (list (funcall  constr new)) akku)
                          akku)
                        constr))
        (T (collect-ands* (rest fset-dnf)
                          (if (member (first (first fset-dnf)) *relevant-features* :test #'eq)
                             (push (first fset-dnf) new)
                           new)                         
                          akku
                          constr))))
 

;;; ********************************************************************************
;;;
;;;                                         SYMBOL TAGSET

(defun symbol-tags (fset-dnf)
  (if (term-p fset-dnf) 
      (when (member (first fset-dnf) *relevant-features* :test #'eq)
        (rest fset-dnf))
    (remove-duplicates
     (symbol-tags* fset-dnf nil nil)
     :test #'string-equal)))

(defun symbol-tags* (fset-dnf &optional new akku)
  (cond ((null fset-dnf) (nreverse 
                          (if new (append (list new) akku)
                            akku)))
        ((eq (first fset-dnf) *conjunction-sign*)
         (symbol-tags* (rest fset-dnf)
                       nil (if new (append (list new) akku)
                             akku)))
        (T (symbol-tags* (rest fset-dnf)
                         (if (member (first (first fset-dnf)) *relevant-features* :test #'eq)
                             (setf new (if new
                                           (format NIL "~a-~a" new (rest (first fset-dnf)))
                                         (format NIL "~a" (rest (first fset-dnf)))))
                           new)
                         akku))))


;;; ********************************************************************************
;;;                                         MAKE NORMALIZED FEATURE VECTOR

;;; makes a term in form of an acons using the features specified in
;;; *relevant-feature* (in the indicated order)
;;; values are choosen if found in morph-result
;;; if a feature does not occur in morph-result then the special value
;;; :no is used

#|
(defun make-default-feature-vector ()
  (list
   (loop for feat in *relevant-features*
      collect
         `(,feat . ,*default-feat-val*))))
|#
(defun make-default-feature-vector ()
  NIL)


(defun feature-vectors (fset-dnf)
  (if (term-p fset-dnf) 
        ;; first handle case that input i only a simple term
      (when (member (first fset-dnf) *relevant-features* :test #'eq)
        (make-feat-vector (list fset-dnf)))
    
    ;; call make-feat-vectors recursively on each term of flat list
    (or
     ;; either return found feature term
      (remove-duplicates
       (collect-ands* fset-dnf nil nil #'make-feat-vector)
       :test #'equal)
      ;; or the default term in case no relevant-feature found in fset-dnf
      (make-default-feature-vector))))

(defun make-feat-vector (known-acons)
  ;; that's a cheap hack to handle default :fin feature for verbs
  (when (assoc :tense known-acons)
    (push `(:form . :fin) known-acons))
  ;; if no person then use 3 as default
  (unless (assoc :person known-acons)
    (push `(:person . 3) known-acons))
  ;; then check each relevant-feature
  (loop for feat in *relevant-features* 
      for known-feat = (assoc feat known-acons)
      collect
        ;; if input acons elements the relevant feature then 
        ;; select this else use default term-value
        (if known-feat 
            known-feat
          `(,feat . ,*DEFAULT-FEAT-VAL*))))

;;; (setf (get 'mapper  :normalizer) #'feature-vectors)


;;; ********************************************************************************
;;;                                         MAKE NORMALIZED BIT VECTOR     
;;;  NOT YET IMPLEMENTED


;;; ********************************************************************************
;;;                                        THE SYSTEM IMMANENT TOP-LEVEL FUNCTION
;;;


;;; a toplevel function gets a lowercase string as input and computes the DNF form 
(defun fset-dnf (word)
  (funcall (get 'mapper  :normalizer) 
   (make-fset-dnf (fset-wa word))))

;;; ********************************************************************************
;;;                                         PAGE and SMES INTERFACE

;;; the top-level function to the tripple form known from page and smes system
;;; i.e., returns a list of tripple where each tripple is of form:
;;; (stem dnf cat)

(defvar *file-output-p* NIL) ;; will output <word>: <dnf> to file *output-dir*dnf-test

(setq *file-output-p* NIL)

(defun handle-one-flexion (cat flexion)
  (let ((flexion-translated (make-translation flexion)))
    (insert-logical-sign
     (append
        (list (get-template-id cat))
      (if flexion-translated
          (if (special-morphix-case flexion-translated)
              (list (get-template-id (first flexion-translated)))
            (list-not-nil flexion-translated))))
     *conjunction-sign*
     *conjunction-type*
     )))

(defun dnf-one-reading (cat reading)
  (funcall
   (get 'mapper  :normalizer)
   (make-fset-dnf 
    (handle-one-flexion cat reading))))

(defun return-morphix-dnf (res)
  (loop 
      for reading in res 
      for cat =
        (second (assoc 'WORTART (rest reading)))
      collect
        (let ((dnf (dnf-one-reading cat (get-flexion-label reading))))
          (when *file-output-p*
            (let ((tmp-file (merge-pathnames (pathname "dnf-test") *out-dir*)))
              (with-open-file (out-stream tmp-file
                               :direction :output
                               :if-exists :append
                               :if-does-not-exist :create)
                (format out-stream "~A: ~W~%" (car reading) dnf))))
          
        (cons (first reading) 
              (cons dnf (rest (get-template-id  cat)))))))

;;; ********************************************************************************
;;;
;;;                                         INITIALIZATION
;;;
;;;
;;; and then memoize building of DNF form (can be done efficiently because finite domain of
;;; inflectional information
;;;
;;; I should be able to store the memozied hash array in a file so that
;;; memoization can be remembered



(defun set-normalizer (&optional fct-name &aux num)
  (unless fct-name
         (format T "~%Choose the function of output normalization:")
         (format T "~%1: dnf as lisp list")
         (format T "~%2: dnf as feature vector")
         (format T "~%3: dnf as symbol~%")
         (setf num (read))
         (setf fct-name (case num
                          (1 #'collect-ands)
                          (2 #'feature-vectors)
                          (3 #'symbol-tags))))
  (setf (get 'mapper  :normalizer) fct-name))

(defun set-dnf (&optional set-norm)
  (setf (get 'mapper  :morph-fct) #'return-morphix-dnf)
  (set-normalizer set-norm)
  (memo::un-memoize 'morphix::dnf-one-reading)
  (memo::memoize 'morphix::dnf-one-reading ;; the memoized function
                 :key #'identity  ;; memoize all actual parameters from memoized-function
                 :test #'equal))

(set-dnf #'feature-vectors)

(defun use-tags-set ()
  (set-dnf #'symbol-tags)
  (setq *file-output-p* T)
  )

(defun init-dnf ()
  (select-relevant-features)
  (set-dnf NIL))

(defun set-vec ()
  (set-dnf #'feature-vectors))

(defun set-sym ()
  (set-dnf #'symbol-tags))

(defun set-feat ()
  (set-dnf #'collect-ands))

(defun set-types ()
  (setf (get 'mapper  :morph-fct) #'return-types))