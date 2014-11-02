;;; -*- Syntax: Common-lisp; Base: 10; Mode: LISP; Package: (:morphix :use :scl :colon-mode :external); Default-character-style: (:FIX :ROMAN :LARGE); -*-

(in-package :morphix)

;;; *morphix-lex-path* defined in  load-morphix.lisp !!

(defvar *prefixlex*
    (merge-pathnames (pathname "prefix")
                     cl-user::*lex-path*)
                                       
  "filename to store additional prefixes during the dialog")

(defvar *tmplex*
    (merge-pathnames (pathname "tmplex")
                     cl-user::*lex-path*)
  "filename to store entries during the clarification-dialog")

(defvar *userlex*
    (merge-pathnames (pathname "userlex")
                     cl-user::*lex-path*)
  "filename to store entries of the actual domain")

(defvar *fail-val* :NO-RES)

; ---------------------------------------------------------------------------

(defvar *IAL* nil "inflectional allomorph lexicon")
;; is defined in ialtree.lisp

(defvar *stem-lexicon* nil "lexicon to contain the stems")
;; is defined in main.lisp

(defvar *fullform-lexicon* nil
  "lexicon to contain uninflectional words")
;; is defined in main.lisp

(defvar %minimal-stem-length% 2
  "used during segmentation of a word")

(defvar %additional-algorithmic% '(stem . t)
  "additional analysis required for a fullform")


; ---------------------------------------------------------------------------
;; templates for lex-entries

(defvar %noun-category% '(wortart . nomen))

(defvar %gender-entry%
	(make-array 4 :initial-contents
		    '(nil (genus . mas)
			  (genus . fem)
			  (genus . ntr))))

(defvar %umlaut-entry% '(uml . t))

(defvar %sg-entry%
	(make-array 10 :initial-contents
	      '(nil (ksg . 1) (ksg . 2) (ksg . 3) (ksg . 4)
		    (ksg . 5) (ksg . 6) (ksg . 7) (ksg . 8)
		    (ksg . 9))))

(defvar %pl-entry%
  (make-array 14 :initial-contents
	      '(nil (kpl . 1) (kpl . 2) (kpl . 3) (kpl . 4)
		    (kpl . 5) (kpl . 6) (kpl . 7) (kpl . 8)
		    (kpl . 9) (kpl . 10) (kpl . 11) (kpl . 12)
		    (kpl . 13))))

(defvar %verb-category%
	'(wortart . verb))

(defvar %vtype-entry%
	(make-array 12 :initial-contents
		    '(nil (vtyp . 1) (vtyp . 2) (vtyp . 3) (vtyp . 4)
			  (vtyp . 5) (vtyp . 6) (vtyp . 7) (vtyp . 8)
			  (vtyp . 9) (vtyp . 10) (vtyp . 11))))

(defvar %segment-entry%
	(make-array 8 :initial-contents
		    '(nil (vgfa . a1) (vgfa . a2) (vgfa . a3) (vgfa . a4)
			  (vgfa . a5) (vgfa . a6) (vgfa . a7))))

(defvar %paux-entry%
	(make-array 2 :initial-contents
		    '((paux . "sei") (paux . "hab"))))

(defvar %adjective-category%
	'(wortart . adjektiv))

(defvar %komp-entry%
	(make-array 5 :initial-contents
		    '(nil (komp . st) (komp . est)
			  (komp . irreg) (komp . irreg-pos))))

(defvar %umlaut-adj-entry%
	(make-array 3 :initial-contents
		    '(nil (uml . nec) (uml . pos))))

(defvar %elision-entry%
	(make-array 3 :initial-contents
		    '(nil (elision . nec) (elision . pos))))

(defvar %pred-e-entry%
	'(pred-e . t))

(defvar %adj-main-entry%
	(make-array 8 :initial-contents
		    '(nil (main . a1) (main . a2) (main . a3)
			  (main . b1) (main . b2) (main . b3)
			  (main . c1))))

(defvar %possessive-category%
	'(wortart . possessivpronomen))

(defvar %determinativ-category%
	'(wortart . determinativ))

(defvar %determinativ-indef-category%
	'(wortart . determinativ-indef))

(defvar %card-category%
	'(wortart . kardinalzahl))

(defvar %det-number%
	(make-array 3 :initial-contents
		    '((det-num . sg+pl)
		      (det-num . sg)
		      (det-num . pl))))

(defvar %card-entry%
	(make-array 4 :initial-contents
		    '(nil (ending . t)
			  (ending . st)
			  (ending . no))))

(defvar %fullform-categories%
	(make-array
	  16 :initial-contents
	  '((wortart verbzusatz) (wortart adverb) (wortart partikel)
	    (wortart koord-konjunktion) (wortart subord-konjunktion)
	    (wortart praeposition) (wortart relativpronomen)
	    (wortart determinativ) (wortart reflexivpronomen)
	    (wortart personalpronomen) (wortart interrogativpronomen)
	    (wortart interpunktion) (wortart frageadverb)
	    nil nil (wortart stop-word))))

(defvar %prep-cases%
  (make-array 7 :initial-contents
	      '(nil (gen) (dat) (akk)
		    (gen dat akk) (gen dat) (dat akk))))

; ---------------------------------------------------------------------------
;;; Data encoding leaves in the inflectional allomorph lexicon

(defvar %deklin-tab%
	(make-array 11 :initial-contents
	      '(nil ((nom)) ((gen)) ((dat)) ((dat akk)) ((nom akk))
		    ((nom dat akk)) ((gen dat akk)) ((nom gen dat akk))
		    ((gen dat)) ((nom gen akk)))))

(defvar %adj-results%
	(make-array 7 :initial-contents
		    '( ;; the encoding for "e"
		      ((ohne ((mas ((pl (nom akk))))
			      (fem ((sg (nom akk)) (pl (nom akk))))
			      (ntr ((pl (nom akk))))))
		       (bestimmt ((mas ((sg (nom))))
				  (fem ((sg (nom akk))))
				  (ntr ((sg (nom akk))))))
		       (unbestimmt ((fem ((sg (nom akk)))))))
		      ;; the encoding for "en"
		      ((ohne ((mas ((sg (gen akk)) (pl (dat))))
			      (fem ((pl (dat))))
			      (ntr ((sg (gen)) (pl (dat))))))
		       (bestimmt
			 ((mas ((sg (gen dat akk))
				(pl (nom gen dat akk))))
			  (fem ((sg (gen dat))
				(pl (nom gen dat akk))))
			  (ntr ((sg (gen dat))
				(pl (nom gen dat akk))))))
		       (unbestimmt
			 ((mas ((sg (gen dat akk))
				(pl (nom gen dat akk))))
			  (fem ((sg (gen dat))
				(pl (nom gen dat akk))))
			  (ntr ((sg (gen dat))
				(pl (nom gen dat akk)))))))
		      ;; the encoding for "es"
		      ((ohne ((ntr ((sg (nom akk))))))
		       (unbestimmt ((ntr ((sg (nom akk)))))))
		      ;; the encoding for "em"
		      ((ohne ((mas ((sg (dat))))
			      (ntr ((sg (dat)))))))
		      ;; the encoding for "er"
		      ((ohne ((mas ((sg (nom)) (pl (gen))))
			      (fem ((sg (gen dat)) (pl (gen))))
			      (ntr ((pl (gen))))))
		       (unbestimmt ((mas ((sg (nom)))))))
		      ;; the encoding for nil
		      ((praedikativ-gebraucht))
		      ;; the encoding for nil
		      ;;  (adjectives not predicative-used)
		      ((unflektiert))))
  "array that contains the various possibilities for adjective-inflection")

; ---------------------------------------------------------------------------
;;; global vars to direct morphological analysis

(defvar *property-retrieval* nil
  "yet analyzed words are only looked up in a hash-table")

(defvar *clarification-dialog-on* t
  "flag to enable or disable the clarification-dialog")

(defvar %morphix-results% (make-hash-table :test #'equal)
  "hash-table to contain words that have been analyzed during one session")

(defvar *whole-input* nil)

(defvar %word-result% nil
  "list containing the result of the morphological analysis for a word")

(defvar *prefix-lexicon* nil
  "will become the pointer to a TRIE structure")

(defvar *additional-selectors* nil
  "list of possible keys in the lexicon to be transported through analysis")

(defvar %key-result% nil
  "list for additional-selector-storage during analysis")

; ---------------------------------------------------------------------------
;;; global vars for morphological synthesis


(defvar *exp-subj-p* nil
  "if t, then an expanded form of subjunctive can be build")

(defvar %verb-suffix-list%
  '(nil "n" "en" "e" "st" "t" "et" "est" "d"
	"ten" "te" "tet" "test"
	"eten" "ete" "etest" "etet")
  "optimization: for verbs check only verb-suffixes during synthesis")

(defvar %noun-suffix-list%
  '(nil "en" "es" "e" "s" "n" "er" "ern" "se" "ses" "sen"
	"nen" "ns" "ens" "ien" "ten" "te")
  "optimization: for nouns check only noun-suffixes during synthesis")

(defvar %adjective-suffix-list%
  '(nil "er" "en" "e" "em" "es"
	"ster" "sten" "ste" "stem" "stes"
	"ester" "esten" "este" "estem" "estes"
	"erer" "eren" "ere" "erem" "eres"
	"st" "est")
  "optimization: for adjectives check only adjective-suffixes")

(defvar %posspron-suffix-list%
  '(nil "es" "em" "en" "e" "er")
  "check only suffixes of possessive-pronouns during synthesis")

(defvar %ordinal-suffix-list%
  '("t" "tes" "tem" "ten" "te" "ter"
    "st" "stes" "stem" "sten" "ste" "ster"
    nil "es" "em" "en" "e" "er")
  "check only these suffixes during synthesis")

(defvar %perspron-tree%
  '((1
     (sg (nom "ich") (gen "meiner") (dat "mir") (akk "mich"))
     (pl (nom "wir") (gen "unser") ((dat akk) "uns")))
    (2
     (sg (nom "du") (gen "deiner") (dat "dir") (akk "dich"))
     (pl (nom "ihr") (gen "euer") ((dat akk) "euch")))
    (2a
     ((sg pl) ((nom akk) "Sie") (gen "Ihrer") (dat "Ihnen")))
    (3
     (sg (nom (mas "er") (fem "sie") (ntr "es"))
	 (gen (mas "seiner") (fem "ihrer") (ntr "seiner"))
	 (dat (mas "ihm") (fem "ihr") (ntr "ihm"))
	 (akk (mas "ihn") (fem "sie") (ntr "es")))
     (pl ((nom akk) "sie") (gen "ihrer") (dat "ihnen"))))
  "generation of persprons is realized as fullforms")

(defvar %reflexivpron-tree%
  '((1 (sg (akk "mich") (dat "mir")) (pl ((akk dat) "uns")))
    (2 (sg (akk "dich") (dat "dir")) (pl ((akk dat) "euch")))
    ((3 2a) ((sg pl) ((akk dat) "sich")))))

(defvar %detdef-tree%
  '((sg (nom (mas "er") (fem "ie") (ntr "as"))
	(gen ((mas ntr) (det "es") (t "essen"))
	     (fem (det "er") (t "eren")))
	(dat ((mas ntr) "em") (fem "er"))
	(akk (mas "en") (fem "ie") (ntr "as")))
    (pl ((nom akk) ((mas fem ntr) "ie"))
	(gen ((mas fem ntr) (det "er") (t "eren")))
	(dat ((mas fem ntr) (det "en") (t "enen"))))))

(defvar %query-tree%
  '((sg (nom ((mas fem) "er") (ntr "as"))
	(gen ((mas fem ntr) "essen"))
	(dat ((mas fem) "em"))
	(akk ((mas fem) "en") (ntr "as")))))

; ---------------------------------------------------------------------------

(defmacro multiple-lex-entry-p (lex-entry)
  `(and (consp ,lex-entry)
	(eq 'MULTIPLE (car ,lex-entry))))

(defmacro multiple-computed-entry-p (expanded-lex-entry)
  `(eq 'MULTIPLE (car ,expanded-lex-entry)))

(defmacro form-multiple-entry (entry1 entry2)
  `(list 'MULTIPLE ,entry1 ,entry2))

(defmacro encoded-entry (stem-entry)
  `(typep ,stem-entry 'fixnum))

(defmacro look-for-suffix (suffix)
  `(do ((n (1- (length ,suffix)) (1- n))
	(tree *IAL* (cdr (assoc (elt ,suffix n) tree))))
      ((minusp n) tree)
     (if (null tree) (return nil))))

(defmacro get-suffix-information (suffix-param)
  `(let ((suffix ,suffix-param))
     (cdr (assoc 'ROOT (if (null suffix)
			   *IAL*
			   (look-for-suffix suffix))))))

(defmacro double-consonant-prefix-p (prefix prefix-length)
  `(eql (elt ,prefix (1- ,prefix-length))
	(elt ,prefix (- ,prefix-length 2))))

(defmacro vowel-p (char) 
  `(member ,char '(#\e #\i #\a #\o #\u)))

(defmacro umlautung (stem)
  `(do* ((sl (length ,stem))
	 (string-pointer sl (1- string-pointer))
	 (char (if (> string-pointer 0) (elt ,stem (1- string-pointer)))
	       (if (> string-pointer 0) (elt ,stem (1- string-pointer))))
	 (state 0))
	((zerop string-pointer) ,stem)
     (cond ((= state 0)
	    (cond ((member char '(#\a #\o))
		   (return (insert-e ,stem string-pointer sl)))
		  ((eql char #\u) (setq state 1))))
	   ((= state 1)
	    (cond ((eql char #\e) (setq state 0))
		  ((eql char #\a)
		   (return (insert-e ,stem string-pointer sl)))
		  (t (return
		       (insert-e ,stem (1+ string-pointer) sl))))))))

(defmacro length-of-prefix (assoc-vrbz-result)
  `(cadr ,assoc-vrbz-result))

(defmacro vrbz-strip-p (assoc-vrbz-result)
  `(cddr ,assoc-vrbz-result))

; ---------------------------------------------------------------------------
;;; macros used during analysis

(defmacro segment-stem (segment-list)
  `(cadr ,segment-list))

(defmacro segment-suffix (segment-list)
  `(caddr ,segment-list))

(defmacro segment-prefix (segment-list)
  `(car ,segment-list))

(defmacro marked-as-homograph (lex-entry multiple-entry-p)
  `(and ,lex-entry
	(if ,multiple-entry-p
	    (do ((rest-entries (cdr ,lex-entry) (cdr rest-entries)))
		((null rest-entries) nil)
	      (if (member %additional-algorithmic% (car rest-entries))
		  (return t)))
	    (member %additional-algorithmic% ,lex-entry))))
	  
(defmacro pointer-entry-p (entry-as-assoc-list)
  `(assoc 'stamm ,entry-as-assoc-list))

(defmacro get-pointered-key (pointer-entry)
  `(cadr ,pointer-entry))

(defmacro get-pointered-segment (pointer-entry)
  `(cddr ,pointer-entry))

(defmacro complex-suffix-p (subtree)
  `(look-for-one-arc 'weg ,subtree))

; ---------------------------------------------------------------------------
;;; macros used during generation

(defmacro single-verb-form-p (tense voice)
  `(and (eq ,voice 'aktiv)
	;; the passive voice yields always to more than one surface word
	(member ,tense '(praesens imperfekt))
	;; in all other bindings of tense there is a combined surface form
	(not *exp-subj-p*)))
       ;; the expanded-subjunctive in GERMAN for the tense 'imperfekt'
       ;; is a combined surface form e.g. "wuerdest tragen"

(defmacro e-elision (string)
  `(let ((stem-length (length ,string)))
     (if (> stem-length 0)
	 (do ((index (1- stem-length) (1- index)))
	     ((= index -1) ,string)
	   (cond ((eql #\e (elt ,string index))
		  ;; found an "e"
		  (return (low-concat ,string ,string
				      0 index (1+ index)
				      stem-length)))))
	 "")))

(defmacro conc-stem+suffix (stem suffix)
  `(if ,suffix
       (low-concat ,stem ,suffix
		   0 (length ,stem)
		   0 (length ,suffix))
       ,stem))


(defmacro code-tense+mood (tense mood)
  ;; converts the combinations of tense and mood to forms known in the
  ;; IAL of MORPHIX in order to enable matching in the further process.
  `(case ,tense
     (praesens (case ,mood
		 (indikativ 'praesens)
		 (konjunktiv 'konjunktiv-1)
		 (imperativ 'imperativ)))
     (imperfekt (case ,mood
		  (konjunktiv 'konjunktiv-2)
		  (indikativ 'imperfekt)))))

(defmacro case-found-p (case deklin-value)
  ;; the numbers in the argument-lists for the member-tests
  ;; encode the allowed classes for these cases
  `(case ,case
     (nom (member ,deklin-value '(8 10 6 5 1)))
     (gen (member ,deklin-value '(8 10 2 9 7)))
     (dat (member ,deklin-value '(8 3 6 9 7 4)))
     (akk (member ,deklin-value '(8 10 5 6 7 4)))))

(defun input-restrictions (mask-number)
  (case mask-number
    (1 '((member result '(1 2 3))))
    (2 '((and (integerp result) (< 0 result 10))))
    (3 '((or (symbolp result) (member result '(0 1)))))
    (4 '((and (integerp result) (< 0 result 5))))
    (5 '((or (symbolp result) (eql result 1))))
    (6 '((symbolp result)))
    (7 '((member result '(1 4 6 7 9 13))))
    (8 '((member result '(0 1 2 6 7 8 9 t))))
    (9 '((member result '(1 6))))
    (10 '((and (integerp result) (< 0 result 14))))
    (11 '((and (integerp result) (< 0 result 8))))
    (12 '((member result '(0 1))))))

(defmacro handle-masks (mask-identifier form)
  `(do ((result ,form ,form))
       ((cond ((cancelation result) (throw '%cancel-sign% nil))
	      (,@(input-restrictions mask-identifier)
	       (return (if (numberp result)
			   result
                         (get-low-string result))))))))

;;;------------- Macros used in paradice interface functions ------------------
;;; basic tripple access functions
;;; use for interfacing to morphix, not inside morphix

(defmacro tripple-stem (tripple)
  `(car ,tripple))

(defmacro tripple-morph-type (tripple)
  `(cadr ,tripple))

(defmacro tripple-pos (tripple)
  `(cddr ,tripple))

;;;------------- Globals used for compund processing --------------------------

;;; GN on 17/6/97
;;; specifies the set of those POS which will be considered as
;;; relevant cats
(defvar *relevant-morphix-cats* NIL)
(setq *relevant-morphix-cats* '(NOMEN ADJEKTIV))

;;; this variable triggers activation of composita analysis inside
;;; morphix
(defvar *handle-composita* nil)
(setq *handle-composita* T)

;;; the following global variable triggers activation of unknown word handling
;;; however, currently not in use

(defvar *handle-unknowns* nil)
(setq *handle-unknowns* NIL)

;;; used for exhaustive or non-exhaustive mode

(defvar *all-composita* nil)

;;; used for exhaustive or non-exhaustive mode in combination
;;; *handle-unknowns*

(defvar *all-unknown-composita* nil)
(setq  *all-unknown-composita* NIL)

;;; triggers tracer for composita processing
(defvar *composita-trace* nil) 
(setq *composita-trace* NIL)

;;; to store non-compound and compund results in different registers
(defvar %composita-result% nil)

(defparameter %minimal-noun-length% 3)

;;; this parameter define the NC delimiter sign
;;; used for handling NC like: DFKI-Gebaeude

(defparameter %delimeter-sign% '#\-)

;;; a simple treatment to suppress overgeneration in case of proper-names:
;;; only those entries which have either no :mcat information
;;; or whose :mcat information is a member of a predined list
;;; will be further considered as valid parts of an NC

(defvar  *special-mcat-info-to-allow* nil)
(setq  *special-mcat-info-to-allow* 
  '(:NACHNAME :BERUF :ANREDE :RANG 
    :profession :locality))

(defun ignore-proper-name-p (leaf-assoc)
  (let ((mcat-entry (dtree::get-leaf-info :mcat leaf-assoc)))
    (when *composita-trace*
      (format T "~%Ignore entry ? ~a" mcat-entry))
    (if mcat-entry
        (member (cddr mcat-entry) *special-mcat-info-to-allow* :test #'eq)
      T)))