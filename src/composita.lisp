;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: composita.lisp
;;;      module: morphix
;;;     version: 4.0
;;;  written by: Guenter Neumann (Copyrights)
;;; last update: 27.1.96
;;;  updated by: Guenter Neumann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;; G.Neumann         |  17/6/97   | allowed for parametrization
;;;                                | of relevant cats considered as right-most element

(in-package :morphix)


;;; ***************  START OF NC PROCESSING STUFF *********************

;;; the top-level function for performing NC analysis
(defun set-NC-infix (&optional reset)
  (setq mo::*noun-gen-sign* (when reset "-G-"))
  (setq mo::*noun-pl-sign* (when reset "-PL-"))
  (setq MO::*verb-infix-sign* (when reset "-V-"))
  (setq mo::*noun-S-sign* (when reset "S"))
  (setq *separator-char* (if reset #\- #\#))
  )

(defun analyze-compound 
    (input-word stem ending  nc-umlaut? &key umlaut? ge? prefix? zu? 
                                  (lexicon *stem-lexicon*)
                                  (leaf-name :info) (exhaustive T))
  (if (or ge? prefix? zu?)
        NIL
      (get-substrings stem ending lexicon nc-umlaut? umlaut?
                    :leaf-name leaf-name :exhaustive exhaustive)))

;;; the main function of NC analysis
;;; it treats the lexicon as a DAG and hence performs a recursive
;;; treatment of the NC
;;; it is an adaption of the basic DTREE function having the same name

(defun get-substrings (word ending lexicon nc-umlaut? umlaut?
                       &key (leaf-name :info) (exhaustive nil)
                            (one-composition nil))
  (when *composita-trace*
    (format T "~2%  WORD: ~a" word)
    (format T "~%  EXHAUSTIVE MODE: ~a" exhaustive))
  (let* ((word-length (length word))
         (lookup (when (>= word-length %minimal-noun-length%)
                   (get-prefix-entries word lexicon nc-umlaut? umlaut? :leaf-name leaf-name)))
         (longest-suffix (first lookup))
         (offset (first longest-suffix))
         (rest-suffixes (rest lookup)))
    (when *composita-trace* (format T "~%  LOOKUP: ~a" lookup))
    (if (null lookup)
        ;; if no more prefixes can be computed
        ;; then if end of word has be reached
        (if (eql (length word) 0)
            ;; at this place: perform morpholigical analysis of the last word
            ;; by first checking whether it is a noun and
            ;; then by checking whether it is a valid inflected form
                (inflect-composita-noun one-composition ending umlaut? nc-umlaut?)
          
          ;; the following is currently too brute-force so I willl not make 
          ;; use of it
          (if (and
               (>= word-length %minimal-noun-length%)
               morphix::*handle-unknowns*)
              (progn 
                (when *composita-trace* (format T "~%  HANDLE UNKNOWN FORM: ~a" word))
                (get-substrings (subseq word 1)
                                     ending
                                     lexicon
                                     (mapcar #'(lambda (x) (- x 1)) nc-umlaut?)
                                     umlaut?
                                     :leaf-name leaf-name
                                     :exhaustive morphix::*all-unknown-composita*
                                     :one-composition one-composition))
                                     
          :done))
      (progn
        ;; compute all possible prefix of next suffix which
        ;; starts at position offset; offset is just the end-marker
        ;; of the previous prefix computed in the above call of
        ;; get-prefix-entries
        ;; the current longest-suffix is pushed into list
        ;; one-composition
        (when *composita-trace*
	  (format T
		  "~%  CHECK LONGEST PREFIX: ~a" longest-suffix)
	  (format T
		  "~%  WITH SUFFIX ~a FROM ~a USING OFFSET ~a" 
		  (subseq word offset) word offset))
        (get-substrings (subseq word offset)
                        ending
                        lexicon
                        (mapcar #'(lambda (x) (- x offset)) nc-umlaut?)
                        umlaut?
                        :leaf-name leaf-name
                        :exhaustive exhaustive
                        :one-composition (cons longest-suffix one-composition))
          (when exhaustive
            ;; if exhaustive flag is set then
            ;; call for all next prefix inside lookup
            ;; the function get-substrings recursively
            (loop for next-smaller-suffix in rest-suffixes
                do
                  (when *composita-trace*
                    (format T
			    "~%      NEXT SMALLER PREFIX: ~a" next-smaller-suffix)
                    (format T
			    "~%      WITH SUFFIX ~a FROM ~a USING OFFSET ~a" 
                            (subseq word (first next-smaller-suffix)) word 
			    (first next-smaller-suffix)))
                  (when
                       (< (first next-smaller-suffix) word-length)
                    (get-substrings 
                     (subseq word (first next-smaller-suffix))
                     ending
                     lexicon
                     (mapcar #'(lambda (x) (- x (first next-smaller-suffix))) nc-umlaut?)
                     umlaut?
                     :leaf-name leaf-name
                     :exhaustive exhaustive
                     :one-composition  (cons next-smaller-suffix one-composition)))))))))



;;; this function takes as input a word and determines all
;;; possible prefixes found in the lexicon
;;; they be returned sorted by their length
;;; delimeters starting a word will be read off

;;; it is a re-formulation of the function dtree::get-prefix-entries

(defun get-prefix-entries (word lexicon nc-umlaut?-out umlaut? 
			   &key (leaf-name :info))
  (when *composita-trace*
    (format T "~%  GET ALL PREFIXES FOR: ~a ~a ~a" word nc-umlaut?-out umlaut?))
  (let* ((w-length (length word))
         (prefix-entries nil)
         (nc-umlaut? (when nc-umlaut?-out 
		       (remove-if-not #'plusp nc-umlaut?-out))))
    (and (plusp w-length)
	 (do ((n 0 (1+ n))
	      (end-length (- w-length 1))
	      (rest-trie lexicon))
	     ((> n end-length)
              ;; if end of word as been reached check whether current
              ;; node in rest-trie has an leaf-name and stem entry
              ;; if so add it to prefix-entries
              ;; then return prefix-entries
              (let* ((leaf-value (rest 
                                  (dtree::get-leaf-info leaf-name rest-trie)))
                     (nc-val (if nc-umlaut?
                                   (if (< n (car nc-umlaut?)) NIL T)))
                     (stem-val (and leaf-value 
				    ;; get ride of this fucking proper-name-entries
                                    ;;(ignore-proper-name-p rest-trie)
                                    (if (not (and nc-val (not umlaut?)))
                                        (proper-nc-pos leaf-value nc-val))
                                    (subseq word 0 n))))
		(when *composita-trace*
		  (format T "~%  POSSIBLE STEM FINAL? ~a ~a ~a ~a ~a" 
			  leaf-value nc-val stem-val 
			  (ignore-proper-name-p rest-trie)
			  (subseq word 0 n)))
                (when (and leaf-value stem-val 
			   (>= (length stem-val) %minimal-noun-length%))
                           
                       (when *composita-trace*
                         (format T "~%    FOUND LONGEST PREFIX: ~a ~a ~a WITH NC-UMLAUT-FLAG SET TO ~a" 
                                 stem-val n nc-umlaut? nc-val))
                       (push (cons n (cons stem-val leaf-value)) 
			     prefix-entries))
                prefix-entries))
           
           (let* ((leaf-value  (rest (dtree::get-leaf-info  
				      leaf-name rest-trie)))
                  (nc-val (if nc-umlaut?
                                   (if (< n (car nc-umlaut?)) NIL T)))
                  (stem-val (and leaf-value
                                 ;; get ride of this fucking proper-name-entries
                                 (ignore-proper-name-p rest-trie)
                                 ;; as long as nc-umlaut? problems i use NIL
                                 (proper-nc-pos leaf-value nc-val)
                                 (subseq word 0 n))))
	     
             (when *composita-trace*
		  (format T "~%  POSSIBLE STEM? ~a ~a ~a ~a ~a" 
			  leaf-value nc-val stem-val 
			  (ignore-proper-name-p rest-trie)
			  (subseq word 0 n)))
             
             (when (and leaf-value stem-val 
			(>= (length stem-val) %minimal-noun-length%))
               (when *composita-trace*
                 (format T "~%    FOUND NEXT PREFIX: ~a ~a ~a WITH NC-UMLAUT-FLAG SET TO ~a" 
                         stem-val n nc-umlaut? nc-val))
               
               (unless nc-val 
		 (push (cons n (cons stem-val leaf-value)) prefix-entries))
               
               (when (> (- w-length n) %minimal-noun-length%)
                   ;; only when the prefix of word beginning at n is greater then
                   ;; %minimal-noun-length% perform fugenlaut computation
                 
                 (let ((separated-form 
			(separate-fugenlaut stem-val leaf-value word n nc-val)))
                   (when *composita-trace*
                     (format T "~%       FUGENLAUT-SEPARTED: ~a" 
			     separated-form))
                   (when separated-form
                     ;; if a fugenlaut could be found than
                     ;; the result will be a correct lookup reading
		     (setq prefix-entries (append 
					   separated-form 
					   prefix-entries))))))
             
             (cond ((not (setq rest-trie
                           (car (dtree::find-daughter (elt word n) rest-trie 
                                                      #'char=  
                                                      #'dtree::node-index))))
                    (return prefix-entries))))))))



;;; test that if umlaut has been reduced for compound whether this is lexically be allowed
;;; never used!

(defun proper-nc-pos (leaf-entry nc-umlaut?)
  (let ((lex-entry (if (multiple-lex-entry-p leaf-entry) 
                       (rest leaf-entry) (list leaf-entry))))
    (loop for entry in lex-entry 
        for integer = (let ((maybe-integer (if (consp entry) (car entry) entry)))
                        (if (numberp maybe-integer) maybe-integer))
        when
          integer
        do
          (multiple-value-bind (cat-code rest)
              (the (values fixnum fixnum) (truncate integer 100000))
            (return
              (or (= 2 cat-code)
                  (= 3 cat-code)
                  (and
                   (= 1 cat-code)
                   (if nc-umlaut?
                       (multiple-value-bind (gender rest2345)
                           (the (values fixnum fixnum) (truncate rest 10000))
                         (multiple-value-bind (umlaut rest345)
                             (the (values fixnum fixnum) (truncate rest2345 1000))
                           (if (= 1 umlaut) T NIL)))
                     T))))))))

(defun inflect-composita-noun (one-composition ending umlaut? nc-umlaut?)
  (when *composita-trace*
    (format T "~%  INLFECT ONE-COMPOSITION WITH: ~w ~a ~a ~a" 
            one-composition ending umlaut? nc-umlaut?))
    (let* (;; first select the right-head stem
           (right-head (second (first one-composition)))
         ;; compute its full lexical entry form
           (lex-entry (when (or (rest one-composition)
                                *handle-unknowns*)
                                ;; the last expression is necessary, since 
                                ;; in the case a composita contains an unknown morphem
                                ;; one-composition can be a one-element list
                        (morphix::compute-stem-entry right-head)))
         ;; store length %composita-result% before it migth be changed
         ;; by verify segments
           (cnt-before (length %composita-result%))
         ;; then only keep noun readings of right head
           (noun-entry 
            (when lex-entry
              (loop for elem in (if (multiple-computed-entry-p lex-entry)
                                    (cdr lex-entry) (list lex-entry))
                                ;; GN on 17/6/97
                                ;; changed definition so that 
                                ;; all POS found in some global variable
                                ;; will be considered for NC processing
                                ;; if this variable is NIL
                                ;; then all POS will be considered
                  when 
                    (or (null *relevant-morphix-cats*)
                        (member (rest (assoc 'WORTART elem))
                                *relevant-morphix-cats* :test #'eq))
                  collect elem)))
         ;; and then run noun inflection
         
           (noun-result (when noun-entry
                          (verify-segment 
                           right-head ending right-head noun-entry umlaut? nil nil nil T)))
         
         ;; check whether it does compute a reading
         ;; this will be the case if the length of %composita-result% has been
         ;; increased
           (has-result (not (= (length %composita-result%) cnt-before))))
      (when *composita-trace*
        (format T "~%  ... YIELDS: ~a" has-result))
      (when has-result
        ;; if so build the external representation (changes are made destrucively)
        (build-noun-composita-result (rest one-composition))
        ;; and last but not least, return the result
        one-composition)))

(defun build-noun-composita-result (but-first-composition)
  (let* ((composita-string (mapcar #'second but-first-composition)))
    
    ;; after having collected all substrings
    ;; change morphix output destrucively
   ;;(format T "~% String: ~a " composita-string)
   ;;(format T "~%NC: ~a"  %composita-result%)
    (loop for elem in %composita-result%
        when (stringp (car elem))
             ;; this test is necessary since during exhaustive mode I already have
             ;; collected some composita readings
        do
          (setf (car elem) (reverse (cons (car elem) composita-string))))))

;;; ********************* SPECIAL FUNCTIONS FOR TREATING NC LINGUISTICS ***************

;;; Next things to do:

;;; - fugenlaute
;;; - filter: teilworte duerfen nur zur klasse nomen, adjektive und verb gehoeren (?!)
;;; - gleiche aufeinanderfolgende Konsonaten (schiffahrt vs. sauerstoffflasche)
;;; - elision in einem prefix-nomen (e.g., Farb-fleck)
;;; - beachte: kranken-haus vs. krank-meldung
;;; - Umlautung in einem prefixwort, e.g., Haeusermeere
;;; - bindestrich-komposita (e.g., Hof- und Haushund)
;;; - getrennte Komposita (Hof- \newline or \blank hund)
;;; - partiell bekannte Komposita (e.g., XXvertreter)

;;; behandlung fugenlaute:
;;; idee:
;;; - lexikon-unabhaengig
;;; - fugenlaute als liste
;;; - dann: erstmal schaun, ob aktuelles wort mit fugenlaut match, dann
;;;   nicht abtrennen
;;; - dann abtrennen und als alternative aufnehmen
;;;
;;; laut duden:

;;; fugenlaute: -es, -en, -s, -n, -er, -e, -ens, -ns
;;; dabei:  -es, -en, -s, -n: 10 -20 %
;;;         -er, -e: 1- 2%

;;; bei Verben: 
;;; - in der regel ohne fugenlaut 
;;; - sonst: -e
;;; Bei nomen:
;;; -e: nur bei nomen, deren plural ebenfalls so gebildet wird; 
;;;     wenn pluralform umlautet, dann auch im teilwort

;;; -er: nur bei nomen, deren plural ebenfalls so gebildet wird; 
;;;      wenn pluralform umlautet, dann auch im teilwort
;;; -(e)n
;;; -(e)s

;;; Adjektive:
;;; - in der Regel keine Fugenlaute

;;; vorgehen:

;;; zuerst schauen, ob gefundener stamm,
;;; im suffix mit fugenlaut match (longest matching)
;;; dann kein fugenlaut abspalten

;;; filter: verwende hierbei rechtschreiberegeln
;;; also: fuer verben nur -e probieren
;;; fuer nomen entsprechende fugenlaute testen

;;; fuer adjetive: erstmal ohne

;;; was tun: wenn lexikoneintrag ambig bezueglich nomen/verb/adjetiv ??



;;; unten: besser schauen, ob lexikon information plural-form generierung erlauben 
;;;        wuerde, wobei vorkommender fugenlaut als prediction fuer die endung gilt

;;; determination of fugenlaut:
;;; - check lexical entry of stem using information found under leaf-name
;;; - distinguish further processing on the basis of pos
;;; - use current position n in word for performing some
;;;   lookahead
;;; - handling of verbs:
;;;   - check whether lookahead(1) yields %verb-fugenlaut%
;;;   - if true then read over lookahead and call
;;;   - get-prefix-entries recursively
;;;   - use additional graphematical rules to direct the spitting of 
;;;     fugen-laut


(defun separate-fugenlaut (stem leaf-entry word n nc-umlaut? &aux res)
  (let ((lex-entry (if (multiple-lex-entry-p leaf-entry) 
                       (rest leaf-entry) (list leaf-entry))))
    ;; first update lex-entry structure
    ;; and loop through the lex-entries
    (loop for entry in lex-entry 
        for integer = (let ((maybe-integer (if (consp entry) (car entry) entry)))
                        (if (numberp maybe-integer) maybe-integer))
        when
          integer
        do
          ;; only if integer is an integer
       (multiple-value-bind (cat-code rest)
           (the (values fixnum fixnum) (truncate integer 100000))
         ;; extract cat-code
         (case cat-code
           ;; branch by means of cat-code
           (1 (let ((noun-fugenlaut-offset
                     (separate-noun-fugenlaut stem rest word n nc-umlaut?)))
                ;; if noun fugenlaut has been separated then
                ;; make new entry for composition used caller function
                (if noun-fugenlaut-offset
                    (push (cons (+ n (car noun-fugenlaut-offset))
                                (cons (rest noun-fugenlaut-offset)  entry))
                          res))))
           
           (2 (unless
                  nc-umlaut?
                  (let ((verb-fugenlaut-offset
                     (separate-verb-fugenlaut stem rest word n)))
                ;; the same for verbs
                (if verb-fugenlaut-offset
                    (push (cons (+ n (car verb-fugenlaut-offset))
                                (cons (rest verb-fugenlaut-offset)  entry))
                          res)))))
           ;; for all other cats do nothing
           (otherwise nil))))
    res))

;;; ******************** VERB HANDLING ****************************************


;;; definition of verb fugen laut according to Duden, page: 452ff
;;; 80 - 90 % ohne fugenlaut

(defvar *verb-infix-sign* "")

(setq *verb-infix-sign* "-V-")

(defun separate-verb-fugenlaut (stem rest-entry word n)
  (when *composita-trace*
    (format T "~%      SEPARATE-VERB-FUGENLAUT WITH: ~a ~a ~a ~a" stem rest-entry word n))
  (let* ((verb-fugen-laut-p (char= (char word n) #\e))
         (left-char (char word (1- n)))
         (right-char (char word (1+ n))))
    
    (if verb-fugen-laut-p
        ;; only if word at position N has fugenlaut E
        (if (member left-char '(#\l #\r) :test #'char=)
            ;; eigentlich fuer mehrsilbige verbstaemme
          NIL
        (if (member left-char '(#\g #\b #\s #\z) :test #'char=)
            (cons 1 (concatenate 'string stem *verb-infix-sign* "E"))
          (if (and (member left-char '(#\d) :test #'char=)
                   (member right-char '(#\a #\u #\o #\i #\m #\p #\r #\t #\g #\k #\s #\z #\w) 
                           :test #'char=))
              (cons 1 (concatenate 'string stem *verb-infix-sign* "E"))
            (if (and (member left-char '(#\t) :test #'char=)
                     (member right-char '(#\t #\s #\z) :test #'char=))
                (cons 1 (concatenate 'string stem *verb-infix-sign* "E")) )))))))


;;; ******************** NOUN HANDLING ****************************************

;;; definition of noun fugen laut according to Duden, page: 452ff
;;; nach duden: 2/3 ohne fugenlaut
;;; 10 - 20 % mit (e)s oder (e)n
;;; 1 - 2% er oder e

;;; from Duden: page 455

;;; these are used to allow -S- even if no proper gen form exists, e.g.
;;; geburtShaus

(defvar *noun-composita-ending* nil)

(setq *noun-composita-ending* (dtree::make-dtree))

(dtree::insert-entry (reverse "at") '(MAS NTR FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "um") '(MAS NTR) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "bahnhof") '(MAS NTR) *noun-composita-ending* :leaf-name :compound)        
(dtree::insert-entry (reverse "bischof") '(MAS NTR) *noun-composita-ending* :leaf-name :compound)     
(dtree::insert-entry (reverse "friedhof") '(MAS NTR) *noun-composita-ending* :leaf-name :compound)    
(dtree::insert-entry (reverse "leumund") '(MAS NTR) *noun-composita-ending* :leaf-name :compound)    
(dtree::insert-entry (reverse "maulwurf") '(MAS NTR) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "taen") '(MAS NTR FEM) *noun-composita-ending* :leaf-name :compound)

(dtree::insert-entry (reverse "ut") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "geburt") '(FEM) *noun-composita-ending* :leaf-name :compound)


(dtree::insert-entry (reverse "heit") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "keit") '(FEM) *noun-composita-ending* :leaf-name :compound)    
(dtree::insert-entry (reverse "schaft") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "ung") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "ion") '(FEM) *noun-composita-ending* :leaf-name :compound)  
(dtree::insert-entry (reverse "itaet") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "zucht") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "sicht") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "nacht") '(FEM) *noun-composita-ending* :leaf-name :compound)    
(dtree::insert-entry (reverse "sucht") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "macht") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "furcht") '(FEM) *noun-composita-ending* :leaf-name :compound)  
(dtree::insert-entry (reverse "flucht") '(FEM) *noun-composita-ending* :leaf-name :compound)
(dtree::insert-entry (reverse "fahrt") '(FEM) *noun-composita-ending* :leaf-name :compound)

(defvar *noun-gen-sign* "")
(setq *noun-gen-sign* "-G-")

(defvar *noun-pl-sign* "")
(setq *noun-pl-sign* "-PL-")

(defvar *noun-S-sign* "")
(setq *noun-S-sign* "S")

(defun separate-noun-fugenlaut (stem entry word n umlaut?)
  (when *composita-trace*
    (format T
            "~%       SEPARATE-NOUN-FUGENLAUT: ~a ~a ~a ~a ~a (<- nc-umlaut)" 
            stem entry word n umlaut?))
  
  (let* ((stem-length (length stem))
         (last-stem-char (char stem (- stem-length 1)))
         (last-butlast-stem-char (char stem (- stem-length 2))))
    (if (dtree::get-prefix-entries (reverse stem) *noun-composita-ending* :leaf-name :compound)
        (if (char= (char word n) #\s)
            (cons 1 (concatenate 'string stem *noun-gen-sign* *noun-S-sign*))) 
          ;; next as else case check umlaut , e.g., eigenschaftENwort
      (if (char= last-stem-char #\e)
          (if (char= (char word n) #\n)
              (if (char= (char word (1+ n)) #\s)
                  (or 
                   (check-compound-noun-form stem "ns" entry umlaut? :gen) ;; check genitiv
                   (check-compound-noun-form stem "n" entry umlaut? :pl)) ;; check plural
                (check-compound-noun-form stem "n" entry umlaut? :pl));; check plural
            (if (char= (char word n) #\s)
                (check-compound-noun-form stem "s" entry umlaut? :gen))) ;; check genitiv
        (if (and (char= last-butlast-stem-char #\e)
                 (or (char= last-stem-char #\r)
                     (char= last-stem-char #\n)))
            (if (char= (char word n) #\s)
                (check-compound-noun-form stem "s" entry umlaut? :gen)
              (check-compound-noun-form stem "" entry umlaut? :pl))
          (if (char= (char word n) #\e)
              (if (char= (char word (1+ n)) #\n)
                  (if (char= (char word (+ 2 n)) #\s)
                      (check-compound-noun-form stem "ens" entry umlaut? :gen) ;; check genitiv
                    (or (check-compound-noun-form stem "en" entry umlaut? :pl)
                        (check-compound-noun-form stem "e" entry umlaut? :pl))) ;; check plural
                (if (char= (char word (1+ n)) #\r)
                    (check-compound-noun-form stem "er" entry umlaut? :pl) ;; check plural
                  (if (char= (char word (1+ n)) #\s)
                      (or (check-compound-noun-form stem "es" entry umlaut? :gen)
			  (check-compound-noun-form stem "s" entry umlaut? :gen)) ;; check gen
                    (check-compound-noun-form stem "e" entry umlaut? :pl))))
            (if (char= (char word n) #\s) 
                (check-compound-noun-form stem "s" entry umlaut? :gen))) ;; check genitiv, e.g., betriebSversammlung
          )))))

;;; hierix: verwende plural und genitiv spezifische funktionen
;;; passe funktion handle nouns an


;;; hier: kasus information abchecken!!
(defun test-plural-for-nc (lexical-entry suffix umlaut-flag)
  (let* ((subtree (get-suffix-information suffix))
         (plural-path (list 'nomen
                            (if umlaut-flag 'red+ 'red-)
                            (if (assoc 'uml lexical-entry) 'uml+ 'uml-)
                            'pl (cdr (assoc 'kpl lexical-entry))))
         (coded-case-list (lookup-tree plural-path subtree))
         (encoded-case-list (when coded-case-list
                              (aref %deklin-tab% (car coded-case-list)))))
         
    (when *composita-trace*
      (format T "~%       PL-PATH: ~S WITH CASE ~a" 
              plural-path encoded-case-list))
    (when
        encoded-case-list
        (member 'NOM (car encoded-case-list) :test #'eq))))



(defun check-compound-noun-form (stem ending rest-entry umlaut? what)
  (let* ((noun-entry (list (decode-noun-entry rest-entry)))
         (verified (case what
                     (:pl (test-plural-for-nc (car noun-entry) ending umlaut?))
                     (:gen (verify-segment stem ending stem noun-entry 
                                           umlaut? nil nil nil T T)))))
    (when *composita-trace*
      (format T "~%       CHECKED NOUN COMPOSITA for ~w: ~a ~a ~w ~a " 
              what stem ending noun-entry umlaut?)
      (format T "~%       RESULT IS ~a"  (if verified T)))       
    
    (if verified
        (cons (length ending) (concatenate 'string stem (case what
                                                          (:pl *noun-pl-sign*)
                                                          (:gen *noun-gen-sign*))
                                           (string-upcase ending))))))
