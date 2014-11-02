;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: main.lisp
;;;      module: morphix
;;;     version: 4.0
;;;  written by: Guenter Neumann (Copyrights)
;;; last update: 27.1.96
;;;  updated by: Guenter Neumann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;; 

(in-package :morphix)

; ---------------------------------------------------------------------------
;;; functions to access elements in the inflectional allomorph lexicon

#+:SYMBOLICS(proclaim '(optimize (safety 0)
				 (space 0)
				 (compilation-speed 0)
				 (speed 3)))

(defun lookup-tree (list tree)
  (declare (optimize speed))
  (loop (cond ((or (null list) (null tree)) (return tree))
	      ((or (eql (caar tree) (car list))
		   (and (consp (caar tree))
			(member (car list) (caar tree))))
	       (setq list (cdr list) tree (cdar tree)))
	      (t (setq tree (cdr tree))))))


#+:SYMBOLICS(proclaim '(inline lookup-tree))
#+:ALLEGRO(declaim (inline lookup-tree))

(defun equal-lookup-tree (list tree)
  (declare (optimize speed))
  (loop (cond ((or (null list) (null tree)) (return tree))
	      ((or (equal (caar tree) (car list))
		   (and (consp (caar tree))
			(member (car list) (caar tree)
				:test #'equal)))
	       (setq list (cdr list) tree (cdar tree)))
	      (t (setq tree (cdr tree))))))

(defun look-for-one-arc (pattern tree)
  (declare (optimize speed))
  (dolist (node tree)
     (if (or (eql (car node) pattern)
             (and (consp (car node)) (member pattern (car node))))
         (return (cdr node)))))

; ---------------------------------------------------------------------------
;;; some basic utilities

(defun list-not-nil (&rest lists)
  ;; there should be no need for a reverse of the result of list-not-nil 
  ;; because an assoc-list is built in the environment where this function is 
  ;; called
  ;; -> the expression         (list-not-nil elem1 elem2 ...)
  ;; evaluates faster than     (delete nil (list elem1 elem2...))
  ;;                        or (remove nil (list elem1 elem2 ...)) 
  ;;                        or (delete-if #'null (list elem1 elem2 ...))  .
  (do* ((rest lists (cdr rest))
	(e (car rest) (car rest))
	(r nil))
       ((null rest) r)
    (if e (setq r (cons e r)))))

#+:SYMBOLICS(proclaim '(inline list-not-nil))

(defun my-destructive-delete (item list)
  (cond ((eql item (car list))
	 (rplaca list (cadr list))
	 (rplacd list (cddr list)))
	(t (delete item list)))
  list)

(defun low-concat (string1 string2 start1 end1 start2 end2)
   ;; a low-level function for concatenation of parts of two
   ;; strings without having to copy these part-strings before
   ;; concatenation.
  (let ((new-string (make-string (+ (- end1 start1) (- end2 start2)))))
    (do ((n 0 (1+ n))
	 (m start1 (1+ m)))
	((= m end1)
	 (do ((x n (1+ x))
	      (m start2 (1+ m)))
	     ((= m end2) new-string)
	   (setf (aref new-string x) (aref string2 m))))
      (setf (aref new-string n) (aref string1 m)))))

(defun insert-e (string position length)
  (let ((new-string (make-string (1+ length))))
    (do ((n 0 (1+ n)))
        ((= n position)
         (setf (aref new-string n) #\e)
         (do ((m position (1+ m)))
             ((= m length) new-string)
           (setf (aref new-string (1+ m)) (aref string m))))
      (setf (aref new-string n) (aref string n)))))

;;;---------------------------------------------------------------------------
;;; Lexicon stuff

;;; Here: I directly added dtree calls for lexicon functions
(defun init-lexica ()
  (setq *stem-lexicon* (dtree::make-dtree))
  (setq *fullform-lexicon* (dtree::make-dtree)))

(init-lexica)


(defun lexicon-insert (key entry lexicon)
  (dtree::insert-entry key entry lexicon :test #'char=))

#-:SYMBOLICS
(defun augmentation (old-value exists-p entry)
  (cond (exists-p
	 (cond ((multiple-lex-entry-p old-value)
		(cond ((member entry (cdr old-value) :test #'equal)
		       old-value)
		      (t (rplacd old-value
				 (cons entry (cdr old-value))))))
	       ((equal entry old-value) old-value)
	       (t (form-multiple-entry entry old-value))))
	(t entry)))

(defun get-lex-entry (key lexicon)
  (dtree::get-entry key lexicon :test #'char=))

#+:ALLEGRO(declaim (inline get-lex-entry))

(defun delete-entry (key lexicon)
  (dtree::delete-entry key lexicon :test #'char=))

(defun get-gender (noun-stem)
  (cdr (assoc 'genus (find-associate-entry
		       (string-downcase noun-stem)
		       'nomen))))
(setf dtree::%multi-entry% 'MULTIPLE)

; ---------------------------------------------------------------------------
;;; decoding of lexical entries!

(defun compute-stem-entry (key)
  (let ((lex-entry (get-lex-entry key *stem-lexicon*)))
    (if lex-entry
	(cond ((encoded-entry lex-entry)
	       (stem-entry-branching lex-entry))
	      ((encoded-entry (car lex-entry))
	       ;; a simple lex-entry with additional-information
	       (nconc (stem-entry-branching (car lex-entry))
		      (cdr lex-entry)))
	      ((multiple-lex-entry-p lex-entry)
	       ;; a list of some entries
	       (do* ((rest (cdr lex-entry) (cdr rest))
		     (entry (car rest) (car rest))
		     (result nil))
		    ((null rest) (cons 'multiple result))
		 (push (cond ((encoded-entry entry)
			      (stem-entry-branching entry))
			     ((encoded-entry (car entry))
			      (nconc (stem-entry-branching
				       (car entry))
				     (cdr entry)))
			     (t entry))
		       result)))
	      (t lex-entry)))))

;;; added by GN: 4/7/95: call compute-stem-entry for given entry

(defun expand-morph-stem-entry (lex-entry)
  (cond ((encoded-entry lex-entry)
         (stem-entry-branching lex-entry))
        ((encoded-entry (car lex-entry))
         ;; a simple lex-entry with additional-information
         (nconc (stem-entry-branching (car lex-entry))
                (cdr lex-entry)))
        ((multiple-lex-entry-p lex-entry)
         ;; a list of some entries
         (do* ((rest (cdr lex-entry) (cdr rest))
               (entry (car rest) (car rest))
               (result nil))
             ((null rest) (cons 'multiple result))
           (push (cond ((encoded-entry entry)
                        (stem-entry-branching entry))
                       ((encoded-entry (car entry))
                        (nconc (stem-entry-branching
                                (car entry))
                               (cdr entry)))
                       (t entry))
                 result)))
        (t lex-entry)))


(defun compute-fullform-entry (key)
  (let ((lex-entry (get-lex-entry key *fullform-lexicon*)))
    (and lex-entry
	 (cond ((encoded-entry lex-entry)
		(fullform-entry-branching lex-entry))
	       ((encoded-entry (car lex-entry))
		(nconc
		 (fullform-entry-branching (car lex-entry))
		 (cdr lex-entry)))
	       ((stringp (car lex-entry))
		(cons (car lex-entry)
		      (cond ((encoded-entry (cadr lex-entry))
			     (nconc
			      (fullform-entry-branching
			       (cadr lex-entry))
			      (cddr lex-entry))))))
	       ((multiple-lex-entry-p lex-entry)
		;; a list of some entries
		(do* ((rest-entries (cdr lex-entry) (cdr rest-entries))
		      (entry (car rest-entries) (car rest-entries))
		      (result nil))
		     ((null rest-entries) (cons 'MULTIPLE result))
		     (push (cond ((encoded-entry entry)
				  (fullform-entry-branching entry))
				 ((encoded-entry (car entry))
				  (nconc
				   (fullform-entry-branching (car entry))
				   (cdr entry)))
				 ((stringp (car entry))
				  (cons (car entry)
					(cond ((encoded-entry (cadr entry))
					       (nconc
						(fullform-entry-branching
						 (cadr entry))
						(cddr entry))))))
				 (t entry))
			   result)))
	       (t lex-entry)))))  

;;; added by GN: 4/7/95: call compute-fullform-entry for given entry

(defun expand-morph-fullform-entry (lex-entry)
  (cond ((encoded-entry lex-entry)
		(fullform-entry-branching lex-entry))
	       ((encoded-entry (car lex-entry))
		(nconc
		 (fullform-entry-branching (car lex-entry))
		 (cdr lex-entry)))
	       ((stringp (car lex-entry))
		(cons (car lex-entry)
		      (cond ((encoded-entry (cadr lex-entry))
			     (nconc
			      (fullform-entry-branching
			       (cadr lex-entry))
			      (cddr lex-entry))))))
	       ((multiple-lex-entry-p lex-entry)
		;; a list of some entries
		(do* ((rest-entries (cdr lex-entry) (cdr rest-entries))
		      (entry (car rest-entries) (car rest-entries))
		      (result nil))
		     ((null rest-entries) (cons 'MULTIPLE result))
		     (push (cond ((encoded-entry entry)
				  (fullform-entry-branching entry))
				 ((encoded-entry (car entry))
				  (nconc
				   (fullform-entry-branching (car entry))
				   (cdr entry)))
				 ((stringp (car entry))
				  (cons (car entry)
					(cond ((encoded-entry (cadr entry))
					       (nconc
						(fullform-entry-branching
						 (cadr entry))
						(cddr entry))))))
				 (t entry))
			   result)))
	       (t lex-entry)))  


(defun stem-entry-branching (integer)
  (multiple-value-bind (cat-code rest)
      (the (values fixnum fixnum) (truncate integer 100000))
    (case cat-code
      (1 (decode-noun-entry rest))
      (2 (decode-verb-entry rest))
      (3 (decode-adjective-entry rest))
      (4 (decode-possessive-entry rest))
      (12 (decode-determinativ-entry integer))
      (18 (decode-determinativ-indef integer))
      (11 '((wortart . relativpronomen)))
      (19 (decode-card-entry integer)))))

#+:SYMBOLICS(proclaim '(notinline stem-entry-branching))

(defun fullform-entry-branching (integer)
  (multiple-value-bind (cat-code rest)
      (the (values fixnum fixnum) (truncate integer 100000))
    (cond ((or (<= 5 cat-code 17) (= cat-code 20))
	   (decode-fullform-entry rest cat-code)))))

(defun decode-noun-entry (fivedigits)
  (multiple-value-bind (gender rest2345)
      (the (values fixnum fixnum) (truncate fivedigits 10000))
    (multiple-value-bind (umlaut rest345)
	(the (values fixnum fixnum) (truncate rest2345 1000))
      (multiple-value-bind (sg pl)
	  (the (values fixnum fixnum) (truncate rest345 100))
	(the list
	     (list-not-nil (aref %pl-entry% pl)
			   (aref %sg-entry% sg)
			   (if (= 1 umlaut) %umlaut-entry%)
			   (aref %gender-entry% gender)
			   %noun-category%))))))

(defun decode-verb-entry (fivedigits)
  (multiple-value-bind (vorne12 rest345)
      (the (values fixnum fixnum) (truncate fivedigits 1000))
    (multiple-value-bind (subsegment vtype)
	(the (values fixnum fixnum) (truncate vorne12 12))
      (nconc 
	(list-not-nil (aref %segment-entry% subsegment)
		      (aref %vtype-entry% vtype)
		      %verb-category%)
	(multiple-value-bind (aux&strip rest45)
	    (the (values fixnum fixnum) (truncate rest345 100))
	  (cond ((plusp rest45)
		 (case aux&strip
		   (0 (list (aref %paux-entry% 0) (list 'vrbz rest45)))
		   (1 (list (aref %paux-entry% 1) (list 'vrbz rest45)))
		   (2 (list (aref %paux-entry% 0)
			    (cons 'vrbz (cons rest45 t))))
		   (3 (list (aref %paux-entry% 1)
			    (cons 'vrbz (cons rest45 t))))))
		(t (list (aref %paux-entry% (rem aux&strip 2))))))))))


(defun decode-adjective-entry (fivedigits)
  (multiple-value-bind (v2 r3456)
      (the (values fixnum fixnum) (truncate fivedigits 10000))
    (multiple-value-bind (umlaut r456)
	(the (values fixnum fixnum) (truncate r3456 1000))
      (multiple-value-bind (elision r56)
	  (the (values fixnum fixnum) (truncate r456 100))
	(multiple-value-bind (pred-e main)
	    (the (values fixnum fixnum) (truncate r56 10))
	  (list-not-nil (aref %adj-main-entry% main)
		        (if (= 1 pred-e) %pred-e-entry%)
			(aref %elision-entry% elision)
			(aref %umlaut-adj-entry% umlaut)
			(aref %komp-entry% v2)
			%adjective-category%))))))
	
(defun decode-possessive-entry (fixnum)
  (list-not-nil
    (aref %elision-entry% (the fixnum (truncate fixnum 10000)))
    %possessive-category%))

(defun decode-determinativ-entry (fixnum)
  (list %determinativ-category%
	(aref %det-number% (the fixnum (rem fixnum 10)))))

(defun decode-determinativ-indef (fixnum)
  (list %determinativ-indef-category%
	(aref %det-number% (the fixnum (rem fixnum 10)))))

(defun decode-card-entry (fixnum)
  (list-not-nil
   (aref %card-entry% (the fixnum (rem fixnum 10)))
   %card-category%))

(defun decode-fullform-entry (fivedigits category-code)
  (multiple-value-bind (v r3456)
      (the (values fixnum fixnum) (truncate fivedigits 10000))
    (case category-code
      (10			     ;; praeposition
	(let ((result (aref %prep-cases%
			    (the fixnum (truncate r3456 1000)))))
	  (list-not-nil (if result (list 'folgekasus result))
			(if (= 1 v) %additional-algorithmic%)
			(aref %fullform-categories%
			      (- category-code 5)))))
      (otherwise
	(list-not-nil (if (= 1 v) %additional-algorithmic%)
		      (aref %fullform-categories%
			    (- category-code 5)))))))

(defun find-associate-entry (stem searched-category)
  (let ((lex-entry (compute-stem-entry stem)))
    (and lex-entry
         (cond ((consp searched-category)
                (if (multiple-computed-entry-p lex-entry)
                    (dolist (entry (cdr lex-entry))
                        (if (member (cdr (assoc 'wortart entry))
                                     searched-category)
                            (return entry)))
                    (if (member (cdr (assoc 'wortart lex-entry))
                                searched-category)
                        lex-entry)))
                ((multiple-computed-entry-p lex-entry)
                 (dolist (entry (cdr lex-entry))
                     (if (eq (cdr (assoc 'wortart entry)) 
                             searched-category)
                         (return entry))))
                ((eq (cdr (assoc 'wortart lex-entry)) searched-category)
                 lex-entry)))))

; ---------------------------------------------------------------------------
;;; MORPHIX: ANALYSIS

; ---------------------------------------------------------------------------
;;; THE SEGMENTATION OF WORDS

(defvar %leaf-name% 'INFO
  "the name of the node which precedes a leaf in the *prefix-lexicon*")

(defun augment-minimal-stem (word-as-string word-length partition-list)
  (let ((result nil))
    (dolist (suffix partition-list (cons (list nil word-as-string nil) result))
      (push (list nil 
		  (subseq word-as-string 0 (- word-length (length suffix)))
		  suffix)
	    result))))

(defun stem+suffix (word w-l)
  (let ((max-suffix (get-max-suffix word w-l)))
    (cond ((null max-suffix)
	   (list (list nil word nil)))
	  (t (augment-minimal-stem word w-l max-suffix)))))


(defun get-max-suffix (w w_l)
  ;; w is word-as-string
  ;; w_l is its length
  (let ((actual nil) (rest *ial*))
    (do ((i (1- w_l) (1- i)))
	((< i %minimal-stem-length%) actual)
      ;; no further tests because minimal-stem-length is reached
      (cond ((setq rest (cdr (assoc (elt w i) rest)))
	     (if (eq 'INFO (caar rest))
		 (setq actual (cdar rest))))
	    (t (return actual))))))
;; hier weiter einbauen

;;; handling of umlautung in German
;;; for noncompound
(defun test-for-umlaut (possible-stem)
  (let ((stem-length (length possible-stem)))
    (if (> stem-length 0)
	(do ((index (1- stem-length) (1- index)))
	    ((zerop index) nil)
	    (cond ((eql #\e (elt possible-stem index))
		   ;; found an "e" of a possible UMLAUT
		   (let ((pred-char (elt possible-stem (1- index))))
		     (cond ((or (member pred-char '(#\a #\o))
				(and (eql pred-char #\u)
				     (if (> index 1)
					 (not
					  (member (elt possible-stem
						       (- index 2))
						  '(#\a #\e)))
				       t)))
			    ;; 'umlautung' occurs in the following cases:
			    ;; "ae" "oe" "ue" but not in the case of
			    ;; 'fallender Diphtong' i.e. "aue" "eue"
			    (return index))))))))))

(defun reduce-umlaut (word &aux (e-position (test-for-umlaut word)))
  (if e-position
      (low-concat word word
		  0 e-position
		  (1+ e-position) (length word))
      word))

;;; handling of umlautung in German
;;; for compound 



;;; compute all possible umlaut-nonumlaut permutations

(defun reduce-compound-umlauts (word &optional (n 0) res n-list 
                                &aux (w-length (length word)))
  (cond ((< (- w-length n) %minimal-noun-length%)
         (remove-duplicates res :test #'equal :key #'first))
        ((not (test-compound-for-umlaut word n))
         (reduce-compound-umlauts word (1+ n) res n-list))
        (T
         (let ((wrd (low-concat word word 0 (1+ n) (+ 2 n) w-length)))
           (reduce-compound-umlauts 
            wrd (1+ n) 
            (reduce-compound-umlauts word (1+ n) 
                                     (cons (cons wrd  (cons n n-list))  
                                           (cons (cons word  n-list)
                                                 res))
                                     n-list)
            (cons n n-list))))))

;;; first, check whether compound has an umlaut at position n
;;; if so, delete it

(defun test-compound-for-umlaut (word n)  
  (or (and (member (char word n) '(#\a #\o) :test #'char=)
           (char= (char word (1+ n)) #\e))
      (and (char= (char word n) #\u)
           (char= (char word (1+ n)) #\e)
           (if (> n 1)
               (not (member (char word (1- n)) 
                            '(#\a #\o) :test #'char=))
             T))))

(defun test-for-prefix (possible-stem prefix &optional (start 0))
  (let ((sl (length possible-stem))
	(pl (length prefix)))
    (do ((n start (1+ n))
	 (m 0 (1+ m)))
	((= m pl) t)
	(cond ((or (>= n sl)
		   (not (eql (elt possible-stem n)
			     (elt prefix m))))
	       (return nil))))))

(defun reduce-segment-stem (segment-list &optional reduction-type)
  ;; reduction-type non-nil means "umlaut"
  (let ((result nil))
    (cond (reduction-type
	   (dolist (segment segment-list result)
	     (push (list nil
			 (reduce-umlaut (segment-stem segment))
			 (segment-suffix segment))
		   result)))
	  (t
           (dolist (segment segment-list result)
	     (let ((stem (segment-stem segment)))
	       (if (> (length stem) 3)
		   (push (list nil
			       (subseq stem 2)
			       (segment-suffix segment))
			 result))))))))

(defun insert-prefix (prefix &aux (p-length (length prefix)))
  (and (stringp prefix) (plusp p-length)
       (let ((trie (assoc (elt prefix 0) *prefix-lexicon*)))
	 (cond (trie (insert-rest-prefix prefix (cdr trie) p-length) t)
	       (t (push (do-trie prefix 0 p-length) *prefix-lexicon*) t)))))

(defun insert-rest-prefix (prefix trie length)
  (cond ((= length 1)
	 (if (assoc %leaf-name% trie) nil
	   (rplacd (last trie)
		   (list (cons %leaf-name% prefix)))))
	(t (do* ((n 1 (1+ n))
                 (end-length (1- length))
  	         (new-char (elt prefix n) (elt prefix n))
                 (orig-trie trie new-trie)
                 (new-trie (cdr (assoc new-char trie))
                           (cdr (assoc new-char new-trie))))
		((= n end-length) 
		 (cond ((null new-trie)
			(rplacd (last orig-trie)
				(list (do-trie prefix n length))))
		       ((not (assoc %leaf-name% new-trie))
			(rplacd (last new-trie)
				(list (cons %leaf-name% prefix))))))
		(cond ((null new-trie)
		       (return (rplacd (last orig-trie)
				       (list (do-trie prefix
						      n
						      length))))))))))

(defun do-trie (prefix stop length)
  (do* ((n length (1- n))
	(result (cons %leaf-name% prefix)
		(list (elt prefix n) result)))
       ((= n stop) result)))

(defun compute-prefix (word &aux (w-length (length word)))
  (and (stringp word) (plusp w-length)
       (do ((n 0 (1+ n)) (result nil) (tmp nil)
            (end-length (max 0 (- w-length %minimal-stem-length% 1)))
	    (rest-trie *prefix-lexicon*))
	   ((> n end-length) result)
	   
	   (cond ((setq rest-trie
			(cdr (assoc (elt word n) rest-trie)))
		  (if (setq tmp
			    (assoc %leaf-name% rest-trie))
		      (push (cdr tmp)
			    result)))
		 (t (return result))))))

(defun delete-prefix (prefix &aux (p-length (length prefix)))
  (and (stringp prefix) (plusp p-length)
       (let ((trie-distribution (get-prefix-path prefix p-length)))
	 (cond (trie-distribution
		;; this is a list indicating the various branching
		;; possibilities in the TRIE. As long as the elements in this
		;; list are 1, the subtrie can be deleted.
		(let ((position-to-cut
			(- (1+ p-length)  ;; the path under %leaf-name%
			   (or (position-if #'(lambda (number) (> number 1))
					    trie-distribution)
			       (1+ p-length)))))
		  (delete-subtrie prefix p-length position-to-cut)) t)))))

(defun prefix-exists-p (prefix)
  (cond ((stringp prefix)
	 (let ((p-length (length prefix)))
	   (cond ((plusp p-length)
		  (do* ((n 0 (1+ n))
			(subtrie (cdr (assoc (elt prefix n)
					     *prefix-lexicon*))
				 (cdr (assoc (elt prefix n)
					     subtrie)))
			(stop-test (1- p-length)))
		       ((= n stop-test);; prefix is complete reTRIEved
			;; now look for the %leaf-name% - node.
			(cond ((assoc %leaf-name% subtrie) t)))
			       ;; prefix is in the lexicon
		       (if (not subtrie)
			   (return nil))))
		 (t t))))
	(t (format t "PREFIX must be a STRING!"))))

(defun get-prefix-path (prefix p-length)
  (do* ((n 0 (1+ n))
	(brother-count (length *prefix-lexicon*)
		       (length subtrie))
	(subtrie (cdr (assoc (elt prefix n)
			     *prefix-lexicon*))
		 (cdr (assoc (elt prefix n)
			     subtrie)))
	(result nil))
       ((= n (1- p-length));; prefix is complete reTRIEved
	;; now look for the %leaf-name% - node.
	(cond ((assoc %leaf-name% subtrie);; prefix is in the lexicon
	       (cons (length subtrie)
		     (cons brother-count result)))
	      (t (format t " ~a not in the *prefix-lexicon*" prefix) nil)))
       
       (cond (subtrie (push brother-count result))
	     (t (format t " ~a not in the *prefix-lexicon*" prefix)
		(return nil)))))

(defun delete-subtrie (prefix p-length position-to-cut)
  (cond ((zerop position-to-cut)
	 ;; reset the *prefix-lexicon*
	 (setq *prefix-lexicon* nil))
	((= 1 position-to-cut)
	 (my-destructive-delete (assoc (elt prefix 0)
				       *prefix-lexicon*)
				*prefix-lexicon*))
	(t (do ((n 0 (1+ n))
		(subtrie *prefix-lexicon*))
	       ((cond ((= n (1- position-to-cut))
		       (my-destructive-delete
			(assoc
			 (if (= n p-length)
			     %leaf-name%
			     (elt prefix n))
			 subtrie)
			subtrie))))
	       (setq subtrie (cdr (assoc (elt prefix n) subtrie)))))))

(defun check-for-triple-consonants (prefix stem &optional (stem-start 0)
				    &aux (sl (length prefix)))
  (char= (elt prefix (1- sl))
	 (elt prefix (- sl 2))
	 (elt stem stem-start)))

(defun form-prefix-entry (prefix stem &optional (stem-start 0))
  (cond ((check-for-triple-consonants prefix stem stem-start)
	 (low-concat prefix stem
		     0 (length prefix)
		     (1+ stem-start) (length stem)))
	(t (low-concat prefix stem
		       0 (length prefix)
		       stem-start (length stem)))))

(defun augment-segments (possible-segments prefix
			 &optional what triple-char-p
			 &aux
			 (prefix-length (+ (length what)
					   (length prefix))))
  
  (do* ((rest-segments possible-segments (cdr rest-segments))
	(segment (car rest-segments) (car rest-segments))
	(segment-stem (segment-stem segment) (segment-stem segment))
	(result nil))
       ((null rest-segments) result)
    (cond ((>= (- (length segment-stem) prefix-length)
	       %minimal-stem-length%)
	   (push (list prefix
		       (if what
			   (subseq segment-stem prefix-length)
			   (if triple-char-p
			       (subseq segment-stem (1- prefix-length))
			       (subseq segment-stem prefix-length)))
		       (segment-suffix segment))
		 result)))))

; ---------------------------------------------------------------------------
;;; CATEGORY-SPECIFIC FUNCTIONS

(defun handle-verbs
       (input-word suffix key lex-entry segment-type sub-tree
		   ge-reduced category
	&optional prefix-separated prefix-lex-entry zu-reduced
	&aux
	(complex-suffix (complex-suffix-p sub-tree))
	(vtype (or (cdr (assoc 'vtyp lex-entry)) 1))
	(sub-segment-type
	  (if (member segment-type '(VGFA PPRF))
	      (cdr (assoc segment-type lex-entry))
	      (if (member segment-type '(SOND1 SOND2 SOND3 SOND4 VGFD))
		  ;; no subcategorization for these categories
		  nil
		  (cadr (assoc segment-type lex-entry))))))
  (cond
    (complex-suffix
     ;; the splitted suffix is a complex-suffix which means that
     ;; beside conjugation-information an attributive-used participle
     ;; can be the result. complex-suffix is the complex-suffix
     ;; which eventually identifies a verified participle as
     ;; an attributive-used one.
     (let ((first-suffix (car complex-suffix))
	   (rest-suffix (cadr complex-suffix)))
       ;; this part of the complex-suffix should be a suffix of a participle.
       ;; if a participle can be found with this suffix, an attributive
       ;; used participle is possible, else only conjugation-information
       (cond ((not (test-participle
		     ge-reduced input-word key lex-entry
		     first-suffix rest-suffix
		     sub-segment-type prefix-separated prefix-lex-entry))
	      ;; although there is a complex suffix, no participle could be
	      ;; verified -> test whether the input is a conjugated form
	      (form-result (handle-conjugation
			     vtype segment-type sub-segment-type
			     sub-tree ge-reduced prefix-lex-entry)
			   category key))
	     (t 
	      (build-verb-result
		key category
		(handle-inflected-participles rest-suffix)
		(handle-conjugation
		  vtype segment-type sub-segment-type
		  sub-tree ge-reduced prefix-lex-entry)
		(if (member first-suffix '("end" "nd") :test #'equal)
		    (if zu-reduced 'partizip-praesens-mit-zu
			'partizip-praesens)
		    'partizip-perfekt))))))
    
    ((look-for-one-arc 'partizip sub-tree)
     ;; if one of the pure participle-suffixes occurs
     ;; handle participles and conjugation
     
     (let ((participle-result
	     (test-participle
	       ge-reduced input-word key lex-entry suffix ""
	       sub-segment-type prefix-separated prefix-lex-entry)))
       (cond ((eq 'pprs participle-result)
	      ;; if a participle-present is found,
	      ;; the input-word can only be a participle
	      (form-result (if zu-reduced
			       (list (list 'partizip-praesens-mit-zu))
			       (list (list 'partizip-praesens)))
			   category key))
	     ((eq 'pprf participle-result)
	      (build-verb-result
		key category (list (list 'partizip-perfekt))
		(handle-conjugation
		  vtype segment-type sub-segment-type
		  sub-tree ge-reduced prefix-lex-entry)))
	     (t ;; no participle-result
	      (form-result (handle-conjugation
			     vtype segment-type sub-segment-type
			     sub-tree ge-reduced prefix-lex-entry)
			   category key)))))
    
    (t ;; no participle-suffix ==> only conjugation-information
     (form-result (handle-conjugation
		    vtype segment-type sub-segment-type
		    sub-tree ge-reduced prefix-lex-entry)
		  category key))))

(defun build-verb-result (stem category participle conjugation
			       &optional attributive-p)
  (cond ((not participle);; there exists only a conjugation-result
	 (form-result conjugation category stem))
	(attributive-p
	 (cond ((null conjugation)
		(list stem
		      (list 'wortart 'attributiv-gebrauchtes attributive-p)
		      (list 'flexion participle)))
	       (t
		(push (additional-keys
		       (list stem (list 'wortart
					'attributiv-gebrauchtes
					attributive-p)
			     (list 'flexion participle)))
		      %word-result%)
		(form-result conjugation category stem))))
	((null conjugation) (form-result participle category stem))
	(t (form-result (cons (car participle) conjugation) category stem))))

(defun handle-conjugation
       (verb-type segment-type sub-segment-type sub-tree
	ge-reduced prefix-lex-entry
	&aux (strip? (and prefix-lex-entry
			  (vrbz-strip-p (assoc 'vrbz prefix-lex-entry)))))
  (cond (ge-reduced nil)
	(strip?
	 (filter-imperativ
	   ;; in order to get not imperativ as result for prefix-verbs
	   ;; which have a separable prefix.
	   (if sub-segment-type
	       (lookup-tree
		 (list 'vtyp verb-type segment-type sub-segment-type)
		 sub-tree)
	       (lookup-tree (list 'vtyp verb-type segment-type)
			    sub-tree))))
	(sub-segment-type
	 (lookup-tree
	   (list 'vtyp verb-type segment-type sub-segment-type) sub-tree))
	(t (lookup-tree (list 'vtyp verb-type segment-type) sub-tree))))

(defun filter-imperativ (inflectional-result)
  (cond ((null inflectional-result) nil)
	((eq (caar inflectional-result) 'imperativ)
	 (filter-imperativ (cdr inflectional-result)))
	(t (cons (car inflectional-result)
		 (filter-imperativ (cdr inflectional-result))))))

(defun test-participle (ge-reduced input-word key-of-vgfa lex-entry
				   left-part-of-suffix
				   &optional (right-part-of-suffix "")
				   sub-segment-type prefix-reduced 
				   prefix-lex-entry)
  
  (cond ((member left-part-of-suffix '("end" "nd") :test #'equal)
	 ;; check for participle-present
	 (cond (ge-reduced nil)
	       ((and (string-equal key-of-vgfa input-word
				   :end2 (length key-of-vgfa))
		     (caar (lookup-tree
			    (list 'verb 'partizip 'vgfa sub-segment-type)
			    (get-suffix-information
			      left-part-of-suffix)))))))
	
	((member left-part-of-suffix '("en" "et" "t" "n") :test #'equal)
	 ;; only lookup of a participle-form
	 ;; in the lex-entry to check for a participle-perfect
	 (let ((pprf-component
		(cdr (assoc 'pprf (if prefix-reduced
				      prefix-lex-entry lex-entry))))
	       (check-string-position
		(- (length input-word) (length right-part-of-suffix))))
	   
	   (cond ((stringp pprf-component)
		  (cond ((string-equal input-word pprf-component
				       :end1 check-string-position)
			 'pprf)))
		 ((and (listp pprf-component)
		       (do ((rest-pprf-info pprf-component
					    (cdr rest-pprf-info)))
			   ((null rest-pprf-info) nil)
			   (if (string-equal input-word
					     (cdar rest-pprf-info)
					     :end1 check-string-position)
			       (return t))))
		  'pprf))))))

(defun handle-prefix-infinitives (category-entry verb-stem-with-prefix
				  segment subtree prefix-separated)
  
  (cond ((not (eq segment 'vgfa)) nil)
	(t (let* ((lex-entry
		    (find-associate-entry verb-stem-with-prefix 'verb))
		  (vrbz-entry (assoc 'vrbz lex-entry)))
	     (add-selector-keys lex-entry)
	     (cond ((or (not lex-entry)
			(not (vrbz-strip-p vrbz-entry))
			(not (eql (length-of-prefix vrbz-entry)
				  (length prefix-separated)))
			(not (equal prefix-separated 
				    (subseq verb-stem-with-prefix 0 
					    (length-of-prefix vrbz-entry)))))
		    nil)

		   ((and (not (assoc 'vgfa category-entry))
			 (assoc 'vrbz category-entry))
		    ;; category-entry is still a prefix-verb
		    (let ((lex-entry
			    (find-associate-entry
			      (subseq verb-stem-with-prefix
				      (+ (length prefix-separated)
					 (length-of-prefix 
					   (assoc 'vrbz category-entry))))
			      '(verb hilfsverb modalverb))))
		      (cond ((member
			       '(infinitiv)
			       (lookup-tree
				 `(vtyp ,(or (cdr (assoc 'vtyp lex-entry)) 1)
					vgfa ,(cdr (assoc 'vgfa lex-entry)))
				 subtree)
			       :test #'equal)
			     (form-result
			       (list (list 'erweiterter-infinitiv))
			       (cdr (assoc 'wortart category-entry))
			       verb-stem-with-prefix)))))

		   ((member '(infinitiv)
			    (lookup-tree
			      `(vtyp ,(or (cdr (assoc 'vtyp category-entry))
					  1)
				     vgfa ,(cdr (assoc 'vgfa
						       category-entry)))
			      subtree)
			    :test #'equal)
		    (form-result (list (list 'erweiterter-infinitiv))
				 (cdr (assoc 'wortart category-entry))
				 verb-stem-with-prefix)))))))

(defun handle-nouns (lexical-entry suffix umlaut-flag)
  (let ((subtree (get-suffix-information suffix))
	(plural-path (list 'nomen
			   (if umlaut-flag 'red+ 'red-)
			   (if (assoc 'uml lexical-entry) 'uml+ 'uml-)
			   'pl (cdr (assoc 'kpl lexical-entry))))
	(singular-path (list 'nomen
			   (if umlaut-flag 'red+ 'red-)
			   (if (assoc 'uml lexical-entry) 'uml+ 'uml-)
			   'sg (cdr (assoc 'ksg lexical-entry)))))

       (let ((coded-pl-result (lookup-tree plural-path subtree))
	     (coded-sg-result (lookup-tree singular-path subtree)))
	    (cond (coded-sg-result
		   (cond (coded-pl-result
			  (list (list 'pl (car coded-pl-result))
				(list 'sg (car coded-sg-result))))
			 (t (list 'sg (car coded-sg-result)))))
		  (coded-pl-result
		   (list 'pl (car coded-pl-result)))))))

(defun noun-result-expander (noun-result gender)
  (cond (noun-result
	 (cond ((consp (car noun-result))
		;; singular and plural information
		(list (list gender
			    (list
			      (cons (caar noun-result)
				    (aref %deklin-tab%
					  (cadar noun-result)))
			      (cons (caadr noun-result)
				    (aref %deklin-tab%
					  (cadadr noun-result)))))))
	       (t  ;; only singular or only plural information
		(list (list gender
			    (list (cons (car noun-result)
					(aref %deklin-tab%
					      (cadr noun-result)))))))))))

(defun handle-adjectives (lex-entry suffix segment-type umlaut-reduced)
  (let ((elision (cdr (assoc 'elision lex-entry)))
	(komparation-class (cdr (assoc 'komp lex-entry)))
	(umlaut (cdr (assoc 'uml lex-entry)))
	(class (cdr (assoc 'main lex-entry)))
	(pred-e (car (assoc 'pred-e lex-entry)))
	(subtree (get-suffix-information suffix)))
    (let ((res
	    (lookup-tree (list 'adjektiv class
			       (or segment-type t)
			       (if umlaut-reduced
				   (cond ((null umlaut) 'red+uml-)
					 ((eq umlaut 'nec) 'red+uml+)
					 (t 'red+uml0))
				   (cond ((null umlaut) 'red-uml-)
					 ((eq umlaut 'nec) 'red-uml+)
					 (t 'red-uml0)))
			       (or elision t)
			       komparation-class
			       (or pred-e t))
			 subtree)))
      
      (if res (code-adj-result res)))))

(defun handle-inflected-participles (rest-suffix)
  (let* ((subtree (get-suffix-information rest-suffix))
	 (res (or (lookup-tree
		    (list 'adjektiv 'a1 t 'red-uml- t 'st t)
		    subtree)
		  (lookup-tree
		    (list 'adjektiv 'a1 t 'red-uml- t 'est t)
		    subtree))))
    (if res (code-adj-result res))))

(defun code-adj-result (comp-result-pairs)
  (do* ((rest comp-result-pairs (cdr rest))
	(actual-pair (car rest) (car rest))
	(result nil))
       ((null rest) result)
    (push (list (car actual-pair)
		(aref %adj-results% (cadr actual-pair)))
	  result)))

(defun handle-possessive-pronouns (suffix umlaut-red segment-type lex-entry)
  (cond (umlaut-red nil)
	((eq (cdr (assoc 'elision lex-entry)) 'pos)
	 (cond ((eq segment-type 'elision)
		(lookup-tree '(possessivpronomen 3 sg+pl)
			     (get-suffix-information suffix)))
	       (t (lookup-tree '(possessivpronomen 2 sg+pl)
			       (get-suffix-information suffix)))))
	(t (lookup-tree '(possessivpronomen 1 sg+pl)
			(get-suffix-information suffix)))))

(defun handle-determiner (category suffix umlaut-red number-info)
  (cond (umlaut-red nil)
	((eq category 'relativpronomen)
	 (car (lookup-tree (list 'determinativ 1 'sg+pl 'artikelwort)
			   (get-suffix-information suffix))))
	(t (car (lookup-tree (list category 1 number-info 'artikelwort)
			     (get-suffix-information suffix))))))

(defun handle-card-ord (key segment suffix lex-entry umlaut-reduced)
  (let ((ord-end (cdr (assoc 'ending lex-entry))))
    (cond (umlaut-reduced nil)
	  (t
	   (and (null suffix) (not segment)
		(push (additional-keys
		       (list key (list 'wortart 'kardinalzahl)))
		      %word-result%))
	   (let ((res
		  (lookup-tree (list 'ordinalzahl (or segment ord-end))
			(get-suffix-information suffix))))
	     (if res (aref %adj-results% (car res))))))))
			    
; ---------------------------------------------------------------------------
;;; FLOW OF CONTROL DURING ANALYSIS

(defun reset-results ()
  (clrhash %morphix-results%))

(defun morphix (word-list)
  (mapcar #'word-analysis word-list))

(defun input (word-list)
  (prog1 (morphix word-list)
	 (if *property-retrieval* (reset-results))))

(defun non-result-handler (word &optional (fail-val *fail-val*))
  (cond (*clarification-dialog-on* (dialog-for word fail-val))
	(t `((,word ,fail-val)))))

(defun dialog-for (word &optional (fail-val *fail-val*))
  (catch '%cancel-sign% (ask-category word))
  (cond ((morphological-analysis word))
	(t `((,word ,fail-val)))))

(defun word-analysis (word &optional (fail-val *fail-val*))
  ;; if input-word was a number or ordinal then
  ;; the finite state automaton for reading yielded a list as result
  ;; else start the analysis
  (if (listp word)
      word
      (cond (*property-retrieval*
	     (cond ((gethash word %morphix-results%))
		   ;; if the word was analysed before, take this result
		   (t
		    (let ((result (delete-duplicates 
				    (morphological-analysis word) :test #'equal)))
		      (cond (result (setf (gethash word %morphix-results%) result))
			    (t (setf (gethash word %morphix-results%) t)
			       ;; mark it as being analyzed without a result
			       (non-result-handler word fail-val)))))))

	    (t (cond ((delete-duplicates
		       (morphological-analysis word) :test #'equal))
		     (t (non-result-handler word fail-val)))))))
;;; it is the adapted version for running compounds
;;; here the storage variable of compounds is mantained

(defun morphological-analysis (word)
  (declare (special %composita-result%))
  (setq %composita-result% nil)
  (let* ((lex-entry (compute-fullform-entry word))
	 (multiple-entry-p (multiple-computed-entry-p lex-entry)))
    (cond ((marked-as-homograph lex-entry multiple-entry-p)
	   (let ((fullform-result
		  (fullform-handling word lex-entry multiple-entry-p)))
	     (dolist (element fullform-result (setq %word-result%
						    fullform-result))
		     (delete %additional-algorithmic% element)
		     ;; delete is ok, because %additional-algorithmic%
		     ;; doesn't occur at the first position
		     ))
	   (algorithmic-analysis word))
	  (lex-entry (setq %word-result%
			   (fullform-handling
			    word lex-entry multiple-entry-p)))
	  (t (setq %word-result% nil)
             (setq %composita-result% nil) ;; reset composita results
	     (algorithmic-analysis word))))
  
  ;; append both result registers
  (append %composita-result% %word-result%))


(defun fullform-handling (fullform-key lex-entry multiple-p)
  (if multiple-p
      (let ((result nil))
	(dolist (entry (cdr lex-entry) result)
		(if (stringp (car entry))
		    (push entry result)
		  (push (cons fullform-key entry) result))))
      (if (stringp (car lex-entry))
	  (list lex-entry)
	  (list (cons fullform-key lex-entry)))))

(defun algorithmic-analysis (word)
  (let* ((w-l (length word))
	 (possible-segments (stem+suffix word w-l)))
    (analyse-segments word possible-segments)
    ;; test for prefix "ge" for past-participles is done in the
    ;; prefix-handling below. The prefix-lexicon must be loaded!
    (and (test-for-umlaut word)
	 (analyse-segments
	  word (reduce-segment-stem possible-segments t) nil nil t))
    (and (> (length word) %minimal-stem-length%)
	 (do* ((prefix-list (compute-prefix word) (cdr prefix-list))
	       (prefix (car prefix-list) (car prefix-list))
	       (prefix-length (length prefix) (length prefix)))

	      ((null prefix-list))
	      (if (double-consonant-prefix-p prefix prefix-length)
		  (let ((next-char
			 (elt (segment-stem (car possible-segments))
			      prefix-length)))
		    (cond ((vowel-p next-char) 
			   (analyse-segments
			    word (augment-segments possible-segments prefix)
			    nil prefix)
			   (analyse-segments
			    word (augment-segments possible-segments
						   prefix nil t)
			    nil prefix))
			  ((eql next-char (elt prefix (1- prefix-length)))
			   nil)
			  (t (analyse-segments
			      word
			      (augment-segments possible-segments prefix)
			      nil prefix))))
		(analyse-segments
		 word (augment-segments possible-segments prefix)
		 nil prefix))
	      
	      (and (test-for-prefix word "ge" prefix-length)
		   (analyse-segments
		    word (augment-segments possible-segments prefix "ge")
		    t prefix))
	      (and (test-for-prefix word "zu" prefix-length)
		   ;; only combinations with 'zu' to check
		   (analyse-segments
		    word (augment-segments possible-segments prefix "zu")
		    nil prefix nil t))))))

;;; adapted version to handle compounds
;;; details of compound processing can be found in
;;; file composita.lisp

(defun analyse-segments (input-word possible-segments
			 &optional ge-reduced prefix-separated
			 umlaut-reduced zu-reduced)
  (dolist (segment possible-segments)
	  (let ((lex-entry (compute-stem-entry (segment-stem segment))))
	    (cond (lex-entry
                   (if (and prefix-separated (equal "ge" prefix-separated)
			    (not ge-reduced) (not zu-reduced))
		       (verify-segment
			input-word
			(segment-suffix segment) (segment-stem segment)
			(if (multiple-computed-entry-p lex-entry)
			    (cdr lex-entry) (list lex-entry))
			umlaut-reduced t nil nil))
		   (verify-segment
		    input-word
		    (segment-suffix segment) (segment-stem segment)
		    (if (multiple-computed-entry-p lex-entry)
			(cdr lex-entry) (list lex-entry))
		    umlaut-reduced ge-reduced
		    prefix-separated zu-reduced)))
            ;; this next condition is added in order to handle
            ;; noun-compounds
            ;; compound-handling can be performed in exhaustive
            ;; or non-exhaustive mode
            (WHEN (and *handle-composita* 
                       ;; if NC should be considered
                       ;; then only if either no lex-entry could be found
                       ;; or if all partitions should be searched for
                       ;; NC processing is started
                       (or
                           *all-composita*   (not lex-entry)))
                  (let* ((reduced-stem 
                          (remove %delimeter-sign% (segment-stem segment)
                                  :test #'char=))
                          (reduced-umlaut-segments (reduce-compound-umlauts reduced-stem))
                          (stem-segments (if reduced-umlaut-segments reduced-umlaut-segments
                                           (list (cons reduced-stem NIL)))))
                     (when *composita-trace*
                       (format T "~%Composita handling is on:~%")
                       (format T "~%  NC-WORDS: ~a" stem-segments)
                       (format T "~% ALL composita trace is: ~a" *all-composita*))
                     (loop for stem-segment in stem-segments 
                         do
                           (analyze-compound input-word
                                                   (first stem-segment)
                                                   (segment-suffix segment)
                                                   ;; next: indication whether umlauting
                                                   ;; inside NC has occured
                                                   (if reduced-umlaut-segments
                                                       (reverse (rest stem-segment))
                                                     NIL)
                                                   :umlaut? umlaut-reduced :ge? ge-reduced
                                                   :prefix? prefix-separated :zu? zu-reduced
                                                   :exhaustive *all-composita*))))
            )))

;;; it is the adapted version for running compounds
;;; the only changes concern storage of found
;;; compound-segmentation in a specific global-variable
;;; %composita-result% (triggered by dynamic free variable *composita*)
;;; and
;;; running the verification without storing verified segments, i.e., 
;;; as a predicate (triggered by dynamic variable *only-test*)

(defun verify-segment (input-word suffix key list-of-lex-entries
				  umlaut-reduced ge-reduced prefix-separated
				  zu-reduced &optional *composita* *only-test*)
  (dolist (lex-entry list-of-lex-entries)
	  (setq %key-result% nil)
	  (let ((pointer-entry (pointer-entry-p lex-entry)))
	    (cond (pointer-entry
		   ;; a lex-entry with the arc 'stamm'
		   (let* ((pointered-key (get-pointered-key pointer-entry))
			  (pointered-segment-type
			   (get-pointered-segment pointer-entry))
			  (pointered-lex
			   (find-associate-category
			    pointered-segment-type
			    (compute-stem-entry pointered-key)
			    key pointered-key))
			  (result
			   (category-branching
			    input-word suffix pointered-key pointered-lex
			    umlaut-reduced ge-reduced
			    prefix-separated zu-reduced
			    (if (> (length lex-entry) 1)
				(delete pointer-entry lex-entry))
			    ;; the lex-entry with the correct "wortart"-arc;
			    ;; a list of special information found at
			    ;; lex-entry. this information will be handled
			    ;; in the specific category-functions
			    pointered-segment-type)))
		     (and result
                          ;; for a comment see notes in last condition
                          (if *composita*
                              (if *only-test* 
                                  (return T)
                                (push (additional-keys result) %composita-result%))
			  (push (additional-keys result) %word-result%)))))
		  (t
		   (let ((result (category-branching
				  input-word suffix key lex-entry
				  umlaut-reduced
				  ge-reduced
				  prefix-separated
				  zu-reduced)))
		     (and result
                          ;; if we are in the composita mode
			  (if *composita*
                              ;; then if we have verified the segment without 
                              ;; computation of any side-effect
                              (if *only-test* 
                                  ;; then simply return  T 
                                  (return T)
                                ;; else the composita results are stored
                                ;; in free variable %composita-result%
                                (push (additional-keys result) %composita-result%))
                            ;; if no NC analysis has been performed then store
                            ;; result in free variable %word-result%
                            (push (additional-keys result) %word-result%)))))))))


(defun find-associate-category
       (search-segment pointered-lex-entry key category-key)
  (if (multiple-computed-entry-p pointered-lex-entry)
      (do* ((rest-pointered-lex-entry (cdr pointered-lex-entry)
				      (cdr rest-pointered-lex-entry))
	    (one-pointered-lex-entry (car rest-pointered-lex-entry)
				     (car rest-pointered-lex-entry)))
	   ((null rest-pointered-lex-entry)
	    (error "~2%in find-associate-category:~
                     ~%the segment-type ~s of the~
                     ~%irregular-form-entry for: ~s
                     ~%can't be found in the category-entry
                     ~%of ~s" search-segment key category-key))
	(cond ((assoc search-segment one-pointered-lex-entry)
	       (return one-pointered-lex-entry))))
					  ; else
      (cond ((assoc search-segment pointered-lex-entry) pointered-lex-entry)
	    (t (error "~2%in find-associate-category~
                        ~%the segment-type ~s of the~
                        ~%irregular-form-entry for ~s
                        ~%can't be found in the category-entry
                        ~%of ~s" search-segment key category-key)))))

(defun category-branching
       (input-word suffix key one-category-entry
	umlaut-reduced ge-reduced prefix-separated zu-reduced
	&optional irregular-stem-info segment-type
	&aux (category (cdr (assoc 'wortart one-category-entry)))
	prefix-verb prefix-verb-entry tmp)
  
  (cond (category 
	 (cond ((member category '(verb modalverb hilfsverb))
		(cond (umlaut-reduced nil)
		      (zu-reduced
		       (if (member suffix '("en" "n") :test #'equal)
			   (handle-prefix-infinitives
			    one-category-entry
			    (form-prefix-entry prefix-separated key)
			    (or segment-type 'vgfa)
			    (look-for-one-arc
			      'verb
			      (get-suffix-information suffix))
			    prefix-separated)
			 (and (or (and (> (length suffix) 2)
				       (string-equal suffix "end" :end1 3))
				  (and (> (length suffix) 1)
				       (string-equal suffix "nd" :end1 2)))
			      (setq prefix-verb
				    (form-prefix-entry prefix-separated key)
				    prefix-verb-entry
				    (find-associate-entry
				     prefix-verb
				     '(verb hilfsverb modalverb)))
			      (cond (prefix-verb-entry
				     (add-selector-keys prefix-verb-entry)
				     (handle-verbs
				      (form-prefix-entry
				       prefix-separated input-word
				       (+ 2 (length prefix-separated)))
				      suffix prefix-verb one-category-entry
				      (or segment-type 'vgfa)
				      (look-for-one-arc
				       'verb (get-suffix-information suffix))
				      ge-reduced category prefix-separated
				      prefix-verb-entry zu-reduced))))))

		      ((setq tmp (assoc 'vrbz one-category-entry))
		       (cond #||((not (and (= (length-of-prefix tmp) 2)
					(test-for-prefix key "ge"))) nil)||#
			     ((not prefix-separated) nil)
			     ((setq prefix-verb
				    (form-prefix-entry prefix-separated key)
				    prefix-verb-entry
				    (find-associate-entry
				     prefix-verb
				     '(verb hilfsverb modalverb)))
			      (add-selector-keys prefix-verb-entry)
			      (let ((reduced-verb (subseq key (length-of-prefix tmp))))
			      (handle-verbs
			       input-word suffix prefix-verb
			       (find-associate-entry
				     reduced-verb
				     '(verb hilfsverb modalverb))
			        (or segment-type 'vgfa)
			       (look-for-one-arc 'verb
				(get-suffix-information suffix))
			       ge-reduced category
			       prefix-separated prefix-verb-entry)))))

		      (prefix-separated
		       (setq prefix-verb
			     (form-prefix-entry prefix-separated key)
			     prefix-verb-entry
			     (find-associate-entry
			      prefix-verb '(verb hilfsverb modalverb)))
		       (cond (prefix-verb-entry
			      (add-selector-keys prefix-verb-entry)
			      (handle-verbs
			       input-word suffix prefix-verb
			       one-category-entry (or segment-type 'vgfa)
			       (look-for-one-arc 'verb
				(get-suffix-information suffix))
			       ge-reduced category
			       prefix-separated prefix-verb-entry))))

		      (t
		       (add-selector-keys one-category-entry)
		       (handle-verbs
			  input-word suffix key one-category-entry
			  (or segment-type 'vgfa)
			  (look-for-one-arc 'verb
                            (get-suffix-information suffix))
			  ge-reduced category))))

	       ((or ge-reduced prefix-separated zu-reduced) nil)
	       ;; only allowed for verbs

	       ((eq category 'nomen) 
		(add-selector-keys one-category-entry)
		(form-result
		  (noun-result-expander
		    (handle-nouns (or irregular-stem-info one-category-entry)
				  ;; stem-info should be a complete lex-entry
				  suffix umlaut-reduced)
		    (cdr (assoc 'genus one-category-entry)))
		  'nomen key))

	       #||
	       ((eq category 'subst-adj)
		(cond (umlaut-reduced nil)
		      (t 	     
		       (form-result
			 (subst-adj-genus-filter
			   (car
			     (lookup-tree
			       '(adjektiv pos)
			       (get-suffix-information
				 (if suffix (string-append "e" suffix)
				     "e"))))
			   (cdr (assoc 'genus one-category-entry)))
			 'subst-adj one-category-entry))))
	       ||#
	       
	       ((eq category 'adjektiv)
		(add-selector-keys one-category-entry)
		(form-result
		  (handle-adjectives
		    one-category-entry suffix segment-type umlaut-reduced)
		  'adjektiv key))
	       
	       ((eq category 'possessivpronomen)
		(add-selector-keys one-category-entry)
		(form-result
		  (handle-possessive-pronouns
		    suffix umlaut-reduced segment-type one-category-entry)
		  'possessivpronomen key))
	       
	       ((member category '(determinativ determinativ-indef
						relativpronomen))
		(add-selector-keys one-category-entry)
		(form-result
		 (handle-determiner
		  category
		  suffix umlaut-reduced
		  (cdr (assoc 'det-num one-category-entry))) category key))
	       ((eq category 'kardinalzahl)
		(add-selector-keys one-category-entry)
		(form-result
		 (handle-card-ord key segment-type suffix one-category-entry
				  umlaut-reduced)
		 'ordinalzahl key))
	       
	       (t
		(format t "~2% WRONG CATEGORY SPECIFIED IN LEXICON FOR ~S"
			  key))))
	(t (format t "~2% MISSING STEM FOR ~S IN THE *stem-lexicon* " key))))


(defun form-result (part-result category stem)
  (if part-result
	(list stem (list 'wortart category)
	      (list 'flexion part-result))))

(defun additional-keys (part-result)
  (if %key-result% (nconc part-result %key-result%) part-result))

(defun add-selector-keys (lex-entry)
  (and *additional-selectors*
       (dolist (selector *additional-selectors*)
	       (let ((tmp (assoc selector lex-entry)))
		 (if tmp (push tmp %key-result%))))))

; ---------------------------------------------------------------------------
; MORPHIX: GENERATION

;;; TABLES FOR VERB INFLECTION

(defvar %verb-segments% nil)
(defvar %combined-conjug-tree% nil)

(setq %verb-segments%
      (make-array
       12
       :initial-contents
       '(nil
	 ((praesens (sg (1 . vgfa+)))
	  (konjunktiv-1 (sg (1 . vgfa+) (3 . vgfa+)))
	  (imperativ (sg . vgfa+)) vgfa)
	 ((imperfekt . vgfc) vgfa)
	 ((imperfekt . vgfc) (konjunktiv-2 . vgfc) vgfa)
	 ((imperfekt . vgfc) (konjunktiv-2 . vgfd) vgfa)
	 ((imperfekt . vgfc) (konjunktiv-2 . vgfc)
	  (praesens (sg (2 . vgfb) (3 . vgfb))) vgfa)
	 ((imperfekt . vgfc) (konjunktiv-2 . vgfd)
	  (praesens (sg (2 . vgfb) (3 . vgfb))) vgfa)
	 ((imperfekt . vgfc) (konjunktiv-2 . vgfd)
	  (praesens (sg (2 . vgfb) (3 . vgfb)))
	  (imperativ (sg . vgfb)) vgfa)
	 ((praesens (sg . vgfb)) vgfa)
	 ((praesens (sg (anrede . vgfa) vgfb))
	  (imperfekt . vgfc) vgfa)
	 ((praesens (sg (anrede . vgfa) vgfb))
	  (imperfekt . vgfc)
	  (konjunktiv-2 . vgfd) vgfa)
	 ((imperfekt . vgfc) (konjunktiv-2 . vgfd)
	  (praesens (sg (1 . sond1) (2 . sond2) (3 . sond3) (anrede . sond4))
		    (pl (1 . sond4) (3 . sond4) (anrede . sond4)))
	  vgfa))))

(setq %combined-conjug-tree%
      '((praesens (indikativ
		   (passiv
		    (info ("werd" . praesens) ((stem . pprf)))))
		  (konjunktiv
		   (passiv
		    (info ("werd" . konjunktiv-1) ((stem . pprf)))))
		  (imperativ
		   (passiv
		    (info ("sei" . imperativ) ((stem . pprf))))))
	
	(imperfekt (indikativ
		    (passiv
		     (info ("werd" . imperfekt)
			   ((stem . pprf)))))
		   (konjunktiv
		    (aktiv
		     (umschreibung
		      ("werd" . konjunktiv-2)
		      ((stem . infinitiv))))
		    (passiv
		     (info ("werd" . konjunktiv-2)
			   ((stem . pprf)))
		     (umschreibung
		      ("werd" . konjunktiv-2)
		      ((stem . pprf) ("werd" . infinitiv))))))
	
	(perfekt (indikativ
		  (aktiv
		   (info (auxiliar . praesens) ((stem . pprf))))
		  (passiv
		   (info ("sei" . praesens)
			 ((stem . pprf) ("werd" . pprf)))))
		 (konjunktiv
		  (aktiv
		   (info (auxiliar . konjunktiv-1) ((stem . pprf))))
		  (passiv
		   (info ("sei" . konjunktiv-1)
			 ((stem . pprf) ("werd" . pprf))))))
	
	(futur-1 (indikativ
		  (aktiv
		   (info ("werd" . praesens)
			 ((stem . infinitiv))))
		  (passiv
		   (info ("werd" . praesens)
			 ((stem . pprf) ("werd" . infinitiv)))))
		 (konjunktiv
		  (aktiv
		   (info ("werd" . konjunktiv-1)
			 ((stem . infinitiv))))
		  (passiv
		   (info ("werd" . konjunktiv-1)
			 ((stem . pprf) ("werd" . infinitiv))))))
	
	(plusquamperfekt (indikativ
			  (aktiv
			   (info (auxiliar . imperfekt)
				 ((stem . pprf))))
			  (passiv
			   (info ("sei" . imperfekt)
				 ((stem . pprf) ("werd" . pprf)))))
			 (konjunktiv
			  (aktiv
			   (info (auxiliar . konjunktiv-2)
				 ((stem . pprf)))
			   (umschreibung
			    ("werd" . konjunktiv-2)
			    ((stem . pprf)
			     (auxiliar . infinitiv))))
			  (passiv
			   (info ("sei" . konjunktiv-2)
				 ((stem . pprf) ("werd" . pprf)))
			   (umschreibung
			    ("werd" . konjunktiv-2)
			    ((stem . pprf) ("werd" . pprf)
			     ("sei" . infinitiv))))))
	
	(futur-2 (indikativ
		  (aktiv
		   (info ("werd" . praesens)
			 ((stem . pprf) (auxiliar . infinitiv))))
		  (passiv
		   (info ("werd" . praesens)
			 ((stem . pprf) ("werd" . pprf)
			  ("sei" . infinitiv)))))
		 (konjunktiv
		  (aktiv
		   (info ("werd" . konjunktiv-1)
			 ((stem . pprf) (auxiliar . infinitiv))))
		  (passiv
		   (info ("werd" . konjunktiv-1)
			 ((stem . pprf) ("werd" . pprf)
			  ("sei" . infinitiv))))))))

; ---------------------------------------------------------------------------
;;; CATEGORY SPECIFIC FUNCTIONS

(defun adjective-inflection (stem comparation attributive-used-p
			     &optional article gender number case
			     &key (lex (find-associate-entry stem 'adjektiv)))
  (let ((umlaut (cdr (assoc 'uml lex))))
    (multiple-value-bind
      (stem-to-use segment-type umlaut-reduced)
	(get-adj-stem stem comparation attributive-used-p lex) 

      (let ((class (cdr (assoc 'main lex)))
	    (segment (or segment-type t))
	    (pred-e (if (assoc 'pred-e lex) 'pred-e t))
	    (elision (or (cdr (assoc 'elision lex)) t))
	    (comparation-class (cdr (assoc 'komp lex)))
	    (uml-search-node
	      (if umlaut-reduced
		  (cond ((null umlaut) 'red+uml-)
			((eq umlaut 'nec) 'red+uml+)
			(t 'red+uml0))
		  (cond ((null umlaut) 'red-uml-)
			((eq umlaut 'nec) 'red-uml+)
			(t 'red-uml0))))
	    (result nil))
	(let ((path (list 'adjektiv class segment uml-search-node elision
			  comparation-class pred-e)))
	  (dolist (suffix %adjective-suffix-list%)
	    (cond ((setq result
			 (adjective-result
			   (lookup-tree path
					(get-suffix-information suffix))
			   stem-to-use suffix attributive-used-p
			   comparation article gender number case))
		   (return result)))))))))

(defun get-adj-stem (stem comparation attributive-used-p lex-entry &aux tmp)
  (case comparation
	(pos (cond ((not attributive-used-p) (values stem nil nil))
		   ((setq tmp (assoc 'pos lex-entry))
		    (values (cdr tmp) 'pos nil))
		   ((assoc 'elision lex-entry)
		    (values (e-elision stem) 'elision nil))
		   (t (values stem nil nil))))
	(kom (cond ((setq tmp (assoc 'kom lex-entry))
		    (values (cdr tmp) 'kom nil))
		   ((assoc 'elision lex-entry)
		    (values (e-elision stem) 'elision nil))
		   ((assoc 'uml lex-entry)
		    (values (umlautung stem) nil t))
		   (t (values stem nil nil))))
	(sup (cond ((setq tmp (assoc 'sup lex-entry))
		    (values (cdr tmp) 'sup nil))
		   ((assoc 'uml lex-entry)
		    (values (umlautung stem) nil t))
		   (t (values stem nil nil))))))

(defun adjective-result (ial-result stem-to-use suffix attributive-used-p
			 comparation article gender number case)
  
  (if ial-result
      (dolist (part ial-result)
	(cond ((eq (car part) comparation)
	       (let ((expanded-result
		       (aref %adj-results% (cadr part))))
		 (cond (attributive-used-p
			(let* ((article-match
				 (assoc article expanded-result))
			       (gender-match
				 (if article-match
				     (assoc gender (cadr article-match))))
			       (number-match
				 (if gender-match
				     (assoc number (cadr gender-match)))))
				
			  (and number-match
			       (member case (cadr number-match))
			       (return
				 (conc-stem+suffix stem-to-use suffix)))))
		       
		       ;; else not attributiv-used-p
		       ((or (assoc 'praedikativ-gebraucht
				   expanded-result)
			    (assoc 'unflektiert expanded-result))
			(return
			  (conc-stem+suffix stem-to-use suffix))))))))))

(defun past-participle-inflection (verb-stem &optional eventually-lex-entry
				   participle-selector comparation
				   attributive-used-p
				   article gender number case)
  (let ((stem-to-use (form-pprf verb-stem eventually-lex-entry participle-selector))
	(result nil)
	(path (list 'adjektiv 'a1 t 'red-uml- t 'st t)))
    (if stem-to-use
	(dolist (suffix %adjective-suffix-list%)
	  (cond ((setq result
		       (adjective-result
			 (lookup-tree path (get-suffix-information suffix))
			 stem-to-use suffix attributive-used-p
			 comparation article gender number case))
		 (return result)))))))


;;; (changed by [WF], 2/08/93 12:29:00
(defun present-participle-inflection (verb-stem &optional additional-zu
				      comparation attributive-used-p
				      article gender number case lex-entry)
  (let* ((stem-to-use (form-ppres verb-stem additional-zu lex-entry))
	 (stem-as-string (and stem-to-use
			      (if (consp stem-to-use)
				  (second stem-to-use)
				  stem-to-use)))
	 (result nil)
	 (path (list 'adjektiv 'a1 t 'red-uml- t 'st t)))
    (if stem-as-string
	(dolist (suffix %adjective-suffix-list%)
	  (cond ((setq result
		       (adjective-result
			 (lookup-tree path (get-suffix-information suffix))
			 stem-as-string suffix attributive-used-p
			 comparation article gender number case))
		 (return (if (consp stem-to-use)
			     (list "zu" result)
			     result))))))))

(defun form-ppres (stem &optional zu entry)
  (let* ((lex-entry (or entry (find-associate-entry
				stem
				'(verb hilfsverb modalverb))))
	 (vrbz (assoc 'vrbz lex-entry))
	 (strip? (vrbz-strip-p vrbz))
	 (prefix-length (length-of-prefix vrbz))
	 (prefix (if prefix-length (subseq stem 0 prefix-length)))
	 (canonical-result
	   (if prefix
	       (if strip?
		   (get-canonical-entry stem prefix prefix-length)
		   (get-canonical-entry stem prefix prefix-length prefix))))
	 (additional-prefix (if (and prefix strip?) (car canonical-result) ""))
	 (canonical-stem (if prefix (cadr canonical-result) stem))
	 (canonical-entry (if prefix (caddr canonical-result) lex-entry))
	 (sub-segment (cdr (assoc 'VGFA canonical-entry)))
	 (search-path (list 'verb 'partizip 'vgfa sub-segment))
	 (participle-main-stem
	   (cond ((lookup-tree
		    search-path (get-suffix-information "end"))
		  (concatenate 'string canonical-stem "end"))
		 (t (concatenate 'string canonical-stem "nd")))))
    (if zu
	(cond (strip? (concatenate 'string prefix "zu" additional-prefix participle-main-stem))
	      (t (list "zu" (concatenate 'string prefix additional-prefix participle-main-stem))))
	(concatenate 'string prefix additional-prefix participle-main-stem))))

;;; bug fixed in call of form-infinitive

;;; M E R G E !!
;;; (changed by [WF], 6/28/93 15:43:32

(defun verb-inflection
       (stem tense mood voice person number
	&optional entry &aux (internal-stem (string-downcase stem)))
  (declare (special stem))
  (let* ((stem-entry (or entry (find-associate-entry internal-stem
						     '(verb hilfsverb modalverb))))
	 (auxiliar (cdr (assoc 'PAUX stem-entry)))
	 (vrbz (assoc 'vrbz stem-entry))
	 (strip? (vrbz-strip-p vrbz))
	 (prefix-length (length-of-prefix vrbz))
	 (prefix (if prefix-length (subseq internal-stem 0 prefix-length))))
    (declare (special auxiliar))
    (let* ((canonical-result
	     (if prefix
		 (if strip?
		     (get-canonical-entry internal-stem prefix prefix-length)
		     (get-canonical-entry internal-stem prefix prefix-length
					  prefix))))
	   (additional-prefix (if prefix (car canonical-result) ""))
	   (canonical-stem (if prefix (cadr canonical-result) internal-stem))
	   (canonical-entry (if prefix (caddr canonical-result) stem-entry)))
      
      (cond ((single-verb-form-p tense voice)
	     (if strip? 
		 (list (surface-verb
			 canonical-stem (code-tense+mood tense mood)
			 number person canonical-entry additional-prefix)
		       prefix)
		 ;; separable-prefix is handled like a infinite-part in
		 ;; a combined surface form.
		 ;; else
		 (surface-verb canonical-stem (code-tense+mood tense mood)
			       number person canonical-entry
			       additional-prefix)))
	    (t (let* ((conjug-table-entry
			(lookup-tree
			  (if *exp-subj-p*
			      (list tense mood voice 'umschreibung)
			      (list tense mood voice 'info))
			  %combined-conjug-tree%))
		      (finite-part (car conjug-table-entry))
		      (infinite-parts (cadr conjug-table-entry))
		      (finite-stem (car finite-part)))
		 (if (not conjug-table-entry)
		     (error "wrong specifications for tense, mood or voice"))
		 (cons
		   (surface-verb
		     (eval finite-stem) (cdr finite-part) number person
		     (find-associate-entry
		       (eval finite-stem) '(verb hilfsverb modalverb)) "")
		   (do* ((rest-infinites infinite-parts (cdr rest-infinites))
			 (infinite-part (car rest-infinites)
					(car rest-infinites))
			 (result nil))
			((null rest-infinites) (nreverse result))
		     (push 
		       (cond ((eq (cdr infinite-part) 'pprf)
			      (form-pprf (eval (car infinite-part))
					 (if (symbolp (car infinite-part))
					     stem-entry)
					 (if (string-equal
					       "werd"
					       (eval (car infinite-part)))
					     'hilfsverb)))
			     ((eq (cdr infinite-part) 'infinitiv)
			      (form-infinitive
				(eval (car infinite-part)))))
		       result)))))))))

(defun get-canonical-entry (stem strip-prefix prefix-length
				 &optional (prefix ""))
  (let* ((new-stems
	  (if (double-consonant-prefix-p strip-prefix prefix-length)
	      (let ((next-char (elt stem prefix-length)))
		   (cond ((vowel-p next-char) 
			  (list (subseq stem prefix-length)
				(subseq stem (1- prefix-length))))
			 ((eql next-char
			       (elt strip-prefix (1- prefix-length))) 
			  nil)
			 (t (list (subseq stem prefix-length)))))
	      (list (subseq stem prefix-length))))
	 (lex-entry
	  (do* ((rest-candidates new-stems (cdr rest-candidates))
		(new-stem (car rest-candidates) (car rest-candidates))
		(result nil))
	       ((null rest-candidates) nil)
	       (cond ((setq result
			    (find-associate-entry
			     new-stem '(verb modalverb hilfsverb)))
		      (setq new-stems new-stem)
		      (return result)))))
	 (new-prefix-length (length-of-prefix (assoc 'vrbz lex-entry)))
	 (new-prefix
	  (if new-prefix-length (subseq new-stems 0 new-prefix-length))))

    (cond (lex-entry
	   (cond (new-prefix
		  (get-canonical-entry new-stems
				       new-prefix
				       (length new-prefix)
				       (if (not (equal "" prefix))
					   (low-concat prefix new-prefix
						       0 (length prefix)
						       0 (length new-prefix))
					   new-prefix)))
		 (t (list prefix new-stems lex-entry))))
	  (t (error "no canonical stem found for the prefix-verb ~s"
		    new-stems)))))

(defun form-pprf
  (stem &optional eventually-lex-entry participle-selector
	&aux (lex-entry (or eventually-lex-entry
			    (find-associate-entry
			      stem
			      '(verb hilfsverb modalverb))))
	(pprf-component (cdr (assoc 'PPRF lex-entry))))
  
  (cond ((stringp pprf-component) pprf-component)
	;; only one past participle in the lex-entry
	((and participle-selector
	      (cdr (assoc participle-selector pprf-component))))
	;; if a selector is available and yields to a result, take
	;; this result.
	(t				; take the first possible result
	 (cdar pprf-component))))

;;; bug fixed 
;;; M E R G E !!
;;; (changed by [WF], 10/22/93 13:36:45

(defun form-infinitive (stem &optional zu entry)
  (let* ((lex-entry (or entry
			(find-associate-entry
			  stem '(verb hilfsverb modalverb))))
	 (vrbz (assoc 'vrbz lex-entry))
	 (strip? (vrbz-strip-p vrbz))
	 (prefix-length (length-of-prefix vrbz))
	 (prefix (if prefix-length (subseq stem 0 prefix-length)))
	 (canonical-result
	   (if prefix
	       (if strip?
		   (get-canonical-entry stem prefix prefix-length)
		   (get-canonical-entry stem prefix prefix-length prefix))))
	 (additional-prefix (if (and prefix strip?) (car canonical-result) ""))
	 (canonical-stem (if prefix (cadr canonical-result) stem))
	 (canonical-entry (if prefix (caddr canonical-result) lex-entry))
	 (sub-segment (cdr (assoc 'VGFA canonical-entry)))
	 (vtype (or (cdr (assoc 'VTYP canonical-entry)) 1))
	 (infinitive-main-stem
	   (cond ((assoc 'infinitiv
			 (lookup-tree
			   (list 'verb 'vtyp vtype 'vgfa sub-segment)
			   (get-suffix-information "en")))
		  (concatenate 'string canonical-stem "en"))
		 ((assoc 'infinitiv
			 (lookup-tree (list 'verb 'vtyp vtype 'vgfa sub-segment)
				      (get-suffix-information "n")))
		  (concatenate 'string canonical-stem "n"))
		 (t (error "not possible to form 'INFINITIVE' for ~s" stem)))))
    (if zu
	(cond (strip? (concatenate
			'string prefix "zu"
			additional-prefix infinitive-main-stem))
	      (t (list "zu"
		       (concatenate
			 'string prefix additional-prefix
			 infinitive-main-stem))))
	(concatenate
	  'string prefix additional-prefix
	  infinitive-main-stem))))


(defun surface-verb (stem tense+mood number person entry prefix)
  (if entry
      (let* ((vtype (or (cdr (assoc 'VTYP entry)) 1))
	     (specification (list tense+mood number
				  (if (member person '("sie" "Sie" "SIE" 2A)
					      :test #'equal)
				      'anrede
				      person)))
	     (possible-segment
	       (get-correct-segment
		 specification
		 (aref %verb-segments% vtype)))
	     (segment (if (eq possible-segment 'VGFA+)
			  (if (assoc 'VGFA+ entry) 'VGFA+ 'VGFA)
			  possible-segment))
	     (segment-specification
	       (if segment (assoc segment entry)
		   (error
		     " in surface-verb: segment in lex-entry ~s not found"
			  entry)))
	     (sub-segment (if (eq 'VGFA segment)
			      (cdr segment-specification)
			      (if (consp (cdr segment-specification))
				  (cadr segment-specification))))
	     (new-stem (cond ((eq segment 'VGFA) stem)
			     ((member segment
				      '(SOND1 SOND2 SOND3 SOND4 VGFD))
			      (cdr segment-specification))
			     (t (cddr segment-specification)))))
	(do ((rest-suffix-list  %verb-suffix-list% (cdr rest-suffix-list))
	     (result nil))
	    ((null rest-suffix-list) nil)
	  
	  (cond ((setq result
		       (verb-result
			 new-stem specification
			 (car rest-suffix-list)
			 prefix
			 vtype segment sub-segment))
		 (return result)))))))

(defun verb-result (stem specification suffix additional-prefix
		    vtype segment sub-segment)
  ;; builds a verb-result and verifies it in the IAL
  ;; specification is a list consisting of tense+mood, number and person
  (let* ((suffix-information (get-suffix-information suffix))
	 (matched-to-tense+mood
	   (lookup-tree
	     (if sub-segment
		 (list 'verb 'vtyp vtype segment sub-segment
		       (car specification))
		 (list 'verb 'vtyp vtype segment
		       (car specification)))
	     suffix-information))
	 (matched-to-num
	   (cond (matched-to-tense+mood
		  (cond ((eq (car specification) 'imperativ)
			 (cond ((and (eql 2 (third specification))
				     (eq (second specification)
					 (caar matched-to-tense+mood)))
				'imperativ)
			       ((and (member (third specification)
					     '("SIE" anrede "Sie" "sie")
					     :test #'equal)
				     (eq (caar matched-to-tense+mood)
					 'anrede))
				'imperativ)))
			((cdr (assoc (second specification)	; numerus
				     (car matched-to-tense+mood)))))))))
    (cond ((and matched-to-num
		(or (eq 'imperativ matched-to-num)
		    (and (member (third specification)
				 '("SIE" anrede "Sie" "sie")
				 :test #'equal)
			 (member 'anrede (car matched-to-num)))
		    (member (third specification)
			    (car matched-to-num))))
	   (if suffix (concatenate 'string additional-prefix stem suffix)
	       (if additional-prefix
		   (concatenate 'string additional-prefix stem)
		   stem))))))

(defun get-correct-segment (specifications information)
  ;; specifications can be a list of tempus numerus person
  (if information
      (cond ((atom information) information)
	    ((do* ((rest-info information (cdr rest-info))
		   (element (car rest-info) (car rest-info)))
		  ((null rest-info) nil)
	       (and (consp element)
		    (eq (car specifications) (car element))
		    (let ((result (get-correct-segment (cdr specifications)
						       (cdr element))))
		      (cond (result (return result)))))
	       (and (atom element) (return element)))))))

(defun noun-inflection (stem case number
			&optional (entry
				    (find-associate-entry
				      (string-downcase stem) 'nomen))
			&aux (internal-stem (string-downcase stem)))
  (if entry
      (do* ((rest-suffix-list %noun-suffix-list% (cdr rest-suffix-list))
	    (suffix (car rest-suffix-list) (car rest-suffix-list))
	    (surface-stem (surface-noun-stem internal-stem number entry))
	    (search-path (noun-search-path number entry))
	    (result nil))
	   ((null rest-suffix-list) nil)
	   ;; loop over relevant suffixes
	(cond ((setq result
		     (noun-result
		      search-path surface-stem case suffix
		      (get-suffix-information suffix)))
	       (return (capitalize-noun result (cdr (assoc 'CAPS entry)))))))))

;;; (changed by [WF], 6/30/93 15:40:53
(defun capitalize-noun (string &optional (chars-to-upcase 1 specified-p))
  (do ((res (copy-seq string))
       (n 0 (1+ n)))
      ((= n (length res)) res)
    (cond ((= n 0) (setf (elt res n) (char-upcase (elt res n))))
	  (t (let ((char (elt res n)))
	       (cond ((and specified-p chars-to-upcase
			   (< n chars-to-upcase))
		      (setf (elt res n) (char-upcase (elt res n))))
		     ((and (member char '(#\/ #\-))
			   (< n (1- (length res))))
		      (setf (elt res (1+ n))
			    (char-upcase (elt res (1+ n)))))))))))

(defun surface-noun-stem (stem number entry)
  (cond ((eq 'sg number) stem)
	;; no change in the singular
	((cdr (assoc 'plural entry)))
	;; if plural and an irregular plural-stem exists, take this one
	((assoc 'uml entry) (umlautung stem))
	;; if plural and the stem is marked for umlautung, then perform
	;; this operation
	(t stem)))

(defun noun-search-path (number entry)
  (cond ((eq 'sg number)
	 (let ((ksg (cdr (assoc 'ksg entry)))
	       (uml (if (assoc 'uml entry) 'uml+ 'uml-)))
	      (list 'nomen 'red- uml 'sg ksg)))
	((eq number 'pl)
	 (let ((irreg-plural-stem (cdr (assoc 'plural entry))))
	   (cond (irreg-plural-stem
		  (let ((kpl (cdr (assoc 'kpl (compute-stem-entry
					       irreg-plural-stem)))))
		       (list 'nomen 'red- 'uml- 'pl kpl)))
		 (t (let ((kpl (cdr (assoc 'kpl entry)))
			  (uml (if (assoc 'uml entry) 'uml+ 'uml-)))
			 (list 'nomen (if (eq uml 'uml+)
					  'red+ 'red-) uml 'pl kpl))))))))

(defun noun-result (search-path surface-stem case suffix ial-tree) 
  (let ((matched-to-case
	 (lookup-tree search-path ial-tree)))
       (cond ((case-found-p case (car matched-to-case))
	      (conc-stem+suffix surface-stem suffix)))))

(defun possessive-inflection (stem gender number case
				   &optional (type 'substantivwort)
				   (article 'ohne))
  (let* ((lex-entry (find-associate-entry stem 'possessivpronomen))
	 (elision (assoc 'elision lex-entry))
	 (new-stem (if elision (e-elision stem) stem)))
  
    (do* ((rest-suffix-list %posspron-suffix-list% (cdr rest-suffix-list))
	  (suffix (car rest-suffix-list) (car rest-suffix-list))
	  (stem-to-use (if suffix new-stem stem) (if suffix new-stem stem))
	  (result nil))
	 ((null rest-suffix-list) nil)
	 (cond ((setq result
		      (posspron-result
		       (lookup-tree
			(list 'possessivpronomen
			      (if (and suffix elision) 3 1)
			      'sg+pl
			      type)
			(get-suffix-information suffix))
		       stem-to-use suffix type article gender number case))
		(return result))))))

(defun posspron-result (ial-result stem-to-use suffix type
				   article gender number case)
  (if ial-result
      (do* ((rests ial-result (cdr rests))
	    (part (car rests) (car rests)))
	   ((null rests) nil)
	   (let* ((article-match (if (eq type 'substantivwort)
				     (assoc article part)
				   part))
		  (gender-match
		   (if article-match
		       (assoc gender
			      (if (eq type 'substantivwort)
				  (cadr article-match)
				article-match))))
		  (number-match
		   (if gender-match (assoc number (cadr gender-match)))))
	     (and number-match
		  (member case (cadr number-match))
		  (return (conc-stem+suffix stem-to-use suffix)))))))

(defun perspron-inflection (person case number &optional gender)
  (let ((part-result (lookup-tree (list person number case)
				  %perspron-tree%)))
    (cond ((stringp (car part-result)) (car part-result))
	  (part-result
	   (car (look-for-one-arc gender part-result))))))

(defun reflexive-inflection (person case number)
  (car (lookup-tree (list person number case) %reflexivpron-tree%)))

(defun detdef-inflection (case number gender &optional (det 'det))
  ;; for the inflection of "der", "die", "das" ...
  ;; if det is specified as t, then build the forms for the relative pronoun:
  ;; "der", "dessen",...
  (let* ((part-result
	  (lookup-tree (list number case gender) %detdef-tree%))
	 (rest (cond ((stringp (car part-result)) (car part-result))
		     (part-result
		      (car (look-for-one-arc det part-result))))))
    (and rest (low-concat "d" rest 0 1 0 (length rest)))))

(defun query-inflection (case number gender)
  ;; only correct for (and (eq number 'sg)
  ;;                       (not (and (eq gender 'ntr)
  ;;                                 (eq case 'dat))))
  ;; see Helbig/Buscha Deutsche Grammatik for details

  (let ((rest (car (lookup-tree (list number case gender) %query-tree%))))
    (and rest (low-concat "w" rest 0 1 0 (length rest)))))

(defun determiner-inflection (stem case number gender
				   &optional (category 'determinativ))
  (let* ((lex-entry (find-associate-entry stem category))
	 (det-num (cdr (assoc 'det-num lex-entry)))
	 (result nil))
    (dolist (suffix (cdr %posspron-suffix-list%))
	    (cond ((setq result
			 (determiner-result
			  (lookup-tree
			   (list 'determinativ 1 (or det-num 'sg+pl)
				 'artikelwort)
			   (get-suffix-information suffix))
			  stem suffix case number gender))
		   (return result))))))

(defun determiner-result (ial-result stem suffix case number gender)
  (if ial-result
      (dolist (part ial-result)
	      (let* ((gender-match (assoc gender part))
		     (number-match (if gender-match
				       (assoc number (cadr gender-match)))))
		(and number-match
		     (member case (cadr number-match))
		     (return (conc-stem+suffix stem suffix)))))))

(defun determiner-indef-inflection (stem case number gender)
  (let* ((lex-entry (find-associate-entry stem 'determinativ-indef))
	 (det-num (cdr (assoc 'det-num lex-entry)))
	 (result nil))
    (dolist (suffix %posspron-suffix-list%)
	    (cond ((setq result
			 (determiner-result
			  (lookup-tree
			   (list 'determinativ-indef 1 det-num 'artikelwort)
			   (get-suffix-information suffix))
			  stem suffix case number gender))
		   (return result))))))

(defun ordinal-inflection (stem attributive-used-p
				&optional article gender number case)
  (let ((lex-entry (find-associate-entry stem 'kardinalzahl))
	(result nil))
    (multiple-value-bind
     (stem-to-use search-arc)
     (get-ordinal-stem stem lex-entry) 

     (dolist (suffix %ordinal-suffix-list%)
	     (cond ((setq result
			  (ordinal-result
			   (lookup-tree
			    (list 'ordinalzahl search-arc)
			    (get-suffix-information suffix))
			   stem-to-use suffix attributive-used-p
			   article case number gender))
		    (return result)))))))
    
(defun get-ordinal-stem (stem lex-entry)
  (if lex-entry
      (let ((ordinal (assoc 'ordinal lex-entry)))
	(if ordinal (values (cdr ordinal) 'ordinal)
	  (let ((ordinal-b (assoc 'ordinal-b lex-entry)))
	    (if ordinal-b (values (cdr ordinal-b) 'ordinal-b)
		(values stem (cdr (assoc 'ending lex-entry)))))))))

(defun ordinal-result (ial-result stem-to-use suffix attributive-used-p
				  article case number gender)
  (if ial-result
      (dolist (part ial-result)
	      (let ((expanded-result (aref %adj-results% part)))
		(cond (attributive-used-p
		       (let* ((article-match (assoc article expanded-result))
			      (gender-match
			       (if article-match
				   (assoc gender (cadr article-match))))
			      (number-match
			       (if gender-match
				   (assoc number (cadr gender-match)))))
				
			 (and number-match
			      (member case (cadr number-match))
			      (return
			       (conc-stem+suffix stem-to-use suffix)))))
		       
		      ;; else not attributive-used-p
		      ((assoc 'praedikativ-gebraucht expanded-result)
		       (return (conc-stem+suffix stem-to-use suffix))))))))

; ---------------------------------------------------------------------------
;;; TOPLEVEL FUNCTIONS FOR A GENERATION DEMO

;;; (changed by [WF], 10/22/93 13:49:49
(defun show-verb (stem)
  (let ((entry (find-associate-entry (string-downcase stem)
				     '(verb modalverb hilfsverb))))
    (cond (entry
	   (setq *exp-subj-p* nil)
	   (format t "~3%")
	   (show-imperativ stem entry)
	   (dolist (tense '(praesens imperfekt perfekt
				     plusquamperfekt futur-1 futur-2) nil)
	     (dolist (mood '(indikativ konjunktiv) nil)
	       (format t "~%~%~10@T~a~2@T~a~2@T~a~%" tense mood "AKTIV")
	       (show-verb-1 stem tense mood 'aktiv entry)
	       (format t "~%~%~10@T~a~2@T~a~2@T~a~%" tense mood "PASSIV")
	       (show-verb-1 stem tense mood 'passiv entry)
	       (if (member tense '(imperfekt plusquamperfekt))
		   (cond ((eq mood 'konjunktiv)
			  (setq *exp-subj-p* t)
			  (format t
				  "~2% umschriebener KONJUNKTIV~2@T~a~2@T~a~%"
				  tense "AKTIV")
			  (show-verb-1 stem tense mood 'aktiv entry)
			  (format t
				  "~2% umschriebener KONJUNKTIV~2@T~a~2@T~a~%"
				  tense "PASSIV")
			  (show-verb-1 stem tense mood 'passiv entry)
			  (setq *exp-subj-p* nil)))))))
	  (t (format t "~% no verb-entry for ~s in the *stem-lexicon*" stem)))))


;;; (changed by [WF], 10/22/93 13:42:51
(defun show-imperativ (stem &optional lex)
  (let ((entry (or lex (find-associate-entry (string-downcase stem)
					     '(verb hilfsverb modalverb)))))
    (setq *exp-subj-p* nil)
    (format t "~% AKTIV ~%")
    (show-imperativ-1
      (verb-inflection stem 'praesens 'imperativ 'aktiv 2 'sg entry)
      'sg)
    (show-imperativ-1
      (verb-inflection stem 'praesens 'imperativ 'aktiv 2 'pl entry)
      'pl)
    (show-imperativ-1
      (verb-inflection stem 'praesens 'imperativ 'aktiv "sie" 'pl entry)
      'pl t)
    (format t "~% PASSIV ~%")
    (show-imperativ-1
      (verb-inflection stem 'praesens 'imperativ 'passiv 2 'sg entry)
      'sg)
    (show-imperativ-1
      (verb-inflection stem 'praesens 'imperativ 'passiv 2 'pl entry)
      'pl)
    (show-imperativ-1
      (verb-inflection stem 'praesens 'imperativ 'passiv "sie" 'pl entry)
      'pl t)))

(defun show-imperativ-1 (conj-result type &optional sie)
  (cond ((stringp conj-result)
	 (if sie (format t "~5@Timperativ anrede: ~a Sie~%" conj-result)
	     (format t "~5@Timperativ ~a: ~a ~%" type conj-result)))
	(sie
	 (format t "~5@Timperativ anrede: ~a Sie " (car conj-result))
	 (dolist (string (cdr conj-result) nil) (format t "~a " string))
	 (terpri))
	(t (format t "~5@Timperativ ~a: " type)
	   (dolist (string conj-result nil) (format t "~a " string))
	   (terpri))))
	 
(defun show-verb-1 (stem tense mood voice &optional lex)
  (let ((entry (or lex (find-associate-entry (string-downcase stem)
					     '(verb hilfsverb modalverb)))))
    (dolist (numerus '(sg pl) nil)
      (dolist (person '(1 2 3) nil)
	(format t "~5@T~12@<~a~>"
		(case numerus
		  (sg (case person
			(1 "ich") (2 "du") (3 "er, sie, es")))
		  (pl (case person
			(1 "wir") (2 "ihr") (3 "sie")))))
	(let ((result (verb-inflection
			stem tense mood voice person numerus entry)))
	  (if (stringp result) (format t "~30@<~a~>" result)
	      (do* ((rest result (cdr rest))
		    (string (car rest) (car rest))
		    (erg-string string (concatenate 'string erg-string " " string)))
		   ((null rest) (format t "~30@<~a~>" erg-string)))))
	(terpri)))))

(defun show-noun (noun)
  (cond ((find-associate-entry (string-downcase noun) 'nomen)
	 (format t "~2%~10@T~15@<~a~>" 'singular)
	 (format t "~20@T~15@<~a~>~2%" 'plural)
	 (dolist (case '(nom gen dat akk) nil)
	   (dolist (numerus '(sg pl) nil)
	     (format t "~5@T~5@<~a:~>" case)
	     (format t "~25@<~a~>" (noun-inflection noun case numerus)))
	   (terpri)) t)
	(t (format t "~% no noun-entry for ~s in the *stem-lexicon*" noun))))

(defun show-adjective (adjective)
  (cond ((find-associate-entry (string-downcase adjective) 'adjektiv)
	 (format t "~3%")
	 (pure-comparation adjective)
	 (show-adjective-1 adjective 'pos)
	 (show-adjective-1 adjective 'kom)
	 (show-adjective-1 adjective 'sup))
	(t (format t "~% no adjective-entry for ~s in the *stem-lexicon*"
		   adjective))))

(defun show-past-participle-intern (verb-stem comparation
				    &optional eventually-lex-entry participle-selector)
  (dolist (number '(sg pl) nil)
    (format t "~2%~43@T~a ~a~%"
	    (case comparation
	      (pos "POSITIV")
	      (kom "KOMPARATIV")
	      (sup "SUPERLATIV"))
	    (case number (sg "SINGULAR") (pl "PLURAL")))
    (format t "~%~20@T~10@<~a~>~15@T~10@<~a~>~15@T~10@<~a~>~%"
	    'mas 'fem 'ntr)
    (dolist (article '(ohne unbestimmt bestimmt) nil)
      (format t "~%-> ~a~%" article)
      (dolist (case '(nom gen dat akk) nil)
	(format t "~a: " case)
	(dolist (gender '(mas fem ntr) nil)
	  (format t "~15@T~10@<~a~>"
		  (past-participle-inflection
		    verb-stem
		    eventually-lex-entry
		    participle-selector
		    comparation t
		    article gender number case)))
	(terpri)))))

(defun show-adjective-1 (adjective comparation)
  (dolist (number '(sg pl) nil)
    (format t "~2%~43@T~a ~a~%"
	    (case comparation
	      (pos "POSITIV")
	      (kom "KOMPARATIV")
	      (sup "SUPERLATIV"))
	    (case number (sg "SINGULAR") (pl "PLURAL")))
    (format t "~%~20@T~10@<~a~>~15@T~10@<~a~>~15@T~10@<~a~>~%"
	    'mas 'fem 'ntr)
    (dolist (article '(ohne unbestimmt bestimmt) nil)
      (format t "~%-> ~a~%" article)
      (dolist (case '(nom gen dat akk) nil)
	(format t "~a: " case)
	(dolist (gender '(mas fem ntr) nil)
	  (format t "~15@T~10@<~a~>"
		  (adjective-inflection adjective comparation t
					article gender number case)))
	(terpri)))))

(defun pure-comparation (adjective)
  (terpri)
  (dolist (comparation '(pos kom sup) nil)
    (format t "    ~a: ~a  " comparation
	    (adjective-inflection adjective comparation nil))))

(defun show-dets&rel (&optional (det 'det))
  (dolist (gender '(mas fem ntr))
    (format t "~%~20@T~a" gender)
    (format t "~2%~10@T~15@<~a~>" 'singular)
    (format t "~20@T~15@<~a~>~2%" 'plural)
    (dolist (case '(nom gen dat akk))
      (dolist (numerus '(sg pl))
	(format t "~5@T~5@<~a:~>" case)
	(format t "~25@<~a~>" (detdef-inflection case numerus gender det)))
      (terpri))))


(defun show-possessives (stem)
  (cond ((find-associate-entry (string-downcase stem) 'possessivpronomen)
	 (dolist (gender '(mas fem ntr) nil)
	   (format t "~%~20@T~a" gender)
	   (format t "~2%~10@T~15@<~a~>" 'singular)
	   (format t "~20@T~15@<~a~>~2%" 'plural)
	   (dolist (case '(nom gen dat akk) nil)
	     (dolist (numerus '(sg pl) nil)
	       (format t "~5@T~5@<~a:~>" case)
	       (format t "~25@<~a~>"
		       (possessive-inflection
			 stem gender numerus case 'artikelwort)))
	     (terpri))))
	(t
	 (format t 
		 "~% no poss-pronoun-entry for ~s in the *stem-lexicon*"
		 stem))))

(defun show-determiner (stem &optional type)
  (cond ((find-associate-entry
	   (string-downcase stem)
	   (case type (indef 'determinativ-indef)
		 (def 'determinativ)
		 (rel 'relativpronomen)))
	  
	 (dolist (gender '(mas fem ntr))
	   (format t "~%~20@T~a" gender)
	   (format t "~2%~10@T~15@<~a~>" 'singular)
	   (format t "~20@T~15@<~a~>~2%" 'plural)
	   (dolist (case '(nom gen dat akk))
	     (dolist (numerus '(sg pl))
	       (format t "~5@T~5@<~a:~>" case)
	       (format t "~25@<~a~>"
		       (if (eq type 'indef)
			   (determiner-indef-inflection
			     stem case numerus gender)
			   (determiner-inflection
			     stem case numerus gender))))
	     (terpri))))
	(t (format t 
		   "~% no entry of type ~s  for ~s in the *stem-lexicon*"
		   type stem))))

(defun show-persprons (person)
  (if (equal 3 person)
      (dolist (gender '(mas fem ntr) nil)
	(show-persprons-1 person gender))
      (show-persprons-1 person)))

(defun show-persprons-1 (person &optional gender)
  (terpri)
  (and gender (format t "~%~30@T~a" gender))
  (format t "~2%~10@T~15@<~a~>" 'singular)
  (format t "~20@T~15@<~a~>~2%" 'plural)
  (dolist (case '(nom gen dat akk) nil)
    (dolist (numerus '(sg pl) nil)
      (format t "~5@T~5@<~a:~>" case)
      (format t "~25@<~a~>"
	      (perspron-inflection person case numerus gender)))
      (terpri)))


(defun show-ordinals (ordinal-stem)
  (dolist (number '(sg pl))
	  (format t "~2%~39@T~a~%"
		  (case number (sg "SINGULAR") (pl "PLURAL")))
	  (format t "~%~17@T~10@<~a~>~15@T~10@<~a~>~15@T~10@<~a~>~%"
		  'mas 'fem 'ntr)
	  (dolist (article '(ohne unbestimmt bestimmt))
		  (format t "~%-> ~a~%" article)
		  (dolist (case '(nom gen dat akk))
			  (format t "~a: " case)
			  (dolist (gender '(mas fem ntr))
				  (format t "~14@T~10@<~a~>"
					  (ordinal-inflection
					   ordinal-stem
					   t article gender number case)))
			  (terpri)))))


;;; (changed by [WF], 10/22/93 13:39:44

(defun all-verb-forms (verb-stem
		       &optional (lex-entry
				   (find-associate-entry
				     (string-downcase verb-stem)
				     '(verb modalverb hilfsverb))))
  (cond (lex-entry
	 (setq *exp-subj-p* nil)
	 (let ((result nil)
	       (tmp nil))
	   (dolist (comp '(;sup
			   ;kom
			   pos))
	     (dolist (numerus '(pl sg))
	       (dolist (article '(ohne unbestimmt bestimmt))
		 (dolist (case '(nom gen dat akk))
		   (dolist (gender '(mas fem ntr))
		     (push (past-participle-inflection
			     verb-stem lex-entry
			     nil comp t article gender numerus case)
			   result)
		     (push (present-participle-inflection
			     verb-stem nil comp t
			     article gender numerus case lex-entry)
			   result)
		     (setq tmp (present-participle-inflection
				 verb-stem t comp t
				 article gender numerus case lex-entry))
		     (if (and tmp (stringp tmp)) (push tmp result))))))
	     (push (past-participle-inflection verb-stem lex-entry nil comp nil)
		   result)
	     (push (present-participle-inflection verb-stem nil comp nil
						  nil nil nil nil lex-entry)
		   result)
	     (setq tmp (present-participle-inflection verb-stem t comp nil
						      nil nil nil nil lex-entry))
	     (if (and tmp (stringp tmp)) (push tmp result)))
	   
	   (dolist (tense '(imperfekt praesens))
	     (dolist (mood '(konjunktiv indikativ))
	       (dolist (numerus '(pl sg))
		 (dolist (person '(3 2 1))
		   (let ((tmp (verb-inflection
				verb-stem tense mood
				'aktiv person numerus lex-entry)))
		     (cond ((stringp tmp) (push tmp result))
			   ((null tmp) (push nil result))
			   (t ;; separated prefix!!
			    (push (first tmp) result)
			    (push (second tmp) result)
			    (push (concatenate 'string
					       (second tmp)
					       (first tmp))
				  result))))))))
	   (if (stringp (setq tmp (verb-inflection verb-stem
						   'praesens
						   'imperativ
						   'aktiv 2 'sg
						   lex-entry)))
	       (push tmp result)
	       (push (car tmp) result))
	   (if (stringp (setq tmp (verb-inflection verb-stem
						   'praesens
						   'imperativ
						   'aktiv 2 'pl
						   lex-entry)))
	       (push tmp result)
	       (push (car tmp) result))
	   (if (stringp (setq tmp (verb-inflection verb-stem
						   'praesens
						   'imperativ
						   'aktiv "sie" 'pl
						   lex-entry)))
	       (push tmp result)
	       (push (car tmp) result))
	   (push (form-infinitive verb-stem nil lex-entry) result)
	   
	   (if (stringp (setq tmp (form-infinitive verb-stem t lex-entry)))
	       (push tmp result))
	   (delete-duplicates result :test #'string-equal :from-end t)))

	(t (format t "~% no verb-entry for ~s in the *stem-lexicon*" verb-stem))))

(defun all-noun-forms (noun-stem &optional lex-entry)
  (let ((entry (or lex-entry
		   (find-associate-entry
		     (string-downcase noun-stem) 'nomen)))
	(result nil))
    (cond (entry
	   (if (pointer-entry-p entry) nil
	       (dolist (number '(pl sg)
			       (delete nil
				       (delete-duplicates
					 result :test #'string-equal
					 :from-end t)))
		 (dolist (case '(akk dat gen nom))
		   (push (noun-inflection noun-stem case number entry)
			 result)))))
	  (t (format t
		     "~% no noun-entry for ~s in the *stem-lexicon*"
		     noun-stem)))))

(defun all-adjective-forms (adj-stem
			    &optional (lex-entry
					(find-associate-entry
					  (string-downcase adj-stem) 'adjektiv)))
  (cond (lex-entry
	 (let ((result nil))
	   (dolist (comp '(sup kom pos))
	     (dolist (numerus '(pl sg))
	       (dolist (article '(ohne unbestimmt bestimmt))
		 (dolist (case '(nom gen dat akk))
		   (dolist (gender '(mas fem ntr))
		     (push (adjective-inflection
			     adj-stem comp t
			     article gender numerus case :lex lex-entry)
			   result)))))
	     (push (adjective-inflection adj-stem comp
					 nil nil nil nil nil :lex lex-entry)
		   result))
	   (delete nil
		   (delete-duplicates result
				      :test #'string-equal
				      :from-end t))))

	(t (format t "~% no adjective entry for ~s in the *stem-lexicon*"
		   adj-stem))))

; MORPHIX: Masks for Clarification-dialog


(defvar %dialog% t)
(defvar %english% t)
(defvar %cancel-sign% 'U)
(defvar *morphix-io* t)

(setq %english%
  (make-array 48 :initial-contents
   '("~%----------------------------------------------------------------------"	; 0
     "~2% What is the FORM in NOMINATIVE SINGULAR of '~a' ?"	; 1 was 5
     "~% for example:    haus   for the input-word \"haeusern\"~
      ~2% if '~a' is a pluraletantum (no singular exists)~
      ~% then enter:  0~2% if ~0@* '~a' is the correct singular form~
      ~% then you can type:  1  instead of typing the word again"	; 2 was 6
     "(an integer is required)"			; 3 was 11
     "~2% Type ~S to abort the clarification-dialog!~
       ~% Else make your choice~:[~;~:* ~a~].~2%  -->  "	; 4
     "~2% What is the FORM in NOMINATIVE PLURAL of '~a' ?"	; 5 was 7
     "~% for example:  geschwister  for the input-word \"geschwistern\"~
      ~2% if '~a' is the correct plural form~
      ~% then you can type:  1  instead of typing the word again"	; 6 was 9
     "~2% What are the correct FORMS in the PLURAL of '~a' ?~
      ~2%~5@T~60,1,1@<NOMINATIV~;DATIV~>"	; 7 was 14
     "~2% What is the GENDER of '~a' ?"		; 8 was 4
     "~2% 1:~5@TMAS~% 2:~5@TFEM~% 3:~5@TNTR"	; 9 was 34
     "~2% What are the correct FORMS IN THE SINGULAR of '~a' ?"	; 10
     "~2% What phenomenon occurs IN THE PLURAL of '~a' ?"	; 11 was 12
     "~% 1: '~a' is a singularetantum (no plural-form exists)~
      ~%          e.g. beginn~
      ~% 2: ~0@*'~a' gets an UMLAUT e.g. vater -> vaeter~
      ~% 3: ~0@*'~a' has an irregular stem in the plural~
      ~%          e.g. atlas -> atlanten~
      ~% 4: ELSE"				; 12 was 13
     "~3% NOTE: -> IT IS NOT POSSIBLE TO FORM 'UMLAUT' FOR: ~a ~%"	; 13 was 15
     "~% for example:    \"kakteen\"  for the input-word \"kaktus\""	; 14 was 8
     "~2% What is the infinitive of '~a' ?"	                        ; 15 was 16
     "~% for example:  \"bekommen\"  for the input-word \"bekaeme\"~
      ~2% if '~a' is the correct infinitive form~
       ~% then you can type:  1  instead of typing the word again"	; 16 was 17
     "~2% Is '~a' a verb with a prefix?"                                ; 17 was 18
     "~2% 1: no prefix, e.g. \"geben\"~
      ~2% 2: '~a' has a prefix which cannot be separated~
       ~%    and the rest-stem is a verb,~
       ~%    e.g. the prefix \"be\" for \"bekommen\"~
      ~2% 3: ~0@*'~a' has a prefix which can be separated~
       ~%    e.g. \"an\" for \"ankommen\""                              ; 18 was 19
     "~2% What is the past participle of '~a' ?"                        ; 19 was 22
     "~% for example:  \"abgeschrieben\"  for the word \"abschreiben\"" ; 20 was 23
     "~2% What is the Auxiliar of '~a' ?"                               ; 21 was 24
     "~2% 0:  SEIN, e.g. \"kommen\"~
      ~2% 1:  HABEN, e.g. \"trinken\""                                  ; 22 was 23
     "~2% What is the prefix of '~a' ?"                                 ; 23 was 20
     "~% for example: \"ab\" for the word \"abschreiben\""              ; 24 was 21
     "~2% What is the category of '~a' ?"	                        ; 25
     "~2% 0:  NOUN ~
      ~% 1:  regular VERB ~
      ~% 2:  ADJECTIVE ~
      ~% 6:  ADVERB ~
      ~% 7:  PARTICLE ~
      ~% 8:  coordinating CONJUNCTION~
      ~% 9:  subordinating CONJUNCTION~
      ~% t:  stop-word (which shall be ignored in MORPHIX)"             ; 26
     "~2% How can '~a' be used ?"                                       ; 27
     "~2% 1: attributive AND in a predicate, as e.g. 'wichtig'~
      ~2% 2: only attributive                as e.g. 'jetzig'~
      ~2% 3: only in a predicate             as e.g. 'schuld'"          ; 28
     "~2% What about inflection and comparison of '~a' ?"               ; 29
     "~2% 1: can be inflected AND compared,     as e.g. 'klein'~
      ~2% 2: can be inflected but NOT compared  as e.g. 'ledig'~
      ~2% 3: NO inflection, NO comparison       as e.g. 'lila'"         ; 30
     "~2% What is the form of '~a' in a predicate position?"            ; 31
     "~% for example:  klein  for the input-word kleinere~
      ~2% if '~a' is the correct form~
      ~% then you can type:  1  instead of typing the word again"       ; 32
     "~2% What is the canonical stem of '~a' ?~
       ~% The form without inflectional suffix is required"             ; 33
     "~% for example:  vorder  for the input-word vorderster~
      ~2% if '~a' is the correct form~
       ~% then you can type:  1  instead of typing the word again"      ; 34
     "~2% How do you construct the form of '~a' in the superlative?"	; 35
     "~% 1: By appending \"st\" at the end of \"~a\"~
      ~%       e.g. klein, kleinst~
      ~2% 2: By appending \"st\" AND UMLAUT required~
      ~%       e.g. klug, kluegst~
      ~2% 3: By appending \"st\" AND UMLAUT possible~
      ~%       e.g. fromm, frommsten, froemmsten~
      ~2% 4: By appending \"est\" at the end of ~0@* \"~a\"~
      ~%       e.g. breit, breitest~
      ~2% 5: By appending \"est\" AND UMLAUT required~
      ~%       e.g. alt, aeltest~
      ~2% 6: By appending \"est\" AND UMLAUT possible~
      ~%       e.g. rot, rotest, roetest~
      ~2% 7: ~0@* '~a' has an irregular stem in the superlative form~
      ~%          e.g. gut, best"                                       ; 36
     "~2% What is the form of '~a' in the comparative form?"            ; 37
     "~% for example:  besser  for the input-word gut"                  ; 38
     "~2% What is the form of '~a' in the superlative form?"            ; 39
     "~% for example:  best  for the input-word gut"                    ; 40
     "~2% Can an Elision of E be done in '~a' ?"	                ; 41
     "~% for example:  \"edel\" -> \"edler\", but not \"schoen\"~
      ~2% 0: NO~
      ~% 1: YES "                                                      ; 42
     "~2% Is '~a' a lemma with an additional category?"                 ; 43
     "~% for example:  \"zeit\" (which is a preposition and a form of a noun)~
      ~2% 0: NO~
      ~% 1: YES "                                                      ; 44

     45 46 47
     )))

(setq %dialog% %english%)


;;; ***************************************************************************
;;; HANDLING THE USER's INPUT

(defvar %clarification-readtable% (copy-readtable))

(defun make-clarification-readtable ()
  (do ((chars '( #\( #\) #\[ #\] #\< #\> #\#
		#\, #\' #\`
		#\" #\"
		#\. #\| #\\ 
		)
	      (cdr chars)))
      ((null chars))
    (set-syntax-from-char
      (car chars) #\Space %clarification-readtable%))) 

(make-clarification-readtable)

(defun user-input-result ()
  (do* ((*readtable* %clarification-readtable%)
	(result (read-from-string (read-line nil) nil nil)
		(read-from-string (read-line nil) nil nil)))
       (result result)))

(defun cancelation (input) (eql %cancel-sign% input))

(defun get-low-string (var)
  (cond ((symbolp var) (string-downcase (symbol-name var)))
	   ;;; no longer necessary ((stringp var) (string-downcase var))
	(t (error "wrong input to the macro get-low-string"))))


    
;;; ******************** MASKS ******************************
;;; PRODUCTION OF OUTPUT FOR THE USER

(defun utter (message-number &rest args)
  (apply #'format *morphix-io* (elt %dialog% message-number) args))

(defun end-mask (&optional additional-string)
  (if additional-string
      (utter 4 %cancel-sign% (elt %dialog% additional-string))
      (utter 4 %cancel-sign% nil)))

(defun separate () (utter 0))

(defun simple-mask (word n1 n2 &optional integer-p)
  ;; n1 and n2 are offsets in the array of control-strings
  ;; which is bound to %dialog%
  (utter n1 word) (separate) (utter n2 word)
  (if integer-p (end-mask 3) (end-mask))
  (user-input-result))

(defun ksg-mask (word gender)
  (let* ((gender-sym (case gender (1 'mas) (2 'fem) (3 'ntr)))
	 (det-in-gen-sing (detdef-inflection 'gen 'sg gender-sym))
	 (det-in-dat-sing (detdef-inflection 'dat 'sg gender-sym)))
    (utter 10 word)
    (format *morphix-io* "~2%~4@T~62,1@<GENITIV~;DATIV~>") (separate)
    (format *morphix-io*
	    "~% 1: ~31@<~a ~a~> ~31@<~a ~a~>~0@*~
	     ~% 2: ~31@<~a ~as~> ~31@<~a ~a~>~0@*~
	     ~% 3: ~31@<~a ~aes~> ~31@<~a ~a{e}~>~0@*~
	     ~% 4: ~31@<~a ~a{e}s~> ~31@<~a ~a{e}~>~0@*~
	     ~% 5: ~31@<~a ~ases~> ~31@<~a ~a{se}~>~0@*~
	     ~% 6: ~31@<~a ~aens~> ~31@<~a ~aen~>~0@*~
	     ~% 7: ~31@<~a ~ans~> ~31@<~a ~an~>~0@*~
	     ~% 8: ~31@<~a ~an~> ~31@<~a ~an~>~0@*~
	     ~% 9: ~31@<~a ~aen~> ~31@<~a ~aen~>"
	    det-in-gen-sing word det-in-dat-sing word)
    (separate) (end-mask 3) (user-input-result)))

(defun kpl-mask (word)
  (utter 7 word) (separate)
  (format *morphix-io*
	  "~% 1:  ~31@<die ~a~> ~31@<den ~a~>~0@*~
           ~% 2:  ~31@<die ~as~> ~31@<den ~as~>~0@*~
           ~% 3:  ~31@<die ~an~> ~31@<den ~an~>~0@*~
           ~% 4:  ~31@<die ~aen~> ~31@<den ~aen~>~0@*~
           ~% 5:  ~31@<die ~anen~> ~31@<den ~anen~>~0@*~
           ~% 6:  ~31@<die ~a~> ~31@<den ~an~>~0@*~
           ~% 7:  ~31@<die ~ae~> ~31@<den ~aen~>~0@*~
           ~% 8:  ~31@<die ~ase~> ~31@<den ~asen~>~0@*~
           ~% 9:  ~31@<die ~aer~> ~31@<den ~aern~>~0@*~
           ~% 10: ~31@<die ~aien~> ~31@<den ~aien~>~0@*~
           ~% 11: ~31@<die ~asen~> ~31@<den ~asen~>~0@*~
           ~% 12: ~31@<die ~aten~> ~31@<den ~aten~>~0@*~
           ~% 13: ~31@<die ~ate~> ~31@<den ~aten~>"
	  word word)
  (separate) (end-mask 3) (user-input-result))

(defun kpl-uml-mask (word)
  (utter 7 word) (separate)
  (format *morphix-io*
     "~% 1:  ~31@<die ~a~> ~31@<den ~a~>~0@*~
      ~% 4:  ~31@<die ~aen~> ~31@<den ~aen~>~0@*~
      ~% 6:  ~31@<die ~a~> ~31@<den ~an~>~0@*~
      ~% 7:  ~31@<die ~ae~> ~31@<den ~aen~>~0@*~
      ~% 9:  ~31@<die ~aer~> ~31@<den ~aern~>~0@*~
      ~% 13: ~31@<die ~ate~> ~31@<den ~aten~>"
     word word)
  (separate) (end-mask 3) (user-input-result))

(defun kpl-plural-mask (word)
  (utter 7 word) (separate)
  (format *morphix-io*
      "~% 1:  ~31@<die ~a~>~:* ~31@<den ~a~>~:*~
       ~% 6:  ~31@<die ~a~>~:* ~31@<den ~an~>" word)
  (separate) (end-mask 3) (user-input-result))

(defun encode-noun-entry (gender uml ksg kpl)
  (+ 100000                    ;; a noun
     (* gender 10000)         ;; gender position
     (if uml (* 1000 uml) 0) ;; umlaut position
     (if ksg (* 100 ksg) 0)  ;; singular-classification
     (or kpl 0)))             ;; plural-classification

(defun encode-verb-entry (vtyp vgfa prefix-type paux prefixlength)
  (+ 200000
     (* 1000 (+ (* 12 vgfa) vtyp))
     (* 100 (cond ((eql 0 paux)
		   (cond ((not prefix-type) 0)
			 ((eql 2 prefix-type) 0)
			 (t 2)))
		  ((eql 1 paux)
		   (cond ((not prefix-type) 1)
			 ((eql 2 prefix-type) 1)
			 (t 3)))))
     prefixlength))

(defun encode-adj-entry (komp uml elision pred-e main)
  (+ 300000
      (* komp 10000)
      (* uml 1000)
      (* elision 100)
      (* pred-e 10)
      main))

(defun encode-fullform-entry (cat additional-algorithmic)
  (+ (if (eql 1 additional-algorithmic) 10000 0)
      (* cat 100000)))

; ---------------------------------------------------------------------------

(defun ask-category (word)
  (let ((cat (handle-masks 8 (simple-mask word 25 26 t))))
    (case cat
      (0 (ask-nouns word))
      (1 (ask-verbs word))
      (2 (ask-adjectives word))
      ((6 7 8 9) (save-fullform-lexicon-entry
	   word (encode-fullform-entry
		  cat
		  (handle-masks 12 (simple-mask word 43 44)))))
      (t (save-fullform-lexicon-entry
	   word (encode-fullform-entry 20 0))))))

;;; ***************************************************************************
;;; CATEGORY SPECIFIC CLARIFICATION DIALOG

(defun ask-nouns (word)
  (let ((canonical-stem (handle-masks 3 (simple-mask word 1 2))))
    (case canonical-stem
      (0 (ask-nouns1 word))
      ;; no singular stem can be defined. word is a pluraletantum.
      ;; no umlautung, kpl is 1 or 6, no gender to be set!
      (1 (ask-nouns2 word word))
      ;; there is a singular stem. It is identical with the parameter value
      (otherwise (ask-nouns2 word canonical-stem)))))

(defun ask-nouns1 (word)
  ;; noun clarification dialog for pluraletantum
  (let ((canonical-stem (handle-masks 5 (simple-mask word 5 6))))
    (if (eql 1 canonical-stem) (setq canonical-stem word))
    (save-stem-lexicon-entry
      canonical-stem
      (encode-noun-entry
	0 0 0 (handle-masks 9 (kpl-plural-mask canonical-stem))))))

(defun ask-nouns2 (word canonical-stem)
  ;; ask gender and singular classification
  (let ((gender (handle-masks 1 (simple-mask canonical-stem 8 9 t))))
    (ask-nouns3 word canonical-stem gender
		(handle-masks 2 (ksg-mask canonical-stem gender)))))

(defun ask-nouns3 (word canonical-stem gender ksg)
  (case (handle-masks 4 (simple-mask canonical-stem 11 12 t))
    (1 ;; a singularetantum. no plural-information to ask
      (save-stem-lexicon-entry canonical-stem
			       (encode-noun-entry gender 0 ksg 0)))
    (2 ;; plural-form will be built with umlautung
      ;; test whether umlautung is possible.
      (let ((uml-stem (umlautung canonical-stem)))
	(cond ((= (length canonical-stem) (length uml-stem))
	       ;; not possible to umlaut
	       (utter 13 canonical-stem)
	       (ask-nouns3 word canonical-stem gender ksg))
	      (t (save-stem-lexicon-entry
		   canonical-stem
		   (encode-noun-entry
		     gender 1 ksg
		     (handle-masks 7 (kpl-uml-mask uml-stem))))))))
    (3 ;; plural will be built with an irregular stem
      ;; if this stem is correctly entered,
      ;; create two new lex-entries
      (let ((irreg-plural-stem
	      (handle-masks 6 (simple-mask canonical-stem 5 14))))
	(save-stem-lexicon-entry
	  canonical-stem
	  (list (encode-noun-entry gender 0 ksg 0)
		(cons 'PLURAL irreg-plural-stem)))
	(save-stem-lexicon-entry
	  irreg-plural-stem
	  (list (encode-noun-entry gender 0 0 1)
		(cons 'STAMM (cons canonical-stem 'PLURAL))))))
    (4 (save-stem-lexicon-entry
	 canonical-stem
	 (encode-noun-entry
	   gender 0 ksg
	   (handle-masks 10 (kpl-mask canonical-stem)))))))

(defun ask-verbs (word)
  (let* ((infinitiv-result (handle-masks 5 (simple-mask word 15 16)))
	 (inf-stem (if (eql 1 infinitiv-result) word infinitiv-result))
	 (inf-length (length inf-stem))
	 (canonical-stem
	   (cond ((and (> inf-length %minimal-stem-length%)
		       (eql #\n (elt inf-stem (1- inf-length))))
		  (if (eql #\e (elt inf-stem (- inf-length 2)))
		      (subseq inf-stem 0 (- inf-length 2))
		      (subseq inf-stem 0 (1- inf-length)))))))
    (if (not canonical-stem)
	(ask-verbs word)
	(ask-verbs1 inf-stem canonical-stem))))

(defun ask-verbs1 (inf stem)
  ;; now ask the prefix-info
  (let ((pref-sep-p (handle-masks 1 (simple-mask inf 17 18 t))))
    (if (eql pref-sep-p 1)			; no prefix
	(ask-verbs2 inf stem nil 0)
	(ask-verbs3 inf stem pref-sep-p))))	; ask prefix

(defun ask-verbs2 (inf stem prefix-p prefixlength)
  ;; now ask the past participle
  (let ((pprf (handle-masks 6 (simple-mask inf 19 20)))
	(paux (handle-masks 8 (simple-mask inf 21 22 t))))
    (ask-verbs4 stem prefix-p prefixlength pprf paux)))

(defun ask-verbs3 (inf stem pref-sep-p)
  (let ((prefix (handle-masks 6 (simple-mask inf 23 24))))
    (cond ((not (prefix-exists-p prefix))
	   (insert-prefix prefix) (save-prefix prefix)))
    (and (ask-verbs2 inf stem pref-sep-p (length prefix))
	 (additional-verb-dialog stem inf prefix))))

(defun additional-verb-dialog (stem inf-stem prefix)
  (let ((prefix-length (length prefix)))
    (if (double-consonant-prefix-p prefix prefix-length)
	(let ((next-char (elt stem prefix-length))
	      (verb (subseq stem prefix-length))
	      (verb-inf (subseq inf-stem prefix-length))
	      (verb2 (subseq stem (1- prefix-length)))
	      (verb2-inf (subseq inf-stem (1- prefix-length))))
	  (cond ((vowel-p next-char)	  ; check both stems
		 (if (not (find-associate-entry verb '(verb hilfsverb modalverb)))
		     (if (not (find-associate-entry verb2 '(verb hilfsverb modalverb)))
			 (or (ask-verbs verb2-inf) (ask-verbs verb-inf)))))
		((eql next-char (elt prefix (1- prefix-length)))
		 ;; warn the user, prefixverb is not ok!
		 nil)
		((not (find-associate-entry verb '(verb hilfsverb modalverb)))
		 (ask-verbs verb-inf))))
	(let ((verb (subseq stem prefix-length))
	      (verb-inf (subseq inf-stem prefix-length)))
	  (if (not (find-associate-entry verb '(verb hilfsverb modalverb)))
	      (ask-verbs verb-inf))))))

(defun ask-verbs4 (stem prefix-p prefixlength pprf paux)
  ;;  determine-vgfa
  (cond ((not prefix-p)
	 (let* ((can-length (length stem))
		(last-char (aref stem (1- can-length)))
		(vgfa
		  (cond ((member last-char '(#\d #\t)) 2)
			((member last-char '(#\s #\z #\x)) 4)
			((member last-char '(#\m #\n))
			 (let ((prev-char (aref stem (- can-length 2))))
			   (if (not (member prev-char
					    '(#\a #\e #\i #\o #\u #\l #\r #\m #\n)))
			       2 1)))
			((eql last-char #\l)
			 (let ((prev-char (aref stem (- can-length 2))))
			   (if (eql prev-char #\e)
			       ;; the following case is necessary to
			       ;; handle "schael" "spiel"...
			       (if (vowel-p (aref stem (- can-length 3)))
				   (if (and (>= (- can-length 4) 0)
					    (eql (aref stem (- can-length 3))
						 #\u)
					    (member (aref stem (- can-length 4))
						    '(#\a #\e)))
				       5 1) 5) 1)))
			((eql last-char #\r)
			 (let ((prev-char (aref stem (- can-length 2))))
			   (if (eql prev-char #\e)
			       ;; the following case is necessary to
			       ;; handle "erklaer"...
			       (if (vowel-p (aref stem (- can-length 3)))
				   (if (and (>= (- can-length 4) 0)
					    (eql (aref stem (- can-length 3))
						 #\u)
					    (member (aref stem (- can-length 4))
						    '(#\a #\e)))
				       6 1) 6) 1)))
			((equal stem "soll") 3)
			;; ((member stem '("sei" "tu")) 7)
			;; yet in irreg-verbs!
			(t 1))))
	   (cond ((member vgfa '(5 6))
		  (let ((elision-stem (e-elision stem)))
		    (save-stem-lexicon-entry
		      stem
		      (list (encode-verb-entry 1 vgfa nil paux 0)
			    (cons 'PPRF pprf)
			    (cons 'VGFA+ (cons (if (eql 5 vgfa) 'A5 'A6)
					       elision-stem))))
		    (save-stem-lexicon-entry
		      elision-stem (list (cons 'STAMM (cons stem 'VGFA+))))) t)
		 (t (save-stem-lexicon-entry
		      stem
		      (list (encode-verb-entry 1 vgfa nil paux 0)
			    (cons 'PPRF pprf))) t))))
	(t (save-stem-lexicon-entry
	     stem
	     (list (encode-verb-entry 0 0 prefix-p paux prefixlength)
		   (cons 'PPRF pprf))) t)))

(defun ask-adjectives (word)
  ;; dialog for the classification information A1, A2, A3, B1, B2, B3, C1
  ;; which is stored as value of the arc 'main' in an entry of an adjective
  (case (handle-masks 1 (simple-mask word 27 28 t))
    (1 (ask-adjectives1 word (handle-masks 1 (simple-mask word 29 30 t))))
    (2 (ask-adjectives1 word (+ 3 (handle-masks 1 (simple-mask word 29 30 t)))))
    (3 (ask-adjectives1 word 7))))

(defun ask-adjectives1 (word main)
  ;; now determine the canonical stem. It will be the key of the entry
  (let* ((asked-adj-stem
	   (if (not (member main '(4 5 6)))
	       (handle-masks 5 (simple-mask word 31 32))
	       (handle-masks 5 (simple-mask word 33 34))))
	 (adj-stem (if (eql 1 asked-adj-stem) word asked-adj-stem)))

    (case main
      ((1 4) (ask-adjectives2 adj-stem main)) ;; inflection and comparison
      ((3 6 7)                                ;; no inflection, no comparison
       (save-stem-lexicon-entry adj-stem (encode-adj-entry 0 0 0 0 main)))
      ((2 5)                                  ;; no comparison
       (let* ((adj-length (length adj-stem))
	      (last-char (and (>= adj-length %minimal-stem-length%)
			      (elt adj-stem (1- adj-length)))))
	 (if last-char
	     (cond ((eql #\e last-char)	  ; case of pred-e t but no elision
		    (save-stem-lexicon-entry
		      (subseq adj-stem 0 (1- adj-length))
		      (encode-adj-entry 0 0 0 1 main)))
		   ((eql #\e (elt adj-stem (- adj-length 2)))
		    (cond ((and (member last-char '(#\l #\r #\n))
				(eql 1 (handle-masks 12 (simple-mask word 41 42))))
			   (save-stem-lexicon-entry
			     adj-stem
			     (encode-adj-entry
			       0 0 (if (eql #\l last-char) 1 2) 0 main))
			   (save-stem-lexicon-entry
			     (e-elision adj-stem)
			     (list (cons 'STAMM (cons adj-stem 'ELISION)))))
			  (t (save-stem-lexicon-entry
			       adj-stem (encode-adj-entry 0 0 0 0 main)))))
		   (t (save-stem-lexicon-entry
			adj-stem (encode-adj-entry 0 0 0 0 main))))))))))

(defun ask-adjectives2 (adj-stem main)
  ;; ask for comparison, check whether pred-e occurs!
  (let* ((adj-length (length adj-stem))
	 (last-char (and (>= adj-length %minimal-stem-length%)
			 (elt adj-stem (1- adj-length))))
	 (pred-e (eql last-char #\e))
	 (canonical-stem (if pred-e
			     (subseq adj-stem 0 (1- adj-length))
			     adj-stem)))
    (if last-char
	(let ((kom-result
		(handle-masks 11 (simple-mask canonical-stem 35 36 t))))
	  (funcall (if (eql kom-result 7)
		       #'ask-adjectives4 #'ask-adjectives3)
		   canonical-stem main pred-e
		   (adjective-elision-cat
		     adj-stem adj-length last-char pred-e)
		   kom-result)))))

(defun adjective-elision-cat (canonical-stem adj-length last-char pred-e)
  (cond (pred-e 0)
	((eql #\e (elt canonical-stem (- adj-length 2)))
	 (if (and (member last-char '(#\r #\n #\l))
		  (eql 1 (handle-masks 12 (simple-mask canonical-stem 41 42))))
	     (if (eql last-char #\l) 1 2) 0))
	(t 0)))

(defun ask-adjectives3 (stem main pred-e elision kom-result)
  ;; kom-result is one of 1 2 3 4 5 6
  ;; (st, st-u-nec, st-u-pos, est, est-u-nec, est-u-pos)
  (case kom-result
    ((1 2 3)
     (save-stem-lexicon-entry
       stem (encode-adj-entry
	      1 (1- kom-result) elision (if pred-e 1 0) main)))
    (otherwise
      (save-stem-lexicon-entry
	stem (encode-adj-entry
	       2 (- kom-result 4) elision (if pred-e 1 0) main)))))

(defun ask-adjectives4 (stem main pred-e elision kom-result)
  (ignore kom-result)
 (let* ((asked-kom-stem (handle-masks 6 (simple-mask stem 37 38)))
	(asked-sup-stem (handle-masks 6 (simple-mask stem 39 40))))

   ;; first, the base entry
   (save-stem-lexicon-entry
     stem
     (list (encode-adj-entry 3 0 elision (if pred-e 1 0) main)
	   (cons 'KOM asked-kom-stem) (cons 'SUP asked-sup-stem)))
   (save-stem-lexicon-entry
     asked-kom-stem (list (cons 'STAMM (cons stem 'KOM))))
   (save-stem-lexicon-entry
     asked-sup-stem (list (cons 'STAMM (cons stem 'SUP))))))


;;; **********************************************************************
;;; SAVING OF RESULTS AFTER A CLARIFICATION-DIALOG FOR A WORD

(defun save-stem-lexicon-entry (stem entry)
  (with-open-file
      (templex-stream *tmplex*
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create)
      (let ((form `(l-s ,stem ,(if (numberp entry) entry (list 'quote entry)))))
	(eval form)
	(format templex-stream "~%~s" form))))

(defun print-unknowns () 
  (maphash #'(lambda (k v) (cond ((eq t v) (print k result))))
	     %morphix-results%))

(defun get-unknowns () 
  (let ((result nil))
    (maphash #'(lambda (k v) (cond ((eq t v) (push k result))))
	     %morphix-results%)
    result))


(defun show-results-to-file (file)
  (with-open-file
    (s file
       :direction :output
       :if-exists :append
       :if-does-not-exist :create)
    (dolist (lines *whole-input*)
      (dolist (line lines)
	(dolist (elem line)
	  (cond ((stringp elem)
		 (format s "~%~S: ~S" elem (gethash elem %morphix-results%)))
		(t (format s "~%   ~S" elem))))))))

(defun save-fullform-lexicon-entry (stem entry)
  (with-open-file
      (templex-stream *tmplex*
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create)
      (let ((form `(l-f ,stem ,(if (numberp entry) entry (list 'quote entry)))))
	(eval form)
	(format templex-stream "~%~s" form))))

(defun save-lex-entry (stem entry lexicon)
    (with-open-file
      (templex-stream *tmplex*
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create)
      (let ((form (list (if (eq lexicon *stem-lexicon*) 'l-s 'l-f)
			stem
			(if (numberp entry) entry (list 'quote entry)))))
	(eval form)
	(format templex-stream "~%~s" form))))

(defun save-prefix (prefix)
  (with-open-file
      (prefixlex-stream *prefixlex*
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create)
    (format prefixlex-stream "~%(I-P ~s)" prefix)))

(defun copy-tmplex-to-userlex ()
  (if (probe-file *tmplex*)
      (with-open-file
	(userlex *userlex*
		 :direction :output
		 :if-exists :append
		 :if-does-not-exist :create)
	(with-open-file (templex-stream *tmplex*
					:direction :input)
	  (do ((char (read-char templex-stream nil 'end)
		     (read-char templex-stream nil 'end)))
	      ((eql char 'end))
	    (write-char char userlex)))
	(delete-file *tmplex*))))

(defun save-entries () (copy-tmplex-to-userlex))



; ---------------------------------------------------------------------------
;;; TOPLEVEL ABBREVIATIONS FOR FUNCTIONS
; GENERATION

(defun s-v (verb-stem) (show-verb verb-stem))
(defun s-i (verb-stem) (show-imperativ verb-stem))
(defun s-v-1 (stem tense mood voice)
 (terpri) (show-verb-1 stem tense mood voice))

(defun s-n (noun-stem) (show-noun noun-stem))

(defun s-a (adjective-stem) (show-adjective adjective-stem))

(defun s-a-1 (adjective-stem comparation)
 (show-adjective-1 adjective-stem comparation))

(defun s-p (possessive-stem) (show-possessives possessive-stem))

(defun s-pe (person) (show-persprons person))

; ---------------------------------------------------------------------------
; ANALYSIS

(defun eingabe (word-list) (input word-list))
(defun satz-analyse (word-list) (morphix word-list))
(defun ruecksetzen () (reset-results))
(defun wort-analyse (word &optional (fail-val *fail-val*)) 
  (word-analysis word fail-val))
					  
(defun W-A (word &optional (fail-val *fail-val*)) 
  (word-analysis (first (morphix-read word)) fail-val))

(defun ANALYZE-LINE (input-string)
  (input (morphix-read input-string)))

; ---------------------------------------------------------------------------
; LEXICON ACCESS

(defun l-f (key value) (lexicon-insert key value *fullform-lexicon*))
(defun l-s (key value) (lexicon-insert key value *stem-lexicon*))
(defun l-i (key value lexicon) (lexicon-insert key value lexicon))
(defun i-p (prefix) (insert-prefix prefix))

(defun l-z (key table) (get-lex-entry key table))
(defun c-s (key) (compute-stem-entry key))
(defun c-f (key) (compute-fullform-entry key))

(defun l-r (key table) (delete-entry key table))

; ---------------------------------------------------------------------------
; CLARIFICATION DIALOG

(defun c-d (word &optional (fail-val *fail-val*)) 
  (dialog-for word fail-val))

