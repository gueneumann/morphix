;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: make-ff-lexicon
;;;      module: morphix
;;;     version: 1.0
;;;  written by: Guenter Neumann
;;; last update: 4/2/99
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Used for creating fullform lexica processable by Kubas C++ Tries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use it:
;;; - (init-smes)
;;; - (init-dnf) selecting lisp list

;;; problems:
;;; - umlauting -> only use Xe writing style (done)

;;; - douple occurency
;;; 
;;; It seems that douple occurrency is a problem wrt separable verb prefix
;;; (e.g., aufhaben -> habe auf
;;;
;;; Features used:
;;; gender, case, number, comp, comp-f, det, tense, person, form, sym
;;; 
(in-package :mo)

(defun init-ff (&key (system :sppc))
  (setq morphix::*relevant-features* 
    '(:SYM :COMP :COMP-F :DET :TENSE :FORM :PERSON :GENDER :NUMBER :CASE))

  (case system
    (:sppc (set-feat))
    (:python (set-vec))
    (otherwise (set-sym))
    )
  )

;;; Obacht: Die Definition ist unterschieldich zu der in main.lisp
;;; wo tmp result berechntet wird

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
			    ;;(push (first tmp) result)
			    ;;(push (second tmp) result)
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
             ;;(push (car tmp) result)
             )
	   (if (stringp (setq tmp (verb-inflection verb-stem
						   'praesens
						   'imperativ
						   'aktiv 2 'pl
						   lex-entry)))
	       (push tmp result)
             ;;(push (car tmp) result)
             )
	   (if (stringp (setq tmp (verb-inflection verb-stem
						   'praesens
						   'imperativ
						   'aktiv "sie" 'pl
						   lex-entry)))
	       (push tmp result)
             ;;(push (car tmp) result)
             )
	   (push (form-infinitive verb-stem nil lex-entry) result)
	   
	   (if (stringp (setq tmp (form-infinitive verb-stem t lex-entry)))
	       (push tmp result)
             )
	   (delete-duplicates result :test #'string-equal :from-end t)))

	(t (format t "~% no verb-entry for ~s in the *stem-lexicon*" verb-stem))))

;;; this version is buggy, because it cannot handle plural stems which do have multiple stems
;;; in order to correct it, I would need the singular stem for unique identification

;;; seems to work well but still have to be integrated into main.lisp file

(defun get-kpl-from-irregular-stem (sg-entry entry &optional &aux kpls)
  (cond ((multiple-computed-entry-p entry)
	 (loop for x in (rest entry) collect
	      (when (string-equal sg-entry
				  (cadr (assoc 'stamm x)))
		(pushnew (cdr (assoc 'kpl x)) kpls)))
	 (when (rest kpls) 
	   (warn "Different KPL classes for entry ~s with same sg-stem ~s" 
		 entry sg-entry))
	 (first kpls))
	(T (cdr (assoc 'kpl entry)))))

(defun noun-search-path (number entry &optional sg-entry)
  (cond ((eq 'sg number)
	 (let ((ksg (cdr (assoc 'ksg entry)))
	       (uml (if (assoc 'uml entry) 'uml+ 'uml-)))
	      (list 'nomen 'red- uml 'sg ksg)))
	((eq number 'pl)
	 (let ((irreg-plural-stem (cdr (assoc 'plural entry))))
	   (cond (irreg-plural-stem
		  (let ((kpl (get-kpl-from-irregular-stem sg-entry (compute-stem-entry
							   irreg-plural-stem))))
		       (list 'nomen 'red- 'uml- 'pl kpl)))
		 (t (let ((kpl (cdr (assoc 'kpl entry)))
			  (uml (if (assoc 'uml entry) 'uml+ 'uml-)))
			 (list 'nomen (if (eq uml 'uml+)
					  'red+ 'red-) uml 'pl kpl))))))))

(defun noun-inflection (stem case number
			&optional (entry
				    (find-associate-entry
				      (string-downcase stem) 'nomen))
			&aux (internal-stem (string-downcase stem)))
  (if entry
      (do* ((rest-suffix-list %noun-suffix-list% (cdr rest-suffix-list))
	    (suffix (car rest-suffix-list) (car rest-suffix-list))
	    (surface-stem (surface-noun-stem internal-stem number entry))
	    (search-path (noun-search-path number entry internal-stem))
	    (result nil))
	   ((null rest-suffix-list) nil)
	   ;; loop over relevant suffixes
	(cond ((setq result
		     (noun-result
		      search-path surface-stem case suffix
		      (get-suffix-information suffix)))
	       (return (capitalize-noun result (cdr (assoc 'CAPS entry)))))))))

(in-package :mo)

(defun enumerate-stems (path node)
  (let ((leaf (cdr (assoc :info (dtree::node-content node)))))
    (when leaf
      (multiple-value-bind (res err)
          (ignore-errors (handle-stem-key 
                          (format nil "~{~A~}" path)
                          (expand-morph-stem-entry leaf)))
        (when err (format T "~&~{~A~}: error ~a~%" path err))))))

(defun enumerate-fullforms (path node)
  (let ((leaf (cdr (assoc :info (dtree::node-content node)))))
    (when leaf
      (multiple-value-bind (res err)
          (ignore-errors (handle-fullform-key 
                          (format nil "~{~A~}" path)))
        (when err (format T "~&Error occured: ~a~%" err))))))

;;; update of function in gen-parse.lisp:
;;; does not perform umlautung!

(defun analyse-all-forms (wf-list)
  (let ((*clarification-dialog-on* nil)
        (*property-retrieval* nil))
    (mapcar #'(lambda (wf)
                (funcall (get 'gen-parse :output-fct) 
                         wf             ; GN changed
                         (funcall (get 'gen-parse :morph-fct) 
                                  wf))) ; GN changed
            wf-list)))


;; this function is used for creating ascci-based fullform lexica
;;; using the feature vector representation

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
                            (concatenate 'string
                              (format NIL "~a ~a "
                                      stem 
                                      (tripple-pos x))
                              (get-value-feats (tripple-morph-type x))
                              ))))
    (when new-reading
      (format stream  "~%~a: ~{~a; ~}" word new-reading))))

;;; a cache for the key-value hashtable
(defvar *reading-hash* (make-hash-table :test #'equal))
(setq *reading-hash* (make-hash-table :test #'equal))

(defvar *val-key-map* nil)
(setq *val-key-map* nil)

;;; reversing the key-val cache
(defun save-val-key-map ()
  (setq *val-key-map*
	(loop for key being the hash-key of *reading-hash* using (hash-value val)
	   collect  (cons val key))))

(defvar *reading-cnt* 0)
(setq *reading-cnt* 0)

(defun reading-insert-hash (reading)
  (multiple-value-bind (val found)
      (gethash reading *reading-hash*)
    (if found val
      (progn 
	(setf (gethash reading *reading-hash*) 
	      (incf *reading-cnt*))
	*reading-cnt*))))

(defun encode-single-reading (x)
  (let ((reading (if (stringp x) x
		      (loop for elem in x
			 append
			   (list (car elem) (rest elem))))))
    (reading-insert-hash reading)
    )
  )

(defun get-value-feats (vec)
  (let ((xx (loop for x in vec collect (encode-single-reading x))))
    (if (or (numberp (car xx)) (stringp (car xx)))
	;; if feat is represented as a symbol
	(format NIL "~{~a ~}" xx)
	(format NIL "~{~{~a ~} ~}" xx))))

;;; top-level function
;;; outfile: the file storage
;;; expr: a regular expression
;;; lexer: either enumerate-stem-types OR enumerate-fullform-types

(defun make-ascii-lex (outfile expr lexer
                          &aux  (tofile (merge-pathnames (pathname outfile)
                                                         *out-dir*)))
  
  (let* ((*out-stream* (open tofile :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)))
         
    (declare (special *out-stream*))
    (setf (get 'gen-parse :morph-fct) 
      #'(lambda (wf) (rt (car (morphix-read wf)))))
    (setf (get 'gen-parse :output-fct) 
      #'(lambda (a b)
          (morphix2dnf-interface a b *out-stream*)))
    
    (funcall lexer expr)
    
    (close *out-stream*)))

;;; abbreviation for *fullfrom-lexicon*

(defun init-make-ff ()
   (setq *reading-hash* (make-hash-table :test #'equal))
   (setq *val-key-map* NIL)
   (setq *reading-cnt* 0)
   (init-ff :system :symbol))

(defun init-make-cocw ()
   (setq *reading-hash* (make-hash-table :test #'equal))
   (setq *val-key-map* NIL)
   (setq *reading-cnt* 0)
   )

;;; make sure to call (init-make-ff)

(defun make-ccw-lex (&optional (file "ccw-ff.txt"))
  (make-ascii-lex file ".*" #'enumerate-fullform-types))

(defun make-ocw-lex (&optional (file "ocw-ff.txt"))
  (make-ascii-lex file ".*" #'enumerate-stem-types))


;;; identify homographs

(defvar *homographs* NIL)
(setq *homographs* NIL)

(defun list-homographs (path node)
  (let ((leaf (cdr (assoc :info (dtree::node-content node)))))
    (when leaf
      (let* ((word (format nil "~{~A~}" path))
             (lex-entry (compute-fullform-entry word))
             (multiple-entry-p (multiple-computed-entry-p lex-entry)))
        (when (marked-as-homograph lex-entry multiple-entry-p)
          (push (format nil "~{~A~}" path) *homographs*))))))
      
 
(defun enumerate-homographs (expr)
  (declare (special *homographs*))
  (setq *homographs* NIL)
  (dtree::match-entry expr *fullform-lexicon*
                      :output-fn #'list-homographs)
  *homographs*
)
                               

(setq *homographs*
  '("zwecks" "zeit" "weit" "wenig" "werktags" "weg" "vorab" "vormittags"
 "vormittag" "vormals" "viel" "vergebens" "unser" "ungleich" "teils" "trotz"
 "treu" "sondern" "sonntags" "sonnabends" "strenggenommen" "still" "selten"
 "seiner" "samstags" "samt" "ringsum" "rings" "rum" "rundweg" "rechts" "recht"
 "notfalls" "noerdlich" "namens" "nahe" "nachmittags" "nachmittag" "morgens"
 "morgen" "montags" "meiner" "mittwochs" "mittags" "mittels" "mitgerechnet"
 "mal" "mangels" "lange" "laut" "jenseits" "ihrer" "ihr" "hoechst" "heute"
 "gleich" "gestern" "gerade" "getrennt" "falls" "freitags" "frei" "feiertags"
 "fest" "ferne" "fern" "eher" "ehe" "eben" "euer" "euch" "erst" "eigentlich"
 "eingeschlossen" "eingangs" "einbegriffen" "kraft" "bloss" "bestimmt"
 "bestehen" "betreffend" "bar" "alltags" "ausgenommen" "ausgangs" "angesichts"
 "anfangs" "aehnlich" "abends" "abend" "abseits" "donnerstags" "dank"
 "dienstags" "diesbezueglich" "deiner"))
