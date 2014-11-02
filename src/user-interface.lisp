;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: paradice-interface.lisp
;;;      module: morphix
;;;     version: 4.0
;;;  written by: Guenter Neumann (Copyrights)bad
;;; last update: 26.1.96
;;;  updated by: Guenter Neumann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MORPHIX")

(defvar *cat2pos* nil)
(setq *property-retrieval* T)
(setq *clarification-dialog-on* nil)

;;; the value of this variable is defined in file
;;; type-interface.lisp of system morphix-lexicon

(defun cat2pos (cat)
  (declare (special *cat2pos*))
  (let ((pos (rest (assoc cat *cat2pos* :test #'eq))))
    (if pos pos (symbol-name cat))))

(defun return-morphix-tags (res)
  (loop 
      for reading in res 
      for cat =
        (first (last (assoc 'WORTART (rest reading))))
      for flex = (second (assoc 'flexion (rest reading)))
      collect
        (cons (first reading) (cons flex (cat2pos cat)))))

;;; dispatcher for the determiniation of the morphological type
;;; representation

(setf (get 'mapper  :morph-fct) #'return-morphix-tags)

(defun inner-rt (wf  &key redundant?)
  (let ((res (word-analysis wf)))
    (if (or (atom res)
            (atom (second (first res))))
        `((,wf  ,*fail-val* . ,*fail-val*))
      (let ((mapping-result (funcall (get 'mapper :morph-fct) res)))
        (if redundant?
            mapping-result
          (delete-if #'(lambda (x) 
                         (eq (first (rest x)) :wrong))
                     (remove-duplicates mapping-result :test #'equal)))))))

(defun rt (wf &key (redundant? nil))
  (inner-rt wf :redundant? redundant?))
 
;;; this is the top-level function used inside the smes system
;;; it is used to integrate composita handling into the overall
;;; system in a very straightforward way
;;; it just maps lists of stems into a string

;;; this variable controls, whether NC should be printed as one
;;; word (GN: 16/6/97, made for SMueller)

(defvar *composita-sepp* NIL)
(setq *composita-sepp* T)
(defvar *separator-char* nil)
(setq *separator-char* #\-)
    
(defun outer-rt (wf)
  (let* ((scanned-word (first (morphix-read wf)))
         (res-rt (inner-rt scanned-word)))
    (loop for elem in res-rt collect
          (if (consp (first elem))
              (cons
               (if *composita-sepp*
                   (make-nc-string (car elem) *separator-char*)
                 (make-nc-string- (car elem)))                           
               (rest elem))
            elem))))

;;; concatenate strings with delimiter

(defun make-nc-string (list-of-strings delimiter)
  (let* ((lngth 0)
         (length-list (loop for x in list-of-strings
                          for l = (length x)
                          collect
                            (progn
                              (setq lngth (+ l lngth))
                              l)))
         (ll (1- (length length-list)))
         (new-string (make-string (+ (1- lngth) (length length-list))))
         (new-str-ptr 0))
    
    ;; concatenate element butlast elements
    
    (loop for j from 0 to (- (length length-list) 2)
        do
          (loop for i from 0 to (1- (nth j length-list))
              do
                (setf (aref new-string new-str-ptr)
                  (aref (nth j list-of-strings) i))
                (incf new-str-ptr))
          (setf (aref new-string new-str-ptr) delimiter)
          (incf new-str-ptr))
    
    ;; concatenate last element
    (loop for i from 0 to (1- (nth ll length-list))
          do
            (setf (aref new-string new-str-ptr)
              (aref (nth ll list-of-strings) i))
            (incf new-str-ptr))
      
    new-string))
;;; concatenate strings without delimiter

(defun make-nc-string- (list-of-strings)
  (let* ((lngth 0)
         (length-list (loop for x in list-of-strings
                          for l = (length x)
                          collect
                            (progn
                              (setq lngth (+ l lngth))
                              l)))
         (new-string (make-string lngth))
         (new-str-ptr 0))
    
    (loop for j from 0 to (- (length length-list) 1)
        do
          (loop for i from 0 to (1- (nth j length-list))
              do
                (setf (aref new-string new-str-ptr)
                  (aref (nth j list-of-strings) i))
                (incf new-str-ptr)))      
    new-string))

;;; a new version used in combination with NC handling
;;; it first calls the morphix-reader which might split a string
;;; like "haus-maus" into a list ("haus" "maus")
;;; in that case, the function inner-rt is called several times
;;; the result will then be mapped to the known output structure

(defun split-and-rt-wrd (wf &key (redundant? nil) (unknowns nil))
  (let* ((wf-list (morphix-read wf T))
         (*handle-unknowns* unknowns))
    (if (consp (rest wf-list))
        (loop for wrd in wf-list
            for res-wrd = (inner-rt wrd :redundant? redundant?)
            collect
              res-wrd))))

            
    

;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-
;;; file gen-parse

;;(defvar *type-list* nil)
    
(defvar *type-hashtable* nil)

(defvar *non-recognized-cats* nil)

(defvar *non-recognized-wf* nil)

(defparameter *para-cnt* 10)

(defvar *wf-cnt* nil)
(setq *wf-cnt* 0)

(defvar *col-cnt* nil)
(setq *col-cnt* 0)

(setq  *type-hashtable*
  (make-hash-table :test #'equal :size 200))

(defstruct type-info
  wf
  cnt)

(defun init-type-hash-table ()
  (loop for type-info in (get-all-types)
      do
        (setf (gethash type-info *type-hashtable*) 
          (make-type-info))))

(defun init-enumeration ()
  (init-type-hash-table)
  (setq *non-recognized-cats* nil)
  (setq *non-recognized-wf* nil)
  (setq *wf-cnt* 0)
  (setq *col-cnt* 0))

(defun print-hash-table (hash-table &key (stream t) (v? :yes))
  (loop for key being the hash-key of hash-table using (hash-value val) do
        (if v?
            (case v?
              (:yes (when (type-info-wf val)
                      (format stream "~%~a: ~a ~%"  key val)))
              (:no (when (not (type-info-wf val))
                     (format stream "~%~a: ~a ~%"  key val))))
          (format stream "~%~a: ~a ~%"  key val))))

(defun update-type-hashtable (wf rt-res)
  (loop for reading in rt-res
      do
        (let* ((stem (first reading))
               (type (second reading))
               (hash-val (gethash type *type-hashtable*)))
          (if hash-val
              (cond ((null (type-info-wf hash-val))
                     (setf (gethash type *type-hashtable*)
                       (make-type-info :wf (list wf)
                                       :cnt 1)))
                    ((> *para-cnt* (length (type-info-wf hash-val)))
                     (setf (type-info-wf hash-val)
                       (adjoin wf (type-info-wf hash-val)
                               :test #'string-equal))
                     (incf (type-info-cnt hash-val))
                     (setf (gethash type *type-hashtable*) hash-val)
                     )
                    (T (incf (type-info-cnt hash-val)) 
                       (setf (gethash type *type-hashtable*) hash-val)
                       ))
            (push wf *non-recognized-wf*)))))

(defun print-dots ()
  (incf *wf-cnt*) (incf *col-cnt*)
  (cond ((= 2000 *col-cnt*)
         (setq *col-cnt* 0)
         (setq *t2* (get-internal-run-time))
         (format T "!~a " (/ (- *t2* *t1*) 1.0 
                             internal-time-units-per-second))
         (setq *t1* (get-internal-run-time)))))

(defun update-type-function (word morphix-result)
  (print-dots)
  (update-type-hashtable word morphix-result))


(defun analyse-all-forms (wf-list)
  (let ((*clarification-dialog-on* nil)
        (*property-retrieval* nil))
    (mapcar #'(lambda (wf)
                (funcall (get 'gen-parse :output-fct) 
                         (map-to-umlaut wf)
                         (funcall (get 'gen-parse :morph-fct) 
                                  (map-to-umlaut wf))))
            wf-list)))

(defun noun-downcase (noun-string)
  (setf (aref noun-string 0) (char-downcase (aref noun-string 0)))
  noun-string)

(defun generate-all-forms (key cat)
  (case cat
    ((VERB MODALVERB HILFSVERB)
     (all-verb-forms key))
    (NOMEN (mapcar #'noun-downcase (all-noun-forms key))) 
    ;; the mapcar is necessary to redo upcase made during generation
    (ADJEKTIV (all-adjective-forms key))
    (DETERMINATIV (all-determiner-forms key 'def))
    (DETERMINATIV-INDEF (all-determiner-forms key 'indef))
    (T (when cat 
         (warn "~a not supported for ~a !" cat key)
         (setq *non-recognized-cats* (adjoin cat *non-recognized-cats*)))
       ;; merken mit adjoin in variable
       NIL)))

(defun handle-fullform-key (key)
  (analyse-all-forms (list key)))

(defun handle-stem-key (key entry)
  (analyse-all-forms 
   (if (multiple-computed-entry-p entry)
       (loop for sub-entry in (rest entry)
           append 
             (generate-all-forms key (rest (assoc 'wortart sub-entry))))
     (generate-all-forms key (rest (assoc 'wortart entry))))))

(defun all-determiner-forms (stem &optional type &aux (res nil))
  (cond ((find-associate-entry
	   (string-downcase stem)
	   (case type (indef 'determinativ-indef)
		 (def 'determinativ)
		 (rel 'relativpronomen)))
	  
	 (dolist (gender '(mas fem ntr))
	   
	   (dolist (case '(nom gen dat akk))
	     (dolist (numerus '(sg pl))
	       (push (if (eq type 'indef)
			   (determiner-indef-inflection
			     stem case numerus gender)
			   (determiner-inflection
                            stem case numerus gender))
                     res))))
         (remove-duplicates res :test #'equal))
	     
	(t (format t 
		   "~% no entry of type ~s  for ~s in the *stem-lexicon*"
		   type stem))))

(defun enumerate-stems (path node)
  (let ((leaf (cdr (assoc :info (dtree::node-content node)))))
    (when leaf
      (multiple-value-bind (res err)
          (ignore-errors (handle-stem-key 
                          (format nil "~{~A~}" path)
                          (expand-morph-stem-entry leaf)))
        (when err (format T "~&Error occured: ~a~%" err))))))

(defun enumerate-fullforms (path node)
  (let ((leaf (cdr (assoc :info (dtree::node-content node)))))
    (when leaf
      (multiple-value-bind (res err)
          (ignore-errors (handle-fullform-key 
                          (format nil "~{~A~}" path)))
        (when err (format T "~&Error occured: ~a~%" err))))))
 
(defun enumerate-stem-types (expr)
  (dtree::match-entry expr *stem-lexicon*
                      :output-fn #'enumerate-stems))

(defun enumerate-fullform-types (expr)
  (dtree::match-entry expr *fullform-lexicon*
                      :output-fn #'enumerate-fullforms))

(defun enumerate-all-stem-types (&key (expr ".*"))
  (init-enumeration)
  (setq *t1* (get-internal-run-time))
  
  (setf (get 'gen-parse :output-fct) 
    #'(lambda (a b) (declare (ignore a b)) NIL))
  
  (setf (get 'gen-parse :morph-fct) 
    #'(lambda (wf) 
        (update-type-function 
         wf
         (outer-rt (car (morphix-read wf))))))
  
  (dtree::match-entry expr *stem-lexicon*
                      :output-fn #'enumerate-stems))

(defun enumerate-all-fullform-types (&key (expr ".*"))
  (init-enumeration)
  (setq *t1* (get-internal-run-time))
  (setf (get 'gen-parse :output-fct) 
    #'(lambda (a b) (declare (ignore a b)) NIL))
  (setf (get 'gen-parse :morph-fct) 
    #'(lambda (wf) 
        (update-type-function 
         wf
         (outer-rt (car (morphix-read wf))))))
  
  (dtree::match-entry expr *fullform-lexicon*
                      :output-fn #'enumerate-fullforms))

;;; interface functions

(defun print-on-screen (word morphix-result)
  (format T "~%~W: ~W" word morphix-result))

(setf (get 'gen-parse :output-fct) #'print-on-screen)

(setf (get 'gen-parse :morph-fct) #'rt)


(defun morphix2c++-interface (word morphix-result 
                              &optional (stream *terminal-io*))
  (let* ((new-reading (loop for x in morphix-result
                          for stem = (if (consp (tripple-stem x))
                                         (format NIL "~a~{-~a~}"
                                                 (first (tripple-stem x)) 
                                                 (rest (tripple-stem x)))
                                       (tripple-stem x))
                          when (stringp (tripple-morph-type x))
                          collect
                            (format NIL "~W ~W ~W "
                                    stem (tripple-morph-type x) 
                                    (tripple-pos x)))))
    
                            
    (format stream  "~%~w : ~{~a~} ;" word new-reading)))




(defun make-c++-lexicon (outfile expr
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
          (morphix2c++-interface a b *out-stream*)))
    
    (funcall #'enumerate-stem-types expr)
    (funcall #'enumerate-fullform-types expr)
    
    (close *out-stream*)))

;;; from file utilities.lisp

;;; the current functions are usefull to run a text file
;;; with morphix and to store the result in an outfile

(defvar *in-dir* (pathname ""))
(setq *in-dir* (pathname "~/tmp/*.lisp"))

(defvar *out-dir* (pathname ""))
(setq *out-dir* (pathname "~/tmp/*.lisp"))

(defun transduce-file (infile outfile
                       &key (fct #'identity)
                       &aux (fromfile (merge-pathnames (pathname infile)
                                                       *in-dir*))
                            (tofile (merge-pathnames (pathname outfile)
                                                     *out-dir*)))
  (with-open-file (out-stream tofile
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-open-file (in-stream fromfile
                     :direction :input)
      (do ((expr (read in-stream nil 'end)
                 (read in-stream nil 'end)))
          ((eql expr 'end))
        (print (funcall fct expr) out-stream)))))

(defun thorsten-rt (string stream)
  (let ((mo-res (rt (car (morphix-read string)))))
    (format stream "~%~a" string)
    (loop for reading in mo-res do
          (format stream " ~a ~{~a ~} ;" (car reading) (second reading)))))

(defun thorsten-file (infile outfile
                       &aux (fromfile (merge-pathnames (pathname infile)
                                                       *in-dir*))
                            (tofile (merge-pathnames (pathname outfile)
                                                     *out-dir*)))
  (with-open-file (out-stream tofile
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-open-file (in-stream fromfile
                     :direction :input)
      (do ((expr (read-line in-stream nil 'end)
                 (read-line in-stream nil 'end)))
          ((eql expr 'end))
        (thorsten-rt expr out-stream)))))


(defun fileout (outfile &rest fct-call
                        &aux  (tofile (merge-pathnames (pathname outfile)
                                                       *out-dir*)))
  (with-open-file (out-stream tofile
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (print (eval fct-call) out-stream)))

;;; this function takes a file of strings and calls paradice main function
;;; rt; the result will be printed using the function morphix2c++-interface
;;; if T is specified for outfile, then *terminal-io* is used

(defun morphix-transduce-file (infile outfile
                               &aux (fromfile (merge-pathnames (pathname infile)
                                                       *in-dir*))
                                    (tofile (unless (eq outfile T)
                                              (merge-pathnames (pathname outfile)
                                                               *out-dir*))))
  (let* ((*out-stream* (if tofile
                           (open tofile :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
                         *terminal-io*)))
    (declare (special *out-stream*))
    
    (with-open-file (in-stream fromfile
                     :direction :input)
      (do* ((expr (read in-stream nil 'end)
                 (read in-stream nil 'end)))
          ((eql expr 'end))
        (let ((str (format nil "~a" expr)))
          (loop for mo-str in (morphix-read str) do
                (morphix2c++-interface mo-str (rt mo-str) 
                                       *out-stream*)))))
    
    (when tofile (close *out-stream*))))
  

;;; the following is a special function only useable together with the 
;;; stuff written inside file gen-parse.lisp
;;; if T is specified for outfile, then *terminal-io* is used

(defun gen-parse-file (outfile fct expr
                       &aux  (tofile (unless (eq outfile T)
                                       (merge-pathnames (pathname outfile)
                                                        *out-dir*))))
  (let* ((*out-stream* (if tofile
                           (open tofile :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
                         *terminal-io*)))
    (declare (special *out-stream*))
    
    (ruecksetzen)
    (setf (get 'gen-parse :morph-fct) 
      #'(lambda (wf) (rt (car (morphix-read wf)))))
    (setf (get 'gen-parse :output-fct) 
      #'(lambda (a b)
          (morphix2c++-interface a b *out-stream*)))
    (funcall fct expr)
    
    (when tofile (close *out-stream*))))


;;; Usage:

;;; Complete malaga list:
