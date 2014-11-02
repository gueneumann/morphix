;; Software copyrights: Guenter Neumann, Donnerstag, 24. Oktober 1991
;;; Last update: 30/1/96

;;  FILE: dtree.lisp
;; contains: functions to hold decision tree
;; toplevel-functions to be called elsewhere are: 

;; insert-entry, get-entry, delete-entry, entry-exists-p

#+:ALLEGRO(declaim (optimize (speed 3) (safety 0)
(space 0) (debug 0)))

(in-package :dtree)

(defstruct node
  index
  content
  daughters
  stat)

(defun make-dtree ()
  (make-node :index 'root))

(defvar *active-dtrees* nil)

(defun init-dtree (dtree-name)
  (push dtree-name *active-dtrees*)
  (eval `(setf ,dtree-name ,(dtree::make-dtree))))

(defun find-daughter (index dtree test key)
  (member index (node-daughters dtree) :test test :key key))

(defun get-leaf-info (leaf-name dtree &key (test #'eq))
  (assoc leaf-name (node-content dtree) :test test))

(defun node-cnt (dtree &aux (nodes 0) (entries 0))
  (declare (special nodes entries))
  (node-cnt* dtree)
  (values nodes entries))

(defun node-cnt* (dtree)
  (cond ((null dtree) NIL)
        (T (loop for dtr in (node-daughters dtree) do
                 (incf nodes)
                 (when (node-content dtr) (incf entries))
                 (node-cnt* dtr)))))
        
                                       

(defun insert-entry (index entry d-tree 
                         &key (overwrite nil) 
                              (leaf-name :info)
                              (test #'char=)
                              (key #'node-index))
  (let ((index-length (length index)))
    (and (plusp index-length)
	 (let ((dtree (find-daughter (elt index 0) d-tree test key)))
           (cond (dtree (insert-rest-key index 
                                         (car dtree)
                                         test key
                                         index-length entry 
                                         overwrite leaf-name)  
                       t)
		 (t (if (consp (node-daughters d-tree))
                        (setf (rest (last (node-daughters d-tree)))
                          (list (do-key-dtree index 0 index-length entry leaf-name)))
                      (setf (node-daughters d-tree)
                        (list (do-key-dtree index 0 index-length entry leaf-name))))
                    t))))))

(defun insert-rest-key 
    (key dtree test slot-key length entry overwrite leaf-name)
  (if (= length 1)
      (let* ((leaf (get-leaf-info leaf-name dtree))
             (result (if leaf 
                         (update-dtree-entry (cdr leaf) entry overwrite))))
        (cond (leaf
               (cond ((equal leaf result))
                     (t (rplacd leaf result))))
              (t (if (consp (node-content dtree))
                     (setf (rest (last (node-content dtree)))
                       (list (cons leaf-name entry)))
                   (setf (node-content dtree)
                     (list (cons leaf-name entry)))))))
    (do* ((n 1 (1+ n))
          (end-length (1- length))
          (new-key (elt key n) (elt key n))
          (orig-dtree dtree new-dtree)
          (new-dtree (car (find-daughter new-key dtree test slot-key))
                     (car (find-daughter new-key new-dtree  test slot-key))))
        ((= n end-length) 
         (cond ((null new-dtree)
                (if (consp (node-daughters orig-dtree))
                    (setf (rest (last (node-daughters orig-dtree)))
                      (list (do-key-dtree key n length entry leaf-name)))
                  (setf (node-daughters orig-dtree)
                    (list (do-key-dtree key n length entry leaf-name)))))
                       
               (t (let* ((leaf (get-leaf-info leaf-name new-dtree))
                         (result (if leaf  (update-dtree-entry 
                                            (cdr leaf) entry overwrite))))
                    (cond (leaf (cond ((equal leaf result))
                                      (t (rplacd leaf result))))
                          (t (if (consp (node-content new-dtree))
                                 (setf (rest (last (node-content new-dtree)))
                                   (list (cons leaf-name entry)))
                               (setf (node-content new-dtree)
                                 (list (cons leaf-name entry))))))))))
      (cond ((null new-dtree)
             (return
               (if (consp (node-daughters orig-dtree))
                   (setf (rest (last (node-daughters orig-dtree)))
                     (list (do-key-dtree key n length entry leaf-name)))
                 (setf (node-daughters orig-dtree)
                   (list (do-key-dtree key n length entry leaf-name))))))))))
                           

(defun do-key-dtree (key stop length entry leaf-name)
  " Build one path for new entry from bottom to top"
  (do* ((n (1- length) (1- n))
	(result (make-node :index (elt key (1- length))
                             :content (list (cons leaf-name entry)))
                (make-node :index (elt key n)
                           :daughters (list result))))
      ((= n stop) result)))

(defvar %multi-entry% ':disjunctive
  "indicator for several entries for one lex-key")

(defmacro multiple-dtree-entry-p (dtree-entry)
  `(and (consp ,dtree-entry)
	(eq %multi-entry% (car ,dtree-entry))))

(defun update-dtree-entry (old-entry new-entry overwrite)
   (cond (overwrite new-entry)
	 ; brute force: if overwrite flag then overwrite the whole
	 ; old-entry with the new one
         ;; here it is assumed that elements can be found by equal
         ;; this should be parametrizable
	 ((multiple-dtree-entry-p old-entry)
 	  (cond ((member new-entry (cdr old-entry) :test #'equal) old-entry)
	        (t (rplacd old-entry (cons new-entry (cdr old-entry))))))
	 ((equal new-entry old-entry) old-entry)
         ;; the else case means: add new information
         (t (list %multi-entry% new-entry old-entry))))


(defun get-entry (key dtree &key (leaf-name :info) 
                                 (test #'char=) 
                                 (slot-key #'node-index))
  (let ((k-length (length key)))
    (and (plusp k-length)
	 (do ((n 0 (1+ n))
	      (end-length (- k-length 1))
	      (rest-dtree dtree))
	     ((> n end-length) (cdr (get-leaf-info leaf-name rest-dtree)))
           (cond ((not (setq rest-dtree
                         (car (find-daughter (elt key n) 
                                             rest-dtree test slot-key))))
                  (return nil)))))))


(defun delete-entry (entry dtree
		     &key (leaf-name :info) (test #'char=) (key #'node-index)
		     &aux (e-length (length entry)))
  (if (and (plusp e-length)
           (entry-exists-p entry dtree 
                           :leaf-name leaf-name :test test :key key))
      (let ((dtree-distribution 
             (get-entry-path entry e-length leaf-name dtree test key)))
        (cond (dtree-distribution
               ;; this is a list indicating the various branching
               ;; possibilities in the TRIE. As long as the elements
               ;; in this
               ;; list are 1, the subtrie can be deleted.
	       ;; NOTE: this is a top-down perspective, because in this dtree I do not have 
	       ;; parent nodes ! This is why I need to store the position of path of the remnoved 
	       ;; entry and have to do a destructive removal of that path
               (let ((position-to-cut
                      (- (1+ e-length)  ;; wg. root
                         (or (position-if #'(lambda (number) (> number 1))
                                          dtree-distribution)
                             (1+ e-length)))))
                 (pprint position-to-cut)
                 (delete-subdtree 
                  entry e-length position-to-cut 
                  leaf-name dtree test key))
               t)))
    (warn "~s not known" entry)))

(defun get-entry-path (entry e-length leaf-name dtree test key)
  (do* ((n 0 (1+ n))
	(daughter-count (length (node-daughters dtree)) 
                        (if (get-leaf-info leaf-name subdtree)
                            (1+ (length (node-daughters subdtree)))
                          (length (node-daughters subdtree))))
	(subdtree (car (find-daughter (elt entry n) dtree test key))
                  (car (find-daughter (elt entry n) subdtree test key)))
	(result nil))
       ((= n (1- e-length));; entry is completely reTRIEved
	;; now look for the leaf-name node.
	(cond ((get-leaf-info leaf-name subdtree)
               ;; possible entry is in the dtree
	       (cons (1+ (length (node-daughters subdtree))) 
                     ;; daughters plus leaf-name
		     (cons daughter-count result)))
	      (t (warn "~s not known" entry))))
       
       (cond (subdtree (push daughter-count result))
	     (t (return (warn "~s not known" entry))))))

(defun delete-subdtree (entry e-length position-to-cut leaf-name dtree test key)
  (cond ((zerop position-to-cut)
	 ;; reset the lexicon
	 (setf (node-daughters dtree) NIL))
	((= 1 position-to-cut)
	 (my-destructive-delete (car (find-daughter (elt entry 0) dtree test key))
				(node-daughters dtree)))
	(t (do ((n 0 (1+ n))
		(subdtree dtree))
	       ((= n (1- position-to-cut))
                (when subdtree
                  (if (= n e-length)
                      (progn
                        (my-destructive-delete (get-leaf-info leaf-name subdtree)
                                               (node-content subdtree))
                        (when (null (car (node-content subdtree)))
                          (setf (node-content subdtree) NIL) T))
                    (progn
                      (my-destructive-delete
                       (car (find-daughter (elt entry n) subdtree test key))
                       (node-daughters subdtree))
                      (when (null (car (node-daughters subdtree)))
                        (setf (node-daughters subdtree) NIL) T)))))
             (setq subdtree (car (find-daughter (elt entry n) subdtree test key)))))))

(defun my-destructive-delete (item list)
  (cond ((eql item (car list))
	 (rplaca list (cadr list))
	 (rplacd list (cddr list)))
	(t (delete item list)))
  list)

(defun entry-exists-p (entry dtree 
                             &key (leaf-name :info) 
                             (test #'char=) (key #'node-index))
  "Check, whether there exists an ENTRY with LEAF-NAME information."
  (let ((e-length (length entry)))
    (cond ((plusp e-length)
           (do* ((n 0 (1+ n))
                 (subdtree (car (find-daughter (elt entry n) dtree test key))
                           (car (find-daughter (elt entry n) subdtree test key)))
                 (stop-test (1- e-length)))
               ((= n stop-test)
                ;; entry is complete reTRIEved
                ;; now look for the leaf-name - node.
                (and subdtree (get-leaf-info leaf-name subdtree)))
             ;; entry is in the lexicon
             (if (not subdtree) (return nil))))
          (t t))))

(defun get-suffix-dtree (index dtree &key (test #'char=) (key #'node-index))
  (let ((index-length (length index)))
    (and (plusp index-length)
	 (do ((n 0 (1+ n))
	      (end-length (- index-length 1))
	      (rest-dtree dtree))
	     ((> n end-length) rest-dtree)
           (cond ((not (setq rest-dtree
                         (car (find-daughter (elt index n) 
                                            rest-dtree test key))))
                  (return nil)))))))

(defun get-leafs (index dtree &key (test #'char=)(key #'node-index))
  (let ((index-length (length index)))
    (and (plusp index-length)
	 (do ((n 0 (1+ n))
	      (end-length (- index-length 1))
	      (rest-dtree dtree))
	     ((> n end-length) (node-content rest-dtree))
           (cond ((not (setq rest-dtree
                         (car (find-daughter (elt index n) 
                                            rest-dtree test key))))
                  (return nil)))))))




(defun get-prefix-entries (index dtree &key (leaf-name :info) 
                                            (test #'char=) (key #'node-index))
  (let ((index-length (length index))
        (prefix-entries nil))
    (and (plusp index-length)
	 (do ((n 0 (1+ n))
	      (end-length (- index-length 1))
	      (rest-dtree dtree))
	     ((> n end-length)
              
              (let ((leaf-value (get-leaf-info leaf-name rest-dtree)))
                (when leaf-value 
                  (push (cons n (rest leaf-value)) prefix-entries))
                prefix-entries))
           
           (let ((leaf-value  (get-leaf-info leaf-name rest-dtree)))
             (when leaf-value
               (push (cons n (rest leaf-value)) prefix-entries))
             
             (cond ((not (setq rest-dtree
                           (car (find-daughter (elt index n) rest-dtree test key))))
                    (return prefix-entries))))))))

(defun get-substrings (index dtree
                       &key (leaf-name :info) (exhaustive T)
                            (caller #'print-res)
                            (one-composition nil) 
                            (test #'char=) (key #'node-index))
  (let* ((index-length (length index))
         (lookup (get-prefix-entries index dtree 
                                     :leaf-name leaf-name :test test :key key))
         (longest-prefix (first lookup))
         (offset (first longest-prefix))
         (rest-prefixes (rest lookup)))
    (if (null lookup)
        (if (eql (length index) 0)
          (funcall caller one-composition)
          :done)
      (progn
        (get-substrings (subseq index offset)
                        dtree
                        :leaf-name leaf-name
                        :exhaustive exhaustive
                        :one-composition 
                        (cons index 
                              (cons longest-prefix one-composition))
                        :test test
                        :key key)
          (when exhaustive
            (loop for next-smaller-prefix in rest-prefixes
                do
                (when
                  (< (first next-smaller-prefix) index-length)
                  (get-substrings 
                   (subseq index (first next-smaller-prefix))
                     dtree
                     :leaf-name leaf-name
                     :exhaustive exhaustive
                     :one-composition  
                     (cons index 
                           (cons next-smaller-prefix one-composition))
                     :test test
                     :key key))))))))


(defun print-res (one-composition)
  (format t "~% ~a" one-composition))
