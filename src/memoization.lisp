(defpackage :memo
   (:use "COMMON-LISP")
)

(in-package :memo)

;;; this file includes functions and methods for performing
;;; memoization of values computed be functions (what else)
;;;
;;; Author: Guenter Neumann, 9.1991 (SFB 314, BiLD)

;;; The stuff is heavily based on  Norvig 91



;;; Basic Idea:
;;; In order to access the memoized version of a function 
;;; when calling that function, change the fn-name's global definition.
;;; This allows access to memoized function also recursively.
;;; Now, memoization of function with more than one argument

(defvar *memoized-function-stack* nil)
(defvar *un-memoized-function-stack* nil)

#||
(defmacro defun-memo (fn args &body body)
  `(memoize (defun ,fn ,args . ,body)))
||#

(defun memo (fn &optional &key (key #'first) (test #'eql) name)
  "return a memo-function of fn. key allows to specify, how the
key for table is computed from the function's arguments"
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let* ((k (funcall key args)))
          (multiple-value-bind (val found)
                               (gethash k table)
            (if found val
                (setf (gethash k table)
                      (apply fn args))))))))

(defun multiple-value-memo (fn &optional &key (key #'first) (test #'eql) name)
  "This version of memo enables us to hash mutliple values of of a function
<fn>. It is currently expected that <fn> uses the Lisp primitive <values>
to return multiple values."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
      #'(lambda (&rest args)
          (let ((k (funcall key args))) 
            (multiple-value-bind (val found)
                                 (gethash k table)
              (if found 
                (values-list val)
                (values-list
                 (setf
                  (gethash k table)
                  (multiple-value-list (apply fn args))))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql) values)
  "Replace fn-name's global definition with a memoized
version. The key word parameter values triggers between memo and 
multiple-value-memo"
  (push fn-name *memoized-function-stack*)
  (setf *un-memoized-function-stack*
        (delete fn-name *un-memoized-function-stack*))
  (setf (get fn-name 'old) (symbol-function fn-name))
  (setf (symbol-function fn-name)
        (if values
          (multiple-value-memo (symbol-function fn-name)
	      :name fn-name
	      :key key
              :test test)
          (memo (symbol-function fn-name)
                :name fn-name
                :key key
                :test test))))
 
(defun un-memoize (fn-name)
  (if (not (member fn-name *memoized-function-stack* :test #'eq))
    (warn "Cannot un-memoize an non memoized function")
    (progn
      (push fn-name *un-memoized-function-stack*)
      (setf *memoized-function-stack*
            (delete fn-name *memoized-function-stack*)) 
      (setf (symbol-function fn-name)
            (get fn-name 'old))
      (setf (get fn-name 'memo) nil))))

(defun clear-memo-table (fn-name)
  (clrhash (get fn-name 'memo)))

(defun show-hash (table &optional (val-p nil))
  (maphash #'(lambda (key val)
	       (if val-p
		   (format t "~&key: ~a ~6Tvalue: ~a" key val)
		 (format t "~&key: ~a ~%" key val)))
	   table))

(defun show-memo-table (fn &optional contents)
  (let ((fn-table (get fn 'memo)))
    (if contents
      (show-hash fn-table contents)
      (describe fn-table))))

#|
;;; GN: March 2004, not compatible with CLISP
;;; Problems with use of in-package()
(defun save-memo-table (fn-name  &optional 
                                 (out-file (concatenate 'string "hd:" 
                                                       (symbol-name fn-name)
                                                       "-MEMO-TMP.LISP")))
  "Prints all key and associated values into out-file named <fn-name>-memo-tmp.lisp.
To avoid problems with packages the functions switches between the package of the
caller and the package of fn-name's function. The time necessary for printing depends
on the printing level of the objects."
  (let ((hsh-counter 0)
        (old-pkg (package-name *package*))
        (sym-pkg (package-name (symbol-package fn-name))))
     (with-open-file (out out-file
        :direction :output
        :if-exists :overwrite
        :if-does-not-exist :create)
       (in-package sym-pkg)
       (maphash
        #'(lambda (key val)
            (format out "~&~D Key:~7T" (incf hsh-counter)) 
            (write key :stream out :pretty t :circle t)
            (format out "~&Val:~5T")
            (write val :stream out :pretty t :circle t))
       (get fn-name 'memo))
       (in-package old-pkg))))
|#
  

;;; compromise between equal and eql when used as hash tests

;;; Norvig uses it for parsing
;;; I have to ckeck whether it is useful for me too


(defmacro cl-user::puthash (key table val)
    `(setf (gethash ,key ,table) ,val))

(defun put-multi-hash (keylist value hash-table)
  "Store a value in a multi-level hash table:
one level for each element of keylist"
  (if (= (length keylist) 1)
      (setf (gethash (first keylist) hash-table) value)
      (let ((table1 (or (gethash (first keylist) hash-table)
			(setf (gethash (first keylist) hash-table)
			      (make-hash-table)))))
	(put-multi-hash (rest keylist) value table1))))



(defun get-multi-hash (keylist hash-table)
  "Fetch a value from a multi-level hash table:
one level for each element of keylist."
  (if (= (length keylist) 1)
      (gethash (first keylist) hash-table)
      (let ((table1 (or (gethash (first keylist) hash-table)
			(setf (gethash (first keylist) hash-table)
			      (make-hash-table)))))
	(get-multi-hash (rest keylist) table1))))

(defun memo-multi (fn &optional &key name (maker #'make-hash-table) 
	     (getter #'gethash) (putter #'puthash))
  "return a memo-function of fn. key allows to specify, how the
key for table is computed from the function's arguments"
  (let ((table (funcall maker)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	  (multiple-value-bind (val found)
	      (funcall getter args table)
	    (if found val
		(funcall putter args (apply fn args) table))))))
	

(defun memoize-multi (fn-name &rest memo-keys)
  "Replace fn-name's global definition with a memoized
version."
  (setf (symbol-function fn-name)
	(apply #'memo-multi
	       (symbol-function fn-name)
	      :name fn-name
	      memo-keys)))
