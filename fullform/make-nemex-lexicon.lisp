(in-package :mo)

;;; load file make-ff-lexicon.lisp

;;; WORKS FOR NEMEXA AND NEMEXF
#|
Format 
0 utf-8 EN 5 5
1 -9.197762 kaushik#ch NG:1:-9.197762
2 -9.197762 chakrabarti NG:1:-9.197762
|#


#|

get all entries via
(dtree::match-entry ".*" *stem-lexicon*)
(dtree::match-entry ".*" *fullform-lexicon*)

eventually also (enumerate-homographs ".*")

for each found entry, encode its lexicon information:

(compute-stem-entry XX)
(compute-fullform-entry YY)

If multiple:
loop through POS-readings
collect POS; if no POS, Determine POS information using stem pointer



|#

;;; check whether wordform as SS in it then mark it

(defun contains-double-s (string)
  (let ((pos-first-s (position #\s string)))
    (when pos-first-s
      (when (< pos-first-s (1- (length string)))
	(char-equal #\s (aref  string (1+ pos-first-s)))))))

;;; get the pos if available, if not, get via stamm pointer
(defun get-pos-rest-value (key assoc-list)
  (let ((found-pos (rest (assoc 'wortart assoc-list))))
    (if found-pos found-pos
	(if (assoc 'stamm assoc-list)
	    (get-pos-for-pointered-entry key (first assoc-list))
	    (pprint key)))
    )
  )

;;; only in case of fullforms; no stamm pointer assumed here
(defun get-pos-cadr-value (key assoc-list)
  (cadr (assoc 'wortart assoc-list))
  )

;;; get the wortart for the stamm pointer entry
;;; MISSING: stamm-lemma is lost -> add it to entry
(defun get-pos-for-pointered-entry (key pointer-entry)
  (let* ((pointered-key (get-pointered-key pointer-entry))
	 (pointered-segment-type  (get-pointered-segment pointer-entry))
	 (result (find-associate-category pointered-segment-type
					  (compute-stem-entry pointered-key) 
					  key pointered-key)))
    (rest (assoc 'wortart result))
    )
  )

(defun morphix-to-nemex (path node compute-morphix-function get-pos-retrieval)
  (let ((leaf (cdr (assoc :info (dtree::node-content node)))))
    (when leaf
      (let* ((word (format nil "~{~A~}" path))
             (lex-entry (funcall compute-morphix-function word))
	     (umlaut (test-for-umlaut word))
	     (doubles (contains-double-s word))
	     (attribute (if *attributes*
			    (if (and umlaut doubles) "ATTR=umlaut-doubles"
				(if umlaut "ATTR=umlaut"
				    (if doubles "ATTR=doubles"
					"")))
			    ""))
             (multiple-entry-p (multiple-computed-entry-p lex-entry)))
	(unless (equal attribute "") (incf *attr-cnt*))
	(if multiple-entry-p
	    (let* ((wortarten (loop for x in (rest lex-entry) collect 
				   (apply get-pos-retrieval (list word x))))
		   (nemex-entry (format nil "~A 0.0 ~A ~{~A:1:0.0 ~}~A" (incf *cnt*) word wortarten attribute))
		   )
	      (format *out-stream* "~&~A~%" nemex-entry))
	    (let* ((info (if (listp (first lex-entry)) lex-entry (rest lex-entry)))
		   (wortart (apply get-pos-retrieval (list word info)))
		   (nemex-entry (format nil "~A 0.0 ~A ~A:1:0.0 ~A" (incf *cnt*) word wortart attribute)))
	      (format *out-stream* "~&~A~%" nemex-entry))
	    )))))

(defun ff-to-nemex (path node)
  (morphix-to-nemex path node #'compute-fullform-entry #'get-pos-cadr-value))

(defun stem-to-nemex (path node)
  (morphix-to-nemex path node #'compute-stem-entry #'get-pos-rest-value))

(defun make-nemex-entries (&key
			     (attributes t)
			     (tofile "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/fullform/morphix-nemex.txt")
			     (expr ".*"))
  (let ((*cnt* 0)
	(*attr-cnt* 0)
	(*attributes* attributes)
	(*out-stream* (open tofile :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)))
    (declare (special *out-stream* *cnt* *attr-cnt* *attributes*))
    
    (dtree::match-entry expr *fullform-lexicon* :output-fn #'ff-to-nemex)
    (dtree::match-entry expr *stem-lexicon* :output-fn #'stem-to-nemex)
    
    (format *out-stream* "~&0 utf-8 DE ~a ~a~%" *cnt* *cnt*)
    (close *out-stream*)

    (values *cnt* *attr-cnt*)
    )
  )


#|
Missing:
- handle STEM pointers if no WORTART is available -> OK
- weights -> use dummies here
- output to file -> OK; "/Users/gune00/dfki/src/Lisp/morphix/pd-vers/fullform/morphix-nemex.txt"

callers:

(make-nemex-entries :attributes nil) 
  -> creates nemex file directly 
  -> I tested it with NemexF and it works !
(make-nemex-entries :attributes T)
  -> this is for creating attributes

- create initial line for file for meta information 
  -> OK, must be moved to first line of file manually




- do umlauting on copied file
-> manually check entries and change ATTR=XY entries and remove feature
|#
