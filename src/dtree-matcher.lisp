;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DTREE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: reg-trie-matcher.lisp
;;;      module: morphix
;;;   $Revision: 1.8 $
;;;  written by: scherf, dfki saarbruecken
;;;       $Date: 1995/08/31 12:57:03 $
;;;  updated by: scherf, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Software copywrights: Oliver Scherf, 1995/09/05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :dtree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; suffix-class-p ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun suffix-class-p (index 
                       expr
                       get-nth-elem
                       get-expr-len
                       first-matcher-p
                       second-matcher-p)
  (and (< index (- (funcall get-expr-len expr) 1))
       (or (funcall first-matcher-p (funcall get-nth-elem (+ index 1) expr))
           (funcall second-matcher-p (funcall get-nth-elem (+ index 1) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; determine-suffix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not praticable enough !!!!
;; Always only non special meaning signs behind .+ or .* expressions allowed.

(defun determine-suffix (index expr 
                               get-nth-elem 
                               get-expr-len
                               is-zero-or-more-matcher-p
                               is-one-or-more-matcher-p
                               is-zero-or-one-matcher-p
                               is-any-char-matcher-p
                               is-begin-of-concat-p
                               is-begin-of-alter-p
                               is-end-of-concat-p
                               is-end-of-alter-p
                               is-or-matcher-p)
  (when (< index (- (funcall get-expr-len expr) 1))
    (let ((f-sign (funcall get-nth-elem (+ index 1) expr)))
      (if (or (funcall is-zero-or-more-matcher-p f-sign)
              (funcall is-one-or-more-matcher-p f-sign)
              (funcall is-zero-or-one-matcher-p f-sign)
              (funcall is-any-char-matcher-p f-sign)
              (funcall is-begin-of-concat-p f-sign)
              (funcall is-begin-of-alter-p f-sign)
              (funcall is-end-of-concat-p f-sign)
              (funcall is-end-of-alter-p f-sign)
              (funcall is-or-matcher-p f-sign))
        NIL
        ;; else
        f-sign))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; lookahead-for-wildcard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; we need this function for checking the next character after the one of
;;; the actual task because the two wildcards "*" and "?" means that the
;;; expression before must not match and thats why we need the trie before the
;;; actual matcher removes it.

(defun lookahead-for-wildcard (index expr trie result
                                  is-leaf-p
                                  index-eq
                                  find-son-with-index
                                  do-forall-sons 
                                  get-nth-elem
                                  get-expr-len
                                  constr-path
                                  output-fn
                                  is-zero-or-more-matcher-p
                                  is-one-or-more-matcher-p
                                  is-zero-or-one-matcher-p
                                  is-any-char-matcher-p
                                  is-begin-of-concat-p
                                  is-begin-of-alter-p
                                  is-end-of-concat-p
                                  is-end-of-alter-p
                                  is-or-matcher-p
                                  constr-rexpr-from-sign-list)
  (when (suffix-class-p index 
                        expr
                        get-nth-elem
                        get-expr-len
                        is-zero-or-more-matcher-p
                        is-zero-or-one-matcher-p)
    ;; yes, a wildcard follows the actual recognized character that means
    ;; we have to look for a leaf in the actual trie of the input task
    (if (= (+ index 2) (funcall get-expr-len expr))
      ;; test if the wildcard is the end of the given regular expression
      ;; print the found leaf and the result list
      (funcall output-fn result trie)
      ;; Else there is a suffix behind the wildcard and we must create
      ;; a new task to process the suffix with the values of the input task
      ;; thats why we also jump over the wildcard
      (scanner-unit (+ 2 index) expr trie result 
                    NIL ;; a new wildcard would be fault
                    is-leaf-p
                    index-eq
                    find-son-with-index
                    do-forall-sons 
                    get-nth-elem
                    get-expr-len
                    constr-path
                    output-fn
                    is-zero-or-more-matcher-p
                    is-one-or-more-matcher-p
                    is-zero-or-one-matcher-p
                    is-any-char-matcher-p
                    is-begin-of-concat-p
                    is-begin-of-alter-p
                    is-end-of-concat-p
                    is-end-of-alter-p
                    is-or-matcher-p
                    constr-rexpr-from-sign-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; any-char-matcher ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun any-char-matcher (index expr trie result
                               is-leaf-p
                               index-eq
                               find-son-with-index
                               do-forall-sons 
                               get-nth-elem
                               get-expr-len
                               constr-path
                               output-fn
                               is-zero-or-more-matcher-p
                               is-one-or-more-matcher-p
                               is-zero-or-one-matcher-p
                               is-any-char-matcher-p
                               is-begin-of-concat-p
                               is-begin-of-alter-p
                               is-end-of-concat-p
                               is-end-of-alter-p
                               is-or-matcher-p
                               constr-rexpr-from-sign-list)
  (funcall do-forall-sons trie 
           #'(lambda (node)
               (if (= index (- (funcall get-expr-len expr) 1))
                 (funcall output-fn (funcall constr-path node result)  node)
                 ;; else there is suffix behind the actual token
                 (scanner-unit (+ index 1) expr node
                               (funcall constr-path node result)
                               T ;; because every sign is matched
                               is-leaf-p
                               index-eq
                               find-son-with-index
                               do-forall-sons 
                               get-nth-elem
                               get-expr-len
                               constr-path
                               output-fn
                               is-zero-or-more-matcher-p
                               is-one-or-more-matcher-p
                               is-zero-or-one-matcher-p
                               is-any-char-matcher-p
                               is-begin-of-concat-p
                               is-begin-of-alter-p
                               is-end-of-concat-p
                               is-end-of-alter-p
                               is-or-matcher-p
                               constr-rexpr-from-sign-list))))
  (lookahead-for-wildcard index expr trie result
                          is-leaf-p
                          index-eq
                          find-son-with-index
                          do-forall-sons 
                          get-nth-elem
                          get-expr-len
                          constr-path
                          output-fn
                          is-zero-or-more-matcher-p
                          is-one-or-more-matcher-p
                          is-zero-or-one-matcher-p
                          is-any-char-matcher-p
                          is-begin-of-concat-p
                          is-begin-of-alter-p
                          is-end-of-concat-p
                          is-end-of-alter-p
                          is-or-matcher-p
                          constr-rexpr-from-sign-list))
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; char-matcher ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun char-matcher (scan-sign rexpr-index rexpr trie result
                               is-leaf-p
                               index-eq
                               find-son-with-index
                               do-forall-sons 
                               get-nth-elem
                               get-expr-len
                               constr-path
                               output-fn
                               is-zero-or-more-matcher-p
                               is-one-or-more-matcher-p
                               is-zero-or-one-matcher-p
                               is-any-char-matcher-p
                               is-begin-of-concat-p
                               is-begin-of-alter-p
                               is-end-of-concat-p
                               is-end-of-alter-p
                               is-or-matcher-p
                               constr-rexpr-from-sign-list)
  (let ((path-test (funcall find-son-with-index scan-sign trie)))
    (when path-test
      ;; yes the path exists and we can add the sign to the result
      (scanner-unit (+ 1 rexpr-index) 
                    rexpr path-test
                    (funcall constr-path path-test result)
                    (list scan-sign)
                    is-leaf-p
                    index-eq
                    find-son-with-index
                    do-forall-sons 
                    get-nth-elem
                    get-expr-len
                    constr-path
                    output-fn
                    is-zero-or-more-matcher-p
                    is-one-or-more-matcher-p
                    is-zero-or-one-matcher-p
                    is-any-char-matcher-p
                    is-begin-of-concat-p
                    is-begin-of-alter-p
                    is-end-of-concat-p
                    is-end-of-alter-p
                    is-or-matcher-p
                    constr-rexpr-from-sign-list)))
  (lookahead-for-wildcard rexpr-index rexpr trie result
                          is-leaf-p
                          index-eq
                          find-son-with-index
                          do-forall-sons 
                          get-nth-elem
                          get-expr-len
                          constr-path
                          output-fn
                          is-zero-or-more-matcher-p
                          is-one-or-more-matcher-p
                          is-zero-or-one-matcher-p
                          is-any-char-matcher-p
                          is-begin-of-concat-p
                          is-begin-of-alter-p
                          is-end-of-concat-p
                          is-end-of-alter-p
                          is-or-matcher-p
                          constr-rexpr-from-sign-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; one-or-more-matcher ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-or-more-matcher (index expr trie result prefix
                                  is-leaf-p
                                  index-eq
                                  get-son-with-index
                                  do-forall-sons 
                                  get-nth-elem
                                  get-expr-len
                                  constr-path
                                  output-fn
                                  is-zero-or-more-matcher-p
                                  is-one-or-more-matcher-p
                                  is-zero-or-one-matcher-p
                                  is-any-char-matcher-p
                                  is-begin-of-concat-p
                                  is-begin-of-alter-p
                                  is-end-of-concat-p
                                  is-end-of-alter-p
                                  is-or-matcher-p
                                  constr-rexpr-from-sign-list)
  (let ((suffix (determine-suffix index expr 
                                  get-nth-elem 
                                  get-expr-len
                                  is-zero-or-more-matcher-p
                                  is-one-or-more-matcher-p
                                  is-zero-or-one-matcher-p
                                  is-any-char-matcher-p
                                  is-begin-of-concat-p
                                  is-begin-of-alter-p
                                  is-end-of-concat-p
                                  is-end-of-alter-p
                                  is-or-matcher-p))
        (suffix-test (if (= (- (funcall get-expr-len expr) 1) index)
                      NIL
                      ;; else
                      T)))
    (when prefix
      (cond ((eq prefix T)
              (if (and (not suffix) (= (- (funcall get-expr-len expr) 1) index)
                       (funcall is-leaf-p trie))
                ;; The given trie is a leaf, that means there are no daughters
                ;; Thats why check whether there is a information under 
                ;; the content field of the trie, because the function preorder
                ;; goes directly into the daughters.
                (funcall output-fn result trie)
                ;; else
                (preorder index expr trie result suffix
                          is-leaf-p
                          index-eq
                          get-son-with-index
                          do-forall-sons 
                          get-nth-elem
                          get-expr-len
                          constr-path
                          output-fn
                          is-zero-or-more-matcher-p
                          is-one-or-more-matcher-p
                          is-zero-or-one-matcher-p
                          is-any-char-matcher-p
                          is-begin-of-concat-p
                          is-begin-of-alter-p
                          is-end-of-concat-p
                          is-end-of-alter-p
                          is-or-matcher-p
                          constr-rexpr-from-sign-list)))
            (T (into-the-deep index expr trie result prefix suffix-test
                              is-leaf-p
                              index-eq
                              get-son-with-index
                              do-forall-sons 
                              get-nth-elem
                              get-expr-len
                              constr-path
                              output-fn
                              is-zero-or-more-matcher-p
                              is-one-or-more-matcher-p
                              is-zero-or-one-matcher-p
                              is-any-char-matcher-p
                              is-begin-of-concat-p
                              is-begin-of-alter-p
                              is-end-of-concat-p
                              is-end-of-alter-p
                              is-or-matcher-p
                              constr-rexpr-from-sign-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; into-the-deep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function is for all other cases of wildcard "*" and "+" following any
;;; expression except the ".". It calls the test-fn in a loop until the
;;; computed subtrie by test-fn is NIL.

(defun into-the-deep (index expr trie result prefix suffix
                            is-leaf-p
                            index-eq
                            get-son-with-index
                            do-forall-sons 
                            get-nth-elem
                            get-expr-len
                            constr-path
                            output-fn
                            is-zero-or-more-matcher-p
                            is-one-or-more-matcher-p
                            is-zero-or-one-matcher-p
                            is-any-char-matcher-p
                            is-begin-of-concat-p
                            is-begin-of-alter-p
                            is-end-of-concat-p
                            is-end-of-alter-p
                            is-or-matcher-p
                            constr-rexpr-from-sign-list)
  (loop with comp-trie = trie
        with res = result
        while comp-trie
        do 
        (if suffix
          (scanner-unit (+ 1 index)
                        expr
                        comp-trie
                        res
                        NIL ;; that means a wildcard is the prefix
                        is-leaf-p
                        index-eq
                        get-son-with-index
                        do-forall-sons 
                        get-nth-elem
                        get-expr-len
                        constr-path
                        output-fn
                        is-zero-or-more-matcher-p
                        is-one-or-more-matcher-p
                        is-zero-or-one-matcher-p
                        is-any-char-matcher-p
                        is-begin-of-concat-p
                        is-begin-of-alter-p
                        is-end-of-concat-p
                        is-end-of-alter-p
                        is-or-matcher-p
                        constr-rexpr-from-sign-list)
          ;; else test if there is leaf
          (funcall output-fn res comp-trie))
        ;; The cocatenation of signs stored in the arguement prefix is 
        ;; matched in this loop.
        (loop for sign in prefix
              for i from 1
              with h-res = res
              while comp-trie
              do
              (setf comp-trie (funcall get-son-with-index sign comp-trie))
              (when comp-trie
                (setf h-res (funcall constr-path comp-trie h-res)))
              finally (if (= i (length prefix))
                        (setf res h-res)
                        ;; else
                        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; preorder ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The function runs through the given trie and visit all nodes according to 
;;; preorder strategy. We distinguish two primary cases. First we have no
;;; suffix behind the kleene star. In this case we give the accumulated path 
;;; characters and the information under leaf-name to the 
;;; function print-result when we reach a leaf. Otherwise we have to process
;;; a suffix. In this case we have again two possibilities. Is the first 
;;; item of the suffix sequence a character can we first test whether 
;;; the actual node is equal with the lookup character and only then 
;;; a new task is produced. In case of no lookup we have to create always
;;; a task.

(defun preorder (index expr trie result suffix
                       is-leaf-p
                       index-eq
                       get-son-with-index
                       do-forall-sons 
                       get-nth-elem
                       get-expr-len
                       constr-path
                       output-fn
                       is-zero-or-more-matcher-p
                       is-one-or-more-matcher-p
                       is-zero-or-one-matcher-p
                       is-any-char-matcher-p
                       is-begin-of-concat-p
                       is-begin-of-alter-p
                       is-end-of-concat-p
                       is-end-of-alter-p
                       is-or-matcher-p
                       constr-rexpr-from-sign-list)
  (funcall do-forall-sons trie
           #'(lambda (node)
               (let ((res (funcall constr-path node result)))
                 (if suffix
                   ;; Now we have to build a new task to process the suffix
                   ;; that means the following characters after the star.
                   (when (funcall index-eq suffix node)
                     (scanner-unit  (+ 2 index)
                                    expr
                                    node
                                    res
                                    NIL
                                    is-leaf-p
                                    index-eq
                                    get-son-with-index
                                    do-forall-sons 
                                    get-nth-elem
                                    get-expr-len
                                    constr-path
                                    output-fn
                                    is-zero-or-more-matcher-p
                                    is-one-or-more-matcher-p
                                    is-zero-or-one-matcher-p
                                    is-any-char-matcher-p
                                    is-begin-of-concat-p
                                    is-begin-of-alter-p
                                    is-end-of-concat-p
                                    is-end-of-alter-p
                                    is-or-matcher-p
                                    constr-rexpr-from-sign-list))
                   ;; else there is no suffix behind the actual matcher
                   ;; and we can give out a result 
                   (funcall output-fn res node))
                 (unless (funcall is-leaf-p node)
                   (preorder index expr node res suffix
                             is-leaf-p
                             index-eq
                             get-son-with-index
                             do-forall-sons 
                             get-nth-elem
                             get-expr-len
                             constr-path
                             output-fn
                             is-zero-or-more-matcher-p
                             is-one-or-more-matcher-p
                             is-zero-or-one-matcher-p
                             is-any-char-matcher-p
                             is-begin-of-concat-p
                             is-begin-of-alter-p
                             is-end-of-concat-p
                             is-end-of-alter-p
                             is-or-matcher-p
                             constr-rexpr-from-sign-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; begin-of-concat ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun begin-of-concat (index expr trie result
                              is-leaf-p
                              index-eq
                              get-son-with-index
                              do-forall-sons 
                              get-nth-elem
                              get-expr-len
                              constr-path
                              output-fn
                              is-zero-or-more-matcher-p
                              is-one-or-more-matcher-p
                              is-zero-or-one-matcher-p
                              is-any-char-matcher-p
                              is-begin-of-concat-p
                              is-begin-of-alter-p
                              is-end-of-concat-p
                              is-end-of-alter-p
                              is-or-matcher-p
                              constr-rexpr-from-sign-list)
  (multiple-value-bind (new-index suffix-list new-trie new-res)
                       (loop for i from (+ index 1)
                             for sign = (funcall get-nth-elem i expr)
                             with comp-trie = trie
                             with h-res = result
                             while
                             (and comp-trie
                                  (not (funcall is-end-of-concat-p sign)))
                             do 
                             (setf comp-trie (funcall get-son-with-index
                                                      sign comp-trie))
                             (when comp-trie
                               (setf h-res 
                                     (funcall constr-path comp-trie h-res)))
                             collect sign into sign-list
                             finally (return (values i sign-list comp-trie
                                                     h-res)))
    (when new-trie
      (scanner-unit (+ 1 new-index)
                    expr
                    new-trie
                    new-res
                    suffix-list
                    is-leaf-p
                    index-eq
                    get-son-with-index
                    do-forall-sons 
                    get-nth-elem
                    get-expr-len
                    constr-path
                    output-fn
                    is-zero-or-more-matcher-p
                    is-one-or-more-matcher-p
                    is-zero-or-one-matcher-p
                    is-any-char-matcher-p
                    is-begin-of-concat-p
                    is-begin-of-alter-p
                    is-end-of-concat-p
                    is-end-of-alter-p
                    is-or-matcher-p
                    constr-rexpr-from-sign-list))
    (lookahead-for-wildcard new-index expr trie result
                            is-leaf-p
                            index-eq
                            get-son-with-index
                            do-forall-sons 
                            get-nth-elem
                            get-expr-len
                            constr-path
                            output-fn
                            is-zero-or-more-matcher-p
                            is-one-or-more-matcher-p
                            is-zero-or-one-matcher-p
                            is-any-char-matcher-p
                            is-begin-of-concat-p
                            is-begin-of-alter-p
                            is-end-of-concat-p
                            is-end-of-alter-p
                            is-or-matcher-p
                            constr-rexpr-from-sign-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; begin-of-alter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The matcher begins after the open brace and take the partial string until
;;; the pipe symbol "|". Then it creates a new task with the old result and
;;; a new expression build from the alternation and the before computed suffix.
;;; Any Kleene modificator behind the alternation is not allowed. 

(defun begin-of-alternation (index expr trie result
                                   is-leaf-p
                                   index-eq
                                   get-son-with-index
                                   do-forall-sons 
                                   get-nth-elem
                                   get-expr-len
                                   constr-path
                                   output-fn
                                   is-zero-or-more-matcher-p
                                   is-one-or-more-matcher-p
                                   is-zero-or-one-matcher-p
                                   is-any-char-matcher-p
                                   is-begin-of-concat-p
                                   is-begin-of-alter-p
                                   is-end-of-concat-p
                                   is-end-of-alter-p
                                   is-or-matcher-p
                                   constr-rexpr-from-sign-list)
  (let ((start (+ 1 index)))
    (multiple-value-bind 
      (suffix-to-add brace-pos)
      (loop for i from start
            for sign = (funcall get-nth-elem i expr)
            while (not (funcall is-end-of-alter-p sign))
            finally (return (values (loop for j from (+ i 1) to
                                          (- (funcall get-expr-len expr) 1)
                                          for signn = (funcall get-nth-elem
                                                               j expr)
                                          collect signn)
                                    i)))
      (loop until (= start (+ 1 brace-pos))
            do
            (multiple-value-bind (pointer one-alter)
                                 (loop for k from start to (- brace-pos 1)
                                       for signnn = (funcall get-nth-elem k expr)
                                       while (not (funcall is-or-matcher-p 
                                                           signnn))
                                       collect signnn into signn-list
                                       finally (return (values k signn-list)))
              (scanner-unit
               0
               (funcall constr-rexpr-from-sign-list
                        (append one-alter suffix-to-add))
               trie
               result
               NIL
               is-leaf-p
               index-eq
               get-son-with-index
               do-forall-sons 
               get-nth-elem
               get-expr-len
               constr-path
               output-fn
               is-zero-or-more-matcher-p
               is-one-or-more-matcher-p
               is-zero-or-one-matcher-p
               is-any-char-matcher-p
               is-begin-of-concat-p
               is-begin-of-alter-p
               is-end-of-concat-p
               is-end-of-alter-p
               is-or-matcher-p
               constr-rexpr-from-sign-list)
              (setf start (+ pointer 1)))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scanner-unit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scanner-unit (index expr trie result prefix
                           is-leaf-p
                           index-eq
                           get-son-with-index
                           do-forall-sons 
                           get-nth-elem
                           get-expr-len
                           constr-path
                           output-fn
                           is-zero-or-more-matcher-p
                           is-one-or-more-matcher-p
                           is-zero-or-one-matcher-p
                           is-any-char-matcher-p
                           is-begin-of-concat-p
                           is-begin-of-alter-p
                           is-end-of-concat-p
                           is-end-of-alter-p
                           is-or-matcher-p
                           constr-rexpr-from-sign-list)
  (if (> index (- (funcall get-expr-len expr) 1))
    ;; ok, we have reached the end of the regular expression. Give out the
    ;; concatenated paths via arguement result and the contents leaf of the
    ;; given tries root node.
    (funcall output-fn result trie)
    ;; else we must scan the next sign
    (let ((scan-sign (funcall get-nth-elem index expr)))
      (cond
       ((funcall is-any-char-matcher-p scan-sign) 
        (any-char-matcher index expr trie result
                          is-leaf-p
                          index-eq
                          get-son-with-index
                          do-forall-sons 
                          get-nth-elem
                          get-expr-len
                          constr-path
                          output-fn
                          is-zero-or-more-matcher-p
                          is-one-or-more-matcher-p
                          is-zero-or-one-matcher-p
                          is-any-char-matcher-p
                          is-begin-of-concat-p
                          is-begin-of-alter-p
                          is-end-of-concat-p
                          is-end-of-alter-p
                          is-or-matcher-p
                          constr-rexpr-from-sign-list))
       ((funcall is-zero-or-one-matcher-p scan-sign)
        (scanner-unit (+ index 1) expr trie result NIL
                      is-leaf-p
                      index-eq
                      get-son-with-index
                      do-forall-sons 
                      get-nth-elem
                      get-expr-len
                      constr-path
                      output-fn
                      is-zero-or-more-matcher-p
                      is-one-or-more-matcher-p
                      is-zero-or-one-matcher-p
                      is-any-char-matcher-p
                      is-begin-of-concat-p
                      is-begin-of-alter-p
                      is-end-of-concat-p
                      is-end-of-alter-p
                      is-or-matcher-p
                      constr-rexpr-from-sign-list))
       ((funcall is-one-or-more-matcher-p scan-sign)
        (one-or-more-matcher index expr trie result prefix 
                             is-leaf-p
                             index-eq
                             get-son-with-index
                             do-forall-sons 
                             get-nth-elem
                             get-expr-len
                             constr-path
                             output-fn
                             is-zero-or-more-matcher-p
                             is-one-or-more-matcher-p
                             is-zero-or-one-matcher-p
                             is-any-char-matcher-p
                             is-begin-of-concat-p
                             is-begin-of-alter-p
                             is-end-of-concat-p
                             is-end-of-alter-p
                             is-or-matcher-p
                             constr-rexpr-from-sign-list))
       ((funcall is-zero-or-more-matcher-p scan-sign)
        (one-or-more-matcher index expr trie result prefix 
                             is-leaf-p
                             index-eq
                             get-son-with-index
                             do-forall-sons 
                             get-nth-elem
                             get-expr-len
                             constr-path
                             output-fn
                             is-zero-or-more-matcher-p
                             is-one-or-more-matcher-p
                             is-zero-or-one-matcher-p
                             is-any-char-matcher-p
                             is-begin-of-concat-p
                             is-begin-of-alter-p
                             is-end-of-concat-p
                             is-end-of-alter-p
                             is-or-matcher-p
                             constr-rexpr-from-sign-list))
       ((funcall is-begin-of-concat-p scan-sign)
        (begin-of-concat index expr trie result
                         is-leaf-p
                         index-eq
                         get-son-with-index
                         do-forall-sons 
                         get-nth-elem
                         get-expr-len
                         constr-path
                         output-fn
                         is-zero-or-more-matcher-p
                         is-one-or-more-matcher-p
                         is-zero-or-one-matcher-p
                         is-any-char-matcher-p
                         is-begin-of-concat-p
                         is-begin-of-alter-p
                         is-end-of-concat-p
                         is-end-of-alter-p
                         is-or-matcher-p
                         constr-rexpr-from-sign-list))
       ((funcall is-begin-of-alter-p scan-sign) 
        (begin-of-alternation index expr trie result
                              is-leaf-p
                              index-eq
                              get-son-with-index
                              do-forall-sons 
                              get-nth-elem
                              get-expr-len
                              constr-path
                              output-fn
                              is-zero-or-more-matcher-p
                              is-one-or-more-matcher-p
                              is-zero-or-one-matcher-p
                              is-any-char-matcher-p
                              is-begin-of-concat-p
                              is-begin-of-alter-p
                              is-end-of-concat-p
                              is-end-of-alter-p
                              is-or-matcher-p
                              constr-rexpr-from-sign-list))
       (T (char-matcher scan-sign index expr trie result
                        is-leaf-p
                        index-eq
                        get-son-with-index
                        do-forall-sons 
                        get-nth-elem
                        get-expr-len
                        constr-path
                        output-fn
                        is-zero-or-more-matcher-p
                        is-one-or-more-matcher-p
                        is-zero-or-one-matcher-p
                        is-any-char-matcher-p
                        is-begin-of-concat-p
                        is-begin-of-alter-p
                        is-end-of-concat-p
                        is-end-of-alter-p
                        is-or-matcher-p
                        constr-rexpr-from-sign-list))))))

;;;;;;;;;;;;;;;;;; Example: The default matcher functions ;;;;;;;;;;;;;;;;;;;;;

;; Functions for the abstraction of a tree
;; default functions are for the Dtree structures
;; implemented by G. Neumann, DFKI

(defun default-is-leaf-p (node)
  (and (not (node-daughters node)) T))

;; Functions for matching a element of the given regular expression with a
;; index of tree node

(defun default-token-and-node-index-eq (token-of-rexpr node)
  (char= token-of-rexpr (node-index node)))

(defun default-get-son-with-index (token-of-rexpr node)
  (find token-of-rexpr (node-daughters node)
        :test #'default-token-and-node-index-eq))

(defun default-do-forall-sons (node fn)
  (loop for one-son in (node-daughters node)
        do (funcall fn one-son)))

(defun default-get-nth-elem (index object)
  (char object index))

(defun default-get-expr-len (object)
  (length object))

(defun default-constr-path (node old-path)
  (let ((index-of-node (node-index node)))
    (append old-path (list index-of-node))))

(defun default-output-fn (path node)
  (let ((leaf (cdr (assoc ':info (node-content node)))))
    (when leaf
      (format T "\"~{~A~}\" ~A~%" path leaf))))

(defun default-is-zero-or-more-matcher-p (token)
  (char= token #\*))

(defun default-is-zero-or-one-matcher-p (token)
  (char= token #\?))

(defun default-is-one-or-more-matcher-p (token)
  (char= token #\+))

(defun default-is-any-char-matcher-p (token)
  (char= token #\.))

(defun default-is-begin-of-concat-p (token)
  (char= token #\())

(defun default-is-begin-of-alter-p (token)
  (char= token #\{))

(defun default-is-end-of-concat-p (token)
  (char= token #\)))

(defun default-is-end-of-alter-p (token)
  (char= token #\}))

(defun default-is-or-matcher-p (token)
  (char= token #\|))

(defun default-constr-rexpr-from-token-list (token-list)
  (coerce token-list 'string))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; match-entry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main function, should be exported. Matches regular over a trie. The
;;; used trie structure is a product of Dr.G.Neumann, DFKI Saarbruecken.
;;; Arguments are:
;;; <expr> a "string" describing the regular expression
;;; <lexicon> a trie structure containing the words
;;; Keywords are:
;;; :help any lisp object, gives a help text if the given object has length
;;; one and is a special character of the regular syntax set. In other cases
;;; all help texts are printed out.

(defun match-entry (expr lexicon &key 
                         (help NIL)
                         (is-leaf-p #'default-is-leaf-p)
                         (index-eq #'default-token-and-node-index-eq)
                         (get-son-with-index #'default-get-son-with-index)
                         (do-forall-sons #'default-do-forall-sons)
                         ;; Functions for the regular expression abstraction
                         (get-nth-elem #'default-get-nth-elem)
                         (get-expr-len #'default-get-expr-len)
                         (constr-path #'default-constr-path)
                         (output-fn #'default-output-fn)
                         (constr-rexpr-from-token-list 
                          #'default-constr-rexpr-from-token-list)
                         ;; Functions describing the matcher tokens
                         (is-zero-or-more-matcher-p 
                          #'default-is-zero-or-more-matcher-p)
                         (is-one-or-more-matcher-p
                          #'default-is-one-or-more-matcher-p)
                         (is-zero-or-one-matcher-p 
                          #'default-is-zero-or-one-matcher-p)
                         (is-any-object-matcher-p 
                          #'default-is-any-char-matcher-p)
                         (is-begin-of-concat-p
                          #'default-is-begin-of-concat-p)
                         (is-begin-of-alter-p 
                          #'default-is-begin-of-alter-p)
                         (is-end-of-concat-p 
                          #'default-is-end-of-concat-p)
                         (is-end-of-alter-p 
                          #'default-is-end-of-alter-p)
                         (is-or-matcher-p 
                          #'default-is-or-matcher-p))
  "Top level function for regular matching over a trie. The first argument ~
   is the regular expression (string) ~
   and the second is the lexicon (trie).~% Keyword ~
   arguments are:~% help ~
   any object giving a help text out if the object is special character of ~
   regular expressions. Has the given object a length bigger than 2 then ~
   the complete help text is printed on the screen output. Other Keywords are:~%
   is-leaf-p ~%index-eq ~%get-son-with-index ~%do-forall-sons ~%~
   get-nth-elem ~%get-expr-len~%constr-path~%output-fn~%~
   is-zero-or-more-matcher-p~%is-one-or-more-matcher-p~%~
   is-zero-or-one-matcher-p~%is-any-char-matcher-p~%~
   is-begin-of-concat-p~%is-begin-of-alter-p~%is-end-of-concat-p~%~
   is-end-of-alter-p~%is-or-matcher-p~%constr-rexpr-from-token-list"
  (if help
    (match-entry-help (name-char help))
    ;; else do it
    (scanner-unit 0 expr lexicon nil T
                  is-leaf-p
                  index-eq
                  get-son-with-index
                  do-forall-sons 
                  get-nth-elem
                  get-expr-len
                  constr-path
                  output-fn
                  is-zero-or-more-matcher-p
                  is-one-or-more-matcher-p
                  is-zero-or-one-matcher-p
                  is-any-object-matcher-p
                  is-begin-of-concat-p
                  is-begin-of-alter-p
                  is-end-of-concat-p
                  is-end-of-alter-p
                  is-or-matcher-p
                  constr-rexpr-from-token-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; match-entry-help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The help function for the main function match-entry.



(defun match-entry-help (matcher-char)
  (let ((h-any-char-matcher 
         "The `.' matches any character. It can be modified by `+', `*' ~
          and `?'.~%Example: \"n.men\" matches all words beginning with a `n' ~
          followed by one variable character and ending with `men'.")
        (h-char-matcher 
         "Any character c except `(', `)', `\{', `\|', `\}', `.', ~
          `+', `*' and `?'; matches itself. It can be modified by `*', `+' and ~
           `?'.~%Example \"ni\" ~
           matches first the character `n`. and then the character `i`. ~
           The character matcher is not casesensitiv !")
        (h-zero-or-more-matcher
         "`*' matches the single character regular expression or subexpression ~
           immadiately preceding it zero or more times. It is not allowed ~
           two combine to regular expressions of the form `.*'. For example ~
           the expression \".*a.*\" is not correct, because the matching ~
           `spaces' of the first `.*' and the second `.*' can overlap and ~
           results can appear twice or more. Also be careful with the `.*' ~
           and `+' combination. For example \".*n+\" \"n+.*\" are also ~
           incorrect because it is possible that same results appear twice ~
           or more. Generally avoid using the `.*' with  other plus or star ~
           matcher expressions if there are not separated by one or more ~
           single characters.~%Example: \".*\" matches all entries ~
           of a trie and \"b*ung\" matches all words beginnig ~
           with zero or more b's and then ending with `ung'.~%ATTENTION: ~
           Any suffix behind a `.*' or `.+' must begin with a sign that have ~
           NO special meaning, that means: None of the matcher signs like `*', ~
           `+', `?', `(', `)', `\{', `\|', `\}' and `.' can be the first sign ~
           after a combination of `.*' or `.+'. ")
        (h-one-or-more-matcher
         "`+' matches the single character regular expression or ~
          subexpression immadiately preceding it one or more times. So the ~
          regular expression `a+' is shorthand for `aa*'. See also the help ~
          for `*'.~%Example: \"ha+r.*\" matches all words beginning with `h' ~
          followed by one or more a's and ending with `r'")
        (h-zero-or-one-matcher
         "`?' optionally matches the single character regular expression or ~
          subexpression immadiately preceding it. Be careful when combining ~
          `.*' and a expression with `?', see the help text of `*' for more ~
          details. ~%Example \".+mm?\" matches all words ending with one or ~
          two m's.")
        (h-begin-concat
         "`(re)' defines a (possibly null) character subexpression re. ~
          Subexpressions can NOT be nested and re describes only a sequence ~
          of characters. This means that re is NOT a regular expression. ~
          It can be modified by `*', `+' and `?'.~%Example:\"(ab)+\" matches ~
          all words build by concatenating the sequence `ab' one or more ~
          times. For example \"(ab*c)\" or \"((ab)c)\" are NOT allowed.")
        (h-begin-alter
         "`{re_1|re_2|...|re_n}' matches the subexpressions re_1 and ~
          re_2 and ... and re_n. The subexpression re_i can be any regular ~
          expression. So the hole `{}' expression can't be modified by the ~
          kleene operators.~%Examples: \"{foo|bar*}\" matches the word `foo' ~
          and the regular expression `bar*'. For example \"{foo|bar}*\" or ~
          \"{{foo|hi}|bar}\" are NOT allowed !")
        (delimitation-string "~%~%")
        (h-is-leaf-p "is-leaf-p(<node>) ~0,70:T[Function]~%Description: ~
                      A function that takes as input a <node> of your tree ~
                      and gives T back in case of <node> is a leaf otherwise ~
                      NIL~%E.g. default-is-leaf-p()")
        (h-index-eq "index-eq(<token> <node>)~0,70:T[Function]~%Description: ~
                     The function tests ~
                     whether the <token> is equal to the index of the <node> ~
                     (T) or not (NIL)~%~
                     E.g. default-token-and-node-index-eq()")
        (h-get-son-with-index "get-son-with-index(<token> <node>)~0,70:T~
                               [Function]~%~
                               Description: A function with the two arguments ~
                               <token> and ~
                             <node>. Tries to find a child of ~
                             <node> that index is index-eq() with <token>. ~
                             If there is no child the function must give NIL ~
                             back.~%E.g. default-get-son-with-index()")
        (h-do-forall-sons "do-for-all-sons(<node> <function>)~0,70:T~
                           [Function]~%Description: ~
                         Takes a <node> and a <function> and applies the ~
                         <function> to all sons of the <node>.~%E.g. ~
                         defautl-do-for-all-sons()")
        (h-get-nth-elem "get-nth-elem(<n> <rexepr>)~0,70:T~
                        [Function]~%Description: Computes the ~
                       nth token of the regular expression <rexepr>.~%E.g. ~
                       default-get-nth-elem()")
        (h-get-expr-len "get-expr-len(<rexepr>)~0,70:T[Function]~%Description: ~
                        Computes the length ~
                       of the regular expression <rexepr>, that means the ~
                       number of all tokens. ~%E.g. default-get-expr-len()")
        (h-constr-path "constr-path(<node> <path>)~0,70:T[Function]~%~
                        Description: Takes a <node> ~
                      and a yet constructed particular <path>. The function ~
                      build a new path by adding the index of the <node> to ~
                      the old <path>. Attention: The matcher begins with the ~
                      root and goes into the deep. So you must control the ~
                      succession by construction of the path or by the output ~
                      in the function output-fn.~%E.g. default-constr-path()")
        (h-output-fn "output-fn(<path> <node>)~0,70:T[Function]~%Description: ~
                    Gets a <path> ~
                    constructed by the function constr-path() and a <node>. ~
                    There is no limitation given for the functionality of this ~
                    function, but you should attend to the content of the ~
                    <node>, because the matcher don't check if a node has a ~
                    information leaf. That means: The matcher dont't look in ~
                    the ~
                    content of the <node> before it calls the output-fn(). ~
                    For example you can use this function for printing out the ~
                    information found under the content field and the <path> ~
                    or you can ignore both arguments and increment only a ~
                    counter if the function is called (So you can count all ~
                    nodes ~
                    of a tree if the regular expression is \".*\") etc.~%E.g. ~
                    default-ouput-fn()")
        (h-is-zero-or-more-matcher-p "is-zero-or-more-matcher-p(<token>)~0,70:T~
                                    [Function]~%Description: The ~
                                    function defines a predicat that checks ~
                                    whether a given <token> of the regular ~
                                    expression is a zero or more times matcher ~
                                    (T) or not (NIL).~%E.g. ~
                                    default-is-zero-or-more-matcher-p()")
        (h-is-one-or-more-matcher-p "is-one-or-more-matcher-p(<token>)~0,70:T~
                                   [Function]~%Description: The ~
                                   function defines a predicat that checks ~
                                   whether a given <token> of the regular ~
                                   expression is a one or more times matcher ~
                                   (T) or not (NIL).~%E.g. ~
                                   default-is-one-or-more-matcher-p()") 
        (h-is-zero-or-one-matcher-p "is-zero-or-one-matcher-p(<token>)~0,70:T~
                                   [Function]~%Description: The ~
                                   function defines a predicat that checks ~
                                   whether a given <token> of the regular ~
                                   expression is a zero or more one time ~
                                   matcher ~
                                   (T) or not (NIL).~%E.g. ~
                                   default-is-zero-or-one-matcher-p()") 
        (h-is-any-object-matcher-p "is-any-object-matcher-p(<token>)~0,70:T~
                                  [Function]~%Description: The ~
                                  function defines a predicat that checks ~
                                  whether a given <token> of the regular ~
                                  expression is an any object matcher ~
                                  (T) or not (NIL).~%E.g. ~
                                  default-any-object-matcher-p()") 
        (h-is-begin-of-concat-p "is-begin-of-concat-p(<token>)~0,70:T[Function]~
                               ~%Description: The ~
                               function defines a predicat that checks ~
                               whether a given <token> of the regular ~
                               expression is the begin of a concatenation ~
                               (T) or not (NIL).~%E.g. ~
                               default-is-begin-of-concat-p()")
        (h-is-begin-of-alter-p "is-begin-of-alter-p(<token>)~0,70:T~
                              [Function]~%Description: The ~
                              function defines a predicat that checks ~
                              whether a given <token> of the regular ~
                              expression is the begin of an alternation ~
                              (T) or not (NIL).~%E.g. ~
                              default-is-begin-of-alter-p()") 
        (h-is-end-of-concat-p "is-end-of-concat-p(<token>)~0,70:T~
                             [Function]~%Description: The ~
                             function defines a predicat that checks ~
                             whether a given <token> of the regular ~
                             expression is the end of a concatenation ~
                             (T) or not (NIL).~%E.g. ~
                             default-is-end-of-concat-p()")
        (h-is-end-of-alter-p "is-end-of-alter-p(<token>)~0,70:T~
                            [Function]~%Description: The ~
                            function defines a predicat that checks ~
                            whether a given <token> of the regular ~
                            expression is the end of an alternation ~
                            (T) or not (NIL).~%E.g. ~
                            default-is-end-of-alter-p()")
        (h-is-or-matcher-p "is-or-matcher-p(<token>)~0,70:T~
                          [Function]~%Description: The ~
                          function defines a predicat that checks ~
                          whether a given <token> of the regular ~
                          expression is the object that separates the ~
                          alternatives of an alternation ~
                          (T) or not (NIL).~%E.g. ~
                          default-is-or-matcher-p()")
      (h-constr-rexpr-from-token-list "constr-rexpr-from-token-list~
                                       (<list of tokens>)~0,70:T[Function]~%~
                                       Description: Converts a list of single ~
                                       tokens of a regular expression to a new ~
                                       regular expression.~%E.g. ~
                                       default-constr-rexpr-from-token-list()"))
     (if matcher-char
       (case matcher-char
         (#\. (format T h-any-char-matcher))
         (#\* (format T h-zero-or-more-matcher))
         (#\+ (format T h-one-or-more-matcher))
         (#\? (format T h-zero-or-one-matcher))
         ((#\( #\)) (format T h-begin-concat))
         ((#\{ #\} #\|) (format T h-begin-alter))
         (T (format T h-char-matcher)))
       ;; else print all help texts
       (progn
         (format T "Descriptions of the matchers semantics with the default ~
                    matcher configuration. The behaviour of the single ~
                    matchers ~
                    is indepentent of the choice of the object they represent. ~
                    So the descriptions are valid for all matchers. :~%~%")
         (format T h-char-matcher)
         (format T delimitation-string)
         (format T h-any-char-matcher)
         (format T delimitation-string)
         (format T h-one-or-more-matcher)
         (format T delimitation-string)
         (format T h-zero-or-more-matcher)
         (format T delimitation-string)
         (format T h-zero-or-one-matcher)
         (format T delimitation-string)
         (format T h-begin-concat)
         (format T delimitation-string)
         (format T h-begin-alter)
         (format T delimitation-string)
         (format T h-is-leaf-p)
         (format T delimitation-string)
         (format T h-index-eq)
         (format T delimitation-string)
         (format T h-get-son-with-index)
         (format T delimitation-string)
         (format T h-do-forall-sons)
         (format T delimitation-string)
         (format T h-get-nth-elem)
         (format T delimitation-string)
         (format T h-get-expr-len)
         (format T delimitation-string)
         (format T h-constr-path)
         (format T delimitation-string)
         (format T h-output-fn)
         (format T delimitation-string)
         (format T h-is-zero-or-more-matcher-p)
         (format T delimitation-string)
         (format T h-is-one-or-more-matcher-p) 
         (format T delimitation-string)
         (format T h-is-zero-or-one-matcher-p)
         (format T delimitation-string) 
         (format T h-is-any-object-matcher-p)
         (format T delimitation-string)
         (format T h-is-begin-of-concat-p)
         (format T delimitation-string)
         (format T h-is-begin-of-alter-p)
         (format T delimitation-string) 
         (format T h-is-end-of-concat-p)
         (format T delimitation-string)
         (format T h-is-end-of-alter-p)
         (format T delimitation-string)
         (format T h-is-or-matcher-p)
         (format T delimitation-string)
         (format T h-constr-rexpr-from-token-list)))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defvar *stelex* morphix::*stem-lexicon*)

(defvar *fullfo* morphix::*fullform-lexicon*)


(defvar test (make-dtree))

(insert-entry "haben" 'haben test :test #'equal)

(insert-entry "heben" 'heben test :test #'equal)

(insert-entry "he" 'he test :test #'equal)

;;(insert-entry "h" 'h test :test #'equal)
;; geht im Moment nicht, da der liebe Guenther Probleme mit 
;; Eintraegen hat, die nur aus einem Key bestehen.

(insert-entry "ha" 'ha test :test #'equal)

(insert-entry "hben" 'hben test :test #'equal)

(insert-entry "haaaben" 'haaaben test :test #'equal)

(insert-entry "haasss" 'haasss test :test #'equal)

(insert-entry "geben" 'geben test :test #'equal)

(insert-entry "ababababab" 'ababababab test :test #'equal)

(insert-entry "haaaatschi" 'haaaatschi test :test #'equal)

(insert-entry "Versicherung" 'Versicherung test :test #'equal)

(insert-entry "Sicherheitsring" 'Sicherheitsring test :test #'equal)

(insert-entry "holen" 'holen test :test #'equal )

(insert-entry "haen" 'haen test :test #'equal)

|#
