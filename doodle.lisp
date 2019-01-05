;(node ntype (:p1 v1 :p2 v2) left right)
;macro: (barnode ntype (:p1 v1 :p2 v2) spec mod head bar)
;(morph ntype (:p1 v1 :p2 v2) root)
;(var name :p1 v1 :p2 v2)

(defvar *lang*)

(defun flatten (tree)
  (if (listp tree)
      (mapcan #'flatten tree)
    (list tree)))
(defun symb (&rest parts)
  (intern (format nil "~{~a~}" parts)))
(defvar lambda-bracket-level 0)
(defun bracket-lambda-reader (stream char)
  (declare (ignore char))
  (let* ((lambda-bracket-level (1+ lambda-bracket-level))
         (s (if (= lambda-bracket-level 1)
                '_
              (symb '_ lambda-bracket-level))))
    `(lambda (,s) ,(read-delimited-list #\] stream t))))
(set-macro-character #\[ #'bracket-lambda-reader)
(set-macro-character #\] (get-macro-character #\) nil))
(defmacro let-gen (syms &body body)
  `(let ,(mapcar [list _ `',(gensym (symbol-name _))] syms) ,@body))

(defmacro node (ntype args &rest children)
  `(list 'node ',ntype ,args ,@children))
(defmacro morph (ntype args root)
  (let-gen
   (res exists2 pos exists1)
   (let ((err (format nil "Morpheme ~a does not exist in language ~a with part of speech ~a." root *lang* ntype)))
     `(list 'morph ',ntype
            (multiple-value-bind
             (,pos ,exists1) (gethash ',ntype (get *lang* 'lexicon))
             (if ,exists1
                 (multiple-value-bind
                  (,res ,exists2) (gethash ',root ,pos)
                  (if ,exists2
                      (append (list ,@args) ,res)
                    (error ,err)))
               (error ,err)))))))
; it might be worth experimenting to see if it would be faster to have
; getvars do lookups instead of copying the args list into each instance
(defun register-morph (pos root &rest args)
  (let ((lex (get *lang* 'lexicon)))
    (unless lex
      (setf (get *lang* 'lexicon) (make-hash-table :test #'eq)))
    (multiple-value-bind
     (posdct p?) (gethash pos lex)
     (unless p?
       (setf (gethash pos lex) (make-hash-table :test #'eq)))
     (multiple-value-bind
      (rootdct r?) (gethash root posdct)
      (if r?
          (error (format nil "Attempting to re-register root ~a with part of speech ~a in langauge ~a." root pos *lang*))
        (setf (gethash root posdct) args))))))
(defun register-many-morphs (pos args roots)
  (mapcar (lambda (r)
            (if (listp r)
                (register-morph pos (car r) :display (cadr r) . args)
              (register-morph pos r :display (string-downcase (symbol-name r))
                              . args)))
          roots))
(defmacro var (name &rest reqs)
  `(list 'var ,(intern (symbol-name name) 'keyword) ,@reqs))
                                        ; todo: error checking?
(defmacro barnode-key (ntype args &key spec mod head bar)
  (let* ((name (symbol-name ntype))
         (base (subseq name 0 (1- (length name)))))
    `(node ,(symb base 'P) ,args ,spec
           (node ,(symb base 'mod) nil ,mod
                 (node ,(symb base 'bar) nil ,head ,bar)))))
(defmacro barnode (ntype args &rest parts)
  `(barnode-key ,ntype ,args
                ,@(mapcan #'list (elt '(() (:head) (:head :bar)
                                        (:spec :head :bar)
                                        (:spec :mod :head :bar))
                                      (length parts))
                          parts)))

(defun lambdify-result (tree)
  (labels ((disvar (tr)
                   (if (listp tr)
                       (if (eq (car tr) 'var)
                           (cadr tr)
                         (mapcar #'disvar tr))
                     tr)))
          `(lambda (&key ,@(remove-if [or (not (symbolp _))
                                          (fboundp _)]
                                      (remove-duplicates (flatten tree)))
                         &allow-other-keys)
             ,(disvar tree))))
(defmacro basic-rule (form result)
  (let ((sen (gensym)) (vars (gensym)) (match (gensym)))
    `(lambda (,sen)
       (multiple-value-bind
        (,vars ,match) (getvars ,sen ,form)
        (when ,match
          (apply ,(lambdify-result result) ,vars))))))
(defmacro multi-rule (&rest layers)
  (let-gen
   (blk fn sen s all-rules rules f r vars parent-vars)
   `(lambda (,sen)
      (let ((,all-rules
             ',(mapcar [mapcar [list (car _2)
                                     (lambdify-result (or (cadr _2) (car _2)))]
                               _]
                       layers)))
        (block ,blk
               (labels
                ((,fn (,s ,rules ,parent-vars)
                      (if ,rules
                          (loop for (,f ,r) in (car ,rules)
                                if (getvars ,s ,f)
                                return
                                (let ((,vars it))
                                  (setf (getf ,vars :@)
                                        (,fn (getf ,vars :@)
                                             (cdr ,rules)))
                                  (apply ,r (append ,vars ,parent-vars)))
                                finally (return-from ,blk nil))
                        ,s)))
                (,fn ,sen ,all-rules nil)))))))
(defun setprop (node prop val)
  (setf (getf (caddr node) prop) val)
  node)
; it's possible that this will have unintended side-effects
; if so, add a copy operation
(defun copyprop (src sprop dest &optional dprop)
  (setf (getf (caddr dest) (or dprop sprop)) (getf (caddr src) sprop))
  dest)

(defun apply-rules (ruleset sen)
  (labels
   ((product (lists)
             (if lists
                 (let ((rest (product (cdr lists))))
                   (mapcan [mapcar [cons _ _2] rest] (car lists)))
               (list nil)))
    (stage (in-sen rules)
           (let ((s (if (and (listp in-sen) (eq (car in-sen) 'node))
                        (mapcar [append (subseq in-sen 0 3) _]
                                (product (mapcar [stage _ rules]
                                                 (cdddr in-sen))))
                      (list in-sen))))
             (mapcan [or (loop for rule in rules
                               if (funcall rule _)
                               collect it)
                         (list _)]
                     s))))
   (reduce (lambda (s r)
             (mapcan [stage _ r] s))
           (cons (list sen) ruleset))))
(defun getvars (tree pattern)
  (labels
   ((fail ()
          (return-from getvars (values nil nil)))
    (gv (tr pt)
        (cond
         ((and (listp pt) (eq (car pt) 'var)) ; variable
          (if (and (null tr) (not (getf pt :opt)))
              (fail)
            (append (list (cadr pt) tr)
                    (loop for (key arg) on (cddr pt) by #'cddr appending
                          (case key
                                (:ntype (unless (eq arg (cadr tr)) (fail)))
                                )))))
          ; todo: check variable conditions
         ((or (not (listp tr)) (not (listp pt)))
          (unless (equalp tr pt) (fail)))
         ((and (eq (type-of (car tr)) 'keyword)
               (not pt))) ; empty list matching arguments
         ((and (eq (type-of (car tr)) 'keyword)
               (eq (type-of (car pt)) 'keyword))
           ; arglists
          (loop for (prop val) on pt by #'cddr
                appending (if (member prop tr)
                              (gv (getf tr prop) val)
                            (fail))))
         ; there's probably a better (more efficient) way to do this
         ; and we may eventually need to check globals for morphemes
         ; (though see the definition of morph above for further discussion)
         ((eq (car tr) (car pt))
          (mapcan #'gv (cdr tr) (cdr pt)))
         (t (fail)))))
   (values (gv tree pattern) t)))

(defun language-syntax (language &rest stages)
  (setf (get language 'syntax) stages))
(defun language-linear (language &rest stages)
  (setf (get language 'linear) stages))
(defun language-surface (language &rest stages)
  (setf (get language 'surface) stages))
