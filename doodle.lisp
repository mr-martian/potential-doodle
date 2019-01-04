;(node ntype (:p1 v1 :p2 v2) left right)
;macro: (barnode ntype (:p1 v1 :p2 v2) spec mod head bar)
;(morph ntype (:p1 v1 :p2 v2) root)
;(var ???)

(defvar *lang*)

(defmacro node (ntype args left right)
  `(list 'node ',ntype ,args ,left ,right))
(defmacro morph (ntype args root)
  `(list 'morph ',ntype ,args ,root))
(defmacro var (name &rest reqs)
  `(list 'var ,(intern (symbol-name name) 'keyword) ,@reqs))
                                        ; todo: error checking?

(defun flatten (tree)
  (if (listp tree)
      (mapcan #'flatten tree)
    (list tree)))
(defmacro basic-rule (form result)
  (let ((sen (gensym)) (vars (gensym)) (match (gensym)))
    `(lambda (,sen)
       (multiple-value-bind
        (,vars ,match) (getvars ,sen ,form)
        (when ,match
          (apply (lambda (&key ,@(remove-duplicates (flatten result)))
                   ,result)
                 ,vars))))))

(defun apply-rules (ruleset sen)
  (let ((result (list sen)))
    (loop for stage in ruleset do
          (setf result (loop for s in result appending
                             (or (loop for rule in stage
                                       if (funcall rule s)
                                       collect it)
                                 (list s)))))
    result))

(defmacro barnode (ntype args &rest parts)
  (let* ((name (symbol-name ntype))
         (base (subseq name 0 (1- (length name))))
         (nodes (mapcar (lambda (x)
                          (intern (format nil "~a~a" base x)))
                        '(P mod bar))))
    (case (length parts)
          (0 `(node ,(first nodes) ,args nil
                    (node ,(second nodes) nil nil
                          (node ,(third nodes) nil nil nil))))
          (1 `(node ,(first nodes) ,args nil
                    (node ,(second nodes) nil nil
                          (node ,(third nodes) nil ,(first parts) nil))))
          (2 `(node ,(first nodes) ,args nil
                    (node ,(second nodes) nil nil
                          (node ,(third nodes) nil
                                ,(first parts) ,(second parts)))))
          (3 `(node ,(first nodes) ,args ,(first parts)
                    (node ,(second nodes) nil nil
                          (node ,(third nodes) nil
                                ,(second parts) ,(third parts)))))
          (t `(node ,(first nodes) ,args ,(first parts)
                    (node ,(second nodes) nil ,(second parts)
                          (node ,(third nodes) nil
                                ,(third parts) ,(fourth parts))))))))
(defmacro barnode-key (ntype args &key spec mod head bar)
  `(barnode ,ntype ,args ,spec ,mod ,head ,bar))


(defun getvars (tree pattern)
  (labels
   ((fail ()
          (return-from getvars (values nil nil)))
    (gv (tr pt)
        (cond
         ((and (listp pt) (eq (car pt) 'var))
          (list (cadr pt) tr)) ; variable
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
         ; and we'll eventually need to check globals for morphemes (probably)
         ((eq (car tr) (car pt))
          (mapcan #'gv (cdr tr) (cdr pt)))
         (t (fail)))))
   (values (gv tree pattern) t)))
