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
(defmacro mvb-if (var form yes &optional no)
  (let-gen (ok)
           `(multiple-value-bind (,var ,ok) ,form
                                 (if ,ok ,yes ,no))))
(defun to-keyword (s)
  (intern (symbol-name s) 'keyword))
(defmacro let-if (var form yes &optional no)
  `(let ((,var ,form)) (if ,var ,yes ,no)))
(defun ensure-list (l)
  (if (listp l) l (list l)))
(defmacro setf-apply (&rest forms)
  `(setf ,@(loop for (a b) on forms by #'cddr
                 appending `(,a (funcall ,b ,a)))))
(defun build-iter (forms)
  (when forms
    (let ((cur (car forms)) (rest (cdr forms)))
      (cond
       ((listp (car cur)) `(progn ,(build-iter cur) ,(build-iter rest)))
       ((eq (car cur) 'for)
        (let (let-vars update check)
          (let-gen
           (loop-start loop-continue loop-end lname length index)
           (multiple-value-bind
             (body sub-vars end) (build-iter rest)
             (destructuring-bind
              (var mode ls &rest opts) (cdr cur)
              (when (eq mode 'leaves)
                (setf ls (list 'flatten ls)
                      mode 'in))
              (when (and (eq mode 'each) (member 'by opts))
                (setf mode 'group))
              (ccase mode
                     ('in
                      (setf let-vars `((,lname ,(if (member 'unique opts)
                                                    (list 'remove-duplicates ls)
                                                  ls)))
                            check `(null ,lname))
                      (nconc let-vars (if (listp var)
                                          `((,length (length ,lname)))
                                        (list var)))
                      (if (listp var)
                          (setf body `(destructuring-bind
                                       ,var (pop ,lname) ,body))
                        (setf update `((setf ,var (pop ,lname))))))
                     ('branches
                      (setf let-vars `((,lname (list ,ls)))
                            update `((setf ,var (pop ,lname))
                                     (if (and ,var (listp ,var))
                                         (progn
                                           (push (cdr ,var) ,lname)
                                           (push (car ,var) ,lname))
                                       (go ,loop-continue)))
                            check `(null ,lname)))
                     )
              (let-if where (getf (remove 'unique opts) 'where)
                      (setf update
                            (append update
                                    `((unless ,where (go ,loop-continue))))))
              `(let* ,(append let-vars sub-vars)
                 (macrolet ((continue () (list 'go ',loop-continue))
                            (break-for () (list 'go ',loop-end)))
                           (tagbody
                            ,loop-start
                            (when ,check (go ,loop-end))
                            ,@update
                            ,body
                            ,loop-continue
                            (go ,loop-start)
                            ,loop-end ,end))))))))
       ((eq (car cur) 'nest)
        (let-gen (newls)
                 `(let ((,newls ,(when (cadr cur) `(list ,(cadr cur)))))
                    (labels
                     ((collect (x) (push x ,newls)))
                     ,(build-iter rest))
                    (collect (nreverse ,newls)))))
       ((eq (car cur) 'seek)
        (let* ((form (cadr cur)) (multi (member 'multi cur))
               (cur (remove 'multi cur))
               (var (or (getf cur 'as) (gensym)))
               (fail (getf cur 'fail)) (found (gensym)) (check (gensym)))
          (values
           (if multi
               `(multiple-value-bind
                 (,var ,check) ,form
                 (if ,check (setf ,found t) (continue))
                 ,(build-iter rest))
             `(let ((,var ,form))
                (if ,var (setf ,found t) (continue))
                ,(build-iter rest)))
           `(,found) `(unless ,found ,fail))))
       (t `(progn ,cur ,(build-iter rest)))))))
(defmacro iter (&rest forms)
  (let ((result (gensym)) (blk (gensym)))
    `(block ,blk
            (let (,result)
              (labels ((collect (x) (push x ,result))
                       (break-iter () (return-from ,blk)))
                      ,(build-iter forms)
                      (nreverse ,result))))))
(defun varnamep (s)
  (and (symbolp s) (not (fboundp s)) (not (keywordp s))))
(defmacro defmacro! (name args &body body)
  `(defmacro ,name ,args
     (let ,(iter (for s leaves body unique)
                 (seek (and (varnamep s)
                            (char= (elt (symbol-name s) 0) #\!)
                            (not (member s args))))
                 (collect `(,s (gensym ,(symbol-name s)))))
       ,@body)))
