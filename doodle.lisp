;(node ntype (:p1 v1 :p2 v2) left right)
;macro: (barnode ntype (:p1 v1 :p2 v2) spec mod head bar)
;(morph ntype (:p1 v1 :p2 v2) root)
;(var name :p1 v1 :p2 v2)
;(collect/distribute ntype var1 var2 var3)

; TODO
; * linear & surface rules
; * transduce
; * where to store translation rules?
; ** retrieve relevant rule set
; * generation rules
; * gen/make/parse
; * interface

(defvar *lang* nil)
(defvar *srclang* nil)
(defvar *destlang* nil)

;;;;;;;;;;
; UTILITIES
;;;;;;;;;;
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
(defvar *log-indent* 0)
(defmacro defun-log (name args &body body)
  (let-gen
   (res fn)
   `(defun ,name ,args
      (let ((*log-indent* (1+ *log-indent*)))
        (format t "~vtEntering function ~a with arguments ~a~%"
                *log-indent* ',name (list ,@args))
        (let ((,res (progn ,@body)))
          (format t "~vtReturning ~a from function ~a~%"
                  *log-indent* ,res ',name)
          ,res)))))
(defun to-keyword (s)
  (intern (symbol-name s) 'keyword))

;;;;;;;;;;
; ELEMENTS
;;;;;;;;;;
(defvar *location* :form)
(defmacro node (ntype args &rest children)
  `(list 'node ',ntype ,args ,@children))
(defmacro morph (ntype args root)
  (let-gen
   (res exists2 pos exists1 table)
   (let ((err (format nil "Morpheme ~a does not exist in language ~a with part of speech ~a." root *lang* ntype)))
     `(list 'morph ',ntype
            (let ((,table (get *lang* 'lexicon)))
              (if ,table
                  (multiple-value-bind
                   (,pos ,exists1) (gethash ',ntype ,table)
                   (if ,exists1
                       (multiple-value-bind
                        (,res ,exists2) (gethash ',root ,pos)
                        (if ,exists2
                            (append (list ,@args :lang ',*lang*) ,res)
                          (error ,err)))
                     (error ,err)))
                (error (format nil "Language ~a has no lexicon." *lang*))))
            ',root))))
; it might be worth experimenting to see if it would be faster to have
; getvars do lookups instead of copying the args list into each instance
(defun register-morph (pos root &rest args)
  (let ((lex (get *lang* 'lexicon)))
    (unless lex
      (setf (get *lang* 'lexicon) (make-hash-table :test #'eq)
            lex (get *lang* 'lexicon)))
    (multiple-value-bind
     (posdct p?) (gethash pos lex)
     (unless p?
       (setf (gethash pos lex) (make-hash-table :test #'eq)
             posdct (gethash pos lex)))
     (multiple-value-bind
      (rootdct r?) (gethash root posdct)
      (if r?
          (error (format nil "Attempting to re-register root ~a with part of speech ~a in langauge ~a." root pos *lang*))
        (setf (gethash root posdct) args))))))
(defun register-many-morphs (pos args roots)
  (mapcar (lambda (r)
            (apply #'register-morph
                   (if (listp r)
                       `(,pos ,(car r) :display ,(cadr r) ,@args)
                     `(,pos ,r :display ,(string-downcase (symbol-name r))
                            ,@args))))
          roots))
(defmacro var (name &rest reqs)
  `(if (eq *location* :form)
       (list 'var ,(to-keyword name) ,@reqs)
     ,name))
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
(defmacro barnode-var (ntype prefix &key spec mod head bar)
  `(barnode-key ,ntype ()
                :spec ,(or spec `(var ,(symb prefix 'spec) :opt t))
                :mod ,(or mod `(var ,(symb prefix 'mod) :opt t))
                :head ,(or head `(var ,(symb prefix 'head) :opt t))
                :bar ,(or bar `(var ,(symb prefix 'bar) :opt t))))
(defmacro collect (ntype &rest vars)
  `(list 'collect ',ntype ,@vars))
(defmacro distribute (ntype &rest vars)
  `(append '(node ,ntype ())
           (mapcan [if (and (listp _) (listp (car _))) _ (list _)]
                   (list ,@vars))))
(defun getroots (tree)
  (when (listp tree)
    (if (eq (car tree) 'node)
        (mapcan #'getroots (cdddr tree))
      (when (eq (car tree) 'morph)
        (cdddr tree)))))

;;;;;;;;;;
; RULES
;;;;;;;;;;
(defun lambdify-result (tree)
  (let ((varls (append (remove-if [or (not (symbolp _))
                                      (fboundp _)
                                      (keywordp _)]
                                  (remove-duplicates (flatten tree)))
                       (loop for (a b c) on (flatten tree) appending
                             (mapcar [symb c _] '(spec mod head bar))))))
    (labels ((disvar (tr)
                     (if (listp tr)
                         (if (eq (car tr) 'var)
                             (cadr tr)
                           (if (eq (car tr) 'collect)
                               (cons 'distribute (mapcar #'disvar (cdr tr)))
                             (mapcar #'disvar tr)))
                       tr)))
            `(lambda (&key ,@varls &allow-other-keys)
               (let ((*location* :result))
                 ,(disvar tree))))))
(defmacro basic-rule (form result &key src dest)
  (let ((sen (gensym)) (vars (gensym)) (match (gensym)))
    `(lambda (,sen)
       (let ((*lang* ',(or src *srclang* *lang*)))
         (multiple-value-bind
          (,vars ,match) (getvars ,sen ,form)
          (when ,match
            (let ((*lang* ',(or dest *destlang* *lang*)))
              (apply ,(lambdify-result result) ,vars))))))))
(defmacro context-rule (context form result &key src dest)
  (let-gen
   (sen cvars fvars vars insert)
   `(lambda (,sen)
      (let ((*lang* ',(or src *srclang* *lang*)))
        (mvb-if
         ,cvars (getvars ,sen ,context)
         (mvb-if
          ,fvars (getvars (getf ,cvars :@) ,form)
          (let* ((*lang* ',(or dest *destlang* *lang*))
                 (,vars (append ,fvars ,cvars))
                 (,insert (apply ,(lambdify-result result) ,vars)))
            (apply ,(lambdify-result context) ,vars))))))))
(defmacro multi-rule (&rest layers)
  (let-gen
   (blk fn sen s all-rules rules f r vars all-vars)
   `(lambda (,sen)
      (let ((,all-rules
             ,(cons 'list (mapcar
                           [cons 'list
                                 (mapcar [list 'list (car _2)
                                               (lambdify-result
                                                (or (cadr _2) (car _2)))]
                                         _)]
                           layers)))
            ,all-vars)
        (block ,blk
               (labels
                ((,fn (,s ,rules)
                      (if ,rules
                          (loop for (,f ,r) in (car ,rules)
                                do (mvb-if
                                    ,vars (getvars ,s ,f)
                                    (progn
                                      (setf ,all-vars (append ,vars ,all-vars))
                                      (setf (getf ,all-vars :@)
                                            (,fn (getf ,all-vars :@)
                                                 (cdr ,rules)))
                                      (return-from ,fn (apply ,r ,all-vars))))
                                finally (return-from ,blk nil))
                        ,s)))
                (,fn ,sen ,all-rules)))))))
(defun setprop (node prop val)
  (setf (getf (caddr node) (to-keyword prop)) val)
  node)
; it's possible that this will have unintended side-effects
; if so, add a copy operation
(defun copyprop (src sprop dest &optional dprop)
  (setf (getf (caddr dest) (to-keyword (or sprop dprop)))
        (getf (caddr src) (to-keyword sprop)))
  dest)

;;;;;;;;;;
; RULE APPLICATION
;;;;;;;;;;
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
         ((and (keywordp (car tr)) (not pt))) ; empty list matching arguments
         ((and (keywordp (car tr)) (keywordp (car pt))) ; arglists
          (loop for (prop val) on pt by #'cddr
                appending (if (member prop tr)
                              (gv (getf tr prop) val)
                            (fail))))
         ; there's probably a better (more efficient) way to do this
         ; and we may eventually need to check globals for morphemes
         ; (though see the definition of morph above for further discussion)
         ((and (eq (car tr) (car pt)) (= (length tr) (length pt)))
          (mapcan #'gv (cdr tr) (cdr pt)))
         ((and (eq (car pt) 'collect) (eq (car tr) 'node)
               (eq (cadr pt) (cadr tr)))
          (let ((track (gensym)) (mode (gensym)) (value (gensym)) vars)
            (mapcar (lambda (var)
                      (when (eq (car var) 'var)
                        (push (cadr var) vars)
                        (setf (get (cadr var) mode)
                              (cond
                               ((getf (cddr var) :opt) :opt)
                               ((getf (cddr var) :multi) :multi)
                               (t :normal)))))
                    (cddr pt))
            (loop for ch in (cdddr tr) appending
                  (loop for var in (cddr pt) do
                        (if (eq (car var) 'var)
                            (when (or (not (get (cadr var) track))
                                      (eq (get (cadr var) mode) :multi))
                              (mvb-if v (getvars ch var)
                                      (progn
                                        (setf (get (cadr var) track) t)
                                        (when (eq (get (cadr var) mode) :multi)
                                            (push ch (get (cadr var) value))
                                            (remf v (cadr var)))
                                        (return v))))
                          (mvb-if v (getvars ch var) (return v))))
                  into ret finally
                  (progn
                    (dolist (var vars)
                      (case (get var mode)
                            (:normal (unless (get var track) (fail)))
                            (:opt (unless (get var track)
                                    (push ret nil)
                                    (push ret var)))
                            (:multi (progn
                                      (push ret (get var value))
                                      (push ret var)))))
                    (return ret)))))
         (t (fail)))))
   (values (gv tree pattern) t)))

;;;;;;;;;;
; TRANSLATION
;;;;;;;;;;
(defun trans-symb (src dest)
  (symb src '-to- dest))
(defun trans-get (src dest prop)
  (get (sym src '-to- dest) prop))

;;;;;;;;;;
; DATA INPUT
;;;;;;;;;;
(defun language-syntax (language &rest stages)
  (setf (get language 'syntax) stages))
(defun language-linear (language &rest stages)
  (setf (get language 'linear) stages))
(defun language-surface (language &rest stages)
  (setf (get language 'surface) stages))
