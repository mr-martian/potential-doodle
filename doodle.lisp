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
(load "util.lisp")

;;;;;;;;;;
; ELEMENTS
;;;;;;;;;;
(defvar *location* :form)
(defmacro node (ntype args &rest children)
  `(list 'node ',ntype ,args ,@children))
(defmacro morph (ntype args root)
  (let-gen
   (res pos table)
   (let ((err (format nil "Morpheme ~a does not exist in language ~a with part of speech ~a." root *lang* ntype)))
     `(list 'morph ',ntype
            (let-if ,table (get *lang* 'lexicon)
                    (mvb-if ,pos (gethash ',ntype ,table)
                            (mvb-if ,res (gethash ',root ,pos)
                                    (append (list ,@args :lang ',*lang*) ,res)
                                    (error ,err))
                            (error ,err))
                    (error (format nil "Language ~a has no lexicon." *lang*)))
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
     (mvb-if rootdct (gethash root posdct)
             (error (format nil "Attempting to re-register root ~a with part of speech ~a in langauge ~a." root pos *lang*))
             (setf (gethash root posdct) args)))))
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
;(defmacro collect (ntype &rest vars)
;  `(list 'collect ',ntype ,@vars))
(defmacro collect (ntype &rest vars)
  `(if (eq *location* :form)
       (list 'collect ',ntype ,@vars)
     (distribute ,ntype ,@vars)))
(defmacro distribute (ntype &rest vars)
  `(append '(node ,ntype ())
           (mapcan [if (and (listp _) (listp (car _))) _ (list _)]
                   (list ,@vars))))
(defun getroots (tree)
  (when (listp tree)
    (if (eq (car tree) 'node)
        (mapcan #'getroots (cdddr tree))
      (when (eq (car tree) 'morph)
        (list tree)))))
(defun is-a (el type)
  (and (listp el) (eq (car el) type)))

;;;;;;;;;;
; RULES
;;;;;;;;;;
(defun lambdify-result (tree)
  (let ((varls (iter ((for s leaves tree unique)
                      (seek (and (symbolp s) (not (fboundp s))
                                 (not (keywordp s))))
                      (collect s))
                     (for br branches tree)
                     (seek (eq (car br) 'barnode-var))
                     (for add in '(spec mod head bar))
                     (collect (symb (caddr br) add)))))
    `(lambda (&key ,@varls &allow-other-keys)
       (let ((*location* :result))
         ,tree))))
(defmacro basic-rule (form result &key src dest)
  (let ((sen (gensym)) (vars (gensym)))
    `(lambda (,sen)
       (let ((*lang* ',(or src *srclang* *lang*)))
         (mvb-if ,vars (getvars ,sen ,form)
                 (let ((*lang* ',(or dest *destlang* *lang*)))
                   (apply ,(lambdify-result result) ,vars)))))))
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
             ,(iter (collect 'list)
                    (for ly in layers)
                    (nest 'list)
                    (for (f &optional r) in ly)
                    (collect `(list ,f ,(lambdify-result (or r f))))))
            ,all-vars)
        (block ,blk
               (labels
                ((,fn (,s ,rules)
                      (if ,rules
                          (iter (for (,f ,r) in (car ,rules))
                                (seek (getvars ,s ,f) as ,vars multi
                                      fail (return-from ,blk nil))
                                (setf-apply ,all-vars [append ,vars _]
                                            (getf ,all-vars :@)
                                            [,fn _ (cdr ,rules)])
                                (return-from ,fn (apply ,r ,all-vars)))
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
           (let ((sens (if (is-a in-sen 'node)
                           (mapcar [append (subseq in-sen 0 3) _]
                                   (product (mapcar [stage _ rules]
                                                    (cdddr in-sen))))
                         (list in-sen))))
             (iter (for s in sens)
                   (for rule in rules)
                   (seek (funcall rule s) as res fail (collect s))
                   (collect res)))))
   (reduce (lambda (s r)
             (mapcan [stage _ r] s))
           (cons (list sen) ruleset))))
(defun getvars (tree pattern)
  (labels
   ((fail ()
          (return-from getvars (values nil nil)))
    (gv (tr pt)
        (cond
         ((is-a pt 'var) ; variable
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
(defun linearize (tree)
  (if (is-a tree 'node)
      (mapcan #'linearize (cdddr tree))
    (if (is-a tree 'morph)
        (list tree))))
(defun apply-linear (ruleset sen)
  (list sen))
(defun apply-surface (ruleset sen)
  (list sen))
(defun find-rules (sen)
  (let ((rules (list :syntax (get *lang* 'syntax)
                     :linear (get *lang* 'linear)
                     :surface (get *lang* 'surface))))
    (iter (for word in (getroots sen))
          (for key in '(:syntax :linear :surface))
          (for (layer rule) in (getf word key))
          (when (>= layer (length (getf rules key)))
            (setf-apply (getf rules key)
                        [append _ (make-list (- (length _) layer -1))]))
          (push rule (nth layer (getf rules key))))
    rules))
(defun process (sen)
  (let ((rules (find-rules sen)))
    (iter (for tree in (apply-rules (second rules) sen))
          (for line in (apply-linear (fourth rules) tree))
          (for string in (apply-surface (sixth rules) line))
          (collect string))))

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
