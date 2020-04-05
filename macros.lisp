(defmacro ->> (&rest exps)
  (reduce (lambda (a b) (append b (list a))) exps))

(defmacro -> (&rest exps)
  (reduce (lambda (a b) (cons (car b) (cons a (cdr b)))) exps))

(defmacro with-gensyms (syms &body exps)
  `(let ,(mapcar (lambda (sym) `(,sym (gensym))) syms)
     ,@exps))

(defun bind-subst (in-exp subst-exp)
  (with-gensyms (res)
    `(let ((,res ,in-exp))
       ,(subst res '? subst-exp))))

(defmacro -?> (&rest exps)
  (reduce #'bind-subst exps))
