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

(defun find-args (exp)
  (labels ((rec (e acc)
               (cond ((and (symbolp e) (char= (aref (symbol-name e) 0) #\%))
                      (adjoin e acc)) 
                     ((consp e)
                      (let ((args (rec (car e) acc)))
                        (rec (cdr e) args)))
                     (t (if (> (length acc) 1)
                            (handler-case (sort acc (lambda (a b) (< (parse-integer 
                                                         (subseq (symbol-name a) 1))
                                                        (parse-integer
                                                         (subseq (symbol-name b) 1)))))
                              (condition () 
                                (error "Argument names must be percentage followed by a number")))
                            acc)))))
      (rec exp nil)))

(defmacro expand-lambda (exp)
  `(let ((args (find-args ,exp)))
    `(lambda ,args ,exp)))

(defun |#f-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((exp (read stream)))
    (expand-lambda exp)))

(set-dispatch-macro-character #\# #\f #'|#f-reader|)
