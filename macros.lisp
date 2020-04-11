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

(defmacro aif (exp true-branch false-branch)
  `(let ((it ,exp))
     (if it
         ,true-branch
         ,false-branch)))

(defmacro sif (exp true-branch false-branch)
  `(symbol-macrolet ((it ,exp))
     (if it
         ,true-branch
         ,false-branch)))

(defun sort-by-number (a b) 
  (< (parse-integer (subseq (symbol-name a) 1))
     (parse-integer (subseq (symbol-name b) 1))))

(defun find-args (exp)
  (labels ((rec (e acc)
             (cond ((and (symbolp e) (char= (aref (symbol-name e) 0) #\%))
                    (adjoin e acc)) 
                   ((consp e)
                    (let ((args (rec (car e) acc)))
                      (rec (cdr e) args)))
                   (t (if (> (length acc) 1)
                          (sort acc #'sort-by-number)
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

(defun |#@-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((decorator (read stream))
        (fdef (read stream)))
    `(progn ,fdef (funcall #',decorator ',(cadr fdef)))))

(defun memoize (fun)
  (let ((old-fun (symbol-function fun))
        (table (make-hash-table :test 'equal)))
    (setf (symbol-function fun) 
          (lambda (&rest args)
            (multiple-value-bind (v found) (gethash args table)
              (if found
                  v
                  (let ((res (apply old-fun args)))
                    (setf (gethash args table) res)
                    res)))))))

(set-dispatch-macro-character #\# #\@ #'|#@-reader|)

(defun numeral-value (numeral)
  (case numeral
    (#\I 1)
    (#\V 5)
    (#\X 10)
    (#\L 50)
    (#\C 100)
    (#\D 500)
    (#\M 1000)
    (otherwise nil)))

(defun parse-numeral (str)
  (let ((numerals (map 'list #'identity str)))
    (labels ((rec (rest total)
               (cond ((null rest) total)
                     ((null (cdr rest))
                      (+ total (numeral-value (car rest))))
                     ((< (numeral-value (car rest)) 
                         (numeral-value (cadr rest)))
                      (rec (cdr rest) (- (+ total (numeral-value (car rest))))))
                     ((> (numeral-value (car rest))
                         (numeral-value (cadr rest)))
                      (+ total (numeral-value (car rest))
                         (rec (cdr rest) 0)))
                     (t (rec (cdr rest) 
                             (+ (numeral-value (car rest)) total))))))
      (rec numerals 0))))

(defun |#r-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((numeral (map 'string #'identity 
                      (loop for c = (peek-char nil stream nil)
                         while (and c (numeral-value c))
                         collect (read-char stream)))))
    (parse-numeral numeral)))

(set-dispatch-macro-character #\# #\r #'|#r-reader|)
