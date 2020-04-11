(eval-when (:compile-toplevel :load-toplevel)
  (load "macros.lisp"))

(defun reverse-minus (l)
  (reduce #f(- %2 %1) l))

#@memoize
(defun fib (n) 
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

#@memoize
(defun change (coins total)
  (cond ((< total 0) 0)
        ((= total 0) 1)
        ((null coins) 0)
        (t (+ (change coins (- total (car coins)))
              (change (cdr coins) total)))))

(defparameter *numerals* '(#rXXIV #rMCMXL))
