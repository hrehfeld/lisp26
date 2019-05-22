
(defun foo ()
  (print 1))

(defun bar ()
  (print 2)
  (print 3))

(defmacro unless (test . body)
  `(if (not ,test) ,@body))

(defun omfg ()
  (unless happy
    (show_it)))

(when nil)

(defun empty ())

;;(defun disgusted? ()
;;  (println 'why?)) ; TODO we need proper name mangling

(defun fib (n)
  (if (< n 2)
      (return n) ;; TODO do return analysis
      (return (+ (fib (- n 2)) (fib (- n 1))))))

(when (== __name__ "__main__")
  (print "Hello from lisp"))
