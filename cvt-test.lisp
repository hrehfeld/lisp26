
;; run with ./lisp cvt-test.lisp > hauke.test

(load-file "std.lisp")

(defun named-op? (exp name)
  (and (cons? exp) (eq (car exp) name)))

(defparameter ifn nil) ; "/Users/thor/Documents/Cloned/hrehfeld-lisp-py/tests.lisp"

(dolist (arg *argv*)
  (= ifn arg))

(unless ifn
  (error "missing input file name"))

(let ((tests (read-file ifn))
      (nout 0))
  (dolist (test tests)
    ;;(println test)
    (let (((src exp) test))
      ;; strip pointless progn
      (when (and (cons? src)
                 (eq (car src) 'progn)
                 (not (cddr src)))
        (= src (cadr src)))
      (when (named-op? exp 'quote)
        (= exp (cadr exp)))
      (println (list src `=> exp)))))
