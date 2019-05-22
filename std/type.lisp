
;; somewhat meta/unstable
(defun type (x)
  (let ((t (meta:expr-type x)))
    (cond
      ((eq t  0) 'nil)
      ((eq t  1) 'gensym)
      ((eq t  2) 'fixnum)
      ((eq t  3) 'symbol)
      ((eq t  4) 'cons)
      ((eq t  5) 'string)
      ((eq t  9) 'vector)
      ((eq t 10) 'builtin)
      ((eq t 11) 'stream)
      (t
       (error "type not implemented for " x t)))))

(defun nil? (x)
  (eq 'nil x))

(defun symbol? (x)
  (eq (type x) 'symbol))

(defun cons? (x)
  (eq (type x) 'cons))

(defun macro? (x)
  (eq (type x) 'macro))

(defun fixnum? (x)
  (eq (type x) 'fixnum))

(defun string? (x)
  (eq (type x) 'string))

(defun gensym? (x)
  (eq (type x) 'gensym))

(defun vector? (x)
  (eq (type x) 'vector))

(defun function? (x)
  (eq (type x) 'function))

(defun number? (exp)
  (fixnum? exp))

(defun list? (exp)
  (or (nil? exp)
      (cons? exp)))
