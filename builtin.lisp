
(load-once "std.lisp")

(defconstant +builtin-tag+ (gensym))

(defun builtin? (exp)
  (and (cons? exp)
       (eq +builtin-tag+ (car exp))))

(defun make-builtin (fun)
  (cons +builtin-tag+ fun))

(defun builtin-fun (exp)
  (cdr exp))
