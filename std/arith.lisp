
;; TODO these should be proper generic functions

(defun + args
  (defun adder (args)
    (if (string? (car args))
             string-add
             fixnum:add))
  (apply (adder args) args))

;;(env-bind *env* '-   fixnum:sub);

;; TODO we need a better way to hide helper functions
(defun arith-helper (fn)
  (lambda (a b . seq)
    (let ((ret (fn a b)))
      (while seq
        (= ret (fn ret (car seq)))
        (= seq (cdr seq)))
      ret)))

(def -   (arith-helper fixnum:sub))

(def *   fixnum:mul);

;;(defgeneric * args)
;;(defmethod * ((fixnum a) (fixnum b))
;;  (fixnum:mul a b))

(def /   (arith-helper fixnum:div))

(def mod fixnum:mod)
(def rem fixnum:rem)
