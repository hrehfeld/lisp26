
(load-once "std.lisp")

(defun closure? (exp)
  (or (function? exp)
      (macro? exp)))

(defun make-closure (tag env params body)
  (cons (cons tag env) (cons params body)))

(defun closure-tag (exp)
  (caar exp))

(defun closure-env (exp)
  (cdar exp))

(defun closure-params (exp)
  (cadr exp))

(defun closure-body (exp)
  (cddr exp))

(defun tagged? (exp tag)
  (and (cons? exp)
       (cons? (car exp))
       (eq (closure-tag exp) tag)))

;;; function

(defparameter +function-tag+ (gensym))

(defun make-function (env params body)
  (make-closure +function-tag+ env params body))

(defun function? (exp)
  (tagged? exp +function-tag+))

;;; macro

(defparameter +macro-tag+ (gensym))

(defun make-macro (env params body)
  (make-closure +macro-tag+ env params body))

(defun macro? (exp)
  (tagged? exp +macro-tag+))
