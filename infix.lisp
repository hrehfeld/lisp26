
(load-file "std.lisp")

;; E -> T {"+" T}
;; T -> F {"*" F}
;; F -> num | sym | "(" E ")"

(defun make-lexer (exps)
  (lambda (cmd)
    (cond ((eq 'peek cmd)
           (if (cons? exps)
               (car exps)
               nil))

          ((eq 'advance cmd)
           (if (cons? exps)
               (= exps (cdr exps))))

          (t (error)))))

(defun infix-helper (exp)
  (defun parse-E (lexer)
    (let ((term (parse-T lexer)))
      (if (eq '+ (lexer 'peek))
          'foo
          term)))

  (defun parse-T (lexer)
    (let ((factor (parse-F lexer)))
      factor))

  (defun parse-F (lexer)
    (let ((token (lexer 'peek)))
      (cond ((or (number? token)
                 (symbol? token))
             (lexer 'advance)
             token)
            (t (error)))))

  (cond ((number? exp)
         exp)
        ((symbol? exp)
         exp)
        ((cons? exp)
         (parse-E (make-lexer exp)))
        (t
         (error (+ "cannot parse " (str exp))))))

(let ((lexer (make-lexer '(1 * (2 + 3)))))
  (println (lexer 'peek))
  (lexer 'advance)
  (println (lexer 'peek)))

(defmacro infix exps
  (infix-helper exps))

(dolist (exp '(1
               a
               (1 + 2)
               (1 - 2)
               (1 * (2 + 3))))
  (println exp)
  (println '=>)
  (println (infix-helper exp))
  (println '==))
