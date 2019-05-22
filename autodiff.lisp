
(load-file "std.lisp")

(defun op? (exp name)
  (and (cons? exp)
       (eq (car exp) name)))

(defun simplify (exp)
  (let ((ret (simplify2 exp)))
    (println exp '=> ret)
    ret))

(defun simplify (exp)
  (cond ((op? exp '+)
         (let ((a (simplify (cadr exp)))
               (b (simplify (caddr exp))))
           (cond ((and (number? a)
                       (number? b))
                  (+ a b))

                 ((eq a 0) b)

                 ((eq b 0) a)

                 ((eq a b)
                  `(* 2 ,a))

                 (t `(+ ,a ,b)))))

        ((op? exp '*)
         (let ((args (map simplify (cdr exp))))
           (let ((a (car args))
                 (b (cadr args)))
             (cond ((and (number? a) (number? b))
                    (* a b))

                   ((or (eq a 0) (eq b 0)) 0)

                   ((eq a 1) b)

                   ((eq b 1) a)

                   (t
                   `(* ,a ,b))))))

        (t
         exp)))

(defun diff (exp var)
  (cond ((number? exp)
         0)

        ((symbol? exp)
         (if (eq exp var)
             1
             0))

        ((op? exp '+)
         (let ((a (cadr exp))
               (b (caddr exp)))
           ;; TODO multiple arguments, e.g. sum
           `(+ ,(diff a var) ,(diff b var))))

        ((op? exp '*)
         (let ((a (cadr exp))
               (b (caddr exp)))
           ;; TODO multiple arguments, e.g. sum
           `(+ (* ,(diff a var) ,b)
               (* ,a ,(diff b var)))))

        ((op? exp 'sin)
         (cons 'cos (cdr exp)))

        ((op? exp 'cos)
         (list '- (cons 'sin (cdr exp))))

        (t
         '#:error)))

(defun dif (exp var)
  (simplify (diff exp var)))

(defun difun (fun)
  (let ((args (cadr fun))
        (body (cddr fun)))
    ;; TODO only works for single statement
    (let ((exp (car body)))
      (if (cdr args)
          (map (lambda (var) (dif exp var)) args) ;; multivariate -> gradient
          (dif exp (car args))))))

(defun test-dif ()
  (dolist (example '((0 x)
                     (x x)
                     (y x)
                     ((+ x 0) x)
                     ((* x x) x)))
    (let ((exp (car example))
          (var (cadr example)))
      (println `(dif ,exp ,var) '=> (dif exp var)))))

(defun test-difun ()
  (dolist (fun '((lambda (x) 0)
                 (lambda (x) 1)
                 (lambda (x) x)
                 (lambda (x) y)
                 (lambda (x) (+ x x))

                 (lambda (x) (sin x))
                 (lambda (x) (cos x))

                 (lambda (x y) (+ x y))
                 (lambda (x y) (* x y))))
    (println `(difun ,fun) '=> (difun fun))))

(defun parse-expr (stream)
  (

(test-dif)
(test-difun)
