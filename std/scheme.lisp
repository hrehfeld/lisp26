
(defmacro begin body
  `((lambda () ,@body)))

(defmacro define (var . rest)
  (if (symbol? var)
      `(def ,var ,(car rest)) ; TODO calls like (define foo blah blub) silently drop blub
      `(def ,(car var)
             (lambda ,(cdr var) ,@rest))))

(defmacro set! (var val)
  `(set ,var ,val))
