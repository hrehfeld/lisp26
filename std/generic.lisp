
(defun generic:table-name (name)
  (intern (+ "*" (symbol-name name) "-dispatch*")))

(defmacro defgeneric (name args) ;; TODO we completely ignore the args!
  (let ((table (generic:table-name name))
        (args (gensym))
        (entry (gensym))
        (fun (gensym))
        (types (gensym)))

    ;; TODO using global leaks too far, should use def-outer/defun-outer
    `(progn
       (def-global ,table '()) ;; TODO should we use specials? or maybe an env: name -> dispatch table
       (defun-global ,name ,args
         (dolist (,entry ,table)
           (let ((,fun (car ,entry))
                 (,types (cdr ,entry)))
             (if (equal (map type ,args) ,types)
                 (return (apply ,fun ,args)))))
         (error "no method found for"',name ,args)))))

(defmacro defmethod (name args . body)
  (defun cadr (x) (car (cdr x))) ;; TODO should this be a dependency?
  (let ((table (generic:table-name name))
        (names (map cadr args))
        (types (map car args)))
    `(push (list (lambda ,names ,@body)
                 ,@(map (lambda (x) `',x) types))
           ,table)))
