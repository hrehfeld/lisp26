
(defmacro with-output-to-string ((name) . body)
  (let ((ret (gensym)))
    `(let ((,name (make-string-output-stream)))
       ,@body
       (let ((,ret (stream-to-string ,name)))
         (stream-close ,name)
         ,ret))))
