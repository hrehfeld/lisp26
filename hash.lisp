
(load-file "std.lisp")

(defconstant +hash-default-size+ 31)
;;(defconstant +hash-tag+ 'hash)
(defconstant +hash-tag+ (gensym))

(defun hash? (exp)
  (and (cons? exp)
       (eq +hash-tag+ (car exp))
       ;;(vector? (-hash-buckets exp))
       ))

;;
;; (make-hash [fun [cmp]])
;;
(defun make-hash args
  (let ((cmp eq)
        (fun eq-hash)
        (buckets (make-vector +hash-default-size+)))
    (when args
      (= fun  (car args))
      (= args (cdr args)))

    (when args
      (= cmp  (car args))
      (= args (cdr args)))

    (list +hash-tag+ cmp fun buckets)))

(defun -hash-cmp (hash)
  (cadr hash))

(defun -hash-fun (hash)
  (caddr hash))

(defun -hash-buckets (hash)
  (cadddr hash))

(defun -hash-index (hash key)
  ;;(assert (hash? hash))
  (let ((code    ((-hash-fun hash) key))
        (buckets (-hash-buckets hash)))
    (mod code (vector-length buckets))))

(defun -hash-find-pair (hash key)
  ;;(assert (hash? hash))
  (let ((test (-hash-cmp hash)))
    (defun helper (kv-pairs)
      (and kv-pairs
           (if (test key (caar kv-pairs))
               (car kv-pairs)
               (helper (cdr kv-pairs)))))

    (let ((index   (-hash-index hash key))
          (buckets (-hash-buckets hash)))
      (helper (vector-get buckets index)))))

(defun hash-put (hash key val)
  ;;(assert (hash? hash))
  (let ((test (-hash-cmp hash)))
    (defun update (kv-pairs)
      (cond
        ((nil? kv-pairs) nil)

        ((test key (caar kv-pairs))
         (rplacd (car kv-pairs) val)
         t)

        (t (update (cdr kv-pairs)))))

    (let* ((index    (-hash-index hash key))
           (buckets  (-hash-buckets hash))
           (kv-pairs (vector-get buckets index)))

      (unless (update kv-pairs)
        (vector-set buckets index (cons (cons key val) kv-pairs))))))

(defun hash-get (hash key)
  (cdr (-hash-find-pair hash key)))

(defun hash-has (hash key)
  ;; TODO we could just return the bucket
  (if (-hash-find-pair hash key)
      t
      nil))
