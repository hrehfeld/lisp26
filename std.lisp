
(load-file "std/boot.lisp")
(load-file "std/list.lisp")
(load-file "std/ctrl.lisp")
(load-file "std/type.lisp")
(load-file "std/generic.lisp")
(load-file "std/stream.lisp")

;;;; "utilities"

(defmacro = (var val)
  `(set ,var ,val))

(defun not (x)
  (if x nil t))

;;;; arithmetic

(load-file "std/arith.lisp")

;;;; logic

(defmacro and args
  (if (nil? args)
      '(quote t)
      (if (nil? (cdr args))
          (car args)
          (list 'if (car args)
                (cons 'and (cdr args))
                'nil))))

(defmacro and. args
  (if args
      (if (cdr args)
          `(if ,(car args)
               (and ,@(cdr args)))
          (car args))
      't))

;; TODO expand only once
(defmacro or args
  (if (nil? args)
      '(quote t)
      (if (nil? (cdr args))
          (car args)
          (list 'if (car args)
                (car args)
                (cons 'or (cdr args))))))

;; comparison

(def == equal)

(defun != (x y) ; TODO varargs
  (not (== x y)))

(defun < args
  (cond ((fixnum? (car args)) (apply fixnum:lt args))
        ((string? (car args)) (apply string:lt args))
        (t (error "cannot compare" args))))

(defun > (x y)
  (< y x))

(defun <= (x y)
  (not (> x y)))

(defun >= (x y)
  (not (< x y)))

;;; c*r

;; TODO multiple dimensions (destructuring)
(defmacro defcomp (name . funs)
  (let ((var (gensym)))
    (defun compose (funs)
      (if funs
          (list (car funs) (compose (cdr funs)))
          var))

    `(defun ,name (,var) ,(compose funs))))

(defcomp caar car car)
(defcomp cadr car cdr)
(defcomp cdar cdr car)
(defcomp cddr cdr cdr)

(defcomp caaar car car car)
(defcomp cadar car cdr car)
(defcomp caadr car car cdr)
(defcomp caddr car cdr cdr)
(defcomp cdddr cdr cdr cdr)

(defcomp cadddr car cdr cdr cdr)
(defcomp cadaar car cdr car car)

(load-file "std/scheme.lisp")

;;; assignment

(defmacro += (var val)
  `(= ,var (+ ,var ,val)))

(defmacro -= (var val)
  `(= ,var (- ,var ,val)))

(defmacro *= (var val)
  `(= ,var (* ,var ,val)))

(defmacro /= (var val)
  `(= ,var (/ ,var ,val)))

;; TODO do we want to (define *special*) first?
;;(define *special* nil)

;;(defmacro make-special (name)
;;  (list 'set! '*special* (list 'cons (list 'quote name) '*special*)))

(defmacro make-special names
  `(set! *special* (append (quote ,names) *special*)))

(defmacro defspecial (name val)
  `(progn (def-global ,name ,val)
          (make-special ,name)))

(defmacro defparameter (name val)
  `(defspecial ,name ,val))

(defmacro defconstant (name val) ; TODO this is a hack
  `(defspecial ,name ,val))

(defmacro step! (var)
  `(set! ,var (cdr ,var)))

(defmacro inc! (var)
  `(set! ,var (+ ,var 1)))

(defmacro dolist ((var seq) . body)
  (let ((itr (gensym)))
    `(let ((,itr ,seq))
       (while ,itr
         (let ((,var (car ,itr)))
           ,@body)
         (step! ,itr)))))

;; TODO handle negative step, e.g. test vlo <= var < vhi (or specialize)
(defmacro dorange ((var lo hi st) . body)
  (let ((vhi (gensym))
        (vst (gensym)))
    `(let ((,var ,lo)
           (,vhi ,hi)
           (,vst ,st))
       (while (< ,var ,vhi)
         ,@body
         (+= ,var ,vst)))))

(defmacro dotimes ((var num) . body)
  (let ((lim (gensym)))
    `(let ((,var 0)
           (,lim ,num))
       (while (< ,var ,lim)
         ,@body
         (inc! ,var)))))

(defun append (a b)
  (if (nil? a)
      b
      (cons (car a) (append (cdr a) b))))

;;(println (append '(a b c) '(d e f)))

(defun filter (proc list)
  (if (nil? list)
      nil
      (if (proc (car list))
          (cons (car list) (filter proc (cdr list)))
          (filter proc (cdr list)))))

(defun funcall (fun . args)
  (apply fun args))

;;; string utilities

(defun strip (str)
  (let ((i0 0)
        (i1 (string-len str)))
    (list i0 i1)))

(defun seq:copy (seq)
  (if seq
      (cons (car seq) (seq:copy (cdr seq)))))

(defun reverse (seq)
  (nreverse (seq:copy seq)))

(defmacro doseq args
  `(dolist ,@args))

(defun join (sep seq)
  (let ((ret "")
        (1st 't))
    (dolist (elt seq)
      (if 1st
          (= 1st nil)
          (= ret (string-add ret sep)))
      (= ret (string-add ret elt)))
    ret))

(defun cons:join:no-empty-check (sep seq)
  (let ((head (car seq))
        (tail (cdr seq)))
    (if (eq tail nil)
        (cons head nil)
      (cons head
            (cons sep
                  (cons:join sep (cdr seq)))))))

(defun cons:join (sep seq)
  (when seq (cons:join:no-empty-check sep seq)))

(defmacro error args
  `(throw nil (list '#:error ,@args)))

(defun stream-at-end (stream)
  (eq (stream-peek-char (stream) 0)))

(defun str (exp)
  (coerce exp 'string))

(defmacro inc (exp)
  `(+= ,exp 1))

(defun read args
  (if args
      (read-from-stream (car args))
      (read-from-stream *read-stream*)))

(defun nth (exp idx)
  (cond ((cons? exp) ; TODO use while and count down index?
         (if (== idx 0)
             (car exp)
             (nth (cdr exp) (- idx 1))))

        ((vector? exp)
         (vector-get exp idx))

        ((string? exp)
         (string-char-at exp idx)) ; TODO this is not bound yet

        (t
         (error "cannot get nth " idx "of" exp))))

(defun vector-fill (vec val)
  (let ((len (vector-length vec))
        (i 0))
    (while (< i len)
      (vector-set vec i val)
      (+= i 1))))

(defun vector-fill (vec val)
  (dotimes (i (vector-length vec))
    (vector-set vec i val)))

;; TODO this is O(S * P)
(defun string:index (str pat)
  (let ((S (string-len str))
        (P (string-len pat)))

    (when (== S 0)
      (when (== P 0)
        (return 0)))

    (defun match-at (index)
      (dotimes (j P)
        (when (!= (string-char-at str (+ index j))
                  (string-char-at pat j))
          (return nil)))
      t)

    (dotimes (i S)
      (when (match-at i)
        (return i)))

    -1))

(defmacro enum body
  (cons 'progn (map (lambda ((idx sym))
                      `(defconstant ,sym ,idx))
                    (enumerate body))))

(defun range-xargs (args)
  (let ((lo 0)
        (hi (car args))
        (st 1)
        (it (cdr args)))
    (when it
      (= lo hi)
      (= hi (car it))
      (= it (cdr it))
      (when it
        (= st (car it)) ;; next two are just error checking
        (= it (cdr it))
        (assert (not it))))

    (list lo hi st)))

(defun range args
  (let (((lo hi st) (range-xargs args))
        (ret nil))

    (while (< lo hi)
      (push lo ret)
      (+= lo st))
    (nreverse ret)))

(defun chars->string (seq)
  (with-output-to-string (out)
    (dolist (ch seq)
      (stream-put-char out ch))))

(defun char->string (ch)
  (chars->string (list ch)))

(defun char->string (ch)
  (with-output-to-string (out)
    (stream-put-char out ch)))

;;;; sequence utilities

(load-file "std/seq.lisp")

(defun cons:index (seq sub)
  (if (== (car seq) sub)
      t
    (let ((tail (cdr seq)))
      (when tail (cons:index tail sub)))))

(defun index (seq sub)
  (cond ((string? seq)
         (string:index seq sub))
        ((cons? seq)
         (cons:index seq sub))
        (t
         (error))))

(defun startswith (a b)
  (== (index a b) 0))

(defun endswith (a b)
  (or
   (== (len b) 0)
   (== (index a b) (- (len a) (len b)))))

(defun split (seq sep)
  (if (== (len sep) 0)
    (error "empty separator"))

  (let ((idx (index seq sep)))
    (if (== idx -1)
        (list seq)
        (cons (slice seq idx)
              (split (slice seq
                            (+ idx (len sep))
                            (len seq))
                     sep)))))

;; TODO should be part of coerce
(defun string->vector (str)
  (let* ((N (len str))
         (vec (make-vector N)))
    (dotimes (i N)
      (vector-set vec i (string-char-at str i)))
    vec))

(defmacro dolen-1 ((var seq) . body)
  (let ((lim (gensym)))
    `(let ((,lim (- (len ,seq) 1)))
       (dotimes (,var ,lim) ,@body))))

;; TODO figure out why this crashes the interpreter sometimes
;;(defmacro doadj ((a b seq) . body)
;;  (let ((i (gensym))
;;        (v (gensym)))
;;    `(let ((,v ,seq))
;;       (dolen-1 (,i ,v)
;;         (let ((,a (nth ,v ,i))
;;               (,b (nth ,v (+ ,i 1))))
;;           ,@body)))))

(defun sorted? (vec)
  ;;(dotimes (i (- (len vec) 1))
  (dolen-1 (i vec)
    ;;(println i)
    (if (> (nth vec i) (nth vec (+ i 1)))
        (return nil)))
  t)

(defun sort (vec)
  vec)

(defun max (a b)
  (if (> a b) a b))

(defun string:lt (a b)
  (let ((n (max (len a) (len b))))
    (defun aref (v i)
      (if (< i n)
          (string-char-at v i)
          0))

    (dotimes (i n)
      (let ((x (aref a i))
            (y (aref b i)))
        (if (< x y)
            (return t))))
    nil))

;; ;)
(defmacro printf (fmt . args)
  `(format t ,fmt ,@args))
