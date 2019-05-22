
(load-file "std.lisp")

;; TODO multiple args
;; TODO balancing
(defun make-rope strings
  (let ((s (car strings)))
    (list 'leaf (len s) s)))

(defun rope:leaf? (rope)
  (eq (car rope) 'leaf))

(defun rope:node? (rope)
  (eq (car rope) 'node))

(defun rope:len (rope)
  (cadr rope))

(defun rope:str (rope)
  (if (rope:leaf? rope)
      (caddr rope)))

(defun rope:left (rope)
  (if (rope:node? rope)
      (caddr rope)))

(defun rope:right (rope)
  (if (rope:node? rope)
      (cadddr rope)))

(defun rope->string (rope)
  (let ((out (make-string-output-stream)))

    (defun << (out val)
      (cond ((string? val) (stream-put-string out val))
            (t (error))))

    (defun helper (rope)
      (if (rope:leaf? rope)
          (<< out (rope:str rope))
          (progn
            (helper (rope:left rope))
            (helper (rope:right  rope)))))

    (helper rope)
    (let ((ret (stream-to-string out)))
      (stream-close out)
      ret)))

(defun rope:+ (a b)
  (let ((la (rope:len a))
        (lb (rope:len b)))
    (list 'node (+ la lb) a b)))

(defun rope:char-at (r i)
  (if (rope:leaf? r)
      (string-char-at (rope:str r) i)
      (let* ((l (rope:left r))
             (n (rope:len l)))
        (if (< i n)
            (rope:char-at l i)
            (rope:char-at (rope:right r) (- i n))))))
