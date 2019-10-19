
(defun zip (a b) ;; TODO generalize to arbitrary number sequences
  (if (and a b)
      (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(defun enumerate (seq)
  (defun helper (acc seq)
    (if seq
        (cons (list acc (car seq)) (helper (+ acc 1) (cdr seq)))))
  (helper 0 seq))

;; TODO should work on any sequence
;;
;; (slice seq hi)
;; (slice seq lo hi)
;; (slice seq lo hi st)
;;
(defun slice (seq . args)
  (let (((lo hi st) (range-xargs args)))

    (when (<= hi 0)
      (+= hi (len seq)))

    (when (< lo 0)
      (+= lo (len seq)))

    (let ((ret nil))
      (dorange (i lo hi st)
               (push (nth i seq) ret))

      (= ret (nreverse ret))

      ;; TODO rework this?
      (when (string? seq)
        (= ret (chars->string ret)))

      ret)))

(defun len (seq)
  ;; TODO reimplement this
  (defun list:len (seq)
    (if seq
        (+ 1 (list:len (cdr seq)))
        0))

  (cond ((string? seq) (string-len seq))
        ((vector? seq) (vector-length seq))
        (t (list:len seq))))

;;(defgeneric len (seq))
;;
;;(defmethod len ((string seq))
;;  (string-len seq))
;;
;;(defmethod len ((vector seq))
;;  (vector-length seq))
;;
;;(defmethod len ((cons seq))
;;  (defun list:len (seq)
;;    (if seq
;;        (+ 1 (list:len (cdr seq)))
;;        0))
;;  (list:len seq))
;;
;;(defmethod len ((nil seq)) 0)



;; todo maybe rename
(defun binary-classify (pred seq)
  (let ((a '())
        (b '()))
    (dolist (elt seq)
                                        ;(println elt (pred elt) a b)
      (if (pred elt)
          (push elt a)
        (push elt b)))
    (list (nreverse a) (nreverse b))))
