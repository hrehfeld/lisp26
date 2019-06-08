
(defun merge (a b)
  (cond ((nil? a) b)
        ((nil? b) a)
        (t (let ((x (car a))
                 (y (car b)))
             (if (<= x y)
                 (cons x (merge (cdr a) b))
                 (cons y (merge a (cdr b))))))))

(defun split (seq)
  (let ((head nil)
        (slow seq)
        (fast seq))
    (while fast
      (push (car slow) head)
      (= slow (cdr slow))
      (= fast (cdr (cdr fast))))
    (= head (nreverse head))
    (list head slow)))

;; non-destructive
(defun mergesort (seq)
  (if (and seq (cdr seq))
      (let (((a b) (split seq)))
        (merge (mergesort a) (mergesort b)))
      seq))

(defun sorted (seq)
  (mergesort seq))
