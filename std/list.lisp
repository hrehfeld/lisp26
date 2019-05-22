
(defun list args args)

(defmacro push (item place)
  `(set ,place (cons ,item ,place)))

(defun foldl (fun val seq)
  (if seq
      (foldl fun (fun val (car seq)) (cdr seq))
      val))

;; TODO test this
;;(defun foldl (fun val seq)
;;  (while seq
;;    (= val (fun val (car seq)))
;;    (= seq (cdr seq)))
;;  val)

(defun foldr (fun val seq)
  (if seq
      (fun (car seq) (foldr fun val (cdr seq)))
      val))

(defun map (fun seq)
  (if seq
      (cons (fun (car seq))
            (map fun (cdr seq)))))
