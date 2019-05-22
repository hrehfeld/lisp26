
(load-file "std.lisp")

(defun line-tree-to-stream (tree out indent)
  (defun emit (str)
    (stream-put-string out str))

  (defun helper (tree prefix)
    (dolist (item tree)
      (cond ((string? item)
             (emit prefix)
             (emit item)
             (emit "\n"))

            (t
             (helper item (+ indent prefix))))))

  (helper tree ""))

(defun emit-tree (tree)
  (line-tree-to-stream tree *print-stream* "    "))

(defun named-op? (exp name)
  (and (cons? exp) (eq (car exp) name)))

(defun any-named-op? (exp . names)
  (dolist (name names)
    (if (named-op? exp name)
        (return t))))

(defun defun? (exp)
  (named-op? exp 'defun))

(defun if? (exp)
  (named-op? exp 'if))

(defun return? (exp)
  (named-op? exp 'return))

(defun when? (exp)
  (named-op? exp 'when))

(defun coerce? (exp)
  (named-op? exp 'coerce))

(defun declare? (exp)
  (named-op? exp 'declare))
