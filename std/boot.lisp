
(core:env-def *env* 'def
          (syntax (var val)
                  `(core:env-def *env* ',var ,val)))

(core:env-def *env* 'set
          (syntax (var val)
                  `(core:env-set *env* ',var ,val)))

(core:env-def *env* 'del
          (syntax (var)
                  `(core:env-del *env* ',var)))

(def t 't)

(def defmacro
    (syntax (name args . body)
            `(def ,name
                 (syntax ,args ,@body))))

;; add a label to definitions
(def defmacro
    (syntax (name args . body)
            `(def ,name
                  (label ,name (syntax ,args ,@body)))))

;; TODO analyze defun body for returns and maybe skip block

;; 1. plain lambda
(defmacro defun (name args . body)
  `(def ,name (lambda ,args ,@body)))

;; 2. body wrapped in block
(defmacro defun (name args . body)
  `(def ,name (lambda ,args (block nil ,@body))))

;; 3. with block and label
(defmacro defun (name args . body)
  `(def ,name (label ,name (lambda ,args (block nil ,@body)))))

(defun std:env-outermost (env)
  (if env
      (if (env-outer env)
          (std:env-outermost (env-outer env))
          env)
      nil))

(defmacro def-global (var val)
  `(core:env-def (std:env-outermost *env*) ',var ,val))

(defmacro defun-global (name args . body)
  `(def-global ,name (lambda ,args (block nil ,@body))))
