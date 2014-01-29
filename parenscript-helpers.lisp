(in-package :web-utils)

(defmacro/ps @@ (&rest args)
    `(chain ,@args))

(defmacro/ps $! (obj handler args &body body)
  "Install an event handler using jQuery on the solected object."
  `(@@ ($ ,obj) (,handler (lambda ,args ,@body))))

(defun lisp->camelcase (symbol &optional classp)
  (let ((capit (string-capitalize (mkstr symbol))))
    (remove #\- (if classp capit (string-downcase capit :start 0 :end 1)))))

(defmacro/ps form-value (id &optional (value nil value-p))
  `(@@ ($ ,(mkstr "#" (lisp->camelcase id))) (val ,@(if value-p (list value)))))

(defmacro/ps form-bind (bindings &body body)
  `(let ,(mapcar #`(,a1 (form-value ,a1)) bindings)
     ,@body))
