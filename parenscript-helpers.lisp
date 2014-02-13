(in-package :web-utils)

(defmacro+ps @@ (&rest args)
    `(chain ,@args))

(defmacro+ps bind-event (obj handler args &body body)
  "Install an event handler using jQuery on the solected object."
  `(@@ ($ ,obj) (,handler (lambda ,args ,@body))))

(defun lisp->camelcase (symbol &optional classp)
  (let ((capit (string-capitalize (mkstr symbol))))
    (remove #\- (if classp capit (string-downcase capit :start 0 :end 1)))))

(defmacro+ps form-value (id &optional (value nil value-p))
  `(@@ ($ ,(mkstr "#" (lisp->camelcase id))) (val ,@(if value-p (list value)))))

(defmacro+ps form-bind (bindings &body body)
  `(let ,(mapcar #`(,a1 (form-value ,a1)) bindings)
     ,@body))

(defmacro+ps cc (symbol-or-string)
  (cl-json:lisp-to-camel-case (mkstr symbol-or-string)))

(defmacro+ps cch (symbol-or-string)
  (concatenate 'string "#" (cl-json:lisp-to-camel-case (mkstr symbol-or-string))))

(defpsmacro user-message (&rest messages)
  `(user-message% (concatenate 'string ,@messages)))

;; setting up webapp keybindings
(defgeneric js-key-code (keybinding))

(defmacro+ps bind-keys (node &rest bindings)
  "Every binding ought to have the form (k ..code..)"
  `(bind-event ,node keydown (event)
     (case (@ event which)
       ,@(mapcar (lambda (b)
                   `(,(js-key-code (first b))
                      ,@(rest b)))
                 bindings))))

(defmethod js-key-code ((char character))
  (+ (char-code char) (- 80 112)))

(defmethod js-key-code ((string string))
  (js-key-code (char string 0)))

(defmethod js-key-code ((symbol symbol))
  (js-key-code (char (string-downcase (symbol-name symbol)) 0)))
