(in-package :web-utils)

(define-easy-handler (js-utils :uri "/scripts/utils.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (defun length=0 (string)
      (= 0 (length string)))

    (defun hide+remove (node)
      (@@ ($ node) (hide "normal" (lambda () (@@ ($ this) (remove))))))

    (defun user-message% (message)
      (@@ console (log message))
      (@@ $ (sticky message))
      nil)))
