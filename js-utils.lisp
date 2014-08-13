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
      nil)

    (defun member (obj array)
      (block member
        (dolist (item array)
          (when (equal obj item)
            (return-from member t)))
        f))

    (defun get-text (c)
      (@@ ($ c) (text)))

    (defun filter (pred list)
      (let ((result (array)))
        (dolist (item list)
          (if (pred item)
              (@@ result (push item))))
        result))

    (defun remove-empty (list)
      (filter (lambda (elt) (< 0 (length elt))) list))
    ))

;;; helper functions for bootstrap
(define-easy-handler (js-bootstrap-utils :uri "/scripts/utils-bootstrap.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (bind-event document ready ()
      (let ((navbar-items ($ ".navbar .navbar-nav li")))
        (bind-event navbar-items click ()
          ;; remove the active mark
          (@@ navbar-items (remove-class "active"))
          (@@ ($ this) (add-class "active")))))))
