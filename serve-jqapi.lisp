(defpackage :serve-static-folder
  (:shadowing-import-from :cl-ppcre #:split)
  (:use :cl :ol :hunchentoot :cl-ppcre :iterate)
  (:export))

(in-package :serve-static-folder)

(defun create-regex-redirect-dispatcher (regex append)
  "Creates a request dispatch function which will internally add
`append' to the end of `script-name', and then go through
`*dispatch-table*' again to find a matching `handler'. Of course, only
if the `regex' matches in the first place."
  (let ((scanner (create-scanner regex)))
    (lambda (request)
      (when (scan scanner (script-name request))
        ;; add the `append' string to the script-name
        (with-slots (script-name) request
          (setf script-name (concatenate 'string script-name
                                         append)))
        ;; go through the dispatchers again, in the hope that one may trigger
        (iter (for dispatcher in *dispatch-table*)
              (thereis (funcall dispatcher request)))))))

(defun create-index-html-dispatcher (prefix)
  (create-regex-redirect-dispatcher
   `(:sequence ,prefix
               (:greedy-repetition 0 nil :everything)
               "/" :end-anchor)
   "index.html"))

(defun serve-static-folder (prefix folder)
  "Serve a folder given by pathname `folder' under `prefix', which
should not have a trailing slash."
  (push (create-folder-dispatcher-and-handler (concatenate 'string prefix "/") folder)
        *dispatch-table*)
  (push (create-index-html-dispatcher prefix)
        *dispatch-table*))

;; provide the jqapi folder
(serve-static-folder "/jqapi" #P"/home/olaf/Downloads/jqapi/")
;; and register it as a web application
(web-utils:register-web-application "jQAPI" "/jqapi/")
