(defpackage :web-utils
  (:use :cl :ol
        :cl-who
        :hunchentoot)
  (:export
   :html-doc
   :html-part
   :define-html-part
   :start-server
   :error-code
   :setup-static-content))
