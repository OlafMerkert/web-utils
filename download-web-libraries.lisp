(defpackage :download-web-libraries
  (:use :cl :ol :iterate
        :web-utils)
  (:export))

(in-package :download-web-libraries)

;;; first, we want the latest bootstrap
(defun get-bootstrap-archive-uri ()
  (let* ((w (uri->html-document "http://getbootstrap.com/"))
         (a (remove-if-not (lambda (a)
                 (search "Download" (text-content a)
                         :test #'char-equal))
                           (css-selectors:query "a" w))))
    (dom:get-attribute (elt a 0) "href")))

(defun get-bootstrap ()
  (ensure-directories-exist web-library-path)
  (uri->file (get-bootstrap-archive-uri)
             (merge-pathnames web-library-path "bootstrap.zip")))


;; (ql:quickload 'css-selectors)

