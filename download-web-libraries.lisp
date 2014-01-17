(in-package :web-utils)

(defpar web-library-directory #P "/var/tmp/web-libraries/")

(defun serve-jquery (&key (version "1.10.2"))
  "download the latest version of jquery an serve it locally"
  (let ((files (mapcar (clambda (format nil x! version))
                       '("jquery-~A.min.js"
                         ;; "jquery-~A.js"
                         "jquery-~A.min.map"))))
    (ensure-directories-exist web-library-directory)
    (mapcar (lambda (file)
             (let ((remote-url (conc "http://code.jquery.com/" file))
                   (local-file (merge-pathnames file web-library-directory))
                   (local-url (conc "/scripts/" file)))
               (unless (uiop:probe-file* local-file)
                 ;; need to download this file
                 (uri->file remote-url local-file))
               (setup-static-content local-url local-file)
               local-url))
            files)))

;;; first, we want the latest bootstrap
#|(defun get-bootstrap-archive-uri ()
  (let* ((w (uri->html-document "http://getbootstrap.com/"))
         (a (remove-if-not (lambda (a)
                 (search "Download" (text-content a)
                         :test #'char-equal))
                           (css-selectors:query "a" w))))
    (dom:get-attribute (elt a 0) "href")))|#

#|(defun get-bootstrap ()
  (ensure-directories-exist web-library-path)
  (uri->file (get-bootstrap-archive-uri)
             (merge-pathnames web-library-path "bootstrap.zip")))|#


