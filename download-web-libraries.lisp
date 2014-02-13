(in-package :web-utils)

(defpar web-library-directory (merge-pathnames ".cache/web-libraries/"
                                               (user-homedir-pathname)))

(defvar loaded-web-libraries (make-hash-table))

(defmacro define-web-library (name &rest properties)
  (alist-bind (params
               files
               remote-prefix
               local-prefix) properties
    `(defmethod load-web-library ((name (eql ',name)) &key ,@params)
       (unless (gethash ',name loaded-web-libraries)
         (download-and-serve
          (mapcar (clambda (format nil x! ,@(mapcar #'unbox1 params))) (list ,@files))
          ,(car remote-prefix)
          ,(car local-prefix))
         (setf (gethash ',name loaded-web-libraries) t)))))

(defgeneric load-web-library (name &key))

(define-web-library :jquery
    (:params (version "1.10.2"))
  (:files "jquery-~A.min.js"
          "jquery-~A.js"
          "jquery-~A.min.map")
  (:remote-prefix "http://code.jquery.com/")
  (:local-prefix "/scripts/"))

(define-web-library :jquery-sticky
  (:files "sticky.js"
          "sticky.css"
          "close.png")
  (:remote-prefix "http://raw2.github.com/ThrivingKings/Sticky/master/")
  (:local-prefix "/scripts/sticky/"))
;; todo modify js to use inline image, or correct the path
;; todo automate using web libraries in sites (at least css and js parts)
;; todo dependencies between web libraries?

(defun download-and-serve (files remote-prefix local-prefix)
  (ensure-directories-exist web-library-directory)
  (mapcar (lambda (file)
            (let ((remote-url (conc remote-prefix file))
                  (local-file (merge-pathnames file web-library-directory))
                  (local-url (conc local-prefix file)))
              (unless (uiop:probe-file* local-file)
                ;; need to download this file
                (uri->file remote-url local-file))
              (setup-static-content local-url local-file)
              local-url))
          files))

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


