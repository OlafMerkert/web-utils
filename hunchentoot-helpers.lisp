(in-package :web-utils)

(defparameter *current-web-server* nil)

(defun start-server (&optional production)
  (if *current-web-server*
      *current-web-server*
      (progn
        (unless production
          ;; provide the list of available applications
          (push (hunchentoot:create-regex-dispatcher
                 "^/$" 'show-available-applications)
                hunchentoot:*dispatch-table*)
          (push (hunchentoot:create-regex-dispatcher
                 "^$" 'show-available-applications)
                hunchentoot:*dispatch-table*))
        (hunchentoot:start
         (setf *current-web-server*
               (make-instance 'hunchentoot:easy-acceptor
                              :port (if production 80 8080)))))))

(defun stop-server ()
  (hunchentoot:stop *current-web-server*))

(defun error-code (&optional (code hunchentoot:+HTTP-NOT-FOUND+))
  (setf (hunchentoot:return-code*) code)
  (hunchentoot:abort-request-handler))

(defun setup-static-content (uri path &rest more-uri-paths)
  "Serve the file at PATH under the given URI, with hunchentoot."
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri path)
        hunchentoot:*dispatch-table*)
  (when more-uri-paths
    (apply #'setup-static-content more-uri-paths)))

(defparameter web-library-path #P"/var/tmp/web-libraries/")

(defparameter available-applications
  '(("Documentation" "/hunchentoot-doc.html")))

(defun show-available-applications ()
  (html/document (:title #1="Your local Hunchentoot instance")
    (:h1 #1#)
    (:p "Currently loaded web applications:")
    (:ul
     (dolist (app available-applications)
       (htm (:li (:a :href (second app) (esc (first app)))))))))

(defun register-web-application (name root-url)
  (pushnew (list name root-url) available-applications :test #'equal))

