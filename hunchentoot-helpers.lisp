(in-package :web-utils)

(defparameter *current-web-server* nil)

(defun start-server (&optional production)
  (unless *current-web-server*
   (hunchentoot:start
    (setf *current-web-server*
          (make-instance 'hunchentoot:easy-acceptor
                         :port (if production 80 8080))))))

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
