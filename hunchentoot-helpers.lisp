(in-package :web-utils)

(defun start-server (&optional production)
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port (if production 80 8080))))

(defun error-code (&optional (code hunchentoot:+HTTP-NOT-FOUND+))
  (setf (hunchentoot:return-code*) code)
  (hunchentoot:abort-request-handler))

(defun setup-static-content (uri path &rest more-uri-paths)
  "Serve the file at PATH under the given URI, with hunchentoot."
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri path)
        hunchentoot:*dispatch-table*)
  (when more-uri-paths
    (apply #'setup-static-content more-uri-paths)))
