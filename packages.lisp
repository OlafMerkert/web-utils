(defpackage :web-utils
  (:use :cl :ol
        :cl-who
        :hunchentoot)
  (:export
   #:start-server
   #:error-code
   #:setup-static-content
   #:uri
   #:xml/node
   #:xml/document
   #:html/node
   #:html/document
   #:xml-output-stream
   #:uri+
   #:uri->html-document
   #:uri->file
   #:text-content
   #:merge-urls
   #:web-library-path
   #:stop-server
   #:register-web-application
   #:serve-jquery
   #:breadcrumb->url
   #:register-breadcrumb-dispatcher
   #:load-web-library))
