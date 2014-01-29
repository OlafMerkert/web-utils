(defsystem :web-utils
  :depends-on (ol-utils
               cl-who
               hunchentoot
               drakma
               cxml
               closure-html
               parenscript)
  :serial t
  :components ((:file "packages")
               (:file "cl-who-helpers")
               (:file "hunchentoot-helpers")
               (:file "url-helpers")
               (:file "breadcrumbs")
               (:file "scrape-utils")
               (:file "parenscript-helpers")
               (:file "download-web-libraries")))

