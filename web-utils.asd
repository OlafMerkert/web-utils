(defsystem :web-utils
  :depends-on (ol-utils
               cl-who
               hunchentoot
               drakma
               cxml
               closure-html
               parenscript
               cl-json)
  :serial t
  :components ((:file "packages")
               (:file "cl-who-helpers")
               (:file "hunchentoot-helpers")
               (:file "url-helpers")
               (:file "breadcrumbs")
               (:file "scrape-utils")
               (:file "parenscript-helpers")
               (:file "js-utils")
               (:file "download-web-libraries")))

