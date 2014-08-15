(defpackage :bootstrap-helpers
  (:nicknames :bootstrap)
  (:use :cl :ol :cl-who :web-utils)
  (:export
   #:navbar
   #:elink
   #:link
   #:bs-body
   #:navbar+
   #:named-section*
   #:html/document+bs))

(in-package :bootstrap-helpers)

(defmacro html/document+bs (parameters &body body)
  `(html/document (:style "/~olaf/style.css"
                     :library :jquery
                     :library :bootstrap
                     :script "/scripts/utils.js"
                     :script "/scripts/utils-bootstrap.js"
                     ,@parameters)
     ,@body))


(defmacro elink (target label)
  `(htm (:a :target "_blank" :href ,target ,label)))

(defmacro link (target label)
  `(htm (:a :href ,target ,label)))

(defmacro navbar+ (title navbar-links &body body)
  "Insert a standard navbar for bootstrap styling"
  (flet ((naventry (link &optional active)
           (when link
             `(:li ,@(if active '(:class "active"))
                   (:a :href ,(first link) (esc ,(second link)))))))
    ;; todo handle case of no nav entries specially
    (let ((navbar-links (group navbar-links 2)))
      `(htm
        (:div :class "navbar navbar-default navbar-fixed-top" :role "navigation"
              (:div :class "container"
                    (:div :class "navbar-header"
                          (:button :type "button" :class "navbar-toggle collapsed" :data-toggle "collapse" :data-target ".navbar-collapse"
                                   (:span :class "sr-only" "toggle navigation")
                                   #2=(:span :class "icon-bar") #2# #2#)
                          (:a :class "navbar-brand" :href "#" (esc ,title)))
                    (:div :class "navbar-collapse collapse"
                          ,(when navbar-links
                                 `(:ul :class "nav navbar-nav"
                                       ,(naventry (first navbar-links) nil)
                                       ,@(mapcar #'naventry (rest navbar-links))))
                          ,@body)))))))

(defmacro navbar (title &rest navbar-links)
  `(navbar+ ,title ,navbar-links))

(defmacro bs-body (&body body)
  `(htm (:div :class "container"
              (:div :class "bs-body"
                    ,@body))))

(defmacro named-section* (title &body body)
  `(htm (:section (:a :name ,title)
                 (:h2 (esc ,title))
                 ,@body)))

;;; if desired, replace the list of available applications with a
;;; boostrap styled version
(defun web-utils::show-available-applications ()
  (let ((server-name (format nil "~:(~A~)'s hunchentoot instance"
                             (sb-unix:uid-username (sb-unix:unix-getuid)))))
    (html/document+bs (:title server-name)
      (navbar server-name
              "/hunchentoot-doc.html" "Documentation")
      (bs-body
        (:h1 (esc server-name))
        (:ul :class "list-unstyled" :style "margin-left: 4em"
           (dolist (app available-applications)
             (unless (string-equal (first app) "Documentation")
               (htm (:li :style "margin-bottom: 4pt"
                       (:a :href (second app)
                          :class "btn btn-default"
                          (:span :class "glyphicon glyphicon-globe"
                             :style (css-lite:inline-css :margin-right "5pt"))
                          (esc (first app))))))))))))
