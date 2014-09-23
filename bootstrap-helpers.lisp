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
   #:html/document+bs
   #:breadcrumbs
   #:bs-body0))

(in-package :bootstrap-helpers)

(load-web-library :jquery)
(load-web-library :bootstrap)

(defmacro html/document+bs (parameters &body body)
  `(html/document (:style "/style/bootstrap-custom.css"
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
        (:div :class "navbar navbar-default" :role "navigation"
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

(defun breadcrumbs (&rest breadcrumbs)
  (html/node
    (:ul :class "breadcrumb"
       (:li (:a :href "/"
               (:span :class "glyphicon glyphicon-home")
               (:span :class "sr-only" "home")))
       (dolist (brc (group breadcrumbs 2))
         (dbind (br label) brc
           (cond ((equal br "#")
                  (cl-who:htm (:li :class "active" (:a :href br (cl-who:esc label)))))
                 ((stringp br)
                  (cl-who:htm (:li (:a :href br (cl-who:esc label)))))
                 ((listp br)            ; a breadcrumb-list
                  (cl-who:htm (:li (:a :href (breadcrumb->url br) (cl-who:esc label)))))))))))


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
                             #+sbcl (sb-unix:uid-username (sb-unix:unix-getuid))
                             #-sbcl "somebody")))
    (html/document+bs (:title server-name)
      (navbar server-name
              "/hunchentoot-doc.html" "Documentation")
      (bs-body
        (:h1 (esc server-name))
        (:ul :class "list-unstyled available-applications"
           (dolist (app available-applications)
             (unless (string-equal (first app) "Documentation")
               (htm (:li
                       (:a :href (second app)
                          :class "btn btn-default"
                          (:span :class "glyphicon glyphicon-globe icon-space-right")
                          (esc (first app))))))))))))

;;; finally, we provide the customisations for the bootstrap style
(register-breadcrumb-dispatcher '(style "bootstrap-custom.css") 'homepage-css :dir-p nil :replace t)
(register-breadcrumb-dispatcher '(style "bootstrap-nonav.css") 'homepage-css-nonav :dir-p nil :replace t)

;; todo adjust the colours and fonts
(defun homepage-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (lass:compile-and-write
   '(body :padding-top "0px")
   '(.navbar :margin-bottom "0px")
   '(.bs-body :padding "10px 15px")
   '(h1 :text-align "center")
   '(.bigger :font-size "120%")
   '((:or .user-help .user-info)
     :padding "1ex"
     :border-radius "5px")
   '(p.dropdown-menu :padding "5pt"
     :padding-left "10pt")
   '(table :font-size "110%")
   '(.icon-space-right :margin-right "5pt")
   '(.icon-space-left :margin-left "5pt")
   '(.medskip-after :margin-bottom "1em")
   '(.available-applications
     :margin-left "4em"
     :margin-top "2em"
     (li :margin-bottom "4pt"))
   '(.statistics-table  :font-size "80%" :width "auto" :margin-top "1em")
   '(.two-digit-num-input :width "4em")))

(defun homepage-css-nonav ()
  (setf (hunchentoot:content-type*) "text/css")
  (lass:compile-and-write
   '(body :padding-top "0px")))
