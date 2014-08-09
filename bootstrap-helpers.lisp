(defpackage :bootstrap-helpers
  (:nicknames :bootstrap)
  (:use :cl :ol :cl-who :web-utils)
  (:export
   #:navbar
   #:elink
   #:link))

(in-package :bootstrap-helpers)

(defmacro elink (target label)
  `(htm (:a :target "_blank" :href ,target ,label)))

(defmacro link (target label)
  `(htm (:a :href ,target ,label)))

(defmacro navbar (title &rest navbar-links)
  "Insert a standard navbar for bootstrap styling"
  (flet ((naventry (link &optional active)
           (when link
            `(:li ,@(if active '(:class "active"))
                  (:a :href ,(first link) ,(second link))))))
    (let ((navbar-links (group navbar-links 2)))
     `(htm
       (:div :class "navbar navbar-inverse navbar-fixed-top" :role "navigation"
             (:div :class "container"
                   (:div :class "navbar-header"
                         (:button :type "button" :class "navbar-toggle collapsed" :data-toggle "collapse" :data-target ".navbar-collapse"
                                  (:span :class "sr-only" "toggle navigation")
                                  #2=(:span :class "icon-bar") #2# #2#)
                         (:a :class "navbar-brand" :href "#" (esc ,title)))
                   (:div :class "navbar-collapse collapse"
                         (:ul :class "nav navbar-nav"
                              ,(naventry (first navbar-links) t)
                              ,@(mapcar #'naventry (rest navbar-links))))))))))
