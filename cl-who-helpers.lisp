(in-package :web-utils)

(defparameter xml-prologue
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

(defparameter xhtml-prologue
  (concatenate
   'string xml-prologue
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

(setf *prologue* xml-prologue
      cl-who::*html-empty-tag-aware-p* nil)

(defparameter xml-output-stream nil)

(defun get-all (keyword argument-list &optional acc)
  "From an argument list such as '(:a 1 :b 2 :a 3), retrieve all
  assignments to KEYWORD. For :a, this function would return '(1 3),
  for :b, just '(2)."
  (if argument-list
      (if (eq keyword (car argument-list))
          (get-all keyword (cddr argument-list)
                   (cons (second argument-list) acc))
          (get-all keyword (cddr argument-list) acc))
      (nreverse acc)))

(defmacro html/document ((&whole parameters &key
                                 title
                                 (lang "en")
                                 style
                                 script)
                         &body body)
  `(with-html-output-to-string (xml-output-stream nil :prologue xhtml-prologue :indent 0)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
            :xml\:lang ,lang
            :lang ,lang
            (:head
             (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
             ,(when title `(:title ,title))
             ,@(when style
                     (mapcar #`(:link :rel "stylesheet" :type "text/css" :href ,a1)
                             (get-all :style parameters))))
            ,@(when script
                    (mapcar #`(:script :type "text/javascript" :src ,a1)
                            (get-all :style parameters)))
            (:body ,@body))))

(defmacro html/node (&body body)
  `(with-html-output (xml-output-stream nil :indent 4)
     ,@body))

(defmacro xml/document ((&key) &body body)
  `(with-html-output-to-string (xml-output-stream nil :prologue xml-prologue :indent 0)
     ,@body))

(defmacro xml/node (&body body)
  `(with-html-output (xml-output-stream nil :indent 4)
     ,@body))

;; TODO how to make indentation consistent for xml/node stuff
;; TODO what about a different approach to this??

(defun uri (base &rest parameters)
  "Format an url with BASE and a number of PARAMETERS, given like
keyword parameters to a function.  Possibly add global state parameters."
  (format nil "~A~@[?~{~(~A~)=~A~^&~}~]" base parameters))
