(in-package :web-utils)

(setf *prologue*
      ;; "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <!DOCTYPE html
      ;; PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
      ;; \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      cl-who::*html-empty-tag-aware-p* nil)
;; TODO maybe add doctype back ? at least locally, when generating html?

(defparameter html-output-stream nil)

(defun keyword/get-all (keyword argument-list &optional acc)
  "From an argument list such as '(:a 1 :b 2 :a 3), retrieve all
  assignments to KEYWORD. For :a, this function would return '(1 3),
  for :b, just '(2)."
  (if argument-list
      (if (eq keyword (car argument-list))
          (keyword/get-all keyword (cddr argument-list)
                           (cons (second argument-list) acc))
          (keyword/get-all keyword (cddr argument-list) acc))
      (nreverse acc)))

(defmacro html-doc ((&whole parameters &key title (lang "en") style script) &body body)
  `(with-html-output-to-string (html-output-stream nil :prologue t :indent 0)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
            :xml\:lang ,lang
            :lang ,lang
            (:head
             (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
             ,(when title `(:title ,title))
             ,@(when style
                     (mapcar #`(:link :rel "stylesheet" :type "text/css" :href ,a1)
                             (keyword/get-all :style parameters))))
            ,@(when script
                    (mapcar #`(:script :type "text/javascript" :src ,a1)
                            (keyword/get-all :style parameters)))
            (:body ,@body))))

(defmacro html-part (&body body)
  `(with-html-output (html-output-stream nil  :indent 4)
     ,@body))

;; TODO improve this and also handle declares and such.
(defmacro with-doc (&body body)
  "If the first entry of body is a string, bind it anaphorically to DOC."
  `(if (stringp (first body))
       (let ((doc (first body))
             (body (rest body)))
         ,@body)
       (let ((doc nil))
         ,@body)))

(defmacro define-html-part (name params &body body)
  `(defun ,name ,params
          (html-part ,@body)))

(defun uri (base &rest parameters)
  "Format an url with BASE and a number of PARAMETERS, given like
keyword parameters to a function.  Possibly add global state parameters."
  (format nil "~A~@[?~{~(~A~)=~A~^&~}~]" base parameters))
