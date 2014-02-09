(in-package :web-utils)

;;; working with breadcrumb (lists of symbols)

(defpar *user-homedirs* #P"/home/")
(defpar *document-root* #P"/var/www/")

(defun breadcrumb->url (list &optional trailing-slash)
  (format nil "~{/~(~A~)~}~:[~;/~]" list trailing-slash))

(defun mkstr/lc (&rest args)
  (string-downcase (apply #'mkstr args)))

(defun breadcrumb->path (bc)
  (if (char= (char (symbol-name (first bc)) 0) #\~)
      ;; user directories
      (merge-pathnames
       (make-pathname :directory
                      (list* :relative
                             (string-downcase
                              (subseq (symbol-name (first bc)) 1))
                             "public_html"
                             (mapcar #'mkstr/lc (rest bc))))
       *user-homedirs*)
      ;; global document root
      (merge-pathnames
       (make-pathname :directory
                      (list* :relative (mapcar #'mkstr/lc bc)))
       *document-root*)))

(defgeneric breadcrumb (object))

;;; use breadcrumbs to setup listeners with hunchentoot
(defvar breadcrumb-dispatchers
  (make-hash-table :test 'equal))

(defmacro! push2 (o!item place1 place2)
  `(progn
     (push ,g!item ,place1)
     (push ,g!item ,place2)))

(defmacro! register-breadcrumb-dispatcher% (o!breadcrumb dispatcher &key replace)
  `(when (or ,replace
             (not #1=(gethash ,g!breadcrumb breadcrumb-dispatchers)))
     (push2 ,dispatcher
            hunchentoot:*dispatch-table* #1#)))

(bind-multi ((symbol symbol function))
  (defmethod register-breadcrumb-dispatcher (breadcrumb (serve-function symbol) &key (dir-p t) replace)
    (let ((url (breadcrumb->url breadcrumb)))
      (register-breadcrumb-dispatcher%
       breadcrumb
       (hunchentoot:create-regex-dispatcher
        (mkstr "^" url "$") serve-function)
       :replace replace)
      (when dir-p
        (register-breadcrumb-dispatcher%
         breadcrumb
         (hunchentoot:create-regex-dispatcher
          (mkstr "^" url "/$") serve-function)
         :replace t)))))

(defun filename (pathname)
  (aif (pathname-type pathname)
       (concatenate 'string #1=(pathname-name pathname) "." it)
       #1#))

(defmethod register-breadcrumb-dispatcher (breadcrumb (file pathname) &key replace (add-filename t))
  (let ((breadcrumb (if add-filename (append1 breadcrumb
                                              (filename file))
                        breadcrumb)))
    (register-breadcrumb-dispatcher%
     breadcrumb
     (hunchentoot:create-static-file-dispatcher-and-handler (breadcrumb->url breadcrumb) file)
     :replace replace)))
