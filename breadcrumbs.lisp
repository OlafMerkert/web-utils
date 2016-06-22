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

(defmethod register-breadcrumb-dispatcher (breadcrumb (file pathname)
                                           &key replace (add-filename t)
                                                (if-not-exists :silent))
  (let ((breadcrumb (if add-filename (append1 breadcrumb
                                              (filename file))
                        breadcrumb)))
    (cond ((uiop/filesystem:file-exists-p file)
           (register-breadcrumb-dispatcher%
            breadcrumb
            (hunchentoot:create-static-file-dispatcher-and-handler (breadcrumb->url breadcrumb) file)
            :replace replace))
          ((eq if-not-exists :error)
           (error "File not found: ~A" file)))))

(declaim (inline remove-file-ending parse-url->breadcrumb))
(defun remove-file-ending (string &optional (max-length 4))
  (mvbind (match registers)
      (cl-ppcre:scan-to-strings
       `(:sequence (:register (:greedy-repetition 0 nil :everything))
                   "." (:greedy-repetition 1 ,max-length :word-char-class))
       string)
    (if match
        (aref registers 0)
        string)))

(defun parse-url->breadcrumb (string)
  "Transform an absolute url (without server, so for instance the
scriptname) to a breadcrumb, ignoring the file ending in the last
part."
  (rest (split-sequence:split-sequence #\/
                        (remove-file-ending (url-decode string))
                        :remove-empty-subseqs t)))
