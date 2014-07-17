(in-package :web-utils)

(defpar web-library-directory (merge-pathnames ".cache/web-libraries/"
                                               (user-homedir-pathname)))

(defvar loaded-web-libraries (make-hash-table))

(defmacro define-web-library (name &rest properties)
  (alist-bind (params
               files
               remote-prefix
               local-prefix) properties
    `(defmethod load-web-library ((name (eql ',name)) &key ,@params)
       (unless (gethash ',name loaded-web-libraries)
         (download-and-serve
          (mapcar (clambda (format nil x! ,@(mapcar #'unbox1 params))) (list ,@files))
          ,(car remote-prefix)
          ,(car local-prefix))
         (setf (gethash ',name loaded-web-libraries) t)))))

(defgeneric load-web-library (name &key))

(define-web-library :jquery
    (:params (version "1.10.2"))
  (:files "jquery-~A.min.js"
          "jquery-~A.js"
          "jquery-~A.min.map")
  (:remote-prefix "http://code.jquery.com/")
  (:local-prefix "/scripts/"))

(define-web-library :jquery-sticky
  (:files "sticky.js"
          "sticky.css"
          "close.png")
  (:remote-prefix "http://raw2.github.com/OlafMerkert/Sticky/master/")
  (:local-prefix "/scripts/sticky/"))

;; todo automate using web libraries in sites (at least css and js parts)
;; todo dependencies between web libraries?

(defun download-and-serve (files remote-prefix local-prefix)
  (ensure-directories-exist web-library-directory)
  (mapcar (lambda (file)
            (let ((remote-url (conc remote-prefix file))
                  (local-file (merge-pathnames file web-library-directory))
                  (local-url (conc local-prefix file)))
              (unless (uiop:probe-file* local-file)
                ;; need to download this file
                (uri->file remote-url local-file))
              (setup-static-content local-url local-file)
              local-url))
          files))

(defun url-basename (url)
  "Given an `url' with forward slashes, find the basename (filename)
contained in it. If no slashes are present, return all of `url', if
the `url' terminates in a slash, return an empty string."
  (strip-to-char #\/ url :from-end t))

(defun extract-archive (archive-path extraction-dir)
  "extract the archive into `extraction-dir' and return a list of the
  relative paths of the files contained in the archive."
  (zip:unzip archive-path extraction-dir)
  (list-archive archive-path))

(defun hash-keys (hashtable)
  "produce a list of the keys present in `hashtable'."
  (let (keys)
    (maphash (ilambda (k v) (push k keys)) hashtable)
    keys))

(defun list-archive (archive-path)
  "return a list of the relative paths of the files contained in the archive."
  (hash-keys (zip:with-zipfile (zipfile archive-path)
               (zip:zipfile-entries zipfile))))

(defun strip-to-char (char string &key from-end)
  "remove everything up to first occurence of `char' in `string'. If there is no occurence, return the entire string."
  (let ((pos (aif (position char string :from-end from-end :test #'char=)
                  (+ 1 it) 0)))
    (subseq string pos)))

(defun load-bootstrap ()
  (let* ((version "3.2.0")
         (archive-url (format nil "https://github.com/twbs/bootstrap/releases/download/v~A/bootstrap-~A-dist.zip" version version)))
    (download-and-serve/archive archive-url "/bootstrap/")))

(defun trailing-slash-p (string)
  (and (< 0 (length string))
       (char= #\/ (alast string))))

(defun download-and-serve/archive (archive-url local-prefix)
  (let* ((archive-filename (url-basename archive-url))
         (local-archive (merge-pathnames archive-filename web-library-directory))
         (local-files))
    (uri->file archive-url local-archive)
    (setf local-files (if (uiop:probe-file* local-archive)
                          (list-archive local-archive)
                          (extract-archive local-archive web-library-directory)))
    (mapcar (lambda (file)
              ;; get rid of the "name" of the zipfile
              (setf file (strip-to-char #\/ file))
              (unless (or (length=0 file)
                          (trailing-slash-p file))
                (let ((local-file (merge-pathnames file web-library-directory))
                      (local-url (conc local-prefix file)))
                  (setup-static-content local-url local-file)
                  local-url)))
            local-files)))
