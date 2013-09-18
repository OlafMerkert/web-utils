(in-package :web-utils)

(defmacro define-string-starts-with (name first-char)
  `(defun ,name (url)
     (and (stringp url) (equal (elt url 0) ,first-char)))  )

;; the links we are looking for are site relative, where page links
;; start with a '?', and image page links start with a '/'
(define-string-starts-with top-level-url-p #\/)
(define-string-starts-with parameter-change-p #\?)

(defun merge-urls (url parent-url)
  "Compute the absolute url of a given site relative url, which may
start with '/' or '?', in which case we remove the correct parts of
parent-url before concatenating."
  (concatenate 'string
               (cond ((top-level-url-p url)
                      ;; http:// has length 7
                      (let ((sub-url-start (or (position #\/ parent-url :start 7) (length parent-url))))
                        (subseq parent-url 0 sub-url-start)))
                     ((parameter-change-p url)
                      (let ((sub-url-start (or (position #\? parent-url :start 7) (length parent-url))))
                        (subseq parent-url 0 sub-url-start)))
                     (t parent-url))
               url))
