(in-package :web-utils)

(defun uri->html-document (url)
  (chtml:parse (drakma:http-request url)
               (cxml-dom:make-dom-builder)))

(defun uri->file (uri filename)
  "Download a binary file from the given uri and save it to filename."
  (with-open-file (output filename :direction :output :if-exists :supersede
                          :element-type 'unsigned-byte)
    (write-sequence (drakma:http-request uri) output)))
