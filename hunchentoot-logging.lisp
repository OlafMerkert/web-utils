(defpackage :hunchentoot-logging
  (:nicknames :ht-log)
  (:use :cl :ol :hunchentoot :clsql)
  (:export
   #:easy-acceptor/db-log))

(in-package :hunchentoot-logging)

(locally-enable-sql-reader-syntax)

(defclass easy-acceptor/db-log (easy-acceptor)
  ()
  (:documentation "TODO"))

(def-view-class http-log ()
  ((logid           :db-kind :key :db-constraints :not-null :type integer
                    :initform (sequence-next 'logid))
   (remote-addr     :type string         :initarg :remote-addr     :nulls-ok t)
   (x-forwarded-for :type string         :initarg :x-forwarded-for :nulls-ok t)
   (authorization   :type string         :initarg :authorization   :nulls-ok t)
   (time            :type universal-time :initarg :time            :nulls-ok t)
   (request-method  :type string         :initarg :request-method  :nulls-ok t)
   (script-name     :type string         :initarg :script-name     :nulls-ok t)
   (query-string    :type string         :initarg :query-string    :nulls-ok t)
   (server-protocol :type string         :initarg :server-protocol :nulls-ok t)
   (return-code     :type integer        :initarg :return-code     :nulls-ok t)
   (content-length  :type integer        :initarg :content-length  :nulls-ok t)
   (referer         :type string         :initarg :referer         :nulls-ok t)
   (user-agent      :type string         :initarg :user-agent      :nulls-ok t)))


(defmethod print-object ((http-log http-log) stream)
  (with-slots #1=(logid remote-addr x-forwarded-for authorization time request-method script-name query-string server-protocol return-code content-length referer user-agent)
    http-log
    (let ((time (hunchentoot::iso-time time)))
     (format stream
             "<id ~A> ~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
             . #1#)))
  http-log)

(defmethod acceptor-log-access ((acceptor easy-acceptor/db-log) &key return-code)
  "Logging to an SQL database."
  (let ((log-entry
         (make-instance 'http-log
                        :remote-addr     (remote-addr*)
                        :x-forwarded-for (header-in* :x-forwarded-for)
                        :authorization   (authorization)
                        :time            (get-universal-time)
                        :request-method  (request-method*)
                        :script-name     (script-name*)
                        :query-string    (query-string*)
                        :server-protocol (server-protocol*)
                        :return-code     return-code
                        :content-length  (content-length*)
                        :referer         (referer)
                        :user-agent      (user-agent)
                        )))
    (update-records-from-instance log-entry :database *http-log-db*)))

(defpar *http-log-db-path* "/home/olaf/tmp/hunchentoot.log.sqlite")

(defvar *http-log-db* nil)

(defun connect-http-log-db ()
  (setf *http-log-db* (connect (list *http-log-db-path*)
                               :database-type :sqlite3))
  (execute-command "PRAGMA synchronous=OFF" :database *http-log-db*)
  (setf *default-caching* nil))

(defun setup-http-log-db ()
  (unless (sequence-exists-p 'logid)
    (create-sequence 'logid))
  (unless (table-exists-p 'http-log)
    (create-view-from-class 'http-log))
  (unless (index-exists-p [time-index])
    (create-index [time-index] :on [http-log] :attributes '([time])))
  (unless (index-exists-p [script-name-index])
    (create-index [script-name-index] :on [http-log] :attributes '([script-name]))))
