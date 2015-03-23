(defpackage :web-authentification
  (:nicknames :web-auth)
  (:use :cl :ol :hunchentoot)
  (:export
   #:protected-p
   #:protection-identifier
   #:authorised-p
   #:authorise
   #:with-protection
   #:with-protection/silent
   #:acquire-authorisation
   #:auth-required-p))

(in-package :web-authentification)

;;; interface for supporting password protected objects
(defgeneric protected-p (object)
  (:documentation "If this returns non-nil, this means we require an
  authentification before granting access to the object. The value
  should indicate how to authentify, for now it should be a string
  which we interpret as a password."))

(defgeneric protection-identifier (object)
  (:documentation "If several (grouped) objects need the same
  password, we only want to ask once for it. So every object needs to
  report the identifier of its group. This can be an arbitrary object,
  by default the object itself."))

(defmethod protection-identifier (object)
  "By default, `protection-identifier' is just `identity'."
  object)

(defgeneric acquire-authorisation (object protection-token)
  (:documentation "Either return `:success', if authorisation was
  successful, or the (string) contents of a webpage which presents
  e.g. a form requesting credentials. The form data could then be
  analysed in the subsequent invocation of this function."))

;;; checking and storing authorisation
(defun authorised-p (object)
  "Check if the user has already supplied the password for `object'."
  (let ((session (start-session)))
    (aand (session-value 'web-access-authorisation session)
          (gethash (protection-identifier object) it))))

(defun authorise (object)
  "Mark `object' as free to access for current user."
  (let* ((session (start-session))
         (table #1=(session-value 'web-access-authorisation session)))
    (unless table
      (setf table (make-hash-table)
            #1# table))
    (setf (gethash (protection-identifier object) table) t)))

(defun auth-required-p (object)
  "Determine if access to this `object' is already granted, or further
authentification is required."
  (and (protected-p object)
       (not (authorised-p object))))

;;; macro for easy access restricting
(defun with-protection% (object acquire-authorisation body-function)
  "Backing function for `with-protection'."
  (declare (inline auth-required-p))
  (if (auth-required-p object)
      (if acquire-authorisation
          ;; produce the login form
          (let ((lf (acquire-authorisation object (protected-p object))))
            (if (eq lf :success)
                (progn
                  (authorise object)
                  (funcall body-function))
                lf))
          (web-utils:error-code +http-forbidden+))
      ;; provide the page content
      (funcall body-function)))

(defmacro! with-protection (object &body body)
  "Helper macro, which causes a password prompt to be shown if the
`object' is protected."
  `(with-protection% ,object t (lambda () ,@body)))

(defmacro! with-protection/silent (object &body body)
  "Helper macro, which denies access to `object' if it is protected
and no yet authorised."
  `(with-protection% ,object nil (lambda () ,@body)))
