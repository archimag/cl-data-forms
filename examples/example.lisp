;;;; example.lisp

(ql:quickload "local-time")
(ql:quickload "restas")
(ql:quickload "closure-template")

(asdf:operate 'asdf:load-op '#:data-forms)

(closure-template:compile-cl-templates
 (merge-pathnames #P"example.tmpl" (or *load-pathname* *compile-file-pathname*)))

(restas:define-module #:data-forms.example
  (:use #:cl)
  (:import-from #:data-forms #:define-form-class)
  (:render-method (make-instance (find-symbol "RENDERER" '#:data-forms.example))))

(in-package #:data-forms.example)

;; user info form

(define-form-class user-info-form ()
  ((username
    :initarg |username|
    :initform nil
    :required "Username is required"
    :label "Username")
   (email
    :initarg |email|
    :initform nil
    :stype data-sift:email
    :required t
    :label "Email Address")))

;; user password form

(define-form-class user-password-form ()
  ((password
    :initarg |password|
    :initform nil
    :required "Password is required"
    :stype (string :min-length 6 :message "Password must be at least 6 characters.")
    :label "Password"
    :itype "password")
   (confirm-password
    :initarg |confirmPassword|
    :initform nil
    :itype "password"
    :label "Confirm password")))

(defmethod shared-initialize :after ((form user-password-form) slot-names &key)
  (unless (equal (slot-value form 'password) (slot-value form 'confirm-password))
    (setf (data-forms:field-error form 'confirm-password)
          "Passwords don't match.")))

;; registration-form

(defmethod data-sift:compile-parse-rule ((rule (eql 'date)) &key)
  (declare (optimize (debug 3)))
  (alexandria:named-lambda date-parser (str)
    (handler-case 
        (local-time:parse-rfc3339-timestring str :allow-missing-time-part t)
      (error ()
        (data-sift::vfail "Invalid date")))))

(defmethod data-sift:compile-render-rule ((rule (eql 'date)) &key)
  (alexandria:named-lambda date-renderer (date)
    (local-time:format-timestring nil date :format local-time:+rfc3339-format/date-only+)))


(define-form-class registration-form (user-info-form user-password-form)
  ((birthday
    :initform nil
    :initarg |birthday|
    :stype date
    :label "Birthday"
    :itype "date")
   (keep-me-signed
    :initform nil
    :initarg |keepMeSigned|
    :label "Keep me signed-in on this computer."
    :itype "checkbox")))

;; routes

(restas:define-route registration-prompt ("" :method :get)
  (make-instance 'registration-form))

(restas:define-route handle-registration ("" :method :post)
  (:additional-variables (post (hunchentoot:post-parameters*)))
  (data-forms:make-form 'registration-form post))

;; renderer

(defclass renderer () ())

(defmethod restas:render-object ((renderer renderer) (form registration-form))
  (data-forms.example.view:registration-page
   (list :form (data-forms:form-data-plist form))))

;; start

(restas:start '#:data-forms.example :port 8080)
