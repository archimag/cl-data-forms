;;;; data-forms.lisp

(in-package #:data-forms)

(defclass data-form-class (standard-class)
  ((field-order :initform ()
               :initarg :field-order
               :reader class-field-order)))


(defmethod direct-slot-definition-class ((class data-form-class) &rest initargs)
  (declare (ignore initargs))
  'data-form-slot-definition)

(defmethod effective-slot-definition-class ((class data-form-class) &rest initargs)
  (declare (ignore initargs))
  'data-form-effective-slot-definition)

(defmethod compute-slots ((class data-form-class))
  (let ((normal-slots (call-next-method)))
    (let ((order (iter (for item in (class-field-order class))
                       (cond
                         ((consp item)
                          (dolist (super (mapcar #'find-class item))
                            (unless (class-finalized-p super)
                              (finalize-inheritance super))
                            (dolist (slot (class-slots super))
                              (collect (slot-definition-name slot)))))
                         (t
                          (collect item))))))
      (sort (copy-list normal-slots)
            #'(lambda (a b)
                (let ((pos-a (position (slot-definition-name a) order))
                      (pos-b (position (slot-definition-name b) order)))
                  (cond
                    ((and pos-a pos-b)
                     (< pos-a pos-b))
                    (pos-a t)
                    (pos-b nil)
                    (t t))))))))

(defmethod validate-superclass ((sub data-form-class) (super standard-class))
  t)

(defclass data-form-slot-definition (standard-direct-slot-definition)
  ((sift-type :initarg :stype :initform nil :reader data-form-field-type)
   (required :initarg :required :initform nil :reader data-form-field-required)
   (additional-data :initform nil)))

(defmethod shared-initialize :after ((slot-definition data-form-slot-definition) slot-names &rest initargs &key &allow-other-keys)
  (unless (slot-definition-initargs slot-definition)
    (error "Not defined initargs for ~A" (slot-definition-name slot-definition)))
  (let ((slot-initargs (flatten (mapcar #'slot-definition-initargs (class-slots (class-of slot-definition))))))
    (setf (slot-value slot-definition 'additional-data)
          (iter (for prop in (plist-alist initargs))
                (unless (member (car prop) slot-initargs)
                  (collect prop))))))

(defun default-field-parser (str)
  str)

(defun default-field-renderer (val)
  (if (stringp val)
      val
      (write-to-string val)))

(defclass data-form-effective-slot-definition (standard-effective-slot-definition)
  ((sift-type
    :reader data-form-field-type)
   (parser
    :initform #'default-field-parser
    :reader data-form-field-parser)
   (renderer
    :initform #'default-field-renderer
    :reader data-form-field-renderer)
   (required
    :reader data-form-field-required)
   (additional-data
    :reader data-form-field-additional-data)))

(defmethod compute-effective-slot-definition ((class data-form-class) name direct-slots)
  (let* ((normal-slot (call-next-method))
         (direct-slot (find name
                            direct-slots
                            :key #'slot-definition-name))
         (sift-type (data-form-field-type direct-slot)))
    (iter (for slot in '(required additional-data sift-type))
          (setf (slot-value normal-slot slot)
                (slot-value direct-slot slot)))
    (when sift-type
      (setf (slot-value normal-slot 'parser)
            (data-sift:compile-parse-rule sift-type))
      (setf (slot-value normal-slot 'renderer)
            (data-sift:compile-render-rule sift-type)))
    normal-slot))

(defclass field-value ()
  ((value :initarg :value :accessor data-form-field-value)
   (error :initarg :error :initform nil :accessor data-form-field-error)))

(defmethod slot-value-using-class ((class data-form-class) object (slot data-form-effective-slot-definition))
  (let ((value (call-next-method)))
    (values (data-form-field-value value)
            (data-form-field-error value))))

(defmethod (setf slot-value-using-class) (new-value (class data-form-class) object (slot data-form-effective-slot-definition))
  (typecase new-value
    (field-value
     (call-next-method))
    (string
     (cond
       ((not (emptyp new-value))
        (handler-case         
            (setf (slot-value-using-class class object slot)
                  (make-instance 'field-value
                                 :value (funcall (data-form-field-parser slot) new-value)))
          (data-sift:validation-fail (err)
            (setf (slot-value-using-class class object slot)
                  (make-instance 'field-value
                                 :value new-value
                                 :error (data-sift:validation-fail-message err))))))
       ((data-form-field-required slot)
        (let ((maybe-msg (data-form-field-required slot)))
          (setf (slot-value-using-class class object slot)
                (make-instance 'field-value
                               :value new-value
                               :error (if (stringp maybe-msg)
                                          maybe-msg
                                          "A value is required.")))))
       (t (setf (slot-value-using-class class object slot)
                (make-instance 'field-value
                               :value nil)))))
    (t
     (setf (slot-value-using-class class object slot)
           (make-instance 'field-value
                          :value new-value)))))

(defun find-field-slot (form field)
  (find field (class-slots (class-of form)) :key #'slot-definition-name))

(defun form-field-values (form field)
  (slot-value-using-class 'data-form-class
                          form
                          (find-field-slot form field)))

(defun field-value (form field)
  (let ((val (find-field-slot form field)))
    (funcall (if val (data-form-field-renderer val))
             (nth-value 0 (form-field-values form field)))))

(defun field-error (form field)
  (nth-value 1 (form-field-values form field)))

(defun (setf field-value) (new-value form field)
  (setf (slot-value form field)
        new-value))

(defun (setf field-error) (new-value form field)
  (setf (slot-value form field)
        (make-instance 'field-value
                       :value (slot-value form field)
                       :error new-value)))

;; define-from-class

(defmacro define-form-class (name superclasses &body slots-and-class-options)
  `(defclass ,name ,superclasses
     ,@slots-and-class-options
     (:field-order ,superclasses ,@(mapcar #'car (car slots-and-class-options)))
     (:metaclass data-form-class)))

;; form values

(defun field-keyword-name (slot)
  (first (slot-definition-initargs slot)))

(defun form-values (form  &aux (form-class (find-class 'data-form-class)))
  (iter (for slot in (class-slots (class-of form)))
        (when (typep slot 'data-form-effective-slot-definition)
          (collect (field-keyword-name slot))
          (collect (nth-value 0 (slot-value-using-class form-class form slot))))))

;; form data

(defun form-field-data (form field-slot)
  (let ((data (data-form-field-additional-data field-slot)))
    (multiple-value-bind (val err) (slot-value-using-class 'data-form-class form field-slot)
      (push (cons :value (if (and val (null err))
                             (funcall (data-form-field-renderer field-slot) val)
                             val))
            data)
      (when err
        (push (cons :error err) data)))
    data))

(defun form-data-plist (form)
  (iter (for slot in (class-slots (class-of form)))
        (when (typep slot 'data-form-effective-slot-definition)
          (collect (field-keyword-name slot))
          (collect (alist-plist (form-field-data form slot))))))

(defun form-data-alist (form)
  (iter (for slot in (class-slots (class-of form)))
        (when (typep slot 'data-form-effective-slot-definition)
          (collect
              (cons (field-keyword-name slot)
                    (form-field-data form slot))))))

;; is valid

(defun is-valid (form)
  (iter (for slot in (class-slots (class-of form)))
        (when (typep slot 'data-form-effective-slot-definition)
          (when (nth-value 1 (slot-value-using-class 'data-form-class form slot))
            (return-from is-valid nil))))
  t)

;; make form

(defun make-form (form-class post-parameters)
  (let ((class (find-class form-class)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (iter (for field-slot in (class-slots (find-class form-class)))
          (when (typep field-slot 'data-form-effective-slot-definition)
            (let ((param-name (field-keyword-name field-slot)))
              (collect param-name  into initargs)
              (collect (or (cdr (assoc param-name post-parameters :test #'string-equal)) "")
                into initargs)))
          (finally
           (return (apply #'make-instance form-class initargs))))))

  
