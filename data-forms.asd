;;;; data-forms.asd

(defsystem #:data-forms
  :depends-on (#:iterate #:closer-mop #:alexandria #:data-sift )
  :pathname "src/"
  :serial t
  :components ((:file "defpackage")
               (:file "data-forms")))
