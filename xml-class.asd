
(asdf:defsystem #:xml-class
  :name "xml-class"
  :description "Metaclass for working with XML documents"
  :author "Orignally by Hans Huebner. Adopted and adapted by Cyrus Harmon."
  :version "0.0.1"
  :licence "BSD"
  :depends-on (closer-mop alexandria local-time cxml cxml-stp)
  :serial t
  :components
  ((:cl-source-file "package")
   (:cl-source-file "xml-utilities")
   (:cl-source-file "xml-class")))

(cl:defpackage #:xml-class-config
  (:export #:*base-directory*))

(cl:defparameter xml-class-config::*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

