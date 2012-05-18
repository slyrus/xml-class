
(asdf:defsystem #:xml-class
  :name "xml-class"
  :description "Metaclass for working with XML documents"
  :version "0.0.1"
  :licence "BSD"
  :depends-on (closer-mop alexandria local-time cxml cxml-stp)
  :serial t
  :components
  ((:cl-source-file "package")
   (:cl-source-file "xml-utilities")
   (:cl-source-file "xml-class")))

