
(defpackage :xml-class
  (:use :cl :alexandria)
  (:export #:integer-or-null
           #:float-or-null

           #:xml-class
           #:xml-object
           #:document
           #:date-time
           
           #:process-node

           #:parse
           #:parse-file))

