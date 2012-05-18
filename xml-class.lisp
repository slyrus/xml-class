
(in-package :xml-class)

(defclass xml-class (closer-mop:standard-class)
  ())

(defmethod closer-mop:validate-superclass ((sub xml-class)
                                           (super closer-mop:standard-class))
  t)

(defclass xml-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((xpath :reader xpath
          :initarg :xpath)
   (xml-type :reader xml-type
             :initarg :xml-type))
  (:default-initargs :xml-type nil))

(defclass xml-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((xpath :reader xpath
          :initarg :xpath)
   (xml-type :reader xml-type
             :initarg :xml-type))
  (:default-initargs :xml-type nil))

(defmethod closer-mop:direct-slot-definition-class ((class xml-class)
                                                    &key xpath &allow-other-keys)
  (cond
    (xpath
     (when (equal #\/ (char xpath 0))
       (error "slot XPaths must not be absolute paths"))
     'xml-direct-slot-definition)
    (t
     (call-next-method))))

(defmethod closer-mop:effective-slot-definition-class ((class xml-class)
                                                       &key &allow-other-keys)
  'xml-effective-slot-definition)

(defmethod closer-mop:compute-effective-slot-definition ((class xml-class)
                                                         (name t)
                                                         direct-slot-definitions)
  ;; there must be a better way to initialize the effective slot definition
  (let ((effective-slot-definition (call-next-method))
        (xml-direct-slot-definition
          (find-if (lambda (direct-slot-definition)
                     (when (typep direct-slot-definition 'xml-direct-slot-definition)
                       direct-slot-definition))
                   direct-slot-definitions)))
    (when xml-direct-slot-definition
      (setf (slot-value effective-slot-definition 'xpath)
            (slot-value xml-direct-slot-definition 'xpath))
      (setf (slot-value effective-slot-definition 'xml-type)
            (slot-value xml-direct-slot-definition 'xml-type)))
    effective-slot-definition))

(defclass xml-object ()
  ((document :initarg :document
             :reader document))
  (:documentation "Topmost object of every instance with the XML-CLASS metaclass")
  (:default-initargs :document nil))

(defmethod initialize-instance :before ((xml-object xml-object) &key document)
  (when document
    (restore-instance document xml-object)))

(defgeneric parse-string (type string)
  (:documentation "Parse a string into the given type"))

(defmethod parse-string ((type t) string)
  (warn "unknown type ~A specified" type)
  string)

(defmethod parse-string ((type (eql t)) string)
  string)

(defmethod parse-string ((type (eql 'string)) string)
  string)

(defmethod parse-string ((type (eql 'keyword)) string)
  (intern (string-upcase string) :keyword))

(defmethod parse-string ((type (eql 'integer)) string)
  (parse-integer string))

(defmethod parse-string ((type (eql 'float)) string)
  (parse-number:parse-number string))

(defmethod parse-string ((type (eql 'date-time)) string)
  (local-time:parse-timestring string))

(defun restore-instance (document xml-object)
  (dolist (slot-definition (closer-mop:class-slots (class-of xml-object)))
    (when (and (typep slot-definition 'xml-effective-slot-definition)
               (slot-boundp slot-definition 'xpath))
      (case (closer-mop:slot-definition-type slot-definition)
        ((string integer float)
         (when-let (string
                    (xpath-find-string document
                                       (slot-value slot-definition 'xpath)))
           (setf (slot-value xml-object (closer-mop:slot-definition-name slot-definition))
                 (parse-string (closer-mop:slot-definition-type slot-definition)
                               (string-trim '(#\Space #\Return #\LineFeed) string)))))
        (t
         (setf (slot-value xml-object (closer-mop:slot-definition-name slot-definition))
               (xpath:evaluate (slot-value slot-definition 'xpath) document))))))
  xml-object)

(defgeneric unparse-object (type object))

(defmethod unparse-object ((type t) object)
  (princ-to-string object))

(defmethod unparse-object ((type (eql 'string)) object)
  object)

(defmethod unparse-object ((type (eql 'date-time)) object)
  (local-time:format-rfc3339-timestring nil object))

(defmethod unparse-object ((type (eql 'keyword)) object)
  (string-downcase object))

(defun update-xpath-in-document (document xpath string)
  (flet
      ((update-node (node value)
         (etypecase node
           (stp:attribute
            (setf (stp:value node) value))
           (stp:element
            (stp:delete-children node)
            (stp:append-child node (stp:make-text value))))))
    (let ((nodes (xpath:all-nodes (xpath:evaluate xpath document))))
      (cond
        ((null nodes)
         ;; no node found, create it
         (let ((node document))
           (dolist (component (cl-ppcre:split #\/ xpath))
             (cond
               ((eql #\@ (char component 0))
                (setf (stp:attribute-value node (subseq component 1)) string)
                (return-from update-xpath-in-document document))
               (t
                (setf node (or (let ((children (stp:filter-children (stp:of-name component) node)))
                                 (when children
                                   (if (rest children)
                                       (error "more than one node unexpectedly matches sub-XPath ~S" xpath)
                                       (first children))))
                               (let ((child (stp:make-element component)))
                                 (stp:append-child node child)
                                 child))))))
           (update-node node string)))
        ((rest nodes)
         (error "more than one node unexpectedly matches XPath ~S" xpath))
        (t
         (update-node (first nodes) string)))))
  document)

(defmethod (setf closer-mop:slot-value-using-class)
    :after
    (object
     (class xml-class)
     instance
     (slot-definition xml-effective-slot-definition))
  (when (and (slot-boundp instance 'document)
             (slot-boundp slot-definition 'xpath))
    (update-xpath-in-document
     (document instance)
     (slot-value slot-definition 'xpath)
     (unparse-object (closer-mop:slot-definition-type slot-definition) object))))
