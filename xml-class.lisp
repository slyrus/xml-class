
(in-package :xml-class)

(defclass standard-xml-class ()
  ((node :initarg :node :reader node))
  (:documentation "Topmost object of every instance with the XML-CLASS metaclass")
  (:default-initargs :node nil))

(defclass xml-class (closer-mop:standard-class)
  ()
  (:documentation "Meta-class for classes associated with XML nodes"))

(defmethod closer-mop:validate-superclass ((sub xml-class)
                                           (super closer-mop:standard-class))
  t)

(defclass xml-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((xpath :reader xpath :initarg :xpath)
   (xml-type :reader xml-type :initarg :xml-type))
  (:default-initargs :xml-type nil))

(defclass xml-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((xpath :reader xpath :initarg :xpath)
   (xml-type :reader xml-type :initarg :xml-type))
  (:default-initargs :xml-type nil))

(defmethod initialize-instance :around
    ((class xml-class) &rest initargs
                       &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'standard-xml-class)))
      ;; 'standard-xml-class is already one of the (indirect) superclasses
      (call-next-method)
      
      ;; 'standard-xml-class is not one of the superclasses, so we have to add it
      (apply #'call-next-method
            class
            :direct-superclasses
            (append direct-superclasses
                    (list (find-class 'standard-xml-class)))
            initargs)))

(defmethod reinitialize-instance :around
  ((class xml-class) &rest initargs
                     &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses
                thereis (subtypep class (find-class 'standard-xml-class)))
          (call-next-method)
          (apply #'call-next-method
                 class
                 :direct-superclasses
                 (append direct-superclasses
                         (list (find-class 'standard-xml-class)))
                 initargs))
      
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method)))

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

;;;
;;; Now back to STANDARD-XML-CLASS

(defmethod initialize-instance :before ((standard-xml-class standard-xml-class) &key node)
  (when node
    (restore-instance node standard-xml-class)))

(defgeneric process-node (type node)
  (:documentation "Parse a string into the given type"))

(defmethod process-node ((type t) node)
  (warn "process-node: unknown type ~A specified" type)
  node)

(defmethod process-node ((type (eql t)) node)
  node)

(defmethod process-node ((type (eql 'string)) node)
  (xpath:string-value node))

(defmethod process-node ((type (eql 'keyword)) node)
  (intern (string-upcase (xpath:string-value node)) :keyword))

(defmethod process-node ((type (eql 'integer)) node)
  (parse-integer (xpath:string-value node)))

(deftype integer-or-null () '(or integer null))

(defmethod process-node ((type (eql 'integer-or-null)) node)
  (let ((string (xpath:string-value node)))
    (when (and string (plusp (length string)))
      (parse-integer string))))

(deftype float-or-null () '(or float null))

(defmethod process-node ((type (eql 'float)) node)
  (parse-number:parse-number (xpath:string-value node)))

(defmethod process-node ((type (eql 'float-or-null)) node)
  (let ((string (xpath:string-value node)))
    (when (and string (plusp (length string)))
      (parse-number:parse-number string))))

(defmethod process-node ((type (eql 'date-time)) node)
  (local-time:parse-timestring (xpath:string-value node)))

(defun restore-instance (node standard-xml-class)
  (dolist (slot-definition (closer-mop:class-slots (class-of standard-xml-class)))
    (when (and (typep slot-definition 'xml-effective-slot-definition)
               (slot-boundp slot-definition 'xpath))
      (setf (slot-value standard-xml-class (closer-mop:slot-definition-name slot-definition))
            (process-node (closer-mop:slot-definition-type slot-definition)
                          (xpath:evaluate (slot-value slot-definition 'xpath) node)))))
  standard-xml-class)

;;; Updating XML objects. This is totally untested.
(defgeneric unparse-object (type object))

(defmethod unparse-object ((type t) object)
  (princ-to-string object))

(defmethod unparse-object ((type (eql 'string)) object)
  object)

(defmethod unparse-object ((type (eql 'date-time)) object)
  (local-time:format-rfc3339-timestring nil object))

(defmethod unparse-object ((type (eql 'keyword)) object)
  (string-downcase object))

(defun update-xpath-in-node (node xpath string)
  (flet
      ((update-node (node value)
         (etypecase node
           (stp:attribute
            (setf (stp:value node) value))
           (stp:element
            (stp:delete-children node)
            (stp:append-child node (stp:make-text value))))))
    (let ((nodes (xpath:all-nodes (xpath:evaluate xpath node))))
      (cond
        ((null nodes)
         ;; no node found, create it
         (let ((node node))
           (dolist (component (cl-ppcre:split #\/ xpath))
             (cond
               ((eql #\@ (char component 0))
                (setf (stp:attribute-value node (subseq component 1)) string)
                (return-from update-xpath-in-node node))
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
  node)

(defmethod (setf closer-mop:slot-value-using-class)
    :after
    (object
     (class xml-class)
     instance
     (slot-definition xml-effective-slot-definition))
  (when (and (slot-boundp instance 'node)
             (slot-boundp slot-definition 'xpath))
    (update-xpath-in-node
     (node instance)
     (slot-value slot-definition 'xpath)
     (unparse-object (closer-mop:slot-definition-type slot-definition) object))))

