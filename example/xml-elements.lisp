
(defpackage :xml-elements
  (:use #:common-lisp #:xml-class))

(in-package :xml-elements)

(defclass maxbondorder ()
  ((source :accessor source :xpath "attribute::source" :type string)
   (bond-order :accessor bond-order :xpath "text()" :type string))
  (:metaclass xml-class))

(defmethod process-node ((type (eql 'maxbondorder)) node)
  (make-instance 'maxbondorder :document node))

(defmethod process-node ((type (eql '(cons maxbondorder))) node)
  (xpath:map-node-set->list (lambda (x)
                              (process-node 'maxbondorder x))
                            node))

(defclass radius ()
  ((radius-type :accessor radius-type :xpath "attribute::type" :type string)
   (unit :accessor unit :xpath "attribute::unit" :type string)
   (value :accessor value :xpath "text()" :type float))
  (:metaclass xml-class))

(defmethod process-node ((type (eql 'radius)) node)
  (make-instance 'radius :document node))

(defmethod process-node ((type (eql '(cons radius))) node)
  (xpath:map-node-set->list (lambda (x)
                              (process-node 'radius x))
                            node))

(defclass radii ()
  ((source :accessor source :xpath "attribute::source" :type string)
   (radii :accessor radii :xpath "radius" :type (cons radius)))
  (:metaclass xml-class))

(defmethod process-node ((type (eql 'radii)) node)
  (make-instance 'radii :document node))

(defmethod process-node ((type (eql '(cons radii))) node)
  (xpath:map-node-set->list (lambda (x)
                              (process-node 'radii x))
                            node))

(defclass electronegativity ()
  ((source :accessor source :xpath "attribute::source" :type string))
  (:metaclass xml-class))

(defmethod process-node ((type (eql 'electronegativity)) node)
  (make-instance 'electronegativity :document node))

(defmethod process-node ((type (eql '(cons electronegativity))) node)
  (xpath:map-node-set->list (lambda (x)
                              (process-node 'electronegativity x))
                            node))

(defclass rgb ()
  ((red :accessor rgb :xpath "attribute::red" :type float)
   (blue :accessor rgb :xpath "attribute::blue" :type float)
   (green :accessor rgb :xpath "attribute::green" :type float))
  (:metaclass xml-class))

(defmethod process-node ((type (eql 'rgb)) node)
  (make-instance 'rgb :document (xpath:first-node node)))

(defclass element ()
  ((atomic-number :accessor atomic-number :xpath "attribute::atomicnumber"
                  :type integer-or-null)
   (id :accessor id :xpath "attribute::id" :type string)
   (name :accessor name :xpath "attribute::name" :type string)
   (group :accessor group :xpath "attribute::group" :type integer-or-null)
   (period :accessor period :xpath "attribute::period" :type integer-or-null)
   (radii-list :accessor radii-list :xpath "radii"
               :type (cons radii))
   (maxbondorder-list :accessor maxbondorder-list :xpath "maxbondorder"
                      :type (cons maxbondorder))
   (mass :accessor mass :xpath "mass" :type float-or-null)
   (electronegativity-list :accessor electronegativity-list :xpath "electronegativity"
                           :type (cons electronegativity))
   (rgb :accessor rgb :xpath "rgb" :type rgb))
  (:metaclass xml-class))

(defmethod process-node ((type (eql 'element)) node)
  (make-instance 'element :document node))

(defmethod process-node ((type (eql '(vector element))) node)
  (let ((elements (xpath:map-node-set->list
                   (lambda (x)
                     (process-node 'element x))
                   node)))
    (make-array (length elements) :initial-contents elements)))

(defclass elements ()
  ((element-vector :accessor element-vector :xpath "elements/element"
                 :type (vector element)))
  (:metaclass xml-class))

(defparameter *elements-document*
  (parse-file (merge-pathnames "example/elementdata.xml"
                               xml-class-config:*base-directory*)))

(defparameter *elements*
  (make-instance 'elements
                 :document *elements-document*))

(defparameter *element-by-atomic-number-hash*
  (let ((h (make-hash-table)))
    (map nil
         (lambda (element)
           (setf (gethash (atomic-number element) h)
                 element))
         (element-vector *elements*))
    h))

(defparameter *element-by-id-hash*
  (let ((h (make-hash-table :test 'equal)))
    (map nil
         (lambda (element)
           (setf (gethash (id element) h)
                 element))
         (element-vector *elements*))
    h))

