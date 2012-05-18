
(in-package :xml-class)

(defun parse (input)
  (cxml:parse input (stp:make-builder)))

(defun parse-file (input)
  (cxml:parse-file input (stp:make-builder)))

(defun serialize-string (element)
  (with-output-to-string (s)
    (stp:serialize element (cxml:make-character-stream-sink s))))

(defun remove-attribute (element name)
  (alexandria:when-let (attribute (stp:find-attribute-named element name))
    (stp:remove-attribute element attribute)))

(defun add-attribute (element name value)
  (stp:add-attribute element (stp:make-attribute value name)))

(defun xpath-find-string (element xpath &key default)
  (let ((node-set (xpath:evaluate xpath element)))
    (if (xpath:node-set-empty-p node-set)
        default
        (xpath:string-value node-set))))

(defun append-simple-element (parent name content)
  (let ((element (stp:make-element name)))
    (stp:append-child element (stp:make-text content))
    (stp:append-child parent element)))
