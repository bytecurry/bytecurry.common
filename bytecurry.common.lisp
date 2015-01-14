;;;; bytecurry.common.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(in-package #:bytecurry.common)

(defgeneric sget (object key &optional default)
  (:documentation "Generic get function. Provides a uniform way to
access properties of an object. The s stands for super, simple, or something else"))

(defmethod sget ((object hash-table) key &optional default)
  "sget implementation for hash-tables"
  (gethash key object default))

(defmethod sget ((object array) subscripts &optional _)
  (declare (ignore _))
  "sget implementation for arrays"
  (aref object subscripts))

(defmethod sget ((object list) key &optional default)
  "default implementation for lists. Assumes a plist"
  (getf object key default))

;;;For alists and list indices we need a wayt to specify a differnt
;;; key

(defclass %alist-ref () ((key :initarg :key)))
(defun alist-key (key)
  "Create a key to access values in alists with sget"
  (make-instance '%alist-ref :key key))
(defclass %idx-ref () ((idx :initarg :idx :type integer)))

(defun idx-key (idx)
  (declare (integer idx))
  "Create a key to access lists by index"
  (make-instance '%idx-ref :idx idx))

(defmethod sget ((object list) (idx %idx-ref) &optional _)
  (declare (ignore _))
  (elt object (slot-value idx 'idx)))

(defmethod sget ((object list) (key %alist-ref) &optional default)
  (or (cdr (assoc (slot-value key 'key) object)) default))

(defun rget (object &rest keys)
  "repeated get. Calls sget on object with each key in turn."
  (iter (for key in keys)
        (for current initially object then (sget current key))
        (finally (return current))))
