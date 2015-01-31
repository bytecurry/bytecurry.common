(in-package #:bytecurry.common)

;;; Begin sget/sset code

(defgeneric sget (object key &key &allow-other-keys)
  (:documentation "Generic get function. Provides a uniform way to
access properties of an object. The s stands for super, simple, or standard"))

(defgeneric sset (object key value &key &allow-other-keys)
  (:documentation "Generic set function. companion to %sget"))

(defsetf sget (object key &rest rest) (store)
  `(sset ,object ,key ,store ,@rest))

(defmacro define-sgetter (lambda-list place-expr &key declarations documentation)
  "Define a getter and setter for a place to use sget with"
  (with-unique-names (value)
    `(progn (defmethod sget ,lambda-list
              ,declarations
              ,documentation
              ,place-expr)
            (defmethod sset ,(insert lambda-list 2 value)
              ,declarations
              ,documentation
              (setf ,place-expr ,value)))))

(define-sgetter ((object standard-object) key &key &allow-other-keys)
    (slot-value object key)
  :documentation "Access a slot on a standard-object")

(define-sgetter ((object hash-table) key &key default)
    (gethash key object default))

(define-sgetter ((object array) subscripts &key &allow-other-keys)
    (aref object subscripts))

(define-sgetter ((object list) key &key default)
    (getf object key default)
  :documentation "default implementation for lists. Assumes a plist")

;;;For alists and list indices we need a wayt to specify a differnt
;;; key

(defclass %idx-ref () ((idx :initarg :idx :type integer)))
(defun idx-key (idx)
  (declare (integer idx))
  "Create a key to access lists by index"
  (make-instance '%idx-ref :idx idx))

(define-sgetter ((object list) (key %idx-ref) &key &allow-other-keys)
    (nth (slot-value key 'key) object)
  :documentation "Get element of list by index")

(defclass %alist-ref ()
  ((item :initarg :item)
   (key :initarg :key :type (or function symbol))
   (test :initarg :test :initform 'eql :type (or function symbol))))
(defun alist-key (item &key key (test 'eql))
  (declare (type (or function symbol) test key))
  "Create a key to access values in alists with sget"
  (make-instance '%alist-ref :item item :key key :test test))

(define-sgetter ((object list) (key %alist-ref) &key &allow-other-keys)
    (cdr (assoc (slot-value key 'item)
                object
                :key (slot-value key 'key)
                :test (slot-value key 'test)))
  :documentation "Get an element from an alist. Note that there is currently no
way to distinguish between a nil value and not found.")

(defun rget (object &rest keys)
  "repeated get. Calls sget on object with each key in turn."
  (iter (for key in keys)
        (for current initially object then (sget current key))
        (finally (return current))))
