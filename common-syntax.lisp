;;;; common-syntax.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>
;;;;
;;;; Readtable for commonly used read macros.


(in-package #:bytecurry.common.syntax)

(defun print-hash-table (table &optional (stream *standard-output*))
  "Print a hashtable in a way that can be read back with hash-table-reader"
  (declare (hash-table table) (stream stream))
  (write-string "#H'" stream)
  (write (hash-table-plist table) :stream stream)
  table)

(locally
    ;;Ignore redefinition warning
    (declare #+sbcl (sb-ext:muffle-conditions sb-kernel:redefinition-with-defmethod))
    (defmethod print-object ((table hash-table) stream)
      "Override printing hash objects, in a way that is readable with the macro"
      (declare (stream stream))
      (if *print-readably*
          (error (make-condition 'print-not-readable :object table))
          (print-hash-table table stream))))

(defun hash-table-reader (stream sub-char numarg)
  "Reader macro function for reading a literal hash table.
If the first character after the sub-char is a #\', then treat it as a literal,
and create the hash-table at read-time."
  (declare (ignore sub-char numarg))
  (let ((props (read stream t nil t)))
    (if (= (first props) 'quote)
        (plist-hash-table (second props)))
        `(plist-hash-table (list ,@props))))

;;; Define the syntax
;;; The readtable defined is available as common-syntax:syntax
(define-package-syntax
  (:merge :standard mexpr:syntax)
  (:macro-char #\@ #'cl-annot.syntax:annotation-syntax-reader)
  (:dispatch-macro-char #\# #\? #'cl-interpol::interpol-reader)
  (:dispatch-macro-char #\# #\H #'hash-table-reader))

(defmacro enable-syntax ()
  "Enable all of the reader macros"
  `(use-syntax :bytecurry.common.syntax))

(defmacro defreadtable-with-syntax (name &rest options)
  "Convenience macro that creates a new named readtable that merges in
the common-syntax readtable"
  `(named-readtables:defreadtable ,name
     (:merge bytecurry.common.syntax:syntax)
     ,@options))
