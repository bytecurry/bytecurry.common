;;;; package.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(defpackage #:bytecurry.common
  (:use #:cl)
  (:export #:sset
           #:rset))

(defpackage #:bytecurry.common.syntax
  (:documentation "Readtable for commonly used read macros. Includes the following syntax:
#n infix reader macro from mexpr
#? string interpolation macro from cl-interpol
@  annotations from cl-annot
#h new hash-table reader macro. Example: #h(:foo 1) => (plist-hash-table '(:foo 1))")
  (:nicknames #:common-syntax)
  (:use #:cl)
  (:import-from #:cl-syntax #:define-package-syntax #:use-syntax)
  (:import-from #:alexandria #:plist-hash-table #:hash-table-plist)
  (:export #:syntax
           #:print-hash-table
           #:enable-syntax))
