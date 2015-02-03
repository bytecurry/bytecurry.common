;;;; package.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(defpackage #:bytecurry.common
  (:use #:cl #:alexandria #:iterate #:bordeaux-threads #:trivial-gray-streams)
  (:export #:define-sgetter
           #:sget
           #:sset
           #:rget
           ;;lists
           #:ninsert-after
           #:insert
           ;; buffers
           #:buffer
           #:make-buffer
           #:buffer-element-type
           #:buffer-limit #:buffer-available #:buffer-remaining
           #:buffer-empty-p #:buffer-full-p
           #:buffer-write-element #:buffer-read-element
           #:buffer-write-sequence #:buffer-read-sequence
           #:buffer-clear
           ;;streams
           #:piped-stream
           #:make-piped-stream
           ;;iterate
           #:in-array
           #:row-major-index
           #:in-row-major-subscripts #:in-column-major-subscripts
           #:row-major-to-subscripts
           ))

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
           #:enable-syntax
           #:defreadtable-with-syntax))
