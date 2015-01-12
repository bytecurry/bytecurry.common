;;;; bytecurry.common.asd
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(asdf:defsystem #:bytecurry.common
  :description "Describe bytecurry.common here"
  :author "Thayne McCombs <astrothayne@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
                #:mexpr
                #:cl-ppcre
                #:iterate
                #:cl-interpol
                #:cl-annot
                #:cl-syntax)
  :serial t
  :components ((:file "package")
               (:file "bytecurry.common")
               (:file "common-syntax")))
