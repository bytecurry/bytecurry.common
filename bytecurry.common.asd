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
                #:cl-syntax
                #:trivial-gray-streams
                #:bordeaux-threads)
  :components ((:file "package")
               (:file "lists" :depends-on ("package"))
               (:file "sget" :depends-on ("package" "lists"))
               (:file "buffer" :depends-on ("package"))
               (:file "streams" :depends-on ("package" "buffer"))
               (:file "iter" :depends-on ("package"))
               (:file "common-syntax" :depends-on ("package"))))
