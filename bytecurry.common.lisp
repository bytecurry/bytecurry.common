;;; common.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>
;;;
;;; Common utilities
(in-package #:bytecurry.common)

(defun alias (new-name fn)
  (declare (symbol new-name) (type (or symbol function) fn))
  "Create an alias for a function.
This is a function which modifies the symbol-function of new-name. Therefore the
alias is created at runtime, and changes the global state even with another function."
  (when (symbolp fn) (setq fn (symbol-function fn))) ;Get the function if a symbol was used
  (setf (symbol-function new-name) fn))

(defun macro-alias (new-name macro-name &optional env)
  (declare (symbol new-name macro-name))
  "Create an alias for a macro.
This is like ALIAS, but for macros.
It probably isn't very useful unless run at compile-time."
  (setf (macro-function new-name) (macro-function macro-name env)))

(defmacro defalias (new-name fn)
  (declare (symbol new-name fn))
  "Define an alias to a function."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (alias ,new-name ,fn)))

(defmacro define-macro-alias (new-name macro-name &environment env)
  (declare (symbol new-name macro-name))
  "Define an alias for a macro."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (macro-alias ',new-name ',macro-name ,env)))
