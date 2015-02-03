;;;; iter.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>
;;;;
;;;; extensions for iterate

(in-package #:bytecurry.common)

(defun %row-major-to-subscripts (dimensions row-major-index)
  (declare (list dimensions) (integer row-major-index))
  "Get the subscripts for the row-major index, in an array of the given dimesnions.
Dimensions should be reversed from the result of array-dimensions."
  (iter (with subscripts = nil)
        (for dim in dimensions)
        (initially (setq curr row-major-index))
        (for (values curr rem) = (truncate curr dim))
        (push rem subscripts)
        (finally (return subscripts))))

(defmacro row-major-to-subscripts (arr row-major-index)
  "Get the subscripts for the row-major indes into the array arr."
  `(%row-major-to-subscripts (reverse (array-dimensions ,arr)) ,row-major-index))

(defmacro-driver (FOR subs IN-ROW-MAJOR-SUBSCRIPTS arr-expr)
  "Iterate over valid subscripts for the given array."
  (let ((kwd (if generate 'generate 'for)))
    (with-unique-names (arr dims idx end)
      `(progn
         (with ,arr = ,arr-expr)
         (with ,idx = -1)
         (with ,dims = (reverse (array-dimensions ,arr)))
         (,kwd ,subs next (progn
                           (incf ,idx)
                           (when (>= ,idx ,end) (terminate))
                           (%row-major-to-subscripts ,dims ,idx)))))))


(defun %initial-subscripts (dims)
  "Get the subscripts for the first element of an array with dimensions of DIMS."
  (iter (for dim in dims)
        (when (= dim 0) (finish))
        (collect 0)
        (else (return nil))))

(defun %next-subscripts (dims current-subscripts)
  "Get the next subscripts for the given dimensions, in column-major order."
  (iter (for dim in dims)
        (for idx in current-subscripts)
        (with carry = t)
        (when carry
          (incf idx)
          (setq carry nil))
        (when (>= idx dim)
          (setq idx 0)
          (setq carry t))
        (collect idx)
        (finally (when carry (return nil)))))


(defmacro-driver (FOR subs IN-COLUMN-MAJOR-SUBSCRIPTS arr-expr)
  "Iterate over valid subscripts in column-major order."
  (let ((kwd (if generate 'generate 'for)))
    (with-unique-names (dims)
      `(progn
         (with ,dims = (array-dimensions ,arr-expr))
         (,kwd ,subs next (or (if-first-time
                               (%initial-subscripts ,dims)
                               (%next-subscripts ,dims ,subs))
                              (terminate)))))))


(defclause-sequence IN-ARRAY ROW-MAJOR-INDEX
  :access-fn 'row-major-aref
  :size-fn 'array-total-size
  :sequence-type 'array
  :element-type t
  :element-doc-string "Elements of an array of any rank."
  :index-doc-string "Row-Major index into the array. You can use row-major-to-subscripts to get the actual subscripts.")
