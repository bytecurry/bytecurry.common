(in-package #:bytecurry.common)

(defun ninsert-after (lst idx item)
  (declare (list lst) (integer idx))
  "Insert item into lst after idx.
lst is a list.
Performs insertion in place."
  (push item (cdr (nthcdr idx lst))))

(defun insert (lst idx item)
  (declare (list lst) (integer idx))
  "Insert item into lst at position idx, non-destructively"
  (let ((result)
        (last))
    (iter (for i  below idx)
          (for (current . rest) on lst)
          (for new-cons = (cons current nil))
          (if result
              (psetf (cdr last) new-cons last new-cons)
              (setf result new-cons last new-cons))
          (finally (return
                     (if result
                         (progn
                           (setf (cdr last) (cons item rest))
                           result)
                         (cons item lst)))))))
