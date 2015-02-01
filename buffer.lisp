;;;; buffer.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>
;;;;
;;;; Buffer class for a fixed-sized buffer of data
(in-package #:bytecurry.common)

(deftype %buffer-head-t () `(integer -1 ,array-dimension-limit))

(defclass buffer ()
  ((backing-vector :initarg :backing :type simple-array)
   (head :initform -1 :type %buffer-head-t :documentation "The start of stored data, or -1 if empty")
   (tail :initform 0 :type array-index
         :documentation "The end of stored data."))
  (:documentation "A circular buffer that can data can be read and written to."))

(defun make-buffer (&key (size 1024) (element-type t))
  (declare (array-length size) (type (or cons symbol) element-type))
  "Create a buffer for temporarily storing data."
  (make-instance 'buffer :backing (make-array size :element-type element-type)))

(declaim (inline buffer-element-type))
(defun buffer-element-type (buff)
  (declare (buffer buff))
  "Get the element type of the buffer."
  (array-element-type (slot-value buff 'backing-vector)))

(declaim (ftype (function (buffer) fixnum) buffer-limit)
         (inline buffer-limit))
(defun buffer-limit (buff)
  (declare (buffer buff))
  "Get the total available size of the buffer"
  (array-dimension (slot-value buff 'backing-vector) 0))

(defun buffer-available (buff)
  (declare (buffer buff))
  "Get the number of elements that are available to be read."
  (with-slots (head tail) buff
    (declare (%buffer-head-t head) (array-index tail))
    (cond ((< head 0 ) 0)
          ((= head tail) (buffer-limit buff))
          ((> tail head) (- tail head))
          (t (+ (- (buffer-limit buff) head) tail)))))

(declaim (inline buffer-remaining))
(defun buffer-remaining (buff)
  (declare (buffer buff))
  "Get the number of elements that can still be written before the buffer is full."
  (- (buffer-limit buff) (buffer-available buff)))

(declaim (inline buffer-empty-p))
(defun buffer-empty-p (buff)
  (declare (buffer buff))
  "Check if the buffer is full"
  (< (slot-value buff 'head) 0))

(declaim (inline buffer-full-p))
(defun buffer-full-p (buff)
  (declare (buffer buff))
  "Check if the buffer is empty"
  (= (slot-value buff 'head) (slot-value buff 'tail)))

(defun buffer-write-element (buff element)
  (declare (buffer buff))
  "Write a single element to the buffer. Returns t if successful, nil if the buffer was full."
  (when (not (buffer-full-p buff))
    (with-slots ((arr backing-vector) head tail) buff
      (when (< head 0)
        (setf head 0 tail 0))
      (setf (svref arr tail) element)
      (setf tail (mod (1+ tail) (array-dimension arr 0)))
    t)))

(defun buffer-read-element (buff &optional default)
  (declare (buffer buff) (optimize (speed 3)))
  "Read a single element from the buffer. If the buffer is empty returns default, and nil as
the second value."
  (if (buffer-empty-p buff)
      (values default nil)
      (with-slots ((arr backing-vector) head tail) buff
        (values (prog1 (svref arr head)
                  (setf head (mod (1+ head) (array-dimension arr 0)))
                  (when (= head tail) (setf head -1)))
                t))))

(defun buffer-write-sequence (buff seq &optional (start 0) (end (length seq)))
  (declare (buffer buff) (sequence seq) (array-index start end))
  "Write from a sequence of values into the buffer. START and END specify the start and end position
of the input sequence to read from. Returns the number of elements written."
  (with-slots ((arr backing-vector) head tail) buff
    (let ((amount-to-save (min (- end start) (buffer-remaining buff)))
          (arr-length (array-dimension arr 0)))
      (when (not (zerop amount-to-save))
        (when (< head 0)
          (setf head 0 tail 0))
        (iter (for element in-sequence seq with-index idx from start below (+ start amount-to-save))
              (setf (svref arr tail) element)
              (setf tail (mod (1+ tail) arr-length))
              (finally (return idx)))))))

(defun buffer-read-sequence (buff seq &optional (start 0) (end (length seq)))
  (declare (buffer buff) (sequence seq) (array-index start))
  "Read from a buffer into a sequence. START and END specify where to store the read data in the output sequence.
Returns the position of the first element of seq that wasn't updated. (>= end)"
  (with-slots ((arr backing-vector) head tail) buff
    (let ((amount-to-retrieve (min (- end start) (buffer-available buff)))
          (arr-length (array-dimension arr 0)))
      (when (not (zerop amount-to-retrieve))
        (iter (for idx from start below (+ start amount-to-retrieve))
              (setf (elt seq idx) (svref arr head))
              (setf head (mod (1+ head) arr-length))
              (finally (when (= head tail)
                         (setf head -1)))
              (finally (return idx)))))))

(defun buffer-clear (buff)
  (declare (buffer buff))
  "Clear the contents of the buffer."
  (with-slots (head tail) buff
    (setf head 0 tail 0)))
