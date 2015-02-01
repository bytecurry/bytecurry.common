;;;; streams.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>
;;;;
;;;; Piped streams
(in-package #:bytecurry.common)

(defclass piped-stream (fundamental-binary-stream fundamental-input-stream fundamental-output-stream)
  ((buffer :initarg :buffer :accessor piped-stream-buffer
            :type buffer
            :documentation "The buffer that backs the stream")
   (lock :initform (make-lock))
   (read-condition :initform (make-condition-variable))
   (write-condition :initform (make-condition-variable)))
  (:documentation "A bidirection stream that pipes data from its input to its
output. To avoid deadlocks it should be read from a differnt thread than it is written to."))

(defun make-piped-stream (&key (size 1024))
  "Create a piped-stream.
SIZE specifies the size of the underlying buffer, and defaults to 1024."
  (make-instance 'piped-stream :buffer (make-buffer :size size :element-type '(unsigned-byte 8))))

(defmethod stream-element-type ((stream piped-stream))
  "Get the underlying element type of the stream"
  '(unsigned-byte 8))

(defmethod stream-read-byte ((stream piped-stream))
  (with-slots (buffer lock read-condition write-condition) stream
    (with-lock-held (lock)
      (iter (while (buffer-empty-p buffer))
            (unless (open-stream-p stream)
              (return-from stream-read-byte :eof))
            (condition-wait read-condition lock))
      (prog1 (buffer-read-element buffer)
        (condition-notify write-condition))))) ; notify writers that there is space

(defmethod stream-write-byte ((stream piped-stream) byte)
  (%pipe-check-open stream)
  (with-slots (buffer lock read-condition write-condition) stream
    (with-lock-held (lock)
      (iter (while (buffer-full-p buffer))
            (condition-wait write-condition lock))
      (buffer-write-element buffer byte)
      (condition-notify read-condition)))) ; notify readers that there is data available

(defmethod stream-read-sequence ((stream piped-stream) seq start end &key &allow-other-keys)
  (with-slots (buffer lock read-condition write-condition) stream
    (with-lock-held (lock)
      (iter (initially (setf pos start))
            (for pos next (buffer-read-sequence buffer seq pos end))
            (for prev previous pos initially start)
            (when (/= prev pos)
              (condition-notify write-condition)) ; notify writers of space available
            (while (and (open-stream-p stream) (< pos end)))
            (condition-wait read-condition lock) ; wait for content to read
            (finally (return pos))))))

(defmethod stream-write-sequence ((stream piped-stream) seq start end &key &allow-other-keys)
  (%pipe-check-open stream)
  (with-slots (buffer lock read-condition write-condition) stream
    (with-lock-held (lock)
      (iter (initially (setf pos start))
            (for pos next (buffer-write-sequence buffer seq pos end))
            (for prev previous pos initially start)
            (when (/= prev pos)
              (condition-notify read-condition)) ; notify readers of content available
            (while (< pos end))
            (condition-wait write-condition lock)))) ; wait for space to write
  seq)

(defmethod stream-clear-input ((stream piped-stream))
  "Clear the buffer. clear-output does the same thing"
  (buffer-clear (slot-value stream 'buffer)))

(defmethod stream-clear-output ((stream piped-stream))
  "Clear the buffer. clear-input does the same thing"
  (buffer-clear (slot-value stream 'buffer)))

(defun %pipe-check-open (stream)
  (declare (piped-stream stream))
  "Raise an end-of-file condition."
  (unless (open-stream-p stream)
    (cerror "Write data anyway."
            (make-condition 'end-of-file :stream stream))))
