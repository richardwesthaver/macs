;;; macs/ffi.lisp --- FFI utils
(in-package :macs.alien)

(defun foreign-int-to-integer (buffer size)
  "Check SIZE of int BUFFER. return BUFFER."
  (assert (= size (sb-alien:alien-size sb-alien:int :bytes)))
  buffer)

(defun foreign-int-to-bool (x size)
  (if (zerop (foreign-int-to-integer x size))
      nil
      t))

(defun bool-to-foreign-int (val)
  (if val 1 0))

(defmacro defbytes (&body bitsets)
  "For each cons-cell in BITSETS, define a new CAR-byte type for each
member of CDR."
  `(loop for set in ',bitsets
	 collect
	 (let* ((ty (car set))
		(pfx
		  (cond
		    ((eq 'signed-byte ty) "I")
		    ((eq 'unsigned-byte ty) "U")
		    ((eq 'float ty) "F")
		    (t (subseq (symbol-name ty) 0 1))))
		(nums (cdr set))
		r) ;result
	   (setf r
		 (mapc
		  (lambda (x)
		    `(deftype ,(symbolicate pfx (format 'nil "~a" x)) ()
		       (cons ,ty ,x)))
		       nums))
	   (cons ty r))))

(defbytes
  (unsigned-byte 1 2 3 4 8 16 24 32 64 128)
  (signed-byte 2 3 4 8 16 24 32 64 128)
  (float 16 24 32 64 128))
