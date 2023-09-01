;;; macs/ffi.lisp --- FFI utils
(in-package :macs.ffi)

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
		       (list ,(symbolicate ty '-byte) ,x)))
		       nums))
	   (cons ty r))))

(defbytes
  (unsigned 1 2 4 8 16 24 32 64 128)
  (signed 2 4 8 16 32 64 128)
  (float 16 32 64 128))
