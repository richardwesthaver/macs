;;; macs/alien.lisp --- foreign alien friends

;;; Code:
(defpackage :macs.alien
  (:use :cl :reexport :sb-vm :sb-alien :sb-ext :sb-c :macs.str :macs.sym :macs.fu)
  (:nicknames :alien)
  (:export
   :copy-c-string
   :foreign-int-to-integer :foreign-int-to-bool :bool-to-foreign-int
   :defbytes
   :u1 :u2 :u3 :u4 :u8 :u16 :u24 :u32 :u64 :u128
   :i2 :i3 :i4 :i8 :i16 :i24 :i32 :i64 :i128
   :f16 :f24 :f32 :f64 :f128))

(in-package :macs.alien)

(reexport-from :sb-vm
	       :include
	       '(:with-pinned-objects :with-pinned-object-iterator :with-code-pages-pinned
		 :sanctify-for-execution))

(defun copy-c-string (src dest &aux (index 0))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0)
            (setf (fill-pointer dest) index)
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

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

;;; Bytes
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
