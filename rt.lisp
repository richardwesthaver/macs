;;; rt.lisp --- macs.rt
;; regression testing library. inspired by PCL and the original CMUCL
;; code which currently resides at sbcl/contrib/sb-rt.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-rt))
(in-package :macs.rt)

(defvar *compile-tests* nil
  "When nil do not compile tests. With a value of t, tests are compiled
with default optimizations else the value is used to configure
compiler optimizations.")
(defvar *catch-test-errors* t "When non-nil, cause errors in a test to be caught.")
(defvar *test-suffix* "-test" "A suffix to append to every `test' defined with `deftest'.")
(defvar *test-suites* nil "List of available `test-suite' objects.")
(defvar *active-test-suite* nil "A 'test-suite-designator' which identifies the current `test-suite'.")
(defvar *test-debug* nil "When non-nil, enable debug-mode for tests defined with `deftest'. The
value is actually treated as a stream-designator - so you can point
the debug output wherever you want.")
(defvar *test-debug-timestamp* t "If non-nil, print a timestamp with debug output. Has no effect when
`*debug-tests*' is nil. The value may be a function in which case it
is used as the function value of `test-debug-timestamp-source'.")
(defvar *testing* nil "Testing state var.")

(declaim (inline test-debug-timestamp-source))
(defun test-debug-timestamp-source ()
  (format nil "~f" (/ (get-internal-real-time) internal-time-units-per-second)))

;; TODO 2023-08-31: single format control string
(defmacro dbg! (&rest args)
  (with-gensyms (dbg)
    `(when-let ((,dbg *test-debug*))
       (format ,dbg ":DBG")
       (if ,*test-debug-timestamp*
	   (format ,dbg " @ ~a~%" (test-debug-timestamp-source))
	   (terpri ,dbg))
       ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
       (map nil (lambda (x) (format *test-debug* " ~x~%" x)) ',args))))

(defun make-test (&rest slots)
  (apply #'make-instance 'test slots))

(defmacro with-test (test))

(defun do-test (test &optional (s *standard-output*))
  "Do TEST, printing results to S (default `*standard-output*')"
  (catch '*in-test*
    (setf (test-lock test) t)
    (let* ((*testing* (test-name test))
	   (bail nil)
	   r)
      (block bail
	(setf r
	      (flet ((%do
		       ()
		       (if-let ((opt *compile-tests*))
			 (multiple-value-list
			  ;; RESEARCH 2023-08-31: with-compilation-unit?
			  (funcall (compile-test test opt)))
			 (multiple-value-list
			  (eval-test test)))))
		(if *catch-test-errors*
		    (handler-bind
			((style-warning #'muffle-warning)
			 (error #'(lambda (c)
				    (setf bail t)
				    (setf r (list c))
				    (return-from bail nil))))
		      (%do))
		    (%do))))))))

(defun do-tests (&optional (s *standard-output*))
  (if (streamp s)
      (do-entries s)
      (with-open-file (stream s :direction :output)
	(do-suite *active-test-suite*))))

(defun continue-testing ()
  (if-let ((test *testing*))
    (throw '*in-test* test)
    (do-suite)))
      
(defmacro with-test-env (&body body))

(defmacro deftest (name &body body)
  "Build a test. BODY is wrapped in `with-test-env' and passed to
`make-test' which returns a value based on the dynamic environment."
  `(make-test :name ,name))

(defmacro defsuite (&rest opts)
  "Define a `test-suite' with provided OPTS. The object returned can be
enabled using the `in-suite' macro, similiar to the `defpackage' API.")

(defgeneric eval-test (self &rest opts))
(defgeneric compile-test (self &rest opts))
(defgeneric pending-tests (self))
(defgeneric add-test (self))
(defgeneric delete-test (self))
(defgeneric find-test (self))
(defgeneric do-suite (self &rest opts))

(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string :accessor test-name))
  (:documentation "Super class for all test-related objects."))

(defmethod initialize-instance :after ((self test-object) &key)
  ;; - generate `:function-name' slot-value (prepend `*test-suffix*')
  ;; - partial-eval of test environment
  ;; - trigger per-test debugging features
  (dbg! "initializing instance:"
	(print-object self nil)))

  ;; HACK 2023-08-31: inherit sxp?
(defclass test (test-object)
  ((function-name :type symbol)
   (args)
   (form :initform nil :type function-lambda-expression)
   (lock :initform nil :initarg :lock :type boolean :accessor test-lock))
  (:documentation "Test class typically made with `deftest'."))

(defmethod eval-test ((self test) &rest opts))
(defmethod compile-test ((self test) &rest opts))

(defclass test-fixture (test-object)
  ()
  (:documentation "A generic wrapper around test fixtures. Our test
  fixtures are lexical environments which specialize on a `test-suite'."))

(defclass test-suite (test-object)
  ((tests :initarg :set :initform nil :type list :accessor tests)
   (should-fail :initarg :should-fail :initform nil :type list :accessor should-fail-tests))
  (:documentation "A class for collections of related `test' objects."))

(defmethod pending-tests ((self test-suite))
  (do ((l (cdr (test-set self)) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (test-lock (car l))
      (push (test-name (car l)) r))))

(defmethod do-suite ((self test-suite) &rest opts)
  (format t "testing ~A / ~A.~%"
	  (count t (cdr (test-set self))
		 :key #'test-lock)
	  (length (cdr (test-set self))))
  (dolist (i (cdr (test-set self)))
    (when (test-lock i)
      (format t "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-test i t))))
  (let ((pending (pending-tests self))
	(expected (make-hash-table :test #'equal)))
    (dolist (ex (should-fail-tests self))
      (setf (gethash ex expected) t))
    (let ((fails
	    (loop for p in pending
		  unless (gethash p expected)
		    collect p)))
      (if (null pending)
	  (format t "~&No tests failed.")
	  (progn
	    (format t "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                  (length pending)
                  (length (cdr (tests self)))
                  pending)
	    (if (null fails)
		(format t "~&No unexpected failures.")
		(when (should-fail-tests self)
		  (format t "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
			  (length fails)
			  fails)))))
      (finish-output t)
      (values (null fails) (null pending) pending))))
