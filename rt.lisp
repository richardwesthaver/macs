;;; rt.lisp --- macs.rt
;; regression testing library. inspired by PCL and the original CMUCL
;; code which currently resides at sbcl/contrib/sb-rt.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-rt))
(in-package :macs.rt)
(in-readtable *macs-readtable*)
(defvar *compile-tests* nil
  "When nil do not compile tests. With a value of t, tests are compiled
with default optimizations else the value is used to configure
compiler optimizations.")
(defvar *catch-test-errors* t "When non-nil, cause errors in a test to be caught.")
(defvar *test-suffix* "-test" "A suffix to append to every `test' defined with `deftest'.")
(defvar *test-suite-list* nil "List of available `test-suite' objects.")
(defvar *test-suite* nil "A 'test-suite-designator' which identifies the current `test-suite'.")
(defvar *test-debug* nil "When non-nil, enable debug-mode for tests defined with `deftest'. The
value is actually treated as a stream-designator - so you can point
the debug output wherever you want.")
(defvar *test-debug-timestamp* t "If non-nil, print a timestamp with debug output. Has no effect when
`*debug-tests*' is nil. The value may be a function in which case it
is used as the function value of `test-debug-timestamp-source'.")
(defvar *testing* nil "Testing state var.")

(deftype test-suite-designator ()
  "Either a symbol or a `test-suite' object."
  '(or test-suite symbol boolean))
    
(declaim (inline test-debug-timestamp-source))
(defun test-debug-timestamp-source ()
  (format nil "~f" (/ (get-internal-real-time) internal-time-units-per-second)))

;; TODO 2023-08-31: single format control string
(defmacro dbg! (&rest args)
  (with-gensyms (dbg)
    `(when-let ((,dbg *test-debug*))
       (format ,dbg "~%:DBG")
       (if ,*test-debug-timestamp*
	   (format ,dbg " @ ~a :: " (test-debug-timestamp-source)))
       ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
       (map nil (lambda (x) (format *test-debug* "~x~%" x)) ,@args))))

(defun make-test (&rest slots)
  (apply #'make-instance 'test slots))

(defun make-suite (&rest slots)
  (apply #'make-instance 'test-suite slots))

(defmacro with-test (test))

(defun do-test (test &optional (s *standard-output*))
  "Do TEST, printing results to S (default `*standard-output*')"
  (catch '*in-test*
    (setf (test-lock test) t)
    (let* ((*testing* (test-name test))
	   (bail nil)
	   r)
      (declare (ignorable bail))
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
      (with-open-stream (stream s)
	(do-suite *test-suite*))
      (with-open-file (stream s :direction :output)
	(do-suite *test-suite*))))

(defun continue-testing ()
  (if-let ((test *testing*))
    (throw '*in-test* test)
    (do-suite *test-suite*)))
      
(defmacro with-test-env (env &body body)
  "Generate a test closure from ENV and BODY."
  ;; TODO 2023-08-31: test
  (in-readtable *macs-readtable*)
  (prog1
      `(lambda () ,@body)
    (in-readtable nil)))

(defmacro deftest (name &body body)
  "Build a test. BODY is wrapped in `with-test-env' and passed to
`make-test' which returns a value based on the dynamic environment."
  `(make-test :name ',name :form ',body))

(defun normalize-test-name (a)
  "Return the normalized `test-suite-designator' of A."
  (etypecase a
    (test-suite (test-name a))
    ((string) (symb (string-upcase a)))
    (t (symb a))))

(defun suite-name= (a b)
  "Return t if A and B are similar `test-suite-designator's."
  (let ((a (normalize-test-name a))
	(b (normalize-test-name b)))
    (equal a b)))

(defmacro defsuite (name &key opts)
  "Define a `test-suite' with provided OPTS. The object returned can be
enabled using the `in-suite' macro, similiar to the `defpackage' API."
     (check-type `,name test-suite-designator)
  `(let ((obj (make-suite :name ',name ,@opts)))
     (if-let ((tail (member-if (lambda (x) (suite-name= ',name x)) *test-suite-list*)))
       (progn
	 (format t "redefining test-suite: ~A" ',name)
	 (if (consp tail)
	     (setf (car tail) obj)
	     (setf tail obj)))
       (push obj *test-suite-list*))))

(declaim (inline assert-suite ensure-suite))
(defun ensure-suite (name)
  (member name *test-suite-list* :test #'suite-name=))
(defun assert-suite (name)
  (check-type name test-suite-designator)
  (assert (ensure-suite name)))

(defmacro in-suite (name)
  "Set `*test-suite*' to the `test-suite' referred to by symbol
NAME. Return the `test-suite'."
  
  (assert-suite name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *test-suite* ',name)))

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
  (dbg! (print-object self nil)))

  ;; HACK 2023-08-31: inherit sxp?
(defclass test (test-object)
  ((function-name :type symbol)
   (args)
   (form :initarg :form :initform nil :type function-lambda-expression :accessor test-form)
   (lock :initform nil :initarg :lock :type boolean :accessor test-lock))
  (:documentation "Test class typically made with `deftest'."))

(defmethod eval-test ((self test) &rest opts)
  (eval `(progn ,@(test-form self))))

(defmethod compile-test ((self test) &rest opts)
  (compile
   nil
   `(lambda ()
      (declare (optimize ,@opts))
      ,@(test-form self))))

(defclass test-fixture (test-object)
  ()
  (:documentation "A generic wrapper around test fixtures. Our test
  fixtures are lexical environments which specialize on a `test-suite'."))

(defclass test-suite (test-object)
  ((tests :initarg :set :initform nil :type list :accessor tests)
   (should-fail :initarg :should-fail :initform nil :type list :accessor should-fail-tests))
  (:documentation "A class for collections of related `test' objects."))

(defmethod pending-tests ((self test-suite))
  (do ((l (cdr (tests self)) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (test-lock (car l))
      (push (test-name (car l)) r))))

(defmethod do-suite ((self test-suite) &rest opts)
  (format t "testing ~A / ~A.~%"
	  (count t (cdr (tests self))
		 :key #'test-lock)
	  (length (cdr (tests self))))
  (dolist (i (cdr (tests self)))
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
