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

(defvar *test-debug* nil "When non-nil, enable debug-mode for tests defined with `deftest'. The
value is actually treated as a stream-designator - so you can point
the debug output wherever you want.")
(defvar *test-debug-timestamp* t "If non-nil, print a timestamp with debug output. Has no effect when
`*debug-tests*' is nil. The value may be a function in which case it
is used as the function value of `test-debug-timestamp-source'.")

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

(defun make-test (env body))
(defmacro with-test (test))
(defun do-test (test))
(defmacro with-test-env (&body body))
(defmacro deftest (name &body body)
  "Build a test. BODY is wrapped in `with-test-env' and passed to
`make-test' which returns a value based on the dynamic environment."
  `())
(defmacro defsuite (&rest opts)
  "Define a `test-suite' with provided OPTS. The object returned can be
enabled using the `in-suite' macro, similiar to the `defpackage' API.")

(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string))
  (:documentation "Super class for all test-related objects."))

(defmethod initialize-instance :after ((self test-object) &key)
  ;; - generate `:fn' slot-value (prepend `*test-suffix*')
  ;; - partial-eval of test environment
  ;; - trigger per-test debugging features
  (dbg! "initializing instance:"
	(print-object self nil)))

(defclass test (test-object)
  ((fn :type symbol)
   (args)
   )
  (:documentation "Test class typically made with `deftest'."))

(defclass test-fixture (test-object)
  ()
  (:documentation "A generic wrapper around test fixtures. Our test
  fixtures are lexical environments which specialize on a `test-suite'."))

(defclass test-suite (test-object)
  ((tests :initarg :tests :initform nil :type list))
  (:documentation "A class for collections of related `test' objects."))
