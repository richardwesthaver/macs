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
(defun make-test (env body))
(defmacro with-test (test))
(defun do-test (test))
(defmacro with-test-env (&body body))
(defmacro deftest (name &body body)
  "Build a test. BODY is wrapped in `with-test-env' and passed to
`make-test' which returns a value based on the dynamic environment."
  `())

(defclass test-object ()
  ()
  (:documentation "Super class for all test-related objects."))

(defclass test (test-object)
  ()
  (:documentation "Test class typically made with `deftest'."))

(defclass test-fixture (test-object)
  ()
  (:documentation "A generic wrapper around test fixtures. Our test
  fixtures are lexical environments which specialize on a `test-suite'."))

(defclass test-suite (test-object)
  ()
  (:documentation "A class for collections of related `test' objects."))
