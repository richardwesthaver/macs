;;; rt.lisp --- macs.rt

;; Regression Testing framework. inspired by PCL, the original CMUCL
;; code, and the SBCL port.

;; - :rt https://www.merl.com/publications/docs/TR91-04.pdf Chapter 1
;; - :com.gigamonkeys.test https://github.com/gigamonkey/monkeylib-test-framework
;; - :sb-rt https://github.com/sbcl/sbcl/blob/master/contrib/sb-rt/rt.lisp

;; This package is intended to provide a modernized Lisp testing
;; library with features found in some of the test frameworks listed
;; below.

;; - :it.bese.fiveam https://github.com/lispci/fiveam
;; - :try https://github.com/melisgl/try
;; - :rove https://github.com/fukamachi/rove

;;; Commentary:

;;; TODO:
#|

- [ ] with-test

- [ ] with-test-env

- [ ] is (assertions)

- [ ] fixtures

- [ ] sxp formatter

|#
;;; Code:
(defpackage :macs.rt
  (:use :cl :macs.sym :macs.cond :macs.readtables :macs.fu)
  (:export
   :*test-debug*
   :*test-debug-timestamp*
   :*compile-tests*
   :*catch-test-errors*
   :*test-suffix*
   :*test-suite*
   :*test-suite-list*
   :test-suites
   :*testing*
   :test-suite-designator
   :check-suite-designator
   :test-debug-timestamp-source
   :dbg!
   :make-test
   :with-test
   :do-test
   :do-tests
   :continue-testing
   :with-test-env
   :deftest
   :suite-name-eq
   :suite-name=
   :make-suite
   :defsuite
   :ensure-suite
   :assure-suite
   :in-suite
   :eval-test
   :compile-test
   :pending-tests
   :push-test
   :pop-test
   :delete-test
   :find-test
   :do-suite
   :test-object
   :test
   :test-fixture
   :test-suite
   :test-name
   :tests
   :test-fails))

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
(defparameter *test-debug-timestamp* t "If non-nil, print a timestamp with debug output. Has no effect when
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
       (if *test-debug-timestamp*
	   (format ,dbg " @ ~a ::~t" (test-debug-timestamp-source)))
       ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
       (map nil (lambda (x) (format ,dbg "~s " x)) ',args))))

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
		    (%do)))))
      (setf (test-lock test) (or (null (test-once test)) bail)))))

(defun do-tests (&optional (s *standard-output*))
  (if (streamp s)
      (do-suite *test-suite* s)
      (with-open-file (stream s :direction :output)
	(do-suite *test-suite* stream))))

(defun continue-testing ()
  (if-let ((test *testing*))
    (throw '*in-test* test)
    (do-suite *test-suite* *standard-output*)))
      
(defmacro with-test-env (env &body body)
  "Generate a test closure from ENV and BODY."
  ;; TODO 2023-08-31: test
  (in-readtable *macs-readtable*)
  (prog1
      `(lambda () ,@body)
    (in-readtable nil)))

;; NOTE 2023-09-01: `pushnew' does not return an indication of whether
;; place is changed - it returns place. This is functionally sound but
;; means that if we want to do something else in the event that place
;; is unchanged, we run into some friction, 
;; https://stackoverflow.com/questions/56228832/adapting-common-lisp-pushnew-to-return-success-failure
(defun spush (item lst &key (test #'eql))
  "Substituting `push'"
  (cond
    ((null lst) (push item lst))
    ((list lst)
     (if-let ((found (member item lst
			     :test test)))
       (progn
	 (rplaca found item)
	 lst)
       (push item lst)))
    #|(or nil '(t (cons item lst)))|#))

;; FIX 2023-08-31: spush, replace with `add-test' method.
(defmacro deftest (name &body body)
  "Build a test. BODY is wrapped in `with-test-env' and passed to
`make-test' which returns a value based on the dynamic environment."
  `(let ((obj (make-test :name ',name :form ',body)))
     (setf (tests (ensure-suite *test-suite*)) (spush obj (tests *test-suite*)))
     obj))

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

(defmacro defsuite (suite-name &key opts)
  "Define a `test-suite' with provided OPTS. The object returned can be
enabled using the `in-suite' macro, similiar to the `defpackage' API."
  (check-type suite-name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((obj (make-suite :name ',suite-name ,@opts)))
       (setf *test-suite-list* (spush obj *test-suite-list* :test #'suite-name=))
       obj)))

(declaim (inline assert-suite ensure-suite))
(defun ensure-suite (name)
  (if-let ((ok (member name *test-suite-list* :test #'suite-name=)))
    (car ok)
    (when (or (eq name t) (null name)) *default-suite*)))

(defun check-suite-designator (suite) (check-type suite test-suite-designator))

(defun assert-suite (name)
  (check-suite-designator name)
  (assert (ensure-suite name)))

(defmacro in-suite (name)
  "Set `*test-suite*' to the `test-suite' referred to by symbol
NAME. Return the `test-suite'."
    (assert-suite name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *test-suite* (ensure-suite ',name))))

(defgeneric eval-test (self &rest opts)
  (:documentation "Eval a `test'."))

(defgeneric compile-test (self &rest opts)
  (:documentation "Compile a `test'."))

(defgeneric pending-tests (self)
  (:documentation "Return a list of pending tests in `test-suite' object SELF."))

(defgeneric push-test (self place)
  (:documentation
   "Push `test' SELF to the value of slot ':tests' in `test-suite' object PLACE."))

(defgeneric pop-test (self)
  (:documentation
   "Pop the first `test' from the slot-value of ':tests' in `test-suite' object SELF."))

(defgeneric delete-test (self &rest opts)
  (:documentation "Delete `test' object specified by `test-object' SELF and OPTS."))

(defgeneric find-test (self &rest opts)
  (:documentation "Find `test' object specified by `test-object' SELF and OPTS."))

(defgeneric do-suite (self stream &rest opts)
  (:documentation
   "Perform actions on `test-suite' object SELF, with output to STREAM, modulo OPTS."))

(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string :accessor test-name))
  (:documentation "Super class for all test-related objects."))

(defmethod print-object ((self test-object) stream)
  "test"
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A"
	    (test-name self))))

;; HACK 2023-08-31: inherit sxp?

(defclass test (test-object)
  ;; RESEARCH 2023-09-02: should this be a string?
  ((function-symbol :type symbol :accessor test-function-symbol)
   (args :type list :accessor test-args :initform nil :initarg :args)
   (form :initarg :form :initform nil :type function-lambda-expression :accessor test-form)
   (lock :initarg :lock :type boolean :accessor test-lock)
   (once :initarg :once :initform nil :type boolean :accessor test-once))
  (:documentation "Test class typically made with `deftest'."))

(defmethod print-object ((self test) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A ~A :lock ~A :once ~A"
	    (test-name self)
	    (test-form self)
	    (test-lock self)
	    (test-once self))))

(declaim (inline make-test-function))
(defun make-test-function (sym)
  (symb sym (string-upcase *test-suffix*)))

;; TODO 2023-09-01: use sxp?
(defun validate-form (form)
  "")

(defmethod initialize-instance ((self test) &key keys)
  (dbg! "building test" self keys)
  (setf (test-function-symbol self) (make-test-function (get keys :name)))
  (setf (test-lock self) t)
  (call-next-method))

(defmethod eval-test ((self test) &rest opts)
  (eval (read (test-form self))))

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
   (fails :initarg :fails :initform nil :type list :accessor test-fails))
  (:documentation "A class for collections of related `test' objects."))

(defmethod print-object ((self test-suite) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A :tests ~A :fails ~A"
	    (test-name self)
	    (length (tests self))
	    (length (test-fails self)))))

(deftype test-suite-designator ()
  "Either a symbol or a `test-suite' object."
  '(or test-suite symbol boolean))

(defmethod pending-tests ((self test-suite))
  (do ((l (tests self) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (test-lock (car l))
      (push (test-name (car l)) r))))

;; HACK 2023-09-01: find better method of declaring failures from
;; within the body of `deftest'.
(defmethod do-suite ((self test-suite) stream &rest opts)
  (format stream "test [:~A]~%"
	  (count t (tests self)
		 :key #'test-lock))
  ;; loop over each test, calling `do-test' if lock=t
  (dolist (i (tests self))
    (when (test-lock i)
      (format stream "~@[~<~%~:;~:@(~S~) ~>~]"
	      (do-test i nil))))
  ;; compare pending vs expected
  (let ((pending (pending-tests self))
	;; TODO - consider test-fn param
	(expected (make-hash-table :test #'equal)))
    ;; TODO - depends on fail infrastructure
    (dolist (ex (test-fails self))
      ;; t isn't really a useful value to put here..
      ;; RESEARCH 2023-09-02: hashset
      (setf (gethash ex expected) t))
    ;; process fails
    (let ((fails
	    ;; collect if pending test not expected
	    (loop for p in pending
		  unless (gethash p expected)
		    collect p)))
      ;; if not pending
      (if (null pending)
	  ;; no pending tests
	  (format stream "~&No tests failed.")
	  ;; pending tests
	  (progn
	    ;; print fails
	    (format stream "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                  (length pending)
                  (length (tests self))
                  pending)
	    ;; if no fails
	    (if (null fails)
		;; no fails
		(format stream "~&No unexpected failures.")
		;; fails
		(when (test-fails self)
		  (format stream "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
			  (length fails)
			  fails)))))
      ;; close stream
      (finish-output stream)
      ;; return values (PASS FAIL TOTAL)
      (values (not fails) (not pending) pending))))

(defvar *default-suite* (defsuite default))

