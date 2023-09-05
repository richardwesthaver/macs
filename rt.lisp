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
#+x86-64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(defpackage :macs.rt
  (:use
   :cl
   :macs.sym :macs.cond :macs.readtables :macs.fu :sb-aprof
   #+x86-64 :sb-sprof)
  (:export
   :*test-debug*
   :*test-debug-timestamp*
   :*compile-tests*
   :*catch-test-errors*
   :*test-suffix*
   :*test-suite*
   :*test-suite-list*
   ;;  TODO 2023-09-04: :*test-profiler-list* not yet
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
   :locked-tests
   :push-test
   :pop-test
   :delete-test
   :find-test
   :get-test-opt
   :do-suite
   :test-object
   :test
   :test-fixture
   :test-suite
   :test-name
   :tests
   :test-fails
   :test-results))
swank::*current-debug-io*

(in-package :macs.rt)
(in-readtable *macs-readtable*)
(defvar *compile-tests* '(optimize sb-c::instrument-consing)
  "When nil do not compile tests. With a value of t, tests are compiled
with default optimizations else the value is used to configure
compiler optimizations.")
(defvar *catch-test-errors* t "When non-nil, cause errors in a test to be caught.")
(defvar *test-suffix* "-test" "A suffix to append to every `test' defined with `deftest'.")
(defvar *test-suite-list* nil "List of available `test-suite' objects.")
(defvar *test-suite* nil "A 'test-suite-designator' which identifies the current `test-suite'.")
(declaim (type (or stream boolean string) *test-input*))
(defvar *test-input* nil "When non-nil, specifies an input stream or buffer for `*testing*'.")
(declaim (type (or stream boolean) *test-debug*))
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

(defmacro with-test ((arg test &rest slots) &body body)
  "Do BODY with ARG bound to `test-object' TEST."
  `(let ((,arg ,test)) ;; TODO: CoW?
     (with-slots ',slots ,test ,@body)))

;; TODO 2023-09-04: optimize
(defun do-tests (&optional (s *standard-output*))
  (if (streamp s)
      (do-suite *test-suite* s)
      (with-open-file (stream s :direction :output)
	(do-suite *test-suite* :stream stream))))

(defun continue-testing ()
  (if-let ((test *testing*))
    (throw '*in-test* test)
    (do-suite *test-suite*)))
      
(defmacro with-test-env (env &body body)
  "Generate a test closure from ENV and BODY."
  ;; TODO 2023-08-31: test
  (in-readtable *macs-readtable*)
  (prog1
      `(lambda () (let ',env ,@body))
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

(defgeneric eval-test (self)
  (:documentation "Eval a `test'."))

(defgeneric compile-test (self &rest opts)
  (:documentation "Compile a `test'."))

(defgeneric locked-tests (self)
  (:documentation "Return a list of locked tests in `test-suite' object SELF."))

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

(defgeneric do-test (self)
  (:documentation "Run `test' SELF, printing results to `*standard-output*'."))

(defgeneric do-suite (self &rest opts)
  (:documentation
   "Perform actions on `test-suite' object SELF, with output to STREAM, modulo OPTS."))

(defgeneric get-test-opt (self key)
  (:documentation
   "Get the value of first cons where car is KEY in :opts slot of SELF."))

(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string :accessor test-name)
   #+nil (cached :initarg :cache :allocation :class :accessor test-cached-p :type boolean))
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
   (lock :initarg :lock :type boolean :accessor test-lock-p)
   (once :initarg :once :initform nil :type boolean :accessor test-once-p))
  (:documentation "Test class typically made with `deftest'."))

(declaim (inline make-test-function))
(defun make-test-function (sym)
  (symb sym (string-upcase *test-suffix*)))

(defmethod initialize-instance ((self test) &key keys)
  (dbg! "building test" self keys)
  (setf (test-function-symbol self) (make-test-function (get keys :name)))
  (setf (test-lock-p self) t)
  (call-next-method))

(defmethod print-object ((self test) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A ~A :lock ~A :once ~A"
	    (test-name self)
	    (test-form self)
	    (test-lock-p self)
	    (test-once-p self))))

;; TODO 2023-09-01: use sxp?
;; (defun validate-form (form))

(defmethod eval-test ((self test))
  (eval (read (test-form self))))

(defmethod compile-test ((self test) &rest opts)
  (compile
   nil
   `(lambda ()
      (declare (optimize ,@opts))
      ,@(test-form self))))

(defmethod do-test ((self test))
  (catch '*in-test* ;; for `continue-testing' restart
    (setf (test-lock-p self) t)
    (let* ((*testing* (test-name self))
	   (bail nil)
	   r)
      (block bail
	(setf r
	      (flet ((%do
		       ()
		       (if-let ((opt *compile-tests*))
			 (multiple-value-list
			  ;; RESEARCH 2023-08-31: with-compilation-unit?
			  (funcall (compile-test self opt)))
			 (multiple-value-list
			  (eval-test self)))))
		(if *catch-test-errors*
		    (handler-bind
			((style-warning #'muffle-warning)
			 (error #'(lambda (c)
				    (setf bail t)
				    (setf r (list c))
				    (return-from bail nil))))
		      (%do))
		    (%do)))))
      (setf (test-lock-p self) (or bail (not (test-once-p self))))
      r)))

(defclass test-fixture (test-object)
  ()
  (:documentation "A generic wrapper around test fixtures. Our test
  fixtures are lexical environments which specialize on a `test-suite'."))

(defclass test-suite (test-object)
  ((tests :initarg :set :initform nil :type list :accessor tests
	  :documentation "test-suite tests")
   (fails :initarg :fails :initform nil :type list :accessor test-fails
	  :documentation "test-suite failures")
   (results :initarg :results :initform nil :type list :accessor test-results
	    :documentation "test-suite results")
   (opts :initarg :opts :initform nil :type list :accessor test-opts))
  (:documentation "A class for collections of related `test' objects."))

(defmethod print-object ((self test-suite) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A :tests ~A :opts ~A"
	    (test-name self)
	    (length (tests self))
	    (test-opts self))))

(deftype test-suite-designator ()
  "Either nil, a symbol, a string, or a `test-suite' object."
  '(or null symbol string test-suite))

(defmethod locked-tests ((self test-suite))
  (do ((l (tests self) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (test-lock-p (car l))
      (push (test-name (car l)) r))))

(defun test-opt-key-p (k)
  "Test if K is a `test-opt-key'."
  (member k '(:profile :save :stream)))

(defun test-opt-valid-p (f)
  "Test if F is a valid `test-opt' form. If so, return F else nil."
  (when (test-opt-key-p (car f))
    f))

(defmethod get-test-opt ((self test-suite) key)
  (assoc key (test-opts self)))

;; HACK 2023-09-01: find better method of declaring failures from
;; within the body of `deftest'.
(defmethod do-suite ((self test-suite) &rest opts)
  ;; collect opts
  (setq opts (unless (null opts) (mapcar #'test-opt-valid-p opts)))
    (with-slots (opts) self
      (let ((stream (assoc :stream (test-opts self))))
	(format stream "testing [:~A]~%"
		(count t (tests self)
		       :key #'test-lock-p))

	;; loop over each test, calling `do-test' if locked
	(dolist (i (tests self))
	  (when (test-lock-p i)
	    (format stream "~@[~<~%~:;~:@(~S~) ~>~]"
		    (do-test i))))
	;; compare locked vs expected
	(let ((locked (locked-tests self))
	      ;; TODO - consider test-fn param
	      (expected (make-hash-table :test #'equal)))
	  ;; TODO - depends on fail infrastructure
	  (dolist (ex (test-fails self))
	    ;; t isn't really a useful value to put here..
	    ;; RESEARCH 2023-09-02: hashset
	    (setf (gethash ex expected) t))
	  ;; process fails
	  (let ((fails
		  ;; collect if locked test not expected
		  (loop for p in locked
			unless (gethash p expected)
			  collect p)))
	    (if (null locked)
		(format stream "~&No tests failed.")
		(progn
		  ;;  RESEARCH 2023-09-04: print fails ??
		  (format stream "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
			  (length locked)
			  (length (tests self))
			  locked)
		  (if (null fails)
		      (format stream "~&No unexpected failures.")
		      (when (test-fails self)
			;; print unexpected failures
			(format stream "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
				(length fails)
				fails)))))
      ;; close stream
      (finish-output stream)
      ;; return values (PASS FAIL TOTAL)
      (values (not fails) (not locked) locked))))))

(defvar *default-suite* (defsuite default))
