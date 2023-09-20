;;; rt.lisp --- macs.rt

;; Regression Testing framework. inspired by PCL, the original CMUCL
;; code, and the SBCL port.

;;; Commentary:

;; - :rt https://www.merl.com/publications/docs/TR91-04.pdf Chapter 1
;; - :com.gigamonkeys.test https://github.com/gigamonkey/monkeylib-test-framework
;; - :sb-rt https://github.com/sbcl/sbcl/blob/master/contrib/sb-rt/rt.lisp

;; This package is intended to provide a modernized Lisp testing
;; library with features found in some of the test frameworks listed
;; below.

;; - :it.bese.fiveam https://github.com/lispci/fiveam
;; - :try https://github.com/melisgl/try
;; - :rove https://github.com/fukamachi/rove

;;; TODO:
#|

- [ ] with-test

- [ ] with-test-env

- [ ] check macros

- [ ] fixtures

- [ ] sxp formatter

|#
;;; Code:
#+x86-64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-sprof))

(defpackage :macs.rt
  (:use
   :cl :sxp
   :sym :list :cond :readtables :fu :fmt :log :ana :pan :sb-aprof
   #+x86-64 :sb-sprof)
  (:nicknames :rt)
  (:export
   :*default-test-opts*
   :*test-debug*
   :*compile-tests*
   :*catch-test-errors*
   :*test-suffix*
   :*default-test-suite-name*
   :*test-suite*
   :*test-suite-list*
   ;;  TODO 2023-09-04: :*test-profiler-list* not yet
   :*testing*
   :test-suite-designator
   :check-suite-designator
   :make-test
   :make-suite
   :test-name=
   :with-test
   :do-test
   :do-tests
   :continue-testing
   :with-test-env
   :ensure-suite
   :test-fixture
   :fixture-prototype
   :make-fixture-prototype
   :make-fixture
   :with-fixture
   :test-result
   :test-pass-p
   :test-fail-p
   :test-skip-p
   :test-failed
   :fail!
   :is
   :signals
   :deftest
   :defsuite
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
   :test-results))

(in-package :macs.rt)
(in-readtable *macs-readtable*)

;;; Vars
(defvar *default-test-opts* '(optimize sb-c::instrument-consing))
(defvar *compile-tests* t
  "When nil do not compile tests. With a value of t, tests are compiled
with default optimizations else the value is used to configure
compiler optimizations.")
(defvar *catch-test-errors* t "When non-nil, cause errors in a test to be caught.")
(defvar *test-suffix* "-test" "A suffix to append to every `test' defined with `deftest'.")
(defvar *test-suite-list* nil "List of available `test-suite' objects.")
(defvar *test-suite* nil "A 'test-suite-designator' which identifies the current `test-suite'.")
(eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *default-test-suite-name* "default"))
(declaim (type (or stream boolean string) *test-input*))
(defvar *test-input* nil "When non-nil, specifies an input stream or buffer for `*testing*'.")
(declaim (type (or stream boolean) *test-debug*))
(defvar *test-debug* *log-level* "When non-nil, enable debug-mode for tests defined with `deftest'. The
value is actually treated as a stream-designator - so you can point
the debug output wherever you want.")

(defvar *testing* nil "Testing state var.")

;;; Utils
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-test (&rest slots)
    (apply #'make-instance 'test slots))
  (defun make-suite (&rest slots)
    (apply #'make-instance 'test-suite slots)))

(defmacro with-test ((arg test &rest slots) &body body)
  "Do BODY with ARG bound to `test-object' TEST."
  `(let ((,arg ,test)) ;; TODO: CoW?
     (with-slots ',slots ,test ,@body)))

;; TODO 2023-09-04: optimize
(declaim (inline do-tests))
(defun do-tests (&optional (suite *test-suite*) (output *standard-output*))
  (if (pathnamep output)
      (with-open-file (stream output :direction :output)
	(do-suite (ensure-suite suite) :stream stream))
      (do-suite (ensure-suite suite) :stream output)))

;; this assumes that *test-suite* is re-initialized correctly to the
;; correct test-suite object.
(defun continue-testing ()
  (if-let ((test *testing*))
    (throw '%in-test test)
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
(defun spush (item lst &key (test #'equal))
  "Substituting `push'"
  (declare (type function test))
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
(declaim (inline normalize-test-name))
(defun normalize-test-name (a)
  "Return the normalized `test-suite-designator' of A."
  (etypecase a
    (string a)
    (symbol (symbol-name a))
    (test-object (test-name a))
    (t (format nil "~A" a))))

(defun test-name= (a b)
  "Return t if A and B are similar `test-suite-designator's."
  (let ((a (normalize-test-name a))
	(b (normalize-test-name b)))
    (string= a b)))

(declaim (inline assert-suite ensure-suite))
(defun ensure-suite (name)
  (if-let ((ok (member name *test-suite-list* :test #'test-name=)))
    (car ok)
    (when (or (eq name t) (null name)) (make-suite :name *default-test-suite-name*))))

(defun check-suite-designator (suite) (check-type suite test-suite-designator))

(defun assert-suite (name)
  (check-suite-designator name)
  (assert (ensure-suite name)))

(declaim (inline test-opt-key-p test-opt-valid-p))
(defun test-opt-key-p (k)
  "Test if K is a `test-opt-key'."
  (member k '(:profile :save :stream)))

(defun test-opt-valid-p (f)
  "Test if F is a valid `test-opt' form. If so, return F else nil."
  (when (test-opt-key-p (car f))
    f))

;;; Conditions
(define-condition test-failed (error)
  ((reason :accessor fail-reason :initarg :reason :initform "unknown")
   (name :accessor fail-name :initarg :name)
   (form :accessor fail-form :initarg :form))
  (:documentation "Signaled when a test fails.")
  (:report (lambda (c s)
	     (format s "The following expression failed: ~S~%~A."
		     (fail-form c)
		     (fail-reason c)))))

;;; Protocol
(defgeneric eval-test (self)
  (:documentation "Eval a `test'."))

(defgeneric compile-test (self &key &allow-other-keys)
  (:documentation "Compile a `test'."))

(defgeneric locked-tests (self)
  (:documentation "Return a list of locked tests in `test-suite' object SELF."))

(defgeneric push-test (self place)
  (:documentation
   "Push `test' SELF to the value of slot ':tests' in `test-suite' object PLACE."))

(defgeneric pop-test (self)
  (:documentation
   "Pop the first `test' from the slot-value of ':tests' in `test-suite' object SELF."))

(defgeneric push-result (self place)
  (:documentation
   "Push object SELF to the value of slot ':results' in object PLACE."))

(defgeneric pop-result (self)
  (:documentation
   "Pop the first `test-result' from the slot-value of ':tests' from object SELF."))

(defgeneric delete-test (self &key &allow-other-keys)
  (:documentation "Delete `test' object specified by `test-object' SELF and optional keys."))

(defgeneric find-test (self name &key &allow-other-keys)
  (:documentation "Find `test' object specified by name and optional keys."))

(defgeneric do-test (self &optional test)
  (:documentation
   "Run `test' SELF, printing results to `*standard-output*'. The second
argument is an optional fixture.

SELF can also be a `test-suite', in which case the TESTS slot is
queried for the value of TEST. If TEST is not provided, pops the car
from TESTS."))

(defgeneric do-suite (self &key &allow-other-keys)
  (:documentation
   "Perform actions on `test-suite' object SELF with optional keys."))

(defgeneric get-test-opt (self key)
  (:documentation
   "Get the value of first cons where car is KEY in :opts slot of SELF."))

;;; Objects
(defclass test-object ()
  ((name :initarg :name :initform (required-argument) :type string :accessor test-name)
   #+nil (cached :initarg :cache :allocation :class :accessor test-cached-p :type boolean))
  (:documentation "Super class for all test-related objects."))

(defmethod print-object ((self test-object) stream)
  "test"
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A"
	    (test-name self))))

;;;; Tests
;; HACK 2023-08-31: inherit sxp?

(defclass test (test-object)
  ((fn :type symbol :accessor test-fn)
   (args :type list :accessor test-args :initform nil :initarg :args)
   (decl :type list :accessor test-decl :initform nil :initarg :decl)
   (form :initarg :form :initform nil :type function-lambda-expression :accessor test-form)
   (doc :initarg :doc :type string :accessor test-doc)
   (lock :initarg :lock :type boolean :accessor test-lock-p)
   (persist :initarg :persist :initform nil :type boolean :accessor test-persist-p))
  (:documentation "Test class typically made with `deftest'."))

(defmethod initialize-instance ((self test) &key name)
  (dbg! "building test" name)
  (setf (test-fn self)
	(make-symbol
	 (format nil "~A~A"
		 name
		 (gensym *test-suffix*))))
  (setf (test-lock-p self) t)
  (call-next-method))

(defmethod print-object ((self test) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A ~A :lock ~A :persist ~A"
	    (test-name self)
	    (test-form self)
	    (test-lock-p self)
	    (test-persist-p self))))

;; TODO 2023-09-01: use sxp?
;; (defun validate-form (form))

(defmethod eval-test ((self test))
  (funcall (lambda () (test-form self))))

(defmethod compile-test ((self test) &key declare &allow-other-keys)
   (compile
    (test-fn self)
    `(lambda ()
       ,@(when declare `((declare ,declare)))
       ,@(test-form self))))

(defun fail! (form &optional fmt &rest args)
  (let ((reason (and fmt (apply #'format nil fmt args))))
    (with-simple-restart (ignore-fail "Continue testing.")
      (error 'test-failed :reason reason :form form))))
  
(defmethod do-test ((self test) &optional fx)
  (declare (ignorable fx))
  (catch '%in-test ;; for `continue-testing' restart
    (setf (test-lock-p self) t)
    (let* ((*testing* (test-name self))
	   (bail nil)
	   r)
      (block bail
	(flet ((%do ()
		 (if-let ((opt *compile-tests*))
		   ;; RESEARCH 2023-08-31: with-compilation-unit?
		   (progn 
		     (when (eq opt t) (setq opt *default-test-opts*)) 
		     (funcall (compile-test self :declare opt))
		     (setf r(make-test-result :pass (format nil "#'~(~A~)" (test-fn self)))))
		   (progn
		     (eval-test self)
		     (setf r (make-test-result :pass (test-form self)))))))
	  (if *catch-test-errors*
	      (handler-bind
		  ((style-warning #'muffle-warning)
		   (error 
		     #'(lambda (c)
			 (setf bail t)
			 (setf r (make-test-result :fail c))
			 (return-from bail nil))))
		(%do))
	      (%do)))
      (setf (test-lock-p self) bail))
      r)))

;;;; Fixtures

;; Our fixtures are just closures - with a pandoric environment. You
;; might call it a domain-specific object protocol.

;; You can build fixtures inside a test or use the push/pop-fixture
;; methods on a `test-suite' object to use it from multiple.

(deftype fixture () 'form)

(declaim (inline %make-fixture-prototype))
(defstruct (fixture-prototype (:constructor %make-fixture-prototype)
			      (:conc-name fxp))
  (kind 'empty :type symbol)
  (form nil :type form))

(defun make-fixture-prototype (kind form)
  (%make-fixture-prototype :kind kind :form form))

(defmacro make-fixture (name largs pargs &body body)
  `(defun ,name () (plambda ,largs ,pargs ,@body)))

(defmacro with-fixture (pargs fx &body body)
  `(with-pandoric ',pargs ,fx ,@body))

;;;; Results
(deftype result-tag ()
  '(or (member :pass :fail :skip) null))

(declaim (inline %make-test-result))
(defstruct (test-result (:constructor %make-test-result)
			(:conc-name  tr-))
  (tag nil :type result-tag :read-only t)
  (form nil :type form))

(defun make-test-result (tag &optional form)
  (%make-test-result :tag tag :form form))

(defmethod test-pass-p ((res test-result))
  (when (eq :pass (tr-tag res)) t))

(defmethod test-fail-p ((res test-result))
  (when (eq :fail (tr-tag res)) t))

(defmethod test-skip-p ((res test-result))
  (when (eq :skip (tr-tag res)) t))

(defmethod print-object ((self test-result) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A"
	    (tr-tag self)
	    (tr-form self))))

;;;; Suites
(defclass test-suite (test-object)
  ((tests :initarg :set :initform nil :type list :accessor tests
	  :documentation "test-suite tests")
   (results :initarg :results :initform nil :type list :accessor test-results
	    :documentation "test-suite results")
   (stream :initarg :stream :initform *standard-output* :type stream :accessor test-stream)
   (opts :initarg :opts :initform nil :type list :accessor test-opts))
  (:documentation "A class for collections of related `test' objects."))

(defmethod print-object ((self test-suite) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A [~d:~d:~d:~d]"
	    (test-name self)
	    (length (tests self))
	    (count t (map-tests self #'test-lock-p))
	    (count t (map-tests self #'test-persist-p))
	    (length (test-results self)))))

;; (defmethod reinitialize-instance ((self test-suite) &rest initargs &key &allow-other-keys))

(deftype test-suite-designator ()
  "Either nil, a symbol, a string, or a `test-suite' object."
  '(or null symbol string test-suite test keyword))

(defmethod map-tests ((self test-suite) function)
  (mapcar function (tests self)))

(defmethod persistent-tests ((self test-suite))
  (do ((l (tests self) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (test-persist-p (car l))
      (push (test-name (car l)) r))))

(defmethod get-test-opt ((self test-suite) key)
  (declare (type keyword key))
  (assoc key (test-opts self)))

(defmethod push-test ((self test) (place test-suite))
  (push self (tests place)))

(defmethod pop-test ((self test-suite))
  (pop (tests self)))

(defmethod push-result ((self test-result) (place test-suite))
  (with-slots (results) place
    (push self results)))

(defmethod pop-result ((self test-suite))
  (pop (test-results self)))

(defmethod find-test ((self test-suite) name &key (test #'test-name=))
  (declare (type (or string symbol) name)
	   (type function test))
  (find name (the list (tests self)) :test test))

(defmethod do-test ((self test-suite) &optional test)
  (push-result 
   (if test
       (do-test (find-test self (test-name test)))
       (do-test (pop-test self)))
   self))

;; HACK 2023-09-01: find better method of declaring failures from
;; within the body of `deftest'.
(defmethod do-suite ((self test-suite) &key stream)
  (when stream (setf (test-stream self) stream))
  (with-slots (name opts stream) self
    (format stream "in suite ~x with ~A/~A tests:~%"
	    name
	    (count t (tests self)
		   :key #'test-lock-p)
	    (length (tests self)))
    ;; loop over each test, calling `do-test' if locked or persistent
    (dolist (i (tests self))
      (when (or (test-lock-p i) (test-persist-p i))
	(format stream "~@[~<~%~:;~:@(~S~) ~>~]"
		(push-result (do-test i) self))))
    ;; compare locked vs expected
    (let ((locked (remove-if #'null (map-tests self (lambda (x) (when (test-lock-p x) x)))))
	  (fails
	    ;; collect if locked test not expected
	    (loop for r in (test-results self)
		  unless (test-pass-p r)
		    collect r)))
      (print locked)
      (if (null locked)
	  (format stream "~&No tests failed.~%")
	  (progn
	    ;;  RESEARCH 2023-09-04: print fails ??
	    (format stream "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		    (length locked)
		    (length (tests self))
		    locked)
	    (unless (null fails)
	      (format stream "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		      (length fails)
		      fails))))
    ;; close stream
    (finish-output stream)
      ;; return values (PASS? LOCKED)
      (values (not fails) locked))))

;;; Checks
;; TODO 2023-09-05: 
(defmacro is (test &rest args)
  "The DWIM Check.

(is (= 1 1) :test 100) ;=> #S(TEST-RESULT :TAG :PASS :TEST (= 1 1))
If TEST returns a truthy value, return a PASS test-result, else return
a FAIL. The TEST is parameterized by ARGS which is a plist or nil.

If ARGS is nil, TEST is bound to to the RESULT slot of the test-result
and evaluated 'as-is'.

(nyi!)
ARGS may contain the following keywords followed by a corresponding
value:

:EXPECTED

:TIMEOUT

:THEN

All other values are treated as let bindings.
"
  (assert (formp test)
	  (test)
	  "TEST must be a form, not ~S" test)
  (flet ((%test (test)
	   `(if-let ((ok ,test))
	      (make-test-result :pass ',test)
	      (make-test-result :fail ',test))))
    `(if (null ,args)
	 ,(%test test)
	 (let* ((%ll ,(mapcar (lambda (x) `(,(symb (car x)) ,@(cdr x)))
			      (group args 2)))
		(%form (list 'let %ll ,test)))
	   (funcall ,#'%test %form)))))

(defmacro signals (condition-spec &body body)
  "Generates a passing TEST-RESULT if body signals a condition of type
CONDITION-SPEC. BODY is evaluated in a block named NIL, CONDITION-SPEC
is not evaluated."
  (let ((block-name (gensym)))
    (destructuring-bind (condition &optional reason-control &rest reason-args)
        (ensure-list condition-spec)
      `(block ,block-name
         (handler-bind ((,condition (lambda (c)
                                      ;; ok, body threw condition
				      ;; TODO 2023-09-05: result collectors
                                      ;; (add-result 'test-passed
                                      ;;            :test-expr ',condition)
                                      (return-from ,block-name (make-test-result :pass ',body)))))
           (block nil
             ,@body))
         (fail!
          ',condition
          ,@(if reason-control
                `(,reason-control ,@reason-args)
                `("Failed to signal a ~S" ',condition)))
         (return-from ,block-name nil)))))

;;; Macros
(defmacro deftest (name props &body body)
  "Build a test parameterized by LAMBDA-LIST. BODY is wrapped in
`with-test-env' and passed to `make-test' which returns a value based
on the dynamic environment."
  (destructuring-bind (pr doc dec fn)
      (multiple-value-bind (forms dec doc)
	  ;; parse body with docstring allowed
	  (sb-int:parse-body
	   (if (listp body) body t) t)
	`(',props ,doc ,dec ,forms))
    (declare (ignore pr))
    `(let ((obj (make-test
		 :name (format nil "~A" ',name)
		 ;; note: we could leave these unbound if we want,
		 ;; personal preference
		 :form ',fn
		 ,@(when doc `(:doc ,doc))
		 ,@(when dec `(:decl ,dec)))))
       (push-test obj *test-suite*)
       obj)))

(defmacro defsuite (suite-name &key (stream '*standard-output*) (opts nil))
    "Define a `test-suite' with provided keys. The object returned can be
enabled using the `in-suite' macro, similiar to the `defpackage' API."
  (check-type suite-name (or symbol string))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((obj (make-suite
		 :name (format nil "~A" ',suite-name)
		 :stream ,stream
		 :opts ',opts)))
       ;; TODO 2023-09-17: shouldn't need setf
       (setq *test-suite-list* (spush obj *test-suite-list* :test #'test-name=))
       obj)))

(defmacro in-suite (name)
  "Set `*test-suite*' to the `test-suite' referred to by symbol
NAME. Return the `test-suite'."
  (assert-suite name)
  `(setf *test-suite* (ensure-suite ',name)))
