;;; tests.lisp --- macs system tests

;;; Code:
(defpackage :macs.tests
  (:use
   :cl
   :macs.readtables
   :macs.reexport
   :macs.str
   :macs.sym
   :macs.list
   :macs.cond
   :macs.fu
   :macs.ana
   :macs.pan
   :macs.fs
   :macs.cli
   :macs.alien
   :macs.rt)
  (:export :run-tests))
(in-package :macs.tests)

(defsuite :macs)
(defsuite :macs.readtables)
(defsuite :macs.rt)
(in-suite :macs)

(deftest rt-fixtures ()
  (is (typep (make-fixture-prototype :empty nil)  'fixture-prototype))
  (is (typep (make-fixture tfix () () t) 'function))
  (let ((fx1 (make-fixture fx1 () (a b c) (setq a 1 b 2 c 3))))
    (is (with-fixture (a b c) fx1 (not (member 'nil (mapcar #'= (list 1 2 3) `(,a ,b ,c))))))))

(deftest rt-results ()
  "Check for basic test-result false-positive/negatives."
  (is (test-pass-p 
       (is (= (+ 2 2) 4))))
  (is (when t t))
  (is (test-pass-p 
       (signals (error t) (test-form (make-instance 'test-result))))))

(deftest readtables ()
  "Test *macs-readtable* without cl-ppcre"
  (is (progn (in-readtable *macs-readtable*) (typep #`(,a1 ,a1 ',a1 ,@a1) 'function))))

#+cl-ppcre
(deftest ppcre-readtables (:persist t)
  "Test *macs-readtable* with cl-ppcre"
  (is (= 1 1)))

(deftest sym ()
  "Test MACS.SYM"
  ;; gensyms
  (with-gensyms (a b c)
    (let ((a 1) (b 1) (c 2))
      (is (and (= a b) (not (= c a))
	       (= (+ a b) (* c b)))))
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq (ensure-symbol 'tests :macs.tests) 'tests))
  (is (eq 'macs.tests::foo (format-symbol :macs.tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz))))

;;; TODO
(deftest str (:disable t)
  "Test MACS.STR")

(deftest list (:disable t)
  "Test MACS.STR")

(deftest log (:disable t)
  "Test MACS.STR")

(deftest cond (:disable t)
  "Test MACS.STR")

(deftest reexport (:disable t)
  "Test MACS.STR")

(deftest thread (:disable t)
  "Test MACS.STR")

(deftest alien (:disable t)
  "Test MACS.STR")

(deftest fmt (:disable t)
  "Test MACS.STR")

(deftest fu (:disable t)
  "Test MACS.STR")

(deftest ana (:disable t)
  "Test MACS.STR")

(deftest pan (:disable t)
  "Test MACS.STR")

;; we should be able to call this from the body of the test, but we
;; get an undefined-function error for 'MACS.RT::MAKE-PROMPT!' -
;; package namespacing issue.
(defvar tpfoo nil)
(make-prompt! tpfoo "testing: ")

(deftest cli ()
  "Test MACS.CLI"
  ;; prompts 
  (let ((*standard-input* (make-string-input-stream (format nil "~A~%~A~%" "foobar" "foobar"))))
    (is (string= (tpfoo-prompt) "foobar"))
    (defvar tcoll nil)
    (defvar thist nil)
    (is (string= "foobar"
		 (cli:completing-read "nothing: " tcoll :history 'thist :default "foobar"))))
  ;; args
  (is (eq (cli:make-shorty "test") #\t))
  (defvar %opts (cli:make-opts '(:name foo :global t :description "bar")
			       '(:name bar :description "foo")))
  (defvar %cmds (cli:make-cmds `(:name baz :description "baz"
				      :opts ,%opts)))
  (is (and %opts %cmds)))

#+nil (test-results *test-suite*)
#+nil (do-tests :macs)
