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
(in-package :macs.rt)

(defsuite :macs)
(defsuite :macs.readtables)
(defsuite :macs.rt)
(in-suite :macs)

(deftest rt-results ()
  "Check for basic test-result false-positive/negatives."
  (is (test-pass-p 
       (is (= (+ 2 2) 4))))
  (is (when t t))
  (is (test-pass-p 
       (signals (error t) (test-form (make-instance 'test-result))))))

(deftest readtables ()
  "Test *macs-readtable* without cl-ppcre"
  (in-readtable *macs-readtable*))

#+cl-ppcre
(deftest ppcre-readtables (:persist t)
  "Test *macs-readtable* with cl-ppcre"
  (is (= 1 1)))

(deftest syms ()
  "Test MACS.SYM"
  ;; gensyms
  (with-gensyms (a b c)
    (let ((a 1) (b 1) (c 2))
      (is (and (= a b) (not (= c a))
	       (= (+ a b) (* c b))))))
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))

  (is (eq (ensure-symbol 'tests :macs.tests) 'tests))
  (is (eq 'macs.tests::foo (format-symbol :macs.tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz)))

(deftest cli ()
  "Test MACS.CLI"
  ;; prompts 
  (defvar tpfoo nil)
  (make-prompt! tpfoo "testing: ")
  (let ((*standard-input* (make-string-input-stream (format nil "~A~%~A~%" "foobar" "foobar"))))
    (is (string= (test-prompt) "foobar"))
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
