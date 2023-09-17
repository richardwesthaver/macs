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
(in-suite :macs)

(deftest rt-results ()
  "Check for basic test-result false-positive/negatives."
  (is (test-fail-p (is (eq '1 (not '1)))))
  (is (test-pass-p (is (= (+ 2 2) 4))))
  (is (when t t)))

(deftest readtables (:persist t) 
  "Test *macs-readtable* without cl-ppcre"
  (in-readtable *macs-readtable*))

#+cl-ppcre
(deftest ppcre-readtables (:persist t)
  "Test *macs-readtable* with cl-ppcre"
  (is = 1 1))

(deftest syms ()
  "Test MACS.SYM"
  (with-gensyms (a b c)
    (let ((a 1) (b 1) (c 2))
      (is (and (= a b) (not (= c a))
	       (= (+ a b) (* c b)) )))))

#+nil (test-results *test-suite*)
#+nil (do-tests :macs)
