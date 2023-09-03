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
(in-packages :macs.tests)
(defsuite macs)
(in-suite macs)
(defun run-tests () (do-tests))

