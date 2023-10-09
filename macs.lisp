;;; pkg.lisp --- macs packages
(defpackage :macs
  (:use 
   :cl
   :readtables
   :reexport
   :macs.str :macs.sym :macs.list :macs.cond
   :macs.fu
   :macs.ana :macs.pan
   :macs.fs :macs.alien)
  (:export :*macs-version*))

(in-package :macs)
(in-readtable *macs-readtable*)
(defvar *macs-version* "0.1.0")
(reexports :readtables :reexport
	   :macs.str :macs.sym :macs.list :macs.cond :macs.fu :macs.ana 
	   :macs.pan :macs.fs :macs.alien)

(defpackage :macs-user
  (:use :cl :macs))
