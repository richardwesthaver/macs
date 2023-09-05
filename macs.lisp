;;; macs.lisp --- macs wrapper package

;;; Code:
(defpackage :macs
  (:use 
   :cl
   :macs.readtables
   :macs.reexport
   :macs.str :macs.sym :macs.list :macs.cond
   :macs.fu
   :macs.ana :macs.pan
   :macs.cli
   :macs.fs :macs.alien)
  (:export :*macs-version*))

(in-package :macs)
(in-readtable *macs-readtable*)
(defvar *macs-version* "0.1.0")
(reexports :macs.readtables
	   :macs.reexport :macs.str :macs.sym :macs.list
	   :macs.cond :macs.fu :macs.ana :macs.pan :macs.cli :macs.fs :macs.alien)
		  
(defpackage :macs-user
  (:use :cl :macs))

