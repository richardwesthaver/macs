;;; macs.lisp --- macs packages
(pkg:defpkg :macs
  (:use 
   :cl
   :readtables
   :reexport
   :str :sym :list :cond
   :fu
   :ana :pan
   :fs :alien)
  (:reexport
   :cl
   :readtables
   :reexport
   :macs.str :macs.sym :macs.list :macs.cond
   :macs.fu
   :macs.ana :macs.pan
   :macs.fs :macs.alien)
  (:export :*macs-version*))

(in-package :macs)
(defvar *macs-version* "0.1.0")
(defpackage :macs-user
  (:use :cl :macs))
(provide :macs)
