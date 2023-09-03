;;; macs.lisp --- macs wrapper package

;;; Code:
(defpackage :macs
  (:use
   :cl
   :macs.readtables
   :macs.reexport :macs.str :macs.sym :macs.list :macs.cond
   :macs.fu :macs.ana :macs.pan :macs.cli :macs.fs :macs.alien)
  (:local-nicknames
   (:readtables :macs.readtables)
   (:reexport :macs.reexport)
   (:str :macs.str)
   (:sym :macs.sym)
   (:ls :macs.list)
   (:cond :macs.cond)
   (:fu :macs.fu)
   (:ana :macs.ana)
   (:pan :macs.pan)
   (:cli :macs.cli)
   (:fs :macs.fs)
   (:alien :macs.alien)))

(in-package :macs)
(in-readtable *macs-readtable*)
(reexports :macs.reexport :str :sym :ls :cond :fu :ana :pan :cli :fs :alien)
(defpackage :macs-user
  (:use :cl :macs))
