;;; fs.lisp --- Filesystem utils

;; TODO

;;; Commentary:

;; I think PN has a 'portable pathname library' in PAICL or
;; something. oldie but goodie for reference.

;;; Code:
(defpackage :macs.fs
  (:use :cl :macs.str :macs.cond :macs.fu)
  (:nicknames :fs)
  (:export))

(in-package :macs.fs)
