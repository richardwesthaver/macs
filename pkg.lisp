;; pkg.lisp --- macs packages
(defpackage :macs.readtables
  (:use :cl)
  (:export
   #:defreadtable
   #:in-readtable
   #:make-readtable
   #:merge-readtables-into
   #:find-readtable
   #:ensure-readtable
   #:rename-readtable
   #:readtable-name
   #:register-readtable
   #:unregister-readtable
   #:copy-named-readtable
   #:list-all-named-readtables
   ;; Types
   #:named-readtable-designator
   ;; Conditions
   #:readtable-error
   #:reader-macro-conflict
   #:readtable-does-already-exist
   #:readtable-does-not-exist
   #:parse-body)
  (:documentation "See MACS.READTABLES::@READTABLES-MANUAL."))

(defpackage :macs.fu
  (:use :cl :macs.readtables)
  (:export :macs-syntax
           #:mkstr
           #:symb
           #:group
           #:flatten
           #:fact
           #:choose
           #:g!-symbol-p
           #:defmacro/g!
           #:o!-symbol-p
           #:o!-symbol-to-g!-symbol
           #:defmacro!
           #:defun!
           #:|#"-reader|
           #:|#`-reader|
           #:|#f-reader|
           #:segment-reader
           #:match-mode-ppcre-lambda-form
           #:subst-mode-ppcre-lambda-form
           #:|#~-reader|
           #:dlambda
           #:make-tlist
           #:tlist-left
           #:tlist-right
           #:tlist-empty-p
           #:tlist-add-left
           #:tlist-add-right
           #:tlist-rem-left
           #:tlist-update
           #:build-batcher-sn
           #:sortf
           #:dollar-symbol-p
           #:if-match
           #:when-match))

(defpackage macs.ana
  (:use :cl :macs.readtables :macs.fu)
  (:export
   :alambda
   #:nlet-tail
   #:alet%
   #:alet
   #:it
   #:aif
   #:this
   #:self))

(defpackage :macs.pan
  (:use :cl :macs.readtables :macs.fu)
  (:export
   #:pandoriclet
   #:pandoriclet-get
   #:pandoriclet-set
   #:get-pandoric
   #:with-pandoric
   #:pandoric-hotpatch
   #:pandoric-recode
   #:plambda
   #:pandoric-eval))

(defparameter *macs-version* "0.1.0")

(defpackage :macs
  (:use :cl :macs.readtables :macs.fu :macs.ana :macs.pan))
