;; pkg.lisp --- macs packages
(defparameter *macs-version* "0.1.0")
(defpackage :macs.sym
  (:use :cl)
  (:export
   #:ensure-symbol
   #:format-symbol
   #:make-keyword
   #:make-gensym
   #:make-gensym-list
   #:symbolicate))
(defpackage :macs.list
  (:use :cl)
  (:export
   #:ensure-car
   #:ensure-cons
   #:ensure-list))
(defpackage :macs.cond
  (:use :cl)
  (:export
   #:required-argument
   #:ignore-some-conditions
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:unwind-protect-case))
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
  (:use :cl :macs.readtables :macs.sym :macs.list)
  (:export #:macs-syntax
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
           #:when-match
	   #:string-designator
	   #:once-only
	   #:parse-body
	   #:parse-ordinary-lambda-list
	   #:with-gensyms
	   #:with-unique-names
	   #:destructuring-case
	   #:destructuring-ccase
	   #:destructuring-ecase))
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
(defpackage :macs.cli
  (:use :cl :macs.fu)
  (:export
   #+uiop :command-line-args
   :*cli-arg0*
   :*cli-args*
   :cli-flag-p
   :with-cli))
(defpackage :macs
  (:use :cl
   :macs.readtables :macs.fu :macs.ana :macs.pan :macs.cli))
