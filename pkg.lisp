;; pkg.lisp --- macs packages
(defparameter *macs-version* "0.1.0")
(defpackage :macs.reexport
  (:use :cl)
  (:export :reexport-from :reexports))
(defpackage :macs.str
  (:use :cl :macs.reexport :uiop)
  (:export
   #:string-designator))
(defpackage :macs.sym
  (:use :cl :macs.str)
  (:export
   #:ensure-symbol
   #:format-symbol
   #:make-keyword
   #:make-gensym
   #:make-gensym-list
   #:with-gensyms
   #:with-unique-names
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
   #:circular-dependency-error
   #:circular-dependency-error-items
   #:unknown-argument-error
   #:unknown-argument-error-name
   #:unknown-argument-error-kind
   #:unknown-argument-error-p
   #:missing-argument-error
   #:missing-argument-error-command
   #:missing-argument-error-p
   #:invalid-argument-error
   #:invalid-argument-error-item
   #:invalid-argument-error-reason
   #:invalid-argument-error-p
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
  (:export
   #:macs-syntax
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
   #+cl-ppcre #:|#~-reader|
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
   #:once-only
   #:parse-body
   #:parse-ordinary-lambda-list
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
  (:use :cl :macs.cond :macs.fu :macs.pan)
  (:import-from :uiop :split-string)
  (:export
   :command-line-args
   :*cli-arg0*
   :*cli-args*
   :*default-cli-opts*
   :cli-flag-p
   :with-cli-handlers
   :cli
   :cli-cmd
   :parse-cli-args
   :parse-args))
(defpackage :macs
  (:use
   :cl
   :macs.reexport
   :macs.str :macs.sym :macs.list
   :macs.cond :macs.readtables
   :macs.fu :macs.ana :macs.pan
   :macs.cli))
