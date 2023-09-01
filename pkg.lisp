;; pkg.lisp --- macs packages
(defparameter *macs-version* "0.1.0")
(defpackage :macs.reexport
  (:use :cl)
  (:export :reexport-from :reexports))
(defpackage :macs.str
  (:use :cl :uiop)
  (:import-from :macs.reexport :reexports)
  (:export
   #:string-designator))
(defpackage :macs.sym
  (:use :cl :macs.str)
  (:export
   #:ensure-symbol
   #:format-symbol
   #:make-keyword
   #:make-slot-name
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
   #:circular-dependency
   #:circular-dependency-items
   #:unknown-argument
   #:unknown-argument-name
   #:unknown-argument-kind
   #:unknown-argument-p
   #:missing-argument
   #:missing-argument-command
   #:missing-argument-p
   #:invalid-argument
   #:invalid-argument-item
   #:invalid-argument-reason
   #:invalid-argument-p
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
   #:parse-body))
(defpackage :macs.fu
  (:use :cl :macs.readtables :macs.sym :macs.list :macs.cond)
  (:export
   #:*macs-readtable*
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
   #:destructuring-ecase
   #:when-let
   #:when-let*
   #:if-let
   #:if-let*))
(defpackage macs.ana
  (:use :cl :macs.readtables :macs.fu)
  (:export
   #:alambda
   #:nlet-tail
   #:alet%
   #:alet
   #:it
   #:aif
   #:this
   #:self))
(defpackage :macs.pan
  (:use :cl :macs.readtables :macs.fu :macs.ana)
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
(defpackage :macs.fs
  (:use :cl :macs.str :macs.cond :macs.fu)
  (:export))
(defpackage :macs.cli
  (:use :cl :macs.sym :macs.cond :macs.fu :macs.pan :macs.str)
  (:import-from :sb-ext :exit)
  (:export
   :command-line-args
   :*cli-arg0*
   :*cli-args*
   :*default-cli-opts*
   :*cli-group-separator*
   :cli-flag-p
   :make-short-name
   :with-cli-handlers
   :defmain
   :main
   :with-cli
   :make-cli
   :print-help
   :print-usage
   :handle-unknown-argument
   :handle-missing-argument
   :handle-invalid-argument
   :cli
   :cli-name
   :cli-opts
   :cli-cmds
   :cli-help
   :cli-version
   :cli-usage
   :cli-opt
   :cli-cmd
   :parse-cli-args
   :parse-args))
(defpackage :macs.rt
  (:use :cl :macs.sym :macs.cond :macs.readtables :macs.fu)
  (:export
   :*test-debug*
   :*test-debug-timestamp*
   :*compile-tests*
   :*catch-test-errors*
   :*test-suffix*
   :*test-suite*
   :*test-suite-list*
   :test-suites
   :*testing*
   :test-suite-designator
   :check-suite-designator
   :test-debug-timestamp-source
   :dbg!
   :make-test
   :with-test
   :do-test
   :do-tests
   :continue-testing
   :with-test-env
   :deftest
   :suite-name-eq
   :suite-name=
   :make-suite
   :defsuite
   :ensure-suite
   :assure-suite
   :in-suite
   :eval-test
   :compile-test
   :pending-tests
   :add-test
   :delete-test
   :find-test
   :do-suite
   :test-object
   :test
   :test-fixture
   :test-suite
   :test-name
   :tests
   :should-fail-tests))

(defpackage :macs.ffi
  (:use :cl :sb-alien :macs.str :macs.sym :macs.fu)
  (:export
   :defbytes
   :u2 :u4 :u8 :u16 :u32 :u64 :u128
   :i4 :i8 :i16 :i32 :i64 :i128
   :f16 :f32 :f64 :f128))
