;;; pkg.lisp --- macs packages

(defpackage :macs.reexport
  (:use :cl)
  (:nicknames :reexport)
  (:export :reexport-from :reexports))

(defpackage :macs.str
  (:use :cl :uiop/driver)
  (:nicknames :str)
  (:import-from :macs.reexport :reexports)
  (:export
   #:string-designator))

(defpackage :macs.sym
  (:use :cl :reexport :str :sb-int)
  (:nicknames :sym)
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
  (:use :cl :macs.reexport)
  (:nicknames :list)
  (:export
   #:ensure-car
   #:ensure-cons))

(defpackage :macs.cond
  (:use :cl)
  (:nicknames :cond)
  (:export
   #:nyi!
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

(defpackage :macs.fu
  (:use :cl :sb-mop :sb-c :macs.readtables :macs.reexport :macs.sym :macs.list :macs.cond)
  (:nicknames :fu)
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
   #:destructuring-case
   #:destructuring-ccase
   #:destructuring-ecase
   #:when-let
   #:when-let*
   #:if-let
   #:if-let*
   :defcmd
   :eval-always
   :merge! :sort!
   :list-slot-values-using-class :list-class-methods :list-class-slots :list-indirect-slot-methods))
   
(defpackage :macs.fmt
  (:use :cl :reexport :str :fu :list)
  (:import-from :uiop :println)
  (:nicknames :fmt)
  (:export :printer-status :fmt-row :fmt-sxhash :iprintln :fmt-tree))

(defpackage :macs.log
  (:use :cl :str :fmt :sym :fu)
  (:nicknames :log)
  (:export :*log-level* :log-level-designator :log-timestamp-source :log! :warn! :info! :debug! :trace!))

(defpackage :macs.ana
  (:use :cl :macs.readtables :macs.reexport :macs.fu)
  (:nicknames :ana)
  (:export
   #:alambda
   #:nlet-tail
   #:alet%
   #:alet
   #:acond2
   #:it
   #:aif
   #:this
   #:self))

(defpackage :macs.pan
  (:use :cl :macs.readtables :macs.fu :macs.ana)
  (:nicknames :pan)
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
  (:nicknames :fs)
  (:export))

(defpackage :macs.cli
  (:use :cl :sym :cond :fu :str :ana :fmt)
  (:import-from :ana :alet)
  (:import-from :uiop :println)
  (:nicknames :cli)
  (:shadowing-import-from :sb-ext :exit)
  (:export
   :*argv*
   :init-args
   :cli-arg0
   :cli-args
   :command-line-args
   :*cli-group-separator*
   :exec-path-list
   :argp
   :make-shorty
   :with-cli-handlers
   :completing-read
   :make-prompt!
   :defmain
   :main
   :with-cli
   :make-cli
   :make-opts
   :make-cmds
   :proc-args
   :parse-args
   :do-cmd
   :print-help
   :print-version
   :print-usage
   :handle-unknown-argument
   :handle-missing-argument
   :handle-invalid-argument
   :cli-opt
   :cli-cmd
   :cli
   :opt
   :cmd
   :cli-name
   :cli-opts
   :cli-cmds
   :cli-thunk
   :cli-description
   :cli-version
   :cli-usage))

(defpackage :macs.alien
  (:use :cl :macs.reexport :sb-vm :sb-alien :sb-ext :sb-c :macs.str :macs.sym :macs.fu)
  (:nicknames :alien)
  (:export
   :foreign-int-to-integer :foreign-int-to-bool :bool-to-foreign-int
   :defbytes
   :u1 :u2 :u3 :u4 :u8 :u16 :u24 :u32 :u64 :u128
   :i2 :i3 :i4 :i8 :i16 :i24 :i32 :i64 :i128
   :f16 :f24 :f32 :f64 :f128))

(defpackage :macs.thread
  (:use :cl :macs.reexport :macs.alien :sb-thread)
  (:nicknames :thread)
  (:export
   :print-thread-info :print-thread-message-top-level :thread-support-p))
