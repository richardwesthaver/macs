;;; macs.asd --- macros for the macro-programmer -*- mode: lisp; -*-
(defsystem "macs"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "macros for the macro-programmer"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :depends-on (:asdf :readtables)
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:module "src"
		:components
		((:file "str")
		 (:file "sym")
		 (:file "list")
		 (:file "cond")
		 (:file "fu")
		 (:file "fmt")
		 (:file "log")
		 (:file "alien")
		 (:file "thread")
		 (:file "fs")
		 (:file "ana")
		 (:file "pan")
		 (:file "cli")))
	       (:file "macs" :depends-on ("src"))))

(defsystem "macs/tests"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :description "tests for the macros for the macro-programmer"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :depends-on (:readtables :macs :rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :macs)))
