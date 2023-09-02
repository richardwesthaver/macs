;;; macs.asd --- macros for the macro-programmer -*- mode: lisp; -*-
(defsystem "macs/readtables"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "named-readtables and friends"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "readtables")))

(defsystem "macs"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "macros for the macro-programmer"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :depends-on (:uiop :sxp :macs/readtables)
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "pkg")
               (:file "reexport")
	       (:file "str")
	       (:file "sym")
	       (:file "list")
	       (:file "cond")
	       (:file "fs")
	       (:file "fu")
	       (:file "ana")
	       (:file "pan")
	       (:file "cli")
	       (:file "ffi")))

(defsystem "macs/rt"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "test framework"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :depends-on (:macs)
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "rt")))


