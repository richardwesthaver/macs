;;; macs.asd
(defsystem "macs"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "macros for the macro-programmer"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "pkg")
	       (:file "readtables")
	       (:file "fu")
	       (:file "pan")
	       (:file "ana")
	       ;;  TODO 2023-07-26: these feel too specific for
	       ;; inclusion in a macro library. should move them to
	       ;; modules designed for working with files/os/data
	       ;; structures..
	       (:file "list")
	       (:file "string")
	       (:file "file")))
