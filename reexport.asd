(defsystem "reexport"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "cl-reexport clone"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "reexport")))
