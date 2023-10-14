(defsystem "readtables"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :maintainer "ellis <ellis@rwest.io>"
  :description "named-readtables and friends"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :depends-on (:pkg)
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "readtables")))
