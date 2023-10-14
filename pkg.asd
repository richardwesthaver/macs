(defsystem "pkg"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :description "home of the 'defpkg' defpackage extensions"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "pkg")))
