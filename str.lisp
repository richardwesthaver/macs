;;; str.lisp --- String utilities
(in-package :macs.str)
(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))
