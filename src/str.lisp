;;; str.lisp --- String utilities

;;; Code:
(defpackage :macs.str
  (:use :cl :uiop/driver)
  (:nicknames :str)
  (:import-from :macs.reexport :reexports)
  (:export
   #:string-designator))

(in-package :macs.str)

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(reexports
 (:sb-unicode
  :include (#:words #:lines #:sentences #:whitespace-p #:uppercase #:lowercase #:titlecase
		    #:word-break-class #:line-break-class #:sentence-break-class #:char-block
		    #:cased-p #:uppercase-p #:lowercase-p #:titlecase-p #:casefold
		    #:graphemes #:grapheme-break-class
		    #:bidi-mirroring-glyph #:bidi-class
		    #:normalize-string #:normalized-p #:default-ignorable-p
		    #:confusable-p #:hex-digit-p #:mirrored-p #:alphabetic-p #:math-p
		    #:decimal-value #:digit-value
		    #:unicode< #:unicode> #:unicode= #:unicode-equal
		    #:unicode<= #:unicode>=)))

;;;  TODO 2023-08-27: camel snake kebab

;;; format recipes
