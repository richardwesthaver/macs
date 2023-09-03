;;; reexport.lisp --- macs.reexport

;; this is pulled directly from Masayuki Takagi's `cl-reexport':
;; https://github.com/takagi/cl-reexport/blob/master/src/cl-reexport.lisp

;;; Code:
(in-package :macs.reexport)

;;; internal
(defun external-symbols (package)
  (let (ret)
    (do-external-symbols (var package ret)
      (push var ret))))

(defun exclude-symbols (exclude symbols)
  (flet ((aux (symbol)
           (member symbol exclude :key #'symbol-name
                                  :test #'string=)))
    (remove-if #'aux symbols)))

(defun include-symbols (include symbols)
  (flet ((aux (symbol)
           (member symbol include :key #'symbol-name
                                  :test #'string=)))
    (if include
        (remove-if-not #'aux symbols)
        symbols)))

;; NOTE 2023-08-28: nested reexports aren't handled correctly

;;; Syntax:
;;;
;;;   REEXPORT-FROM package-from &key include exclude
;;;
;;; Arguments and values:
;;;
;;;   package-from --- a package designator from which symbols are reexported
;;;
(defun reexport-from (package-from &key include exclude)
  (unless (not (and include exclude))
    (error "INCLUDE option and EXCLUDE option are exclusive."))
  (let ((symbols (include-symbols include
                   (exclude-symbols exclude
                     (external-symbols package-from)))))
    (import symbols)
    (export symbols)
    symbols))

;; TODO 2023-08-27: handle include and exclude keywords
;; - also handle package name prefixes 'macs.'.
(defmacro reexports (&rest pkgs)
  "Reexport external symbols in PKGS from the current package."
  `(loop for p in ',pkgs
	 collect
	 (if (consp p)
	     (apply #'reexport-from p)
	     (funcall #'reexport-from p))))
	 
	    
	    
