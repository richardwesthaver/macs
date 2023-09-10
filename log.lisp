;;; log.lisp --- logging facade for lisp

;; this package contains a simple logging facade for lisp applications
;; and libraries.

;;; Commentary:

;; Use `*log-level*' to set the current level of logging.

;; print macros: log! warn! info! dbg! trace!
(in-package :macs.log)

(defparameter *log-level* nil)

;; TODO 2023-09-09: 
(defparameter *log-timestamp* t "If non-nil, print a timestamp with log output. The value may be a
function in which case it is used as the function value of
`log-timestamp-source'.")

(declaim (inline log-timestamp-source))
(defun log-timestamp-source ()
  (format nil "~f" (/ (get-internal-real-time) internal-time-units-per-second)))

(defmacro log! (opts &rest args))

(defmacro warn! (opts &rest args))

(defmacro info! (opts &rest args))

;; TODO 2023-08-31: single format control string
(defmacro dbg! (&rest args)
  (with-gensyms (dbg)
    `(when-let ((,dbg *log-level*))
       (format ,dbg ":DBG")
       (if *log-timestamp*
	   (format ,dbg " @ ~A ::~t" (log-timestamp-source)))
       ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
       (map nil (lambda (x) (format ,dbg "~A " x)) ',args))))

(defmacro trace! (opts &rest args))
