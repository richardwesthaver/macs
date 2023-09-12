;;; cli.lisp --- cli programming api and utils

;; This package contains a simple api and macros for building lisp CLI
;; programs.

;;; Commentary:

;; - inspired by: clingon, uiop

;; Basic assumptions at runtime:
;;   - running in a POSIX-compliant shell
;;   - output stream supports UTF-8

;;; Code:
(in-package :macs.cli)

(defun cli-arg0 () (car sb-ext:*posix-argv*))
(defun cli-args () (cdr sb-ext:*posix-argv*))

(declaim (inline exec-path-list))
(defun exec-path-list ()
  (let ((var (sb-posix:getenv "PATH")))
    (mapcar #'directory
	    (loop for i = 0 then (1+ j)
		  as j = (position #\: var :start i)
		  collect (subseq var i j)
		while j))))

(defparameter *cli-group-separator*
  "--"
  "A marker specifying the end of a unique group of CLI args.")

;; uiop:command-line-arguments

(defmacro arg-p (arg)
  "Test for presence of ARG in `*cli-args*'. Return the tail of
`*cli-args*' starting from the position of FLAG."
  `(member ,arg (cli-args) :test #'string=))

(defmacro make-shorty (name)
  "Return the first char of symbol or string NAME."
  `(character (aref (if (stringp ,name) ,name (symbol-name ,name)) 0)))

;; (defun treat-as-argument (condition)
;;   "A handler which can be used to invoke the `treat-as-argument' restart"
;;   (invoke-restart (find-restart 'treat-as-argument condition)))

;; (defun discard-argument (condition)
;;   "A handler which can be used to invoke the `discard-argument' restart"
;;   (invoke-restart (find-restart 'discard-argument condition)))

(defmacro with-cli-handlers (form)
  "A wrapper which handles common cli errors that may occur during
evaluation of FORM."
  `(handler-case ,form
     (sb-sys:interactive-interrupt ()
       (format *error-output* "~&(:SIGINT)~&")
       (exit :code 130))
     (error (c)
       (format *error-output* "~&~A~&" c)
       (exit :code 1))))

(defmacro defmain (ret &body body)
  "Define a main function in the current package which returns RET.

Note that this macro does not export the defined function and requires
`macs.cli:main' to be an external symbol."
  `(progn
     (declaim (type stream output))
     (defun main (&key (output *standard-output*))
       "Run the top-level function and print to OUTPUT."
       (let ((*standard-output* output))
	 (with-cli-handlers
	     (progn ,@body ,ret))))))

(defvar *argv*)

(defun init-args () (setq *argv* (cons (cli-arg0) (cli-args))))

(defmacro with-cli (slots cli &body body)
  "Like with-slots with some extra bindings."
  ;;  `(with-pandoric nil nil
  `(progn
     (init-args)
     (with-slots ,slots ,cli
       ,@body)))

(defun make-cli (kind &rest slots)
  "Creates a new CLI object of the given kind."
  (declare (type (member :opt :cmd :cli t) kind))
  (apply #'make-instance
   (cond
     ((eql kind :cli) 'cli)
     ((eql kind :opt) 'cli-opt)
     ((eql kind :cmd) 'cli-cmd)
     (t 'cli))
    slots))

(defmacro make-opts (&rest opts)
  `(map 'vector
	(lambda (x)
	  (etypecase x
	    (string (make-cli :opt :name x))
	    (symbol (make-cli :opt :name (string-downcase (symbol-name x)) :global t))
	    (list (apply #'make-cli :opt x))))
	',opts))

(defmacro make-cmds (&rest opts)
  `(map 'vector
	(lambda (x)
	  (etypecase x
	    (string (make-cli :cmd :name x))
	    (symbol (make-cli :cmd :name (string-downcase (symbol-name x))))
	    (list (apply #'make-cli :cmd x))))
	',opts))

(defgeneric parse-args (self args)
  (:documentation "Parse ARGS using SELF."))

(defgeneric run-cmd (self)
  (:documentation "Run the command SELF."))

(defgeneric print-help (self)
  (:documentation "Format cli SELF as a helpful string."))

(defgeneric print-version (self)
  (:documentation "Print the version of SELF."))

(defgeneric print-usage (self)
  (:documentation "Format cli SELF as a useful string."))

(defgeneric handle-unknown-argument (self arg)
  (:documentation "Handle an unknown argument."))

(defgeneric handle-missing-argument (self arg)
  (:documentation "Handle a missing argument."))

(defgeneric handle-invalid-argument (self arg)
  (:documentation "Handle an invalid argument."))

(defclass cli-opt ()
  ;; note that cli-opts can have a nil or unbound name slot
  ((name :initarg :name :initform nil :accessor cli-name :type (or null string))
   (val :initarg :val :initform nil :accessor cli-val)
   (global :initarg :global :initform nil :accessor global-opt-p)
   (description :initarg :description :accessor cli-description :type string))
  (:documentation "CLI option"))

(defmethod print-object ((self cli-opt) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :global ~A :val ~A"
            (cli-name self)
	    (global-opt-p self)
	    (cli-val self))))

(defmethod print-usage ((self cli-opt))
  (format nil "~A~A~A"
	  (cli-name self)
	  (if (global-opt-p self) "* " "  ")
	  (if-let ((d (and (slot-boundp self 'description) (cli-description self))))
	    (format nil ":: ~A" d)
	    "")))

(defclass cli-cmd ()
  ;; name slot is required and must be a string
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
   (opts :initarg :opts :initform nil :accessor cli-opts :type (or (vector cli-opt) null))
   (cmds :initarg :cmds :initform nil :accessor cli-cmds :type (or (vector cli-cmd) null))
   (description :initarg :description :accessor cli-description :type string))
  (:documentation "CLI command"))

(defmethod print-object ((self cli-cmd) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :opts ~A :cmds ~A"
            (cli-name self)
            (length (cli-opts self))
	    (length (cli-cmds self)))))

(defclass cli (cli-cmd)
  ;; name slot defaults to *package*, must be string
  ((name :initarg :name :initform (string-downcase (package-name *package*)) :accessor cli-name :type string)
   (version :initarg :version :initform "0.1.0" :accessor cli-version :type string))
  (:documentation "CLI"))

(defmethod print-usage ((self cli))
  (iprintln (format nil "usage: ~A [global] <command> [<arg>]~%" (cli-name self))))

(defmethod print-version ((self cli))
  (println (cli-version self)))

(defmethod print-help ((self cli))
  (println (format nil "~A v~A" (cli-name self) (cli-version self)))
  (print-usage self)
  (iprintln (cli-description self))
  (terpri)
  (iprintln "options:")
  (with-slots (opts cmds) self
    (unless (null opts)
      (loop for o across opts
	    do (iprintln (print-usage o) 4)))
    (terpri)
    (iprintln "commands:")
    (unless (null cmds)
      (loop for c across cmds
	    do (iprintln c 4)))))
