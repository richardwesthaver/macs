(in-package :macs.cli)

(defvar *cli-arg0* (car sb-ext:*posix-argv*))
(defvar *cli-args* (cdr sb-ext:*posix-argv*))

(defparameter *default-cli-opts*
  '(*cli-help-flag*
    *cli-version-flag*
    *cli-license-flag*)
  "A list of default flags to add to a top-level CLI program.")

(defparameter *cli-group-separator*
  "--"
  "A marker specifying the end of a unique group of CLI args.")

(defun command-line-args () (uiop:command-line-arguments))

(defmacro cli-flag-p (flag)
  "Test for presence of FLAG in `*cli-args*'. Return the tail of
`*cli-args*' starting from the position of FLAG."
  `(member ,flag *cli-args* :test #'string-equal))

(defmacro make-short-name (name) "make short-name"
  `(character (aref (if (stringp ,name) ,name (symbol-name ,name)) 0)))

;; (defun treat-as-argument (condition)
;;   "A handler which can be used to invoke the `treat-as-argument' restart"
;;   (invoke-restart (find-restart 'treat-as-argument condition)))

;; (defun discard-argument (condition)
;;   "A handler which can be used to invoke the `discard-argument' restart"
;;   (invoke-restart (find-restart 'discard-argument condition)))

(defmacro with-cli-handlers (&body body)
  "A wrapper which handles common cli errors."
  `(handler-case ,body
     (sb-sys:interactive-interrupt ()
	 (format *error-output* "~&(:SIGINT)~&")
	 (exit :code 130))
     (error (c)
       (format *error-output* "~&~A~&" c)
       (exit :code 1))))

(defmacro defmain (&body body)
  "Define a main function in the current package."
  (let ((pkg *package*)
	(main 'main))
    `(progn
       (defun ,main ()
	 (with-cli-handlers ,@`(list ,@body '(exit :code 0))))
       (export 'main ,pkg))))

(defmacro with-cli (largs env &body body) ;with-pandoric
  "Eval BODY with pandoric variables LARGS which should be accessible
from the provided closure ENV."
  `(with-pandoric ,largs ,env
     (with-cli-handlers (progn ,@body))))

(defmacro make-cli (kind &optional &rest slots)
  "Creates a new CLI object of the given kind."
  (declare (type (member :main :opt :cmd t) kind))
  `(make-instance
    ,(cond
       ((eql kind :cli) ''cli)
       ((eql kind :main) ''cli)
       ((eql kind :opt) 'cli-opt)
       ((eql kind :cmd) 'cli-cmd)
       (t ''cli))
    ,@slots))

(defgeneric parse-args (args obj)
  (:documentation "Parse ARGS against OBJ."))

(defgeneric format-help (obj)
  (:documentation "Format cli OBJ as a helpful string."))

(defgeneric format-usage (obj)
  (:documentation "Format cli OBJ as a useful string."))

(defgeneric handle-unknown-argument (obj arg))
(defgeneric handle-missing-argument (obj arg))
(defgeneric handle-invalid-argument (obj arg))

(defclass cli-opt ()
  ((name :initarg :name :initform nil :accessor cli-name :type (or null string))))

(defclass cli-cmd ()
  ((name :initarg :name :initform nil :accessor cli-name :type (or null string))
   (opts :initarg :opts :initform nil :accessor cli-opts)
   (usage :initarg :opts :initform nil :accessor cli-usage))
  (:documentation "A CLI command."))

(defclass cli ()
  ((name :initarg :name :initform (package-name *package*) :accessor cli-name :type string)
   (opts :initarg :opts :initform nil :accessor cli-opts :type (or list (vector cli-opt)))
   (cmds :initarg :cmds :initform nil :accessor cli-cmds :type (or list (vector cli-cmd)))
   (help :initarg :help :initform nil :accessor cli-help)
   (version :initarg :version :initform "0.1.0" :accessor cli-version :type string))
  (:documentation "CLI"))

(defmethod print-object ((self cli) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "name=~A opts=~A cmds=~A help=~A version=~A"
            (cli-name self)
            (length (cli-opts self))
	    (length (cli-cmds self))
	    (cli-help self)
	    (cli-version self))))

(defun parse-cli-args ())
