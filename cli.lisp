;;; cli.lisp --- CLI utils

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

(defmacro cli-flag-p (flag)
  "Test for presence of FLAG in `*cli-args*'. Return the tail of
`*cli-args*' starting from the position of FLAG."
  `(member ,flag (cli-args) :test #'string-equal))

(defmacro make-short-name (name) "make short-name"
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
       (let ((*standard-output* output))
	 (with-cli-handlers (progn ,@body ,ret))))))

(defvar *argv*)

(defun init-args () (setq *argv* (cons (cli-arg0) (cli-args))))

(defmacro with-cli (slots cli &body body)
  "Like with-slots with some extra bindings."
  ;;  `(with-pandoric nil nil
  `(progn
     (init-args)
     (with-slots ,slots ,cli
       ,@body)))
;;)

(defmacro make-cli (kind &rest slots)
  "Creates a new CLI object of the given kind."
  (declare (type (member :main :opt :cmd :cli t) kind))
  `(make-instance
    ,(cond
       ((eql kind :cli) ''cli)
       ((eql kind :main) ''cli)
       ((eql kind :opt) ''cli-opt)
       ((eql kind :cmd) ''cli-cmd)
       (t ''cli))
    ,@slots))

(defgeneric parse-args (self args)
  (:documentation "Parse ARGS using SELF."))

(defgeneric print-help (self)
  (:documentation "Format cli SELF as a helpful string."))

(defgeneric print-version (self)
  (:documentation "Print the version of SELF."))

(defgeneric print-version (self)
  (:documentation "Print the banner of SELF."))

(defgeneric print-usage (self)
  (:documentation "Format cli SELF as a useful string."))

(defgeneric handle-unknown-argument (self arg)
  (:documentation "Handle an unknown argument."))

(defgeneric handle-missing-argument (self arg)
  (:documentation "Handle a missing argument."))

(defgeneric handle-invalid-argument (self arg)
  (:documentation "Handle an invalid argument."))

(defclass cli-opt ()
  ((name :initarg :name :initform nil :accessor cli-name :type (or null string))
   (val :initarg :val :initform nil :accessor cli-val))
  (:documentation "CLI option"))

(defclass cli-cmd ()
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
   (opts :initarg :opts :initform nil :accessor cli-opts :type (or (vector cli-opt) null))
   (cmds :initarg :cmds :initform nil :accessor cli-cmds :type (or (vector cli-cmd) null)))
  (:documentation "CLI command"))

(defclass cli (cli-cmd)
  ((name :initarg :name :initform (package-name *package*) :accessor cli-name :type string)
   (version :initarg :version :initform "0.1.0" :accessor cli-version :type string)
   (banner :initarg :banner :accessor cli-banner :type string)
   (help :initarg :help :accessor cli-help :type string))
  (:documentation "CLI"))

(defmethod print-object ((self cli) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "name=~A opts=~A cmds=~A version=~A"
            (cli-name self)
            (length (cli-opts self))
	    (length (cli-cmds self))
	    (cli-version self))))

(defmethod print-help ((self cli))
  (print (cli-help self) t))

(defmethod print-version ((self cli))
  (print (cli-version self) t))

(defmethod print-banner ((self cli))
  (princ (cli-banner self) t))
