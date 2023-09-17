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

;;; Macros
(defmacro argp (arg &optional (args (cli-args)))
  "Test for presence of ARG in ARGS. Return the tail of
ARGS starting from the position of ARG."
  `(member ,arg ,args :test #'string=))

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

(defun init-args () (setq *argv* (cons (cli-arg0) (cli-args))))

(defmacro with-cli (slots cli &body body)
  "Like with-slots with some extra bindings."
  ;;  `(with-pandoric nil nil
  `(progn
     (init-args)
     (with-slots ,slots ,cli
       ,@body)))

(defun completing-read (prompt collection
			&key (history nil) (default nil)
			  (key nil) (test nil))

  "A simplified COMPLETING-READ for common-lisp.

The Emacs completion framework includes a function called
`completing-read' which prompts the user for input from the
mini-buffer. It is a very flexible interface which can be used to read
user input programatically. This is incredibly useful for building
data entry interfaces -- for example see the `make-prompt!' macro.

Obviously writing a completion framework is out-of-scope, but we can
simulate one by embedding a DSL in our prompters if we choose. For
example, perhaps we treat a single '?' character as a request from the
user to list valid options while continue waiting for input."
  (princ prompt)
  (let ((r (if collection
	       (find (read-line) collection :key key :test test)
	       (or (read-line) default))))
    (prog1 
	r
      (push r (symbol-value history)))))

(defmacro! make-prompt! (o!var &optional o!prompt)
  "Generate a 'prompter' from list or variable VAR and optional
PROMPT string.

This isn't an ideal solution as it does in fact expose a dynamic
variable (VAR-prompt-history). We should generate accessors and
keep the variables within lexical scope of the generated
closure."
  `(let ((,g!s (cond ;; prefix symbol
                ((listp ',o!var) ,o!var)
                ((boundp ',o!var) (symbol-value ,o!var))
		((symbolp ',o!var) nil)
                (t (error 'invalid-argument
			  :reason "first arg must be a bound symbol or a list."
			  :item ',o!var))))
         (,g!p ,(when (stringp o!prompt) o!prompt)) ;; prompt string
         (,g!h ',(symb o!var '-prompt-history))) ;; history symbol
     (with-compilation-unit (:policy '(optimize))
       ;;1 we use defvar for the implicit bindp check
       (defvar ,(symb o!var '-prompt-history) nil)
       ;;2 our new SYM-prompt function
       (declaim (inline ,(symb o!var '-prompt)))
       (defun ,(symb o!var '-prompt) (&optional default)
	 ,(format nil "Prompt for a value from `~A', use DEFAULT if non-nil
and no value is provided by user, otherwise fallback to the `car'
of `~A-PROMPT-HISTORY'." o!var o!var)
	 (completing-read
          (format nil "~A [~A]: "
		  ,g!p
		  (car (symbol-value ,g!h)))
	  ,g!s :history ,g!h :default (car (symbol-value ,g!h)))))))

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

;;; Utils
(defvar *argv*)

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

;; RESEARCH 2023-09-12: closed over hash-table with short/long flags
;; to avoid conflicts. if not, need something like a flag-function
;; slot at class allocation.
(defun make-opts (&rest opts)
  (map 'vector
       (lambda (x)
	 (etypecase x
	   (string (make-cli :opt :name x))
	   (symbol (make-cli :opt :name (string-downcase (symbol-name x)) :global t))
	   (list (apply #'make-cli :opt x))))
       opts))

(defun make-cmds (&rest opts)
  (map 'vector
	(lambda (x)
	  (etypecase x
	    (string (make-cli :cmd :name x))
	    (symbol (make-cli :cmd :name (string-downcase (symbol-name x))))
	    (list (apply #'make-cli :cmd x))))
	opts))

;;; Protocol
(defgeneric parse-args (self args)
  (:documentation "Parse ARGS using SELF."))

(defgeneric do-cmd (self)
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

;;; Objects
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
  (format nil " -~(~{~A~^/--~}~)~A~A"
	  (if-let ((n (cli-name self)))
	    (list (make-shorty n) n)
	    'dyn)
	  (if (global-opt-p self) "* " "  ")
	  (if-let ((d (and (slot-boundp self 'description) (cli-description self))))
	    (format nil ":  ~A" d)
	    "")))

(defclass cli-cmd ()
  ;; name slot is required and must be a string
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
   (opts :initarg :opts :initform nil :accessor cli-opts :type (or (vector cli-opt) null))
   (cmds :initarg :cmds :initform nil :accessor cli-cmds :type (or (vector cli-cmd) null))
   (thunk :initarg :thunk :accessor cli-thunk :type lambda)
   (description :initarg :description :accessor cli-description :type string))
  (:documentation "CLI command"))

(defmethod print-object ((self cli-cmd) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A :opts ~A :cmds ~A"
	  (cli-name self)
          (length (cli-opts self))
	  (length (cli-cmds self)))))

(defmethod print-usage ((self cli-cmd))
  (with-slots (opts cmds) self
    (format nil "~(~A~)  ~A~A~A"
	    (cli-name self)
	    (if-let ((d (and (slot-boundp self 'description) (cli-description self))))
	      (format nil ":  ~A" d)
	      "")
	    (if (null opts)
		""
		(format nil "~{~%    ~A~^~}" (loop for o across opts collect (print-usage o))))
	    (if (null cmds)
		""
		(format nil "~%    ~{!  ~A~}" (loop for c across cmds collect (print-usage c)))))))

;; typically when starting from a top-level `cli', the global
;; `cli-opts' will be parsed first, followed by the first command
;; found. If a command is found, the tail of the list is passed as
;; arguments to this function, which can pass additonal arguments to
;; nested commands.

;;  TODO 2023-09-12: Parsing restarts at the `*cli-group-separator*'
;; if present, or stops as EOI.
(defmethod parse-args ((self cli-cmd) args)
  (with-slots (opts cmds) self
    (let ((args (cdr args))
	  r)
      (push 
       (loop 
	 for i from 0
	 for a in (cdr args)
	 for c across cmds
	 if (string= a (cli-name c))
	   ;;  TODO 2023-09-12: better parsing strat
	   collect (parse-args c (nthcdr i args)))
	 r)
      (push
       (loop
	 for i from 0
	 for a in args
	 for o across opts
	 if (and 
	     (print a t)
	     (char-equal (aref a 0) #\-) 
	     (let ((%i (string-trim "-" a)))
	       (or (string= %i (cli-name o)) 
		   (string= %i (make-shorty (cli-name o))))))
	   collect (nth (1+ i) args))
       r)
      r)))

;; warning: make sure to fill in the opt and cmd slots with values
;; from the top-level args before doing a command.
(defmethod do-cmd ((self cli-cmd))
  (if (slot-boundp self 'thunk)
      ;; TODO 2023-09-12: handle args/env
      (funcall (cli-thunk self))
      (error 'slot-unbound 'thunk)))

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
	    do (iprintln (print-usage c) 4)))))
