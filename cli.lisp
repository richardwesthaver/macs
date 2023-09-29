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
     (parse-args ,cli *argv*)
     (with-slots ,slots ,cli
       ,@body)))

;;; Prompts
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
  (let* ((coll (symbol-value collection))
	 (r (if coll
		(find (read-line) coll :key key :test test)
		(or (read-line) default))))
    (prog1
	r
    (setf (symbol-value history) (push r history)))))

(defmacro make-prompt! (var &optional prompt)
  "Generate a 'prompter' from list or variable VAR and optional
PROMPT string.

This isn't an ideal solution as it does in fact expose a dynamic
variable (VAR-prompt-history). We should generate accessors and
keep the variables within lexical scope of the generated
closure."
  (with-gensyms (s p h)
    `(let ((,s (if (boundp ',var) (symbol-value ',var) 
		   (progn 
		     (defvar ,(symb var) nil)
		     ',(symb var))))
           (,p (when (stringp ,prompt) ,prompt)) ;; prompt string
           (,h ',(symb var '-prompt-history))) ;; history symbol
       (defvar ,(symb var '-prompt-history) nil)
       (defun ,(symb var '-prompt) ()
	 ,(format nil "Prompt for a value from `~A', use DEFAULT if non-nil
and no value is provided by user, otherwise fallback to the `car'
of `~A-PROMPT-HISTORY'." var var)
	 (completing-read
          (format nil "~A [~A]: "
		  (or ,p ">")
		  (car (symbol-value ,h)))
	  ,s :history ,h :default nil)))))

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

(defun long-opt-p (str)
  (and (char= (aref str 0) (aref str 1) #\-)
       (> (length str) 2)))

(defun short-opt-p (str)
  (and (char= (aref str 0) #\-)
       (not (char= (aref str 1) #\-))
       (> (length str) 1)))

(defun opt-group-p (str)
  (string= str *cli-group-separator*))

(defun opt-prefix-eq (ch str)
  (char= (aref str 0) ch))

;;; Protocol
(defgeneric push-cmd (cmd place))
(defgeneric push-opt (opt place))

(defgeneric pop-cmd (place))
(defgeneric pop-opt (place))

(defgeneric find-cmd (self name))

(defgeneric find-opt (self name))

(defgeneric find-short-opt (self ch))

(defgeneric parse-args (self args &key &allow-other-keys)
  (:documentation "Parse list of strings ARGS using SELF.

A list of the same length as ARGS is returned containing 'cli-ast'
objects: (OPT . (or char string)) (CMD . string) NIL"))

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
  ((name :initarg :name :initform (required-argument :name) :accessor cli-name :type string)
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

(defmethod push-cmd ((self cli-cmd) (place cli-cmd))
  (vector-push self (cli-cmds place)))

(defmethod push-opt ((self cli-opt) (place cli-cmd))
  (vector-push self (cli-opts place)))

(defmethod pop-cmd ((self cli-cmd))
  (vector-pop (cli-cmds self)))

(defmethod pop-opt ((self cli-opt))
  (vector-pop (cli-opts self)))

;; typically when starting from a top-level `cli', the global
;; `cli-opts' will be parsed first, followed by the first command
;; found. If a command is found, the tail of the list is passed as
;; arguments to this function, which can pass additonal arguments to
;; nested commands.

;;  TODO 2023-09-12: Parsing restarts at the `*cli-group-separator*'
;; if present, or stops at EOI.

(declaim (inline %make-cli-node))
(defstruct (cli-node (:constructor %make-cli-node)) kind form)

(defun make-cli-node (kind form)  
  (%make-cli-node :kind kind :form form))

(declaim (inline %make-cli-ast))
(defstruct (cli-ast (:constructor %make-cli-ast)) ast)

(defun make-cli-ast (nodes)  
  (%make-cli-ast :ast nodes))

(defmethod find-cmd ((self cli-cmd) name)
  (find name (cli-cmds self) :key #'cli-name :test #'string=))

(defmethod find-opt ((self cli-cmd) name)
  (find name (cli-opts self) :key #'cli-name :test #'string=))

(defmethod find-short-opt ((self cli-cmd) ch)
  (find ch (cli-opts self) :key #'cli-name :test #'opt-prefix-eq))

(defmethod proc-args ((self cli-cmd) args)
  "process ARGS into a basic ast with validation."
  (make-cli-ast
   (loop 
     for a in args
     ;; SHORT OPT
     if (and (short-opt-p a)
	     (find-short-opt self (aref a 1)))
       collect (make-cli-node 'opt (aref a 1))
     ;; LONG OPT
     else if (and (long-opt-p a)
		  (find-opt self (string-trim "-" a)))
	    collect (make-cli-node 'opt (string-trim "-" a))
     ;; OPT GROUP
     else if (opt-group-p a)
	    collect nil
     ;; CMD
     else if (find-cmd self a)
	    collect (make-cli-node 'cmd a)
     ;; ARG
     else collect (make-cli-node 'arg a))))

(defmethod parse-args ((self cli-cmd) args &key (compile nil))
  "Parse ARGS and return the updated object SELF.

ARGS is assumed to be a valid cli-ast (list of cli-nodes), unless
COMPILE is t, in which case a list of strings is assumed."
  (with-slots (opts cmds) self
    (let ((args (if compile (proc-args self args) args)))
      (print args)
      self)))

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

(defmethod parse-args ((self cli) (args list) &key (compile t))
  "Parse list of string arguments ARGS and return the updated object SELF."
  ;; TODO 2023-09-25: room to optimize here
  (with-slots (opts cmds) self
    (let ((args (if compile (proc-args self args) args)))
      (debug! args)
      self)))
