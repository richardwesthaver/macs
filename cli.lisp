(in-package :macs.cli)

(defvar *cli-arg0* (car sb-ext:*posix-argv*))
(defvar *cli-args* (cdr sb-ext:*posix-argv*))

(defparameter *default-cli-opts*
  '(*cli-help-flag*
    *cli-version-flag*
    *cli-license-flag*)
  "A list of default flags to add to a top-level CLI program.")

(defun command-line-args () (uiop:command-line-arguments))

(defmacro cli-flag-p (flag)
  "Test for presence of FLAG in `*cli-args*'. Return the tail of
`*cli-args*' starting from the position of FLAG."
  `(member ,flag *cli-args* :test #'string-equal))

;; (defun treat-as-argument (condition)
;;   "A handler which can be used to invoke the `treat-as-argument' restart"
;;   (invoke-restart (find-restart 'treat-as-argument condition)))

;; (defun discard-argument (condition)
;;   "A handler which can be used to invoke the `discard-argument' restart"
;;   (invoke-restart (find-restart 'discard-argument condition)))

(defmacro with-cli-handlers (body)
  "A wrapper which handles the `sb-sys:interactive-interrupt' condition,
usually triggered via C-c."
  `(handler-case ,body
     (sb-sys:interactive-interrupt ()
       (progn
	 (format *error-output* "(:C-~:@C)~&" #\c)
	 (sb-ext:exit)))))

(defclass cli-cmd ()
  ((name :initarg :name :initform nil :accessor cli-cmd-name)
   (opts :initarg :opts :initform nil :accessor cli-cmd-opts)
   (usage :initarg :opts :initform nil :accessor cli-cmd-usage))
  (:documentation "A CLI command."))

(defmethod print-object ((obj cli-cmd) stream)
  (print-unreadable-object (cli-cmd stream :type t)
    (format stream "name=~A opts=~A"
            (cli-cmd-name command)
            (length (cli-cmd-opts command)))))

(defun parse-cli-args ())
