(in-package :macs.cli)

#+uiop (defun command-line-args () (uiop:command-line-arguments))

(defvar *cli-arg0* (car sb-ext:*posix-argv*))
(defvar *cli-args* (cdr sb-ext:*posix-argv*))

(defmacro cli-flag-p (flag)
  "Test for presence of FLAG in `*cli-args*'. Return the tail of
`*cli-args*' starting from the position of FLAG."
  `(member ,flag *cli-argv* :test #'string-equal))

(defmacro with-cli (body)
  "A wrapper which handles the `sb-sys:interactive-interrupt' condition,
usually triggered via C-c."
  `(handler-case ,body
     (sb-sys:interactive-interrupt ()
       (progn
	 (format *error-output* "C-c.~&")
	 (sb-ext:exit)))))
