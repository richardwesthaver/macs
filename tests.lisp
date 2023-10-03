;;; tests.lisp --- macs system tests

;;; Commentary:

;; TODO: fix false positives when using (eval-test)

;;; Code:
(defpackage :macs.tests
  (:use
   :cl
   :macs.readtables
   :macs.reexport
   :macs.str
   :macs.fmt
   :macs.sym
   :macs.list
   :macs.cond
   :macs.log
   :macs.fu
   :macs.ana
   :macs.pan
   :macs.fs
   :macs.cli
   :macs.alien
   :macs.thread
   :macs.rt)
  (:export :run-tests))
(in-package :macs.tests)

(defsuite :macs)
(defsuite :macs.readtables)
(defsuite :macs.rt)
(in-suite :macs)
(in-readtable *macs-readtable*)

(deftest rt (:bench 100 :profile t :persist t)
  (is (typep (make-fixture-prototype :empty nil) 'fixture-prototype))
  (is (typep (make-fixture tfix () () t) 'function))
  (let ((fx1 (make-fixture fx1 () (a b c) (setq a 1 b 2 c 3))))
    (funcall fx1)
    (with-fixture (a b c) fx1 
      (is (not (member 'nil (mapcar #'= (list 1 2 3) (list a b c)))))))
  (signals (error t) (test-form (make-instance 'test-result))))

(deftest readtables ()
  "Test *macs-readtable* without cl-ppcre"
  (is (typep #`(,a1 ,a1 ',a1 ,@a1) 'function))
  (is (string= #"test "foo" "# "test \"foo\" "))
  (is (string= #$test "1 2 3"$# "test \"1 2 3\"")))

#+cl-ppcre
(deftest ppcre-readtables (:persist t)
  "Test *macs-readtable* with cl-ppcre"
  (is (= 1 1)))

(deftest sym ()
  "Test MACS.SYM"
  ;; gensyms
  (is (not (equalp (make-gensym 'a) (make-gensym 'a))))
  (is (eq (ensure-symbol 'tests :macs.tests) 'tests))
  (is (eq 'macs.tests::foo (format-symbol :macs.tests "~A" 'foo)))
  (is (eq (make-keyword 'fizz) :fizz)))

;;; TODO
(deftest str ()
  "Test MACS.STR"
  (is (typep "test" 'string-designator))
  (is (typep 'test 'string-designator))
  (is (typep #\C 'string-designator))
  (is (not (typep 0 'string-designator))))

(deftest list ()
  "Test MACS.STR"
  ;; same object - a literal
  (is (eq (ensure-car '(0)) (ensure-car 0)))
  (is (eq (ensure-car '(nil)) (ensure-car nil)))
  ;; different objects
  (is (not (eq (ensure-cons 0) (ensure-cons 0))))
  (is (equal (ensure-cons 0) (ensure-cons 0))))

(deftest log ()
  "Test MACS.STR"
  (is (debug! "test" *log-level*)))

(deftest cond ()
  "Test MACS.STR")

(deftest reexport ()
  "Test MACS.STR")

(deftest thread ()
  "Test MACS.STR"
  (is (stringp (print-thread-info nil))))

(deftest alien ()
  "Test MACS.STR"
  (is (= 0 (foreign-int-to-integer 0 4)))
  (is (= 1 (bool-to-foreign-int t))))

(deftest fmt ()
  "Test MACS.STR"
  (is (string= (format nil "| 1 | 2 | 3 |~%") (fmt-row '(1 2 3))))
  (is (string= (fmt-sxhash (sxhash t)) (fmt-sxhash (sxhash t))))
  (is (string= 
       ;; note the read-time-eval..
       #.(fmt-tree nil '(foobar (:a) (:b) (c) (d)) :layout :down)
       #"FOOBAR
 ├─ :A
 ├─ :B
 ├─  C
 ╰─  D
"#))
;; with plist option
  (is (string= 
       #.(fmt:fmt-tree nil '(sk-project :name "foobar" :path "/a/b/c.asd" :vc :hg) :layout :down :plist t)
       #"SK-PROJECT
 ├─ :NAME
 │   ╰─ "foobar"
 ├─ :PATH
 │   ╰─ "/a/b/c.asd"
 ╰─ :VC
     ╰─ :HG
"#)))

(deftest ana ()
  "Test MACS.STR"
  (is (= 8 
	 (aif (+ 2 2)
	      (+ it it)))))

(deftest pan ()
  "Test MACS.STR"
  (let ((p
	  (plambda (a) (b c)
		   (if (not a)
		       (setq b 0
			     c 0)
		       (progn (incf b a) (incf c a))))))
    (with-pandoric (b c) p
      (is (= 0 (funcall p nil)))
      (is (= 1 (funcall p 1)))
      (is (= 1 b c)))))

;; WARNING bugs ahead

;; we should be able to call this from the body of the test, but we
;; get an undefined-function error for 'MACS.RT::MAKE-PROMPT!'
(unless *compile-tests*
  (deftest cli-prompt ()
    "Test MACS.CLI prompts"
    (make-prompt! tpfoo "testing: ")
    (defvar tcoll nil)
    (defvar thist nil)
    (let ((*standard-input* (make-string-input-stream 
			     (format nil "~A~%~A~%" "foobar" "foobar"))))
      ;; prompts 
      (is (string= (tpfoo-prompt) "foobar"))
      (is (string= "foobar"
		   (cli:completing-read "nothing: " tcoll :history thist :default "foobar"))))))

(defparameter *opts* (cli:make-opts (:name foo :global t :description "bar")
			    (:name bar :description "foo")))

(defparameter *cmd1* (make-cli :cmd :name "holla" :opts *opts* :description "cmd1 description"))
(defparameter *cmd2* (make-cli :cmd :name "ayo" :cmds #(*cmd1*) :opts *opts* :description "cmd1 description"))
(defparameter *cmds* (cli:make-cmds (:name "baz" :description "baz" :opts *opts*)))

(defparameter *cli* (make-cli t :opts *opts* :cmds *cmds* :description "test cli"))
(deftest cli ()
  "test MACS.CLI OOS."
  (let ((cli *cli*))
    (is (eq (make-shorty "test") #\t))
    (is (equalp (proc-args cli '("-f" "baz" "--bar" "fax")) ;; not eql
		(make-cli-ast 
		 (list (make-cli-node 'opt (find-short-opt cli #\f))
		       (make-cli-node 'cmd (find-cmd cli "baz"))
		       (make-cli-node 'opt (find-opt cli "bar"))
		       (make-cli-node 'arg "fax")))))
    (is (parse-args cli '("--bar" "baz" "-f" "yaks")))

    (unless *compile-tests*
      (is (= 1 (funcall (gen-cli-thunk 1 nil (lambda () $a0)))))
      (is (= 2 (funcall (gen-cli-thunk nil (2 3) (lambda () $a1)))))
      (is (= 3 (funcall (gen-cli-thunk nil (2 3) (lambda () $a2)))))q
      (let ((c1 (parse-args cli '("--foo" "boombap"))))
	;; the form we pass in needs to be unevaluated - this is a function
	(is (install-thunk c1 (lambda () $a0 'thunk-ok)))
	(is (eql 'thunk-ok (do-cmd c1)))))
    (is (stringp
	 (with-output-to-string (s)
	   (print-version cli s)
	   (print-usage cli s)
	   (print-help cli s))))))
