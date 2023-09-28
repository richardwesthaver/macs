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

(setq *log-level*  :debug)

(deftest rt ()
  (is (typep (make-fixture-prototype :empty nil) 'fixture-prototype))
  (is (typep (make-fixture tfix () () t) 'function))
  (let ((fx1 (make-fixture fx1 () (a b c) (setq a 1 b 2 c 3))))
    (funcall fx1)
    (with-fixture (a b c) fx1 
      (is (not (member 'nil (mapcar #'= (list 1 2 3) (list a b c)))))))
  (signals (error t) (test-form (make-instance 'test-result))))

(deftest readtables ()
  "Test *macs-readtable* without cl-ppcre"
  (is (typep #`(,a1 ,a1 ',a1 ,@a1) 'function)))

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

(deftest log (:disable t)
  "Test MACS.STR"
  (let ((*log-level* :debug))
    (debug! "test" *log-level*)))

(deftest cond (:disable t)
  "Test MACS.STR")

(deftest reexport (:disable t)
  "Test MACS.STR")

(deftest thread (:disable t)
  "Test MACS.STR"
  (print-thread-info))

(deftest alien ()
  "Test MACS.STR"
  (is (= 0 (foreign-int-to-integer 0 4)))
  (is (= 1 (bool-to-foreign-int t))))

(deftest fmt ()
  "Test MACS.STR"
  (is (string= (format nil "| 1 | 2 | 3 |~%") (fmt-row '(1 2 3))))
  (is (string= (fmt-sxhash (sxhash t)) (fmt-sxhash (sxhash t))))
  (is (string= 
       (fmt-tree nil '(foobar (:a) (:b) (c) (d)) :layout :down)
       "FOOBAR
 ├─ :A
 ├─ :B
 ├─  C
 ╰─  D
")))

(deftest fu (:disable t)
  "Test MACS.STR")

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
  (make-prompt! tpfoo "testing: ")
  (defvar tcoll nil)
  (defvar thist nil)
				
  (deftest cli-prompt ()
    "Test MACS.CLI prompts"
    (let ((*standard-input* (make-string-input-stream 
			     (format nil "~A~%~A~%" "foobar" "foobar"))))
      ;; prompts 
      (is (string= (tpfoo-prompt) "foobar"))
      (is (string= "foobar"
		   (cli:completing-read "nothing: " tcoll :history thist :default "foobar"))))))

(defvar opts (cli:make-opts '(:name foo :global t :description "bar")
			    '(:name bar :description "foo")))

(defvar cmds (cli:make-cmds `(:name baz :description "baz" :opts ,opts)))

(deftest cli ()
  "test MACS.CLI OOS."
  (let ((cli (make-cli t :opts opts :cmds cmds :description "test cli")))
    (is (eq (cli:make-shorty "test") #\t))
    (is (equal (parse-args cli '("-f" "--bar"))
	       '((opt . #\f) (opt . "bar"))))
    (is (null (print-version cli)))
    (is (null (print-usage cli)))
    (is (null (print-help cli)))))

#+nil (test-results *test-suite*)
#+nil (do-tests :macs)
