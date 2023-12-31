;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(defpackage :macs.thread
  (:use :cl :reexport :macs.alien :sb-thread)
  (:nicknames :thread)
  (:export
   :print-thread-info :print-thread-message-top-level :thread-support-p))

(in-package :macs.thread)

;; (reexport-from :sb-thread
;; 	       :include '(:main-thread
;; 			  :*current-thread*
;; 			  :list-all-threads
;; 			  :thread
;; 			  :thread-alive-p
;; 			  :thread-name
;; 			  :thread-error
;; 			  :thread-yield
;; 			  :make-thread
;; 			  :join-thread
;; 			  :destroy-thread
;; 			  :interrupt-thread
;; 			  :semaphore
;; 			  :get-semaphore
;; 			  :make-semaphore
;; 			  :mutex
;; 			  :get-mutex
;; 			  :make-mutex
;; 			  :spinlock
;; 			  :get-spinlock
;; 			  :make-spinlock))

(defun thread-support-p () (member :thread-support *features*))

(defun print-thread-info (&optional (stream *standard-output*))
  (let* ((curr-thread sb-thread:*current-thread*)
         (curr-thread-name (sb-thread:thread-name curr-thread))
         (all-threads (sb-thread:list-all-threads)))
	(format stream "Current thread: ~a~%~%" curr-thread)
	(format stream "Current thread name: ~a~%~%" curr-thread-name)
	(format stream "All threads:~% ~{~a~%~}~%" all-threads)))

(eval-when (:compile-toplevel)
  (defun print-thread-message-top-level (msg)
    (sb-thread:make-thread
     (lambda ()
       (format #.*standard-output* msg)))
    nil))
