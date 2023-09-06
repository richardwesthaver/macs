;;; threads.lisp --- Multi-thread utilities

;; Threading Macros

;;; Commentary:

;; mostly yoinked from sb-thread and friends

;;; Code:
(in-package :macs.thread)

(reexport-from :sb-thread
	       :include '(:main-thread
			  :*current-thread*
			  :list-all-threads
			  :thread
			  :thread-alive-p
			  :thread-name
			  :thread-error
			  :thread-yield
			  :make-thread
			  :join-thread
			  :destroy-thread
			  :interrupt-thread
			  :semaphore
			  :get-semaphore
			  :make-semaphore
			  :mutex
			  :get-mutex
			  :make-mutex
			  :spinlock
			  :get-spinlock
			  :make-spinlock))

(defun thread-support-p () (member :thread-support *features*))

(defun print-thread-info ()
  (let* ((curr-thread sb-thread:*current-thread*)
         (curr-thread-name (sb-thread:thread-name curr-thread))
         (all-threads (sb-thread:list-all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)

(eval-when (:compile-toplevel)
  (defun print-thread-message-top-level (msg)
    (sb-thread:make-thread
     (lambda ()
       (format #.*standard-output* msg)))
    nil))
