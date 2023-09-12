;;; fmt.lisp --- printer and format utils

;;; Code:
(in-package :macs.fmt)

(reexport-from :uiop :include '(println writeln))

(defun printer-status ()
  (format t ";;           *print-array* = ~a~%" *print-array*)
  (format t ";;            *print-base* = ~a~%" *print-base*)
  (format t ";;            *print-case* = ~a~%" *print-case*)
  (format t ";;          *print-circle* = ~a~%" *print-circle*)
  (format t ";;          *print-escape* = ~a~%" *print-escape*)
  (format t ";;          *print-gensym* = ~a~%" *print-gensym*)
  (format t ";;          *print-length* = ~a~%" *print-length*)
  (format t ";;           *print-level* = ~a~%" *print-level*)
  (format t ";;           *print-lines* = ~a~%" *print-lines*)
  (format t ";;     *print-miser-width* = ~a~%" *print-miser-width*)
  (format t ";; *print-pprint-dispatch* = ~a~%" *print-pprint-dispatch*)
  (format t ";;          *print-pretty* = ~a~%" *print-pretty*)
  (format t ";;           *print-radix* = ~a~%" *print-radix*)
  (format t ";;        *print-readably* = ~a~%" *print-readably*)
  (format t ";;    *print-right-margin* = ~a~%" *print-right-margin*))

;;; Tables
(defun fmt-row (data)
  (format nil "| ~{~A~^ | ~} |~%" data))

;;; IDs
(defun fmt-sxhash (code)
  "Turn the fixnum value CODE into a human-friendly string. CODE should
be produced by `sxhash'."
  (let (r)
    (dotimes (i 8 r)
      (push (ldb (byte 8 (* i 8)) code) r))
    (format
     nil
     "~{~A~^-~}"
     (mapcar
      (lambda (x) (format nil "~{~(~2,'0x~)~}" x))
      (group r 2)))))
