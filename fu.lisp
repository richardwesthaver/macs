;;; fu.lisp --- utility functions
(in-package :fu)

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun flatten (x)
    (labels ((rec (x acc)
                  (cond ((null x) acc)
                        #+(and sbcl (not lol::old-sbcl))
                        ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                        ((atom x) (cons x acc))
                        (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
      (rec x nil)))

  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2))))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro/g! ,name ,args
         ,@(when docstring
            (list docstring))
         ,@declarations
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body))))))

(defmacro defun! (name args &body body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar (lambda (s)
                         `(,s (gensym ,(subseq (symbol-name s)
                                               2))))
                       syms)
           ,@body)))))

(defun segment-reader (stream ch n)
  (if (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                         g!args
                         `(cdr ,g!args)))))
           ds))))

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil)) ;; For gc
      (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defun build-batcher-sn (n)
  (let* (network
         (tee (ceiling (log n 2)))
         (p (ash 1 (- tee 1))))
    (loop while (> p 0) do
      (let ((q (ash 1 (- tee 1)))
            (r 0)
            (d p))
        (loop while (> d 0) do
          (loop for i from 0 to (- n d 1) do
            (if (= (logand i p) r)
              (push (list i (+ i d))
                    network)))
          (setf d (- q p)
                q (ash q -1)
                r p)))
      (setf p (ash p -1)))
    (nreverse network)))

(defmacro! sortf (comparator &rest places)
  (if places
    `(tagbody
       ,@(mapcar
           #`(let ((,g!a #1=,(nth (car a1) places))
                   (,g!b #2=,(nth (cadr a1) places)))
               (if (,comparator ,g!b ,g!a)
                 (setf #1# ,g!b
                       #2# ,g!a)))
           (build-batcher-sn (length places))))))

#+cl-ppcre
(defun dollar-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 1)
       (string= (symbol-name s)
                "$"
                :start1 0
                :end1 1)
       (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))

(defmacro! if-match ((match-regex str) then &optional else)
  (let* ((dollars (remove-duplicates
                   (remove-if-not #'dollar-symbol-p
                                  (flatten then))))
         (top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>))
                  0)))
    `(multiple-value-bind (,g!matches ,g!captures) (,match-regex ,str)
       (declare (ignorable ,g!matches ,g!captures))
       (let ((,g!captures-len (length ,g!captures)))
         (declare (ignorable ,g!captures-len))
         (symbol-macrolet ,(mapcar #`(,(symb "$" a1)
                                       (if (< ,g!captures-len ,a1)
                                           (error "Too few matchs: ~a unbound." ,(mkstr "$" a1))
                                           (aref ,g!captures ,(1- a1))))
                                   (loop for i from 1 to top collect i))
           (if ,g!matches
               ,then
               ,else))))))


(defmacro when-match ((match-regex str) &body forms)
  `(if-match (,match-regex ,str)
     (progn ,@forms)))
