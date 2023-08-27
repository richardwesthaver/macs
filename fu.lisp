;;; fu.lisp --- utility functions
(in-package :macs.fu)

;;; From LOL
(defun group (source n)
  (when (zerop n) (error "zero length"))
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
                        #+sbcl
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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                 collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (defun |#f-reader| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (setq numarg (or numarg 3))
    (unless (<= numarg 3)
      (error "Bad value for #f: ~a" numarg))
    `(declare (optimize (speed ,numarg)
                        (safety ,(- 3 numarg))))))

;; Nestable suggestion from Daniel Herring
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun |#"-reader| (stream sub-char numarg)
   (declare (ignore sub-char numarg))
   (let (chars (state 'normal) (depth 1))
     (loop do
          (let ((curr (read-char stream)))
            (cond ((eq state 'normal)
                   (cond ((char= curr #\#)
                          (push #\# chars)
                          (setq state 'read-sharp))
                         ((char= curr #\")
                          (setq state 'read-quote))
                         (t
                          (push curr chars))))
                  ((eq state 'read-sharp)
                   (cond ((char= curr #\")
                          (push #\" chars)
                          (incf depth)
                          (setq state 'normal))
                         (t
                          (push curr chars)
                          (setq state 'normal))))
                  ((eq state 'read-quote)
                   (cond ((char= curr #\#)
                          (decf depth)
                          (if (zerop depth) (return))
                          (push #\" chars)
                          (push #\# chars)
                          (setq state 'normal))
                         (t
                          (push #\" chars)
                          (if (char= curr #\")
                              (setq state 'read-quote)
                              (progn
                                (push curr chars)
                                (setq state 'normal)))))))))
     (coerce (nreverse chars) 'string))))

; This version is from Martin Dirichs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= #\newline curr))
        (push curr chars))
      (let ((pattern (nreverse chars))
            output)
        (labels ((match (pos chars)
                   (if (null chars)
                       pos
                       (if (char= (nth pos pattern) (car chars))
                           (match (1+ pos) (cdr chars))
                           (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
          (do (curr
               (pos 0))
              ((= pos (length pattern)))
            (setf curr (read-char stream)
                  pos (match pos (list curr)))
            (push curr output))
          (coerce
           (nreverse
            (nthcdr (length pattern) output))
           'string))))))

; (set-dispatch-macro-character #\# #\> #'|#>-reader|)

(defun segment-reader (stream ch n)
  (if (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
 ``(lambda (,',g!str)
     (ppcre:scan-to-strings
       ,(if (zerop (length ,g!mods))
          (car ,g!args)
          (format nil "(?~a)~a" ,g!mods (car ,g!args)))
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

#+cl-ppcre
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
        ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          1)
          (coerce (loop for c = (read-char stream)
                     while (alpha-char-p c)
                     collect c
                     finally (unread-char c stream))
                  'string)))
        ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          2)))
        (t (error "Unknown #~~ mode character"))))))

; #+cl-ppcre (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defreadtable macs-syntax
    (:merge :standard)
    (:dispatch-macro-char #\# #\" #'|#"-reader|)
    (:dispatch-macro-char #\# #\> #'|#>-reader|)
    #+cl-ppcre
    (:dispatch-macro-char #\# #\~ #'|#~-reader|)
    (:dispatch-macro-char #\# #\` #'|#`-reader|)
    (:dispatch-macro-char #\# #\f #'|#f-reader|)))

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

(in-readtable macs-syntax)

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


#+cl-ppcre
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

#+cl-ppcre
(defmacro when-match ((match-regex str) &body forms)
  `(if-match (,match-regex ,str)
     (progn ,@forms)))

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

;;; alexandria/macros.lisp
(defmacro with-gensyms (names &body forms)
  "Binds a set of variables to gensyms and evaluates the implicit progn FORMS.

Each element within NAMES is either a symbol SYMBOL or a pair (SYMBOL
STRING-DESIGNATOR). Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL STRING-DESIGNATOR) specifies that the variable named by SYMBOL
should be bound to a symbol constructed using GENSYM with the string designated
by STRING-DESIGNATOR being its first argument."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

(defmacro with-unique-names (names &body forms)
  "Alias for WITH-GENSYMS."
  `(with-gensyms ,names ,@forms))

(defmacro once-only (specs &body forms)
  "Constructs code whose primary goal is to help automate the handling of
multiple evaluation within macros. Multiple evaluation is handled by introducing
intermediate variables, in order to reuse the result of an expression.

The returned value is a list of the form

  (let ((<gensym-1> <expr-1>)
        ...
        (<gensym-n> <expr-n>))
    <res>)

where GENSYM-1, ..., GENSYM-N are the intermediate variables introduced in order
to evaluate EXPR-1, ..., EXPR-N once, only. RES is code that is the result of
evaluating the implicit progn FORMS within a special context determined by
SPECS. RES should make use of (reference) the intermediate variables.

Each element within SPECS is either a symbol SYMBOL or a pair (SYMBOL INITFORM).
Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL INITFORM) specifies a single intermediate variable:

- INITFORM is an expression evaluated to produce EXPR-i

- SYMBOL is the name of the variable that will be bound around FORMS to the
  corresponding gensym GENSYM-i, in order for FORMS to generate RES that
  references the intermediate variable

The evaluation of INITFORMs and binding of SYMBOLs resembles LET. INITFORMs of
all the pairs are evaluated before binding SYMBOLs and evaluating FORMS.

Example:

  The following expression

  (let ((x '(incf y)))
    (once-only (x)
      `(cons ,x ,x)))

  ;;; =>
  ;;; (let ((#1=#:X123 (incf y)))
  ;;;   (cons #1# #1#))

  could be used within a macro to avoid multiple evaluation like so

  (defmacro cons1 (x)
    (once-only (x)
      `(cons ,x ,x)))

  (let ((y 0))
    (cons1 (incf y)))

  ;;; => (1 . 1)

Example:

  The following expression demonstrates the usage of the INITFORM field

  (let ((expr '(incf y)))
    (once-only ((var `(1+ ,expr)))
      `(list ',expr ,var ,var)))

  ;;; =>
  ;;; (let ((#1=#:VAR123 (1+ (incf y))))
  ;;;   (list '(incf y) #1# #1))

  which could be used like so

  (defmacro print-succ-twice (expr)
    (once-only ((var `(1+ ,expr)))
      `(format t \"Expr: ~s, Once: ~s, Twice: ~s~%\" ',expr ,var ,var)))

  (let ((y 10))
    (print-succ-twice (incf y)))

  ;;; >>
  ;;; Expr: (INCF Y), Once: 12, Twice: 12"
  (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                         gensyms names-and-forms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defun parse-ordinary-lambda-list (lambda-list &key (normalize t)
                                   allow-specializers
                                   (normalize-optional normalize)
                                   (normalize-keyword normalize)
                                   (normalize-auxilary normalize))
  "Parses an ordinary lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   (name init suppliedp)

3. Name of the rest parameter, or NIL.

4. Keyword parameter specifications, normalized into form:

   ((keyword-name name) init suppliedp)

5. Boolean indicating &ALLOW-OTHER-KEYS presence.

6. &AUX parameter specifications, normalized into form

   (name init).

7. Existence of &KEY in the lambda-list.

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt))
           (setf keyp t))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
              (check-variable elt "required parameter")
              (push elt required))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             ((and normalize-optional tail)
                              (setf elt (append elt '(nil))))
                             (normalize-optional
                              (setf elt (append elt '(nil nil)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil nil))))))
              (push (ensure-list elt) optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (symbolp keyword)
                                  (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                        keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (when normalize-keyword
                                (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv)))))
                       (cond ((cdr tail)
                              (check-spec tail "keyword-supplied-p parameter"))
                             ((and normalize-keyword tail)
                              (setf tail (append tail '(nil))))
                             (normalize-keyword
                              (setf tail '(nil nil))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (make-keyword elt) elt) nil nil)
                                   elt))))
              (push elt keys))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (progn
                    (check-variable elt "&aux parameter")
                    (setf elt (list* elt (when normalize-auxilary
                                           '(nil))))))
              (push elt aux))
             (t
              (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

;;;; DESTRUCTURING-*CASE

(defun expand-destructuring-case (key clauses case)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case (car ,key)
           ,@(mapcar (lambda (clause)
                       (destructuring-bind ((keys . lambda-list) &body body) clause
                         `(,keys
                           (destructuring-bind ,lambda-list (cdr ,key)
                             ,@body))))
                     clauses))
         (error "Invalid key to DESTRUCTURING-~S: ~S" ',case ,key))))

(defmacro destructuring-case (keyform &body clauses)
  "DESTRUCTURING-CASE, -CCASE, and -ECASE are a combination of CASE and DESTRUCTURING-BIND.
KEYFORM must evaluate to a CONS.

Clauses are of the form:

  ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

The clause whose CASE-KEYS matches CAR of KEY, as if by CASE, CCASE, or ECASE,
is selected, and FORMs are then executed with CDR of KEY is destructured and
bound by the DESTRUCTURING-LAMBDA-LIST.

Example:

 (defun dcase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar: ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))
     ((t &rest rest)
      (format nil \"unknown: ~S\" rest))))

  (dcase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (dcase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (dcase (list :alt1 1))         ; => \"alt: 1\"
  (dcase (list :alt2 2))         ; => \"alt: 2\"
  (dcase (list :quux 1 2 3))     ; => \"unknown: 1, 2, 3\"

 (defun decase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar: ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase (list :alt1 1))         ; => \"alt: 1\"
  (decase (list :alt2 2))         ; => \"alt: 2\"
  (decase (list :quux 1 2 3))     ; =| error
"
  (expand-destructuring-case keyform clauses 'case))

(defmacro destructuring-ccase (keyform &body clauses)
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro destructuring-ecase (keyform &body clauses)
  (expand-destructuring-case keyform clauses 'ecase))

(dolist (name '(destructuring-ccase destructuring-ecase))
  (setf (documentation name 'function) (documentation 'destructuring-case 'function)))
