;;; sym.lisp --- inspired by alexandria/symbols.lisp
(in-package :macs.sym)

(declaim (inline ensure-symbol))
(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example:

  (ensure-symbol :cons :cl) => cl:cons, :external
"
  (intern (string name) package))

(defun maybe-intern (name package)
  (values
   (if package
       (intern name (if (eq t package) *package* package))
       (make-symbol name))))

(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to string designator CONTROL as
if by FORMAT within WITH-STANDARD-IO-SYNTAX, and then creates a symbol named
by that string.

If PACKAGE is NIL, returns an uninterned symbol, if package is T, returns a
symbol interned in the current package, and otherwise returns a symbol
interned in the package designated by PACKAGE."
  (maybe-intern (with-standard-io-syntax
                  (apply #'format nil (string control) arguments))
                package))

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

(defmacro make-slot-name (name)
  "make slot-name"
  `(intern ,(string-upcase name) :keyword))

(defun make-gensym (name)
  "If NAME is a non-negative integer, calls GENSYM using it. Otherwise NAME
must be a string designator, in which case calls GENSYM using the designated
string as the argument."
  (gensym (if (typep name '(integer 0))
              name
              (string name))))

(defun make-gensym-list (length &optional (x "G"))
  "Returns a list of LENGTH gensyms, each generated as if with a call to MAKE-GENSYM,
using the second (optional, defaulting to \"G\") argument."
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop repeat length
          collect (gensym g))))

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

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))
