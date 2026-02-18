(in-package :parsex-cl/functional-interface)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun prepare-function-slot-name (func-name)
    "Prepares slot name for function identified by the argument `func-name`, by appending '-FN'.
For example, 'IS-EMPTY-P' becomes 'IS-EMPTY-P-FN'."
    (concatenate 'string (symbol-name func-name) "-FN"))

  (defun prepare-function-slot (struct-name func-name)
    (let ((struct-pkg (symbol-package struct-name)))
      (intern (prepare-function-slot-name func-name) struct-pkg)))

  (defun compose-struct-slot-accessor (struct-name func-name)
    (let ((struct-pkg (symbol-package struct-name)))
      (intern (concatenate 'string (symbol-name struct-name) "-" (prepare-function-slot-name func-name))
              struct-pkg)))

  (defstruct lambda-list-arg-details
    (required-args nil :type list)
    (optional-args nil :type list)
    (rest-arg nil :type symbol)
    (keyword-args nil :type list))

  (defun lambda-list-to-args (lambda-list)
    "Analyzes a lambda list, such as '(arg1 arg2 &optional arg3 &key (arg4 10)), and extracts argument
names into a `lambda-list-arg-details` struct, with separate slots for required, optional, rest,
keywords.
Note that all slots are lists, except the rest arg, which is a symbol (argument name).
TODO: I'm not handling &allow-other-keys for now (TODO: check the impact!!)."
    (multiple-value-bind (required optional rest keyword) (alex:parse-ordinary-lambda-list lambda-list)
      (make-lambda-list-arg-details :required-args required
                                    :optional-args (mapcar #'car optional)
                                    :rest-arg rest
                                    :keyword-args (loop for ((kw-name name) init _) in keyword
                                                        append (list kw-name name)))))

  (defun prepare-args-form (lambda-list-arg-details)
    "Uses argument names provided in the struct `lambda-list-arg-details`, and prepares a form of args,
suitable to be passed to funcall. Note the following:
- In case a &rest argument is included, we skip the keywords, since they will be included in the &rest
argument (after being evaluated during macro invocation). Normally this situation should be avoided (i.e.
both &rest and &keyword together!).
- Also should avoid having &optional and &keyword together. This and the previous advices are general
Common Lisp recommendations."
    (let ((rest-arg (lambda-list-arg-details-rest-arg lambda-list-arg-details)))
      (values (append (lambda-list-arg-details-required-args lambda-list-arg-details)
                      (lambda-list-arg-details-optional-args lambda-list-arg-details)
                      (if rest-arg
                          (list rest-arg)
                          (lambda-list-arg-details-keyword-args lambda-list-arg-details)))
              ;; TODO: probably no more need for this second value
              rest-arg))))

(defmacro define-functional-interface (interface-name (&optional included-interface) &body body)
  "Define a functional interface, composed of a struct containing function objects (closures), and a set
of macros to provide the user with a simple interface to call those functions, using funcall, on the
corresponding struct's slot.
Arguments:
- `interface-name`: interface name, which will be given to the struct as a name.
- `included-interface`: included struct (optional).
- `body`: definitions of the functions, each in the form (func-name lambda-list &key doc), where;
- `func-name`:the function's name, which will be also used as a name for the corresponding macro.
- `lambda-list`: the argument list, as will be expected by the macro.
- `doc`: optional keyword argument, containing documentation for the function. It will be used as
docstring for the corresponding macro.
Note: the macro also takes a struct instance as the first argument. This is how it will access the
corresponding function object.
Example usage:
(define-functional-interface %interface-of-two-funcs ()
  (func1 (arg1 arg2 &key arg3) :doc \"this is func1.\")
  (func2 (arg1 arg2 &key arg3) :doc \"this is func2.\"))"
  (multiple-value-bind (functions declarations interface-doc) (alex:parse-body body :documentation t)
    (when declarations
      (error "Declarations are not allowed!"))
    (let ((slots nil)
          (macro-defs nil))
      (dolist (function functions)
        (destructuring-bind (func-name lambda-list &key doc) function
          (let ((slot-name (prepare-function-slot interface-name func-name))
                (slot-accessor-name (compose-struct-slot-accessor interface-name func-name))
                (arg-list (prepare-args-form (lambda-list-to-args lambda-list))))
            (push `(,slot-name nil :type function) slots)
            (push `(defmacro ,func-name (,interface-name ,@lambda-list) ,@(when doc (list doc))
                     `(funcall (,',slot-accessor-name ,,interface-name) ,,@arg-list))
                  macro-defs))))
      `(progn (defstruct (,interface-name ,@(when included-interface
                                              (list `(:include ,included-interface))))
                ,@(when interface-doc (list interface-doc))
                ,@(nreverse slots))
              ,@(nreverse macro-defs)))))
