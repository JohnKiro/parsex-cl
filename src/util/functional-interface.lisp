(in-package :parsex-cl/functional-interface)

(declaim (type (integer 1 2) *interface-version*))
(defparameter *interface-version* 2 "With version 2, the macro accepts function object as argument.legacy
code would use 1.")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun append-suffix-to-symbol (sym suffix)
    "Append a string suffix `suffix` to a symbol `sym`, and intern the result in the same package as
the input symbol's. Returns the interned symbol."
    (declare (type symbol sym)
             (type string suffix))
    (let ((pkg (symbol-package sym))
          (sym-name (symbol-name sym)))
      (intern (concatenate 'string sym-name suffix) pkg)))

  (defun prepend-prefix-to-symbol (sym prefix)
    "Prepend a string prefix `prefix` to a symbol `sym`, and intern the result in the same package as
the input symbol's. Returns the interned symbol."
    (declare (type symbol sym)
             (type string prefix))
    (let ((pkg (symbol-package sym))
          (sym-name (symbol-name sym)))
      (intern (concatenate 'string prefix sym-name) pkg)))

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
              rest-arg)))

  (defclass interface-function-metadata ()
    ((%func-name :reader func-name
                 :type symbol
                 :documentation "Function name that will be exposed to the user.")
     (%func-slot-name :reader func-slot-name
                      :type symbol
                      :documentation "Function slot name, as defined internally in the FI struct. It will
be created based on the function name, by appending '-FN'.")
     (%func-slot-accessor-name :reader func-slot-accessor-name
                               :type symbol
                               :documentation "Function slot accessor name. It will be created based on
the interface name and function slot name following the default struct scheme (struct-slot).")
     (%func-lambda-list-raw :reader func-lambda-list-raw
                            :type list
                            :documentation "Lambda list that the function accepts, in its raw form.")
     (%func-lambda-list-details :reader func-lambda-list-details
                                :type lambda-list-arg-details
                                :documentation "Details of lambda list that the function accepts.")
     (%func-lambda-list-form :reader func-lambda-list-form
                             :type list
                             :documentation "Arguments in the form of use (e.g. in a funcall form).")
     (%func-doc :reader func-doc
                :type string
                :documentation "Documentation string for the interface function."))
    (:documentation "Helper class, consolidating all meta-data for a specific function in the interface,
for use by the macros to create the interface."))

  (defmethod initialize-instance :after ((obj interface-function-metadata) &key function interface-name)
    "Initialize the object based on the function's raw details provided in the `function` parameter,
which is in the same format as extracted from the body of the `define-functional-interface` macro."
    (destructuring-bind (func-name lambda-list &key doc) function
      (with-slots (%func-name
                   %func-slot-name
                   %func-slot-accessor-name
                   %func-lambda-list-raw
                   %func-lambda-list-details
                   %func-lambda-list-form
                   %func-doc)
          obj
        (setf %func-name func-name
              %func-slot-name (append-suffix-to-symbol func-name "-FN")
              %func-slot-accessor-name (compose-struct-slot-accessor interface-name func-name)
              %func-lambda-list-raw lambda-list
              %func-lambda-list-details (lambda-list-to-args lambda-list)
              %func-lambda-list-form (prepare-args-form %func-lambda-list-details)
              %func-doc doc))))

  (defun prepare-funcall-macro-definitions (interface-name interface-metadata-objects)
    "Prepare funcall macros, given the interface name and metadata."
    (declare (type symbol interface-name)
             (type list interface-metadata-objects))
    (let ((macro-defs nil))
      (dolist (imo interface-metadata-objects)
        (class-util:let-slots ((func-name . func-name)
                               (slot-accessor-name . func-slot-accessor-name)
                               (lambda-list .  func-lambda-list-raw)
                               (arg-list . func-lambda-list-form)
                               (docstring . func-doc))
                              imo
          (let* ((interface-name-arg (append-suffix-to-symbol interface-name "-OBJ"))
                 (complete-lambda-list (case *interface-version*
                                         (1 `(,interface-name-arg ,@lambda-list))
                                         (2 `((&key ,interface-name-arg function-obj) ,@lambda-list))))
                 (funcall-arg
                   (case *interface-version*
                     (1 `(cond
                           (,interface-name-arg `(,',slot-accessor-name ,,interface-name-arg))
                           (t (error "Must specify interface object!"))))
                     (2 `(cond
                           (function-obj function-obj)
                           (,interface-name-arg `(,',slot-accessor-name ,,interface-name-arg))
                           (t (error "Must specify either function object or interface object!")))))))
            (push `(defmacro ,func-name ,complete-lambda-list
                     ,@(when docstring (list docstring))
                     `(funcall ,,funcall-arg ,,@arg-list))
                  macro-defs))))
      macro-defs))

  (defun prepare-with-macro-definition (interface-name interface-metadata-objects)
    "Prepare WITH-<interface-name> macro, given the interface name and metadata."
    (declare (type symbol interface-name)
             (type list interface-metadata-objects))
    (let* ((the-with-macro-name (prepend-prefix-to-symbol interface-name "WITH-"))
           (func-names (loop for imo in interface-metadata-objects
                             collect (slot-value imo '%func-name))))
      `(defmacro ,the-with-macro-name ((interface-object &key ,@func-names) &body body)
         (let* ((let-slot-binding-forms-unfiltered
                  (list ,@(loop for imo in interface-metadata-objects
                                for func-name in func-names
                                for accessor-name = (slot-value imo '%func-slot-accessor-name)
                                collect `(when ,func-name
                                           (cons ,func-name ',accessor-name)))))
                (let-slot-binding-forms
                  (delete nil let-slot-binding-forms-unfiltered)))
           `(class-util:let-slots ,let-slot-binding-forms ,interface-object
              ,@(cons `(declare (type ,',interface-name ,interface-object))
                      body)))))))

;; TODO: consider optionally specifying naming for the functions, to ensure they are in same package as
;; the struct itself (e.g. in case interface name is specified with package (mypkg:my-func-interface),
;; in which case, the functions (make-... etc.) will be defined in current package, not 'mypkg'.
;; example:
;;
;; (defstruct (cl-user::my-struct
;;              (:conc-name cl-user::my-struct-)
;;              (:constructor cl-user::make-my-struct)
;;              (:predicate cl-user::my-struct-p)
;;              (:copier cl-user::copy-my-struct))
;;  slot1 ...)
(defmacro %define-functional-interface (interface-name (&optional included-interface) &body body)
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
The function object FUNCALLed by the macro can be specified in the macro invocation in one of two ways:
directly, by passing the `function-obj` keyword parameter, or the macro can retrieve it from the
interface object, also passed as keyword argument. Priority is given to the function argument, and the
interface object is used only if the function argument is not provided. If both are missing, an error
will be thrown during macro invocation. Note that version 1 of the interface does not support passing the
function object, and the interface object is passed as positional argument. This macro is internal, and
is used by wrapper public macros that set the version (`*interface-version*` variable) before expansion.
The wrapper macro for version 2 is found in this package, while version 1 is handled in a separate
package."
  (declare (type symbol interface-name included-interface))
  (multiple-value-bind (functions declarations interface-doc) (alex:parse-body body :documentation t)
    (when declarations
      (error "Declarations are not allowed!"))
    (let ((slots nil)
          (interface-metadata-objects (mapcar (lambda (func)
                                                (make-instance 'interface-function-metadata
                                                               :interface-name interface-name
                                                               :function func))
                                              functions)))
      (dolist (imo interface-metadata-objects)
        (class-util:let-slots ((slot-name . func-slot-name)) imo
          (push `(,slot-name nil :type function) slots)))
      `(progn (defstruct (,interface-name ,@(when included-interface
                                              (list `(:include ,included-interface))))
                ,@(when interface-doc (list interface-doc))
                ,@(nreverse slots))
              ,(prepare-with-macro-definition interface-name interface-metadata-objects)
              ,@(prepare-funcall-macro-definitions interface-name interface-metadata-objects)))))

(defmacro define-functional-interface (interface-name (&optional included-interface) &body body
                                       &environment env)
  "Define a functional interface, composed of a struct containing function objects (closures), and a set
of macros to provide the user with a simple interface to call those functions, using funcall, on the
corresponding struct's slot. For more details, refer to documentation of `%define-functional-interface`.
Example usage:
```
(define-functional-interface spaceship ()
  \"Spaceship for games.\"
  (accelerate (rate) :doc \"Accelerate spaceship by `rate`.\")
  (fire-missile (direction) :doc \"Fire missile in specified `direction`.\"))

(define-functional-interface shielded-spaceship (spaceship)
  \"Shielded spaceship for games.\"
  (activate-shield (&key duration) :doc \"Activate shield for specific `duration` in seconds.\"))

(let ((my-shielded-spaceship
        (make-shielded-spaceship
         :accelerate-fn (lambda (rate)
                          (format t \"Accelerating by ~a..~%\" rate))
         :fire-missile-fn (lambda (direction)
                            (format t \"Firing in ~a direction..~%\" direction))
         :activate-shield-fn (lambda (&key duration)
                               (format t \"Activating shield for ~a seconds..~%\" duration)))))
  (with-shielded-spaceship (my-shielded-spaceship :activate-shield activate-shield-func)
    (with-spaceship (my-shielded-spaceship :accelerate accelerate-func :fire-missile fire-missile-func)
      (activate-shield (:shielded-spaceship-obj my-shielded-spaceship) :duration 100)
      (accelerate (:function-obj accelerate-func) 10)
      (fire-missile (:spaceship-obj my-shielded-spaceship) 'north))))
```
Note in the previous example how we needed to use separate WITH- macro invocations for parent and child
interfaces. A more sophisticated implementation would retrieve interface functions from parent, and
providing them to the child's WITH-, but I prefer simplicity, which still works, though a bit more
verbose (may reconsider in the future though)."
  (let ((*interface-version* 2))
    ;; ensure expansion in current context, with version = 1
    (macroexpand-1 `(%define-functional-interface ,interface-name ,(when included-interface
                                                                     (list included-interface))
                      ,@body)
                   env)))
