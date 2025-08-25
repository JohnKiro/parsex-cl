(in-package :parsex-cl/class-util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list-of-symbols-p (x)
    (or (null x)
        (and (consp x)
             (every #'symbolp x))))

  (deftype list-of-symbols ()
    '(satisfies list-of-symbols-p)))

(defmacro define-class-of-functions (class-name direct-superclasses &key doc slots)
  "Utility macro to simplify creation of a class where all slots are functions. Each slot in the
`slots` argument is expected in the form (slot :doc doc-string).
Note that the main reason I chose to use a class instead of a struct is to be able to provide a
documentation string for each slot (i.e. for each function).
Also note that I'm using the :doc keyword since it is treated specially by Slime, to highlight the
string in special color, indicating documentation."
  (declare (type symbol class-name)
           (type list-of-symbols direct-superclasses)
           (type (or null string) doc)
           (type list slots))
  `(defclass ,class-name ,direct-superclasses
     ,(loop for (slot-name &key doc) in slots
            collect `(,slot-name :initarg ,(sym:sym-to-kw slot-name)
                                 :reader ,slot-name
                                 :initform (error "Must provide ~a!" ',slot-name)
                                 :type function
                                 ,@(when doc `(:documentation ,doc))))
     ,@(when doc (list `(:documentation ,doc)))))

(defmacro define-class-of-functions-constructor (class-name constructor-name (&rest slots))
  "Utility macro to create a construction macro for a given class of functions, to be used instead
of `make-instance`. Its sole usefulness is that it allows the user to provide each function's body,
without enclosing it inside a '(lambda () ....)' form. I.e. it's typically used when creating
closures, not when the functions are already created (and hence specified with a hash-quote, such
as #'function-name).
Note that this is a limited implementation, where all functions take no arguments. If this is not
the case, then use the `make-instance` method instead (which is the more general case).
Also note that it supports passing function objects besides code to be wrapped in a LAMBDA."
  (declare (type list-of-symbols slots))
  (flet ((prepare-arg-key-and-val (arg-name)
           "Prepare key and value arg expansion code for `make-instance`, given the arg name."
           (list (sym:sym-to-kw arg-name)
                 `(if (typep ,arg-name 'cons)
                      (if (eq (car ,arg-name) 'function)
                          ,arg-name
                          `#'(lambda ()
                               ,,arg-name))
                      (error "Invalid function argument for ~a!" ',arg-name)))))
    `(defmacro ,constructor-name (&key ,@slots)
       (let ((args (list ,@(loop for slot-name in slots
                                 append (prepare-arg-key-and-val slot-name)))))
         `(make-instance ',',class-name ,@args)))))

(defmacro define-class-of-functions-with-constructor (class-name direct-superclasses
                                                      &key doc slots constructor-name)
  "Combines `define-class-of-functions` and `define-class-of-functions-constructor` to create
both the class and its corresponding constructor macro. Refer to the documentation of these two
macros for more details.
Note: if the `constructor-name` is not provided (absent or nil), then no constructor macro is
created. This would typically be the case when some of the functions take arguments, so
`make-instance` would be used instead of a constructor macro (as explained in the documentaion of
`define-class-of-functions-constructor`)."
  (declare (type symbol class-name constructor-name)
           (type list-of-symbols direct-superclasses)
           (type (or null string) doc)
           (type list slots))
  (let ((slot-names (mapcar #'first slots)))
    `(progn
       (define-class-of-functions ,class-name ,direct-superclasses :doc ,doc :slots ,slots)
       ,@(when constructor-name
           `((define-class-of-functions-constructor ,class-name ,constructor-name ,slot-names))))))

(defmacro let-slots ((&rest vars-and-slot-readers) obj &body body)
  "Macro receiving a list of elements in the form (slot-var . slot-reader), and expanding into code
that LET-binds each slot value (by calling `slot-reader`) to corresponding `slot-var`, around
`body`. This is useful in cases where the slots are not modified, and we want to avoid the cost of
reading the slot over and over (e.g. in tight loops)."
  `(let ,(loop for (slot-var . slot-reader) in vars-and-slot-readers
               collect `(,slot-var (,slot-reader ,obj)))
     ,@body))

(defmacro with-function-slots-funcall-macros ((&rest macro-names-and-slot-readers) obj &body body)
  "Macro that defines local macros that expand to FUNCALLing function slots in provided object
`obj`, to be available in `body`. The `macro-names-and-slot-readers` argument is a list of elements
in the form (macro-name slot-reader). Slot readers must match object's slot readers. Macros will
accept a variable number of arguments."
  (let* ((macro-names (mapcar #'car macro-names-and-slot-readers))
         (slot-readers (mapcar #'cdr macro-names-and-slot-readers))
         (vars-and-slot-readers (mapcar #'(lambda (e) (cons (gensym) e)) slot-readers)))
    `(let-slots ,vars-and-slot-readers ,obj
       (macrolet
           ,(loop for (slot-var . slot-reader) in vars-and-slot-readers
                  for macro-name in macro-names
                  collect `(,macro-name (&rest args)
                                        `(funcall ,',slot-var ,@args)))
         ,@body))))
