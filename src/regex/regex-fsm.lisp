(in-package :parsex-cl/regex/fsm)

;;;; This package is for common behavior to both FSM types (NFA, DFA)

(defgeneric traverse-fsm-transitions (root-state traversal-fn)
  (:documentation "Traverse an FSM starting from a ROOT-STATE, with traversal function
TRAVERSAL-FN. The traversal function expects three parameters: origin state, transition element,
destination state."))

(defgeneric fsm-acceptance-state-p (fsm-state)
  (:documentation "Predicate to indicate whether the state is acceptance or not. Initially it will
be used only in GraphViz export, to give acceptance nodes different style."))

(defgeneric fsm-dead-end-state-p (fsm-state)
    (:documentation "Predicate to indicate whether the state is dead-end or not. Initially it will
be used only in GraphViz export, to give dead-end nodes different style."))

;; TODO: may move to a graph util package (traversal), or: remove overlap in features with
;; traverse-fsm-transitions!!!
(defmacro with-unique-visit ((item-var root-item traversal-func-name
                              &key (lookup-test 'eq)) &body body)
  "Expands into code that ensures the code provided by `body` is executed only once, for each
traversed item. The body is used to create a LABEL, with the name as specified in
traversal-func-name`, taking a single argument that will have the name specified with the `item-var`
symbol.
The traversal function is typically recursive (i.e. called within the `body`).
The `root-item` is passed to the traversal function to initiate the recursive (depth-first)
traversal. Finally, the `lookup-test` specifies the equality function to be used in the traversal
lookup."
  (alexandria:with-gensyms (traversal-lookup-table-var)
    `(let ((,traversal-lookup-table-var (make-hash-table :test ',lookup-test)))
       (labels ((,traversal-func-name (,item-var)
                  (unless #1=(gethash ,item-var ,traversal-lookup-table-var)
                          ;;unfortunately #1= in UNLESS confuses indentation
                          (setf #1# t)
                          ,@body)))
         (,traversal-func-name ,root-item)))))
