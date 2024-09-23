(in-package :parsex-cl.regex-nfa)

(defgeneric regex-to-nfa (regex input-nfa-state)
  (:documentation "Parses REGEX and generates corresponding NFA section, starting at
INPUT-NFA-STATE. In the process, it creates as many states for the element, glues with input state,
and returns output state as continuation point."))

(defgeneric compound-regex-to-nfa (regex-head regex-tail input-nfa-state)
  (:documentation "Parses a compound REGEX and generates corresponding NFA section, starting at
 INPUT-NFA-STATE."))

(defmethod regex-to-nfa ((regex character) input-nfa-state)
  (let ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

;; NOTE: the only symbol currently supported is :any-char, but I chose to have more generic code
;; than specializing on (eql :any-char).
(defmethod regex-to-nfa ((regex symbol) input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex cons) input-nfa-state)
  (compound-regex-to-nfa (car regex) (cdr regex) input-nfa-state))

(defmethod regex-to-nfa ((regex string) input-nfa-state)
  (loop for ch across regex
        for in-state = input-nfa-state then out-state
        for out-state = (make-instance 'nfa-state)
        do (add-nfa-normal-transition in-state ch out-state)
        finally (return out-state)))

(defmethod compound-regex-to-nfa ((regex-head (eql :char-range)) regex-tail input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (destructuring-bind (char-start char-end) regex-tail
      (add-nfa-normal-transition input-nfa-state
                                 (make-instance 'chars:char-range
                                                :char-start char-start
                                                :char-end char-end) output-state)
      output-state)))

(defmethod compound-regex-to-nfa ((regex-head (eql :seq)) regex-tail input-nfa-state)
  (reduce #'(lambda (previous-output-nfa-state elem)
              (regex-to-nfa elem previous-output-nfa-state))
          regex-tail
          :initial-value input-nfa-state))

(defmethod compound-regex-to-nfa ((regex-head (eql :or)) regex-tail input-nfa-state)
  (let ((output-state (make-instance 'nfa-state)))
    (map nil
         #'(lambda (elem)
             (let ((out-state-i (regex-to-nfa elem input-nfa-state)))
               (add-nfa-auto-transition out-state-i output-state)))
         regex-tail)
    output-state))

(defmethod compound-regex-to-nfa ((regex-head (eql :?)) regex-tail input-nfa-state)
  (let* ((actual-regex (first regex-tail))
         (output-state (regex-to-nfa actual-regex input-nfa-state)))
    (add-nfa-auto-transition input-nfa-state output-state)
    output-state))

;;TODO: SIMPLIFY, AS DONE FOR :+ BELOW
(defmethod compound-regex-to-nfa ((regex-head (eql :*)) regex-tail input-nfa-state)
  (let* ((s1 (make-instance 'nfa-state))
         (actual-regex (first regex-tail))
         (s2 (regex-to-nfa actual-regex s1))
         (output-state (make-instance 'nfa-state)))
    (add-nfa-auto-transition input-nfa-state s1)
    (add-nfa-auto-transition input-nfa-state output-state)
    (add-nfa-auto-transition s2 s1)
    (add-nfa-auto-transition s2 output-state)
    output-state))

(defmethod compound-regex-to-nfa ((regex-head (eql :+)) regex-tail input-nfa-state)
  (let* ((actual-regex (first regex-tail))
         (s1 (regex-to-nfa actual-regex input-nfa-state))
         (s2 (regex-to-nfa actual-regex s1)))
    (add-nfa-auto-transition s2 s1)
    (add-nfa-auto-transition s1 s2)
    s2))

(defmethod compound-regex-to-nfa ((regex-head (eql :not)) regex-tail input-nfa-state)
  (error "TODO: IMPLEMENT THE NOT-ELEMENT!"))

(defun parse-and-produce-nfa (regex)
  (let* ((root-state (make-instance 'nfa-state))
         (terminus-nfa-state (regex-to-nfa regex root-state)))
    (setf (terminus terminus-nfa-state) t)
    root-state))

(defclass nfa-state ()
  ((normal-transitions :initform nil :type list :accessor normal-transitions)
   (auto-transitions :initform nil :type list :accessor auto-transitions)
   ;;NOTE: terminus state will not have any normal transitions, so may enhance by
   ;;prohibiting incosistency (introduce class hierarchy level).
   ;;However, terminus state is not known when the state is constructed, so cannot determine its
   ;;type beforehand. It's still possible to change CLOS class, but probably not worth the
   ;;complexity.
   (terminus :initform nil :type (or null t) :accessor terminus)))

;;;TODO: CHECK WITH NOT-ELEMENT above (remove one of them?)
(defclass negated-nfa-state (nfa-state)
  ((negated-state :initform (error "Negated state is mandatory!") :type nfa-state)))


;;; defines a normal NFA transition upon matching of ELEMENT, to NEXT-STATE.
;;; TODO: for now, element type restricted to one of three types with character hardcoded.

(defclass nfa-transition ()
  ((element :initarg :element
            :initform (error "element must be specified!")
            :type (or character chars:char-range (eql :any-char))
            :reader element)
   (next-state :initarg :next-state
               :initform (error "next-state must be specified!")
               :type (or null nfa-state)
               :reader next-state)))

(defun add-nfa-normal-transition (orig-state element dest-state)
  "Add NFA transition from ORIG-STATE, on ELEMENT (chararcter/char-range/any-char), to DEST-STATE."
  (let ((transition (make-instance 'nfa-transition :element element :next-state dest-state)))
    (push transition (normal-transitions orig-state))))

(defun add-nfa-auto-transition (orig-state dest-state)
  "Add NFA auto transition from ORIG-STATE to DEST-STATE."
  (push dest-state (auto-transitions orig-state)))

#+nil(defun add-nfa-special-transition (orig-state element dest-state)
       "Add NFA transition from ORIG-STATE, on special (symbol-based) ELEMENT, to DEST-STATE. Currently,
the following special elements are defined: :any-char, :any-other-char)"
       (ecase element
         (:any-char (push dest-state (transitions-on-any-char orig-state)))
         (:any-other-char (push dest-state (transitions-on-any-other-char orig-state)))))

;;; NFA state defines a normal transition table (element --> next state), E-transitions (auto),
;;; and a default transition (transition on any other input, including case no input). Also it
;;; includes a negated-state which is used in case the state is result of expansion of a NOT
;;; element

;;; predicate - whether NFA state is a result of a NOT
(defun negated-nfa-state-p (state)
  (not (null (nfa-state-negated-state state))))


;;; TODO: may change recursion into iteration.
;;; TODO: may change accumulation in list into a hashtable (for perf).
;;; TODO: actually can combine previous todos by using fsm-traversal function instead (see
;;; fsm-traversal package.
;;; NOTE: Since a single instance is created for each state, so address comparison
;;; (using EQ) is sufficient.
;;; NOTE: duplication is automatically handled by prepare-nfa-state-closure.
(defun prepare-nfa-state-closure-union (states)
  "Extracts a union of NFA closures for a set of states (STATES)."
  (labels ((prepare-nfa-state-closure (initial-accumulator state)
             (if (member state initial-accumulator :test #'eq)
                 initial-accumulator ;state has already been traversed
                 (let ((accumul (cons state initial-accumulator))
                       (direct-autos (slot-value state 'auto-transitions)))
                   (etypecase direct-autos
                     (null accumul)
                     (cons (reduce #'prepare-nfa-state-closure
                                   direct-autos
                                   :initial-value accumul)))))))
    (reduce #'prepare-nfa-state-closure states :initial-value nil)))


;;; Prepare ordered list of characters, defining the range splitting points.
;;; Input: List of NFA states, typically representing a union of state closures
;;; (on the transition originating side).
(defun collect-char-range-splitting-points (nfa-states)
  (let ((result nil))
    (dolist (nfa-state nfa-states result)
      (let ((normal-transitions (normal-transitions nfa-state)))
        (dolist (trans normal-transitions)
          (let ((element (slot-value trans 'element)))
            (typecase element
              (character
               (setf result (chars:insert-char-in-order (chars:dec-char element) result))
               (setf result (chars:insert-char-in-order element result)))
              (chars:char-range
               (setf result (chars:insert-char-in-order (chars:dec-char (chars:char-start element))
                                                        result))
               (setf result (chars:insert-char-in-order (chars:char-end element) result))))))))))

(defun simple-element-equal (element other-obj)
  "Equality test for all three types of simple elements (character, char-range, symbol)."
  (or (eq element other-obj)
      (typecase element
        (character (and (typep other-obj 'character)
                        (char= element other-obj)))
        (chars:char-range (and (typep other-obj 'chars:char-range)
                         (chars:char-range-equal element other-obj))))))

;;;TODO: REFACTOR
(defun create-nfa-normalized-transition-table (nfa-state-closure-union)
  "Given an NFA state closure union (NFA-STATE-CLOSURE-UNION), prepare a transition table that maps
each normalized element to corresponding set of destination NFA states (representing a destination
state's closure). by normalized, we mean that range elements are split as needed, to remove any
overlaps. Each element could be single char, char range, any-char, or any-other-char."
  (let ((assoc-list nil)
        (splitting-points (collect-char-range-splitting-points nfa-state-closure-union)))
    (labels ((add-trans (element next-state)
               (let* ((entry (assoc element assoc-list :test #'simple-element-equal))
                      (arr (or (cdr entry)
                               (make-array 10 :adjustable t :fill-pointer 0))))
                 (vector-push-extend next-state arr)
                 (unless entry
                   (push (cons element arr) assoc-list)))))
      (dolist (nfa-state nfa-state-closure-union assoc-list)
        ;; transition on "any char" are covered here
        (dolist (trans (normal-transitions nfa-state))
          (with-slots (element next-state) trans
            (typecase element
              (chars:char-range (let ((split-ranges (chars:split-char-range element
                                                                            splitting-points)))
                                  (dolist (r split-ranges)
                                    (add-trans r next-state))))
              (t (add-trans element next-state)))))))))

(defun create-nfa-normalized-transition-table-iterator (nfa-state-closure-union)
  (let ((table (create-nfa-normalized-transition-table nfa-state-closure-union)))
    (lambda ()
      (pop table))))
