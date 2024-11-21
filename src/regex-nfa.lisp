(in-package :parsex-cl.regex-nfa)

(defgeneric regex-to-nfa (regex input-nfa-state)
  (:documentation "Traverses REGEX object tree and generates corresponding NFA section, starting at
INPUT-NFA-STATE. In the process, it creates as many states for the element, glues with input state,
and returns output state as continuation point."))

(defmethod regex-to-nfa ((regex elm:single-char-element) input-nfa-state)
  (let ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

;; NOTE: the only symbol currently supported is :any-char, but I chose to have more generic code
;; than specializing on (eql :any-char).
(defmethod regex-to-nfa ((regex symbol) input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:char-range-element) input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:sequence-element) input-nfa-state)
  (reduce #'(lambda (previous-output-nfa-state elem)
              (regex-to-nfa elem previous-output-nfa-state))
          (elm:inner-elements regex)
          :initial-value input-nfa-state))

(defmethod regex-to-nfa ((regex elm:or-element) input-nfa-state)
  (let ((output-state (make-instance 'nfa-state)))
    (map nil
         #'(lambda (elem)
             (let ((out-state-i (regex-to-nfa elem input-nfa-state)))
               (add-nfa-auto-transition out-state-i output-state)))
         (elm:inner-elements regex))
    output-state))

(defmethod regex-to-nfa ((regex elm:zero-or-one-element) input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (output-state (regex-to-nfa inner-regex input-nfa-state)))
    (add-nfa-auto-transition input-nfa-state output-state)
    output-state))

;;TODO: SIMPLIFY, AS DONE FOR :+ BELOW
(defmethod regex-to-nfa ((regex elm:zero-or-more-element) input-nfa-state)
  (let* ((s1 (make-instance 'nfa-state))
         (inner-regex (elm:inner-element regex))
         (s2 (regex-to-nfa inner-regex s1))
         (output-state (make-instance 'nfa-state)))
    (add-nfa-auto-transition input-nfa-state s1)
    (add-nfa-auto-transition input-nfa-state output-state)
    (add-nfa-auto-transition s2 s1)
    (add-nfa-auto-transition s2 output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:one-or-more-element) input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (s1 (regex-to-nfa inner-regex input-nfa-state))
         (s2 (regex-to-nfa inner-regex s1)))
    (add-nfa-auto-transition s2 s1)
    (add-nfa-auto-transition s1 s2)
    s2))

(defmethod regex-to-nfa ((regex elm:negated-element) input-nfa-state)
  (error "TODO: IMPLEMENT THE NOT-ELEMENT!"))

(defun parse-and-produce-nfa (regex)
  "Produces NFA starting at root regex element. Its importance is in identifying the terminus state.
TODO: after latest changes, it does NOT actually parse, so consider renaming."
  (let* ((root-state (make-instance 'nfa-state))
         (terminus-nfa-state (regex-to-nfa regex root-state)))
    (setf (terminus terminus-nfa-state) t)
    (values root-state terminus-nfa-state)))

(defclass nfa-state ()
  ((normal-transitions :initform nil :type list :accessor normal-transitions)
   (auto-transitions :initform nil :type list :accessor auto-transitions)
   ;;NOTE: terminus state will not have any normal transitions, so may enhance by
   ;;prohibiting inconsistency (introduce class hierarchy level).
   ;;However, terminus state is not known when the state is constructed, so cannot determine its
   ;;type beforehand. It's still possible to change CLOS class, but probably not worth the
   ;;complexity.
   (terminus :initform nil :type (or null t) :accessor terminus)))

(defparameter *verbose-printing* nil "Enable/disable verbose object printing.")

(defmethod print-object ((object nfa-state) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *verbose-printing*
      (with-slots (normal-transitions auto-transitions terminus) object
        (if normal-transitions
            (princ "Normal transitions on: ")
            (princ "No normal transitions, "))
        (loop for nt in normal-transitions
              do (princ (element nt))
              do (princ ", "))
        (format t "~a auto transitions, " (length auto-transitions))
        (format t "terminus: ~a " terminus)))))
;;;TODO: CHECK WITH NOT-ELEMENT above (remove one of them?)
(defclass negated-nfa-state (nfa-state)
  ((negated-state :initform (error "Negated state is mandatory!") :type nfa-state)))


;;; defines a normal NFA transition upon matching of ELEMENT, to NEXT-STATE.
;;; TODO: for now, element type restricted to one of three types with character hardcoded.

(defclass nfa-transition ()
  ((element :initarg :element
            :initform (error "element must be specified!")
            :type (elm:simple-element)
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

#+nil
(defun add-nfa-special-transition (orig-state element dest-state)
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
                   (reduce #'prepare-nfa-state-closure direct-autos :initial-value accumul)))))
    (reduce #'prepare-nfa-state-closure states :initial-value nil)))

(defun terminal-nfa-closure-union-p (nfa-states)
  "Determines whether the NFA closure provided in NFA-STATES is terminal, which is the case
when any of the NFA states in the closure is the terminus state produced by the NFA. Since the
terminus state indicates matching success, then terminal also means acceptance."
  (dolist (s nfa-states nil)
    (when (terminus s)
      (return t))))

(defun collect-char-range-splitting-points (nfa-states)
  (let ((result (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (labels ((append-bounds (char-left char-right)
               (vector-push-extend (chars:dec-char char-left) result)
               (vector-push-extend char-right result)))
      (dolist (nfa-state nfa-states result)
        (let ((normal-transitions (normal-transitions nfa-state)))
          (dolist (trans normal-transitions)
            (let ((element (slot-value trans 'element)))
              (etypecase element
                (elm:single-char-element (let ((bound (elm:single-char element)))
                                           (append-bounds bound bound)))
                (elm:char-range-element (append-bounds (elm:char-start element)
                                                       (elm:char-end element)))
                ;;do nothing
                (symbol nil)))))))
    (sort (remove-duplicates result :test #'char=) #'char<)))

;;;TODO: REFACTOR
(defun create-nfa-normalized-transition-table (nfa-state-closure-union)
  "Given an NFA state closure union (NFA-STATE-CLOSURE-UNION), prepare a transition table that maps
each normalized element to corresponding set of destination NFA states (representing a destination
state's closure). by normalized, we mean that range elements are split as needed, to remove any
overlaps. Each element could be single char, char range, any-char, or any-other-char (currently
not used)."
  (let ((assoc-list nil)
        (splitting-points (collect-char-range-splitting-points nfa-state-closure-union)))
    (labels ((add-trans (element next-state)
               (let* ((entry (assoc element assoc-list :test #'elm:simple-element-equal))
                      (arr (or (cdr entry)
                               (make-array 10 :adjustable t :fill-pointer 0))))
                 (vector-push-extend next-state arr)
                 (unless entry
                   (push (cons element arr) assoc-list)))))
      (dolist (nfa-state nfa-state-closure-union assoc-list)
        ;; transition on "any char" are covered here
        (dolist (trans (normal-transitions nfa-state))
          (with-slots (element next-state) trans
            (etypecase element
              (elm:char-range-element (let ((split-ranges (elm:split-char-range element
                                                                                splitting-points)))
                                        (dolist (r split-ranges)
                                          (add-trans r next-state))))
              (t (add-trans element next-state)))))))))

(defmethod fsm:fsm-acceptance-state-p ((fsm-state nfa-state))
  (terminus fsm-state))


;; TODO: a macro would be more convenient
(defmethod fsm:traverse-fsm-transitions ((root-state nfa-state) traversal-fn)
  "Traverse all transitions in the NFA state machine, starting from an initial state ROOT-STATE.
This includes both normal and auto transitions. TRAVERSAL-FN is called for each transition. Note
that initial state does not necessarily have to be the FSM root state."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (nfa-state)
               (unless (gethash nfa-state traversal-mark-lookup-table)
                 (setf (gethash nfa-state traversal-mark-lookup-table) t)
                 (loop for trans in (normal-transitions nfa-state)
                       for elem = (element trans)
                       for next-state = (next-state trans)
                       do (funcall traversal-fn nfa-state elem next-state)
                          (iter next-state))
                 (loop for dest in (auto-transitions nfa-state)
                       do (funcall traversal-fn nfa-state :auto dest)
                          (iter dest)))))
      (iter root-state))))
