(in-package :parsex-cl.regex-nfa)

(defgeneric regex-to-nfa (regex input-nfa-state)
  (:documentation "Traverses REGEX object tree and generates corresponding NFA section, starting at
INPUT-NFA-STATE. In the process, it creates as many states for the element, glues with input state,
and returns output state as continuation point."))

(defmethod regex-to-nfa ((regex elm:single-char-element) input-nfa-state)
  (let ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex (eql elm:+ANY-CHAR-ELEMENT+)) input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (add-nfa-transition-on-any-char input-nfa-state output-state)
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
             (let* ((input-state-i (make-instance 'nfa-state))
                    (out-state-i (regex-to-nfa elem input-state-i)))
               (add-nfa-auto-transition input-nfa-state input-state-i)
               (add-nfa-auto-transition out-state-i output-state)))
         (elm:inner-elements regex))
    output-state))

(defmethod regex-to-nfa ((regex elm:zero-or-one-element) input-nfa-state)
  (let* ((s1 (make-instance 'nfa-state))
         (inner-regex (elm:inner-element regex))
         (s2 (regex-to-nfa inner-regex s1))
         (output-state (make-instance 'nfa-state)))
    (add-nfa-auto-transition input-nfa-state s1)
    (add-nfa-auto-transition input-nfa-state output-state)
    (add-nfa-auto-transition s2 output-state)
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
         (s2 (regex-to-nfa inner-regex s1))
         (output-state (make-instance 'nfa-state)))
    (add-nfa-auto-transition s2 s1)
    (add-nfa-auto-transition s1 s2)
    (add-nfa-auto-transition s2 output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:negated-element) input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (output-state-inner (regex-to-nfa inner-regex input-nfa-state))
         (glue-state (make-instance 'nfa-state))
         (negation-exit-elem (make-instance 'elm:zero-or-more-element
                                            :element elm:+any-char-element+))
         (output-state (regex-to-nfa negation-exit-elem glue-state)))
    (multiple-value-bind (inner-dead-ends inner-absolutely-dead-ends inner-continuation-points)
        (collect-nfa-non-acceptance-states input-nfa-state output-state-inner)
      ;; traverse NFA sub-tree, and connect each dead-end state to the new (+ any-char) element,
      ;; then to output state
      ;; actually for now, we separated the traversal (above) from the connecting (below)
      (loop for continuation-point in inner-continuation-points
            do (set-dead-end continuation-point)
            do (unset-nfa-transition-on-any-other continuation-point))
      (loop with inner-continuation-point-closures = (prepare-nfa-state-closure-union
                                                      inner-continuation-points)
            for dead-end in inner-dead-ends
            do (when (and (toggle-nfa-transition-on-any-other dead-end glue-state)
                          (not (member dead-end inner-continuation-point-closures :test #'eql)))
                 ;; TODO: give user the choice (greedy/non-greedy)
                 (add-nfa-auto-transition dead-end output-state)
                 (unset-dead-end dead-end)))
      ;; absolute dead-ends are connected directly to output state
      (loop for dead-end in inner-absolutely-dead-ends
            do (unset-dead-end dead-end)
               ;; TODO: rather than creating this transition and creating output-state, check the
               ;; dead-end that has no auto transitions out, and use it as output state
            do (add-nfa-auto-transition dead-end output-state))
      output-state)))

;;; TODO: THIS FUNCTION IS CANDIDATE TO BE TRANSFORMED INTO A GENERIC TRAVERSAL, with flexibility
;;; in whether to traverse normal/auto/both transitions, also can return the list of traversed
;;; states as a useful by-product.
(defun collect-nfa-non-acceptance-states (start-state end-state)
  "Traverse a portion of the NFA, starting at START-STATE, and collect all states that are not
connected to END-STATE, neither directly nor via a series of auto transitions. These states are
non-acceptance states, that will eventually be handled by a parent negation element by converting
them into continuation points. Among these states, those that also have no outgoing transitions of
any type (other than auto transitions), are called absolute dead-end states, and are returned in a
separate list. Finally, all other states, namely those that are connected to continuation points are
also returned in a separate list. The three lists are returned as three values in the following
order: (VALUES DEAD-ENDS ABSOLUTELY-DEAD-ENDS CONTINUATION-POINTS).
TODO: in later version, this function may also do the connecting, but I'm starting with the most
simple version."
  (let ((dead-ends nil)
        (absolutely-dead-ends nil)
        (continuation-points nil)
        (traversed nil))
    (labels ((recurse (state)
               "Recursively handle state and all states reachable from it, and return indication of
its type (T -> continuation point, NIL -> dead-end)."
               (if (member state traversed :test #'eq)
                   (member state continuation-points :test #'eq)
                   (progn
                     (push state traversed)
                     (loop for normal-trans-i in (normal-transitions state)
                           for state-i = (next-state normal-trans-i)
                           do (recurse state-i))
                     (dolist (state-i (transitions-on-any-char state))
                       (recurse state-i))
                     (let ((next-state-on-any-other-char (transition-on-any-other state)))
                       (when next-state-on-any-other-char
                         (recurse next-state-on-any-other-char)))
                     (let ((is-acceptance nil))
                       (when (eq state end-state)
                         (setf is-acceptance t))
                       (dolist (state-i (auto-transitions state))
                         (when (recurse state-i)
                           ;;if state-i is acceptance, then so is the one leading to it
                           (setf is-acceptance t)))
                       (if is-acceptance
                           (pushnew state continuation-points :test #'eq)
                           (if (or (normal-transitions state) (transitions-on-any-char state)
                                   (transition-on-any-other state))
                               (pushnew state dead-ends :test #'eq)
                               (pushnew state absolutely-dead-ends :test #'eq)))
                       is-acceptance)))))
      (recurse start-state)
      (values dead-ends absolutely-dead-ends continuation-points))))

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
   (transitions-on-any-char :initform nil :type list :accessor transitions-on-any-char)
   (transition-on-any-other :initform nil :type (or null nfa-state) :reader transition-on-any-other)
   (is-dead-end :initform nil :type boolean :reader is-dead-end-p)
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
      (with-slots (normal-transitions auto-transitions transitions-on-any-char terminus) object
        (if normal-transitions
            (princ "Normal transitions on: ")
            (princ "No normal transitions, "))
        (loop for nt in normal-transitions
              do (princ (element nt))
              do (princ ", "))
        (format t "~a auto transitions, " (length auto-transitions))
        (format t "~a transitions on any char, " (length transitions-on-any-char))
        (format t "terminus: ~a " terminus)))))

(defun set-dead-end (state)
  "Set dead-end flag for state. This is normally called upon NFA negation, to convert a continuation
point into a dead-end."
  (setf (slot-value state 'is-dead-end) t))

(defun unset-dead-end (state)
  "Unset dead-end flag for state. This is normally called upon NFA negation, to convert a dead-end
into a continuation point."
  (setf (slot-value state 'is-dead-end) nil))

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
               ;; TODO: It's not possible for a transition to lead to a null state, adjust type
               ;; accordingly.
               :type (or null nfa-state)
               :reader next-state)))

(defmethod initialize-instance :after ((transition nfa-transition) &key)
  "Transition constructor for element type validation."
  (with-slots (element) transition
    (let ((required-element-type 'elm:simple-element))
      (unless (typep element required-element-type)
        (error "Invalid transition element type for transition ~a. Expecting ~a, got ~a!"
               transition required-element-type element)))))

(defun add-nfa-normal-transition (orig-state element dest-state)
  "Add NFA transition from ORIG-STATE, on ELEMENT (chararcter/char-range), to DEST-STATE."
  (let ((transition (make-instance 'nfa-transition :element element :next-state dest-state)))
    (push transition (normal-transitions orig-state))))

(defun add-nfa-transition-on-any-char (orig-state dest-state)
  "Add NFA transition on any char from ORIG-STATE to DEST-STATE."
  (push dest-state (transitions-on-any-char orig-state)))

(defun add-nfa-auto-transition (orig-state dest-state)
  "Add NFA auto transition from ORIG-STATE to DEST-STATE."
  (push dest-state (auto-transitions orig-state)))

(defun set-nfa-transition-on-any-other (orig-state dest-state)
  "Set the NFA transition on any char from ORIG-STATE to DEST-STATE, unless it's already set, in
which case, an error is thrown."
  (if (transition-on-any-other orig-state)
      (error "Transition on any other char already set for ~a!" orig-state)
      (setf (slot-value orig-state 'transition-on-any-other) dest-state)))

(defun unset-nfa-transition-on-any-other (orig-state)
  "Unset the NFA transition on any char from ORIG-STATE, if already set, otherwise, do nothing."
  (when (transition-on-any-other orig-state)
    (setf (slot-value orig-state 'transition-on-any-other) nil)))

(defun toggle-nfa-transition-on-any-other (orig-state dest-state)
  "Set the NFA transition on any char from ORIG-STATE to DEST-STATE, unless it's already set, in
which case, it is unset. The use case here is to negate a previous negation (where the two negations
cancel each other). Returns the newly set value (NIL / DEST-STATE)."
  (if (transition-on-any-other orig-state)
      (setf (slot-value orig-state 'transition-on-any-other) nil)
      (setf (slot-value orig-state 'transition-on-any-other) dest-state)))

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
#+nil
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
                                                       (elm:char-end element)))))))))
    (sort (remove-duplicates result :test #'char=) #'char<)))

;;;TODO: REFACTOR
(defun create-nfa-normalized-transition-table (nfa-state-closure-union)
  "Given an NFA state closure union (NFA-STATE-CLOSURE-UNION), prepare a transition table that maps
each normalized element to corresponding set of destination NFA states (representing a destination
state's closure). by normalized, we mean that range elements are split as needed, to remove any
overlaps. Each element could be single char, char range, any-char, or any-other-char. We also add
all destination states of the transition on any-char to the destination states of the normal
transition being handled. This is since all normal elements are implicitly part of the any-char
space."
  (let ((assoc-list nil)
        (splitting-points (collect-char-range-splitting-points nfa-state-closure-union)))
    (labels ((add-trans (element next-state)
               "Add unique transition on ELEMENT to NEXT-STATE."
               (let* ((entry (assoc element assoc-list :test #'elm:simple-element-equal))
                      (arr (or (cdr entry)
                               (make-array 10 :adjustable t :fill-pointer 0))))
                 (vector-push-extend next-state arr)
                 (unless entry
                   (push (cons element arr) assoc-list))))
             (add-trans-on-any-char (orig-closure-union element)
               "Add all transitions on any-char to the transitions on ELEMENT."
               (dolist (orig-state orig-closure-union)
                 (dolist (next-state-on-any-char (transitions-on-any-char orig-state))
                   (add-trans element next-state-on-any-char)))))
      ;; handle normal transitions
      (dolist (nfa-state nfa-state-closure-union assoc-list)
        (dolist (trans (normal-transitions nfa-state))
          (with-slots (element next-state) trans
            (etypecase element
              (elm:char-range-element
               (let ((split-ranges (elm:split-char-range element splitting-points)))
                 (dolist (r split-ranges)
                   (add-trans r next-state)
                   (add-trans-on-any-char nfa-state-closure-union r))))
              (elm:single-char-element
               (add-trans element next-state)
               (add-trans-on-any-char nfa-state-closure-union element)))))
        ;; TODO: probably no reason to keep the following types of transitions in the same assoc
        ;;       table. Consider separating, and consider creating a class for normalized transition
        ;;       table.
        ;; handle transitions on any-char
        (dolist (next-state-on-any-char (transitions-on-any-char nfa-state))
          (add-trans elm:+ANY-CHAR-ELEMENT+ next-state-on-any-char))
        ;; handle transitions on any-other-char
        (let ((next-state-on-any-other-char (transition-on-any-other nfa-state)))
          (when next-state-on-any-other-char
            (add-trans elm::+ANY-OTHER-CHAR-ELEMENT+ next-state-on-any-other-char)))))))

(defmethod fsm:fsm-acceptance-state-p ((fsm-state nfa-state))
  (terminus fsm-state))


;; TODO: a macro would be more convenient
(defmethod fsm:traverse-fsm-transitions ((root-state nfa-state) traversal-fn)
  "Traverse all transitions in the NFA state machine, starting from an initial state ROOT-STATE.
This includes both normal, transitions on any-char, and auto transitions. TRAVERSAL-FN is called for
each transition. Note that initial state does not necessarily have to be the FSM root state."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (nfa-state)
               (unless (gethash nfa-state traversal-mark-lookup-table)
                 (setf (gethash nfa-state traversal-mark-lookup-table) t)
                 (loop for trans in (normal-transitions nfa-state)
                       for elem = (element trans)
                       for next-state = (next-state trans)
                       do (funcall traversal-fn nfa-state elem next-state)
                          (iter next-state))
                 (loop for dest in (transitions-on-any-char nfa-state)
                       do (funcall traversal-fn nfa-state elm:+ANY-CHAR-ELEMENT+ dest)
                          (iter dest))
                 (loop for dest in (auto-transitions nfa-state)
                       do (funcall traversal-fn nfa-state :auto dest)
                          (iter dest))
                 (let ((next-state-on-any-other-char (transition-on-any-other nfa-state)))
                   (when next-state-on-any-other-char
                     (funcall traversal-fn nfa-state :any-other-char next-state-on-any-other-char)
                     (iter next-state-on-any-other-char))))))
      (iter root-state))))
