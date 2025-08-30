(in-package :parsex-cl/regex/nfa/state)

(defclass nfa-state ()
  ((%normal-transitions :initform nil :type list :reader normal-transitions)
   (%auto-transitions :initform nil :type list :reader auto-transitions)
   (%transitions-on-any-char :initform nil :type list :reader transitions-on-any-char)
   (%transition-on-any-other :initform nil :type (or null nfa-state)
                             :reader transition-on-any-other)
   (%dead-end :initform nil :type boolean :reader dead-end-p)
   ;;NOTE: terminus state will not have any normal transitions, so may enhance by
   ;;prohibiting inconsistency (introduce class hierarchy level).
   ;;However, terminus state is not known when the state is constructed, so cannot determine its
   ;;type beforehand. It's still possible to change CLOS class, but probably not worth the
   ;;complexity.
   (%terminus :initform nil :type (or null t) :reader terminus-p)))

(defparameter *verbose-printing* nil "Enable/disable verbose object printing.")

(defmethod print-object ((object nfa-state) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when *verbose-printing*
      (with-slots (%normal-transitions %auto-transitions %transitions-on-any-char %terminus) object
        (if %normal-transitions
            (princ "Normal transitions on: ")
            (princ "No normal transitions, "))
        (loop for nt in %normal-transitions
              do (princ (trans:element nt))
              do (princ ", "))
        (format t "~a auto transitions, " (length %auto-transitions))
        (format t "~a transitions on any char, " (length %transitions-on-any-char))
        (format t "terminus: ~a " %terminus)))))

(defun set-dead-end (state)
  "Set dead-end flag for state. This is normally called upon NFA negation, to convert a continuation
point into a dead-end."
  (setf (slot-value state '%dead-end) t))

(defun unset-dead-end (state)
  "Unset dead-end flag for state. This is normally called upon NFA negation, to convert a dead-end
into a continuation point."
  (setf (slot-value state '%dead-end) nil))

;;;TODO: CHECK WITH NOT-ELEMENT above (remove one of them?)
(defclass negated-nfa-state (nfa-state)
  ((negated-state :initform (error "Negated state is mandatory!") :type nfa-state)))

(defun add-nfa-normal-transition (orig-state element dest-state)
  "Add NFA transition from ORIG-STATE, on ELEMENT (chararcter/char-range), to DEST-STATE."
  (let ((transition (make-instance 'trans:nfa-transition :element element :next-state dest-state)))
    (push transition (slot-value orig-state '%normal-transitions))))

(defun add-nfa-transition-on-any-char (orig-state dest-state)
  "Add NFA transition on any char from ORIG-STATE to DEST-STATE. Note that any-char invalidates
any-other-char (since it accepts any-char, leaving nothing to any-other-char to match. For this
reason, this function also clears any-other-char transition (if any)."
  (push dest-state (slot-value orig-state '%transitions-on-any-char))
  (unset-nfa-transition-on-any-other orig-state))

(defun add-nfa-auto-transition (orig-state dest-state)
  "Add NFA auto transition from ORIG-STATE to DEST-STATE."
  (push dest-state (slot-value orig-state '%auto-transitions)))

(defun delete-auto-transition (state state-to-be-deleted)
  "Delete NFA state `state-to-be-deleted` from NFA state `state`'s auto transitions. Note that we
depend on the uniqueness of state objects, hence, we use the default EQL test. Also note that
although we use a vector (not a list), we set the returned value back, to keep the code agnostic to
sequence type."
  (with-slots (%auto-transitions) state
    (setf %auto-transitions (delete state-to-be-deleted %auto-transitions))))

(defun set-nfa-transition-on-any-other (orig-state dest-state)
  "Set the NFA transition on any other char from ORIG-STATE to DEST-STATE, except in one of two
cases:
1) If a transition on any-char is found for ORIG-STATE, then this transition is skipped, since it
won't be effective anyway (since any-char matches any char, so the path on any-other-char will never
be used.
2) If a transition on any other char from ORIG-STATE already exists. This is because we already have
all required paths (every char is covered). NOTE that currently I'm assuming that this transition
would be eventually cleaned-up, which is not a good assumption, since it assumes a specific
scenario. TODO: change into a simpler setter.
The returned value is either the newly-set value, or NIL otherwise."
  (with-slots (#1=%transitions-on-any-char #2=%transition-on-any-other) orig-state
    (if (or #1# #2#)
        nil
        (setf #2# dest-state))))

(defun unset-nfa-transition-on-any-other (orig-state)
  "Unset the NFA transition on any char from ORIG-STATE. It has no effect if it's already not set."
  (setf (slot-value orig-state '%transition-on-any-other) nil))

(defun set-terminus (nfa-state)
  "Mark state `nfa-state` as terminus."
  (setf (slot-value nfa-state '%terminus) t))

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
                       (direct-autos (slot-value state '%auto-transitions)))
                   (reduce #'prepare-nfa-state-closure direct-autos :initial-value accumul)))))
    (reduce #'prepare-nfa-state-closure states :initial-value nil)))

(defun terminal-nfa-closure-union-p (nfa-states)
  "Determines whether the NFA closure provided in NFA-STATES is terminal, which is the case
when any of the NFA states in the closure is the terminus state produced by the NFA. Since the
terminus state indicates matching success, then terminal also means acceptance."
  (dolist (s nfa-states nil)
    (when (terminus-p s)
      (return t))))

(defun collect-char-range-splitting-points (nfa-states)
  (let ((result (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (labels ((append-bounds (char-left char-right)
               (vector-push-extend (chars:dec-char char-left) result)
               (vector-push-extend char-right result)))
      (dolist (nfa-state nfa-states result)
        (let ((normal-transitions (normal-transitions nfa-state)))
          (dolist (trans normal-transitions)
            (let ((element (trans:element trans)))
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
space. Note that both any-char and any-other-char are merged, and inserted as any-other-char. This
is since as far as DFA is concerned, both will be handled as any-other-char."
  (let ((assoc-list nil)
        (splitting-points (collect-char-range-splitting-points nfa-state-closure-union)))
    (labels ((add-trans (element next-state)
               "Add unique transition on ELEMENT to NEXT-STATE."
               (when (eq element elm:+ANY-CHAR-ELEMENT+)
                 (setf element elm::+ANY-OTHER-CHAR-ELEMENT+))
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
          (with-accessors ((element trans:element) (next-state trans:next-state)) trans
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

;;; TODO: THIS FUNCTION IS CANDIDATE TO BE TRANSFORMED INTO A GENERIC TRAVERSAL, with flexibility
;;; in whether to traverse normal/auto/both transitions, also can return the list of traversed
;;; states as a useful by-product.
(defun analyze-nfa-state-reachability (start-state end-state)
  "Traverse a portion of the NFA, starting at START-STATE, and prepares two separate lists of
states: 1) All states that have END-STATE in their closures, and 2) All states that do not have
END-STATE in their closures. The typical usage is in handling the negation regex, where the first
list corresponds to matching points, while the second corresponds to non-matching points.
The two lists are returned as two values: (VALUES AUTO-CONNECTED NON-AUTO-CONNECTED)."
  (let ((non-auto-connected nil)
        (auto-connected nil)
        (traversed nil))
    (labels ((recurse (state)
               "Recursively handle state and all states reachable from it, and return indication of
its type (T -> auto-connected, NIL -> non-auto-connected)."
               (if (member state traversed :test #'eq)
                   (member state auto-connected :test #'eq)
                   (progn
                     (push state traversed)
                     (loop for normal-trans-i in (normal-transitions state)
                           for state-i = (trans:next-state normal-trans-i)
                           do (recurse state-i))
                     (dolist (state-i (transitions-on-any-char state))
                       (recurse state-i))
                     (let ((next-state-on-any-other-char (transition-on-any-other state)))
                       (when next-state-on-any-other-char
                         (recurse next-state-on-any-other-char)))
                     (let ((is-auto-connected nil))
                       (when (eq state end-state)
                         (setf is-auto-connected t))
                       (dolist (state-i (auto-transitions state))
                         (when (recurse state-i)
                           ;;if state-i is auto-connected, then so is the one leading to it
                           (setf is-auto-connected t)))
                       (if is-auto-connected
                           (pushnew state auto-connected :test #'eq)
                           (pushnew state non-auto-connected :test #'eq))
                       is-auto-connected)))))
      (recurse start-state)
      (values auto-connected non-auto-connected))))

(defmethod fsm:fsm-acceptance-state-p ((fsm-state nfa-state))
  (terminus-p fsm-state))

;; TODO: a macro would be more convenient
(defmethod fsm:traverse-fsm-transitions ((root-state nfa-state) traversal-fn)
  "Traverse all transitions in the NFA state machine, starting from an initial state ROOT-STATE.
This includes both normal, transitions on any-char, any-other-char, and auto transitions.
TRAVERSAL-FN is called for each transition. Note that initial state does not necessarily have to be
the FSM root state."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (nfa-state)
               (unless (gethash nfa-state traversal-mark-lookup-table)
                 (setf (gethash nfa-state traversal-mark-lookup-table) t)
                 (loop for trans in (normal-transitions nfa-state)
                       for elem = (trans:element trans)
                       for next-state = (trans:next-state trans)
                       do (funcall traversal-fn nfa-state elem next-state)
                          (iter next-state))
                 (loop for dest in (transitions-on-any-char nfa-state)
                       do (funcall traversal-fn nfa-state elm:+ANY-CHAR-ELEMENT+ dest)
                          (iter dest))
                 (loop for dest in (auto-transitions nfa-state)
                       do (funcall traversal-fn nfa-state :auto dest)
                          (iter dest))
                 ;; TODO: might just call the traversal function, even if transition on any other
                 ;; is NIL
                 (alexandria:when-let (next-state (transition-on-any-other nfa-state))
                   (funcall traversal-fn nfa-state elm::+ANY-OTHER-CHAR-ELEMENT+ next-state)
                   (iter next-state)))))
      (iter root-state))))
