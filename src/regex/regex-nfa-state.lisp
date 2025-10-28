(in-package :parsex-cl/regex/nfa/state)

(defclass nfa-state ()
  ((%normal-transitions :initform nil :type list :reader normal-transitions)
   (%auto-transitions :initform nil :type list :reader auto-transitions)
   (%transitions-on-any-char :initform nil :type list :reader transitions-on-any-char)
   (%transition-on-any-other :initform nil :type (or null nfa-state)
                             :reader transition-on-any-other)
   (%dead-end :initform nil :type boolean :reader dead-end-p)
   (%negated :initform nil :initarg :negated :type boolean :reader negated-p)
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
            (princ "Normal transitions on: " stream)
            (princ "No normal transitions, " stream))
        (loop for nt in %normal-transitions
              do (princ (trans:element nt) stream)
              do (princ ", " stream))
        (format stream "~a auto transitions, " (length %auto-transitions))
        (format stream "~a transitions on any char, " (length %transitions-on-any-char))
        (format stream "terminus: ~a " %terminus)))))

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
  "Add NFA transition on any char from ORIG-STATE to DEST-STATE."
  (push dest-state (slot-value orig-state '%transitions-on-any-char)))

(defun add-nfa-auto-transition (orig-state dest-state)
  "Add NFA auto transition from ORIG-STATE to DEST-STATE."
  (push dest-state (slot-value orig-state '%auto-transitions)))

(defun delete-auto-transition (orig-state dest-state)
  "Delete auto transition from `orig-state` to `dest-state`. Note that we depend on the uniqueness
of state objects, hence, we use the default EQL test."
  (with-slots (%auto-transitions) orig-state
    (setf %auto-transitions (delete dest-state %auto-transitions))))

(defun set-nfa-transition-on-any-other (orig-state dest-state)
  "Set the NFA transition on any other char from ORIG-STATE to DEST-STATE, except in one of two
cases:
1) If a transition on any-char is found for ORIG-STATE, then this transition is skipped, since it
won't be effective anyway (since any-char matches any char, so the path on any-other-char will never
be used. This is just for purpose of optimization.
2) If a transition on any other char from ORIG-STATE already exists. In this case, I'm throwing an
error.
The returned value is either the newly-set value, or NIL otherwise.
TODO: I WON'T NEED DEST-STATE ACTUALLY (T/NIL will do the job)."
  (with-slots (#1=%transitions-on-any-char #2=%transition-on-any-other) orig-state
    (cond
      (#1# nil)
      (#2# (error "Transition on any other char is already set for origin state ~a (BUG??)!"
                  orig-state))
      (t (setf #2# dest-state)))))

(defun unset-nfa-transition-on-any-other (orig-state)
  "Unset the NFA transition on any char from ORIG-STATE. It has no effect if it's already not set."
  (setf (slot-value orig-state '%transition-on-any-other) nil))

(defun toggle-negation (nfa-state)
  "Toggle the NFA state negation flag. A negated NFA state indicates no matching. For example, for
a (not #\b) regex, the state coming from 'b' is negated. This function toggles the negation flag
(t -> nil, nil -> t).
UPDATE: probably won't need it!"
  (setf #1=(slot-value nfa-state '%negated) (not #1#)))

(defun set-terminus (nfa-state)
  "Mark state `nfa-state` as terminus."
  (setf (slot-value nfa-state '%terminus) t))

;;; TODO: may change recursion into iteration.
;;; TODO: may change accumulation in list into a hashtable (for perf).
;;; TODO: check possibility to combine previous todos by using fsm-traversal function instead (see
;;; fsm-traversal package.
;;; NOTE: Since a single instance is created for each state, so address comparison (using EQ) is
;;; sufficient.
;;; NOTE: duplication is automatically handled by prepare-nfa-state-closure.
(defun %accumulate-nfa-state-closure (initial-accumulator state)
  "Accumulates NFA closure for `state` into `accumulator`, and returns the final accumulator value.
If `state` is found in the accumulator, it is skipped as the accumulator plays also the role of
traversal lookup. This is a helper function."
  (if (member state initial-accumulator :test #'eq)
      initial-accumulator
      (let ((accumulator (cons state initial-accumulator))
            (direct-autos (slot-value state '%auto-transitions)))
        (reduce #'%accumulate-nfa-state-closure direct-autos :initial-value accumulator))))

(defun prepare-nfa-state-closure (state)
  "Computes NFA closure for state `state`."
  (%accumulate-nfa-state-closure nil state))

(defun prepare-nfa-state-closure-union (states)
  "Computes a union of NFA closures for a set of states `states`."
  (reduce #'%accumulate-nfa-state-closure states :initial-value nil))

;; TODO: considering creating a more general and flexible traversal macro, replacing even the
;; FSM traversal generic method
(defmacro do-normal-transitions ((transition-var
                                  element-reader-name-var
                                  next-state-reader-name-var) list-of-states &body body)
  "Iterate on normal transitions of a list of states (typically a state closure or a state closure
union, exposing in each iteration the transition object, as well as accessors for the transition's
element and next state.
TODO: may consider passing the name vars as keywords, since I may not always be interested in all
exposed variables, and wish to access only some of them. However, there is no cost in declaring
variables that won't be used in the body (since `with-accessors` just defines symbol macros).
TODO: may (also) define another macro that takes a single state, and compute its closure first,
before iterating on closure's normal transitions. Such macro might be called
`do-closure-normal-transitions`.
TODO: THINK ALSO ABOUT ANY-CHAR TRANSITIONS!"
  (alexandria:with-gensyms (nfa-state)
    `(dolist (,nfa-state ,list-of-states)
       (dolist (,transition-var (normal-transitions ,nfa-state))
         (with-accessors ((,element-reader-name-var trans:element)
                          (,next-state-reader-name-var trans:next-state)) ,transition-var
           ,@body)))))

(defun terminal-nfa-closure-union-p (nfa-states)
  "Determines whether the NFA closure provided in NFA-STATES is terminal, which is the case
when any of the NFA states in the closure is the terminus state produced by the NFA. Since the
terminus state indicates matching success, then terminal also means acceptance.
UPDATE (EXPERIMENTAL): if it's negated, then not necessarily it's acceptance (depending on the
scanned char)."
  ;;TODO: (find-if #'terminus-p nfa-states)
  (dolist (s nfa-states nil)
    (when (terminus-p s)
      (return t))))

(defun collect-char-range-splitting-points (nfa-states)
  "Collects char range splitting points (characters) into a sorted vector of characters."
  (let ((result (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (labels ((append-bounds (char-left char-right)
               (when (typep char-left 'character)
                 (vector-push-extend (chars:dec-char char-left) result))
               (when (typep char-right 'character)
                 (vector-push-extend char-right result))))
      (dolist (nfa-state nfa-states result)
        (dolist (trans (normal-transitions nfa-state))
          (let ((element (trans:element trans)))
            (etypecase element
              (elm:single-char-element (let ((bound (elm:single-char element)))
                                         (append-bounds bound bound)))
              (elm:char-range-element (append-bounds (elm:char-start element)
                                                     (elm:char-end element))))))))
    (sort (remove-duplicates result :test #'char=) #'char<)))

;;;TODO: REFACTOR (e.g. extract normalized transition table as abstract data type)
(defun create-nfa-normalized-transition-table (nfa-state-closure-union)
  "Given an NFA state closure union (NFA-STATE-CLOSURE-UNION), prepare a transition table that maps
each normalized element to corresponding set of destination NFA states (representing a destination
state's closure). by normalized, we mean that range elements are split as needed, to remove any
overlaps. Each element could be single char, char range, any-char, or any-other-char. We also add
all destination states of the transition on any-char to the destination states of the normal
transition being handled. This is since all normal elements are implicitly part of the any-char
space. Note that both any-char and any-other-char are merged, and inserted as any-other-char. This
is since as far as DFA is concerned, both will be handled as any-other-char.
UPDATE: this last part is not accurate, since any-other-char merging should be treated in a special
way (e.g. merging any-other than 'a' with any-other than 'b' ==> cancel each other)."
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
      (dolist (nfa-state nfa-state-closure-union assoc-list)
        ;; handle normal transitions
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
        ;; TODO: REMOVE, since in latest changes (Sept 2025), there won't be such transition
        (let ((next-state-on-any-other-char (transition-on-any-other nfa-state)))
          (when next-state-on-any-other-char
            (add-trans elm::+ANY-OTHER-CHAR-ELEMENT+ next-state-on-any-other-char)))))))

;;; TODO: THIS FUNCTION IS CANDIDATE TO BE TRANSFORMED INTO A GENERIC TRAVERSAL, with flexibility
;;; in whether to traverse normal/auto/both transitions, also can return the list of traversed
;;; states as a useful by-product.
(defun analyze-nfa-state-reachability (start-state end-state)
  "Traverse a portion of the NFA, starting at START-STATE, and prepares a hash table with keys as
the traversed states (object reference), and values as condition of each state. Condition is one of:
:AUTO-CONNECTED - having `end-state` in its closure.
:ELEMENT-CONNECTED - can reach `end-state` via a combination of auto and normal/any-char
transitions (but is not having `end-state` in its closure).
:AUTO-AND-ELEMENT-CONNECTED - having paths of both of the previous types to `end-state`.
:NOT-CONNECTED - does not reach `end-state` in any way.
The typical usage is in handling the negation regex."
  (let ((output-table (make-hash-table))
        (traversal-table (make-hash-table))
        (pending-states-table (make-hash-table))
        (confirmed-states-table (make-hash-table))
        (iteration-count 5))
    (labels ((recurse (state)
               "Recursively handle state and all states reachable from it, and return indication of
its type (:AUTO-CONNECTED, :ELEMENT-CONNECTED, :AUTO-AND-ELEMENT-CONNECTED or :NOT-CONNECTED)."
               #+nil(format t "DEBUG: Analyzing reachability for state ~a..~&" state)
               (let (auto-connected
                     element-connected
                     to-be-revisited
                     current-state-status)
                 (setf current-state-status (or #1=(gethash state output-table)
                                                (and (eq state end-state) :auto-connected)
                                                ;; inital status
                                                :not-connected))
                 (case current-state-status
                   (:auto-connected (setf auto-connected t))
                   (:element-connected (setf element-connected t))
                   (:auto-and-element-connected (setf auto-connected t
                                                      element-connected t)))
                 (format t "Analyzing reachability for state ~a ..~%" state)
                 (format t "---------------------------------------------------~%")
                 ;;(break)
                 ;; TODO: merge with following UNLESS into if/else.
                 (when (gethash state traversal-table)
                   #+nil(unless (eq current-state-status :auto-and-element-connected)
                          (setf (gethash state pending-states-table) t)
                          (setf to-be-revisited t))
                   (unless (gethash state confirmed-states-table)
                     (setf to-be-revisited t)))
                 (unless (gethash state traversal-table)
                   (setf (gethash state traversal-table) t)
                   (dolist (normal-trans-i (normal-transitions state))
                     (let ((state-i (trans:next-state normal-trans-i)))
                       (format t "Handling transition on ~a to ~a ..~%"
                               (trans:element normal-trans-i)
                               state-i)
                       (multiple-value-bind (to-be-revisited-i auto-connected-i element-connected-i)
                           (recurse state-i)
                         (unless element-connected ;avoid redundant hash table updates
                           (if (setf element-connected (or auto-connected-i element-connected-i))
                               (progn (if auto-connected
                                          (setf #1# :auto-and-element-connected)
                                          (setf #1# :element-connected))
                                      (setf to-be-revisited nil))
                               (when to-be-revisited-i
                                 ;; propagate revised flag only if no element conn found so far
                                 (setf to-be-revisited t)))))))
                   ;;TODO: code very similar to above (hope to remove this duplication)
                   ;; probably I'll end up including the transitions on any char together with
                   ;; the normal transitions (would simplify a lot of code, though not
                   ;; necessarily better for efficiency.
                   (dolist (state-i (transitions-on-any-char state))
                     (multiple-value-bind (to-be-revisited-i auto-connected-i element-connected-i)
                         (recurse state-i)
                       (unless element-connected ;avoid redundant hash table updates
                         (if (setf element-connected (or auto-connected-i element-connected-i))
                             (progn (if auto-connected
                                        (setf #1# :auto-and-element-connected)
                                        (setf #1# :element-connected))
                                    (setf to-be-revisited nil))
                             (when to-be-revisited-i
                               ;; propagate revised flag only if no element conn found so far
                               (setf to-be-revisited t))))))
                   (dolist (state-i (auto-transitions state))
                     (format t "Handling auto transition to ~a ..~%" state-i)
                     (multiple-value-bind (to-be-revisited-i auto-connected-i element-connected-i)
                         (recurse state-i)
                       (unless (and auto-connected element-connected) ;avoid unnecessary processing
                         (when auto-connected-i
                           (setf auto-connected t))
                         (when element-connected-i
                           (setf element-connected t))
                         (if auto-connected
                             (if element-connected
                                 (setf #1# :auto-and-element-connected)
                                 (setf #1# :auto-connected))
                             (if element-connected
                                 (setf #1# :element-connected)
                                 (setf #1# :not-connected)))
                         (if (and auto-connected element-connected) ;skip revisiting if settled
                             (setf to-be-revisited nil)
                             (when to-be-revisited-i
                               ;; transition needs to be revisited, so does current state as well.
                               (setf to-be-revisited t))))))
                   ;; conclusion based on all traversed element transitions
                   ;; this IF form is needed, since not all states have transitions out, so the
                   ;; above incremental updates won't apply to them
                   (if auto-connected
                       (if element-connected
                           (setf #1# :auto-and-element-connected)
                           (setf #1# :auto-connected))
                       (if element-connected
                           (setf #1# :element-connected)
                           (setf #1# :not-connected)))
                   (if (and #+nil(not (and auto-connected element-connected))
                            to-be-revisited)
                       ;; state analysis not fully determined
                       (setf (gethash state pending-states-table) :pending)
                       ;; state analysis fully determined
                       (progn
                         (remhash state pending-states-table)
                         (setf (gethash state confirmed-states-table) t))))
                 (format t "State ~a traversal status: to-be-revisited = ~a, auto-connected = ~a,
  element-connected = ~a.~%" state to-be-revisited auto-connected element-connected)
                 (values to-be-revisited auto-connected element-connected))))
      (loop
        (format t "NFA state reachability analysis iteration # ~a, pending states: ~a ..~%"
                iteration-count (hash-table-count pending-states-table))
        (format t "==============================================================~%")
        (setf traversal-table (make-hash-table))
        (recurse start-state)
        (decf iteration-count)
        (when (or (zerop (hash-table-count pending-states-table))
                  (<= iteration-count 0)) ;just during testing, to avoid infinite loop in case of bug
          (return output-table))))))

(defun states-have-trans-on-any-other-p (nfa-states)
  "Finds if any of the argument `nfa-states` has a transition on any-other-char. The `nfa-states`
is a list of NFA states, that could typically be a closure. We avoid to compute the closure inside
the function, since it is typically already prepared by the client code."
  (find-if #'transition-on-any-other nfa-states))

(defmethod fsm:fsm-acceptance-state-p ((fsm-state nfa-state))
  (terminus-p fsm-state))

(defmethod fsm::fsm-dead-end-state-p ((fsm-state nfa-state))
  (dead-end-p fsm-state))

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


