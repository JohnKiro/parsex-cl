(in-package :parsex-cl/regex/nfa/state)

(defclass nfa-state ()
  ((%normal-transitions :initform nil :type list :reader normal-transitions)
   (%auto-transitions :initform nil :type list :reader auto-transitions)
   (%transition-on-any-other :initform nil :type boolean
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
      (with-slots (%normal-transitions %auto-transitions %terminus) object
        (if %normal-transitions
            (princ "Normal transitions on: " stream)
            (princ "No normal transitions, " stream))
        (loop for nt in %normal-transitions
              do (princ (trans:element nt) stream)
              do (princ ", " stream))
        (format stream "~a auto transitions, " (length %auto-transitions))
        (format stream "terminus: ~a " %terminus)))))

(defun set-dead-end (state)
  "Set dead-end flag for state. This is normally called upon NFA negation, to convert a continuation
point into a dead-end. It also clears all transitions out of it."
  (with-slots (#1=%normal-transitions #2=%auto-transitions #3=%dead-end)
      state
    #+nil(setf #1# nil
          #2# nil
          #3# nil)
    ;; TODO: needed??
    (setf #3# t)))

(defun unset-dead-end (state)
  "Unset dead-end flag for state. This is normally called upon NFA negation, to convert a dead-end
into a continuation point."
  ;; TODO: I think not needed
  (with-slots (#1=%dead-end) state
    (when #1#
      #+nil(error "Shouldn't be set and this shouldn't be needed! BUG??")
      (setf #1# nil))))

(defun add-nfa-normal-transition (orig-state element dest-state)
  "Add NFA transition from ORIG-STATE, on ELEMENT (chararcter/char-range), to DEST-STATE."
  (let ((transition (make-instance 'trans:nfa-transition :element element :next-state dest-state)))
    (push transition (slot-value orig-state '%normal-transitions))))

(defun add-nfa-transition-on-any-char (orig-state dest-state)
  "Add NFA transition on any char from ORIG-STATE to DEST-STATE."
  (add-nfa-normal-transition orig-state elm:*any-char-element* dest-state))

(defun add-nfa-auto-transition (orig-state dest-state)
  "Add NFA auto transition from ORIG-STATE to DEST-STATE."
  (push dest-state (slot-value orig-state '%auto-transitions)))

(defun delete-auto-transition (orig-state dest-state)
  "Delete auto transition from `orig-state` to `dest-state`. Note that we depend on the uniqueness
of state objects, hence, we use the default EQL test."
  (with-slots (%auto-transitions) orig-state
    (setf %auto-transitions (delete dest-state %auto-transitions))))

(defun delete-auto-transitions (orig-state)
  "Delete all auto transitions from `orig-state`."
  (setf (slot-value orig-state '%auto-transitions) nil))

(defun delete-all-outgoing-transitions (orig-state)
  "Delete all outgoing transitions from `orig-state`, including auto, normal, and any-char
transitions (TODO: may also include any-other - EXPERIMENT!)."
  (with-slots (#1=%auto-transitions #2=%normal-transitions) orig-state
    (setf #1# nil
          #2# nil)))

(defun set-nfa-transition-on-any-other (orig-state)
  "Set the NFA transition on any other char from ORIG-STATE, except if it's already set,
in which case, an error is thrown."
  (with-slots (#1=%transition-on-any-other) orig-state
    (when #1#
      (error "Transition on any other char is already set for origin state ~a (BUG??)!" orig-state))
    (setf #1# t)))

(defun reset-nfa-transition-on-any-other (orig-state)
  "Unset the NFA transition on any other char from ORIG-STATE. It has no effect if it's already not set."
  (setf (slot-value orig-state '%transition-on-any-other) nil))

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

(defmacro with-split-element ((element split-element-var splitting-points) &body body)
  "Analyze the `element` and splits it if necessary, according to `splitting-points`. The split element
is made available with the name `split-element-var`, for use in the `body`. If the element is a single
character element, then it is provided as it is, without splitting."
  `(etypecase ,element
     (elm:char-range-element
      (dolist (,split-element-var (elm:split-char-range ,element ,splitting-points))
        ,@body))
     (elm:single-char-element
      (let ((,split-element-var ,element))
        ,@body))))

(defun terminal-nfa-closure-union-p (nfa-states)
  "Determines whether the NFA closure provided in NFA-STATES is terminal, which is the case
when any of the NFA states in the closure is the terminus state produced by the NFA. Since the
terminus state indicates matching success, then terminal also means acceptance.
UPDATE: with the introduction of negation (and dead-end states), a terminus may not indicate acceptance,
because if the closure `nfa-states` includes also a dead-end state, then the terminus should not be
accepting. For this reason, I'll introduce a separate function to check acceptance (to be used in
matching instead of this one, however, as the implementation of negation matures more, this interface
may change."
  ;;TODO: (find-if #'terminus-p nfa-states)
  (dolist (s nfa-states nil)
    (when (terminus-p s)
      (return t))))

(defun acceptance-nfa-closure-union-p (nfa-states)
  "Determines whether the NFA closure provided in NFA-STATES is accepting, which is the case
when any of the NFA states in the closure is the terminus state produced by the NFA, and none of the NFA
states is dead-end. See also `terminal-nfa-closure-union-p`."
  (let (terminus)
    (dolist (s nfa-states terminus)
      (when (dead-end-p s)
        (return nil))
      (when (terminus-p s)
        (setf terminus t)))))

(defun collect-char-range-splitting-points (nfa-states)
  "Collects char range splitting points (characters) from `nfa-states` into a sorted vector of
 characters. NFA states argument should be a list of normal transitions."
  (declare (type list nfa-states))
  (multiple-value-bind (iter-fn get-result-fn) (elm:make-char-range-splitting-points-extractor)
    (do-normal-transitions (_ state _) nfa-states
      (funcall iter-fn state))
    (funcall get-result-fn)))

;;;TODO: REFACTOR (e.g. extract normalized transition table as abstract data type)
(defun create-nfa-normalized-transition-table (nfa-state-closure-union)
  "Given an NFA state closure union (NFA-STATE-CLOSURE-UNION), prepare a transition table that maps
each normalized element to corresponding set of destination NFA states (representing a destination
state's closure). by normalized, we mean that range elements are split as needed, to remove any
overlaps. Each element could be single char or char range."
  (let ((assoc-list nil)
        (splitting-points (collect-char-range-splitting-points nfa-state-closure-union)))
    (labels ((add-trans (element next-state)
               "Add unique transition on ELEMENT to NEXT-STATE."
               (let* ((entry (assoc element assoc-list :test #'elm:simple-element-equal))
                      (arr (or (cdr entry)
                               (make-array 10 :adjustable t :fill-pointer 0))))
                 (vector-push-extend next-state arr)
                 (unless entry
                   (push (cons element arr) assoc-list)))))
      (do-normal-transitions (_ element next-state) nfa-state-closure-union
        (with-split-element (element e splitting-points)
          (add-trans e next-state)))
      assoc-list)))

;;; TODO: THIS FUNCTION IS CANDIDATE TO BE TRANSFORMED INTO A GENERIC TRAVERSAL, with flexibility
;;; in whether to traverse normal/auto/both transitions, also can return the list of traversed
;;; states as a useful by-product.
;;; TODO: Currently traversal starts with element transitions, then auto transitions. Think about the
;;; other order (auto then element).
;;; Some notes about the implementation:
;;; - since NFA is generally cyclic graph, so I have to not only keep track of traversal, but also I try
;;; to collect as much info as possible about each state as soon as possible, since this will help
;;; resolve the type of other states as well (noting the recursive traversal).
;;; - The idea of pending/resolved status is that a state's condition regarding connectivity to end-state
;;; depends on the transitions it is having, so until we collect enough info about the state (via its
;;; transitions), its condition remains unresolved. And resolution may be delayed in case of cycles, but
;;; this is not generally the case, since even with the cycles, enough info could be collected from other
;;; transitions. This is the main idea behind the iterative traversal algorithm I'm using.
;;; - Element transitions contribute to resolution of :element-connected status, so once this status is
;;; determined for a given state, we reach element resolution for that state, regardless of whether
;;; the remaining element transitions in the loop need to be revisited or not. Auto transitions on the
;;; other hand contribute to resolution of both :element-connected, :auto-connected, and
;;; :auto-and-element-connected statuses.
;;; - I tried to avoid SETF unless necessary, that's why the code has many WHEN and UNLESS forms that
;;; check whether a SETF is needed.
;;; - To monitor resolution progress, we keep flags that are updated whenever a state has been updated
;;; recently (with pending/resolved) status, so if an iteration is performed without any progress, we
;;; consider the resolution to be completed. UPDATE: will ignore the resolution progress, and monitor the
;;; "pending" discovery progress only: if an iteration passes without marking any states as "pending",
;;; then we're done with all states. Note that each iteration, we traverse the whole NFA, so we
;;; discover all pending states (not just the newly discovered ones).
(defun analyze-nfa-state-reachability (start-state end-state)
  "Traverse a portion of the NFA, starting at START-STATE, and prepares a hash table with keys as
the traversed states (object reference), and values as condition of each state. Condition is one of:
:AUTO-CONNECTED - having `end-state` in its closure.
:ELEMENT-CONNECTED - can reach `end-state` via a combination of auto and normal transitions (but is not
 having `end-state` in its closure).
:AUTO-AND-ELEMENT-CONNECTED - having paths of both of the previous types to `end-state`.
:NOT-CONNECTED - does not reach `end-state` in any way.
The typical usage is in handling the negation regex."
  (let ((output-table (make-hash-table))
        (traversal-table (make-hash-table))
        (resolution-progress-table (make-hash-table))
        (pending-state-discovered nil)
        ;;cap iteration count, to avoid infinite loop in case of bug (5 is chosen arbitrarily!)
        (iteration-count 5))
    (labels ((recurse (state)
               "Recursively handle state and all states reachable from it, and return indication of
its type (:AUTO-CONNECTED, :ELEMENT-CONNECTED, :AUTO-AND-ELEMENT-CONNECTED or :NOT-CONNECTED)."
               #+nil(format t "DEBUG: Analyzing reachability for state ~a..~&" state)
               (let (auto-connected ;state is auto-connected
                     element-connected ;state is element-connected
                     pending-elem-trans-resolution ;element transition paths not resolved
                     pending-auto-trans-resolution ;auto transition paths not resolved
                     resolved)
                 (case (or #1=(gethash state output-table)
                           (and (eq state end-state) :auto-connected))
                   (:auto-connected (setf auto-connected t))
                   (:element-connected (setf element-connected t))
                   (:auto-and-element-connected (setf auto-connected t
                                                      element-connected t)))
                 (if (gethash state traversal-table)
                     ;; the ORing is just to avoid accessing hash table (cheaper)
                     (when (or (and auto-connected element-connected)
                               (eq (gethash state resolution-progress-table) :resolved))
                       (setf resolved t))
                     (progn
                       (setf (gethash state traversal-table) t)
                       (dolist (normal-trans-i (normal-transitions state))
                         (multiple-value-bind (resolved-i auto-connected-i element-connected-i)
                             (recurse (trans:next-state normal-trans-i))
                           (unless element-connected ;already settled?
                             (if (setf element-connected (or auto-connected-i element-connected-i))
                                 (progn (if auto-connected
                                            (setf #1# :auto-and-element-connected
                                                  (gethash state resolution-progress-table) :resolved)
                                            (setf #1# :element-connected)))
                                 ;; set pending resolution flag when trans not resolved (unless flag
                                 ;; already set) - note that we ignore this if EC is already settled
                                 (unless (and resolved-i pending-elem-trans-resolution)
                                   (setf pending-elem-trans-resolution t))))))
                       (dolist (state-i (auto-transitions state))
                         (multiple-value-bind (resolved-i auto-connected-i element-connected-i)
                             (recurse state-i)
                           (unless (and auto-connected element-connected) ;avoid unnecessary processing
                             (unless (or auto-connected (setf auto-connected auto-connected-i))
                               (unless (and resolved-i pending-auto-trans-resolution)
                                 (setf pending-auto-trans-resolution t)))
                             (unless (or element-connected (setf element-connected element-connected-i))
                               (unless (and resolved-i pending-elem-trans-resolution)
                                 (setf pending-elem-trans-resolution t)))
                             (if auto-connected
                                 (if element-connected
                                     (setf #1# :auto-and-element-connected
                                           (gethash state resolution-progress-table) :resolved)
                                     (setf #1# :auto-connected))
                                 (when element-connected
                                   (setf #1# :element-connected))))))
                       ;; no element or auto connectivity, and nothing pending, so this state must be
                       ;; not connected to end-state (typically it has not transitions out at all)
                       (unless (or auto-connected
                                   element-connected
                                   pending-auto-trans-resolution
                                   pending-elem-trans-resolution)
                         (setf #1# :not-connected))
                       ;; finally, determine resolution
                       (setf resolved (and (or auto-connected (not pending-auto-trans-resolution))
                                           (or element-connected (not pending-elem-trans-resolution))))
                       (if resolved
                           ;; state analysis fully determined
                           (unless (eq (gethash state resolution-progress-table) :resolved)
                             (setf (gethash state resolution-progress-table) :resolved))
                           ;; state analysis not fully determined
                           (setf (gethash state resolution-progress-table) :pending
                                 pending-state-discovered t))
                       (remhash state traversal-table)))
                 (values resolved auto-connected element-connected))))
      (loop
        (recurse start-state)
        #+debug (assert (zerop (hash-table-count traversal-table)))
        (decf iteration-count)
        (unless (and pending-state-discovered (> iteration-count 0))
          (return output-table))
        (setf pending-state-discovered nil)))))

(defun states-have-trans-on-any-other-p (nfa-states)
  "Finds if any of the argument `nfa-states` has a transition on any-other-char. The `nfa-states`
is a list of NFA states, that could typically be a closure. We avoid to compute the closure inside
the function, since it is typically already prepared by the client code."
  (find-if #'transition-on-any-other nfa-states))

(defmethod fsm:fsm-acceptance-state-p ((fsm-state nfa-state))
  (terminus-p fsm-state))

(defmethod fsm:fsm-dead-end-state-p ((fsm-state nfa-state))
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
                 (loop for dest in (auto-transitions nfa-state)
                       do (funcall traversal-fn nfa-state :auto dest)
                          (iter dest)))))
      (iter root-state))))
