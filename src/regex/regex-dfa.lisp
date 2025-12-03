(in-package :parsex-cl/regex/dfa)

;;;; ---------------------------------------------------
;;;; Code for DFA construction, based on regex (and NFA)
;;;; ---------------------------------------------------

;;; A DFA state contains the union of NFA states that form the DFA state, as well as an
;;; association of transitions (simple element --> next DFA state).
(defclass dfa-state ()
  ((%transitions :accessor transitions
                 :initform nil
                 :type list
                 :documentation "Transitions to other DFA states, based on single char/char range")
   (%candidate-matching-point :reader candidate-matching-point-p
                              :type boolean
                              :documentation "whether this is accepting state. It is derived from
NFA-STATES, but included as separate slot to avoid computation each time it is needed.")
   (%dead-end :type boolean
              :reader dead-end-p
              :documentation "Dead-end status, derived from corresponding NFA states."))
  (:documentation "DFA state corresponding to an NFA closure union."))

(defmethod initialize-instance :after ((dfa-state dfa-state) &key nfa-states)
  "Object initializer that sets candidate-terminal (accepting / non-accepting) and dead-end flags,
based on NFA states corresponding to DFA state being created."
  (let ((matching-or-not (nfa-state:acceptance-nfa-closure-union-p nfa-states))
        (dead-end (dolist (nfa-state nfa-states nil)
                    (when (nfa-state:dead-end-p nfa-state)
                      (return t)))))
    (with-slots (#1=%candidate-matching-point #2=%dead-end) dfa-state
      (setf #1# matching-or-not
            #2# dead-end))))

(defun dfa-state-definitely-terminal-p (dfa-state)
  "Indicate whether the DFA state DFA-STATE is definitely terminal, in other words, having no
transitions out, besides being candidate terminal (i.e. acceptance). It must be used only in regex
matching (after DFA is completely constructed).
NOTE: with the implementation of regex negation, and hence the introduction of dead-end states (that
have no transitions out, but also are not acceptance), it's not sufficient to identify acceptance
by just having no transitions out, hence, we're also checking the candidate terminal status. I'm
still not sure if we'll have such case in DFA, will revise if we need this change.
UPDATE: I'm also taking dead-end status into consideration, since it should override terminal
status. Actually I need to investigate this further, as I think this condition should not happen,
and would indicate bug. Also more likely that the matching function would need to be modified to
check the dead-end status. UPDATE: consider (not (or (not #\B) (not #\D))), this results in the
case of a transition from 'B' to two NFA states, one is terminal, and the other is dead-end. Now the
meaning should reject all characters, since the inner OR accepts all characters. This means that the
condition in fact could happen, and in this example, we see that the dead-end status should override
the terminal (acceptance) status."
  (and (null (transitions dfa-state))
       (candidate-matching-point-p dfa-state)))

(defun dfa-state-dead-end-p (dfa-state)
  "Indicate whether the DFA state DFA-STATE is dead-end, in other words, having no
transitions out, but not candidate matching point. It must be used only in regex
matching (after DFA is completely constructed). UPDATE: not sure which should have priority:
being candidate matching point or being dead-end. Having both in the same closure could be
problematic (bug??). I'm still experimenting, and now I'll make dead-end status override candidate
matching status. Also I see confusion: dead-end in NFA VS dead-end in DFA (different meanings?).
These states are ones that were negated by an outer negation element. Currently not used, and may
remove it (see DFA-STATE-DEFINITELY-TERMINAL-P)."
  (and (null (transitions dfa-state))
       (not (candidate-matching-point-p dfa-state))))

(defun lookup-dfa-transition (simple-element origin-dfa-state)
  "Find whether there is already a transition on SIMPLE-ELEMENT in ORIGIN-DFA-STATE. A simple
element can be either single char, char range."
  (declare (dfa-state origin-dfa-state))
  (assoc simple-element (slot-value origin-dfa-state '%transitions)
         :test #'elm:simple-element-equal))

(defun add-dfa-transition (origin-dfa-state simple-element destination-dfa-state)
  (declare (dfa-state origin-dfa-state destination-dfa-state))
  (if (lookup-dfa-transition simple-element origin-dfa-state)
      (error "Transition already present (BUG?):")
      (push (cons simple-element destination-dfa-state) (transitions origin-dfa-state))))

;;; TODO: produce-dfa and parse-and-produce-dfa can be converted into methods of a single generic
;;; Note that in lookup, we cannot pass the state union instead of closure union, and this is
;;; because the same closure union could result from two different state unions,
;;; and we need to lookup the same entry for both, in such case. Of course, we could still pass the,
;;; closure, and compute the union inside `find-dfa-state`, but we also need the union in
;;; `create-nfa-normalized-transition-table`, so we would need to compute it twice (may think about
;;; rearranging code.
(defun produce-dfa (nfa-root-state)
  "Produce DFA state machine, given NFA state machine's root state `nfa-root-state` Returns root
DFA state."
  (declare (type nfa-state:nfa-state nfa-root-state))
  (let ((nfa-root-state-closure (nfa-state:prepare-nfa-state-closure nfa-root-state))
        ( traversed-dfa-states nil))
    (labels ((produce-dfa-rec (nfa-states)
               "Produce DFA recursively, starting from `nfa-states`, which represent an NFA states
 closure union, to which the DFA state will correspond."
               (declare (cons nfa-states))
               #+debug (format t "Traversed DFA lookup table so far: ~a.~&" traversed-dfa-states)
               (labels ((lookup-dfa-state ()
                          "Look up entry in traversal lookup table, return DFA if found, or NIL."
                          (loop for (nfa-states-i . dfa-state-i) in traversed-dfa-states
                                unless (set-exclusive-or nfa-states nfa-states-i)
                                  do (return-from lookup-dfa-state dfa-state-i))
                          nil)
                        (find-dfa-state ()
                          "Look up NFA state closure union provided in NFA-STATES, in the
TRAVERSED-DFA-STATES DFA lookup a-list. If not found, it creates a new DFA state and appends an
entry for it it to TRAVERSED-DFA-STATES. Finally it returns the DFA state, indicating whether it's
already found or newly created."
                          (let ((dfa-state (lookup-dfa-state)))
                            (if dfa-state
                                (values dfa-state 'already-found)
                                (let ((new-dfa-state (make-instance 'dfa-state
                                                                    :nfa-states nfa-states)))
                                  (push (cons nfa-states new-dfa-state) traversed-dfa-states)
                                  (values new-dfa-state 'newly-created))))))
                 (multiple-value-bind (dfa-state found-or-new) (find-dfa-state)
                   (if (eq found-or-new 'already-found)
                       dfa-state
                       (let ((nfa-normalized-transition-table
                               (nfa-state:create-nfa-normalized-transition-table nfa-states)))
                         ;; compute union of state closures from union of states
                         (loop for (element . dest-state-union) in nfa-normalized-transition-table
                               ;; TODO: can't we do this prepare call within the call to
                               ;; nfa:create-nfa-normalized-transition-table? See comment above (we
                               ;; need it also in find-dfa-state.
                               for dest-closure-union = (nfa-state:prepare-nfa-state-closure-union
                                                         dest-state-union)
                               for dest-dfa = (produce-dfa-rec dest-closure-union)
                               do (add-dfa-transition dfa-state element dest-dfa))
                         dfa-state))))))
      (produce-dfa-rec nfa-root-state-closure))))

(defun regex-element-to-dfa (regex)
  "Produce DFA state machine, given given regex element `regex`. Returns root DFA state."
  (let ((root-nfa-state (nfa:produce-nfa regex)))
    (produce-dfa root-nfa-state)))


;;; Note: currently returning only destination DFA state, may find later that I need
;;; the match criterion as well (i.e. not extracting the CDR part).
(defun find-matching-transition (origin-dfa-state char)
  "Find matching transition from ORIGIN-DFA-STATE, based on input CHAR.
Returns destination DFA state. This function is used by the regex matching logic, unlike the
above functions which are used during the DFA tree preparation. Later may rearrange to make this
distinction clear."
  (cdr (assoc char (transitions origin-dfa-state)
              :test #'elm:match-char-against-simple-element)))

;;; ---------
;;; FSM methods are currently used in Graphviz Dot diagram generation only
;;; ---------

(defmethod fsm:fsm-acceptance-state-p ((fsm-state dfa-state))
  (candidate-matching-point-p fsm-state))

(defmethod fsm:fsm-dead-end-state-p ((fsm-state dfa-state))
  (dead-end-p fsm-state))

(defmethod fsm:traverse-fsm-transitions ((root-state dfa-state) traversal-fn)
  "Traverse all transitions in the DFA state machine, starting from ROOT-STATE. TRAVERSAL-FN is
 called for each transition."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (dfa-state)
               (unless (gethash dfa-state traversal-mark-lookup-table)
                 (setf (gethash dfa-state traversal-mark-lookup-table) t)
                 (loop for (elem . next-state) in (transitions dfa-state)
                       do (funcall traversal-fn dfa-state elem next-state)
                          (iter next-state)))))
      (iter root-state))))
