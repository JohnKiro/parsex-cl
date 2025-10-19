(in-package :parsex-cl/regex/dfa)

;;;; ---------------------------------------------------
;;;; Code for DFA construction, based on regex (and NFA)
;;;; ---------------------------------------------------

;;; A DFA state contains the union of NFA states that form the DFA state, as well as an
;;; association of transitions (simple element --> next DFA state).
(defclass dfa-state ()
  ((%nfa-states :initarg :nfa-states
                :reader nfa-states
                :initform (error "nfa-states must be specified!")
                :documentation "List of NFA states represented by this DFA state")
   (%transitions :accessor transitions
                 :initform nil
                 :type list
                 :documentation "Transitions to other DFA states, based on single char/char range")
   (%transition-on-any-other :accessor transition-on-any-other
                             :initform nil
                             :type (or null dfa-state)
                             :documentation "destination DFA state on any other char")
   (%candidate-terminal :reader candidate-terminal
                        :type boolean
                        :documentation "whether this is accepting state. It is derived from
NFA-STATES, but included as separate slot to avoid computation each time it is needed.")
   (%dead-end :initarg :dead-end
              :initform nil
              :type boolean
              :reader dead-end-p
              :documentation "Dead-end status, derived from corresponding NFA states."))
  (:documentation "DFA state corresponding to an NFA closure union."))

(defmethod initialize-instance :after ((dfa-state dfa-state) &key)
  "Object initializer that sets CANDIDATE-TERMINAL flag (accepting / non-accepting)"
  (with-slots (%nfa-states %candidate-terminal) dfa-state
    (let ((terminal-or-not (nfa-state:terminal-nfa-closure-union-p %nfa-states)))
      (setf %candidate-terminal terminal-or-not))))

(defun dfa-state-definitely-terminal-p (dfa-state)
  "Indicate whether the DFA state DFA-STATE is definitely terminal, in other words, having no
transitions out, besides being candidate terminal (i.e. acceptance). It must be used only in regex
matching (after DFA is completely constructed).
NOTE: with the implementation of regex negation, and hence the introduction of dead-end states (that
have no transitions out, but also are not acceptance), it's not sufficient to identify acceptance
by just having no transitions out, hence, we're also checking the candidate terminal status. I'm
still not sure if we'll have such case in DFA, will revise if we need this change."
  (and (null (transitions dfa-state))
       (null (transition-on-any-other dfa-state))
       (candidate-terminal dfa-state)))

(defun dfa-state-dead-end-p (dfa-state)
  "Indicate whether the DFA state DFA-STATE is dead-end, in other words, having no
transitions out, but not candidate terminal (i.e. non-accepting). It must be used only in regex
matching (after DFA is completely constructed).
These states are ones that were negated by an outer negation element. Currently not used, and may
remove it (see DFA-STATE-DEFINITELY-TERMINAL-P)."
  (and (null (transitions dfa-state))
       (null (transition-on-any-other dfa-state))
       (not (candidate-terminal dfa-state))))

(defun create-dfa-state-set ()
  (make-array 50 :adjustable t :fill-pointer 0))
;;; TODO: alternative to storing NFA closure union in the DFA state, just for the purpose of
;;; this lookup, use an a-list instead of a vector, that associates the NFA closure to DFA state.
;;; This is because I don't think we need to keep the NFA closure after the DFA state machine is
;;; prepared.


(defun lookup-dfa-transition (simple-element origin-dfa-state)
  "Find whether there is already a transition on SIMPLE-ELEMENT in ORIGIN-DFA-STATE. A simple
element can be either single char, char range, or any-other-char. Note that during normalized
transition table preparation, elements any-char and any-other-char are merged (if both found in the
same NFA closure union. This is because in DFA, both will have the meaning of any-other-char."
  (declare (dfa-state origin-dfa-state))
  (assoc simple-element (slot-value origin-dfa-state '%transitions)
         :test #'elm:simple-element-equal))

(defun add-dfa-transition (origin-dfa-state simple-element destination-dfa-state)
  (declare (dfa-state origin-dfa-state destination-dfa-state))
  ;; TODO: revise later (these are element types or transition types?)
  (if (or (eq simple-element :any-char) (eq simple-element elm::+ANY-OTHER-CHAR-ELEMENT+))
      (if (transition-on-any-other origin-dfa-state)
          (error "A transition on any other is already present!")
          (setf (transition-on-any-other origin-dfa-state) destination-dfa-state))
      (if (lookup-dfa-transition simple-element origin-dfa-state)
          (error "Transition already present (BUG?):")
          (push (cons simple-element destination-dfa-state) (transitions origin-dfa-state)))))

;;; TODO: produce-dfa and parse-and-produce-dfa can be converted into methods of a single generic
(defun produce-dfa (nfa-root-state)
  (let ((nfa-root-state-closure (nfa-state:prepare-nfa-state-closure nfa-root-state))
        (dfa-state-set (create-dfa-state-set)))
    ;;root state's closure union is root state's closure (union of one).
    (produce-dfa-rec nfa-root-state-closure dfa-state-set)))

(defun lookup-dfa-state (nfa-states traversed-dfa-states)
  (declare (cons nfa-states))
  (loop for dfa-state across traversed-dfa-states
        do (let* ((nfa-states-loop (slot-value dfa-state '%nfa-states))
                  (difference (set-exclusive-or nfa-states nfa-states-loop)))
             (unless difference
               (return-from lookup-dfa-state dfa-state))))
  nil)

(defun find-dfa-state (nfa-states traversed-dfa-states)
  "Look up NFA state closure union provided in NFA-STATES, in the TRAVERSED-DFA-STATES 
DFA lookup vector. If not found, it creates a new DFA state and appends it to
TRAVERSED-DFA-STATES. Finally it returns the DFA state, indicating whether it's already
found or newly created."
  (declare (cons nfa-states))
  (let ((dfa-state (lookup-dfa-state nfa-states traversed-dfa-states)))
    (if dfa-state
        (values dfa-state 'already-found)
        (let* ((dead-end (dolist (nfa-state nfa-states nil)
                           (when (nfa-state:dead-end-p nfa-state)
                             (return t))))
               (new-dfa-state (make-instance 'dfa-state :nfa-states nfa-states :dead-end dead-end)))
          (vector-push-extend new-dfa-state traversed-dfa-states)
          (values new-dfa-state 'newly-created)))))

;;; Note that we cannot pass the state union instead of closure union, and this is
;;; because the same closure union could result from two different state unions,
;;; and we need to lookup the same entry for both, in such case.
(defun produce-dfa-rec (nfa-state-closure-union traversed-dfa-states)
  (multiple-value-bind (dfa-state found-or-new) (find-dfa-state nfa-state-closure-union
                                                                traversed-dfa-states)
    (if (eq found-or-new 'already-found)
        dfa-state
        (let ((nfa-normalized-transition-table
                (nfa-state:create-nfa-normalized-transition-table nfa-state-closure-union)))
          ;;replace each entry value with union of state closures (in place of union of states)
          (loop for (element . dest-state-union) in nfa-normalized-transition-table
                ;; TODO: can't we do this prepare call within the call to
                ;; nfa:create-nfa-normalized-transition-table??
                for dest-closure-union = (nfa-state:prepare-nfa-state-closure-union
                                          dest-state-union)
                for dest-dfa = (produce-dfa-rec dest-closure-union traversed-dfa-states)
                do (add-dfa-transition dfa-state element dest-dfa))
          dfa-state))))

;;; Public interface function (regex --> DFA root state)
(defun parse-and-produce-dfa (regex)
  (let ((root-nfa-state (nfa:produce-nfa regex)))
    (produce-dfa root-nfa-state)))


;;; Note: currently returning only destination DFA state, may find later that I need
;;; the match criterion as well (i.e. not extracting the CDR part).
(defun find-matching-transition (origin-dfa-state char)
  "Find matching transition from ORIGIN-DFA-STATE, based on input CHAR.
Returns destination DFA state. This function is used by the regex matching logic, unlike the
above functions which are used during the DFA tree preparation. Later may rearange to make this
distiction clear."
  (or (cdr (assoc char (transitions origin-dfa-state)
                  :test #'elm:match-char-against-simple-element))
      (transition-on-any-other origin-dfa-state)))

;;; ---------
;;; FSM methods are currently used in Graphviz Dot diagram generation only
;;; ---------

(defmethod fsm:fsm-acceptance-state-p ((fsm-state dfa-state))
  (candidate-terminal fsm-state))

(defmethod fsm::fsm-dead-end-state-p ((fsm-state dfa-state))
  (dead-end-p fsm-state))

(defmethod fsm:traverse-fsm-transitions ((root-state dfa-state) traversal-fn)
  "Traverse all transitions in the DFA state machine, starting from ROOT-STATE. This includes
both normal and transitions on any other char. TRAVERSAL-FN is called for each transition."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (dfa-state)
               (unless (gethash dfa-state traversal-mark-lookup-table)
                 (setf (gethash dfa-state traversal-mark-lookup-table) t)
                 (loop for (elem . next-state) in (transitions dfa-state)
                       do (funcall traversal-fn dfa-state elem next-state)
                          (iter next-state))
                 (let ((dest-on-any-other (transition-on-any-other dfa-state)))
                   (when dest-on-any-other
                     (funcall traversal-fn dfa-state :any-other-char dest-on-any-other)
                     (iter dest-on-any-other))))))
      (iter root-state))))
