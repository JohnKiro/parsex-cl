(in-package :parsex-cl.regex)

;;; TODO: split into multiple files.

#||
Other element types not needing special class:
- symbols (currently only :any-char (corresponds to . in regex).
- single character.
- string (string of character corresponds to a sequence-element where all elements are characters).
||#


;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: accept reversed char range
;;;; TODO: may split function using other helper functions (DONE)
;;;; TODO: flatten (simplify specific cases such as seq within seq)
;;;; TODO: more unit test cases, reorganize packages


;;; A DFA state contains the union of NFA states that form the DFA state, as well as an
;;; association of transitions (simple element --> next DFA state).
(defclass dfa-state ()
  ((nfa-states :initarg :nfa-states
               :reader nfa-states
               :initform (error "nfa-states must be specified!"))
   (transitions :accessor transitions :initform nil :type list)
   (transition-on-any-other :accessor transition-on-any-other
                            :initform nil
                            :type (or null dfa-state))
   (candidate-terminal :initarg :candidate-terminal
                       :reader candidate-terminal
                       :type boolean
                       :initform nil)))

(defun dfa-state-definitely-terminal-p (dfa-state)
  "Indicate whether the DFA state DFA-STATE is definitely terminal, in other words, having no
transitions out. It must be used only in regex matching (after DFA is completely constructed). Note
that it will also be a candidate terminal."
  (and (null (transitions dfa-state))
       (null (transition-on-any-other dfa-state))
       (if (candidate-terminal dfa-state)
           t
           (error "A definitely terminal DFA that is not also candidate terminal: BUG??!"))))

(defun create-dfa-state-set ()
  (make-array 50 :adjustable t :fill-pointer 0))
;;; TODO: alternative to storing NFA closure union in the DFA state, just for the purpose of
;;; this lookup, use an a-list instead of a vector, that associates the NFA closure to DFA state.
;;; This is because I don't think we need to keep the NFA closure after the DFA state machine is
;;; prepared.
(defun lookup-dfa-state (nfa-states traversed-dfa-states)
  (declare (cons nfa-states))
  (loop for dfa-state across traversed-dfa-states
        do (let* ((nfa-states-loop (slot-value dfa-state 'nfa-states))
                  (difference (set-exclusive-or nfa-states nfa-states-loop)))
             (unless difference
               (return-from lookup-dfa-state dfa-state))))
  nil)

(defun terminal-nfa-closure-union-p (nfa-states)
  "Determines whether the NFA closure provided in NFA-STATES is terminal, which is the case
when any of the NFA states in the closure is the terminus state produced by the NFA."
  (dolist (s nfa-states nil)
    (when (nfa:terminus s)
      (return t))))

(defun find-dfa-state (nfa-states traversed-dfa-states)
  "Look up NFA state closure union provided in NFA-STATES, in the TRAVERSED-DFA-STATES 
DFA lookup vector. If not found, it creates a new DFA state and appends it to
TRAVERSED-DFA-STATES. Finally it returns the DFA state, indicating whether it's already
found or newly created."
  (declare (cons nfa-states))
  (let ((dfa-state (lookup-dfa-state nfa-states traversed-dfa-states)))
    (if dfa-state
        (values dfa-state 'already-found)
        (let* ((terminal-or-not (terminal-nfa-closure-union-p nfa-states))
               (new-dfa-state (make-instance 'dfa-state
                                             :nfa-states nfa-states
                                             :candidate-terminal (if terminal-or-not
                                                                     t
                                                                     nil))))
          (vector-push-extend new-dfa-state traversed-dfa-states)
          (values new-dfa-state 'newly-created)))))


(defun lookup-dfa-transition (simple-element origin-dfa-state)
  "Find whether there is already a transition on SIMPLE-ELEMENT in ORIGIN-DFA-STATE."
  (declare (dfa-state origin-dfa-state))
  (assoc simple-element (slot-value origin-dfa-state 'transitions) :test #'nfa:simple-element-equal))

(defun add-dfa-transition (origin-dfa-state simple-element destination-dfa-state)
  (declare (dfa-state origin-dfa-state destination-dfa-state))
  (if (eq simple-element :any-char)
      (if (transition-on-any-other origin-dfa-state)
          (error "A transition on any other is already present!")
          (setf (transition-on-any-other origin-dfa-state) destination-dfa-state))
      (if (lookup-dfa-transition simple-element origin-dfa-state)
          (error "Transition already present (BUG?):")
          (push (cons simple-element destination-dfa-state) (transitions origin-dfa-state)))))

(defun produce-dfa (nfa-root-state)
  (let ((nfa-root-state-closure (nfa:prepare-nfa-state-closure-union (list nfa-root-state)))
        (dfa-state-set (create-dfa-state-set)))
    ;;root state's closure union is root state's closure (union of one).
    (produce-dfa-rec nfa-root-state-closure dfa-state-set)))

;;; Note that we cannot pass the state union instead of closure union, and this is
;;; because the same closure union could result from two different state unions,
;;; and we need to lookup the same entry for both, in such case.
(defun produce-dfa-rec (nfa-state-closure-union traversed-dfa-states)
  (multiple-value-bind (dfa-state found-or-new) (find-dfa-state nfa-state-closure-union
                                                                traversed-dfa-states)
    (if (eq found-or-new 'already-found)
        dfa-state
        (let ((nfa-normalized-transition-table-iterator-fn
                (nfa:create-nfa-normalized-transition-table-iterator nfa-state-closure-union)))
          ;;replace each entry value with union of state closures (in place of union of states)
          (loop for trans = (funcall nfa-normalized-transition-table-iterator-fn)
                while trans
                for (element . dest-state-union) = trans
                for dest-closure-union = (nfa:prepare-nfa-state-closure-union dest-state-union)
                for dest-dfa = (produce-dfa-rec dest-closure-union traversed-dfa-states)
                do (add-dfa-transition dfa-state element dest-dfa))
          dfa-state))))

;;; Note: currently returning only destination DFA state, may find later that I need
;;; the match criterion as well (i.e. not extracting the CDR part).
(defun find-matching-transition (origin-dfa-state char)
  "Find matching transition from ORIGIN-DFA-STATE, based on input CHAR.
Returns destination DFA state."
  (or (cdr (assoc char (transitions origin-dfa-state)
                  :test (lambda (ch elem)
                          (etypecase elem
                            (character (char= ch elem))
                            (chars:char-range (and (char>= ch (chars:char-start elem))
                                             (char<= ch (chars:char-end elem))))))))
      (transition-on-any-other origin-dfa-state)))

(defstruct regex-matching-result
  (status nil :type (or (eql :regex-matched) (eql :regex-not-matched)))
  (token :tokens-not-implemented-yet))


(defun match-regex (input-source root-dfa-state &aux (last-candidate-terminal-dfa nil))
  (labels ((prepare-result (status)
             ;;putting this here since we need to call it when scanning is terminated
             (input:notify-match-termination input-source)
             (make-regex-matching-result :status status))
           (transit (origin-dfa-state)
             (when (candidate-terminal origin-dfa-state)
               (setf last-candidate-terminal-dfa origin-dfa-state)
               (input:register-candidate-matching-point input-source))
             (if (dfa-state-definitely-terminal-p origin-dfa-state)
                 ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                 ;; match is exact or not. Leaving this up to the caller.
                 (prepare-result :regex-matched)
                 (if (input:source-empty-p input-source)
                     (prepare-result (if last-candidate-terminal-dfa
                                         :regex-matched
                                         :regex-not-matched))
                     (let* ((next-ch (input:read-next-item input-source))
                            (dest-dfa-state (find-matching-transition origin-dfa-state next-ch)))
                       (if dest-dfa-state
                           (progn
                             (input:advance-reading-position input-source)
                             (transit dest-dfa-state))
                           (prepare-result (if last-candidate-terminal-dfa
                                               :regex-matched
                                               :regex-not-matched))))))))
    (transit root-dfa-state)))



;;; Public interface function (regex --> DFA root state)
(defun parse-and-produce-dfa (regex)
  (let ((root-nfa-state (nfa:parse-and-produce-nfa regex)))
    (produce-dfa root-nfa-state)))
