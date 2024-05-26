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
                                 (make-instance 'char-range
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
   ;;prohibiting incosistency (introduce class hierarchy level)
   (terminus :initform nil :type (or null t) :accessor terminus)))

;;;TODO: CHECK WITH NOT-ELEMENT above (remove one of them?)
(defclass negated-nfa-state (nfa-state)
  ((negated-state :initform (error "Negated state is mandatory!") :type nfa-state)))


;;; defines a normal NFA transition upon matching of ELEMENT, to NEXT-STATE.
;;; TODO: for now, element type restricted to one of three types with character hardcoded.

;;; NOTE: currently the any-char transitions are separated and not represented as nfa-transition.
(defclass nfa-transition ()
  ((element :initarg :element :initform (error "element must be specified!")
            :type (or character char-range (eql :any-char)))
   (next-state :initarg :next-state :initform (error "next-state must be specified!")
               :type (or null nfa-state))))

(defun add-nfa-normal-transition (orig-state element dest-state)
  (let ((transition (make-instance 'nfa-transition :element element :next-state dest-state)))
    (push transition (normal-transitions orig-state))))

;;; add auto transition to an NFA state
(defun add-nfa-auto-transition (orig-state dest-state)
  (push dest-state (auto-transitions orig-state)))

;;; NFA state defines a normal transition table (element --> next state), E-transitions (auto),
;;; and a default transition (transition on any other input, including case no input). Also it
;;; includes a negated-state which is used in case the state is result of expansion of a NOT
;;; element

;;; predicate - whether NFA state is a result of a NOT 
(defun negated-nfa-state-p (state)
  (not (null (nfa-state-negated-state state))))


;;; TODO: may change recursion into iteration.
;;; TODO: may change accumulation in list into a hashtable (for perf).
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
               (setf result (insert-char-in-order (dec-char element) result))
               (setf result (insert-char-in-order element result)))
              (char-range
               (setf result (insert-char-in-order (dec-char (char-start element)) result))
               (setf result (insert-char-in-order (char-end element) result))))))))))


(defun simple-element-equal (element other-obj)
  (labels ((char-range-equal (elem other-obj)
             (and (typep other-obj 'char-range)
                  (eql (char-start elem) (char-start other-obj))
                  (eql (char-end elem) (char-end other-obj))))
           (single-char-equal (elem other-obj)
             (and (typep other-obj 'character)
                  (char= elem other-obj))))
    (etypecase element
      (character (single-char-equal element other-obj))
      (char-range (char-range-equal element other-obj))
      (symbol (eq element :any-char)))))


(defun create-nfa-transition-association-collection (range-splitting-points)
  "Creates an environment and interface for NFA transition associations, which map simple
elements (single char, char range, any-char) to set of destination NFA states (representing the
state's closure). The interface provides two functions: one to add transition (includes splitting
range into sub-ranges as needed, based on character splitting points), and another to create
iterators on the transitions."
  (let ((assoc-list nil))
    (labels ((add-trans (element next-state)
               (let* ((entry (assoc element assoc-list
                                    :test #'simple-element-equal))
                      (arr (or (cdr entry)
                               (make-array 10 :adjustable t :fill-pointer 0))))
                 (vector-push-extend next-state arr)
                 (unless entry
                   (push (cons element arr) assoc-list)))))
      (let ((transition-adder
              (lambda (transition)
                (with-slots (element next-state) transition
                  (typecase element
                    (char-range (let ((split-ranges
                                        (split-char-range element range-splitting-points)))
                                  (dolist (r split-ranges)
                                    (add-trans r next-state))))
                    (t (add-trans element next-state))))))
            (transition-iterator-factory (lambda ()
                                           (let ((list-iter assoc-list))
                                             (lambda ()
                                               (pop list-iter))))))
        (values transition-adder transition-iterator-factory)))))


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
                       :type (or (eql t) (eql nil))
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
    (when (terminus s)
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
  (assoc simple-element (slot-value origin-dfa-state 'transitions) :test #'simple-element-equal))

(defun add-dfa-transition (origin-dfa-state simple-element destination-dfa-state)
  (declare (dfa-state origin-dfa-state destination-dfa-state))
  (if (eq simple-element :any-char)
      (setf (transition-on-any-other origin-dfa-state) destination-dfa-state)
      (if (lookup-dfa-transition simple-element origin-dfa-state)
          (error "Transition already present (BUG?):")
          (push (cons simple-element destination-dfa-state) (transitions origin-dfa-state)))))

(defun produce-dfa (nfa-root-state)
  (let ((nfa-root-state-closure (prepare-nfa-state-closure-union (list nfa-root-state)))
        (dfa-state-set (create-dfa-state-set)))
    ;;root state's closure union is root state's closure (union of one).
    (produce-dfa-rec nfa-root-state-closure dfa-state-set)))

;;; Note that we cannot pass the state union instead of closure union, and this is
;;; is because the same closure union could result from two different state unions,
;;; and we need to lookup the same entry for both, in such case.
(defun produce-dfa-rec (nfa-state-closure-union traversed-dfa-states)
  (multiple-value-bind (dfa-state found-or-new) (find-dfa-state nfa-state-closure-union
                                                                traversed-dfa-states)
    (if (eq found-or-new 'already-found)
        dfa-state
        (let ((splitting-points (collect-char-range-splitting-points nfa-state-closure-union)))
          (multiple-value-bind (trans-adder-fn trans-iterator-factory-fn)
              (create-nfa-transition-association-collection splitting-points)
            (dolist (nfa-state nfa-state-closure-union)
              ;;; TODO: may consider handling "trans on any char here", but current solution makes
              ;;; code simpler (though could be slightly less performant due to extra check in
              ;;; each transition below.
              (dolist (trans (normal-transitions nfa-state))
                (funcall trans-adder-fn trans)))
            ;;replace each entry value with union of state closures (in place of union of states)
            (loop with iterator-fn = (funcall trans-iterator-factory-fn)
                  for trans = (funcall iterator-fn)
                  while trans
                  for (element . dest-state-union) = trans
                  for dest-closure-union = (prepare-nfa-state-closure-union dest-state-union)
                  for dest-dfa = (produce-dfa-rec dest-closure-union traversed-dfa-states)
                  do (add-dfa-transition dfa-state element dest-dfa)))
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
                            (char-range (and (char>= ch (char-start elem))
                                             (char<= ch (char-end elem))))))))
      (transition-on-any-other origin-dfa-state)))

(defstruct regex-matching-result
  (status nil :type (or (eql :regex-matched) (eql :regex-not-matched)))
  (token :tokens-not-implemented-yet))


(defun match-regex (input-source root-dfa-state &aux (last-candidate-terminal-dfa nil))
  (labels ((prepare-result (status)
             ;;putting this here since we need to call it when scanning is terminated
             (notify-match-termination input-source)
             (make-regex-matching-result :status status))
           (transit (origin-dfa-state)
             (when (candidate-terminal origin-dfa-state)
               (setf last-candidate-terminal-dfa origin-dfa-state)
               (register-candidate-matching-point input-source))
             (if (dfa-state-definitely-terminal-p origin-dfa-state)
                 ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                 ;; match is exact or not. Leaving this up to the caller.
                 (prepare-result :regex-matched)
                 (if (source-empty-p input-source)
                     (prepare-result (if last-candidate-terminal-dfa
                                         :regex-matched
                                         :regex-not-matched))
                     (let* ((next-ch (read-next-item input-source))
                            (dest-dfa-state (find-matching-transition origin-dfa-state next-ch)))
                       (if dest-dfa-state
                           (progn
                             (advance-reading-position input-source)
                             (transit dest-dfa-state))
                           (prepare-result (if last-candidate-terminal-dfa
                                               :regex-matched
                                               :regex-not-matched))))))))
    (transit root-dfa-state)))



;;; Public interface function (regex --> DFA root state)
(defun parse-and-produce-dfa (regex)
  (let ((root-nfa-state (parse-and-produce-nfa regex)))
    (produce-dfa root-nfa-state)))
