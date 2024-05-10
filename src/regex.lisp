(in-package :parsex-cl.regex)

;;; TODO: split into multiple files.

#||
Other element types not needing special class:
- symbols (currently only :any-char (corresponds to . in regex).
- single character (for now, I'm encapsulating it in single-char-element, mainly to group
  it together with char-range-element as simple-element).
- string (string of character corresponds to a sequence-element where all elements are characters).
||#


;;; Single character elements (range, any-char, specific char)
(defclass simple-element () ()
  (:documentation "Base class for single char regex elements (single char and char range)."))

;;; TODO: probably remove (use character directly). The problem is that I would have two
;;; identical produce-nfa methods :(
;;; Also may need an object, to include some properties, such as "accepted token" (not sure).
(defclass single-char-element (simple-element)
  ((single-char :initarg :single-char :initform (error "Mandatory")
                :reader single-char :type character))
  (:documentation "Wrapper object for single-char elements."))

;; TODO: add constructor that ensures char-end >= char-start
(defclass char-range-element (simple-element)
  ((char-start :initarg :char-start :initform (error "Mandatory")
               :reader char-start :type character)
   (char-end :initarg :char-end :initform (error "Mandatory")
             :reader char-end :type character))
  (:documentation "Char range regex element."))

(defmethod print-object ((object single-char-element) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "(~a)" (single-char object))))

(defmethod print-object ((object char-range-element) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots ((s char-start) (e char-end)) object
      (format stream "(~a - ~a)" s e ))))


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

;;TODO: probably will remove the single-char-element (being almost a useless wrapper)
(defmethod regex-to-nfa ((regex character) input-nfa-state)
  (let ((elem (make-instance 'single-char-element :single-char regex))
        (output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state elem output-state)
    output-state))

;; NOTE: the only symbol currently supported is :any-char, but I chose to have more generic code
;; than specializing on (eql :any-char).
(defmethod regex-to-nfa ((regex symbol) input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex cons) input-nfa-state)
  (compound-regex-to-nfa (car regex) (cdr regex) input-nfa-state))

;;TODO: probably will remove the single-char-element (being almost a useless wrapper)
(defmethod regex-to-nfa ((regex string) input-nfa-state)
  (loop for ch across regex
        for elem = (make-instance 'single-char-element :single-char ch)
        for in-state = input-nfa-state then out-state
        for out-state = (make-instance 'nfa-state)
        do (add-nfa-normal-transition in-state elem out-state)
        finally (return out-state)))

(defmethod compound-regex-to-nfa ((regex-head (eql :char-range)) regex-tail input-nfa-state)
  (let* ((output-state (make-instance 'nfa-state)))
    (destructuring-bind (char-start char-end) regex-tail
      (add-nfa-normal-transition input-nfa-state
                                 (make-instance 'char-range-element
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
            :type (or simple-element character (eql :any-char)))
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


;;; Extract E-closure for a certain state (by recursively collecting auto states),
;;; and combine it with input accumulation of other states' closures.
;;; Arguments:
;;; - state: state for which closure is to be computed.
;;; - initial-accumulator: accumulator of closures to be combined with this one.
;;; TODO: may change recursion into iteration.
;;; TODO: may change accumulation in list into a hashtable (for perf).
;;; NOTE: Since a single instance is created for each state, so address comparison
;;; (using EQ) is sufficient.
(defun prepare-nfa-state-closure (initial-accumulator state)
  (labels ((nfa-state-closure-recurse (initial-accumul state-iter)
             (if (member state-iter initial-accumul :test #'eq)
                 initial-accumul ;state has already been traversed
                 (let ((accumul (cons state-iter initial-accumul))
                       (direct-autos (slot-value state-iter 'auto-transitions)))
                   (etypecase direct-autos
                     (null accumul)
                     (cons (reduce #'nfa-state-closure-recurse
                                   direct-autos
                                   :initial-value accumul)))))))
    (nfa-state-closure-recurse initial-accumulator state)))


;;; Extracts a union of NFA closures for a set of states.
;;; NOTE: duplication is automatically handled by prepare-nfa-state-closure.
(defun prepare-nfa-state-closure-union (states)
  (let ((result (reduce #'prepare-nfa-state-closure states :initial-value nil)))
    result))


;;; Inserts a character in a sorted list (maintaining ascending order).
;;; Used in char range splitting (to remove overlaps).
;;; NOTE: in case of initial empty list, or insertion at head, returned pointer
;;; must be used (since the function is non-destructive in this case). So
;;; in general, need to save the returned pointer.
;;; Candidate for move to a utils package!
;;; TODO: simplify?
(defun insert-char-in-order (char chars)
  (cond
    ((null chars) (cons char nil))
    ((char< char (car chars)) (cons char chars))
    ((char= char (car chars)) chars)
    (t (labels ((insert-it-recurse (previous-pointer)
                  ;;NOTE: returned value actually not used
                  (let ((current-pointer (cdr previous-pointer)))
                    (if (null current-pointer)
                        (setf (cdr previous-pointer) (cons char nil))
                        (let ((char-iter (car current-pointer)))
                          (cond
                            ((char> char char-iter) (insert-it-recurse current-pointer))
                            ((char= char char-iter) current-pointer)
                            (t (let ((new-cell (cons char current-pointer)))
                                 (setf (cdr previous-pointer) new-cell)))))))))
         (insert-it-recurse chars)
         chars))))

;;; Utility function to increment a character
;;; TODO: utils package.
(defun inc-char (ch)
  (code-char (1+ (char-code ch))))

;;; Utility function to decrement a character
;;; TODO: utils package.
(defun dec-char (ch)
  (code-char (1- (char-code ch))))


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
              (single-char-element (setf result (insert-char-in-order (dec-char
                                                                       (single-char element))
                                                                      result))
                                   (setf result (insert-char-in-order (single-char element)
                                                                      result)))
              (char-range-element (setf result (insert-char-in-order (dec-char
                                                                      (char-start element))
                                                                     result))
                                  (setf result (insert-char-in-order (char-end element)
                                                                     result))))))))))


;;; Splits a char range (specified by start and end) into a number of ranges
;;; based on provided splitting points (list of characters).
;;; Preconditions: end >= start, splitting points must be a sorted list.
;;; TODO: may use other data structures later.
(defun split-char-range (char-range-element splitting-points)
  (with-slots ((start char-start) (end char-end)) char-range-element
    (labels
        ((split-range-recurse (next-start splitting-points acc)
           (cond
             ((char> next-start end) acc) ;happens only if start and end are mixed up
             ((null splitting-points) (cons (make-instance 'char-range-element
                                                           :char-start next-start
                                                           :char-end end) acc))
             (t (let ((next-splitting-pt (car splitting-points))
                      (subsequent-splitting-pts (cdr splitting-points)))
                  (cond
                    ((char>= next-splitting-pt end) (cons (make-instance 'char-range-element
                                                                         :char-start next-start
                                                                         :char-end end) acc))
                    ((char>= next-splitting-pt next-start) (split-range-recurse
                                                            (inc-char next-splitting-pt)
                                                            subsequent-splitting-pts
                                                            (cons
                                                             (make-instance
                                                              'char-range-element
                                                              :char-start next-start
                                                              :char-end next-splitting-pt)
                                                             acc)))
                    (t (split-range-recurse next-start subsequent-splitting-pts acc))))))))
      (split-range-recurse start splitting-points nil))))

;;; Element comparison functions and methods. The functions are handy when the type is known.
(defun char-range-equal (elem other-obj)
  (and (typep other-obj 'char-range-element)
       (eql (char-start elem) (char-start other-obj))
       (eql (char-end elem) (char-end other-obj))))

(defun single-char-equal (elem other-obj)
  (and (typep other-obj 'single-char-element)
       (eql (single-char elem) (single-char other-obj))))

(defun simple-element-equal (element other-obj)
  (etypecase element
    (single-char-element (single-char-equal element other-obj))
    (char-range-element (char-range-equal element other-obj))
    (symbol (eq element :any-char))))


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
                    (char-range-element (let ((split-ranges
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
;;; association of transitions (simple-element --> next DFA state).
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
transitions out. It must be used only in regex matching (after DFA is completely constructed)."
  (null (transitions dfa-state)))

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
  (let ((nfa-root-state-closure (prepare-nfa-state-closure nil nfa-root-state))
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


(defclass dfa-fsm-operators ()
  ((input-empty-predicate :initarg :input-empty-predicate
                          :initform (error "input-empty-predicate is mandatory!"))
   (read-input-fn :initarg :read-input-fn
                  :initform (error "read-input-fn is mandatory!"))
   (advance-input-fn :initarg :advance-input-fn
                     :initform (error "advance-input-fn is mandatory!"))
   (append-to-accumulator-fn :initarg :append-to-accumulator-fn
                             :initform nil)))


;;; Note: currently returning only destination DFA state, may find later that I need
;;; the match criterion as well (i.e. not extracting the CDR part).
(defun find-matching-transition (origin-dfa-state char)
  "Find matching transition from ORIGIN-DFA-STATE, based on input CHAR.
Returns destination DFA state."
  (or (cdr (assoc char (transitions origin-dfa-state)
                  :test (lambda (ch elem)
                          (etypecase elem
                            (single-char-element (char= ch (single-char elem)))
                            (char-range-element (and (char>= ch (char-start elem))
                                                     (char<= ch (char-end elem))))))))
      (transition-on-any-other origin-dfa-state)))

(defstruct regex-matching-result
  input-interface-fn
  (accumulator-interface-fn nil)
  (status nil :type (or (eql :regex-matched) (eql :regex-not-matched)))
  (token :tokens-not-implemented-yet))

(defun match-regex (input-interface-fn root-dfa-state &key (accumulator-interface-fn nil))
  (multiple-value-bind (input-empty-p read-input-fn advance-input-fn)
      (funcall input-interface-fn)
    (labels ((prepare-result (status)
               (make-regex-matching-result :input-interface-fn input-interface-fn
                                           :accumulator-interface-fn accumulator-interface-fn
                                           :status status))
             (transit (origin-dfa-state)
               (if (dfa-state-definitely-terminal-p origin-dfa-state)
                   (prepare-result :regex-matched)
                   (if (funcall input-empty-p)
                       (prepare-result (if (candidate-terminal origin-dfa-state)
                                           :regex-matched
                                           :regex-not-matched))
                       (let* ((next-ch (funcall read-input-fn))
                              (dest-dfa-state (find-matching-transition origin-dfa-state next-ch)))
                         (if dest-dfa-state
                             (let ((append-to-accumulator-fn
                                     (nth-value 1 (if accumulator-interface-fn
                                                      (funcall accumulator-interface-fn)
                                                      (values nil nil)))))
                               (funcall advance-input-fn)
                               (and append-to-accumulator-fn
                                    (funcall append-to-accumulator-fn next-ch))
                               (transit dest-dfa-state))
                             (prepare-result (if (candidate-terminal origin-dfa-state)
                                                 :regex-matched
                                                 :regex-not-matched))))))))
      (transit root-dfa-state))))



;;; TODO: may make a reusable OOP framework like in PAIP ch. 13
(defun create-basic-accumulator (&key (initial-size 20))
  (let* ((accumul (make-array initial-size :element-type 'character
                                           :adjustable t :fill-pointer 0))
         (retrieve-accumulator-value-fn (lambda () accumul))
         (append-to-accumulator-fn (lambda (char)
                                        (vector-push-extend char accumul))))
    (lambda ()
      (values retrieve-accumulator-value-fn append-to-accumulator-fn))))


(defun create-basic-input (input-text &key (initial-reading-index 0) (make-copy nil))
  (declare (string input-text)
           (fixnum initial-reading-index))
  (let* ((index initial-reading-index)
        (text (if make-copy
                  (copy-seq input-text)
                  input-text))
        (input-empty-predicate (lambda ()
                                 (>= index (length text))))
        (read-input-fn (lambda ()
                         (char text index)))
        (advance-input-fn (lambda ()
                            (incf index))))
    (lambda ()
      (values input-empty-predicate read-input-fn advance-input-fn))))

;;; Public interface function (regex --> DFA root state)
(defun parse-and-produce-dfa (regex)
  (let ((root-nfa-state (parse-and-produce-nfa regex)))
    (produce-dfa root-nfa-state)))
