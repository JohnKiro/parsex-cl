#|
Dec 2023. Updated based on "token-scanner.d" (dlang).

Also more simplicity, by using functions instead of interfaces that have
a single method. Hope this will be simplest and most elegant version.
|#
(in-package :parsex-cl.tokenizer)


;;; Scanner normal state.
;;; Slots:
;;; - transition finder function (atom -> transition)
;;; - token on no input: token to return when no atoms are left in input
(defclass normal-state ()
  ((transition-finder-func :initarg :transition-finder-func
                           :initform (error "Mandatory: transition-finder-func")
                           :reader transition-finder-func
                           :type function)
   (token-on-no-input :initarg :token-on-no-input
                      :initform (error "Mandatory: token-on-no-input")
                      :reader token-on-no-input)))

;;; Scanner terminal state.
;;; Slots:
;;; - token: Scanner's output token
(defclass terminal-state ()
  ((token :initarg :token :initform (error "Mandatory: token") :reader token)))



;;; tokenizer finite state machine configuration
;;; this abstraction combines the following concepts:
;;; input, atom accumulator, transition.
;;; TODO: think about whether this is good or not!
(defclass tokenizer-fsm-config ()
  ((fsm-prep-func :initarg :fsm-prep-func
                  :initform (error "Mandatory: FSM prep func.")
                  :documentation "Finite State Machine prep func"
                  :type function)
   (input-empty-predicate-func :initarg :input-empty-predicate-func
                               :initform (error "Mandatory: input-empty-predicate-func")
                               :documentation "Predicate to test whether input is empty"
                               :type function)
   (atom-retrieving-func :initarg :atom-retrieving-function
                         :initform (error "Mandatory: Atom retrieving func.")
                         :documentation "Retrieves next atom from FSM."
                         :type function)
   (fsm-updating-func :initarg :fsm-updating-func
                      :initform (error "Mandatory: FSM updating function.")
                      :documentation "Updates FSM state, based on transition"
                      :type function)
   (next-state-retrieval-func :initarg :next-state-retrieval-func
                      :initform (error "Mandatory: Next state retrieval function.")
                      :documentation "Retrieve next state from transition"
                      :type function)
   (output-prep-func :initarg :output-prep-func
                     :initform (error "Mandatory: Tokenizer output prep func")
                     :documentation "Prepares tokenizer's output"
                     :type function)))


;;; tokenizer state handling methods
(defgeneric transit (current-state fsm-config fsm))

;;; final state: prepare output based on FSM and token
(defmethod transit ((current-state terminal-state) (fsm-config tokenizer-fsm-config) fsm)
  (let ((terminal-token (slot-value current-state 'token))
        (output-prep-func (slot-value fsm-config 'output-prep-func)))
    (funcall output-prep-func terminal-token fsm)))

;;;for normal state, use next atom from input to find transition and follow transition,
;;;unless when input is empty, at which case, we terminate with default token of current state
(defmethod transit ((current-state normal-state) (fsm-config tokenizer-fsm-config) fsm)
  (with-slots (input-empty-predicate-func
               atom-retrieving-func
               fsm-updating-func
               next-state-retrieval-func
               output-prep-func) fsm-config
    (if (funcall input-empty-predicate-func fsm)
        (funcall output-prep-func
                 (slot-value current-state 'token-on-no-input)
                 fsm)
        (let* ((atom (funcall atom-retrieving-func fsm))
               (transition (funcall (slot-value current-state 'transition-finder-func) atom))
               (next-state (funcall next-state-retrieval-func transition current-state))
               (updated-fsm (funcall fsm-updating-func fsm transition)))
          (transit next-state fsm-config updated-fsm)))))


;;; initiate the traversal of states, starting from an "initial state"
(defun tokenize (fsm-config input initial-state)
  "Tokenization function that should be used by the user."
  (let ((initial-fsm (funcall (slot-value fsm-config 'fsm-prep-func) input)))
    (transit initial-state fsm-config initial-fsm)))
