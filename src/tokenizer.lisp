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

(defclass transition ()
  ((atom-handling :initarg :atom-handling
                  :initform (error "Mandatory: atom-handling!")
                  :reader atom-handling)))

(defclass transition-to-other (transition)
  ((next-state :initarg :next-state
               :initform (error "Mandatory: next-state!")
               :type (or normal-state terminal-state))))

(defclass transition-to-self (transition)
  ())

;;; TODO: alternatively (probably for better performance, may change to defun.
;;; In this case, also combine transition hierarchy into a single class.
(defgeneric get-next-state (transition current-state))

(defmethod get-next-state ((transition transition-to-other) current-state)
  (slot-value transition 'next-state))

(defmethod get-next-state ((transition transition-to-self) current-state)
  current-state)

;;; prepare initial FSM, given the tokenizer input
(defgeneric init-fsm (input))

;;; retrieve atom from input in fsm (and keeping it there)
(defgeneric retrieve-atom (fsm))

;;; fsm -> input empty/not empty
(defgeneric input-empty-p (fsm))

;;; return updated FSM based on atom-handling
;;; may return same FSM object or a new one (up to implementor)
(defgeneric update-fsm (fsm atom-handling))

;;; tokenizer state handling methods
(defgeneric transit (origin-state fsm))

(defclass tokenizer-output ()
  ((token :initarg :token
          :initform (error "Token (acceptance or error) is mandatory")
          :reader token)
   (fsm :initarg :fsm
        :initform (error "fsm (tokenizer status) is mandatory")
        :reader fsm)))

;;; token + fsm -> tokenizer output
(defun prepare-output (fsm token)
  (make-instance 'tokenizer-output :token token :fsm fsm))

;;; final state: prepare output based on FSM and token
(defmethod transit ((current-state terminal-state) fsm)
  (let ((terminal-token (slot-value current-state 'token)))
    (prepare-output fsm terminal-token)))

;;;for normal state, use next atom from input to find transition and follow transition,
;;;unless when input is empty, at which case, we terminate with default token of current state
(defmethod transit ((current-state normal-state) fsm)
  (if (input-empty-p fsm)
      (prepare-output fsm (slot-value current-state 'token-on-no-input))
      (let* ((atom (retrieve-atom fsm))
             (transition (funcall (slot-value current-state 'transition-finder-func) atom))
             (next-state (get-next-state transition current-state))
             (atom-handling (atom-handling transition))
             (updated-fsm (update-fsm fsm atom-handling)))
        (transit next-state updated-fsm))))


;;; initiate the traversal of states, starting from an "initial state"
;;; assumes the FSM is prepared once (before scanning 1st token), using init-fsm
;;; returns the tokenizer output
;;; subsequent calls to tokenize expect the FSM to progress through the input
;;; so the retrieve-updated-fsm method should be called on the output to retrieve the updated FSM
;;; for the next tokenizer run.
(defun tokenize (initial-state fsm)
  "Tokenization function that should be used by the user."
  (transit initial-state fsm))

;;; Simplifies the usage of the tokenizer, by providing a lambda that can be called sequentially
;;; to retrieve token by token. See the basic text tokenizer for demonstration (test code).
(defun create-tokenizer (initial-state input)
  (let ((fsm (init-fsm input)))
    (lambda ()
      (let ((output (tokenize initial-state fsm)))
        (setf fsm (slot-value output 'fsm))
        output))))
