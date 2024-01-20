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

;;; retrieve atom from input, and keeping it there (shouldn't advance)
(defgeneric retrieve-atom (input))

;;; input empty/not empty
(defgeneric input-empty-p (input))

;;; return updated input based on atom-handling
;;; may return same input object, or a new one (up to implementor)
(defgeneric update-input (input atom-handling))

;;; return updated accumulator based on atom-handling
;;; may return same accumulator object, or a new one (up to implementor)
(defgeneric update-accumulator (accumulator atom atom-handling))

;;; tokenizer state handling methods
(defgeneric transit (origin-state input accumulator))

(defclass tokenizer-output ()
  ((token :initarg :token
          :initform (error "Token (acceptance or error) is mandatory")
          :reader token)
   (tokenizer-input :initarg :tokenizer-input
                    :initform (error "tokenizer-input is mandatory")
                    :reader tokenizer-input)
   (accumulator :initarg :accumulator
                :initform (error "accumulator is mandatory")
                :reader accumulator)))

;;; input + accumulator + token => tokenizer output
(defun prepare-output (input accumulator token)
  (make-instance 'tokenizer-output :tokenizer-input input :accumulator accumulator :token token))

;;; final state: prepare output based on terminal token
(defmethod transit ((current-state terminal-state) input accumulator)
  (let ((terminal-token (slot-value current-state 'token)))
    (prepare-output input accumulator terminal-token)))

;;;for normal state, use next atom from input to find transition and follow transition,
;;;unless when input is empty, at which case, we terminate with default token of current state
(defmethod transit ((current-state normal-state) input accumulator)
  (if (input-empty-p input)
      (prepare-output input accumulator (slot-value current-state 'token-on-no-input))
      (let* ((atom (retrieve-atom input))
             (transition (funcall (slot-value current-state 'transition-finder-func) atom))
             (next-state (get-next-state transition current-state))
             (atom-handling (atom-handling transition))
             (updated-input (update-input input atom-handling))
             (updated-accumulator (update-accumulator accumulator atom atom-handling)))
        (transit next-state updated-input updated-accumulator))))


;;; Initiate the traversal of states, starting from an "initial state"
;;; Returns the tokenizer output.
;;; NOTE: the returned output contains the updated input, for the next tokenization run.
;;; TODO: may remove it since "create-tokenizer" is more handy.
(defun tokenize (initial-state input accumulator-factory-fn)
  "Tokenization function that should be used by the user."
  (let ((initial-accumulator (funcall accumulator-factory-fn)))
    (transit initial-state input initial-accumulator)))

;;; Simplifies the usage of the tokenizer, by providing a lambda that can be called sequentially
;;; to retrieve token by token. See the basic text tokenizer for demonstration (test code).
(defun create-tokenizer (initial-state initial-input accumulator-factory-fn)
  (let ((input initial-input))
    (lambda ()
      (let* ((initial-accumulator (funcall accumulator-factory-fn))
             (output (transit initial-state input initial-accumulator)))
        (setf input (slot-value output 'tokenizer-input))
        output))))
