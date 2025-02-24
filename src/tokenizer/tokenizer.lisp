#|
Dec 2023. Updated based on "token-scanner.d" (dlang).

Also more simplicity, by using functions instead of interfaces that have
a single method. Hope this will be simplest and most elegant version.
|#
(in-package :parsex-cl/tokenizer)

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
;;; TODO: after reconsideration, I'll keep generic methods for usages where the code user
;;; is the one who should provide the behavior. In this case here, we just want to choose
;;; behavior based on state type, which is either TERMINAL-STATE or NORMAL-STATE, so a
;;; function and etypecase will do the job (and code will be simpler).
(defgeneric transit (origin-state input accumulator))


;;; prepare a suitable representation for the result, based on token + accumulator
(defgeneric prepare-tokenization-result (token accumulator))

;;; token + accumulator + input => tokenizer output
(defun prepare-output (token accumulator input-for-next-run)
  (values (prepare-tokenization-result token accumulator)
          input-for-next-run))

;;; final state: prepare output based on terminal token
(defmethod transit ((current-state terminal-state) input accumulator)
  (let* ((terminal-token (terminal-token current-state)))
    (prepare-output terminal-token accumulator input)))

;;;for normal state, use next atom from input to find transition and follow transition,
;;;unless when input is empty, at which case, we terminate with default token of current state
(defmethod transit ((current-state normal-state) input accumulator)
  (if (input-empty-p input)
      (prepare-output (token-on-no-input current-state)
                      accumulator
                      input)
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
;;; TODO: may remove it since "create-tokenizer" is more handy. However, may keep it as
;;; lower-level tokenization function, and use it in create-tokenizer (higher-level interface).
(defun tokenize (initial-state initial-input accumulator-factory-fn)
  "Tokenization function that should be used by the user."
  (let ((initial-accumulator (funcall accumulator-factory-fn)))
    (transit initial-state initial-input initial-accumulator)))

;;; Simplifies the usage of the tokenizer, by providing a lambda that can be called sequentially
;;; to retrieve token by token. See the basic text tokenizer for demonstration (test code).
(defun create-tokenizer (initial-state initial-input accumulator-factory-fn)
  (let ((input initial-input))
    (lambda ()
      (multiple-value-bind  (tokenizer-result input-for-next-run)
          (tokenize initial-state input accumulator-factory-fn)
        (setf input input-for-next-run)
        (values tokenizer-result input-for-next-run)))))


