(in-package :parsex-cl.basic-string-tokenizer)

#|
Implementation of a tokenizer for string-based source.
input: string
fsm: class (resizable vector of characters for atoms, reference to input string)
Initially, I'll use a list of chars for simplicity
output: contents of fsm + acceptance/error token
|#

(defclass string-fsm ()
  ((input-text :initarg :input-text :reader input-text)
   (reading-index :initform 0 :accessor reading-index)
   (accumulator :initform '() :accessor accumulator)))

;;; prepare initial FSM, given the tokenizer input
(defmethod init-fsm ((input string))
  (make-instance 'string-fsm :input-text input))

(defmethod input-empty-p ((fsm string-fsm))
  (>= (reading-index fsm)
      (length (input-text fsm))))

(defmethod retrieve-atom ((fsm string-fsm))
  (aref (input-text fsm) (reading-index fsm)))

;;; We return the same FSM object, since the caller expects it
;;; TODO: may define a type for atom-handling (3 symbols: skip/use/keep)
(defmethod update-fsm ((fsm string-fsm) atom-handling)
  (progn (case atom-handling
           (:skip (incf (reading-index fsm)))
           (:use (progn (setf (accumulator fsm)
                              (append (accumulator fsm) (list (retrieve-atom fsm))))
                        (incf (reading-index fsm))))
           (:keep nil)
           (t (error "Invalid character handling ~a!" atom-handling)))
         fsm))


;;; testing code
;;; TODO: relocate
;;; TODO: make it easier to tokenize multiple tokens (keep track of char index in input)
;;; NEXT: NEED TO FIGURE OUT HOW TO HANDLE FSM AND OUTPUT!!!

(defun test-tokenizer ()
  (let* ((acceptance-state (make-instance 'terminal-state
                                          :token :TOKEN-ENDING-WITH-DOLLAR-SIGN))
         (trans-to-acceptance (make-instance 'transition-to-other
                                             :next-state acceptance-state
                                             :atom-handling :use))
         (self-trans (make-instance 'transition-to-self :atom-handling :use))
         (test-initial-state (make-instance 'normal-state
                                            :token-on-no-input :INPUT-EMPTY-TOKEN
                                            :transition-finder-func (lambda (c)
                                                                      (if (char= c #\$)
                                                                          trans-to-acceptance
                                                                          self-trans))))
         (sample-tokenizer (create-tokenizer test-initial-state "one$two$three$")))
    (loop for out in (list (funcall sample-tokenizer)
                           (funcall sample-tokenizer)
                           (funcall sample-tokenizer)
                           (funcall sample-tokenizer)
                           (funcall sample-tokenizer))
         do (format t
                    "Token: ~a, text: ~a~%"
                    (slot-value out 'token)
                    (accumulator (fsm out))))))
