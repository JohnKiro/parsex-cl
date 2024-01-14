(in-package :parsex-cl.basic-string-tokenizer)

#|
Implementation of a tokenizer for string-based source.
input: string
fsm: class (resizable vector of characters for atoms, reference to input string)
Initially, I'll use a list of chars for simplicity
output: contents of fsm + acceptance/error token
|#

(defclass fsm ()
  ((input-text :initarg :input-text :reader input-text)
   (reading-index :initform 0 :accessor reading-index)
   (accumulator :initform '() :accessor accumulator)))

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


(defun input-empty (fsm)
  (declare (type fsm fsm))
  (>= (reading-index fsm)
      (length (input-text fsm))))

(defun get-next-char (fsm)
  (declare (type fsm fsm))
  (aref (input-text fsm) (reading-index fsm)))

(defun update-fsm (fsm transition)
  (declare (type fsm fsm)
           (type transition transition))
  (progn (case (atom-handling transition)
           (:skip (incf (reading-index fsm)))
           (:use (progn (setf (accumulator fsm)
                              (append (accumulator fsm) (list (get-next-char fsm))))
                        (incf (reading-index fsm))))
           (:keep nil)
           (t (error "Invalid character handling ~a!" (atom-handling transition))))
         fsm))

(defgeneric get-next-state (transition current-state))

(defmethod get-next-state ((transition transition-to-other) current-state)
  (slot-value transition 'next-state))

(defmethod get-next-state ((transition transition-to-self) current-state)
  current-state)

;;;TODO: move outside of this generic (abstraction)
(defclass tokenizer-output ()
  ((atom-accumulator :initarg :atom-accumulator
                     :initform (error "Atom accumulator is mandatory")
                     :reader atom-accumulator)
   (token :initarg :token
          :initform (error "Token (acceptance or error) is mandatory")
          :reader token)))

(defun prepare-output (token fsm)
  (make-instance 'tokenizer-output :token token :atom-accumulator (accumulator fsm)))


(defparameter tokenizer-config
  (make-instance 'tokenizer-fsm-config
                 :fsm-prep-func (lambda (input)
                                  (make-instance 'fsm :input-text input))
                 :input-empty-predicate-func #'input-empty
                 :atom-retrieving-function #'get-next-char
                 :fsm-updating-func #'update-fsm
                 :next-state-retrieval-func #'get-next-state
                 :output-prep-func #'prepare-output))

;;; testing code
;;; TODO: relocate
;;; TODO: make it easier to tokenize multiple tokens (keep track of char index in input
(defun test-tokenizer ()
  (let* ((acceptance-state (make-instance 'terminal-state
                                              :token :TOKEN-ENDING-WITH-DOLLAR-SIGN))
        (trans-to-acceptance (make-instance 'transition-to-other
                                            :next-state acceptance-state
                                            :atom-handling :use))
        (self-trans (make-instance 'transition-to-self :atom-handling :use))
        (test-initial-state (make-instance 'normal-state
                                           :token-on-no-input :INPUT-EMPTY
                                           :transition-finder-func (lambda (c)
                                                                     (if (char= c #\$)
                                                                         trans-to-acceptance
                                                                         self-trans))))
        (output (tokenize tokenizer-config "one$two$three$" test-initial-state)))
    (format t
            "Tokenization done. Token: ~a, token text: ~a"
            (token output)
            (atom-accumulator output))))



