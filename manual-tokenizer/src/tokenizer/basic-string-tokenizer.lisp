(in-package :parsex-cl/manual-tokenizer/basic-string-tokenizer)

#|
Implementation of a tokenizer for string-based source.

Input is implemented as a string + reading index (TODO: specify slot type).
For the accumulator, initially, I'll use a list of chars for simplicity.
|#

(defclass string-input ()
  ((input-text :initarg :input-text :reader input-text)
   (reading-index :initarg :reading-index :initform 0 :accessor reading-index)))

(defclass basic-tokenization-result ()
  ((accumulator :initarg :accumulator :reader accumulator)
   (token :initarg :token :reader token)))

;;; prepare initial accumulator (empty list of characters)
(defun init-accumulator ()
  '())

(defmethod input-empty-p ((input string-input))
  (>= (reading-index input)
      (length (input-text input))))

(defmethod retrieve-atom ((input string-input))
  (aref (input-text input) (reading-index input)))

;;; We return the same input object, since the caller expects it
;;; note that case returns evaluation of last form for the matching clause.
;;; TODO: may define a type for atom-handling (3 symbols: skip/use/keep)
(defmethod update-input ((input string-input) atom-handling)
  (ecase atom-handling
    ((:skip-atom :use-atom) (incf (reading-index input)) input)
    (:keep-atom input)))

;;; Return an updated accumulator, based on atom handling.
(defmethod update-accumulator ((accumulator list) (atom character) atom-handling)
  (ecase atom-handling
    (:use-atom (push atom accumulator))
    ((:skip-atom :keep-atom) accumulator)))


(defmethod prepare-tokenization-result ((token symbol) (accumulator list))
  (make-instance 'basic-tokenization-result
                 :token token
                 :accumulator (reverse accumulator)))
