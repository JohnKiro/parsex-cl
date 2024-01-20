(in-package :parsex-cl.basic-string-tokenizer)

#|
Implementation of a tokenizer for string-based source.

Input is implemented as a string + reading index (TODO: specify slot type).
For the accumulator, initially, I'll use a list of chars for simplicity.
|#

(defclass string-input ()
  ((input-text :initarg :input-text :reader input-text)
   (reading-index :initarg :reading-index :initform 0 :accessor reading-index)))

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
    ((:skip :use) (incf (reading-index input)) input)
    (:keep input)))

;;; Return an updated accumulator, based on atom handling.
(defmethod update-accumulator ((accumulator list) (atom character) atom-handling)
  (ecase atom-handling
    (:use (append accumulator (list atom)))
    ((:skip :keep) accumulator)))


;;; testing code
;;; TODO: relocate
;;; To use it, simply call it, and inspect output.
;;; TODO: automatic output verification (asserts)
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
         (input (make-instance 'string-input :input-text "one$two$three$"))
         (sample-tokenizer (create-tokenizer test-initial-state input #'init-accumulator)))
    (loop for out in (list (funcall sample-tokenizer)
                           (funcall sample-tokenizer)
                           (funcall sample-tokenizer)
                           (funcall sample-tokenizer)
                           (funcall sample-tokenizer))
         do (format t
                    "Token: ~a, text: ~a~%"
                    (token out)
                    (accumulator out)))))
