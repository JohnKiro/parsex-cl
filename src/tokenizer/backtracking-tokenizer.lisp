(in-package :parsex-cl/backtracking-tokenizer)

;;;;
;;;; tokenizer interface supporting backtracking, including default (probably sufficient) implementation
;;;;

;;;
;;; generic tokenizer interface (struct + funcall generation macros)
;;;
(func:define-functional-interface backtracking-tokenizer ()
  (get-tokens
   ()
   :doc "Retrieve next token(s) from either source or backtracking buffer.")
  (notify-token-match-success
   ()
   :doc "Notify tokenizer that a match is identified. it typically advances the backtracking index.")
  (mark-backtracking-position
   (owner)
   :doc "Called by a construct before parsing, for backtracking in case of parsing failure.")
  (unmark-backtracking-position
   (owner)
   :doc "Called by a construct after successful parsing, to cancel the marked backtracking.")
  (rewind-token-position
   (owner)
   :doc "Called by a construct to backtrack to a previously marked position (parsing failure).")
  (dump-internal-state
   ()
   :doc "Dump tokenizer internal state as a p-list (useful for debugging and testing)."))

;;;
;;; basic backtracking tokenizer implementation
;;;

(defun create-backtracking-tokenizer (underlying-tokenizer)
  "Creates a backtracking tokenizer that conforms with the `backtracking-tokenizer` interface. It
controls the passed `underlying-tokenizer`, which is a `tokenizer` struct instance. It uses the passed
`find-matching-token-fn` function to check whether expected token matches one of the actual token.
The implementation supports backtracking by keeping a buffer of all collected tokens, as well as a stack 
of backtracking markers. The returned tokenizer object supports operations to match next token against
expected token(s), passed as argument, mark and unmark/rewind backtracking position, and dumping internal
state as a p-list (for testing/debugging)."
  #+nil(declare (optimize (debug 0) (speed 3)))
  (let ((backtracking-buffer (make-array 100 :adjustable t :fill-pointer 0))
        (backtracking-markers nil)
        (backtracking-index 0)
        (backtrack nil))
    (labels ((get-tokens ()
               "Retrieve next token(s) from either source or backtracking buffer."
               (if (and (< backtracking-index (length backtracking-buffer)))
                   (prog1
                       ;; TODO: back to AREF after testing (doesn't check fill-pointer limit, but faster)
                       (elt backtracking-buffer backtracking-index)
                     #+nil(incf backtracking-index))
                   ;;todo: why am i not checking for possible tokenization error??
                   (let ((tok (funcall underlying-tokenizer)))
                     #+nil(setf backtrack nil)
                     (vector-push-extend tok backtracking-buffer)
                     tok)))
             (notify-token-match-success ()
               "Notify tokenizer that a match is identified. It advances the backtracking index."
               #+nil(declare (optimize (debug 0) (speed 3)))
               (incf backtracking-index))
             (mark-backtracking-position (owner)
               "Called by a construct before parsing, for backtracking in case of parsing failure."
               (push (cons (if t #+nil backtrack
                               (min backtracking-index (length backtracking-buffer))
                               #+nil(length backtracking-buffer))
                           owner)
                     backtracking-markers))
             (unmark-backtracking-position (owner)
               "Called by a construct after successful parsing, to cancel the marked backtracking."
               ;; this may be useful in testing (consider removing this later, for perf, noting that
               ;; the owner is still available to the caller (may check it at the caller side)
               (destructuring-bind (position . expected-owner) (first backtracking-markers)
                 (declare (ignorable position))
                 (unless (eq owner expected-owner)
                   (error "Unexpected mark owner!")))
               (pop backtracking-markers))
             (rewind-token-position (owner)
               "Called by a construct to backtrack to a previously marked position (parsing failure)."
               (destructuring-bind (position . expected-owner) (first backtracking-markers)
                 (unless (eq owner expected-owner)
                   (error "Unexpected mark owner!"))
                 (setf backtracking-index position)
                 #+nil(setf backtrack t)))
             (dump-internal-state ()
               "Dump tokenizer internal state as a p-list."
               (let ((backtracking-buffer-top (subseq backtracking-buffer
                                                      (max 0 (- (length backtracking-buffer) 5)))))
                 `(:backtracking-buffer ,backtracking-buffer-top 
                   :backtracking-index ,backtracking-index
                   :backtracking-markers ,backtracking-markers
                   :backtrack ,backtrack))))
      (make-backtracking-tokenizer :get-tokens-fn #'get-tokens
                                   :notify-token-match-success-fn #'notify-token-match-success
                                   :mark-backtracking-position-fn #'mark-backtracking-position
                                   :unmark-backtracking-position-fn #'unmark-backtracking-position
                                   :rewind-token-position-fn #'rewind-token-position
                                   :dump-internal-state-fn #'dump-internal-state))))
