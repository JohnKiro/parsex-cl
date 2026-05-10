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
   :doc "Retrieve next token(s) from either source or backtracking buffer. The backtracking buffer is
used in case some tokens are pending in the backtracking buffer, otherwise, the source is used.
In the second case, the retrieved token(s) are also appended to the backtracking buffer, together with
the token accumulated slice indices, as a pair: (tokens . slice-indices).
Returns next token(s) and slice indices as a pair.
Note that calling it successively returns the same result, unless a call to another state-changing
function (e.g. `advance`) intervenes.")
  (mark-backtracking-position
   (owner)
   :doc "Called by a construct before parsing, for backtracking in case of parsing failure.")
  (unmark-backtracking-position
   (owner)
   :doc "Called by a construct after parsing termination, to cancel the marked backtracking.")
  (rewind-token-position
   (owner)
   :doc "Called by a construct to backtrack to a previously marked position (parsing failure).")
  (dump-internal-state
   ()
   :doc "Dump tokenizer internal state as a p-list (useful for debugging and testing)."))

;;;
;;; basic backtracking tokenizer implementation
;;;

(defun create-backtracking-tokenizer (underlying-tokenizer input-source)
  "Creates a backtracking tokenizer that conforms with the `backtracking-tokenizer` interface. It
controls the passed `underlying-tokenizer`, which is a closure that returns next token from input.
The `input-source` argument is used to register the accumulated token value corresponding
to each token. The implementation supports backtracking by keeping a buffer of all collected tokens, as
well as a stack of backtracking markers. The returned tokenizer object supports operations to retrieve
next token (without advancing), advance tokenizer, mark and unmark/rewind backtracking position, and
dumping internal state as a p-list (for testing/debugging)."
  #+nil(declare (optimize (debug 0) (speed 3)))
  (let ((backtracking-buffer (make-array 100 :adjustable t :fill-pointer 0))
        (backtracking-markers nil)
        (backtracking-index 0))
    (labels ((get-tokens ()
               "Retrieve next token(s) from either source or backtracking buffer. The backtracking
buffer is used in case some tokens are pending in the backtracking buffer, otherwise, the source is used.
In the second case, the retrieved token(s) are also appended to the backtracking buffer, together with
the token accumulated slice indices. Note that calling it successively returns the same result, unless
a call to another state-changing function (e.g. `advance`) intervenes.
TODO: it's not yet clear the situation in case of tokenization error, or empty input!"
               (prog1
                   (if (< backtracking-index (length backtracking-buffer))
                       ;; TODO: back to AREF after testing (doesn't check fill-pointer limit, but faster)
                       (elt backtracking-buffer backtracking-index)
                       (let* ((tok (funcall underlying-tokenizer)))
                         #+debug(format t "~%Underlying tokenizer returned ~a.~%" tok)
                         (when tok ;otherwise: input exhausted or tokenization error (we don't care)
                           (let ((tok-and-indices (cons tok (input:retrieve-last-accumulated-indices
                                                             input-source))))
                             (vector-push-extend tok-and-indices backtracking-buffer)
                             tok-and-indices))))
                 (advance)))
             (advance ()
               "Advance tokenizer so that next call to `get-tokens` would provide the token at next
 position. Calling it successively while within the backtracking buffer would advance through the
 backtracking buffer, until it reaches the end. Calling it beyond the backtracking buffer any number of
times, will have no effect, as `get-tokens` detects the beyond-backtracking condition, and it would
retrieve from the underlying tokenizer. Same happens when calling it with an empty backtracking buffer
(before any calls to `get-tokens`). Returns NIL in all cases."
               (incf backtracking-index)
               nil)
             (mark-backtracking-position (owner)
               "Called by a construct before parsing, for backtracking in case of parsing failure."
               (push (cons backtracking-index owner) backtracking-markers))
             (unmark-backtracking-position (owner)
               "Called by a construct after parsing termination, to cancel the marked backtracking."
               (let ((upcoming-marker (first backtracking-markers)))
                 (unless upcoming-marker
                   (error "Backtracking log empty!"))
                 ;; this may be useful in testing (consider removing this later, for perf, noting that
                 ;; the owner is still available to the caller (may check it at the caller side)
                 (destructuring-bind (position . expected-owner) upcoming-marker
                   (declare (ignorable position))
                   (unless (eq owner expected-owner)
                     (error "Unexpected mark owner (expected ~a, received ~a)!" expected-owner owner)))
               (pop backtracking-markers)))
             (rewind-token-position (owner)
               "Called by a construct to backtrack to a previously marked position (parsing failure)."
               (let ((upcoming-marker (first backtracking-markers)))
                 (unless upcoming-marker
                   (error "Backtracking log empty!"))
                 (destructuring-bind (position . expected-owner) upcoming-marker
                   (unless (eq owner expected-owner)
                     (error "Unexpected mark owner (expected ~a, received ~a)!" expected-owner owner))
                   (setf backtracking-index position))))
             (dump-internal-state ()
               "Dump tokenizer internal state as a p-list."
               (let ((backtracking-buffer-top (subseq backtracking-buffer
                                                      (max 0 (- (length backtracking-buffer) 5)))))
                 `(:backtracking-buffer ,backtracking-buffer-top 
                   :backtracking-index ,backtracking-index
                   :backtracking-markers ,backtracking-markers))))
      (make-backtracking-tokenizer :get-tokens-fn #'get-tokens
                                   :mark-backtracking-position-fn #'mark-backtracking-position
                                   :unmark-backtracking-position-fn #'unmark-backtracking-position
                                   :rewind-token-position-fn #'rewind-token-position
                                   :dump-internal-state-fn #'dump-internal-state))))

