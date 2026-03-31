(in-package :parsex-cl/backtracking-tokenizer)

;;;;
;;;; tokenizer interface supporting backtracking, including default (probably sufficient) implementation
;;;;

;; default matching (token equality check)
(defun find-matching-token (expected-token actual-tokens)
  "Matches `expected-token` against one of the `actual-tokens` sequence. returns found token or NIL.
Default equality test is used (EQL). See also `*token-matching-fn*`."
  (find expected-token actual-tokens))

(defparameter *token-matching-fn* #'find-matching-token
  "Configurable token matching function, that matches expected token (1st arg) against one of the actual
tokens receives from the regex machine (2nd arg). Implementations should return the matched token or NIL
if no match. By default, it points to `find-matching-token`, which is an implementation that relies on
default equality test (EQL), and assumes the actual tokens as a sequence.
For more sophisticated needs, the user could set the dynamic variable to a custom implementation. For
example, in case it needs to inspect the actual token text (say, to map token via lookup table), a custom
implementation could have access to the input source (e.g. within a closure env).
Note: may in the future return additional values (e.g. allowing NIL itself as a valid token ID.")

;;;
;;; generic tokenizer interface (struct + funcall generation macros)
;;;

(func:define-functional-interface backtracking-tokenizer ()
  (match-token
   (expected-token)
   :doc "Match `expected-token` against next token(s) from tokenizer. In case of success and currently
retrieving from backtracking buffer, it advances the backtracking index.")
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

(defun create-backtracking-tokenizer (underlying-tokenizer input-source)
  "Creates a backtracking tokenizer that conforms with the `backtracking-tokenizer` interface. It
controls the passed `underlying-tokenizer`, which is a `tokenizer` struct instance. It uses the function
specified by the dynamic var *token-matching-fn* to check whether expected token matches one of the
actual token. The `input-source` argument is used to register the accumulated token value corresponding
to each token. The implementation supports backtracking by keeping a buffer of all collected tokens, as
well as a stack of backtracking markers. The returned tokenizer object supports operations to match next
token against expected token(s), passed as argument, mark and unmark/rewind backtracking position, and
dumping internal state as a p-list (for testing/debugging)."
  #+nil(declare (optimize (debug 0) (speed 3)))
  (let ((backtracking-buffer (make-array 100 :adjustable t :fill-pointer 0))
        (backtracking-markers nil)
        (backtracking-index 0)
        (backtrack nil))
    (labels ((get-tokens ()
               "Retrieve next token(s) from either source or backtracking buffer. The backtracking
buffer is used in case some tokens are pending in the backtracking buffer, otherwise, the source is used.
In the second case, the retrieved token(s) is also appended to the backtracking buffer, together with the
token accumulated slice indices.
TODO: it's not yet clear the situation in case of tokenization error, or empty input!"
               (if (and #+nil backtrack (< backtracking-index (length backtracking-buffer)))
                   (prog1
                       ;; TODO: back to AREF after testing (doesn't check fill-pointer limit, but faster)
                       (elt backtracking-buffer backtracking-index)
                     #+nil(incf backtracking-index))
                   ;;todo: why am i not checking for possible tokenization error??
                   (let* ((tok (funcall underlying-tokenizer))
                          (tok-and-indices (cons tok (input:retrieve-last-accumulated-indices
                                                      input-source))))
                     #+nil(setf backtrack nil)
                     (unless tok
                       (error "Tokenization error?? TODO: What to do??"))
                     (vector-push-extend tok-and-indices backtracking-buffer)
                     tok-and-indices)))
             (match-token (expected-token)
               "Match `expected-token` against next token(s) from tokenizer. In case of success and
currently retrieving from backtracking buffer, it advances the backtracking index. Returns matching
details as three values: matched token ID, status code (keyword), token value slice indices."
               (declare (optimize (debug 3) (speed 0)))
               #+nil(break)
               (alexandria:if-let ((tokenizer-result (get-tokens)))
                 (destructuring-bind (actual-tokens . acc-indices) tokenizer-result
                   (let ((match-result (funcall *token-matching-fn* expected-token actual-tokens)))
                     (if match-result
                         (progn
                           (incf backtracking-index)
                           ;;TODO: SHOULD ACTUALLY RETURN RESULT, NOT JUST :OK (IN PROGRESS)!
                           (values match-result :ok acc-indices))
                         (values nil :no-match nil))))
                 (values nil :invalid-token-or-empty-input nil)))
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
      (make-backtracking-tokenizer :match-token-fn #'match-token
                                   :mark-backtracking-position-fn #'mark-backtracking-position
                                   :unmark-backtracking-position-fn #'unmark-backtracking-position
                                   :rewind-token-position-fn #'rewind-token-position
                                   :dump-internal-state-fn #'dump-internal-state))))
