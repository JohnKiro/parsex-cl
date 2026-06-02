(in-package :parsex-cl/backtracking-tokenizer)

;;;;
;;;; tokenizer interface supporting backtracking, including default (probably sufficient) implementation
;;;;

;;;
;;; generic tokenizer interface (struct + funcall generation macros)
;;;

(func-v1:define-functional-interface backtracking-tokenizer ()
  (get-tokens
   ()
   :doc "Retrieve next token(s) from either source or backtracking buffer. The backtracking buffer is
used in case some tokens are pending in the backtracking buffer, otherwise, the source is used.
In the second case, the retrieved token(s) are also appended to the backtracking buffer, together with
the token accumulated slice indices, as a pair: (tokens . slice-indices).
Returns next token(s) and slice indices as a pair.
Note that calling it successively returns the same result, unless a call to another state-changing
function (e.g. `advance`) intervenes. In case it receives an error from the underlying tokenizer,
it returns NIL as a primary value, and a status as a secondary value, indicating the error.")
  (put-back-tokens
   ()
   :doc "Put back tokens retrieved by last call to `get-tokens`.")
  (get-current-backtracking-position
   ()
   :doc "Retrieve an object describing the current tokenizer position, allowing the caller to learn about
the progress (by comparing with a previous reading). The representation of the position is left up to the
implementation. The only requirement is to have compatibility with the `compare-positions` operation.")
  (compare-positions
   (position-older position-newer)
   :doc "Compares between the two backtracking positions `position-older` and `position-newer`, which
both are expected to be retrieveng using `get-current-backtracking-position`, typically to check whether
the tokenizer has progressed between the two points. Implementations should support the following
possible return values: :progress (older position precedes newer position), :no-progress (older position
is same as newer position).")
  (mark-backtracking-position
   (owner)
   :doc "Called by a construct before parsing, for backtracking in case of parsing failure. It must also
return the marked position, in a format that is compatible with `compare-positions`.")
  (unmark-backtracking-position
   (owner)
   :doc "Called by a construct after parsing termination, to cancel the marked backtracking. It must also
return the unmarked position, in a format that is compatible with `compare-positions`.")
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
When there is failure, it returns NIL, and a secondary value to describe the specific error (see cases
2 and 3 below).
Here are the identified special cases coming from the underlying tokenizer:
1) case regex matches, but no tokens reported: it acts normally: keeps the NIL tokens in the backtracking
buffer, together with the indices. This case will typically be prevented, as the grammar handling will
ensure no NIL tokens. TODO: may reconsider this case, and report it as error instead.
2) case regex does not match: returns NIL, and regex matching error status as secondary value.
3) case input exhausted: returns NIL, and input exhausted status as secondary value."
               (declare (optimize (debug 3) (speed 0)))
               (if (< backtracking-index (length backtracking-buffer))
                   ;; TODO: back to AREF after testing (doesn't check fill-pointer limit, but faster)
                   (prog1
                       (elt backtracking-buffer backtracking-index)
                     (incf backtracking-index))
                   (multiple-value-bind (tokenizer-result tokenizer-status)
                       (funcall underlying-tokenizer)
                     (if tokenizer-result
                         (let ((regex-status (match:regex-matching-result-status tokenizer-result)))
                           (if (eq regex-status :regex-matched)
                               (let* ((tok (match:regex-matching-result-tokens tokenizer-result))
                                      (tok-and-indices (cons tok
                                                             (input:retrieve-last-accumulated-indices
                                                              input-source))))
                                 (vector-push-extend tok-and-indices backtracking-buffer)
                                 (incf backtracking-index)
                                 ;; alternatively: (setf backtracking-index (length backtracking-buffer))
                                 tok-and-indices)
                               (values nil regex-status)))
                         ;; nil tokenizer-result actually implies input-exhausted
                         (values nil tokenizer-status)))))
             (put-back-tokens ()
               "Put back tokens retrieved by last call to `get-tokens`."
               (unless (plusp backtracking-index)
                 (error "No tokens retrieved!"))
               (decf backtracking-index))
             (get-current-backtracking-position ()
               "Retrieve an object describing the current tokenizer position. The main use of the
returned position is to pass it as argument to `compare-positions`. For now, I'm simply using the value
of the backtracking index (analysis and testing will show whether it's sufficient)."
               backtracking-index)
             (compare-positions (position-older position-newer)
               "Compare two positions along the progression of the tokenizer."
               (format t "Comparing older (previous) ~a with newer (current) ~a..~%"
                       position-older position-newer)
               (cond
                 ((< position-older position-newer) :progress)
                 ((= position-older position-newer) :no-progress)
                 (t :regression!)))
             (mark-backtracking-position (owner)
               "Called by a construct before parsing, for backtracking in case of parsing failure. It
also returns the marked position, in a format that is compatible with `compare-positions`, which is
simply the backtracking index."
               (push (cons backtracking-index owner) backtracking-markers)
               backtracking-index)
             (unmark-backtracking-position (owner)
               "Called by a construct after parsing termination, to cancel the marked backtracking. It
also returns the unmarked position, in a format that is compatible with `compare-positions`, which is
simply the backtracking index."
               ;; TODO: check NIL before calling FIRST needlessly
               (let ((upcoming-marker (first backtracking-markers)))
                 (unless upcoming-marker
                   (error "Backtracking log empty!"))
                 ;; this may be useful in testing (consider removing this later, for perf, noting that
                 ;; the owner is still available to the caller (may check it at the caller side)
                 (destructuring-bind (position . expected-owner) upcoming-marker
                   (unless (eq owner expected-owner)
                     (error "Unexpected mark owner (expected ~a, received ~a)!" expected-owner owner))
                   (pop backtracking-markers)
                   position)))
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
                                                      0 (max 0 (length backtracking-buffer)))))
                 `(:backtracking-buffer ,backtracking-buffer-top 
                   :backtracking-index ,backtracking-index
                   :backtracking-markers ,backtracking-markers))))
      (make-backtracking-tokenizer
       :get-tokens-fn #'get-tokens
       :put-back-tokens-fn #'put-back-tokens
       :get-current-backtracking-position-fn #'get-current-backtracking-position
       :compare-positions-fn #'compare-positions
       :mark-backtracking-position-fn #'mark-backtracking-position
       :unmark-backtracking-position-fn #'unmark-backtracking-position
       :rewind-token-position-fn #'rewind-token-position
       :dump-internal-state-fn #'dump-internal-state))))
