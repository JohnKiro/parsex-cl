(in-package :parsex-cl/regex/match)

;;;; -----------------------
;;;; Code for regex matching
;;;; -----------------------

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: flatten (simplify specific cases such as seq within seq)
;;;; TODO: more unit test cases, reorganize packages
;;;; TODO: may consider removing this struct, and just returning the DFA itself, which contains the
;;;; tokens, and if the returned value is NIL, then we understand that there is no match (instead
;;;; of :regex-not-matched) - the gain is: getting rid of this struct (hence, less concepts, less
;;;; complexity, and also retrieving the token only when needed (in the client code instead of
;;;; in `match-regex` itself).

(defstruct regex-matching-result
  (status nil :type (member :regex-matched :regex-not-matched nil))
  (tokens nil))

(defun regex-matched-p (regex-matching-result)
  "Returns true if `regex-matching-result` indicates matching success."
  (eq (regex-matching-result-status regex-matching-result) :regex-matched))

(defun match-regex (input-source root-dfa-state &aux (last-candidate-terminal-dfa nil))
  "Matches input text from input source `input-source` against regex specified by its root DFA state
`root-dfa-state`, and returns a result structure of type regex-matching-result`, including matching
status and matched token(s)."
  (labels ((prepare-result (dfa-state)
             "Prepare result based on `dfa-state`. Note that if dfa-state is NIL, then no match."
             ;;putting this here since we need to call it when scanning is terminated
             ;;TODO: either rename label (to be more meaningful, or move this elsewhere)
             (input:notify-match-termination input-source)
             (if dfa-state
                 (make-regex-matching-result :status :regex-matched
                                             :tokens (dfa:tokens dfa-state))
                 (make-regex-matching-result :status :regex-not-matched)))
           (transit (origin-dfa-state)
             (when (dfa:candidate-matching-point-p origin-dfa-state)
               (setf last-candidate-terminal-dfa origin-dfa-state)
               (input:register-candidate-matching-point input-source))
             (if (dfa:dfa-state-definitely-terminal-p origin-dfa-state)
                 ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                 ;; match is exact or not). Leaving this up to the caller.
                 (prepare-result origin-dfa-state)
                 (if (input:source-empty-p input-source)
                     (prepare-result last-candidate-terminal-dfa)
                     (let* ((next-ch (input:read-next-item input-source))
                            (dest-dfa-state (dfa:find-matching-transition origin-dfa-state
                                                                          next-ch)))
                       (if dest-dfa-state
                           (progn
                             (input:advance-reading-position input-source)
                             (transit dest-dfa-state))
                           (prepare-result last-candidate-terminal-dfa)))))))
    (transit root-dfa-state)))
