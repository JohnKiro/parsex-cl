(in-package :parsex-cl/regex)

;;;; -----------------------
;;;; Code for regex matching
;;;; -----------------------

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: flatten (simplify specific cases such as seq within seq)
;;;; TODO: more unit test cases, reorganize packages

;;; Note: currently returning only destination DFA state, may find later that I need
;;; the match criterion as well (i.e. not extracting the CDR part).
(defun find-matching-transition (origin-dfa-state char)
  "Find matching transition from ORIGIN-DFA-STATE, based on input CHAR.
Returns destination DFA state."
  (or (cdr (assoc char (dfa:transitions origin-dfa-state)
                  :test #'elm:match-char-against-simple-element))
      (dfa:transition-on-any-other origin-dfa-state)))

(defstruct regex-matching-result
  (status nil :type (or (eql :regex-matched) (eql :regex-not-matched) nil))
  (token :tokens-not-implemented-yet))

(defun match-regex (input-source root-dfa-state &aux (last-candidate-terminal-dfa nil))
  (labels ((prepare-result (status)
             ;;putting this here since we need to call it when scanning is terminated
             (input:notify-match-termination input-source)
             (make-regex-matching-result :status status))
           (transit (origin-dfa-state)
             (when (dfa:candidate-terminal origin-dfa-state)
               (setf last-candidate-terminal-dfa origin-dfa-state)
               (input:register-candidate-matching-point input-source))
             (if (dfa:dfa-state-definitely-terminal-p origin-dfa-state)
                 ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                 ;; match is exact or not). Leaving this up to the caller.
                 (prepare-result :regex-matched)
                 (if (input:source-empty-p input-source)
                     (prepare-result (if last-candidate-terminal-dfa
                                         :regex-matched
                                         :regex-not-matched))
                     (let* ((next-ch (input:read-next-item input-source))
                            (dest-dfa-state (find-matching-transition origin-dfa-state next-ch)))
                       (if dest-dfa-state
                           (progn
                             (input:advance-reading-position input-source)
                             (transit dest-dfa-state))
                           (prepare-result (if last-candidate-terminal-dfa
                                               :regex-matched
                                               :regex-not-matched))))))))
    (transit root-dfa-state)))


