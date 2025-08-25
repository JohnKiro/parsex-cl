(in-package :parsex-cl/regex)

;;;; -----------------------
;;;; Code for regex matching
;;;; -----------------------

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: flatten (simplify specific cases such as seq within seq)
;;;; TODO: more unit test cases, reorganize packages


(defstruct regex-matching-result
  (status nil :type (or (eql :regex-matched) (eql :regex-not-matched) nil))
  (token :tokens-not-implemented-yet))

(defun match-regex (input-source root-dfa-state &aux (last-candidate-terminal-dfa nil))
  (input:with-regex-input-handler-funcall-macros (input:source-empty-p
                                                  input:read-next-item
                                                  input:advance-reading-position
                                                  input:register-candidate-matching-point
                                                  input:notify-match-termination) input-source
    (labels ((prepare-result (status)
               "Prepare result based on matching status (STATUS); T: match, NIL: no match."
               ;;putting this here since we need to call it when scanning is terminated
               (input:notify-match-termination)
               (make-regex-matching-result :status (if status :regex-matched :regex-not-matched)))
             (transit (origin-dfa-state)
               (when (dfa:candidate-terminal origin-dfa-state)
                 (setf last-candidate-terminal-dfa origin-dfa-state)
                 (input:register-candidate-matching-point))
               (if (dfa:dfa-state-definitely-terminal-p origin-dfa-state)
                   ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                   ;; match is exact or not). Leaving this up to the caller.
                   (prepare-result t)
                   (if (input:source-empty-p)
                       (prepare-result last-candidate-terminal-dfa)
                       (let* ((next-ch (input:read-next-item))
                              (dest-dfa-state (dfa:find-matching-transition origin-dfa-state
                                                                            next-ch)))
                         (if dest-dfa-state
                             (progn
                               (input:advance-reading-position)
                               (transit dest-dfa-state))
                             (prepare-result last-candidate-terminal-dfa)))))))
      (transit root-dfa-state))))


