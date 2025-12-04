(in-package :parsex-cl/regex)

;;;; -----------------------
;;;; Code for regex matching
;;;; -----------------------

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: flatten (simplify specific cases such as seq within seq)
;;;; TODO: more unit test cases, reorganize packages


(defstruct regex-matching-result
  (status nil :type (member :regex-matched :regex-not-matched nil))
  (tokens nil))

(defun match-regex (input-source root-dfa-state &aux (last-candidate-terminal-dfa nil))
  (input:with-regex-input-handler-funcall-macros (input:source-empty-p
                                                  input:read-next-item
                                                  input:advance-reading-position
                                                  input:register-candidate-matching-point
                                                  input:notify-match-termination) input-source
    (labels ((prepare-result (dfa-state)
               "Prepare result based on `dfa-state`. Note that NIL means no match."
               ;;putting this here since we need to call it when scanning is terminated
               ;;TODO: either rename label (to be more meaningful, or move this elsewhere)
               (input:notify-match-termination)
               (if dfa-state
                   (make-regex-matching-result :status :regex-matched
                                               :tokens (dfa:tokens dfa-state))
                   (make-regex-matching-result :status :regex-not-matched)))
             (transit (origin-dfa-state)
               (when (dfa:candidate-matching-point-p origin-dfa-state)
                 (setf last-candidate-terminal-dfa origin-dfa-state)
                 (input:register-candidate-matching-point))
               (if (dfa:dfa-state-definitely-terminal-p origin-dfa-state)
                   ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                   ;; match is exact or not). Leaving this up to the caller.
                   (prepare-result origin-dfa-state)
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
