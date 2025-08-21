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
  (let ((source-empty-p-fn (input::source-empty-p-fn input-source))
        (read-next-item-fn (input::read-next-item-fn input-source))
        (advance-reading-position-fn (input::advance-reading-position-fn input-source))
        (register-candidate-matching-point-fn (input::register-candidate-matching-point-fn
                                               input-source))
        (notify-match-termination-fn (input::notify-match-termination-fn input-source)))
    (labels ((prepare-result (status)
               "Prepare result based on matching status (STATUS); T: match, NIL: no match."
               ;;putting this here since we need to call it when scanning is terminated
               (funcall notify-match-termination-fn)
               (make-regex-matching-result :status (if status :regex-matched :regex-not-matched)))
             (transit (origin-dfa-state)
               (when (dfa:candidate-terminal origin-dfa-state)
                 (setf last-candidate-terminal-dfa origin-dfa-state)
                 (funcall register-candidate-matching-point-fn))
               (if (dfa:dfa-state-definitely-terminal-p origin-dfa-state)
                   ;; I'm not checking whether input is empty or not here (e.g. to determine if the
                   ;; match is exact or not). Leaving this up to the caller.
                   (prepare-result t)
                   (if (funcall source-empty-p-fn)
                       (prepare-result last-candidate-terminal-dfa)
                       (let* ((next-ch (funcall read-next-item-fn))
                              (dest-dfa-state (dfa:find-matching-transition origin-dfa-state
                                                                            next-ch)))
                         (if dest-dfa-state
                             (progn
                               (funcall advance-reading-position-fn)
                               (transit dest-dfa-state))
                             (prepare-result last-candidate-terminal-dfa)))))))
      (transit root-dfa-state))))


