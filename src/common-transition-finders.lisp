(in-package :parsex-cl.common-transition-finders)

;;; Finds matching transition, based on specified atom.
;;; Arguments:
;;; - Atom to match
;;; - Sequence of matching functions (atom -> transition)
;;; Returns found transition, or nil if none found.
;;; Note: matcher function returns either a transition (truth), or nil.
(defun find-transition-from-set (atom transition-matchers)
  "Returns first matching transition, based on the specified atom."
  (find-if (lambda (matcher) (funcall matcher atom))
           transition-matchers))

(defun create-set-based-transition-finder (transition-matchers)
  (lambda (atom)
    (find-transition-from-set atom transition-matchers)))

;;; Below struct + function provide an alternative to FIND-TRANSITION (TODO: decide whether
;;; to keep either or both).
;;; An entry in a transition table, which contains the matcher predicate (atom -> t/nil)
;;; and the corresponding (matching) transition.
(defstruct transition-table-entry matcher-predicate transition)

;;; Similar to find-transition, but instead of a sequence of matching functions, it accepts a
;;; sequence of TRANSITION-TABLE-ENTRY structs.
;;; Returns found transition, or nil if none found.
;;; Note: I prefer the simpler version above, which is also more efficient.
(defun find-transition%2 (atom transition-table)
  (let ((trans-table-entry (find-if (lambda (matcher-predicate)
                                      (funcall matcher-predicate atom))
                                    transition-table
                                    :key #'transition-table-entry-matcher-predicate)))
    (if trans-table-entry (slot-value trans-table-entry 'transition) nil)))


