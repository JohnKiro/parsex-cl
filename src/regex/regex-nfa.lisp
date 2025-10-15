(in-package :parsex-cl/regex/nfa)

(defgeneric regex-to-nfa (regex input-nfa-state)
  (:documentation "Traverses REGEX object tree and generates corresponding NFA section, starting at
INPUT-NFA-STATE. In the process, it creates as many states for the element, glues with input state,
and returns output state as continuation point."))

(defmethod regex-to-nfa ((regex elm:single-char-element) input-nfa-state)
  (let ((output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex (eql elm:+ANY-CHAR-ELEMENT+)) input-nfa-state)
  (let* ((output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-transition-on-any-char input-nfa-state output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:char-range-element) input-nfa-state)
  (let* ((output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-normal-transition input-nfa-state regex output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:sequence-element) input-nfa-state)
  (reduce #'(lambda (previous-output-nfa-state elem)
              (regex-to-nfa elem previous-output-nfa-state))
          (elm:inner-elements regex)
          :initial-value input-nfa-state))

(defmethod regex-to-nfa ((regex elm:or-element) input-nfa-state)
  (let ((output-state (make-instance 'state:nfa-state)))
    (map nil
         #'(lambda (elem)
             (let* ((input-state-i (make-instance 'state:nfa-state))
                    (out-state-i (regex-to-nfa elem input-state-i)))
               (state:add-nfa-auto-transition input-nfa-state input-state-i)
               (state:add-nfa-auto-transition out-state-i output-state)))
         (elm:inner-elements regex))
    output-state))

(defmethod regex-to-nfa ((regex elm:zero-or-one-element) input-nfa-state)
  (let* ((s1 (make-instance 'state:nfa-state))
         (inner-regex (elm:inner-element regex))
         (s2 (regex-to-nfa inner-regex s1))
         (output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-auto-transition input-nfa-state s1)
    (state:add-nfa-auto-transition input-nfa-state output-state)
    (state:add-nfa-auto-transition s2 output-state)
    output-state))

;; Alternative simplified (experimental) version
(defmethod regex-to-nfa% ((regex elm:zero-or-more-element) input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (output-state (regex-to-nfa inner-regex input-nfa-state)))
    (state:add-nfa-auto-transition input-nfa-state output-state)
    (state:add-nfa-auto-transition output-state input-nfa-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:zero-or-more-element) input-nfa-state)
  (let* ((s1 (make-instance 'state:nfa-state))
         (inner-regex (elm:inner-element regex))
         (s2 (regex-to-nfa inner-regex s1))
         (output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-auto-transition input-nfa-state s1)
    (state:add-nfa-auto-transition input-nfa-state output-state)
    (state:add-nfa-auto-transition s2 s1)
    (state:add-nfa-auto-transition s2 output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:one-or-more-element) input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (s1 (regex-to-nfa inner-regex input-nfa-state))
         (s2 (regex-to-nfa inner-regex s1))
         (output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-auto-transition s2 s1)
    (state:add-nfa-auto-transition s1 s2)
    (state:add-nfa-auto-transition s2 output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:negated-element) input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (output-state-inner (regex-to-nfa inner-regex input-nfa-state))
         (glue-state (make-instance 'state:nfa-state))
         (output-state (if (elm:greedy-p regex)
                           (regex-to-nfa (make-instance 'elm:zero-or-more-element
                                                        :element elm:+any-char-element+)
                                         glue-state)
                           glue-state))
         (state-reachability (state:analyze-nfa-state-reachability input-nfa-state
                                                                   output-state-inner)))
    (labels ((cleanup-dead-paths-on-auto (orig-state)
               "Cleanup auto transitions, where destination is the output-state-inner."
               (state:delete-auto-transition orig-state output-state-inner)))
      ;; traverse NFA sub-tree, and states that have output-state-inner in their closures will
      ;; be converted to dead-ends, and states that can reach output-state-inner via elements
      ;; will get a transition on any-char to output-state (later: optionally via a
      ;; (+ any-char))
      (let ((inner-cont-pts-closures (state:prepare-nfa-state-closure-union
                                      (loop for state-i being the hash-keys in state-reachability
                                              using (hash-value s-status)
                                            when (eq s-status :auto-connected) collect state-i))))
        (loop for state-i being the hash-keys in state-reachability using (hash-value s-status)
              do (ecase s-status
                   (:auto-connected
                    (state:set-dead-end state-i)
                    ;; just simplification, not needed (generated DFA not affected, test cases pass)
                    (cleanup-dead-paths-on-auto state-i))
                   (:element-connected
                    (state:set-nfa-transition-on-any-other state-i glue-state)
                    (unless (member state-i inner-cont-pts-closures :test #'eql)
                      ;; TODO: give user the choice (greedy/non-greedy)
                      (state:add-nfa-auto-transition state-i output-state)
                      (state:unset-dead-end state-i)))
                   (:not-connected ;; previously condition "absolute dead-end"
                    (state:add-nfa-auto-transition state-i output-state)
                    (state:unset-dead-end state-i)))))
      (let ((traversal-lookup (make-hash-table)))
        (labels ((add-inversion-transitions (state)
                   "Add transitions corresponding to inversions resulting from the negation."
                   #+nil(format t "DEBUG: Calling ADD-INVERSION-TRANSITIONS for state ~a..~&" state)
                   (unless #1=(gethash state traversal-lookup)
                           ;;unfortunately #1= in UNLESS confuses indentation
                           (setf #1# t)
                           (let ((closure (state:prepare-nfa-state-closure-union (list state))))
                             (if (state::states-have-trans-on-any-other-p closure)
                                 (progn
                                   ;; clear "any-other" transitions from the whole closure, since
                                   ;;we'll convert them to normal transitions
                                   ;; TODO: try to avoid having to do this (merge with 1st pass?)
                                   (dolist (s closure)
                                     (state:unset-nfa-transition-on-any-other s))
                                   ;; convert "any-other" transitions into normal transitions (using
                                   ;; inversion)
                                   ;; note that we add the created transitions to the closure's
                                   ;; initial point (actually it doesn't matter which state in the
                                   ;; closure gets the transitions)
                                   (let ((splitting-pts (state::collect-char-range-splitting-points
                                                         closure))
                                         (split-elements nil))
                                     (format t "DEBUG: Splitting points: ~s.~&" splitting-pts)
                                     (state::do-normal-transitions (_ element next-state) closure
                                       ;; check to avoid inverting an inverted element
                                       (unless  (eq next-state glue-state)
                                         (etypecase element
                                           (elm:char-range-element
                                            (let ((split-ranges (elm:split-char-range
                                                                 element splitting-pts)))
                                              (dolist (r split-ranges)
                                                (push r split-elements))))
                                           (elm:single-char-element
                                            (push element split-elements)))))
                                     (loop for inv-elem in (elm::invert-elements
                                                            (elm::sort-simple-elements
                                                             split-elements))
                                           do (state:add-nfa-normal-transition state inv-elem
                                                                               glue-state))))
                                 (progn
                                   ;; else: no any-other trans, => traverse closure
                                   (dolist (s closure)
                                     (add-inversion-transitions s))))
                             ;; traverse normal transitions (TODO: and ANY-CHAR transitions as
                             ;; well??)
                             (state::do-normal-transitions (trans elm next-state) closure
                               (add-inversion-transitions next-state))))))
          (add-inversion-transitions input-nfa-state)))
      ;; connect the NOT element to the rest of the NFA
      output-state)))


(defmethod regex-to-nfa% ((regex elm:inv-element) input-nfa-state)
  (loop for elm of-type (or elm:single-char-element elm:char-range-element)
          across (elm:inner-elements regex)
        ;; TODO: alternatively, since elm is restricted to be char/char-range, I could just add
        ;; normal transition from input state on elm, to a dead-end state (to be created)
        do (regex-to-nfa elm input-nfa-state))
  (let ((output-state (make-instance 'state:nfa-state)))
    (state:set-nfa-transition-on-any-other input-nfa-state output-state)
    output-state))

(defmethod regex-to-nfa ((regex elm:inv-element) input-nfa-state)
  (loop for elm of-type (or elm:single-char-element elm:char-range-element)
          across (elm:inner-elements regex)
        ;; TODO: alternatively, since elm is restricted to be char/char-range, I could just add
        ;; normal transition from input state on elm, to a dead-end state (to be created)
        do (regex-to-nfa elm input-nfa-state))
  (let ((output-state (make-instance 'state:nfa-state)))
    (state:set-nfa-transition-on-any-other input-nfa-state output-state)
    output-state))

(defun produce-nfa (regex)
  "Produces NFA starting at root regex element. Its importance is in identifying the terminus
state."
  (let* ((root-state (make-instance 'state:nfa-state))
         (terminus-nfa-state (regex-to-nfa regex root-state)))
    (state:set-terminus terminus-nfa-state)
    (values root-state terminus-nfa-state)))

