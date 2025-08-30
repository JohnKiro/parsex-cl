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
         (negation-exit-elem (make-instance 'elm:zero-or-more-element
                                            :element elm:+any-char-element+))
         (output-state (if (elm:greedy-p regex)
                           (regex-to-nfa negation-exit-elem glue-state)
                           glue-state)))
    (multiple-value-bind (inner-continuation-points inner-dead-ends)
        (state:analyze-nfa-state-reachability input-nfa-state output-state-inner)
      ;; traverse NFA sub-tree, and connect each dead-end state to the new (+ any-char) element,
      ;; then to output state
      ;; actually for now, we separated the traversal (above) from the connecting (below)
      (labels ((absolute-dead-end-p (state)
                 (not (or (state:normal-transitions state)
                          (state:transitions-on-any-char state)
                          ;; TODO: REVISE!!!!
                          (state:transition-on-any-other state))))
               (cleanup-dead-paths-on-any-other-char (orig-state)
                 "Cleanup transitions on any-other-char, in case destination is continuation point."
                 ;; TODO: may also cleanup auto-transitions to CT (going to be dead-end)
                 (when (member #1=(state:transition-on-any-other orig-state)
                               inner-continuation-points)
                   (state:unset-nfa-transition-on-any-other orig-state)))
               (cleanup-dead-paths-on-auto (orig-state)
                 "Cleanup auto transitions, where destination is the output-state-inner."
                 (state:delete-auto-transition orig-state output-state-inner)))
        (loop for inner-contin in inner-continuation-points
              do (state:set-dead-end inner-contin)
              do (progn
                   (cleanup-dead-paths-on-any-other-char inner-contin)
                   (cleanup-dead-paths-on-auto inner-contin)))
        (loop with inner-continuation-point-closures = (state:prepare-nfa-state-closure-union
                                                        inner-continuation-points)
              for inner-dead-end in inner-dead-ends
              do (if (absolute-dead-end-p inner-dead-end)
                     (progn
                       ;; TODO: give user the choice (greedy/non-greedy)
                       ;; TODO: rather than creating this transition and creating output-state ,
                       ;; check the dead-end that has no auto transitions out, and use it as output
                       ;; state
                       (state:add-nfa-auto-transition inner-dead-end output-state)
                       (state:unset-dead-end inner-dead-end))
                     (when (and (state:set-nfa-transition-on-any-other inner-dead-end glue-state)
                                (not (member inner-dead-end inner-continuation-point-closures
                                             :test #'eql)))
                       ;; TODO: give user the choice (greedy/non-greedy)
                       (state:add-nfa-auto-transition inner-dead-end output-state)
                       (state:unset-dead-end inner-dead-end)))
              do (cleanup-dead-paths-on-any-other-char inner-dead-end))))
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

