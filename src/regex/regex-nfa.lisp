(in-package :parsex-cl/regex/nfa)

(defgeneric regex-to-nfa (regex input-nfa-state)
  (:documentation "Traverses REGEX object tree and generates corresponding NFA section, starting at
INPUT-NFA-STATE. In the process, it creates as many states for the element, glues with input state,
and returns output state as continuation point."))

(defmethod regex-to-nfa ((regex elm:single-char-element) input-nfa-state)
  (let ((output-state (make-instance 'state:nfa-state)))
    (state:add-nfa-normal-transition input-nfa-state regex output-state)
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
  "Testing new implementation of ANALYZE-NFA-STATE-REACHABILITY -- VERY PROMISING. I will build on
this one from now on."
  #+debug(format t "Regex to NFA for a negation, input state: ~a~&" input-nfa-state)
  (let* ((inner-regex (elm:inner-element regex))
         (output-state-inner (regex-to-nfa inner-regex input-nfa-state))
         (glue-state (make-instance 'state:nfa-state))
         (output-state (if (elm:greedy-p regex)
                           (regex-to-nfa (make-instance 'elm:zero-or-more-element
                                                        :element elm:*any-char-element*)
                                         glue-state)
                           glue-state))
         (state-reachability (state:analyze-nfa-state-reachability input-nfa-state
                                                                   output-state-inner)))
    ;; traverse NFA sub-tree, and states that have output-state-inner in their closures will
    ;; be converted to dead-ends, and states that can reach output-state-inner via elements
    ;; will get a transition on any-char to output-state (later: optionally via a
    ;; (+ any-char))
    (let ((inner-cont-pts-closures (state:prepare-nfa-state-closure-union
                                    (loop for state-i being the hash-keys in state-reachability
                                            using (hash-value s-status)
                                          when (member s-status '(:auto-connected
                                                                  :auto-and-element-connected))
                                            collect state-i))))
      ;; TODO: may postpone adding the auto transition to output, since it would cause the
      ;; output-state to be added to closures processed in 2nd pass below (extra loop iteration
      ;; without need), but if I split, I'll have another loop at the end.
      (loop for state-i being the hash-keys in state-reachability using (hash-value s-status)
            do (ecase s-status
                 (:auto-connected
                  ;; I think I'll keep the dead-end meaning to output-state-inner only
                  (state:set-dead-end state-i)
                  ;; experimental! (see test cases negation-5 --> negation-10
                  ;; the idea is: (not #\A) includes {"", "AA", "AB", ...}, but more work is needed
                  ;; and user may not expect it. It may be activated based on an option (TODO)
                  #+nil(state:set-nfa-transition-on-any-other state-i glue-state)
                  ;; Cleanup auto transitions, where destination is the output-state-inner.
                  ;; This is just simplification, not needed (cleans up generated NFA, but DFA not
                  ;; affected, test cases pass)
                  ;; UPDATE: new meaning of dead-end => should not delete such transition!
                  #+nil(state:delete-auto-transition state-i output-state-inner)
                  ;; Experimental: remove all auto-transitions out of these ones (in order to remove
                  ;; all transitions out of the dead-end (inner continuation point)
                  #+nil(state::delete-auto-transitions state-i)
                  ;; Experimental: also delete all normal and any-char transitions (for same reason)
                  #+nil(state::delete-all-outgoing-transitions state-i))
                 (:auto-and-element-connected
                  (state:set-dead-end state-i)
                  (state:set-nfa-transition-on-any-other state-i))
                 (:element-connected
                  ;; add any-other trans, unless this state was marked as dead-end in inner negation
                  (unless (state:dead-end-p state-i)
                    (state:set-nfa-transition-on-any-other state-i))
                  (unless (member state-i inner-cont-pts-closures :test #'eql)
                    ;; TODO: give user the choice (greedy/non-greedy)
                    (state:add-nfa-auto-transition state-i output-state)
                    (state:unset-dead-end state-i)))
                 (:not-connected
                  (state:add-nfa-auto-transition state-i output-state)
                  (state:unset-dead-end state-i)))))
    (fsm:with-unique-visit (state input-nfa-state add-inversion-transitions)
      (let ((closure (state:prepare-nfa-state-closure state)))
        (if (state:states-have-trans-on-any-other-p closure)
            #+nil(state:transition-on-any-other state)
            (progn
              ;; clear "any-other" transitions from the whole closure, since
              ;;we'll convert them to normal transitions (inversion)
              ;; TODO: try to avoid having to do this
              (dolist (s closure)
                (state:reset-nfa-transition-on-any-other s))
              ;; convert "any-other" transitions into normal transitions (using
              ;; inversion)
              ;; note that we add the created transitions to the closure's
              ;; initial point (actually it doesn't matter which state in the
              ;; closure gets the transitions)
              (let ((splitting-pts (state:collect-char-range-splitting-points closure))
                    (split-elements nil))
                #+debug(format t "Splitting points: ~s.~&" splitting-pts)
                (state:do-normal-transitions (_ element next-state) closure
                  ;; check to avoid inverting an inverted element (not sure if this is possible, but
                  ;; maybe in NFAs having complex closures, due to recursion)
                  (unless (eq next-state glue-state)
                    (state:with-split-element (element e splitting-pts)
                      (push e split-elements))))
                (loop for inv-elem in (elm:invert-elements
                                       (elm:sort-simple-elements split-elements))
                      do (state:add-nfa-normal-transition state inv-elem glue-state))))
            ;; else: no any-other trans, => traverse closure
            (dolist (s closure)
              (add-inversion-transitions s)))
        ;; traverse normal transitions
        (state:do-normal-transitions (trans elm next-state) closure
          (add-inversion-transitions next-state))))
    ;; connect the NOT element to the rest of the NFA
    output-state))

(defmethod regex-to-nfa ((regex elm:inv-element) input-nfa-state)
  (let* ((output-state (make-instance 'state:nfa-state))
         (elements (elm:inner-elements regex))
         (splitting-pts (elm:collect-char-range-splitting-points elements))
         (split-elements nil))
    (loop for element across elements
          do (state:with-split-element (element e splitting-pts)
               (push e split-elements)))
    (let* ((sorted-elements (elm:sort-simple-elements split-elements))
           (inverted-elements (elm:invert-elements sorted-elements)))
      (dolist (e inverted-elements)
        (state:add-nfa-normal-transition input-nfa-state e output-state)))
    output-state))

(defmethod regex-to-nfa ((regex elm:repeated-element) input-nfa-state)
  (let* ((output-state (make-instance 'state:nfa-state)))
    (with-accessors ((inner-element elm:inner-element)
                     (min-count elm:min-count)
                     (max-count elm:max-count)) regex
      (let ((input-nfa-state-i input-nfa-state))
        ;; 1 to min
        (loop repeat min-count
              do #1=(setf input-nfa-state-i (regex-to-nfa inner-element input-nfa-state-i)))
        #2=(state:add-nfa-auto-transition input-nfa-state-i output-state)
        ;; min + 1 to max
        (loop repeat (- max-count min-count)
              do #1# #2#)))
    output-state))

(defun produce-nfa (regex)
  "Produces NFA starting at root regex element. Its importance is in identifying the terminus
state."
  (let* ((root-state (make-instance 'state:nfa-state))
         (terminus-nfa-state (regex-to-nfa regex root-state)))
    (state:set-terminus terminus-nfa-state)
    (values root-state terminus-nfa-state)))

