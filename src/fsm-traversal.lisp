(in-package :parsex-cl.fsm-traversal)

(defgeneric traverse-fsm-transitions (root-state traversal-fn))

(defmethod traverse-fsm-transitions ((root-state parsex-cl.regex-nfa:nfa-state) traversal-fn)
  "Traverse all transitions in the NFA state machine, starting from ROOT-STATE. This includes
both normal and auto transitions. TRAVERSAL-FN is called for each transition."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (nfa-state)
               (unless (gethash nfa-state traversal-mark-lookup-table)
                 (setf (gethash nfa-state traversal-mark-lookup-table) t)
                 (loop for trans in (parsex-cl.regex-nfa:normal-transitions nfa-state)
                       for elem = (parsex-cl.regex-nfa:element trans)
                       for next-state = (parsex-cl.regex-nfa:next-state trans)
                       do (funcall traversal-fn nfa-state elem next-state)
                          (iter next-state))
                 (loop for dest in (parsex-cl.regex-nfa:auto-transitions nfa-state)
                       do (funcall traversal-fn nfa-state :auto dest)
                          (iter dest)))))
      (iter root-state))))

(defmethod traverse-fsm-transitions ((root-state dfa-state) traversal-fn)
  "Traverse all transitions in the DFA state machine, starting from ROOT-STATE. This includes
both normal and transitions on any other char. TRAVERSAL-FN is called for each transition."
  (let ((traversal-mark-lookup-table (make-hash-table)))
    (labels ((iter (dfa-state)
               (unless (gethash dfa-state traversal-mark-lookup-table)
                 (setf (gethash dfa-state traversal-mark-lookup-table) t)
                 (loop for (elem . next-state) in (transitions dfa-state)
                       do (funcall traversal-fn dfa-state elem next-state)
                          (iter next-state))
                 (let ((dest-on-any-other (transition-on-any-other dfa-state)))
                   (when dest-on-any-other
                     (funcall traversal-fn dfa-state :any-other-char dest-on-any-other)
                     (iter dest-on-any-other))))))
      (iter root-state))))
