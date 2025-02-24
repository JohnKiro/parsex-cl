(in-package :parsex-cl/regex/fsm)

;;;; This package is for common behavior to both FSM types (NFA, DFA)

(defgeneric traverse-fsm-transitions (root-state traversal-fn)
  (:documentation "Traverse an FSM starting from a ROOT-STATE, with traversal function
TRAVERSAL-FN. The traversal function expects three parameters: origin state, transition element,
destination state."))

(defgeneric fsm-acceptance-state-p (fsm-state)
  (:documentation "Predicate to indicate whether the state is acceptance or not. Initially it will
be used only in GraphViz export, to give acceptance nodes different style."))

