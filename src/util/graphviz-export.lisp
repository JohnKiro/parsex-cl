(in-package :parsex-cl.graphviz-util)


;;; for reference (copied from regex.lisp for now)
(defun dfa-to-graphvizdot (root-dfa)
  "Generate a Graphviz DOT string for the DFA state machine."
  (let ((dfa-address-map (make-array 50 :adjustable t :fill-pointer 0))
        (terminal-index 0)
        (traversed-dfas nil))
    (labels ((get-dfa-index (dfa-state)
               (or (position dfa-state dfa-address-map)
                   (vector-push-extend dfa-state dfa-address-map)))
             (get-fresh-terminal-index ()
               (concatenate 'string "T" (write-to-string (incf terminal-index))))
             (el-to-string (el)
               (typecase el
                 (character el)
                 (elm:char-range-element (format nil "~a - ~a"
                                                 (elm:char-start el) (elm:char-end el)))
                 (t el)))
             (print-transition (source dest el stream)
               (when (and source dest)
                 (format stream "~%~a -> ~a [label=\"~a\"];"
                         (get-dfa-index source)
                         (get-dfa-index dest)
                         (el-to-string el))))
             (print-terminal-transition (source stream)
               (when (and source (not (dfa-state-definitely-terminal-p source)))
                 (format stream "~%~a -> ~a [label=\"~a (~a)\"];"
                         (get-dfa-index source)
                         (get-fresh-terminal-index)
                         ;;other/no input
                         "O/No-In"
                         (if (candidate-terminal source)
                             ;;match
                             "M"
                             ;;no match
                             "NM"))))
             (iter (start-dfa stream)
               (unless (find start-dfa traversed-dfas)
                 (push start-dfa traversed-dfas)
                 (loop for (el . dest-dfa) in (transitions start-dfa)
                       do (print-transition start-dfa dest-dfa el stream)
                          (iter dest-dfa stream))
                 (let ((dest-dfa-on-any-other (transition-on-any-other start-dfa)))
                   (when dest-dfa-on-any-other
                     (print-transition start-dfa dest-dfa-on-any-other "Other" stream)
                     (iter dest-dfa-on-any-other stream)))
                 (print-terminal-transition start-dfa stream))))
      (with-output-to-string (a-stream)
        (format a-stream "digraph \{~%")
        (format a-stream "rankdir = LR;~%")
        (iter root-dfa a-stream)
        (format a-stream "~%\}~%")))))

(defgeneric element-to-edge (element))

(defmethod element-to-edge ((element character))
  element)

(defmethod element-to-edge ((element elm:single-char-element))
  (format nil "~a" (elm:single-char element)))

(defmethod element-to-edge ((element elm:char-range-element))
  (format nil "~a - ~a" (elm:char-start element) (elm:char-end element)))

(defmethod element-to-edge ((element (eql :auto)))
  #\GREEK_SMALL_LETTER_EPSILON)

(defmethod element-to-edge ((element (eql :any-char)))
  "OTHER")

(defmethod element-to-edge ((element (eql :any-other-char)))
  "ANY-OTHER")

(defun fsm-to-graphvizdot (root-fsm-state &key (output-file nil) (output-stream nil) (regex nil))
  "Generate a Graphviz DOT string for an NFA or DFA state machine, starting at ROOT-FSM-STATE. The
generated string is returned, and is also optionally dumped to specified output file and/or output
stream. The optional REGEX parameter is also included in the diagram as a description for the regex.
Two keyword options also control the dumping:
OUTPUT-FILE specifies the output file path, and is NIL by default (no dumping to output file).
OUTPUT-STREAM specifies a stream to dump to (besides the output file). If T is passed, standard
output is used. NIL (default) disables the dumping."
  (labels ((fsm-transitions-to-dot (output-stream)
             (let ((state-index -1)
                   (state-lookup (make-hash-table)))
               (labels ((get-state-index (state)
                          (or (gethash state state-lookup)
                              (setf (gethash state state-lookup) (incf state-index))))
                        (transition-to-dot (src elem dst)
                          ;;TODO: REFACTOR!!
                          (when (fsm:fsm-acceptance-state-p src)
                            (format output-stream "~%   ~a [style=filled, fillcolor=~a];"
                                    (get-state-index src)
                                    "lightgreen"))
                          (when (fsm:fsm-acceptance-state-p dst)
                            (format output-stream "~%   ~a [style=filled, fillcolor=~a];"
                                    (get-state-index dst)
                                    "lightgreen"))
                          (format output-stream "~%    ~a -> ~a [label=\"~a\"];"
                                  (get-state-index src)
                                  (get-state-index dst)
                                  (element-to-edge elem))))
                 (fsm:traverse-fsm-transitions root-fsm-state #'transition-to-dot)))))
    (let ((result (with-output-to-string (output-stream)
                    (format output-stream "digraph \{~%")
                    (format output-stream "rankdir = LR;~%")
                    (when regex
                      (format output-stream "~%description [label=\"~a\", shape=box, style=filled,"
                              regex)
                      (princ  "color=lightyellow];" output-stream))
                    (fsm-transitions-to-dot output-stream)
                    (format output-stream "~%\}~%"))))
      (when output-file
        (with-open-file (file-stream output-file :direction :output :if-exists :supersede)
          (princ result file-stream)))
      (when output-stream
        (format output-stream "Generated DOT output:~%~a" result))
      result)))

(defun generate-graphviz-dot-diagram (root-fsm-state output-file &optional regex)
  "Generate diagram for an FSM (whether NFA or DFA), given its root state (ROOT-FSM-STATE). The
output is written to a file whose path is specified by the OUTPUT-FILE. An optional argument REGEX
may be used to describe the regex, to be displayed in the diagram."
  (let* ((dot (fsm-to-graphvizdot root-fsm-state :regex regex))
         (command (format nil "echo ~s | dot  -Tsvg -Nfontcolor=red -Nshape=circle -o ~a"
                          dot output-file)))
    (uiop:run-program command :output *standard-output*)))
