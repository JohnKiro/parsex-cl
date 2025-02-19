(in-package :parsex-cl.nfa-element.test)

(def-suite :parsex-cl.nfa-element.test-suite
  :description "Tests the NFA element package"
  :in :parsex-cl.test-suite)

(in-suite :parsex-cl.nfa-element.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(test char-range-equality
  (let ((range1 (make-instance 'elm:char-range-element :char-start #\a :char-end #\f))
        (range2 (make-instance 'elm:char-range-element :char-start #\a :char-end #\f)))
    (is (elm:char-range-equal range1 range2))))

(defun prepare-char-ranges-from-list (ranges)
  "Maps a list of char ranges in the form (start . end) into a list of char-range objects."
  (loop for (start . end) in ranges
        collect (make-instance 'elm:char-range-element :char-start start :char-end end)))

(test char-range-splitting-test1
  "Test character splitting, including splitting points at boundaries."
  (let* ((splitting-points #(#\a #\b #\e #\f #\p #\w))
         (range (make-instance 'elm:char-range-element :char-start #\b :char-end #\f))
         (expected-ranges (prepare-char-ranges-from-list '((#\b . #\b)
                                                           (#\c . #\e)
                                                           (#\f . #\f))))
         (split-ranges (elm:split-char-range range splitting-points)))
    (is (eq nil (set-exclusive-or expected-ranges split-ranges :test #'elm:char-range-equal)))))

(test char-splitting-test
  "TODO: change/remove as we shouldn't depend on NFA or SEXP here!"
  (let* ((regex '(:or
                  (:char-range #\a #\d)
                  (:char-range #\c #\f)
                  "lmn"
                  #\x
                  #\y
                  #\z))
         (regex-obj-tree (sexp:prepare-regex-tree regex))
         (root-state (nfa:produce-nfa regex-obj-tree))
         (root-closure (nfa::prepare-nfa-state-closure-union (list root-state)))
         (splitting-points (nfa::collect-char-range-splitting-points root-closure)))
    (format t "~&Root closure: ~a" root-closure)
    (format t "~&Splitting chars for root state's closure: ~a~&" splitting-points)
    (is (equal splitting-points (coerce #(#\` #\b #\d #\f #\k #\l #\w #\x #\y #\z) 'string)))))
