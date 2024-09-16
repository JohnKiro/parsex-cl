(in-package :parsex-cl.chars.test)

(def-suite :parsex-cl.chars.test-suite
  :description "Tests the chars util"
  :in :parsex-cl.test-suite)

(in-suite :parsex-cl.chars.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(test char-range-equality
  (let ((range1 (make-instance 'chars:char-range :char-start #\a :char-end #\f))
        (range2 (make-instance 'chars:char-range :char-start #\a :char-end #\f)))
    (is (chars:char-range-equal range1 range2))))

(defun prepare-char-ranges-from-list (ranges)
  "Maps a list of char ranges in the form (start . end) into a list of char-range objects."
  (loop for (start . end) in ranges
        collect (make-instance 'chars:char-range :char-start start :char-end end)))

(test char-range-splitting-test1
  "Test character splitting, including splitting points at boundaries."
  (let* ((splitting-points '(#\a #\b #\e #\f #\p #\w))
         (range (make-instance 'chars:char-range :char-start #\b :char-end #\f))
         (expected-ranges (prepare-char-ranges-from-list '((#\b . #\b)
                                                           (#\c . #\e)
                                                           (#\f . #\f))))
         (split-ranges (chars:split-char-range range splitting-points)))
    (is (eq nil (set-exclusive-or expected-ranges split-ranges :test #'chars:char-range-equal)))))

(test char-splitting-test
  (let* ((regex '(:or
                  (:char-range #\a #\d)
                  (:char-range #\c #\f)
                  "lmn"
                  #\x
                  #\y
                  #\z))
         (root-state (nfa:parse-and-produce-nfa regex))
         (root-closure (nfa::prepare-nfa-state-closure-union (list root-state)))
         (splitting-points (nfa::collect-char-range-splitting-points root-closure)))
    (format t "~&Splitting chars for root state's closure: ~a~&" splitting-points)
    (is (equal splitting-points '(#\` #\b #\d #\f #\k #\l #\w #\x #\y #\z)))))

(defmacro define-regex-matching-test (test-name
                                      &key
                                        (description nil)
                                        regex
                                        input-text
                                        (expected-matching-status :regex-not-matched)
                                        (expected-accumulator-value input-text))
  (let ((thefuncall `(run-regex-matching-test
                      ',regex
                      ,input-text
                      :expected-matching-status ,expected-matching-status
                      :expected-accumulator-value ,expected-accumulator-value)))
    (if description
        `(test ,test-name ,description ,thefuncall)
        `(test ,test-name ,thefuncall))))


