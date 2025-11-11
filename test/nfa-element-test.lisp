(in-package :parsex-cl.test/nfa-element.test)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun prepare-element-objects-from-list (element-details)
    "Maps a list of combination of char ranges in the form (start . end), and single chars in the
form (ch), into a list of corresponding element objects."
    (loop for (start . end) in element-details
          collect (if end
                      (elm::make-char-range :start start :end end)
                      (make-instance 'elm:single-char-element :single-char start)))))

(test char-range-splitting-test1
  "Test character splitting, including splitting points at boundaries."
  (let* ((splitting-points "abefpw" #+nil#(#\a #\b #\e #\f #\p #\w))
         (range (make-instance 'elm:char-range-element :char-start #\b :char-end #\f))
         (expected-ranges (prepare-element-objects-from-list '((#\b)
                                                               (#\c . #\e)
                                                               (#\f))))
         (split-ranges (elm:split-char-range range splitting-points)))
    (is (eq nil (set-exclusive-or expected-ranges split-ranges :test #'elm:simple-element-equal)))))

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
         (root-closure (nfa-state:prepare-nfa-state-closure-union (list root-state)))
         (splitting-points (nfa-state::collect-char-range-splitting-points root-closure)))
    (format t "~&Root closure: ~a" root-closure)
    (format t "~&Splitting chars for root state's closure: ~a~&" splitting-points)
    (is (equal splitting-points (coerce #(#\` #\b #\d #\f #\k #\l #\w #\x #\y #\z) 'string)))))

;;Testing char element inversion
(defmacro define-element-inversion-test (name &key doc input-elements expected)
  "Define an element inversion test, specified by `name`, and having description `doc`. The elements
are given in `elements`, and the expected inversion is given in `expected`, both of which should be
a list with list elements either dotted pairs in the form (start-char .end-char), or single elements
in the form (single-char)."
  (alexandria:with-gensyms (input-elements-var actual-var expected-var)
    `(fiveam:test ,name ,@(when doc (list doc))
       (let* ((,input-elements-var (prepare-element-objects-from-list ',input-elements))
              (,expected-var (prepare-element-objects-from-list ',expected))
              (,actual-var (elm::invert-elements ,input-elements-var)))
         ;; check: actual and expected having same length, and same elements (as a set)
         ;; this is a fairly sufficient validation
         (is (and (= (length ,actual-var) (length ,expected-var))
                  (equal (set-exclusive-or ,actual-var ,expected-var
                                           :test #'elm:simple-element-equal)
                         nil)))))))

(define-element-inversion-test element-inversion-test1
  :doc "Inversion of two normal ranges, leading to three ranges, including two open-ended."
  :input-elements ((#\b . #\m)
                   (#\v . #\y))
  :expected ((:min . #\a)
             (#\n . #\u)
             (#\z . :max)))

(define-element-inversion-test element-inversion-test2
  :doc "Inversion of two ranges, including one open-ended (MIN -> char)."
  :input-elements ((:min . #\m)
                   (#\v . #\y))
  :expected ((#\n . #\u)
             (#\z . :max)))

(define-element-inversion-test element-inversion-test3
  :doc "Inversion of two open-ended ranges."
  :input-elements ((:min . #\m)
                   (#\v . :max))
  :expected ((#\n . #\u)))

(define-element-inversion-test element-inversion-test4
  :doc "Inversion of three ranges, including two open-ended."
  :input-elements ((:min . #\m)
                   (#\q . #\s)
                   (#\v . :max))
  :expected ((#\n . #\p)
             (#\t . #\u)))

(define-element-inversion-test element-inversion-test5
  :doc "Inversion of three ranges, including one open-ended (char -> MAX)."
  :input-elements ((#\b . #\m)
                   (#\q . #\s)
                   (#\v . :max))
  :expected ((:min . #\a)
             (#\n . #\p)
             (#\t . #\u)))

(define-element-inversion-test element-inversion-test6
  :doc "Inversion of three elements: two ranges and a single char element in between."
  :input-elements ((#\b . #\m)
                   (#\q)
                   (#\v . :max))
  :expected ((:min . #\a)
             (#\n . #\p)
             (#\r . #\u)))

(define-element-inversion-test element-inversion-test7
  :doc "Inversion of no elements, resulting in full range (MIN -> MAX)."
  :input-elements nil
  :expected ((:min . :max)))


(define-element-inversion-test element-inversion-test8
  :doc "Inversion of overlapping elements (still ordered), including a full-range element, resulting
in NIL."
  :input-elements ((:min . :max)
                   (#\q)
                   (#\v . :max))
  :expected nil)

(define-element-inversion-test element-inversion-test9
  :doc "Inversion of two adjacent elements (no gap between them), including an open-ended range."
  :input-elements ((:min . #\a)
                   (#\b))
  :expected ((#\c . :max)))

(define-element-inversion-test element-inversion-test10
  :doc "Inversion of two adjacent elements (no gap between them), with no open-ended ranges."
  :input-elements ((#\b . #\m)
                   (#\n))
  :expected ((:min . #\a)
             (#\o . :max)))

(define-element-inversion-test element-inversion-test11
  :doc "Inversion of multiple adjacent elements."
  :input-elements ((#\b . #\m)
                   (#\n . #\q)
                   (#\r . #\x))
  :expected ((:min . #\a)
             (#\y . :max)))

(define-element-inversion-test element-inversion-test12
  :doc "Inversion of elements including a range having a single char."
  :input-elements ((#\b . #\m)
                   (#\r . #\r)
                   (#\y . :max))
  :expected ((:min . #\a)
             (#\n . #\q)
             (#\s . #\x)))

(define-element-inversion-test element-inversion-test13
  :doc "Inversion of elements that result in single chars in the output."
  :input-elements ((#\b . #\m)
                   (#\o . #\r)
                   (#\t . :max))
  :expected ((:min . #\a)
             (#\n)
             (#\s)))

(define-element-inversion-test element-inversion-test14
  :doc "Inversion of duplicate element (same element repeated in input)."
  :input-elements ((#\b . #\m)
                   (#\b . #\m)
                   (#\o . #\r))
  :expected ((:min . #\a)
             (#\n)
             (#\s . :max)))
