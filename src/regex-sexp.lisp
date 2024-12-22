(in-package :parsex-cl.regex-sexp)

;;; Handling of regex in the form of sexp (sexp --> regex object tree).

(defconstant +regex-element-tags-package+ (let ((pkg-name :regex.element.tags))
                                            (or (find-package pkg-name)
                                                (make-package pkg-name)))
  "Package dedicated for regex tag names (SEQ, OR, +, etc.).")

;; TODO: reusable utility, or use Alexandria's ENSURE-SYMBOL
(defun reintern (symbol package)
  "Intern a symbol SYMBOL into a package PACKAGE, and return the newly interned symbol.
The difference from the standard INTERN function is that it receives a symbol, not a symbol name. So
practically it copies a symbol from a package into another one."
  (intern (symbol-name symbol) (find-package package)))

(defmacro case-list-regex (element-tag &body body)
  (labels ((prepare-body (body)
             (loop for (each-tag . each-clause) in body
                   collect (cons (reintern each-tag +regex-element-tags-package+) each-clause))))
    (let ((reinterned-element-tag (gensym)))
      `(let ((,reinterned-element-tag (reintern ,element-tag +regex-element-tags-package+)))
         (ecase ,reinterned-element-tag
           ,@(prepare-body body))))))

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: may split function using other helper functions
;;;; TODO: flatten (simplify) specific cases such as seq within seq
(defun prepare-regex-tree (regex)
  "Takes an input list as regex element, and recursively constructs an equivalent regex element
 object."
  (etypecase regex
    (character (make-instance 'elm:single-char-element :single-char regex))
    (keyword regex) ;"any char" (TODO: pass unchanged, or generate any-char-element?)
    (list (case-list-regex (car regex)
            ;; note that I could use any symbols (no need for keywords), but keywords are better to
            ;; avoid confusing the syntax coloring
            (:char-range (make-instance 'elm:char-range-element
                                        :char-start (second regex)
                                        :char-end (third regex)))
            (:seq (make-instance 'elm:sequence-element
                                 :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (:or (make-instance 'elm:or-element
                                :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (:* (make-instance 'elm:zero-or-more-element :element (prepare-regex-tree
                                                                   (second regex))))
            (:+ (make-instance 'elm:one-or-more-element :element (prepare-regex-tree
                                                                  (second regex))))
            (:? (make-instance 'elm:zero-or-one-element :element (prepare-regex-tree
                                                                  (second regex))))
            (:not (make-instance 'elm:negated-element :element (prepare-regex-tree
                                                                (second regex))))))
    (string (make-instance 'elm:sequence-element
                           :elements (map 'vector #'(lambda (ch)
                                                      (make-instance 'elm:single-char-element
                                                                     :single-char ch)) regex)))))
