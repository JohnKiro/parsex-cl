(in-package :parsex-cl/functional-interface/v1)

#|
This is a legacy version of the macro, kept for compatibility with dependent client code. Newer code
should use version 2, which stabilizes the user interface.

Here is an example of usage (compare with the one given in the main file, for v2):

(define-functional-interface spaceship ()
  "Spaceship for games."
  (accelerate (rate) :doc "Accelerate spaceship by `rate`.")
  (fire-missile (direction) :doc "Fire missile in specified `direction`."))

(define-functional-interface shielded-spaceship (spaceship)
  "Shielded spaceship for games."
  (activate-shield (&key duration) :doc "Activate shield for specific `duration` in seconds."))

(let ((my-shielded-spaceship
        (make-shielded-spaceship
         :accelerate-fn (lambda (rate)
                          (format t "Accelerating by ~a..~%" rate))
         :fire-missile-fn (lambda (direction)
                            (format t "Firing in ~a direction..~%" direction))
         :activate-shield-fn (lambda (duration)
                               (format t "Activating shield for ~a seconds..~%"
                                       duration)))))
  (activate-shield my-shielded-spaceship :duration 100)
  (accelerate my-shielded-spaceship 100)
  (fire-missile my-shielded-spaceship 'north))
|#

(defmacro define-functional-interface (interface-name (&optional included-interface) &body body
                                       &environment env)
  (let ((func::*interface-version* 1))
    ;; ensure expansion in current context, with version = 1
    (macroexpand-1 `(func::%define-functional-interface ,interface-name ,(when included-interface
                                                                           (list included-interface))
                      ,@body)
                   env)))
