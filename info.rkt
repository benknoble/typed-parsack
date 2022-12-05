#lang info
(define collection "typed-parsack")
(define deps '("parsack-lib"
               "typed-racket-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/typed-parsack.scrbl" ())))
(define pkg-desc "typed parsack")
(define version "0.0")
(define pkg-authors '(benknoble))
(define license 'MIT)
