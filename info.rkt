#lang info

(define collection "packrat")
(define version "2.4")
(define pkg-desc "Simple Packrat parser.")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/packrat.scrbl")))
(define pkg-authors '(dvanhorn simonhaines))
