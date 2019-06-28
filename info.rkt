#lang info

(define collection "packrat")
(define version "2.4")
(define pkg-desc "Simple Packrat parser.")
(define deps '("base" "rackunit-lib" "srfi-lite-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/packrat.scrbl")))
(define pkg-authors '(dvanhorn simonhaines))
