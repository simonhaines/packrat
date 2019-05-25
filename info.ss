#lang setup/infotab
(define name "Simple Packrat Parser.")
(define scribblings '(("scribblings/packrat.scrbl")))
(define categories '(devtools))
(define required-core-version "5.0.1")
(define repositories (list "4.x"))
(define primary-file 
  '("main.ss"))
(define blurb
  (list '(div "Simple Packrat Parser.")))
(define release-notes 
  (list
   '(div "Applicable structures used in place of functions for fast contract checking.")))
