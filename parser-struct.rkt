#lang racket
(provide parser)

;; A Parser is a (parser [Results -> Result]).
(struct parser (f) #:property prop:procedure (struct-field-index f))
(provide (struct-out parser))
