#lang racket/base

(require "combinator.rkt")
(require "parser-struct.rkt")
(provide parse)

(define (object->external-representation o)
  (let ((s (open-output-string)))
    (write o s)
    (get-output-string s)))

(define-syntax parse
  (syntax-rules (:= quote ? ! @ /)
    ((_ start (nonterminal (alternative body0 body ...) ...) ...)
     (let ()
       (define nonterminal
         (parser
          (lambda (results)
            (results->result results 'nonterminal
                             (lambda ()
                               ((parse* #f "alts" nonterminal
                                        ((begin body0 body ...) alternative) ...)
                                results))))))
       ...
       start))))

(define-syntax parse*
  (syntax-rules (:= quote ? ! @ /)
    ((_ #f "alts" nt (body alternative))
     (parse* #f "alt" nt body alternative))
    
    ((_ #f "alts" nt (body alternative) rest0 rest ...)
     (packrat-or (parse* #f "alt" nt body alternative)
                 (parse* #f "alts" nt rest0 rest ...)))
    
    ((_ #f "alt" nt body ())
     (parser
      (lambda (results) (make-result body results))))
    
    ((_ #f "alt" nt body ((! fails ...) rest ...))
     (packrat-unless (string-append "Nonterminal " (symbol->string 'nt)
                                    " expected to fail "
                                    (object->external-representation '(fails ...)))
                     (parse* #f "alt" nt #t (fails ...))
                     (parse* #f "alt" nt body (rest ...))))
    
    ((_ #f "alt" nt body ((/ alternative ...) rest ...))
     (packrat-check (parse* #f "alts" nt (#t alternative) ...)
                    (parser
                     (lambda (result) (parse* #f "alt" nt body (rest ...))))))
    
    ((_ #f "alt" nt body (var := 'val rest ...))
     (packrat-check-base 'val
                         (lambda (var)
                           (parse* #f "alt" nt body (rest ...)))))
    
    ((_ #f "alt" nt body (var := @ rest ...))
     (lambda (results)
       (let ((var (parse-results-position results)))
         ((parse* #f "alt" nt body (rest ...)) results))))
    
    ((_ #f "alt" nt body (var := (? p) rest ...))
     (packrat-check-pred p
                         (lambda (var)
                           (parse* #f "alt" nt body (rest ...)))))
    
    ((_ #f "alt" nt body (var := val rest ...))
     (packrat-check val
                    (lambda (var)
                      (parse* #f "alt" nt body (rest ...)))))
    
    ((_ #f "alt" nt body (? p rest ...))
     (packrat-check-pred p
                         (lambda (dummy)
                           (parse* #f "alt" nt body (rest ...)))))
    
    ((_ #f "alt" nt body ('val rest ...))
     (packrat-check-base 'val
                         (lambda (dummy)
                           (parse* #f "alt" nt body (rest ...)))))
    
    ((_ #f "alt" nt body (val rest ...))
     (packrat-check val
                    (lambda (dummy)
                      (parse* #f "alt" nt body (rest ...)))))))
