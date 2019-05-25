#lang scheme
;; Packrat Parser Library (Parser Syntax)
;;
;; Copyright (c) 2010 David Van Horn <dvanhorn@ccs.neu.edu>
;; Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(require "combinator.ss")
(require "parser-struct.ss")
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
