#lang racket/base

(require racket/contract/base
         racket/bool
         (only-in srfi/1 lset-union)
         "parser-struct.rkt")

;; Structures

(define-struct parse-position (file line column))
(define-struct parse-results (position base [next* #:mutable] map))
(define-struct parse-result (successful? semantic-value next error))
(define-struct parse-error (position expected messages))
(provide/contract 
 (struct parse-position 
         ([file string?] [line number?] [column number?]))
 (struct parse-result 
         ([successful? boolean?]
          [semantic-value any/c]
          [next (or/c false? parse-results?)]
          [error (or/c false? parse-error?)]))
 (struct parse-results 
         ([position (or/c false? parse-position?)]
          [base any/c]
          [next* (or/c false? parse-results? (-> parse-results?))]
          [map (hash/c symbol? (or/c false? parse-result?))]))
 (struct parse-error
         ([position (or/c parse-position? false?)]
          [expected (or/c false? (listof any/c))]
          [messages (listof string?)])))

;; Values

(define-syntax define/contract
  (syntax-rules ()
    [(define/contract (nm a ...) con body ...)
     (begin
       (provide/contract [nm con])
       (define (nm a ...) body ...))]))

(define/contract (top-parse-position filename) 
  (-> string? parse-position?)
  (make-parse-position filename 1 0))

(define/contract (update-parse-position pos ch)
  (-> parse-position? char? parse-position?)
  (let ((file (parse-position-file pos))
        (line (parse-position-line pos))
        (column (parse-position-column pos)))
    (case ch
      ((#\return) 
       (make-parse-position file line 0))
      ((#\newline) 
       (make-parse-position file (+ line 1) 0))
      ((#\tab) 
       (make-parse-position file line (* (quotient (+ column 8) 8) 8)))
      (else 
       (make-parse-position file line (+ column 1))))))

(define/contract (empty-results pos)
  (-> (or/c parse-position? false?) parse-results?)
  (make-parse-results pos #f #f (make-hasheq)))

(define/contract (make-results pos base next-generator)
  (-> (or/c parse-position? false?) 
      (or/c false? (cons/c any/c any/c)) 
      (-> parse-results?) 
      parse-results?)
  (make-parse-results pos base next-generator (make-hasheq)))

(define/contract (make-error-expected pos thing)
  (-> (or/c parse-position? false?) any/c parse-error?)
  (make-parse-error pos (list thing) '()))

(define/contract (make-error-message pos msg)
  (-> (or/c parse-position? false?) string? parse-error?)
  (make-parse-error pos '() (list msg)))

(define/contract (make-result semantic-value next)
  (-> any/c parse-results? parse-result?)
  (make-parse-result #t semantic-value next #f))

(define/contract (parse-error->parse-result err)
  (-> parse-error? parse-result?)
  (make-parse-result #f #f #f err))

(define/contract (make-expected-result pos thing)
  (-> (or/c parse-position? false?) any/c parse-result?)
  (parse-error->parse-result (make-error-expected pos thing)))

(define/contract (make-message-result pos msg)
  (-> (or/c parse-position? false?) string? parse-result?)
  (parse-error->parse-result (make-error-message pos msg)))

(define/contract (base-generator->results generator)
  (-> (-> (values (or/c parse-position? false?) 
                  (or/c (cons/c any/c any/c) false?)))
      parse-results?)
  ;; Note: applies first next-generator, to get first result
  (define (results-generator)
    (let-values (((pos base) (generator)))
      (if (not base)
	  (empty-results pos)
	  (make-results pos base results-generator))))
  (results-generator))

(define/contract (parse-results-next results)
  (-> parse-results? parse-results? #;(or/c parse-results? false?))
  (let ((next (parse-results-next* results)))
    (if (procedure? next)
	(let ((next-value (next)))
	  (set-parse-results-next*! results next-value)
	  next-value)
	(if (false? next)
            (error "Got false!")
            next))))

(define/contract (results->result results key fn)
  (-> parse-results? symbol? (-> parse-result?) parse-result?)  
  (let ((results-map (parse-results-map results)))
    (cond
      ((hash-ref results-map key (lambda () #f))
       => (lambda (value)
            (if (not value)
                (error "Recursive parse rule" key)
                value)))
      (else (begin
              (hash-set! results-map key #f)
              (let ((result (fn)))
                (hash-set! results-map key result)
                result))))))

(define/contract (parse-position>? a b)
  (-> (or/c parse-position? false?) (or/c parse-position? false?) boolean?)
  (cond
   ((not a) #f)
   ((not b) #t)
   (else (let ((la (parse-position-line a)) (lb (parse-position-line b)))
	   (or (> la lb)
	       (and (= la lb)
		    (> (parse-position-column a) (parse-position-column b))))))))

(define/contract (parse-error-empty? e)
  (-> parse-error? boolean?)
  (and (null? (parse-error-expected e))
       (null? (parse-error-messages e))))

(define/contract (merge-parse-errors e1 e2)
  (-> (or/c parse-error? false?) 
      (or/c parse-error? false?) 
      (or/c parse-error? false?))
  (cond
   ((not e1) e2)
   ((not e2) e1)
   (else
    (let ((p1 (parse-error-position e1))
	  (p2 (parse-error-position e2)))
      (cond
       ((or (parse-position>? p1 p2) (parse-error-empty? e2)) e1)
       ((or (parse-position>? p2 p1) (parse-error-empty? e1)) e2)
       (else (make-parse-error p1
			       (lset-union equal?
					   (parse-error-expected e1)
					   (parse-error-expected e2))
			       (lset-union equal?
					   (parse-error-messages e1)
					   (parse-error-messages e2)))))))))

(define/contract (merge-result-errors result errs)
  (-> parse-result? (or/c parse-error? false?) parse-result?)
  (make-parse-result (parse-result-successful? result)
		     (parse-result-semantic-value result)
		     (parse-result-next result)
		     (merge-parse-errors (parse-result-error result) errs)))

;---------------------------------------------------------------------------
; Combinators

(define/contract (packrat-check-base token-kind k)
  (-> any/c (-> any/c parser?) parser?)
  (parser
   (lambda (results)
     (let ((base (parse-results-base results)))
       (if (eqv? (and base (car base)) token-kind)
           ((k (and base (cdr base))) (parse-results-next results))
           (make-expected-result (parse-results-position results)
                                 (if (not token-kind)    ;; This seems buggy:
                                     "end-of-file"       ;; What if you were expecting the string "end-of-file"?
                                     token-kind)))))))

(define/contract (packrat-check-pred token-pred k)
  (-> (-> any/c boolean?)
      (-> any/c parser?)
      parser?)
  (parser
   (lambda (results)
     (let ((base (parse-results-base results)))
       (if (and base (token-pred (car base)))
           ((k (and base (cdr base))) (parse-results-next results))
           (make-expected-result (parse-results-position results)
                                 token-pred))))))

(define/contract (packrat-check p k)
  (-> parser? (-> any/c parser?) parser?)
  (parser
   (lambda (results)
     (let ((result (p results)))
       (if (parse-result-successful? result)
           (merge-result-errors ((k (parse-result-semantic-value result))
                                 (parse-result-next result))
                                (parse-result-error result))
           result)))))

(define/contract (packrat-or p1 p2)
  (-> parser? parser? parser?)
  (parser
   (lambda (results)
     (let ((result (p1 results)))
       (if (parse-result-successful? result)
           result
           (merge-result-errors (p2 results)
                                (parse-result-error result)))))))

(define/contract (packrat-unless explanation p1 p2)
  (-> string? parser? parser? parser?)
  (parser
   (lambda (results)
     (let ((result (p1 results)))
       (if (parse-result-successful? result)
           (make-message-result (parse-results-position results)
                                explanation)
           (p2 results))))))

;; ----

(define/contract (packrat-port-results filename p)
  (-> string? port? parse-results?)
  (base-generator->results
   (let ((ateof #f)
         (pos (top-parse-position filename)))
     (lambda ()
       (if ateof
           (values pos #f)
           (let ((x (read-char p)))
             (if (eof-object? x)
                 (begin
                   (set! ateof #t)
                   (values pos #f))
                 (let ((old-pos pos))
                   (set! pos (update-parse-position pos x))
                   (values old-pos (cons x x))))))))))

(define/contract (packrat-string-results filename s)
  (-> string? string? parse-results?)
  (base-generator->results
   (let ((idx (box 0))
         (len (string-length s))
         (pos (box (top-parse-position filename))))
     (lambda ()
       (if (= (unbox idx) len)
           (values (unbox pos) #f)
           (let ((x (string-ref s (unbox idx)))
                 (old-pos (unbox pos)))
             (set-box! pos (update-parse-position (unbox pos) x))
             (set-box! idx (add1 (unbox idx)))
             (values old-pos (cons x x))))))))

(define/contract (packrat-list-results tokens)
  (-> (listof any/c) parse-results?)
  (base-generator->results
   (let ((stream tokens))
     (lambda ()
       (if (null? stream)
	   (values #f #f)
	   (let ((base-token (car stream)))
	     (set! stream (cdr stream))
	     (values #f base-token)))))))

;; This code is derived from and released under the same license as:
;; http://dev.lshift.net/tonyg/json-scheme/portable-packrat.scm

;; It has been edited for content and contracted for her pleasure.
