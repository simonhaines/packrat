#lang scribble/doc
@(require scribble/manual
          scribble/eval
          planet/util
          (planet cce/scheme:7:2/planet)
          (planet cce/scheme:7:2/scribble)
          (for-label (this-package-in main))
          (for-label scheme))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require 
                (planet ,(this-package-version-symbol main))))
    the-eval))

@title{@bold{Packrat}: Simple Packrat Parsing}

@author+email["David Van Horn" "dvanhorn@ccs.neu.edu"]

This module provides a small library of Packrat parsing combinators
and a syntax for defining parsers.

This code is based on the portable packrat parsing library by
Tony Garnock-Jones:
@itemize{@item{@url{http://www.lshift.net/~tonyg/packrat.pdf}}
         @item{@url{http://dev.lshift.net/tonyg/json-scheme/}}}

@link[(format 
       "http://planet.plt-scheme.org/trac/newticket?component=~a%2F~a&planetversion=(~a+~a)"
       (this-package-version-owner)
       (this-package-version-name)
       (this-package-version-maj)
       (this-package-version-min))]{Report a bug}.

@table-of-contents[]
@section{Main}

@defmodule/this-package[]

This module provides bindings from the combinator library and the parser syntax.

@section{Combinator library}
@defmodule/this-package[combinator]

@defstruct[parse-position ([filename string?] [line number?] [column number?])]{ }

@defstruct[parse-result 
           ([successful? boolean?]
            [semantic-value any/c]
            [next (or/c false? parse-results?)]
            [error (or/c false? parse-error?)])]{ }
                                                
@defstruct[parse-results 
           ([position (or/c false? parse-position?)]
            [base any/c]
            [next* (or/c false? parse-results? (-> parse-results?))]
            [map (hash/c symbol? (or/c false? parse-result?))])]{ }

@defstruct[parse-error
           ([position (or/c parse-position? false?)]
            [expected (or/c false? (listof any/c))]
            [messages (listof string?)])]{ }

@defproc[(top-parse-position [filename string?]) parse-position?]{ }

@defproc[(update-parse-position [pos parse-position?] [ch char?]) parse-position?]{ }
  
@defproc[(empty-results [pos (or/c parse-position? false?)]) parse-results?]{ }
  
@defproc[(make-results [pos (or/c parse-position? false?)]
                       [base (or/c false? (cons/c any/c any/c))]
                       [next-generator (-> parse-results?)])      
         parse-results?]{ }
  
@defproc[(make-error-expected [pos (or/c parse-position? false?)]
                              [thing any/c])
         parse-error?]{ }

@defproc[(make-error-message [pos parse-position?] 
                             [msg string?])
         parse-error?]{ }
  
@defproc[(make-result [semantic-value any/c] [next parse-results?]) 
         parse-result?]{ }

@defproc[(parse-error->parse-result [err parse-error?]) parse-result?]{ }
  
@defproc[(make-expected-result [pos (or/c parse-position? false?)] 
                               [thing any/c])
         parse-result?]

@defproc[(make-message-result [pos (or/c parse-position? false?)]
                              [msg string?])
         parse-result?]{ }
  
@defproc[(base-generator->results 
          [generator
           (-> (values (or/c parse-position? false?) 
                       (or/c (cons/c any/c any/c) false?)))])
         parse-results?]{ }
      
@defproc[(parse-results-next [results parse-results?]) parse-results?]{ }
  
@defproc[(results->result [results parse-results?] [key symbol?] [fn (-> parse-result?)])
         parse-result?]{ }
  
@defproc[(parse-position>? [a (or/c parse-position? false?)] [b (or/c parse-position? false?)]) boolean?]{ }

@defproc[(parse-error-empty? [e parse-error?]) boolean?]{ }

@defproc[(merge-parse-errors [e1 (or/c parse-error? false?)] [e2 (or/c parse-error? false?)])
         (or/c parse-error? false?)]{ }

@defproc[(merge-result-errors [result parse-result?] [errs (or/c parse-error? false?)]) parse-result?]{ }

@defproc[(packrat-check-base [token-kind any/c] [k (-> any/c (-> parse-results? parse-result?))])
         (-> parse-results? parse-result?)]{ }

@defproc[(packrat-check-pred [token-pred (-> any/c boolean?)] [k (-> any/c (-> parse-results? parse-result?))])      
         (-> parse-results? parse-result?)]{ }
                                           
@defproc[(packrat-check [parser (-> parse-results? parse-result?)] 
                        [k (-> any/c (-> parse-results? parse-result?))])
         (-> parse-results? parse-result?)]{ }
      
@defproc[(packrat-or [p1 (-> parse-results? parse-result?)] [p2 (-> parse-results? parse-result?)])
         (-> parse-results? parse-result?)]
      
@defproc[(packrat-unless [explanation string?] 
                         [p1 (-> parse-results? parse-result?)] 
                         [p2 (-> parse-results? parse-result?)])
         (-> parse-results? parse-result?)]{ }

@defproc[(packrat-port-results [filename string?] [p port?])
         parse-results?]{ }
                        
@defproc[(packrat-string-results [filename string?] [s string?]) parse-results?]{ }

@defproc[(packrat-list-results [tokens (listof any/c)])
         parse-results?]{ }
  
@section{Parser syntax}
@defmodule/this-package[parser]
     

@defform/subs[#:literals (! / := |@| quote)
                         (parse id ([nonterminal-id (sequence body body0 ...)] ...))
                         ([sequence (part ...)]
                          [part (! part ...)
                                (/ sequence ...)
                                (? expr)
                                (code:line id := (quote kind))
                                (code:line id := |@|)
                                (code:line id := nonterminal-id)
                                (code:line id := (? expr))
                                (code:line nonterminal-id)])]
                          
                                                                      
     
                              

@section{Examples}

Here is an example of a simple calculator.

@defexamples[#:eval the-eval
                    (define calc 
                      (parse expr
                             (expr ((a := mulexp '+ b := mulexp)
                                    (+ a b))
                                   ((a := mulexp) a))
                             (mulexp ((a := simple '* b := simple)
                                      (* a b))
                                     ((a := simple '* b := simple)
                                      (* a b))
                                     ((a := simple) a))
                             (simple ((a := 'num) a)
                                     (('oparen a := expr 'cparen) a))))
                    (define g 
                      (packrat-list-results '((num . 1) (+) (num . 2) (*) (num . 3))))
                    (parse-result-semantic-value (calc g))]

See the tests source file for an example of a parser for a simplified Scheme grammar.


@section{Test suite}
@defmodule/this-package[test]

Requiring this module will run the test suite.
           

@index-section[]
