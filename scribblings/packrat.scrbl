#lang scribble/manual

@(require packrat
		  scribble/examples
		  (for-syntax packrat/parse)
          (for-label racket/base packrat))

@title[#:version "2.4"]{Packrat: Simple Packrat Parsing}
@author[(author+email "David Van Horn" "dvanhorn@ccs.neu.edu")
        (author+email "Simon Haines" "simon.haines@scalardata.com")]

@defmodule[packrat]{
This module provides a small library of parsing combinators and a syntax for defining parsers based on the portable packrat parsing library by Tony Garnock-Jones: @url{http://www.lshift.net/~tonyg/packrat.pdf}

Bug reports, suggestions and pull requests are welcome via @hyperlink["https://github.com/simonhaines/packrat"]{GitHub}.
}

@(table-of-contents)

@section{Combinator library}
@defmodule[packrat/combinator]

@defstruct[parse-position 
           ([filename string?]
            [line number?]
            [column number?])]{
A @racket[parse-position] structure represents a character location in an input stream.
}

@defstruct[parse-result 
           ([successful? boolean?]
            [semantic-value any/c]
            [next (or/c false? parse-results?)]
            [error (or/c false? parse-error?)])]{
A @racket[parse-result] structure describes the results of an attempt at a parse at a particular position in the input stream. It can either record a successful parse, in which case it contains an associated semantic-value, or a failed parse, in which case it contains a @racket[parse-error] structure.
}

@defstruct[parse-results 
           ([position (or/c false? parse-position?)]
            [base any/c]
            [next* (or/c false? parse-results? (-> parse-results?))]
            [map (hash/c symbol? (or/c false? parse-result?))])]{
A @racket[parse-results] structure notionally describes all possible parses that can be attempted from a particular point in an input stream, and the results of those parses. It contains a @racket[parse-position] structure, which corresponds to the position in the input stream that this @racket[parse-results] represents, and a map associated "key objects" with instances of @racket[parse-result].

Atomic objects (known as "base values"; usually either character or token/semantic-value pairs) are represented specially in the @racket[parse-results] data structure, as an optimisation: the two fields @racket[base] and @racket[next*] represent the implicit successful parse of a base value at the current position. The @racket[base] field contains a pair of a token-class-identifier and a semantic value unless the parse-results data structure as a whole is representing the end of the input stream, in which case it will contain @racket[#f].
}

@defstruct[parse-error
           ([position (or/c parse-position? false?)]
            [expected (or/c false? (listof any/c))]
            [messages (listof string?)])]{
A @racket[parse-error] structure represents collected error information from attempted parses. It contains two kinds of error report: a collection of "expected token" messages, and a collection of free-format message strings.
}

@defproc[(top-parse-position [filename string?]) parse-position?]{ 
Constructs a @racket[parse-position] representing the very beginning of an input stream. The argument is passed into @racket[make-parse-position] as the @racket[filename] parameter.
}

@defproc[(update-parse-position [pos parse-position?]
                                [ch char?]) parse-position?]{
Given a position, and the character occuring at that position, returns the position of the next character in the input stream. Most characters simple increment the column number. Expections to this rule are: @racket[#\return], which resets the column number to zero; @racket[#\newline], which both resets the column number to zero and increments the line number; and @racket[#\tab], which increments the column number to the nearest multiple of eight, just as a terminal with an eight-column tab stop setting might do.
}
  
@defproc[(empty-results [pos (or/c parse-position? false?)]) parse-results?]{
Creates an empty @racket[parse-results] structure using the @racket[pos] argument as the current position.
}

@defproc[(make-results [pos (or/c parse-position? false?)]
                       [base (or/c false? (cons/c any/c any/c))]
                       [next-generator (-> parse-results?)])      
         parse-results?]{
Constructs a @racket[parse-results] instance with the supplied @racket[pos], @racket[base] and @racket[next-generator], and a @racket[hash-eq] dictionary for the @racket[map].
}
  
@defproc[(make-error-expected [pos (or/c parse-position? false?)]
                              [thing any/c])
         parse-error?]{
Constructs a @racket[parse-error] with the supplied @racket[pos] and @racket[thing] as the "expected token" message.
}

@defproc[(make-error-message [pos parse-position?] 
                             [msg string?])
         parse-error?]{
Constructs a @racket[parse-error] with the supplied @racket[pos] and @racket[msg] as the "general error" message.
}
  
@defproc[(make-result [semantic-value any/c] [next parse-results?]) 
         parse-result?]{
Constructs a successful @racket[parse-result] with the supplied @racket[semantic-value] and @racket[next] instance. 
}

@defproc[(parse-error->parse-result [err parse-error?])
         parse-result?]{
Transforms the @racket[err] into a failed @racket[parse-result].
}
  
@defproc[(make-expected-result [pos (or/c parse-position? false?)] 
                               [thing any/c])
         parse-result?]{
Combines @racket[make-error-expected] and @racket[parse-error->parse-result] to construct a failed @racket[parse-result] with the supplied @racket[pos] and @racket[thing] as the "expected token" message.
}

@defproc[(make-message-result [pos (or/c parse-position? false?)]
                              [msg string?])
         parse-result?]{
Combines @racket[make-error-message] and @racket[parse-error->parse-result] to construct a failed @racket[parse-result] with the supplied @racket[pos] and @racket[msg] as the "general error" message.
}
  
@defproc[(base-generator->results 
          [generator
           (-> (values (or/c parse-position? false?) 
                       (or/c (cons/c any/c any/c) false?)))])
         parse-results?]{
This function is used to set up an initial input stream of base tokens. The argument is to be a nullary function returning multiple values, the first is a @racket[parse-position] or @racket[#f], and the second is a base token--a pair of a token class identifier and a semantic value. The argument is called every time the parser needs to read a fresh base token from the input stream.
}
      
@defproc[(parse-results-next [results parse-results?]) parse-results?]{
Accesses the @racket[next] value of a @racket[parse-results] instance, and removes this value from the list of @racket[parse-results]. If the @racket[next] value is @racket[#f] an error is generated.
}
  
@defproc[(results->result [results parse-results?] [key symbol?] [fn (-> parse-result?)])
         parse-result?]{
This is the central function that drives the parsing process. It examines the result map in the @racket[parse-results], searching for an entry matching the @racket[key]. If an entry is found, the associated @racket[parse-result] is returned; otherwise, the @racket[fn] is called and the resulting @racket[parse-result] is both stored in the result map and returned.
}
  
@defproc[(parse-position>? [a (or/c parse-position? false?)] [b (or/c parse-position? false?)]) boolean?]{
Compares the two @racket[parse-position] instances, and returns true if the first position is later in the input stream (according to the @racket[line] and @racket[col] values) than the second position.
}

@defproc[(parse-error-empty? [e parse-error?]) boolean?]{
A predicate for @racket[parse-error] instances that returns @racket[#t] if the instance contains no "expected token" or "general error" messages.
}

@defproc[(merge-parse-errors [e1 (or/c parse-error? false?)] [e2 (or/c parse-error? false?)])
         (or/c parse-error? false?)]{
Merges two @racket[parse-error] instances. If one instance represents a position earlier in the input stream than the other, that instance is returned. If they both represent the same position, the "expected token" sets are united and the "general error" sets are appended to form a new @racket[parse-error] at the same position.
}

@defproc[(merge-result-errors [result parse-result?] [errs (or/c parse-error? false?)]) parse-result?]{
Merges @racket[parse-result] and @racket[parse-error] instances to create a new @racket[parse-result] with the errors merged with @racket[merge-parse-errors].
}

@defproc[(packrat-check-base [token-kind any/c] [k (-> any/c (-> parse-results? parse-result?))])
         (-> parse-results? parse-result?)]{
Returns a combinator which, if the next base token has a token class identifier equal to @racket[token-kind], evaluates @racket[k] with the semantic value of the next base token. The result should be another parser combinator, which is applied to the @racket[parse-results] representing the remainder of the input stream.
}

@defproc[(packrat-check-pred [token-pred (-> any/c boolean?)] [k (-> any/c (-> parse-results? parse-result?))])      
         (-> parse-results? parse-result?)]{
Returns a combinator which tests the next base token with @racket[token-pred] and if the result is @racket[#t], evaluates @racket[k] with the semantic value of the token, similarly to @racket[packrat-check-base].
}
                                           
@defproc[(packrat-check [parser (-> parse-results? parse-result?)] 
                        [k (-> any/c (-> parse-results? parse-result?))])
         (-> parse-results? parse-result?)]{
Returns a combinator which attempts to parse using @racket[parser], and if the parse is successful, hands the resulting semantic value to @racket[k] and continues parsing using the resulting combinator.
}
      
@defproc[(packrat-or [p1 (-> parse-results? parse-result?)] [p2 (-> parse-results? parse-result?)])
         (-> parse-results? parse-result?)]{
Returns a combinator which attempts to parse using @racket[p1], only trying @racket[p2] if @racket[p1] fails to parse the input. This is the basic combinator used to implement a choice among several alternative means of parsing an input stream.
}
      
@defproc[(packrat-unless [explanation string?] 
                         [p1 (-> parse-results? parse-result?)] 
                         [p2 (-> parse-results? parse-result?)])
         (-> parse-results? parse-result?)]{
The combinator returned from this function first tries @racket[p1]. If it fails, @racket[p2] is tried; otherwise an error containing @racket[explanation] is returned. This can be used to assert that a particular sequence of tokens does not occur at the current position before continuing.
}

@defproc[(packrat-port-results [filename string?] [p port?])
         parse-results?]{
Returns an input stream generator from the @racket[filename] and @racket[p] port. See @racket[base-generator->results].
}
                        
@defproc[(packrat-string-results [filename string?] [s string?]) parse-results?]{
Returns an input stream generator from the @racket[filename] and @racket[s] string. See @racket[base-generator->results].
}

@defproc[(packrat-list-results [tokens (listof any/c)])
         parse-results?]{
Returns an input stream generator from the @racket[tokens]. See @racket[base-generator->results].
}

  
@section{Parser syntax}
@defmodule[packrat/parse]

@defform/subs[#:literals (! / := |@| quote)
              (parse id (nonterminal-id (sequence body body0 ...) ...) ...)
              ([sequence (part ...)]
               [part (! part ...)
                     (/ sequence ...)
                     (? expr)
					 (code:line nonterminal-id)
                     (code:line id := (quote kind))
                     (code:line id := |@|)
                     (code:line id := nonterminal-id)
                     (code:line id := (? expr))])]{
This macro provides syntactic sugar for building complex parser combinators from simpler combinators.

Each nonterminal definition expands into a parser-combinator, and the result of the form is the parser-combinator for the @racket[id] nonterminal, which must be defined as one of the @racket[nonterminal-id] forms.

The @racket[(! part ...)] syntax expands into a @racket[packrat-unless] form.

The @racket[(/ sequence ...)] syntax expands into a @racket[packrat-or] form.

The @racket[(? expr)] syntax expands into a @racket[packrat-check-pred] form.

Each @racket[nonterminal-id] expands into a @racket[results->result] formed from the body of the nonterminal definition.

The @racket[id :=] parts create a variable binding for @racket[id] in the @racket[body] expressions:
@itemize[
  @item{The @racket['kind] form expands into @racket[packrat-check-base].}
  @item{The @racket[|@|] binds @racket[id] to the @racket[parse-position] at that point in the input stream.}
  @item{The @racket[nonterminal-id] form expands into @racket[packrat-check] with the procedure associated with the nonterminal passed as the combinator argument.}
  @item{The @racket[?] expands into @racket[packrat-check-pred].}
]}

@section{Examples}

@(define packrat-eval
         (let ()
           (define e (make-base-eval))
           (e '(require racket/base))           
           (e '(require packrat))
           e))

Here is an example of a simple calculator.

@(examples
  #:eval packrat-eval
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
  (parse-result-semantic-value (calc g)))

See the tests source file for an example of a parser for a simplified Scheme grammar.

@section{Test suite}
@defmodule[packrat/test]

Requiring this module will run the test suite.
           
