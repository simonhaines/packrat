#lang racket/base
(require rackunit
         "parse.rkt"
         "combinator.rkt")

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

(define g (packrat-list-results '((num . 1) (+) (num . 2) (*) (num . 3))))
(check-equal? (parse-result-semantic-value (calc g)) 7)

;; Char ... -> Number
;; Given cs lexed as a number.
(define (number . cs)
  (string->number (apply string cs)))

;; Char ... -> Identifier
;; Given cs lexed as an identifier.
(define (identifier . cs)
  (string->symbol (apply string cs)))

;; Simplified Scheme lexer and parser.
(define lexer
  (parse <lexeme>
    (<lexeme>
     [(a := <number>)     (cons 'N a)]
     [(a := <identifier>) (cons 'I a)]
     [(a := <boolean>)    (cons 'B a)]
     [(a := <character>)  (cons 'C a)]
     [(a := <string>)     (cons 'S a)]
     [('#\() '(LP)]
     [('#\)) '(RP)]
     [('#\.) '(DOT)]
     [('#\# '#\() '(VP)]
     [('#\') '(Q)]
     [('#\`) '(QQ)]
     [('#\, '#\@) '(US)]
     [('#\,) '(UQ)]
     
     [(<atmosphere> a := <lexeme>) a])
    
    (<atmosphere>* 
     ((<atmosphere> <atmosphere>*) #f)
     (() #f))
    (<atmosphere> 
     ((<whitespace>) #f)
     ((<comment>) #f))    
    (<comment> 
     (('#\; <not-line-ending>*) #f))    
    (<not-line-ending>* 
     ((? (λ (x) (not (eqv? #\newline x))) <not-line-ending>*) #f)
     (() #f))    
    (<whitespace> 
     ((? char-whitespace?) #f))
    (<identifier> 
     ((a := <initial> b := <subsequent>*) (apply identifier a b))
     ((a := <peculiar-identifier>) a))    
    (<peculiar-identifier> 
     (('#\+) '+)
     (('#\-) '-))    
    (<initial> 
     ((a := <letter>) a)
     ((a := <special-initial>) a))    
    (<subsequent>* 
     ((s := <subsequent> s* := <subsequent>*) (cons s s*))
     (() '()))    
    (<subsequent> 
     ((a := <initial>) a)
     ((a := <digit>) a)
     ((a := <special-subsequent>) a))
    (<special-subsequent>
     (('#\+) #\+)
     (('#\-) #\-)
     (('#\.) #\.)
     (('#\@) #\@))    
    (<special-initial> 
     (('#\!) #\!)
     (('#\$) #\$)
     (('#\%) #\%)
     (('#\&) #\&)
     (('#\*) #\*)
     (('#\/) #\/)
     (('#\:) #\:)
     (('#\<) #\<)
     (('#\=) #\=)
     (('#\>) #\>)
     (('#\?) #\?)
     (('#\^) #\^)
     (('#\_) #\_)
     (('#\~) #\~))
    (<letter> 
     ((a := (? char-alphabetic?)) a))
    (<boolean> 
     (('#\# '#\t) #t)
     (('#\# '#\f) #f))    
    (<number> 
     ((f := <sign> ds := <digit>+) (apply number f ds)))
    (<character> (('#\# '#\\ c := <c>) c))
    (<c> 
     (('#\n '#\e '#\w '#\l '#\i '#\n '#\e) #\newline)
     (('#\s '#\p '#\a '#\c '#\e) #\space)
     ((c := (? char?)) c))    
    (<string> 
     (('#\" s := <string-elems>* '#\") (apply string s)))
    (<string-elems>* 
     ((s := <string-element> cs := <string-elems>*) (cons s cs))
     (() '()))    
    (<string-element> 
     ((c := (? (λ (x) (not (or (char=? x '#\\)
                               (char=? x '#\"))))))
      c)
     (('#\\ '#\\) '#\\)
     (('#\\ '#\") '#\"))    
    (<sign> 
     (('#\-) #\-)
     (('#\+) #\+)
     (() #\+))
    (<digit>+ 
     ((d := <digit> ds := <digit>+) (cons d ds))
     ((d := <digit> (! <digit>)) (list d)))
    (<digit> 
     (('#\0) #\0)
     (('#\1) #\1)
     (('#\2) #\2)
     (('#\3) #\3)
     (('#\4) #\4)
     (('#\5) #\5)
     (('#\6) #\6)
     (('#\7) #\7)
     (('#\8) #\8)
     (('#\9) #\9))))


(define parser    
  (parse <datum>                  
    (<datum> 
     [(a := <lexeme-datum>) a]     
     [(a := <compound-datum>) a])      
    (<lexeme-datum>
     [(a := 'N) a]
     [(a := 'I) a]
     [(a := 'B) a]       
     [(a := 'C) a]
     [(a := 'S) a])            
    (<compound-datum> 
     [(a := <list>) a]
     [(a := <vector>) a])
    (<list> 
     (('LP ds := <datum>* 'RP) ds)
     (('LP ds := <datum>+ 'DOT d := <datum> 'RP)
      (append ds d))
     ((a := <abbreviation>) a))
    (<abbreviation>
     ((p := <abbreviation-prefix> d := <datum>) (cons p (list d))))
    (<abbreviation-prefix>
     (('Q) 'quote)
     (('QQ) 'quasiquote)
     (('UQ) 'unquote)
     (('US) 'unquote-splicing))          
    (<vector> 
     (('VP ds := <datum>* 'RP) ds))
    (<datum>+ 
     ((d := <datum> ds := <datum>*) (cons d ds)))
    (<datum>* 
     ((d := <datum> ds := <datum>*) (cons d ds))
     (() '()))))


(define (lex s)
  (let loop ((rs (packrat-string-results "fn" s)))
    (let ((r (lexer rs)))
      (if (parse-result-successful? r)          
          (cons (parse-result-semantic-value r)
                (loop (parse-result-next r)))                 
          '()))))

(define (par l)
  (let ((r (parser (packrat-list-results l))))
    (if (parse-result-successful? r)          
        (parse-result-semantic-value r)
        (parse-result-error r))))

(define (read-string s)
  (par (lex s)))

(check-equal? (lex "x") '((I . x)))
(check-equal? (lex "-16") '((N . -16)))
(check-equal? (lex "(") '((LP)))
(check-equal? (lex "#\\(") '((C . #\()))
(check-equal? (lex "#t") '((B . #t)))
(check-equal? (lex "#f") '((B . #f)))
(check-equal? (lex "+") '((I . +)))
(check-equal? (lex "-") '((I . -)))
(check-equal? (lex "'") '((Q)))
(check-equal? (lex ",") '((UQ)))
(check-equal? (lex ",@") '((US)))
(check-equal? (lex "string-append0") '((I . string-append0)))
(check-equal? (parse-error? (read-string "")) #t)
(check-equal? (read-string "x") 'x)
(check-equal? (read-string "5") 5)
(check-equal? (read-string "-16") -16)
(check-equal? (read-string "#t") #t)
(check-equal? (read-string "#f") #f)
(check-equal? (read-string "#\\a") #\a)
(check-equal? (read-string "\"\"") "")
(check-equal? (read-string "\"hi\"") "hi")
(check-equal? (read-string "<hi>") '<hi>)
(check-equal? (read-string "string->number") 'string->number)
(check-equal? (read-string "()") '())
(check-equal? (read-string "(1 . 2)") '(1 . 2))
(check-equal? (read-string "(1 . 2 )") '(1 . 2))
(check-equal? (read-string "( 1 . 2)") '(1 . 2))
(check-equal? (read-string "(1 2)") '(1 2))
(check-equal? (read-string "(1 2 )") '(1 2))
(check-equal? (read-string "( 1 2)") '(1 2))
(check-equal? (read-string "'(1 2)") ''(1 2))
(check-equal? (read-string ",(1 2)") ',(1 2))
(check-equal? (read-string ",@(1 2)") ',@(1 2))
(check-equal? (read-string #<<***
(define (f x) (string-append "(f x)" x))
***
)
              '(define (f x) (string-append "(f x)" x)))
