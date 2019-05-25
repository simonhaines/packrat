#lang scheme
(require "parse.ss")
(require "combinator.ss")
(require test-engine/scheme-tests)

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
(check-expect (parse-result-semantic-value (calc g)) 7)

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
     (() empty))    
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
     (() empty))    
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
     (() empty))))


(define (lex s)
  (let loop ((rs (packrat-string-results "fn" s)))
    (let ((r (lexer rs)))
      (if (parse-result-successful? r)          
          (cons (parse-result-semantic-value r)
                (loop (parse-result-next r)))                 
          empty))))

(define (par l)
  (let ((r (parser (packrat-list-results l))))
    (if (parse-result-successful? r)          
        (parse-result-semantic-value r)
        (parse-result-error r))))

(define (read-string s)
  (par (lex s)))

(check-expect (lex "x") '((I . x)))
(check-expect (lex "-16") '((N . -16)))
(check-expect (lex "(") '((LP)))
(check-expect (lex "#\\(") '((C . #\()))
(check-expect (lex "#t") '((B . #t)))
(check-expect (lex "#f") '((B . #f)))
(check-expect (lex "+") '((I . +)))
(check-expect (lex "-") '((I . -)))
(check-expect (lex "'") '((Q)))
(check-expect (lex ",") '((UQ)))
(check-expect (lex ",@") '((US)))
(check-expect (lex "string-append0") '((I . string-append0)))
(check-expect (parse-error? (read-string "")) #t)
(check-expect (read-string "x") 'x)
(check-expect (read-string "5") 5)
(check-expect (read-string "-16") -16)
(check-expect (read-string "#t") #t)
(check-expect (read-string "#f") #f)
(check-expect (read-string "#\\a") #\a)
(check-expect (read-string "\"\"") "")
(check-expect (read-string "\"hi\"") "hi")
(check-expect (read-string "<hi>") '<hi>)
(check-expect (read-string "string->number") 'string->number)
(check-expect (read-string "()") '())
(check-expect (read-string "(1 . 2)") '(1 . 2))
(check-expect (read-string "(1 . 2 )") '(1 . 2))
(check-expect (read-string "( 1 . 2)") '(1 . 2))
(check-expect (read-string "(1 2)") '(1 2))
(check-expect (read-string "(1 2 )") '(1 2))
(check-expect (read-string "( 1 2)") '(1 2))
(check-expect (read-string "'(1 2)") ''(1 2))
(check-expect (read-string ",(1 2)") ',(1 2))
(check-expect (read-string ",@(1 2)") ',@(1 2))
(check-expect (read-string #<<***
(define (f x) (string-append "(f x)" x))
***
)
              '(define (f x) (string-append "(f x)" x)))

(test)
