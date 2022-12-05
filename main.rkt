#lang typed/racket

(provide Parser
         ParserResult
         Reply)

(define-type (Reply A) (U (Ok A) Error))
(define-type (ParserResult A) (U (Consumed A) (Empty A)))
(define-type (Parser A) (→ Input-Port (ParserResult A)))

(require typed/racket/unsafe)
(unsafe-require/typed
  parsack
  [#:struct Error ()]
  [#:struct (A) Ok ([parsed : A])]
  [#:struct (A) Consumed ([reply : (Reply A)])]
  [#:struct (A) Empty ([reply : (Reply A)])]

  [parse-result (∀ (A) (Parser A) (U Path-String Input-Port) → A)]
  [parse (∀ (A) (Parser A) (U Path-String Input-Port) → (ParserResult A))]

  [return (∀ (A) A → (Parser A))]
  [>>= (∀ (B A) (Parser A) (A → (Parser B)) → (Parser B))]
  [>> (∀ (A B) (Parser A) (Parser B) → (Parser B))]

  ;; [<or> (∀ (A B ...) (Parser A) (Parser B) ... B → (Parser (U A B ... B)))]
  [<or> (∀ (A B) (Parser A) (Parser B) → (Parser (U A B)))]
  ;; [choice (∀ (A B ...) (List (Parser A) (Parser B) ... B) → (Parser (U A B … B)))]
  [choice (∀ (A) (Listof (Parser A)) → (Parser A))]
  [<any> (∀ (A B) (Parser A) (Parser B) → (Parser (U A B)))]

  [many (∀ (A B) (->* ((Parser A))
                      (#:till (Parser B)
                       #:or ((Parser Null) (Parser (Listof A)) → (Parser (Listof A))))
                      (Parser (Listof A))))]
  [many1 (∀ (A B) (->* ((Parser A))
                       (#:till (Parser B)
                        #:or ((Parser Null) (Parser (Listof A)) → (Parser (Listof A))))
                       (Parser (Pairof A (Listof A)))))]

  [skipMany (∀ (A) (Parser A) → (Parser Null))]
  [skipMany1 (∀ (A) (Parser A) → (Parser Null))]

  [sepBy (∀ (A B) (Parser A) (Parser B) → (Parser (Listof A)))]
  [sepBy1 (∀ (A B) (Parser A) (Parser B) → (Parser (Pairof A (Listof A))))]
  [endBy (∀ (A B) (Parser A) (Parser B) → (Parser (Listof A)))]

  [between (∀ (A B C) (Parser B) (Parser A) (Parser C) → (Parser A))]

  [lookAhead (∀ (A) (Parser A) → (Parser A))]

  [<!> (∀ (Q P) (Parser P) (Parser Q) → (Parser Q))]
  [notFollowedBy (∀ (A) (Parser A) → (Parser Null))]

  [satisfy ((Char → Boolean) → (Parser Char))]
  [char (Char → (Parser Char))]
  [charAnyCase (Char → (Parser Char))]
  [noneOf (String → (Parser Char))]
  [oneOf (String → (Parser Char))]
  [oneOfStrings (String * → (Parser (Listof Char)))]
  [oneOfStringsAnyCase (String * → (Parser (Listof Char)))]
  [string (String → (Parser (Listof Char)))]

  [$letter (Parser Char)]
  [$digit (Parser Char)]
  [$alphaNum (Parser Char)]
  [$hexDigit (Parser Char)]
  [$space (Parser Char)]
  [$spaces (Parser Char)]
  [$anyChar (Parser Char)]
  [$newline (Parser Char)]
  [$tab (Parser Char)]
  [$eol (Parser (Listof Char))]
  [$eof (Parser Null)]
  [$identifier (Parser (Listof Char))]

  [setState (Symbol Any → (Parser Any))]
  [getState (Symbol → (Parser Any))]

  [try (∀ (A) (Parser A) → (Parser A))]

  [<?> (∀ (A) (Parser A) String → (Parser A))]

  [$err (Parser Any)]
  [err (String → (Parser Any))]

  [byte (Byte → (Parser Byte))]
  [bytestring (Bytes → (Parser (Listof Byte)))])
;; for some reason provide has to come after unsafe-require/typed.
;; unsafe-require/typed/provide doesn't work (doesn't accept polymorphic
;; struct syntax, like require/typed/provide). unsafe-provide doesn't work
;; (generic "bad-syntax" error).
(provide
  Ok Error Consumed Empty

  parse-result parse

  return >>= >>

  <or> choice <any>

  many many1

  skipMany skipMany1

  sepBy sepBy1 endBy

  between

  lookAhead

  <!> notFollowedBy

  satisfy char charAnyCase noneOf oneOf oneOfStrings oneOfStringsAnyCase string

  $letter $digit $alphaNum $hexDigit $space $spaces $anyChar $newline $tab $eol
  $eof $identifier

  try

  <?>

  $err err)

(provide withState)
(require (only-in parsack withState))

(provide parser-compose
         parser-seq)
(require syntax/parse/define
         (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax-parser parser-compose
  [(_ p:expr) (syntax/loc this-syntax p)]
  [(_ [x:id {~datum :} {~describe "type" t} {~datum <-} p:expr] e ...)
   (syntax/loc this-syntax (>>= p (λ ([x : t]) (parser-compose e ...))))]
  [(_ q:expr p:expr ...) (syntax/loc this-syntax (>> q (parser-compose p ...)))])

(define-syntax (parser-seq stx)
  (define-syntax-class clause
    #:attributes (x cp)
    [pattern ({~datum ~} p)
             #:attr x #f
             #:with cp #'p]
    [pattern (p {~datum :} {~describe "type" t})
             #:with x (generate-temporary)
             #:with cp #`(x : t <- p)])
  (syntax-parse stx
    [(_ p:clause ... {~optional {~seq #:combine-with combine:expr} #:defaults ([combine #'list])})
     (syntax/loc this-syntax (parser-compose p.cp ... (return (combine {~? p.x} ...))))]))

(module+ csv
  (define $oneCell (many (noneOf ",\n")))
  (define $cells : (Parser (Listof (Listof Char)))
    (parser-compose [c : (Listof Char) <- $oneCell]
                    [cs : (Listof (Listof Char)) <- $remainingCells]
                    (return (cons c cs))))
  (define $remainingCells (<or> (>> (char #\,) $cells) (return null)))
  (define $line
    (parser-seq [$cells : (Listof (Listof Char))]
                (~ $eol)
                #:combine-with values))
  (define $csv (many $line))
  (equal? (parse-result $csv "cell1,cell2\ncell3,cell4\n")
          '(((#\c #\e #\l #\l #\1) (#\c #\e #\l #\l #\2))
            ((#\c #\e #\l #\l #\3) (#\c #\e #\l #\l #\4)))))

;; Useless!
(: syntax/p (∀ (A) (Parser A) → (Parser (Syntaxof Any))))
(define (syntax/p p)
  (λ (in)
    (define-values (r c pos) (port-next-location in))
    (match (p in)
      [(Consumed (Ok #{a : A}))
       (define-values (r2 c2 pos2) (port-next-location in))
       (Consumed
         (Ok (datum->syntax #f a (list (object-name in) r c pos
                                       (and pos pos2 (- pos2 pos))))))]
      [(Empty (Ok #{a : A}))
       (define-values (r2 c2 pos2) (port-next-location in))
       (Empty
         (Ok (datum->syntax #f a (list (object-name in) r c pos
                                       (and pos pos2 (- pos2 pos))))))]
      [(Consumed (Error)) (Consumed (Error))]
      [(Empty (Error)) (Empty (Error))])))
