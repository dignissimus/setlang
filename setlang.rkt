#lang racket

(struct parse-result (parsed remaining) #:inspector #f)
(struct variable-assignment-data (identifier expression) #:inspector #f)

(define (consume-whitespace str)
  (match str
    ["" ""]
    [else (if (char-whitespace? (string-ref str 0))
      (consume-whitespace (substring str 1)) str)]))

(define (parse-fail str) (parse-result 'nothing str))
(define no-parse (parse-fail 'no-parse))
(define (parse-nothing str) (parse-result (list) str))

(define (many parser)
  (lambda (str)
    (define result (parser str))
    (match result
      [(parse-result 'nothing remaining) (parse-nothing str)]
      [(parse-result parsed remaining) 
        (define next-result ((many parser) remaining))
        (match next-result
          [(parse-result 'nothing _) (parse-result (list parsed) remaining)]
          [(parse-result parsed_ remaining_) (parse-result (cons parsed parsed_) remaining_)])])))

(define (many1 parser)
  (lambda (str)
    (define result ((many parser) str))
    (match result
      [(parse-result '() _) no-parse]
      [(parse-result _ _) result])))

(define (parse-result-monoid-append left right)
  (match left [(parse-result 'nothing _) right] [else left]))

(define (apply-to x) (lambda (f) (f x)))

(define (any-of . parsers)
  (lambda (str)
    (foldl parse-result-monoid-append no-parse (map (apply-to str) parsers))))

(define (combine parsers)
  (lambda (str)
    (match parsers
      ['() (parse-nothing str)]
      [(cons p ps)
        (define result (p str))
        (match result
          [(parse-result 'nothing _) no-parse]
          [(parse-result parsed remaining)
            (define result_ ((combine ps) remaining))
            (match result_
              [(parse-result 'nothing _) no-parse]
              [(parse-result parsed_ remaining_) (parse-result (cons parsed parsed_) remaining_)])])])))

(define (chain parsers f)
  (lambda (str)
    (define result ((combine parsers) str))
    (match result
      [(parse-result 'nothing _) no-parse]
      [(parse-result parsed remaining)
       (parse-result (apply f parsed) remaining)])))

(define (parse-string literal)
  (lambda (str)
    (if (string-prefix? str literal)
      (parse-result literal (substring str (string-length literal))) no-parse)))

(define parse-character (compose parse-string string))

(define alphanum (apply any-of (map parse-character (string->list "abcdefghijklmnopqrstuvwxyz0123456789"))))
(define parse-identifier (many1 alphanum))
(define parse-symbol (combine (list (parse-string "'") (many1 alphanum))))
(define parse-equals (parse-string "="))
(define (leading-whitespace parser)
  (lambda (str)
    (define str_ (consume-whitespace str))
    (parser str_)))
(define (parse-set-body str) 
  (define parser_ (many (leading-whitespace parse-expression)))
  (parser_ str))
(define parse-set
  (chain (list (parse-string "{") parse-set-body (parse-string "}"))
      (lambda (lb set-body rb) set-body)))
(define parse-expression (any-of parse-identifier parse-symbol parse-set))

(define parse-variable-assignment
  (chain (list parse-identifier (leading-whitespace parse-equals) (leading-whitespace parse-expression))
    (lambda (identifier equals expression) (variable-assignment-data (string-join identifier "") expression))))

(define top-level-statement (leading-whitespace (any-of parse-variable-assignment)))
(define parse-program (many1 top-level-statement))