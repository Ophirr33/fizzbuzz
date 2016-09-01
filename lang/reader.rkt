#lang racket
(require syntax/strip-context)

(provide (rename-out [fizz-read read]
                     [fizz-read-syntax read-syntax]))

(define (rule->function sym)
  (let* ([split (string-split (symbol->string sym) "=")]
         [word (car split)]
         [num (string->number (cadr split))])
    (lambda (n) (if (= 0 (modulo n num)) word #f))))

(define (range-maker sym)
  (let* ([split (string-split (symbol->string sym) "...")]
         [num1 (string->number (car split))]
         [num2 (string->number (cadr split))])
    (in-range num1 (add1 num2))))

(define (read-all in)
  (let ([item (read in)])
    (if (eof-object? item)
        empty
        (cons item (read-all in)))))

(define (fizz-read in)
  (syntax->datum (fizz-read-syntax #f in)))

(define (fizz-read-syntax src in)
  (with-syntax ([range (range-maker (read in))]
                [rules (map rule->function (read-all in))])
    (strip-context
     #'(module anything racket
         (for [(i 'range)]
           (define (buzzes) (map (lambda (x) (x i)) 'rules))
           (if (ormap identity (buzzes))
               (map display (filter identity (buzzes)))
               (display i))
           (display "\n"))))))

