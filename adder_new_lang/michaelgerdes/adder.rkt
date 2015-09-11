#lang racket/base
(require srfi/13/string)

(define (adder numbers)
  (define delimiters (list "," "\n"))

  (define (custom-delimiter?) 
    (equal? (string-ref numbers 0) #\/))

  (define (first-custom-delimiter custom-delimiters)
    (substring custom-delimiters 0 (string-contains custom-delimiters "]")))

  (define (rest-custom-delimiters custom-delimiters)
    (let ([potential-delimiter (substring custom-delimiters (string-contains custom-delimiters "]"))])
      (if (equal? (string-ref potential-delimiter 1) #\newline)
        null 
        (substring potential-delimiter 2))))

  (define (add-multi-length-custom-delimiters custom-delimiters)
    (if (null? custom-delimiters)
      null
      (begin
        (set! delimiters (cons (first-custom-delimiter custom-delimiters) 
                               delimiters))
        (add-multi-length-custom-delimiters (rest-custom-delimiters custom-delimiters)))))

  (define (add-custom-delimiters)
    (if (equal? (string-ref numbers 2) #\[)
      (add-multi-length-custom-delimiters (substring numbers 3))
      (set! delimiters (cons (string (string-ref numbers 2)) delimiters))))

  (define (numbers-without-custom-delimiters)
    (substring numbers (+ (string-contains numbers "\n") 1)))

  (define (adder-iter acc numbers)
    (if (null? numbers)
      acc
      (let ([number-parser (number-parser numbers delimiters)])
        (adder-iter (+ acc (car number-parser))
                    (cdr number-parser)))))

  (if (zero? (string-length numbers))
    0 
    (if (custom-delimiter?)
      (begin
        (add-custom-delimiters) 
        (adder-iter 0 (numbers-without-custom-delimiters)))
      (adder-iter 0 numbers))))

(define (number-parser numbers delimiters)
  (define (delimiter-index delimiter)
    (car delimiter))

  (define (delimiter-length delimiter)
    (cadr delimiter))

  (define (min-indexed-delimiter delimiters)
    (if (null? delimiters)
      #f
      (foldl (lambda (smallest-delimiter delimiter)
               (if (< (delimiter-index delimiter) (delimiter-index smallest-delimiter))
                 delimiter
                 smallest-delimiter))
             (car delimiters)
             (cdr delimiters))))


  (define (first-delimiter numbers)
    (min-indexed-delimiter 
      (filter (lambda (delimiter)
                (not (equal? #f delimiter)))  
              (map (lambda (delimiter)
                     (let ([index (string-contains numbers delimiter)])
                       (if index 
                         (list index (string-length delimiter)) 
                         #f))) 
                   delimiters))))

  (define (first-number numbers)
    (let ([first-delimiter (first-delimiter numbers)])
      (let ([first-number (if first-delimiter
                            (string->number (substring numbers 0 (delimiter-index first-delimiter)))
                            (string->number numbers))])
        (if (> first-number 1000)
          0
          first-number))))

  (define (rest-numbers numbers)
    (let ([first-delimiter (first-delimiter numbers)])
      (if first-delimiter
        (substring numbers (+ (delimiter-index first-delimiter) 
                              (delimiter-length first-delimiter)))
        null)))

  (cons (first-number numbers) (rest-numbers numbers)))

(provide adder)
