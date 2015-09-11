#lang racket/base
(require srfi/13/string)

(define default-delimiters (list "," "\n"))

(define (adder numbers)
  (define (adder-iter acc numbers delimiters)
    (if (null? numbers)
      acc
      (let ([number-parser (number-parser numbers delimiters)])
        (adder-iter (+ acc (first-number number-parser))
                    (rest-numbers number-parser)
                    delimiters))))

  (if (zero? (string-length numbers))
    0 
    (let ([delimiter-parser (delimiter-parser numbers)])
      (adder-iter 0 
                  (numbers-without-delimiter-defintions delimiter-parser) 
                  (delimiters delimiter-parser)))))

(define (first-number number-parser)
  (car number-parser))

(define (rest-numbers number-parser)
  (cdr number-parser))

(define (numbers-without-delimiter-defintions delimiter-parser)
  (car delimiter-parser))

(define (delimiters delimiter-parser)
  (cadr delimiter-parser))

(define (delimiter-parser numbers)
  (define (custom-delimiter?) 
    (equal? (string-ref numbers 0) #\/))

  (define (first-custom-delimiter custom-delimiters)
    (substring custom-delimiters 0 (string-contains custom-delimiters "]")))

  (define (rest-custom-delimiters custom-delimiters)
    (let ([potential-delimiter 
            (substring custom-delimiters (string-contains custom-delimiters "]"))])
      (if (equal? (string-ref potential-delimiter 1) #\newline)
        null 
        (substring potential-delimiter 2))))

  (define (multi-length-custom-delimiters custom-delimiters)
    (if (null? custom-delimiters)
      null
      (cons (first-custom-delimiter custom-delimiters) 
            (multi-length-custom-delimiters (rest-custom-delimiters custom-delimiters)))))

  (define (custom-delimiters)
    (if (equal? (string-ref numbers 2) #\[)
      (append (multi-length-custom-delimiters (substring numbers 3)) default-delimiters)
      (cons (string (string-ref numbers 2)) default-delimiters)))

  (define (numbers-without-custom-delimiters)
    (substring numbers (+ (string-contains numbers "\n") 1)))

  (if (custom-delimiter?) 
    (list (numbers-without-custom-delimiters) (custom-delimiters))
    (list numbers default-delimiters)))

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
                            (string->number 
                              (substring numbers 0 (delimiter-index first-delimiter)))
                            (string->number numbers))])
        (cond [(> first-number 1000) 0]
              [(< first-number 0) (raise (make-exn:fail "No negatives."
                                                        (current-continuation-marks)))]
              [else first-number]))))

  (define (rest-numbers numbers)
    (let ([first-delimiter (first-delimiter numbers)])
      (if first-delimiter
        (substring numbers (+ (delimiter-index first-delimiter) 
                              (delimiter-length first-delimiter)))
        null)))

  (cons (first-number numbers) (rest-numbers numbers)))

(provide adder)
