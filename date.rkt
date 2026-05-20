#lang racket

;; a simple data representation of dates

(provide #; Date today my-date?)

#; {type Date = [List Natural Natural Natural]}

(define (my-date? x)
  (match x
    [(list (? natural? year) (? natural? month) (? natural? day))
     #true]
    [_
     (error 'my-date "date expected, found ~a" x)]))
  
#; { -> Date}
(define (today)
  (let ([d (seconds->date (current-seconds))])
    (list (date-year d) (date-month d) (date-day d))))

(module+ test
  (require rackunit)

  (check-true (my-date? (today)))
  (check-exn #px"date expected" (λ () (my-date? '(1 2 3 4)))))