
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define ones (lambda () (cons 1 ones)))

(define (sequence low high stride)
  (cond [(> low high) '()]
        [#t (cons low (sequence (+ low stride) high stride))]))


(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let* ([i (remainder n (length xs))]) (
                                                   cond [(= i 0) (car xs)]
                                                        [#t (car (list-tail xs i))]))]))

(define (stream-for-n-steps s n)
  (cond [(<= n 0) '()]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (- 0 x) (lambda () (f (+ x 1))))]
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cond [(> x 0) (cons "dan.jpg" (lambda () (f (- 0 x))))]
                      [#t (cons "dog.jpg" (lambda () (f (- 0 x))))]))])
    (lambda () (f 1))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda() (f stream))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))
