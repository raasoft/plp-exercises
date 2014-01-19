#lang racket

; Implement an analogous of the numberList function of the previous exercise either in Scheme or in Ruby 
; (it is not necessary to use the same monadic construction).

; i.e.: 
; "that takes as input a list of numbers and returns a list of pairs of numbers (x, y), 
; where the first component is the same as the value x at the same position in the input list, 
; while y is the state when x was reached. The state is incremented by x, when x is reached.

; For instance, the procedure call (numberlist ’(1 3 22 -5) 0), where the second parameter is the initial state, 
; should return ’((1 . 1) (3 . 4) (22 . 26) (-5 . 21)).


(define (numberlist list k) 
  (define status k)
  (map (lambda (x)
         (begin 
                (set! status (+ status x)) 
                (cons x status)
         )
       )
       list
  )
)

(numberlist '(1 3 22 -5) 0)


; Other implementation

(define (numberlist lst state)
  (if (null? lst)
      ’()
      (let* ((x        (car lst))
             (newstate (+ x state)))
        (cons (cons x newstate)
             (numberlist (cdr lst) newstate)))))