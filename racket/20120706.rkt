#lang racket

; Define an iterator for lists in Racket, such that calling it returns an element.
; When there are no more elements, it returns symbol <<end>>

; e.g.

;(define il (make-iter ’(1 2))) (il) returns 1
;(il) returns 2
;(il) returns <‌<end>‌>

(define (make-iter list)
  
  (let [[iter list]]
    (lambda () 
      (if (not (null? iter))
          (let [[listCopy  iter]]
            (set! iter (rest iter))
            (first listCopy)
            )
          '<<end>>
      )
    )
  )
)                   

(define il (make-iter '(1 2)))
(il)
(il)
(il)

; Define an analogous iterator for vectors.

(define (make-iter-vector vector)
  
  (let [[index 0]]
    (lambda ()
      (if (not (= index (vector-length vector)))
          (let [[oldIndex index]]
            (set! index (add1 index))
            (vector-ref vector oldIndex)
            )
          '<<end>>
          )
      )
    )
  )

(define iv (make-iter-vector (vector 5 6 )))
(iv)
(iv)
(iv)

; Define a new construct "for/in" which iterates on lists or vectors.
; e.g.
;(for x in ’(1 2 3) (display x)) shows 123
;(for x in ’#(c a s a) (display x)(display ".")) shows c.a.s.a.

(define-syntax for
    (syntax-rules
        
      (in)
      ((_ element in listOrVector func ...)
      (cond [(vector? listOrVector) (for-each (lambda (element) func ...))]
            [(list? listOrVector) (vector-for-each (lambda (element) func ...))]
      )
      )
    )
  )

(for x in ’(1 2 3) (display x)) shows 123
(for x in ’#(c a s a) (display x)(display ".")) shows c.a.s.a.