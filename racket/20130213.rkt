#lang racket


; 1. Define a portion of a set library for Scheme, optimized for lookup (procedure in?) and insertion 
;    (procedure put!) of elements. Set elements may be numbers or symbols.
; 2. Define an intersection procedure for this library that can have any number of sets as 
;    arguments (at least one).

(define-syntax create-set
  
  (syntax-rules ()
    (
     (_)
     (make-hasheq)
    )
  )
)

(define-syntax put!
  (syntax-rules ()
    (
     (_ set key)
     (begin
       (cond [(number? key) (hash-set! set key true)] ;Come valore ci metto true, non mi interessa riempirlo davvero
             [(symbol? key) (hash-set! set key true)] ;Come valore ci metto true, non mi interessa riempirlo davvero
       )
       hash
     )
    )
  )
)


(define-syntax in?
  (syntax-rules ()
    (
     (_ set key)
     (cond [(false? (hash-ref set key false)) false]
           [else true]
     )
    )
  )
)

(define set1 (create-set))
(put! set1 'ciao)
(put! set1 'come)
(put! set1 'ti)
(put! set1 'chiami)
(put! set1 'ragazzo?)
(in? set1 'ciao)
(in? set1 'lollete)
(hash->list set1)

(define set2 (create-set))
(put! set2 'ciao)
(put! set2 'come)
(put! set2 'ragazzo)
(in? set2 'lollete)
(hash->list set2)

(define set3 (create-set))
(put! set3 'come)
(hash->list set3)

; Define an intersection procedure for this library that can have any number of sets as arguments (at least one).

(define (isect set1 set2) 
  (define intersection (create-set))
  (map (lambda (x) (put! intersection x)) (filter (lambda (x) (in? set2 x)) (hash-keys set1) ))
  intersection
)

(define-syntax intersect
  (syntax-rules () ;Base case
    (
     (_ set1)
     (set1)
    )
    
    (
     (_ set1 set2)
     (isect set1 set2)
    )
 
    (
     (_ set1 set2 ...)
     (foldl isect set1 (list set1 set2 ...))
    )
  )
)

(display "Intersection!")
(hash->list (intersect set1 set2 set3))