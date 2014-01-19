#lang racket

;Scheme does not natively support matrices: there are just vectors.

;Of course, we can define a matrix as a vector of vectors, each having the same size.

; 1. Please define make-matrix, with three parameters: r, c, and fill. 
;    This procedure returns a matrix having r rows and c columns, with every cell initialized with the value fill.
; a) Hint: the standard library offers a procedure make-vector with two arguments: the first is the requested vector 
;    size, while the second is the content we want to initialize the vector with.

; 2. Define also the setter and the accessor, called matrix-set! and matrix-ref respectively, 
;    with the natural parameters.

(define (make-matrix r c fill) 
  ;(make-vector r (make-vector c fill)) NON FARE COSÍ! http://stackoverflow.com/questions/16641644/scheme-racket-vector-in-vector-tranformation
  ;Why? because you're copying the exact same vector in all off new-table's positions, so whenever you update one value, it'll change all of them at the same time. It's easy to see this:

  (define table (make-vector r (make-vector c fill))) ;Prima li creo tutti uguali
  (apply vector-map vector (vector->list table)) ;Poi prendo ciascuno e gli applico il costruttore: inutile ma efficace!
  
)


(define (matrix-set! matr r c newValue)
  (vector-set! (vector-ref matr r) c newValue)
  matr
)

(define (matrix-ref! matrix r c)   
  (vector-ref (vector-ref matrix r) c)
)

(define (build-list n proc)
 (define (adder)
    (let [[N -1]]
      (lambda (y)
        (set! N (add1 N))
        (proc N)
      )
    )
  )
  (define adderFunc (adder))
  (map adderFunc (vector->list (make-vector n 0)))
)

(define (build-vect n proc)
  (list->vector (build-list n proc))
)

(define (eye n)
  (define mat (make-matrix n n 0))
  (map  (lambda (x)
                      (matrix-set! mat (sub1 x) (sub1 x) 1)) (build-list n add1))
  mat
)

(eye 3)