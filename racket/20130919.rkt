(define (subsets e)

  (let loop ((l e)

	(out '(())))

(if (null? l)

    out

    (loop (cdr l)

          (append out

(map (lambda (x) (cons (car l) x)) out))))))

(define (make-object list) 
  (let ((status list))
    
    
     (define member?
       (lambda (x)
         (if (member x status)
             true
             false
         )
       )
     )
    
    (define subsetsum
       (lambda (x)
         (> (length (filter (lambda (k) (eq? x k)) (map (lambda (y) (apply + y)) (subsets list)))) 0)
       )
     )
    
    
    
    
    
     (lambda (method . args) ; Dispatcher
       (apply
        (case method [(member?) member?]
                     [(subsetsum) subsetsum]
                     [else (error "Method missing")]
        )
        args
       ) 
     )
  )
)

(define ob (make-object '(3 2 7)))
(ob 'member? 3)
(ob 'subsetsum 9)
(ob 'subsetsum 5)
(ob 'subsetsum 12)
(ob 'subsetsum 3)
(ob 'subsetsum 4)
(ob 'subsetsum 111)