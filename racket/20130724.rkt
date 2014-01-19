#lang racket

; Define a mutable variant of DList either in Scheme or in Ruby. 
; You are requested to define the DList datatype; Dcar, Dcdr, and Dcons, i.e. car, cdr, cons variants for DLists; 
; and DList=? that holds if both its arguments are equal.

; i.e. onsider an immutable doubly linked list datatype (DList), where each node has two pointers, 
; one to the previous node (prev) and another to the next node (next), together with its local datum. 
; There is a special value Nil, denoting the empty DList. A well-formed DList has always the first node 
; with prev set to Nil, and the last node with next set to Nil.

(struct DElem
  (prev
  datum
  next)
  #:mutable
)

(define Nil (DElem (#f #f #f)))
(define (Nil? x) (eq? x Nil))     ; For convenience

;Dcar restituisce il dato corrente
(define (Dcar x) 
  
  (if (Nil? x)
      (error "Dcar of Nil")
      (DList-datum x)
  )
)

;Dcdr restituisce il resto della lista
(define (Dcdr x) 
  
  (if (Nil? x)
      (error "Dcdr of Nil")
      ;(DList-next x) ;SBAGLIATO! Cosí ci portiamo appresso il nodo precedente, dobbiamo settare il puntatore a "null"!
      (let [(next DList-next x)]
        (DList (Nil (DList-datum next) (DList-next next)))
      )
  )
)

(define (Dcons node datum) ; Qui si intende che si mette in TESTA il datum 
  (if (Nil? node)
      (Dlist Nil datum Nil) ;Diventa la nuova testa in caso
      (let* [ ;Attenzione, deve essere MUTABILE quindi niente Dcdr per prendere velocemente il newCdr
            (newCdr  (DList (Nil (DList-datum node) (DList-next node)))) ; Il Nil é temporaneo, lo metteremo apposto dopo
            (newNode (DList (Nil datum newCdr)))
            ]
            (DList-set-prev! newCdr newNode)
       newNode
      ) 
  )
)

(define (Dlist=? list1 list2)
  (cond [(and (Nil? list1) (Nil? list2)) true]
        [(xor (Nil? list1) (Nil? list2)) true]
        [else (and (equal? (DList-datum list1) (DList-datum list2))
                   (DList=? (DList-next list1) (DList-next list2)))]
  )
)
      
      
