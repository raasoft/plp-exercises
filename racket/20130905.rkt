#lang racket

; Consider the parent-pointer implementation of general trees (PPT), where every node has local data (e.g. a name), and only one pointer, pointing to its parent. Of course, roots nodes do not point to other nodes.
; PPTs are generally implemented as arrays containing pairs (index of parent, node name), and one of such data structures can contain one or more different trees, e.g. using a Scheme-like syntax:

; #((? . R) (0 . A) (0 . B) (1 . C) (1 . D) (1 . E) (2 . F) (? . W) (7 . X) (7 . Y))

; where root nodes have first component “?”. Nodes are usually referenced through their index.
; PPTs are efficient for checking if two nodes belong to the same tree (we have just to check if the root is
; the same for both), so are often used to represent partitions.

; You are requested to implement a mutable version of PPTs in Scheme. In particular, you must:

; 1. Define the operation find-root, to obtain the root of the tree containing the given node.

; 2. Define the operation union!, that takes two nodes and, if they belong to different trees, merges the two
;    trees by making the root of the first node’s tree the parent of the root of the second node’s tree.


(define (find-parent node-index tree)
  (car (vector-ref tree node-index))
)

(define (find-root node-index tree) 
  (let ((p (find-parent node-index tree)))
    (if  (eq? p #f)
         node-index
         (find-root p tree)
    )
  )
)

(define (parent-set! node-index newParent-index tree) 
  (vector-set! tree node-index (cons newParent-index (cdr (vector-ref tree node-index))))
)

(define (union! node1 node2 tree)
  (let [(root1 (find-root node1))
        (root2 (find-root node2))]
    (unless (eq? root1 root2) (parent-set! root2 root1 tree))
        
    )
  )
       
;; an example
(define my-tree (vector
                 '(#f . R)
                 '(0 . A)
                 '(0 . B)
                 '(1 . C)
                 '(1 . D)
                 '(1 . E)
                 '(2 . F)
                 '(#f . W)
                 '(7 . X)
                 '(7 . Y)
                 '(7 . Z)))

(find-root 3 my-tree) ; 0
(find-root 9 my-tree) ; 7