#lang racket

; Consider the proto-oo system presented in class.
; Please implement a chat command for performing a "dialogue" between two objects,
; say o1 and o2.
; For example the command (chat 01 o2 x m1 m2 ...) must send the message m1 with
; argument x to o1, obtaining a result. Such result is then used as argument of method m2 of o2, and so on.
; This means that, using a C++-like notation, it performs the following calls: ...o1.m3(o2.m2(o1.m1(x)))

(define O1 "01")
(define O2 "02")
(define (apply arg func obj) (string-append obj "." func "(" arg ")"))


(define-syntax chat
    (syntax-rules()
      
      [
       (chat o1 o2 x m1)
       (apply x m1 o1)
      ]
      
      [(chat o1 o2 x m1 m2 ...)
       (chat o2 o1 (apply x m1 o1) m2 ... )
       ]
    )   
)

(display (chat O1 O2 "x" "m1" "m2" "m3" "m4"))