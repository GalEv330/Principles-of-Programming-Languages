#lang racket

(provide (all-defined-out))

(define id (lambda (x) x))
(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

;;; Q1.a
; Signature: compose(f g)
; Type: [T1 -> T2] * [T2 -> T3]  -> [T1->T3]
; Purpose: given two unary functions return their composition, in the same order left to right
; test: ((compose - sqrt) 16) ==> -4
;       ((compose not not) true)==> true
(define compose
  (lambda (f g)
    (lambda (x)
       (g (f x)))))


; Signature: pipe(lst-fun)
; Type: [[T1 -> T2],[T2 -> T3]...[Tn-1 -> Tn]]  -> [T1->Tn]
; Purpose: Returns the composition of a given list of unary functions. For (pipe (list f1 f2 ... fn)), returns the composition fn(....(f1(x)))
; test: ((pipe (list sqrt - - number?)) 16) ==> true
;       ((pipe (list sqrt - - number? not)) 16) ==> false
;       ((pipe (list sqrt add1 - )) 100) ==> -11
(define pipe
  (lambda (fs)  
    (if (empty? (cdr fs))
        (car fs)
        (compose (car fs) (pipe (cdr fs))))))

; Signature: compose$(f$ g$ cont)
; Type: [T1 * [T2->T3]] * [T3 * [T4->T5]] * [[T1 * [T2->T3]] -> T3] -> [T1 * [T4->T5]] -> T5
; Purpose: Composes two unary CPS functions.
(define compose$
  (lambda (f$ g$ cont)
    (cont (lambda (x next-cont)
                (f$ x (lambda (res)
                          (g$ res next-cont)))))))

; Signature: pipe$(lst-fun,cont)
;         [T1 * [T2->T3] ] -> T3,
;         [T3 * [T4 -> T5] ] -> T5,
;         ...,
;         [[T2n-1 * [T2n -> T2n+1]] -> T2n+1]
;        *
;       [[T1 * [T2n -> T2n+1]] -> T2n+1] -> 
;              [[T1 * [T2n+1 -> T2n+2]] -> T2n+2]
;      -> [T1 * [T2n+1 -> T2n+2]] -> T2n+2
; Purpose: Returns the composition of a given list of unry CPS functions. 
(define pipe$
  (lambda (fs cont)  
    (if (empty? (cdr fs))
        (cont (lambda (x next-cont)
                  ((car fs) x next-cont)))
        (pipe$ 
               (cdr fs)
               (lambda (res)
                   (compose$ (car fs) res cont))))))


;;; Q2a
; Signature: reduce1-lzl(reducer, init, lzl) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> -> T2
; Purpose: Returns the reduced value of the given lazy list
(define reduce1-lzl 
  (lambda (reducer init lzl)
   (if (empty-lzl? lzl)
        init
        (reduce1-lzl reducer (reducer init (head lzl)) (tail lzl)))
  )
)

;;; Q2b
; Signature: reduce2-lzl(reducer, init, lzl, n) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> * Number -> T2
; Purpose: Returns the reduced value of the first n items in the given lazy list
(define reduce2-lzl 
  (lambda (reducer init lzl n)
    (if (or (empty-lzl? lzl) (< n 1))
        init
        (reduce2-lzl reducer (reducer init (head lzl)) (tail lzl) (- n 1)))
  )
)  

;;; Q2c
; Signature: reduce3-lzl(reducer, init, lzl) 
; Type: [T2 * T1 -> T2] * T2 * LzL<T1> -> Lzl<T2>
; Purpose: Returns the reduced values of the given lazy list items as a lazy list
(define reduce3-lzl 
  (lambda (reducer init lzl)
    (if (empty-lzl? lzl)
        empty-lzl
        (let ((new_init (reducer init (head lzl))))
            (cons-lzl new_init (lambda () (reduce3-lzl reducer new_init (tail lzl))))))
  )
)

;;; Q2e
; Signature: integers-steps-from(from,step) 
; Type: Number * Number -> Lzl<Number>
; Purpose: Returns a list of integers from 'from' with 'steps' jumps
(define integers-steps-from
  (lambda (from step)
    (cons-lzl
        from
        (lambda() (integers-steps-from (+ from step) step)))
  )
)

;;; Q2f
; Signature: generate-pi-approximations() 
; Type: Empty -> Lzl<Number>
; Purpose: Returns the approximations of pi as a lazy list
(define generate-pi-approximations
  (lambda ()
    (map-lzl 
      (lambda (x) (* x 8)) 
      (reduce3-lzl 
        +
        0
        (map-lzl
          (lambda (x) (/ 1 (* x (+ x 2))))
          (integers-steps-from 1 4))))
   )
 )