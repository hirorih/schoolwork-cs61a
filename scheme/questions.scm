(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (cond ((null? rests) nil)
    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))
  )
)

(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) '()))
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  (define (enumer n lst)
    (cond ((null? lst) nil)
          (else (cons (list n (car lst)) (enumer (+ n 1) (cdr lst))))
    )
  )
  (enumer 0 s)
)

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (add s v)
    (cond ((and (null? s) (null? v)) '())
          ((null? s) (cons (car v) (add s (cdr v))))
          (else (cons (car s) (add (cdr s) v)))
    )
)

(define (list-change total denoms)
  (cond((or (null? denoms) (< total 0)) '())
        ((> (car denoms) total) (list-change total (cdr denoms)))
        ((= (car denoms) total) (cons (list (car denoms)) (list-change total (cdr denoms))))
        (else (add (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )
)

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
            (cond
               ((eq? 'define form) (cons 'lambda (cons (map let-to-lambda params) (cons (map let-to-lambda body) nil))))
               (else (cons form (cons (map let-to-lambda params) (map let-to-lambda body))))
            )
          )
        )
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
               (define expression (zip values))
               (cons (cons 'lambda (cons (car expression) (map let-to-lambda body))) (map let-to-lambda (cadr expression)))
           ))
        (else
          (cons (car expr) (map let-to-lambda (cdr expr)))
         )
  )
)

