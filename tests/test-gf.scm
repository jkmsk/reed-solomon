(use-modules (srfi srfi-64)
             (ice-9 match)
             (reed-solomon gf))

(test-begin "gf")

;; list of (primitive-poly deg card order), covering GF(2), GF(16) and GF(256)
(define field-cases
  (list (list #b11 1 2 1)
        (list #b10011 4 16 15)
        (list #b100011101 8 256 255)))

(for-each
 (match-lambda
   ((poly deg card order)
    (let ((gf (make-gf poly)))
      (test-assert (simple-format #f "make-gf ~a returns a procedure" poly)
        (procedure? gf))
      (test-equal (simple-format #f "deg: GF(~a) has degree ~a" card deg)
        deg (gf 'deg))
      (test-equal (simple-format #f "card: GF(~a) has ~a elements" card card)
        card (gf 'card))
      (test-equal (simple-format #f "order: GF(~a) Fq* has order ~a" card order)
        order (gf 'order))
      (test-equal (simple-format #f "order = card - 1, for GF(~a)" card)
        (- card 1) order))))
 field-cases)

;; list of (primitive-poly i a), meaning alpha^i = a for a canonical i (0 <= i < order)
(define exp-log-cases
  (list (list #b10011 0 #b1)
        (list #b10011 1 #b10)
        (list #b10011 4 #b11)
        (list #b100011101 0 #b1)
        (list #b100011101 1 #b10)
        (list #b100011101 8 #b11101)))

(for-each
 (match-lambda
   ((poly i a)
    (let ((gf (make-gf poly)))
      (test-equal (simple-format #f "exp: GF(~a) alpha^~a = ~a" (gf 'card) i a)
        a (gf-exp gf i))
      (test-equal (simple-format #f "log: GF(~a) log(~a) = ~a" (gf 'card) a i)
        i (gf-log gf a))
      (test-equal (simple-format #f "exp/log round-trip: log(exp(~a)) = ~a mod order" i i)
        (modulo i (gf 'order)) (gf-log gf (gf-exp gf i)))
      (test-equal (simple-format #f "log/exp round-trip: exp(log(~a)) = ~a" a a)
        a (gf-exp gf (gf-log gf a))))))
 exp-log-cases)

;; list of primitive-poly, one entry per distinct field
(define poly-cases
  (list #b10011
        #b100011101))

(for-each
 (lambda (poly)
   (let ((gf (make-gf poly)))
     (test-equal (simple-format #f "exp: GF(~a) alpha^order wraps to alpha^0 = 1" (gf 'card))
       #b1 (gf-exp gf (gf 'order)))
     (test-equal (simple-format #f "exp/log round-trip: log(exp(order)) = 0, for GF(~a)" (gf 'card))
       (modulo (gf 'order) (gf 'order)) (gf-log gf (gf-exp gf (gf 'order))))
     (test-equal (simple-format #f "exp: GF(~a) alpha^card wraps to alpha^1" (gf 'card))
       #b10 (gf-exp gf (gf 'card)))
     (test-equal (simple-format #f "exp/log round-trip: log(exp(card)) = 1, for GF(~a)" (gf 'card))
       (modulo (gf 'card) (gf 'order)) (gf-log gf (gf-exp gf (gf 'card))))
     (test-error (simple-format #f "log(0) is undefined and raises an error, for GF(~a)" (gf 'card))
       #t (gf-log gf #b0))
     (test-error (simple-format #f "inv(0) is undefined and raises an error, for GF(~a)" (gf 'card))
       #t (gf-inv gf #b0))
     (test-error (simple-format #f "an unknown message raises an error, for GF(~a)" (gf 'card))
       #t (gf 'unknown-op))))
 poly-cases)

;; list of (primitive-poly a b sum product), covering the additive
;; identity and multiplicative absorbing element, self-cancelling
;; addition, general cases (GF(16) and GF(256)), and a multiplication
;; that overflows and needs reduction.
(define add-mul-cases
  (list (list #b100011101 #b101 #b0 #b101 #b0)
        (list #b100011101 #b101 #b101 #b0 #b10001)
        (list #b100011101 #b11 #b101 #b110 #b1111)
        (list #b100011101 #b101 #b1 #b100 #b101)
        (list #b10011 #b10 #b11 #b1 #b110)
        (list #b10011 #b1000 #b100 #b1100 #b110)))

(for-each
 (match-lambda
   ((poly a b sum product)
    (let ((gf (make-gf poly)))
      (test-equal (simple-format #f "add: GF(~a) ~a + ~a = ~a" (gf 'card) a b sum)
        sum (gf-add gf a b))
      (test-equal (simple-format #f "mul: GF(~a) ~a * ~a = ~a" (gf 'card) a b product)
        product (gf-mul gf a b))
      (test-equal (simple-format #f "add commutative: ~a + ~a = ~a + ~a" a b b a)
        (gf-add gf a b) (gf-add gf b a))
      (test-equal (simple-format #f "mul commutative: ~a * ~a = ~a * ~a" a b b a)
        (gf-mul gf a b) (gf-mul gf b a)))))
 add-mul-cases)

;; list of (primitive-poly a inverse-of-a)
(define inv-cases
  (list (list #b100011101 #b1 #b1)
        (list #b100011101 #b10 #b10001110)
        (list #b10011 #b11 #b1110)))

(for-each
 (match-lambda
   ((poly a inverse)
    (let ((gf (make-gf poly)))
      (test-equal (simple-format #f "inv: GF(~a) inverse of ~a is ~a" (gf 'card) a inverse)
        inverse (gf-inv gf a))
      (test-equal (simple-format #f "inv: ~a * inv(~a) = 1" a a)
        #b1 (gf-mul gf a (gf-inv gf a))))))
 inv-cases)

(test-end "gf")
