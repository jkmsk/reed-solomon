(use-modules (srfi srfi-64)
             (ice-9 match)
             (reed-solomon gf)
             (reed-solomon poly))

(test-begin "poly")

(define g (make-gf #b100011101))

;; list of (u expected), covering an empty list, an all-zero list of
;; several superfluous zeros, a low-order zero, several superfluous
;; high-order zeros, and an already-canonical polynomial.
(define normalize-cases
  (list (list '() (list #b0))
        (list (list #b0) (list #b0))
        (list (list #b0 #b0 #b0) (list #b0))
        (list (list #b0 #b1) (list #b0 #b1))
        (list (list #b1 #b10 #b0 #b0) (list #b1 #b10))
        (list (list #b1 #b10 #b11) (list #b1 #b10 #b11))))

(for-each
 (match-lambda
   ((u expected)
    (test-equal (simple-format #f "normalize: ~a -> ~a" u expected)
      expected (poly-normalize u))))
 normalize-cases)

;; list of (args result), covering shift by one and shift by i on a
;; general polynomial, plus one case per arity.
(define shift-cases
  (list (list (list (list #b1 #b10 #b11))
              (list #b0 #b1 #b10 #b11))
        (list (list '())
              (list #b0))
        (list (list 2 (list #b0))
              (list #b0))
        (list (list 0 (list #b1 #b10 #b11))
              (list #b1 #b10 #b11))
        (list (list 3 (list #b1 #b10 #b11))
              (list #b0 #b0 #b0 #b1 #b10 #b11))
        (list (list 1 (list #b1 #b10 #b11))
              (list #b0 #b1 #b10 #b11))))

(for-each
 (match-lambda
   ((args result)
    (test-equal (simple-format #f "shift: (poly-shift ~a) = ~a" args result)
      result (apply poly-shift args))))
 shift-cases)

;; list of (u v sum product), covering empty, zero, identity,
;; self, mixed-length and general polynomials.
(define poly-cases
  (list (list '() '()
              (list #b0) (list #b0))
        (list '() (list #b100 #b101)
              (list #b100 #b101) (list #b0))
        (list (list #b100 #b101) '()
              (list #b100 #b101) (list #b0))
        (list (list #b0) (list #b0)
              (list #b0) (list #b0))
        (list (list #b0) (list #b1 #b10 #b11)
              (list #b1 #b10 #b11) (list #b0))
        (list (list #b1) (list #b100 #b101)
              (list #b101 #b101) (list #b100 #b101))
        (list (list #b1 #b10 #b11) (list #b1 #b10 #b11)
              (list #b0) (list #b1 #b0 #b100 #b0 #b101))
        (list (list #b0 #b0 #b11) (list #b100 #b101)
              (list #b100 #b101 #b11) (list #b0 #b0 #b1100 #b1111))
        (list (list #b1 #b10 #b11) (list #b100 #b101)
              (list #b101 #b111 #b11) (list #b100 #b1101 #b110 #b1111))
        (list (list #b1) (list #b10 #b11 #b100)
              (list #b11 #b11 #b100) (list #b10 #b11 #b100))))

(for-each
 (match-lambda
   ((u v sum product)
    (test-equal (simple-format #f "add: ~a + ~a = ~a" u v sum)
      sum (poly-add g u v))
    (test-equal (simple-format #f "mul: ~a * ~a = ~a" u v product)
      product (poly-mul g u v))
    (test-equal (simple-format #f "add commutative: ~a + ~a = ~a + ~a" u v v u)
      (poly-add g u v) (poly-add g v u))
    (test-equal (simple-format #f "mul commutative: ~a * ~a = ~a * ~a" u v v u)
      (poly-mul g u v) (poly-mul g v u))))
 poly-cases)

;; list of (scalar u result), covering the absorbing scalar,
;; the identity scalar, an empty polynomial, and the combination
;; of both, a general case, and a non-zero scalar times the zero
;; polynomial.
(define scale-cases
  (list (list #b0 (list #b1 #b10 #b11)
              (list #b0))
        (list #b1 (list #b1 #b10 #b11)
              (list #b1 #b10 #b11))
        (list #b11 '()
              (list #b0))
        (list #b0 '()
              (list #b0))
        (list #b11 (list #b1 #b10 #b11)
              (list #b11 #b110 #b101))
        (list #b11 (list #b0)
              (list #b0))))

(for-each
 (match-lambda
   ((scalar u result)
    (test-equal (simple-format #f "scale: ~a * ~a = ~a" scalar u result)
      result (poly-scale g scalar u))))
 scale-cases)

(define (divmod->list gf u v)
  (call-with-values (lambda () (poly-divmod gf u v)) list))

;; list of (u v quotient remainder), covering deg(u) < deg(v), exact
;; division at the same degree, exact division in the general case, a
;; non-zero remainder, and non-canonical u/v.
(define divmod-cases
  (list (list (list #b1 #b10) (list #b1 #b10 #b11)
              (list #b0) (list #b1 #b10))
        (list (list #b1 #b10) (list #b1 #b10)
              (list #b1) (list #b0))
        (list (list #b100 #b1101 #b110 #b1111) (list #b100 #b101)
              (list #b1 #b10 #b11) (list #b0))
        (list (list #b101 #b1101 #b110 #b1111) (list #b100 #b101)
              (list #b1 #b10 #b11) (list #b1))
        (list (list #b100 #b1101 #b110 #b1111) (list #b100 #b101 #b0)
              (list #b1 #b10 #b11) (list #b0))
        (list (list #b100 #b1101 #b110 #b1111 #b0) (list #b100 #b101)
              (list #b1 #b10 #b11) (list #b0))))

(for-each
 (match-lambda
   ((u v quotient remainder)
    (test-equal (simple-format #f "divmod: ~a / ~a = ~a, ~a" u v quotient remainder)
      (list quotient remainder) (divmod->list g u v))
    (test-equal (simple-format #f "divmod round-trip: q*v + r = ~a" u)
      (poly-normalize u) (poly-add g (poly-mul g quotient v) remainder))
    (test-equal (simple-format #f "mod: ~a mod ~a = ~a (matches divmod's remainder)" u v remainder)
      remainder (poly-mod g u v))))
 divmod-cases)

(test-error "divmod: dividing by the zero polynomial raises an error"
  #t (poly-divmod g (list #b1 #b10 #b11) (list #b0)))

(test-error "mod: dividing by the zero polynomial raises an error"
  #t (poly-mod g (list #b1 #b10 #b11) (list #b0)))

;; direct-eval : reference implementation (direct sum of c_i *
;; point^i), used to independently cross-check poly-eval
(define (direct-eval u point)
  (let loop ((cs u) (point-i #b1) (acc #b0))
    (if (null? cs)
        acc
        (loop (cdr cs) (gf-mul g point-i point) (gf-add g acc (gf-mul g (car cs) point-i))))))

;; list of (u point result), covering the 0-poly, a constant
;; polynomial, evaluation at 0, a general case, and an empty
;; polynomial.
(define eval-cases
  (list (list (list #b0) #b11 #b0)
        (list (list #b101) #b11 #b101)
        (list (list #b1 #b10 #b11) #b0 #b1)
        (list (list #b1 #b10 #b11) #b100 #b111001)
        (list '() #b11 #b0)))

(for-each
 (match-lambda
   ((u point result)
    (test-equal (simple-format #f "eval: ~a at point=~a = ~a" u point result)
      result (poly-eval g u point))
    (test-equal (simple-format #f "eval matches direct sum: ~a at point=~a" u point)
      (direct-eval u point) (poly-eval g u point))))
 eval-cases)

;; list of (u expected-deriv), covering a general polynomial with a mix
;; of odd/even-degree terms, a degree drop caused by an even-degree
;; leading term, a constant, the zero-polynomial, and non-canonical input.
(define deriv-cases
  (list (list (list #b1 #b10 #b11 #b100 #b101)
              (list #b10 #b0 #b100))
        (list (list #b11 #b101 #b100)
              (list #b101))
        (list (list #b111)
              (list #b0))
        (list (list #b0)
              (list #b0))
        (list '()
              (list #b0))
        (list (list #b11 #b101 #b100 #b0 #b0)
              (list #b101))))

(for-each
 (match-lambda
   ((u expected)
    (test-equal (simple-format #f "deriv: d/dX(~a) = ~a" u expected)
      expected (poly-deriv u))))
 deriv-cases)

(test-end "poly")
