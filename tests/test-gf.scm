(use-modules (srfi srfi-64)
             (reed-solomon gf))

(test-begin "gf")

(test-assert "make-gf returns a procedure"
  (procedure? (make-gf #b100011101)))

(test-equal "deg: GF(2)   X+1 has degree 1"
  1 ((make-gf #b11) 'deg))

(test-equal "deg: GF(16)  X^4+X+1 has degree 4"
  4 ((make-gf #b10011) 'deg))

(test-equal "deg: GF(256) X^8+X^4+X^3+X^2+1 has degree 8"
  8 ((make-gf #b100011101) 'deg))

(test-equal "card: GF(2)   has 2 elements"
  2 ((make-gf #b11) 'card))

(test-equal "card: GF(16)  has 16 elements"
  16 ((make-gf #b10011) 'card))

(test-equal "card: GF(256) has 256 elements"
  256 ((make-gf #b100011101) 'card))

(test-equal "order: GF(2)   Fq* has order 1"
  1 ((make-gf #b11) 'order))

(test-equal "order: GF(16)  Fq* has order 15"
  15 ((make-gf #b10011) 'order))

(test-equal "order: GF(256) Fq* has order 255"
  255 ((make-gf #b100011101) 'order))

(test-equal "order = card - 1"
  (- ((make-gf #b100011101) 'card) 1) ((make-gf #b100011101) 'order))

(test-equal "exp: GF(16)  alpha^0 = 1"
  1 (gf-exp (make-gf #b10011) 0))

(test-equal "exp: GF(16)  alpha^1 = X = 2"
  2 (gf-exp (make-gf #b10011) 1))

(test-equal "exp: GF(16)  alpha^4 = X+1 = 3 (from X^4+X+1 = 0)"
  3 (gf-exp (make-gf #b10011) 4))

(test-equal "exp: GF(16)  alpha^15 = 1 (order of Fq* is 15)"
  1 (gf-exp (make-gf #b10011) 15))

(test-equal "exp: GF(16)  alpha^16 = X = 2"
  2 (gf-exp (make-gf #b10011) 16))

(test-equal "exp: GF(256) alpha^0 = 1"
  1 (gf-exp (make-gf #b100011101) 0))

(test-equal "exp: GF(256) alpha^1 = X = 2"
  2 (gf-exp (make-gf #b100011101) 1))

(test-equal "exp: GF(256) alpha^8 = X^4+X^3+X^2+1 = 29 (from X^8+X^4+X^3+X^2+1 = 0)"
  29 (gf-exp (make-gf #b100011101) 8))

(test-equal "exp: GF(256) alpha^255 = 1 (order of Fq* is 255)"
  1 (gf-exp (make-gf #b100011101) 255))

(test-equal "exp: GF(256) alpha^256 wraps around to alpha^1"
  2 (gf-exp (make-gf #b100011101) 256))

(test-error "an unknown message raises an error"
  #t ((make-gf #b100011101) 'unknown-op))

(test-end "gf")
