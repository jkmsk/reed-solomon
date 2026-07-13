(use-modules (srfi srfi-64)
             (reed-solomon gf))

(test-begin "gf")

(test-assert "make-gf returns a procedure"
  (procedure? (make-gf #x11D)))

(test-equal "deg: GF(2)   X+1 (11) has degree 1"
  1 ((make-gf #x3) 'deg))

(test-equal "deg: GF(16)  X^4+X+1 (10011) has degree 4"
  4 ((make-gf #x13) 'deg))

(test-equal "deg: GF(256) X^8+X^4+X^3+X^2+1 (100011101) has degree 8"
  8 ((make-gf #x11D) 'deg))

(test-equal "card: GF(2)   has 2 elements"
  2 ((make-gf #x3) 'card))

(test-equal "card: GF(16)  has 16 elements"
  16 ((make-gf #x13) 'card))

(test-equal "card: GF(256) has 256 elements"
  256 ((make-gf #x11D) 'card))

(test-equal "order: GF(2)   Fq* has order 1"
  1 ((make-gf #x3) 'order))

(test-equal "order: GF(16)  Fq* has order 15"
  15 ((make-gf #x13) 'order))

(test-equal "order: GF(256) Fq* has order 255"
  255 ((make-gf #x11D) 'order))

(test-equal "order = card - 1"
  (- ((make-gf #x11D) 'card) 1) ((make-gf #x11D) 'order))

(test-error "an unknown message raises an error"
  #t ((make-gf #x11D) 'unknown-op))

(test-end "gf")
