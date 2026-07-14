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

(test-equal "exp: GF(16)  alpha⁰ = 1"
  #b1 (gf-exp (make-gf #b10011) 0))

(test-equal "exp: GF(16)  alpha¹ = X"
  #b10 (gf-exp (make-gf #b10011) 1))

(test-equal "exp: GF(16)  alpha⁴ = X+1 (from X⁴+X+1 = 0)"
  #b11 (gf-exp (make-gf #b10011) 4))

(test-equal "exp: GF(16)  alpha^15 = 1 (order of Fq* is 15)"
  #b1 (gf-exp (make-gf #b10011) 15))

(test-equal "exp: GF(16)  alpha^16 = X"
  #b10 (gf-exp (make-gf #b10011) 16))

(test-equal "exp: GF(256) alpha⁰ = 1"
  #b1 (gf-exp (make-gf #b100011101) 0))

(test-equal "exp: GF(256) alpha¹ = X"
  #b10 (gf-exp (make-gf #b100011101) 1))

(test-equal "exp: GF(256) alpha⁸ = X⁴+X³+X²+1 (from X⁸+X⁴+X³+X²+1 = 0)"
  #b11101 (gf-exp (make-gf #b100011101) 8))

(test-equal "exp: GF(256) alpha^255 = 1 (order of Fq* is 255)"
  #b1 (gf-exp (make-gf #b100011101) 255))

(test-equal "exp: GF(256) alpha^256 wraps around to alpha^1"
  #b10 (gf-exp (make-gf #b100011101) 256))

(test-equal "log: GF(16)  log(1) = 0 (alpha⁰ = 1)"
  0 (gf-log (make-gf #b10011) #b1))

(test-equal "log: GF(16)  log(X) = 1 (alpha¹ = X)"
  1 (gf-log (make-gf #b10011) #b10))

(test-equal "log: GF(16)  log(X+1) = 4 (alpha⁴ = X+1)"
  4 (gf-log (make-gf #b10011) #b11))

(test-equal "log: GF(256) log(1) = 0 (alpha⁰ = 1)"
  0 (gf-log (make-gf #b100011101) #b1))

(test-equal "log: GF(256) log(X) = 1 (alpha¹ = X)"
  1 (gf-log (make-gf #b100011101) #b10))

(test-equal "log: GF(256) log(X⁴+X³+X²+1) = 8 (alpha⁸ = X⁴+X³+X²+1)"
  8 (gf-log (make-gf #b100011101) #b11101))

(test-equal "log is the inverse of exp"
  8 (gf-log (make-gf #b100011101) (gf-exp (make-gf #b100011101) 8)))

(test-error "log(0) is undefined and raises an error"
  #t (gf-log (make-gf #b100011101) #b0))

(test-equal "add: identity element (x + 0 = x)"
  #b101 (gf-add (make-gf #b100011101) #b101 #b0))

(test-equal "add: involution (x + x = 0)"
  #b0 (gf-add (make-gf #b100011101) #b101 #b101))

(test-equal "add: commutative"
  (gf-add (make-gf #b100011101) #b11 #b101) (gf-add (make-gf #b100011101) #b101 #b11))

(test-equal "add: GF(256) (X+1) + (X²+1) = X²+X"
  #b110 (gf-add (make-gf #b100011101) #b11 #b101))

(test-equal "mul: absorbing element (x * 0 = 0)"
  #b0 (gf-mul (make-gf #b100011101) #b101 #b0))

(test-equal "mul: absorbing element (0 * y = 0)"
  #b0 (gf-mul (make-gf #b100011101) #b0 #b101))

(test-equal "mul: identity element (x * 1 = x)"
  #b101 (gf-mul (make-gf #b100011101) #b101 #b1))

(test-equal "mul: commutative"
  (gf-mul (make-gf #b100011101) #b11 #b101) (gf-mul (make-gf #b100011101) #b101 #b11))

(test-equal "mul: GF(16)  X * (X+1) = X²+X"
  #b110 (gf-mul (make-gf #b10011) #b10 #b11))

(test-equal "mul: GF(256) (X+1) * (X²+1) = X³+X²+X+1"
  #b1111 (gf-mul (make-gf #b100011101) #b11 #b101))

(test-equal "mul: GF(16)   X³ * X² = X⁵ ≡ X²+X (mod X⁴+X+1) since X⁴ ≡ X+1"
  #b110 (gf-mul (make-gf #b10011) #b1000 #b100))

(test-equal "inv: inverse of 1 is 1"
  #b1 (gf-inv (make-gf #b100011101) #b1))

(test-equal "inv: GF(256) inverse of X is X⁷+X³+X²+X"
  #b10001110 (gf-inv (make-gf #b100011101) #b10))

(test-equal "inv: GF(256) x * inv(x) = 1"
  #b1 (gf-mul (make-gf #b100011101) #b101 (gf-inv (make-gf #b100011101) #b101)))

(test-equal "inv: GF(16)  x * inv(x) = 1"
  #b1 (gf-mul (make-gf #b10011) #b11 (gf-inv (make-gf #b10011) #b11)))

(test-error "inv(0) is undefined and raises an error"
  #t (gf-inv (make-gf #b100011101) #b0))

(test-error "an unknown message raises an error"
  #t ((make-gf #b100011101) 'unknown-op))

(test-end "gf")
