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

(test-error "an unknown message raises an error"
  #t ((make-gf #x11D) 'unknown-op))

(test-end "gf")
