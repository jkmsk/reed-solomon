(use-modules (srfi srfi-64)
             (reed-solomon gf)
             (reed-solomon poly))

(test-begin "poly")

(define g (make-gf #b100011101))

(test-equal "add: both empty -> empty"
  '() (poly-add g '() '()))

(test-equal "add: identity element (u + 0-poly = u)"
  (list #b1 #b10 #b11) (poly-add g (list #b1 #b10 #b11) '()))

(test-equal "add: identity element, other side (0-poly + u = u)"
  (list #b1 #b10 #b11) (poly-add g '() (list #b1 #b10 #b11)))

(test-equal "add: involution (u + u = 0-poly)"
  (list #b0 #b0 #b0) (poly-add g (list #b1 #b10 #b11) (list #b1 #b10 #b11)))

(test-equal "add: commutative"
  (poly-add g (list #b1 #b10 #b11) (list #b100 #b101))
  (poly-add g (list #b100 #b101) (list #b1 #b10 #b11)))

(test-equal "add: same length"
  (list #b101 #b111 #b11)
  (poly-add g (list #b1 #b10 #b11) (list #b100 #b101)))

(test-equal "add: shorter first operand is implicitly zero-padded"
  (list #b11 #b11 #b100)
  (poly-add g (list #b1) (list #b10 #b11 #b100)))

(test-equal "add: shorter second operand is implicitly zero-padded"
  (list #b11 #b11 #b100)
  (poly-add g (list #b10 #b11 #b100) (list #b1)))

(test-end "poly")
