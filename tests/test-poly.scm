(use-modules (srfi srfi-64)
             (reed-solomon gf)
             (reed-solomon poly))

(test-begin "poly")

(define g (make-gf #b100011101))

(test-equal "add: both empty -> canonical 0-poly"
  (list #b0) (poly-add g '() '()))

(test-equal "add: identity element (u + 0-poly = u)"
  (list #b1 #b10 #b11) (poly-add g (list #b1 #b10 #b11) '()))

(test-equal "add: identity element, other side (0-poly + u = u)"
  (list #b1 #b10 #b11) (poly-add g '() (list #b1 #b10 #b11)))

(test-equal "add: involution (u + u = 0-poly)"
  (list #b0) (poly-add g (list #b1 #b10 #b11) (list #b1 #b10 #b11)))

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

(test-equal "scale: empty polynomial stays empty"
  '() (poly-scale g #b11 '()))

(test-equal "scale: absorbing scalar (#b0 * u = 0-poly)"
  (list #b0) (poly-scale g #b0 (list #b1 #b10 #b11)))

(test-equal "scale: identity scalar (#b1 * u = u)"
  (list #b1 #b10 #b11) (poly-scale g #b1 (list #b1 #b10 #b11)))

(test-equal "scale: every coefficient multiplied by the scalar"
  (list #b11 #b110 #b101) (poly-scale g #b11 (list #b1 #b10 #b11)))

(test-equal "mul: empty u -> empty"
  '() (poly-mul g '() (list #b100 #b101)))

(test-equal "mul: identity element (#b1 * v = v)"
  (list #b100 #b101) (poly-mul g (list #b1) (list #b100 #b101)))

(test-equal "mul: absorbing element (#b0 * v = 0-poly)"
  (list #b0) (poly-mul g (list #b0) (list #b100 #b101)))

(test-equal "mul: zero head coefficients are skipped correctly (#b11 X² * v)"
  (list #b0 #b0 #b1100 #b1111)
  (poly-mul g (list #b0 #b0 #b11) (list #b100 #b101)))

(test-equal "mul: (#b1+#b10 X+#b11 X²) * (#b100+#b101 X) = #b100+#b1101 X+#b110 X²+#b1111 X³"
  (list #b100 #b1101 #b110 #b1111)
  (poly-mul g (list #b1 #b10 #b11) (list #b100 #b101)))

(test-equal "mul: commutative"
  (poly-mul g (list #b1 #b10 #b11) (list #b100 #b101))
  (poly-mul g (list #b100 #b101) (list #b1 #b10 #b11)))

(define (divmod->list gf u v)
  (call-with-values (lambda () (poly-divmod gf u v)) list))

(test-equal "divmod: deg(u) < deg(v) -> quotient 0-poly, remainder u"
  (list (list #b0) (list #b1 #b10))
  (divmod->list g (list #b1 #b10) (list #b1 #b10 #b11)))

(test-equal "divmod: exact division, same degree (u = v) -> quotient #b1, remainder 0-poly"
  (list (list #b1) (list #b0))
  (divmod->list g (list #b1 #b10) (list #b1 #b10)))

(test-equal "divmod: exact division ((#b1+#b10 X+#b11 X²)*(#b100+#b101 X)) / (#b100+#b101 X) -> quotient #b1+#b10 X+#b11 X², remainder 0-poly"
  (list (list #b1 #b10 #b11) (list #b0))
  (divmod->list g (list #b100 #b1101 #b110 #b1111) (list #b100 #b101)))

(test-equal "divmod: non-zero remainder"
  (list (list #b1 #b10 #b11) (list #b1))
  (divmod->list g (list #b101 #b1101 #b110 #b1111) (list #b100 #b101)))

(test-equal "divmod: round-trip (q*p + r = u)"
  (list #b101 #b1101 #b110 #b1111)
  (call-with-values
      (lambda () (poly-divmod g (list #b101 #b1101 #b110 #b1111) (list #b100 #b101)))
    (lambda (q r) (poly-add g (poly-mul g q (list #b100 #b101)) r))))

(test-end "poly")
