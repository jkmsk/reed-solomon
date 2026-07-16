(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (reed-solomon gf)
             (reed-solomon poly)
             (reed-solomon core))

(test-begin "core")

(define g (make-gf #b10011))

(test-equal "generator: GF(16) RS(15,11), value (X+alpha)(X+alpha²)(X+alpha³)(X+alpha⁴)"
  (list #b111 #b1000 #b1100 #b1101 #b1)
  (generator g 15 11))

(test-equal "generator: GF(16) RS(15,13), value (X+alpha)(X+alpha²)"
  (list #b1000 #b110 #b1)
  (generator g 15 13))

(test-equal "generator: degree is n-k"
  4 (poly-degree (generator g 15 11)))

(test-equal "generator: is monic"
  #b1 (last (generator g 15 11)))

(test-assert "generator: alpha¹..alpha^(n-k) are roots"
  (let ((gen (generator g 15 11)))
    (every (lambda (i) (zero? (poly-eval g gen (gf-exp g i))))
           (iota 4 1))))

(test-end "core")
