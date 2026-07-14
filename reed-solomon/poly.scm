;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Polynomials in Fq[X] (represented as lists of Fq integers)
;; Lowest degree first: [c0, c1, ..., ck] = c0 + c1*X + ... + ck*X^k
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon poly)
  #:use-module (ice-9 match)
  #:use-module (reed-solomon gf)
  #:export (poly-add))

;; poly-add : gf u v -> u + v, coefficient-wise in Fq (shorter side
;; implicitly zero-padded); builds the result directly in order, no
;; final reverse needed
(define (poly-add gf u v)
  (match (list u v)
    ((() ()) '())
    ((() _) (poly-add gf v (list 0)))
    ((_ ()) (poly-add gf u (list 0)))
    (((cu . cus) (cv . cvs))
     (cons (gf-add gf cu cv) (poly-add gf cus cvs)))))
