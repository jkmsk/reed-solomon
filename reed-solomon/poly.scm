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
  #:export (poly-add poly-scale poly-mul))

;; poly-add : gf u v -> u + v, added coefficient-wise in Fq
(define (poly-add gf u v)
  (match (list u v)
    ((() ()) '())
    ((() _) (poly-add gf v (list 0)))
    ((_ ()) (poly-add gf u (list 0)))
    (((cu . cus) (cv . cvs))
     (cons (gf-add gf cu cv) (poly-add gf cus cvs)))))

;; poly-scale : gf scalar u -> scalar * u
;; Every coefficient of u multiplied by scalar in Fq.
(define (poly-scale gf scalar u)
  (if (zero? scalar)
      (list 0)
      (map (lambda (coeff) (gf-mul gf scalar coeff)) u)))

;; poly-shift : u -> X * u
(define (poly-shift u)
  (cons 0 u))

;; poly-mul : gf u v -> u * v
;; u = c0 + X*u' (head c0, tail u'), so u*v = c0*v + X*(u'*v):
;; scale v by the head of u, add the recursive product shifted by one.
(define (poly-mul gf u v)
  (match u
    (() '())
    ((c . cs) (poly-add gf (poly-scale gf c v) (poly-shift (poly-mul gf cs v))))))