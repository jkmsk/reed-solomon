;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Polynomials in Fq[X] (represented as lists of Fq integers)
;; Lowest degree first: [c0, c1, ..., ck] = c0 + c1*X + ... + ck*X^k
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon poly)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (reed-solomon gf)
  #:export (poly-add poly-scale poly-mul poly-divmod))

;; poly-add : gf u v -> u + v
;; The sum of two polynomials over Fq normalized.
(define (poly-add gf u v)
  (define (trim-cons head tail)
    (if (and (zero? head) (null? tail)) '() (cons head tail)))
  (define (add u v)
    (match (list u v)
      ((() ()) '())
      ((() (cv . cvs)) (trim-cons cv (add cvs '())))
      (((cu . cus) ()) (trim-cons cu (add cus '())))
      (((cu . cus) (cv . cvs)) (trim-cons (gf-add gf cu cv) (add cus cvs)))))
  (let ((result (add u v)))
    (if (null? result) (list 0) result)))

;; poly-scale : gf scalar u -> scalar * u
;; Every coefficient of u multiplied by scalar in Fq.
(define (poly-scale gf scalar u)
  (if (zero? scalar)
      (list 0)
      (map (lambda (coeff) (gf-mul gf scalar coeff)) u)))

;; poly-shift : u -> X * u
;; poly-shift : i u -> X^i * u
(define poly-shift
  (case-lambda
    ((u) (cons 0 u))
    ((i u) (if (= i 0) u (poly-shift (- i 1) (poly-shift u))))))

;; poly-mul : gf u v -> u * v
;; u = c0 + X*u' (head c0, tail u'), so u*v = c0*v + X*(u'*v):
;; scale v by the head of u, add the recursive product shifted by one.
(define (poly-mul gf u v)
  (match u
    (() '())
    ((c . cs) (poly-add gf (poly-scale gf c v) (poly-shift (poly-mul gf cs v))))))

;; poly-degree : u -> deg(u)
;; Returns degree of polynomial u, assuming u is in canonical form.
(define (poly-degree u) (- (length u) 1))

;; poly-divmod : gf u v -> (values q r), the quotient and remainder
;; of dividing u by v (u = q*v + r, deg(r) < deg(v)). While
;; deg(u) >= deg(v), cancel u's leading term with a matching term of
;; the quotient, subtract that term times v from u, and recurse on
;; what's left; each term is added to the quotient on the way back up.
(define (poly-divmod gf u v)
  (define degree-v (poly-degree v))
  (define lead-coeff-v-inv (gf-inv gf (list-ref v degree-v)))
  (define (div u)
    (let* ((degree-u (poly-degree u))
           (degree-diff (- degree-u degree-v)))
      (if (negative? degree-diff)
          (values (list 0) u)
          (let* ((lead-coeff-u (list-ref u degree-u))
                 (mono-coeff (gf-mul gf lead-coeff-u lead-coeff-v-inv))
                 (mono-shifted (poly-shift degree-diff (list mono-coeff)))
                 (mono-times-v (poly-shift degree-diff (poly-scale gf mono-coeff v)))
                 (next-u (poly-add gf u mono-times-v)))
            (receive (q r) (div next-u)
              (values (poly-add gf q mono-shifted) r))))))
  (div u))
