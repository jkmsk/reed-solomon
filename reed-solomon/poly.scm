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
  #:export (poly-add poly-scale poly-shift poly-mul poly-divmod poly-mod poly-eval poly-degree poly-normalize))

(define (poly-normalize u)
  "Return U with any superfluous high-order zero coefficients stripped."
  (define (trim-cons head tail)
    (if (and (zero? head) (null? tail)) '() (cons head tail)))
  (define (normalize u)
    (if (null? u) '() (trim-cons (car u) (normalize (cdr u)))))
  (let ((result (normalize u)))
    (if (null? result) (list 0) result)))

(define (poly-add gf u v)
  "Return the sum of the polynomials U and V over the field GF, normalized."
  (define (add u v)
    (match (list u v)
      ((() ()) '())
      ((() _) (add v (list 0)))
      ((_ ()) (add u (list 0)))
      (((cu . cus) (cv . cvs)) (cons (gf-add gf cu cv) (add cus cvs)))))
  (poly-normalize (add u v)))

(define (poly-scale gf scalar u)
  "Return the polynomial U with every coefficient multiplied by
SCALAR, in the field GF."
  (if (zero? scalar)
      (list 0)
      (map (lambda (coeff) (gf-mul gf scalar coeff)) u)))

(define poly-shift
  (case-lambda
    "Return X * U, shifting the coefficients of U up by one position.
With the two-argument form, return X^I * U, shifted by I positions."
    ((u) (poly-shift 1 u))
    ((i u)
     (let ((u (poly-normalize u)))
       (if (equal? u (list 0)) u (append (make-list i 0) u))))))

(define (poly-mul gf u v)
  "Return the product of the polynomials U and V over the field GF.
U = c + X*cs (head c, tail cs), so U*V = c*V + X*(cs*V): scale V
by the head of U, and add the recursive product shifted by one."
  (match u
    (() (list 0))
    ((c . cs) (poly-add gf (poly-scale gf c v) (poly-shift (poly-mul gf cs v))))))

(define (poly-degree u)
  "Return the degree of the polynomial U, assuming U is in canonical form."
  (- (length u) 1))

(define (poly-divmod gf u v)
  "Divide the polynomial U by V over the field GF and return two
values, the quotient q and the remainder r, such that U = q*V + r
with deg(r) < deg(V). While deg(U) >= deg(V), cancel U's leading
term with a matching term of the quotient, subtract that term
times V from U, and recurse on what's left; each term is added to
the quotient on the way back up."
  (let ((v (poly-normalize v)))
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
    (div (poly-normalize u))))

(define (poly-mod gf u v)
  "Return the remainder r of dividing the polynomial U by V over the
field GF (deg(r) < deg(V))."
  (receive (_ r) (poly-divmod gf u v) r))

(define (poly-eval gf u point)
  "Evaluate the polynomial U at POINT, over the field GF, using
Horner's method: U(X) = c0 + X*(c1 + X*(c2 + ... + X*ck)), so rather
than computing powers of POINT separately, recurse on U's structure
(head c, tail cs) as U(POINT) = c + POINT*cs(POINT)."
  (match u
    (() 0)
    ((c . cs) (gf-add gf c (gf-mul gf point (poly-eval gf cs point))))))
