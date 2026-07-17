;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Reed-Solomon Encoder/Decoder: Systematic encoding (poly division)
;; and Decoding (Syndromes, Extended Euclidean, Chien Search, Forney).
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon core)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (reed-solomon gf)
  #:use-module (reed-solomon poly)
  #:export (generator encode syndromes euclid))

(define (generator gf n k)
  "Return the generator polynomial of the RS(N,K) code over the field
GF: g(X) = prod_{i=1}^{N-K} (X + alpha^i)."
  (fold (lambda (i g) (poly-mul gf (list (gf-exp gf i) 1) g))
        '(1)
        (iota (- n k) 1)))

(define (pad-codeword codeword n)
  "Return CODEWORD, zero-padded to exactly N symbols (codewords must
keep a fixed length, unlike normalized polynomials)."
  (let ((extra (- n (length codeword))))
    (if (positive? extra) (append codeword (make-list extra 0)) codeword)))

(define (encode gf n msg)
  "Systematically encode the message MSG (m(X)) into an N-symbol
codeword (c(X)) over the field GF: c(X) = m(X)*X^(N-k) -
(m(X)*X^(N-k) mod g(X)), where k is the length of MSG."
  (let* ((k (length msg))
         (gen-poly (generator gf n k))
         (augmented-msg (poly-shift (- n k) msg))
         (parity (poly-mod gf augmented-msg gen-poly))
         (codeword (poly-add gf augmented-msg parity)))
    (pad-codeword codeword n)))

(define (syndromes gf n k received)
  "Return the syndromes (S_1 ... S_(N-K)) of the possibly corrupted
codeword RECEIVED, over the field GF: S_i = RECEIVED(alpha^i). A
codeword is always a multiple of the generator polynomial, whose
roots are alpha^1..alpha^(N-K), so every syndrome is 0 iff RECEIVED
has no error."
  (map (lambda (i) (poly-eval gf received (gf-exp gf i))) (iota (- n k) 1)))

(define (euclid gf n k syndrome)
  "Solve the key equation for the syndrome polynomial SYNDROME (S(X)),
over the field GF: find omega and sigma such that omega(X) =
sigma(X)*S(X) mod X^(N-K), with deg(sigma) <= (N-K)/2, by running
the extended Euclidean algorithm on X^(N-K) and S(X) and stopping as
soon as the remainder's degree is small enough, rather than running
it to completion. sigma is the Bezout coefficient of S(X) (the
coefficient of X^(N-K) itself isn't tracked, since decoding doesn't
need it); omega is the matching term of the same remainder sequence
a plain Euclidean algorithm would use to compute gcd(X^(N-K), S(X))
by dividing until the remainder hits 0; here we stop earlier, at the
first remainder small enough to satisfy deg(sigma) <= (N-K)/2, so
omega is generally just an intermediate remainder, not that final
gcd. Return two values, the error evaluator polynomial omega and the
error locator polynomial sigma."
  (define (step omega0 omega1 sigma0 sigma1)
    (receive (q omega) (poly-divmod gf omega0 omega1)
      (let ((sigma (poly-add gf sigma0 (poly-mul gf q sigma1))))
        (if (< (* 2 (poly-degree omega)) (- n k))
            (values omega sigma)
            (step omega1 omega sigma1 sigma)))))
  (step (poly-shift (- n k) (list 1)) (poly-normalize syndrome) (list 0) (list 1)))