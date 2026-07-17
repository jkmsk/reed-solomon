;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Reed-Solomon Encoder/Decoder: Systematic encoding (poly division)
;; and Decoding (Syndromes, Extended Euclidean, Chien Search, Forney).
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon core)
  #:use-module (srfi srfi-1)
  #:use-module (reed-solomon gf)
  #:use-module (reed-solomon poly)
  #:export (generator encode))

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
