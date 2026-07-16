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
  #:export (generator))

;; generator : gf n k -> g(X) = prod_{i=1}^{n-k} (X + alpha^i)
;; The generator polynomial of the RS(n,k) code over Fq.
(define (generator gf n k)
  (fold (lambda (i g) (poly-mul gf (list (gf-exp gf i) 1) g))
        '(1)
        (iota (- n k) 1)))
