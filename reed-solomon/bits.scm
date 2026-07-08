;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Splitting and recomposing binary streams
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon bits)
  #:use-module (srfi srfi-1)
  #:export (split-into flatten))

;; split-into : n lst -> list of lists
;; Splits lst into consecutive chunks of length n, padding the last
;; chunk with 0 if the list's length isn't a multiple of n.
(define (split-into n lst)
  (if (<= n 0)
      (error "split-into: chunk size must be positive" n))
  (define (pad-to-multiple lst)
    (let ((r (modulo (length lst) n)))
      (if (zero? r)
          lst
          (append lst (make-list (- n r) 0)))))
  (let loop ((lst (pad-to-multiple lst)))
    (if (null? lst)
        '()
        (cons (take lst n) (loop (drop lst n))))))

(define (flatten lst)
  (error "flatten: to implement"))
