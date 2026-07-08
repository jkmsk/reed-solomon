;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Splitting and recomposing binary streams
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon bits)
  #:use-module (srfi srfi-1)
  #:export (split-into group-into flatten))

;; split-into/pad : pad size lst -> list of lists
;; Splits lst into consecutive chunks of length size, padding the last
;; chunk with pad if the list's length isn't a multiple of size.
(define (split-into/pad pad size lst)
  (if (<= size 0)
      (error "split-into/pad: chunk size must be positive" size))
  (define (pad-to-multiple lst)
    (let ((r (modulo (length lst) size)))
      (if (zero? r)
          lst
          (append lst (make-list (- size r) pad)))))
  (let loop ((lst (pad-to-multiple lst)))
    (if (null? lst)
        '()
        (cons (take lst size) (loop (drop lst size))))))

;; split-into : size lst -> list of lists
;; split-into/pad specialized to pad with 0.
(define (split-into size lst)
  (split-into/pad 0 size lst))

;; group-into : symbol-size group-size lst -> list of lists of lists
;; Splits lst into symbols of length symbol-size, then groups those
;; symbols into blocks of group-size symbols, padding the last block
;; with padding-symbols if needed.
(define (group-into symbol-size group-size lst)
  (let ((symbols (split-into symbol-size lst)))
    (split-into/pad (make-list symbol-size 0) group-size symbols)))

(define (flatten lst)
  (error "flatten: to implement"))
