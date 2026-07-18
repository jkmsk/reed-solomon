;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Splitting and recomposing binary streams
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon bits)
  #:use-module (srfi srfi-1)
  #:export (split-into flatten bits->integer integer->bits))

(define (split-into/pad fill size lst)
  "Return LST split into consecutive chunks of length SIZE, padding
the last chunk with FILL if LST's length isn't a multiple of SIZE."
  (define (pad-to-multiple lst)
    (let ((r (modulo (length lst) size)))
      (if (zero? r)
          lst
          (append lst (make-list (- size r) fill)))))
  (when (<= size 0)
    (error "split-into/pad: chunk size must be positive" size))
  (let loop ((lst (pad-to-multiple lst)))
    (if (null? lst)
        '()
        (cons (take lst size) (loop (drop lst size))))))

(define (split-into size lst)
  "Return LST split into consecutive chunks of length SIZE, padding
the last chunk with 0 if LST's length isn't a multiple of SIZE."
  (split-into/pad 0 size lst))

(define (flatten lst)
  "Return LST with one level of nesting removed, concatenating its
sublists into one."
  (apply append lst))

(define (bits->integer bits)
  "Return the integer represented by BITS, a list of 0s and 1s,
most-significant bit first."
  (fold (lambda (bit acc) (+ (* acc 2) bit)) 0 bits))

(define (integer->bits n size)
  "Return N as a list of SIZE bits (0s and 1s), most-significant bit
first."
  (map (lambda (i) (if (logbit? i n) 1 0)) (iota size (- size 1) -1)))
