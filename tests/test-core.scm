(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (ice-9 match)
             (reed-solomon gf)
             (reed-solomon poly)
             (reed-solomon core))

(test-begin "core")

(define g (make-gf #b10011))
(define msg (list #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011))

;; list of (n k expected-generator), covering RS(15,11) and RS(15,13).
(define generator-cases
  (list (list 15 11 (list #b111 #b1000 #b1100 #b1101 #b1))
        (list 15 13 (list #b1000 #b110 #b1))))

(for-each
 (match-lambda
   ((n k expected)
    (let ((gen (generator g n k)))
      (test-equal (simple-format #f "generator: GF(16) RS(~a,~a), value ~a" n k expected)
        expected gen)
      (test-equal (simple-format #f "generator: degree is n-k, for RS(~a,~a)" n k)
        (- n k) (poly-degree gen))
      (test-equal (simple-format #f "generator: is monic, for RS(~a,~a)" n k)
        #b1 (last gen))
      (test-assert (simple-format #f "generator: alpha^1..alpha^(n-k) are roots, for RS(~a,~a)" n k)
        (every (lambda (i) (zero? (poly-eval g gen (gf-exp g i))))
               (iota (- n k) 1))))))
 generator-cases)

;; list of (msg expected-codeword), covering a general message and a
;; message whose leading (highest-degree) symbol is 0.
(define encode-cases
  (list (list msg
              (list #b1000 #b100 #b110 #b1001 #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011))
        (list (append (drop-right msg 1) (list 0))
              (list #b1001 #b1100 #b1 #b110 #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b0))))

(for-each
 (match-lambda
   ((msg expected)
    (let ((codeword (encode g 15 msg))
          (k (length msg)))
      (test-equal (simple-format #f "encode: GF(16) RS(15,~a), concrete codeword" k)
        expected codeword)
      (test-equal (simple-format #f "encode: GF(16) RS(15,~a), codeword has exactly 15 symbols" k)
        15 (length codeword))
      (test-assert (simple-format #f "encode: GF(16) RS(15,~a), codeword is a multiple of the generator" k)
        (equal? (list #b0) (poly-mod g codeword (generator g 15 k))))
      (test-equal (simple-format #f "encode: GF(16) RS(15,~a), the message reappears unchanged in the high-order positions" k)
        msg (list-tail codeword (- 15 k))))))
 encode-cases)

;; list of (n k received expected-syndromes), covering a clean
;; codeword for two different (n,k) and a corrupted codeword
(define syndrome-cases
  (list (list 15 11 (encode g 15 msg)
              (list #b0 #b0 #b0 #b0))
        (list 15 13
              (list #b1 #b101 #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011 #b1100 #b1101)
              (list #b0 #b0))
        (list 15 11 (poly-add g (encode g 15 msg) (list #b1))
              (list #b1 #b1 #b1 #b1))))

(for-each
 (match-lambda
   ((n k received expected)
    (test-equal (simple-format #f "syndromes: GF(16) RS(~a,~a), received=~a" n k received)
      expected (syndromes g n k received))))
 syndrome-cases)

(test-end "core")
