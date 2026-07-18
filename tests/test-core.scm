(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (ice-9 match)
             (ice-9 receive)
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

(define msg2 (list #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011 #b1100 #b1101))

;; list of (n k received expected-syndromes expected-omega
;; expected-sigma expected-locations expected-corrected), covering a
;; clean codeword for two different (n,k) and three corrupted
;; codewords.
(define decode-cases
  (list (list 15 11 (encode g 15 msg)
              (list #b0 #b0 #b0 #b0) #f #f #f #f)
        (list 15 13 (encode g 15 msg2)
              (list #b0 #b0) #f #f #f #f)
        (list 15 11 (poly-add g (encode g 15 msg) (list #b1))
              (list #b1 #b1 #b1 #b1) (list #b1) (list #b1 #b1)
              (list 0) (encode g 15 msg))
        (list 15 11 (poly-add g (encode g 15 msg) (poly-add g (list #b1) (poly-shift 3 (list #b1))))
              (list #b1001 #b1101 #b1011 #b1110) (list #b1001) (list #b1 #b1001 #b1000)
              (list 0 3) (encode g 15 msg))
        (list 15 13 (poly-add g (encode g 15 msg2) (poly-shift 2 (list #b1)))
              (list #b100 #b11) (list #b1110) (list #b1010 #b1110)
              (list 2) (encode g 15 msg2))))

(for-each
 (match-lambda
   ((n k received expected-syndromes expected-omega expected-sigma expected-locations expected-corrected)
    (let ((syndrome (syndromes g n k received)))
      (test-equal (simple-format #f "syndromes: GF(16) RS(~a,~a), received=~a" n k received)
        expected-syndromes syndrome)
      (if expected-omega
          (receive (omega sigma) (euclid g n k syndrome)
            (test-equal (simple-format #f "euclid: GF(16) RS(~a,~a), syndrome=~a, omega=~a" n k syndrome expected-omega)
              expected-omega omega)
            (test-equal (simple-format #f "euclid: GF(16) RS(~a,~a), syndrome=~a, sigma=~a" n k syndrome expected-sigma)
              expected-sigma sigma)
            (test-assert (simple-format #f "euclid: GF(16) RS(~a,~a), deg(sigma) <= (n-k)/2" n k)
              (<= (poly-degree sigma) (quotient (- n k) 2)))
            (test-equal (simple-format #f "euclid: GF(16) RS(~a,~a), key equation omega(X) = sigma(X)*S(X) mod X^(n-k)" n k)
              (poly-normalize omega)
              (poly-mod g (poly-mul g sigma syndrome) (poly-shift (- n k) (list 1))))
            (let ((locations (chien-search g n sigma)))
              (test-equal (simple-format #f "chien-search: GF(16) RS(~a,~a), sigma=~a, locations=~a" n k sigma expected-locations)
                expected-locations locations)
              (let* ((errors (forney g n omega sigma locations))
                     (corrected (poly-add g received errors)))
                (test-equal (simple-format #f "forney: GF(16) RS(~a,~a), corrected codeword matches the original" n k)
                  expected-corrected corrected))))
          (test-assert (simple-format #f "syndromes: GF(16) RS(~a,~a), clean codeword has no error" n k)
            (every zero? syndrome))))))
 decode-cases)

(test-end "core")
