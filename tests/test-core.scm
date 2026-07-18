(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (ice-9 match)
             (ice-9 receive)
             (reed-solomon gf)
             (reed-solomon poly)
             (reed-solomon bits)
             (reed-solomon core))

(test-begin "core")

(define poly #b10011)
(define g (make-gf poly))
(define m (g 'deg))

(define msg1 (list #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011))
(define msg2 (list #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011 #b1100 #b1101))
(define msg3 (list #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b0))
(define cw1 (encode g 15 11 msg1))
(define cw2 (encode g 15 13 msg2))
(define cw3 (encode g 15 11 msg3))

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

;; list of (msg k expected-codeword), covering a general message and a
;; message whose leading (highest-degree) symbol is 0.
(define encode-cases
  (list (list msg1 11
              (list #b1000 #b100 #b110 #b1001 #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b1011))
        (list (append (drop-right msg1 1) (list 0)) 11
              (list #b1001 #b1100 #b1 #b110 #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b0))))

(for-each
 (match-lambda
   ((msg k expected)
    (let ((codeword (encode g 15 k msg)))
      (test-equal (simple-format #f "encode: GF(16) RS(15,~a), concrete codeword" k)
        expected codeword)
      (test-equal (simple-format #f "encode: GF(16) RS(15,~a), codeword has exactly 15 symbols" k)
        15 (length codeword))
      (test-assert (simple-format #f "encode: GF(16) RS(15,~a), codeword is a multiple of the generator" k)
        (equal? (list #b0) (poly-mod g codeword (generator g 15 k))))
      (test-equal (simple-format #f "encode: GF(16) RS(15,~a), the message reappears unchanged in the high-order positions" k)
        msg (list-tail codeword (- 15 k))))))
 encode-cases)

;; encode accepts a MSG shorter than K directly: a short message and
;; its zero-padded equivalent must produce the exact same codeword.
(define short-msg (list #b1 #b10 #b11))
(define padded-msg (append short-msg (make-list (- 11 (length short-msg)) 0)))

(test-equal "encode: GF(16) RS(15,11), a message shorter than K is equivalent to one padded with high-order 0s"
  (encode g 15 11 padded-msg) (encode g 15 11 short-msg))

;; list of (message corrupted? n k received expected-syndromes
;; expected-omega expected-sigma expected-locations
;; expected-corrected), covering a clean codeword for two different
;; (n,k), two corrupted RS(15,11) codewords, a corrupted RS(15,13)
;; codeword, and a corrupted codeword whose message has a leading
;; (highest-degree) symbol of 0.
(define decode-cases
  (list (list msg1 #f 15 11 cw1
              (list #b0 #b0 #b0 #b0) #f #f #f #f)
        (list msg2 #f 15 13 cw2
              (list #b0 #b0) #f #f #f #f)
        (list msg1 #t 15 11 (poly-add g cw1 (list #b1))
              (list #b1 #b1 #b1 #b1) (list #b1) (list #b1 #b1)
              (list 0) cw1)
        (list msg1 #t 15 11 (poly-add g cw1 (list #b1 #b0 #b0 #b1))
              (list #b1001 #b1101 #b1011 #b1110) (list #b1001) (list #b1 #b1001 #b1000)
              (list 0 3) cw1)
        (list msg2 #t 15 13 (poly-add g cw2 (poly-shift 2 (list #b1)))
              (list #b100 #b11) (list #b1110) (list #b1010 #b1110)
              (list 2) cw2)
        (list msg3 #t 15 11 (list #b1000 #b1100 #b1 #b110 #b1 #b10 #b11 #b100 #b101 #b110 #b111 #b1000 #b1001 #b1010 #b0)
              (list #b1 #b1 #b1 #b1) (list #b1) (list #b1 #b1)
              (list 0) cw3)))

(for-each
 (match-lambda
   ((message corrupted? n k received expected-syndromes expected-omega expected-sigma expected-locations expected-corrected)
    (let ((syndrome (syndromes g n k received)))
      (test-equal (simple-format #f "syndromes: GF(16) RS(~a,~a), received=~a" n k received)
        expected-syndromes syndrome)
      (if corrupted?
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
                     (corrected (poly-add g received errors))
                     (corrected (append corrected (make-list (- n (length corrected)) 0))))
                (test-equal (simple-format #f "forney: GF(16) RS(~a,~a), corrected codeword matches the original" n k)
                  expected-corrected corrected))))
          (test-assert (simple-format #f "syndromes: GF(16) RS(~a,~a), clean codeword has no error" n k)
            (every zero? syndrome)))
      (test-equal (simple-format #f "decode: GF(16) RS(~a,~a), decoded message" n k)
        message (decode g n k received)))))
 decode-cases)

;; RS(15,11) corrects at most t=(n-k)/2=2 errors; 3 errors is beyond
;; the code's capacity, so sigma has no root among the candidate
;; positions and decode must raise an error rather than silently
;; returning a wrong message.
(test-error "decode: GF(16) RS(15,11), too many errors raises an error"
  #t (decode g 15 11 (poly-add g cw1 (list #b1 #b0 #b0 #b1 #b0 #b0 #b0 #b1))))

(define (bits-of symbols)
  (flatten (map (lambda (symbol) (integer->bits symbol m)) symbols)))

;; list of (msg n k codeword), covering a general message for two
;; different (n,k) and a message whose leading (highest-degree)
;; symbol is 0.
(define encode-bits-cases
  (list (list msg1 15 11 cw1)
        (list msg2 15 13 cw2)
        (list msg3 15 11 cw3)))

(for-each
 (match-lambda
   ((msg n k codeword)
    (test-equal (simple-format #f "encode-bits: GF(16) RS(~a,~a)" n k)
      (bits-of codeword) (encode-bits poly n k (bits-of msg)))))
 encode-bits-cases)

;; list of (n k received message), reusing the same clean and
;; corrupted scenarios as decode-cases.
(define decode-bits-cases
  (list (list 15 11 cw1 msg1)
        (list 15 13 cw2 msg2)
        (list 15 11 (poly-add g cw1 (list #b1)) msg1)
        (list 15 11 (poly-add g cw1 (list #b1 #b0 #b0 #b1)) msg1)
        (list 15 13 (poly-add g cw2 (poly-shift 2 (list #b1))) msg2)))

(for-each
 (match-lambda
   ((n k received message)
    (test-equal (simple-format #f "decode-bits: GF(16) RS(~a,~a)" n k)
      (bits-of message) (decode-bits poly n k (bits-of received)))))
 decode-bits-cases)

(test-error "decode-bits: GF(16) RS(15,11), too many errors raises an error"
  #t (decode-bits poly 15 11 (bits-of (poly-add g cw1 (list #b1 #b0 #b0 #b1 #b0 #b0 #b0 #b1)))))

(test-end "core")
