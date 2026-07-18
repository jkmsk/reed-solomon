(use-modules (srfi srfi-64)
             (ice-9 match)
             (reed-solomon bits))

(test-begin "bits")

;; list of (size lst expected), covering an exact multiple of the
;; block length, a last block padded with zero, an empty list, and a
;; single block shorter than the requested length.
(define split-into-cases
  (list (list 2 '(1 2 3 4 5 6) '((1 2) (3 4) (5 6)))
        (list 3 '(1 2 3 4 5) '((1 2 3) (4 5 0)))
        (list 3 '() '())
        (list 3 '(1) '((1 0 0)))))

(for-each
 (match-lambda
   ((size lst expected)
    (test-equal (simple-format #f "split-into: (split-into ~a ~a) = ~a" size lst expected)
      expected (split-into size lst))))
 split-into-cases)

;; list of (lst expected), covering a general list of lists, empty
;; sublists mixed in, and an empty list.
(define flatten-cases
  (list (list '((1 2) (3) (4 5 6)) '(1 2 3 4 5 6))
        (list '(() (1) () (2) ()) '(1 2))
        (list '() '())))

(for-each
 (match-lambda
   ((lst expected)
    (test-equal (simple-format #f "flatten: (flatten ~a) = ~a" lst expected)
      expected (flatten lst))))
 flatten-cases)

(test-equal "flatten after split-into recovers the padded stream"
  '(1 2 3 4 5 0)
  (flatten (split-into 3 '(1 2 3 4 5))))

;; list of (bits expected), covering the empty list, all-zero bits,
;; all-one bits, and a general case.
(define bits->integer-cases
  (list (list '() 0)
        (list '(0 0 0) 0)
        (list '(1 1 1 1) 15)
        (list '(1 0 1 1) 11)))

(for-each
 (match-lambda
   ((bits expected)
    (test-equal (simple-format #f "bits->integer: ~a -> ~a" bits expected)
      expected (bits->integer bits))))
 bits->integer-cases)

;; list of (n size expected), covering zero, a value using every bit
;; of SIZE, and a size larger than strictly needed (leading zeros).
(define integer->bits-cases
  (list (list 0 3 '(0 0 0))
        (list 15 4 '(1 1 1 1))
        (list 11 4 '(1 0 1 1))
        (list 11 6 '(0 0 1 0 1 1))))

(for-each
 (match-lambda
   ((n size expected)
    (test-equal (simple-format #f "integer->bits: ~a in ~a bits -> ~a" n size expected)
      expected (integer->bits n size))))
 integer->bits-cases)

(test-equal "bits->integer/integer->bits round-trip"
  200 (bits->integer (integer->bits 200 8)))

(test-end "bits")
