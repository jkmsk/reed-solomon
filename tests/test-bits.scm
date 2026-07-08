(use-modules (srfi srfi-64)
             (reed-solomon bits))

(test-begin "bits")

(test-equal "split-into: exact multiple of the block length"
  '((1 2) (3 4) (5 6))
  (split-into 2 '(1 2 3 4 5 6)))

(test-equal "split-into: last block padded with zero"
  '((1 2 3) (4 5 0))
  (split-into 3 '(1 2 3 4 5)))

(test-equal "split-into: empty list"
  '()
  (split-into 3 '()))

(test-equal "split-into: single block shorter than length"
  '((1 0 0))
  (split-into 3 '(1)))

(test-equal "group-into: no padding needed at either level"
  '(((1 2) (3 4)) ((5 6) (7 8)))
  (group-into 2 2 '(1 2 3 4 5 6 7 8)))

(test-equal "group-into: symbol-level padding needed"
  '(((1 2) (3 4)) ((5 0) (0 0)))
  (group-into 2 2 '(1 2 3 4 5)))

(test-equal "group-into: empty list"
  '()
  (group-into 2 2 '()))

(test-equal "flatten: list of lists"
  '(1 2 3 4 5 6)
  (flatten '((1 2) (3) (4 5 6))))

(test-equal "flatten: with empty sublists"
  '(1 2)
  (flatten '(() (1) () (2) ())))

(test-equal "flatten: empty list"
  '()
  (flatten '()))

(test-equal "flatten after split-into recovers the padded stream"
  '(1 2 3 4 5 0)
  (flatten (split-into 3 '(1 2 3 4 5))))

(test-end "bits")
