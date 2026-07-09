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

(test-equal "flatten2: list of lists of lists"
  '(1 2 3 4 5 6 7 8)
  (flatten2 '(((1 2) (3 4)) ((5 6) (7 8)))))

(test-equal "flatten2: with empty sublists at every level"
  '(1 2)
  (flatten2 '(() ((1)) (() (2)) ())))

(test-equal "flatten2: empty list"
  '()
  (flatten2 '()))

(test-equal "flatten2 after group-into recovers the padded stream"
  '(1 2 3 4 5 0 0 0)
  (flatten2 (group-into 2 2 '(1 2 3 4 5))))

(test-equal "hamming-distance: identical lists"
  0
  (hamming-distance '(1 2 3 4) '(1 2 3 4)))

(test-equal "hamming-distance: all positions differ"
  4
  (hamming-distance '(1 2 3 4) '(0 0 0 0)))

(test-equal "hamming-distance: some positions differ"
  2
  (hamming-distance '(1 2 3 4) '(1 0 3 0)))

(test-equal "hamming-distance: both empty"
  0
  (hamming-distance '() '()))

(test-equal "hamming-distance: stops at the shorter list"
  1
  (hamming-distance '(1 2 3) '(1 0)))

(test-equal "hamming-distance: nested elements compared with equal?"
  1
  (hamming-distance '((1 2) (3 4)) '((1 2) (9 9))))

(test-end "bits")
