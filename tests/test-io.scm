(use-modules (srfi srfi-64)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (reed-solomon io))

(test-begin "io")

(define test-path
  (string-append (or (getenv "TMPDIR") "/tmp")
                  "/reed-solomon-test-io-" (number->string (getpid)) ".bin"))

;; list of byte lists, covering an empty file, a single byte, the
;; extremes of the byte range, and a general case.
(define write-read-cases
  (list '()
        '(0)
        '(255)
        '(72 101 108 108 111 0 255 128 1)))

(for-each
 (lambda (bytes)
   (write-bytes test-path bytes)
   (test-equal (simple-format #f "write-bytes/read-bytes round-trip: ~a" bytes)
     bytes (read-bytes test-path)))
 write-read-cases)

;; read-bytes checked against a file written independently of write-bytes.
(call-with-output-file test-path
  (lambda (port) (put-bytevector port (u8-list->bytevector '(1 2 3 254 255))))
  #:binary #t)
(test-equal "read-bytes: reads a file written independently of write-bytes"
  '(1 2 3 254 255) (read-bytes test-path))

;; write-bytes checked against a file read independently of read-bytes.
(write-bytes test-path '(10 20 30))
(test-equal "write-bytes: a file it writes can be read independently of read-bytes"
  '(10 20 30) (bytevector->u8-list (call-with-input-file test-path get-bytevector-all #:binary #t)))

(delete-file test-path)

(test-end "io")
