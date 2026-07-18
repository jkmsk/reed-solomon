;; -- Reed-Solomon ----------------------------------------------------------
;;
;; Reading and writing files as flat lists of byte values
;; created by jkmsk <jkmsk@spinode.fr>, Copyright 2026
;;
;; --------------------------------------------------------------------------

(define-module (reed-solomon io)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (read-bytes write-bytes))

(define (read-bytes path)
  "Return the contents of the file at PATH as a list of byte values
(0-255). Empty files read back as an empty list."
  (let ((bv (call-with-input-file path get-bytevector-all #:binary #t)))
    (if (eof-object? bv) '() (bytevector->u8-list bv))))

(define (write-bytes path bytes)
  "Write BYTES (a list of byte values, 0-255) to the file at PATH."
  (call-with-output-file path
    (lambda (port) (put-bytevector port (u8-list->bytevector bytes)))
    #:binary #t))
