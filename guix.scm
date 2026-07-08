(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system guile)
             (gnu packages guile)
             ((guix licenses) #:prefix license:))

(package
  (name "reed-solomon-scm")
  (version "0.1.0")
  (source (local-file "." "reed-solomon-scm-checkout"
                       #:recursive? #t
                       #:select? (git-predicate (dirname (current-filename)))))
  (build-system guile-build-system)
  (arguments
   (list #:source-directory "."))
  (native-inputs (list guile-3.0))
  (inputs (list guile-3.0))
  (synopsis "A Reed-Solomon implementation in Scheme for learning purposes")
  (description
   "A Reed-Solomon encoder/decoder in Scheme, featuring Galois Field GF(2^m) arithmetic, polynomials over fields, and systematic encoding/decoding using the extended Euclidean algorithm.")
  (home-page "https://github.com/jkmsk/reed-solomon")
  (license license:gpl3+))
