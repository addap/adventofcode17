(load "../auxiliaries.scm")
(define txt "input.txt")
(define in (read (open-input-file txt)))

(define (solve pps)
  (length (filter valid? pps)))
(define (valid? pp)
  (not (there-exists? (cross-product-wo-reflexive pp pp)
		      (lambda (c) (equal? (car c) (cdr c))))))
