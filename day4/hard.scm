(load "../util.scm")
(define txt "input.txt")
(define in (read (open-input-file txt)))

(define (solve pps)
  (length (filter valid? pps)))
(define (valid? pp)
  (not (there-exists? (cross-product-wo-reflexive pp pp)
		      (lambda (c) (anagram? (car c) (cdr c))))))
;; as we parse them as sexprs, the words will be symbols, so we have to convert them to string first before sorting their
;; characters to find out if they're anagrams
(define (anagram? s1 s2)
  (equal? (sort (string->list (symbol->string s1)) char<?)
	  (sort (string->list (symbol->string s2)) char<?)))

(solve in)
