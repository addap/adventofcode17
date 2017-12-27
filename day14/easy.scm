(load "../util.scm")
(load "../day10/hard.scm")

(define key "amgozmfv")
(define keys
  (map (lambda (n) (string-append key "-" (number->string n)))
       (enumerate-interval 0 128)))

(define (solve)
  (length (remove-all '(#\0)
		      (fold-left append '()
				 (chainmap `(,hash ,hex->bin ,string->list) keys)))))
