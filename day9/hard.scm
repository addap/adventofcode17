(define stream (open-input-file "./input.txt"))

(define (scan score result skip stream garbage)
  (let ((c (read-char stream)))
    (if (eq? c #\!)
	(begin
	  (read-char stream)
	  (scan score result skip stream garbage))
	(cond ((eof-object? c) garbage)
	      ((eq? c #\>) (scan score result #f stream garbage))
	      (skip (scan score result skip stream (+ garbage 1)))
	      ((eq? c #\<) (scan score result #t stream garbage))
	      ((eq? c #\{) (scan (+ score 1) result skip stream garbage))
	      ((eq? c #\}) (scan (- score 1) (+ result score) skip stream garbage))
	      ((or (eq? c #\,)
		   (eq? c #\space)
		   (eq? c #\newline)) (scan score result skip stream garbage))
	      (else (error "Malformed Stream:" c))))))

(define (test s)
  (scan 0 0 #f (open-input-string s) 0))


	
