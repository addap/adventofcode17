(define stream (open-input-file "./input.txt"))

(define (scan score result skip stream)
  (let ((c (read-char stream)))
    (if (eq? c #\!)
	(begin
	  (read-char stream)
	  (scan score result skip stream))
	(cond ((eof-object? c) result)
	      ((eq? c #\>) (scan score result #f stream))
	      (skip (scan score result skip stream))
	      ((eq? c #\<) (scan score result #t stream))
	      ((eq? c #\{) (scan (+ score 1) result skip stream))
	      ((eq? c #\}) (scan (- score 1) (+ result score) skip stream))
	      ((or (eq? c #\,)
		   (eq? c #\space)) (scan score result skip stream))
	      (else (error "Malformed Stream"))))))

(define (test s)
  (scan 0 0 #f (open-input-string s)))
	
