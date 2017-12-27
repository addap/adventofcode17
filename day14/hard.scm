(load "../util.scm")
(load "../day10/hard.scm")

(define key "amgozmfv")
(define keys
  (map (lambda (n) (string-append key "-" (number->string n)))
       (enumerate-interval 0 128)))
(define hashes (map hash keys))
(define memory-list (chainmap `(,hex->bin ,string->list) hashes))

;; todo move to util, nested lists to nested vector
(define mem (list->vector (flatmap (lambda (y row)
				     (map (lambda (x cell)
					    (list x y cell))
					  (enumerate-interval 0 128)
					  row))
				   (enumerate-interval 0 128)
				   memory-list)))

(define (used? x) (eq? (third x) #\1))

(define (adjacent x y u)
  (define (clamp a) (cond ((< a 0) 0)
			  ((> a 127) 127)
			  (else a)))
  (filter used? (list (vector-ref u (nested-coordinates 128 (clamp (- x 1)) (clamp y)))
		      (vector-ref u (nested-coordinates 128 (clamp (+ x 1)) (clamp y)))
		      (vector-ref u (nested-coordinates 128 (clamp x) (clamp (- y 1))))
		      (vector-ref u (nested-coordinates 128 (clamp x) (clamp (+ y 1)))))))

(define (find-regions memory r)
  (define (unset-all region memory)
    (let ((new (vector-copy memory)))
      (for-each (lambda (c)
		  (vector-set! new
			       (nested-coordinates 128
						   (first c) (second c))
			       (list (first c) (second c) #\0)))
		region)
      new))
  (let ((start (find used? (vector->list memory))))
    (if start
	(let ((region (find-connected (list start) '() memory adjacent)))
	  (find-regions (unset-all region memory) (+ r 1)))
	r)))

(define (solve)
  (find-regions coordinates 0)) 

