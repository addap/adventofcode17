(load "../util.scm")

(define (parse m)
  (let* ((func (string-ref m 0))
	 (slash (string-find-next-char m #\/))
	 (index1 (if slash
		 (substring m 1 slash)
		 (string-tail m 1)))
	 (index2 (if slash (string-tail m (+ slash 1)) #f)))
    (lambda (ps)
      (let ((copy (vector-copy ps)))
	(cond ((eq? func #\s) (let* ((i1 (string->number index1))
				     (i2 (- (vector-length copy) i1))
				     (tail (vector-tail copy i2)))
				(subvector-move-right! copy 0 i2 copy i1)
				(subvector-move-right! tail 0 i1 copy 0)
				copy))
	      ((eq? func #\x) (let ((program1 (vector-ref copy (string->number index1)))
				    (program2 (vector-ref copy (string->number index2))))
				(vector-set! copy (string->number index2) program1)
				(vector-set! copy (string->number index1) program2)
				copy))
	      ((eq? func #\p) (vector-map (lambda (p) (cond ((string=? p index1) index2)
							    ((string=? p index2) index1)
							    (else p)))
					  copy))
	      (else (error "unknown dance move" func)))))))

(define moves (chainmap `(,symbol->string ,parse) (call-with-input-file "./input.txt" read)))

(define programs (list->vector (tabulate 16
					 (lambda (c) (char->string (ascii->char (+ c 97)))))))

;; apply cycles and apply-cumulative 
(define (disjunct-cycles mapping)
  (define (find-cycle m res)
    (let ((next (find (lambda (n) (equal? (car n)
					  (cdr m)))
		      mapping)))
      (cond ((not next) (error "mapping not bijective" mapping))
	    ((member next res) (cons m res))
	    (else (find-cycle next (cons m res))))))
  (if (null? mapping)
      '()
      (let ((cycle (find-cycle (car mapping) '())))
	(cons cycle
	      (disjunct-cycles (remove-all cycle mapping))))))

;; since the function is bijective one can decompose the mapping into disjunct cycles
;; these can easily be computed (longest is 4) as we only have to do (remainder 1_000_000_000 length) to see in which
;; state it will be in after one billion iterations.
;; My initial thought that there can be 16! ~ 22 trillion constellation is wrong because we apply the same function over and over again
;; TODO: make find-cycles procedure
(define (find-mapping start end)
  "Calculates a map consisting of indices that show at what place the program in that position of the list will end up after one iteration of the dance.
   If we change the mapping so that the index is the position of the program that will end up in that place of the list (inverse function), dance can be expressed without side-effects, will do later"
  (let ((p1 (vector->list start))
	(p2 (vector->list end)))
    (vector-map (lambda (s e) (cons s e))
		start end)))
			      
    
(define dance
  (find-mapping programs (chain moves programs)))
(define (solve)
  (reduce-left string-append "" (vector->list (iter 1000000000 programs dance))))
