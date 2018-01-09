(define seedA 277)
(define seedB 349)
(define factorA 16807)
(define factorB 48271)
(define mod 2147483647)
(define 16b (expt 2 16))

(define (next f s m)
  (let ((n (remainder (* s f) mod)))
    (if (= (remainder n m) 0)
	n
	(next f n m))))

(define (count-matches n a b res)
  (if (= n 0) res
      (let ((new-a (next factorA a 4))
	    (new-b (next factorB b 8)))
	(count-matches (- n 1) new-a new-b
		       (+ res (if (= (remainder new-a 16b)
				     (remainder new-b 16b))
				  1 0))))))
		 
(define (solve)
  (count-matches 5000000 seedA seedB 0))
