(define path (call-with-input-file "./input.txt" read))

(define origin '(0 0 0))
(define (add cube1 cube2)
  (map + cube1 cube2))
(define (direction-map dir)
  (cond ((eq? dir 's) '(0 -1 1))
	((eq? dir 'n) '(0 1 -1))
	((eq? dir 'ne) '(1 0 -1))
	((eq? dir 'sw) '(-1 0 1))
	((eq? dir 'nw) '(-1 1 0))
	((eq? dir 'se) '(1 -1 0))))

(define (traverse-track-route path start)
  (fold-left (lambda (res dir)
	       (add res
		    (direction-map dir)))
	     start
	     path))

(define (distance cube1 cube2)
  (/ (fold-left + 0 (map abs (map - cube1 cube2)))
     2))

(define (solve)
  (distance (traverse path origin)
	    origin))
		      
 
      
