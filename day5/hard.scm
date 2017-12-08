(define in (read (open-input-file "input.txt")))

(define (maze l pos)
  (cons l pos))
(define (mlist maze) (car maze))
(define (value maze) (list-ref (mlist maze) (position maze)))
(define (position maze) (cdr maze))
(define (jump maze step)
  (let ((newpos (+ (position maze)
		   (value maze)))
	(newval (if (>= (value maze) 3)
		    (- (value maze) 1)
		    (+ (value maze) 1))))
    (if (or (< newpos 0) (>= newpos (length (car maze))))
	step
	(begin
	  (list-set! (mlist maze) (position maze) newval)
	  (set-cdr! maze newpos)
	  (jump maze (+ step 1))))))

(jump (maze in 0) 1)
;; 26948068
  
    
  
