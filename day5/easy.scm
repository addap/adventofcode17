(define in (read (open-input-file "input.txt")))

(define (maze l pos)
  (cons l pos))
(define (position maze) (cdr maze))
(define (set-position! maze newpos) (set-cdr! maze newpos))
(define (mlist maze) (car maze))
(define (value maze) (list-ref (mlist maze) (position maze)))
(define (set-value! maze newval) (list-set! (mlist maze) (position maze) newval))
(define (set-value-position maze newpos newval)
  (let ((curpos (posi
(define (step maze step)
  (let ((newpos (+ (position maze)
		   (value maze)))
	(newval ((+ (value maze) 1))))
    (if (or (< newpos 0) (>= newpos (length (car maze))))
	step
	(let* (
	  (jump maze (+ step 1))))))

(jump (maze in 0) 1)
	
  
    
  
