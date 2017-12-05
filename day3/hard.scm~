(load "../auxiliaries.scm")

(define input 289326)

;; 2 methods, either take 289326 steps and keep track of the coordinates, then compute manhattan
;; or find a closed form solution 
;; vllt eine circular list um die richtungen zu wechseln
;; '(right up left down ...)
;; oder eine turn methode die eben matched
(define (calc-spiral-steps n)
  (define (next d)
    (let ((dir (car d))
	  (scale (cadr d))
	  (count (caddr d)))
      (cond ((> count 0) (list dir scale (- count 1)))
	    ((equal? dir '(1 0)) (list '(0 1) scale (-  scale 1)))
	    ((equal? dir '(0 1)) (list '(-1 0) (+ scale 1) scale))
	    ((equal? dir '(-1 0)) (list '(0 -1) scale (- scale 1)))
	    ((equal? dir '(0 -1)) (list '(1 0) (+ scale 1) scale))
	    (else (display "something wrong")
		  (display d)))))
  (define (add-coord c1 c2)
    (list (+ (car c1) (car c2))
	  (+ (cadr c1) (cadr c2))))
  (define (manhattan c1 c2)
    (+ (abs (- (car c1) (car c2)))
       (abs (- (cadr c1) (cadr c2)))))
  (define (help d c n)
    (if (<= n 1)
	c
	(help (next d) (add-coord c (car d)) (- n 1))))
  (manhattan (help (list '(1 0) 1 0) '(0 0) n)
	     '(0 0)))

(calc-spiral-steps input)

