(load "../util.scm")

(define firewall
  (map (lambda (l) (append '(1 1) l))
       (call-with-input-file "./input.txt" read)))

(define test-firewall '((1 1 0 3)
			(1 1 1 2)
			(1 1 4 4)
			(1 1 6 4)))
			

(define (scanner layer) (first layer))
(define (direction layer) (second layer))
(define (depth layer) (third layer))
(define (range layer) (fourth layer))

;; check if the scanner would be in position 1 after (+ (depth layer) delay) steps
(define (catches? layer delay)
  (= 0
     (remainder (+ delay (depth layer))
		(* 2 (- (range layer) 1)))))
	 
(define (solve)
  (first-true (lambda (d)
		(every (lambda (l) (not (catches? l d)))
		       firewall))
	      0))
