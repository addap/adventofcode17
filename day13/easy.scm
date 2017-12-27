(define firewall
  (map (lambda (l) (append '(1 1) l))
       (call-with-input-file "./input.txt" read)))

(define (scanner layer) (first layer))
(define (direction layer) (second layer))
(define (depth layer) (third layer))
(define (range layer) (fourth layer))

(define (move layer)
  (append (cond ((= (scanner layer) 1) '(2 1))
		((= (scanner layer) (range layer)) (list (- (range layer) 1) -1))
		(else (list (+ (scanner layer) (direction layer)) (direction layer))))
	  (list (depth layer) (range layer))))

(define (step position firewall severity)
  (if (> position 99) severity
      (let* ((current-layer (find (lambda (l) (= position (depth l))) firewall))
	     (new-firewall (map move firewall))
	     (s (if current-layer
		    (if (= (scanner current-layer) 1)
			(* (depth current-layer) (range current-layer))
			0)
		    0)))
	(step (+ position 1) new-firewall (+ severity s)))))
  
(define (solve)
  (step 0 firewall 0))
