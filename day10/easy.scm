(load "../util.scm")
(define lengths '(189 1 111 246 254 2 0 120 215 93 255 50 84 15 94 62))
(define input (enumerate-interval 0 256))

(define (hash list pos lengths skip)
  (if (null? lengths)
      list
      (begin
	(let* ((len (car lengths))
	       (lst (rotate-list list pos))
	       (sp (split lst len))
	       (newlist (append (reverse (car sp)) (cdr sp)))
	       (newpos (+ pos len skip)))
	  (hash (rotate-list newlist (- pos))
		newpos
		(cdr lengths)
		(+ skip 1))))))
(define (solve)
  (let ((h (hash input 0 lengths 0)))
    (* (car h) (cadr h))))
