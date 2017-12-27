(define programs (call-with-input-file "./input.txt" read))

(define (lookup cell table)
  (find (lambda (c) (= (car c) cell)) table))

;; maybe abstract and move to util
(define (bf-search open closed universe)
  (if (null? open) closed
      (let ((id (car open)))
	(if (memq id closed)
	    (bf-search (cdr open) closed universe)
	    (let ((children (cdr (lookup id universe))))
	      (bf-search (append (cdr open) children) (cons id closed) universe))))))
(define (solve)
  (length (bf-search '(0) '() programs)))
