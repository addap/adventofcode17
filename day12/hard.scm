(load "../util.scm")
(define programs (call-with-input-file "./input.txt" read))

(define (lookup cell table)
  (find (lambda (c) (= (car c) cell)) table))

;; maybe abstract and move to util
(define (bf-search open closed universe)
  (if (null? open) closed
      (let ((id (car open)))
	(if (find (lambda (p) (= id (car p))) closed)
	    (bf-search (cdr open) closed universe)
	    (let* ((program (lookup id universe))
		   (children (cdr program)))
	      (bf-search (append (cdr open) children) (cons program closed) universe))))))

(define (find-groups universe result)
  (if (null? universe) result
      (let* ((current (caar universe))
	     (group (bf-search (list current) '() universe)))
	(find-groups (remove-all group universe) (cons group result)))))
	
(define (solve)
  (length (find-groups programs '())))
