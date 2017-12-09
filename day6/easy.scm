(define in '(5 1 10 0 1 7 13 14 3 12 8 10 7 12 0 6))

(define (realloc banks step mem)
  (define (help m banks)
    (if (= m 0) banks
	(let* ((newbanks (rotate-list 1 banks)))
	  (help (- m 1) (cons (+ 1 (car newbanks))
			      (cdr newbanks))))))
  (if (member banks mem)
      step
      (let* ((m (apply max banks))
	    (i (find-index m banks))
	    (newbanks (rotate-list (- (length banks)
				      (modulo (+ i m) (length banks)))
				   (help m (cons 0 (cdr (rotate-list i banks)))))))
	(realloc newbanks (+ step 1) (cons banks mem)))))
