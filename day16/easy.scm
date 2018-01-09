(load "../util.scm")

(define (parse m)
  (let* ((func (string-ref m 0))
	 (slash (string-find-next-char m #\/))
	 (p1 (if slash
		 (substring m 1 slash)
		 (string-tail m 1)))
	 (p2 (if slash (string-tail m (+ slash 1)) #f)))
    (lambda (ps)
      (cond ((eq? func #\s) (rotate-list ps (- (string->number p1))))
	    ((eq? func #\x) (let ((new (list-copy ps)))
			      (list-set! new (string->number p1) (list-ref ps (string->number p2)))
			      (list-set! new (string->number p2) (list-ref ps (string->number p1)))
			      new))
	    ((eq? func #\p) (map (lambda (p) (cond ((string=? p p1) p2)
						   ((string=? p p2) p1)
						   (else p)))
				 ps))
	    (else (error "unknown dance move" func))))))

(define moves (chainmap `(,symbol->string ,parse) (call-with-input-file "./input.txt" read)))

(define programs (tabulate 16 (lambda (c) (char->string (ascii->char (+ c 97))))))

(define (solve)
  (reduce-left string-append "" (chain moves programs)))
