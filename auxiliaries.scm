(define (rotate-list list n)
  (let* ((l (modulo n (length list)))
	 (front (sublist list 0 l))
	 (back (sublist list l (length list))))
    (append back front)))

(define (foldl f initial list)
  (if (null? list) initial
      (foldl f (f (car list) initial) (cdr list))))

(define (digits n)
  (define (help n)
    (if (< n 10) (cons n '())
      (let* ((qr (integer-divide n 10))
	     (q (integer-divide-quotient qr))
	     (r (integer-divide-remainder qr)))
	(cons r (help q)))))
  (reverse (help n)))

(define (divides n x)
  (and (exact? n) (exact? x)
       (= (remainder x n) 0)))

(define (filter p seq)
  (cond ((null? seq) '())
	((p (car seq)) (cons (car seq) (filter p (cdr seq))))
	(else (filter p (cdr seq)))))
	 
(define (enumerate-interval n m)
  (if (>= n m) '()
      (cons n (enumerate-interval (+ n 1) m))))

(define (flatmap proc list)
  (fold-right append '() (map proc list)))

(define (cross-product list1 list2)
  (flatmap (lambda (e1)
	 (map (lambda (e2)
		(cons e1 e2))
	      list2))
	   list1))

(define (cross-product-wo-reflexive list1 list2)
  (remove-step 0 (+ (length list2) 1)
	       (cross-product list1 list2)))

(define (remove-step start step list)
  (cond ((null? list) '())
	((= start 0) (remove-step (- step 1) step (cdr list)))
	(else (cons (car list)
		    (remove-step (- start 1) step (cdr list))))))

     
