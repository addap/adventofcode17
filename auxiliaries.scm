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
