(define (rotate-list list n)
  "Rotates a list by taking the first n elements and appending them to the rest.
If a negative argument is given the last n elements are prepended to the rest"
  (let* ((l (modulo n (length list)))
	 (front (sublist list 0 l))
	 (back (sublist list l (length list))))
    (append back front)))

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

(define (flatmap proc . lists)
  (fold-left append '() (apply map (cons proc lists))))

(define (chainmap fs list)
  (if (null? fs) list
      (chainmap (cdr fs) (map (car fs) list))))

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

(define (find-index x list)
  (define (help n list)
    (cond ((null? list) #f)
	  ((equal? x (car list)) n)
	  (else (help (+ n 1) (cdr list)))))
  (help 0 list))

(define (remove-duplicates list)
  (define (help l result)
    (cond ((null? l) result)
	  ((member (car l) result) (help (cdr l) result))
	  (else (help (cdr l) (cons (car l)
				    result)))))
  (help list '()))

(define (tabulate n f)
  (map f (enumerate-interval 0 n)))

(define (make-circular list)
  (define (help l)
    (if (null? (cdr l))
	(set-cdr! l list)
	(help (cdr l))))
  (help list))

(define (split list s)
  (cons (take list s) (drop list s)))
(define (split-per list s)
  (if (null? list)
      '()
      (let ((sp (split list s)))
	(cons (car sp) (split-per (cdr sp) s)))))

(define (assert b err)
  (if b #t (error err)))

(define (iter n initial proc)
  (if (<= n 0) initial
      (iter (- n 1) (proc initial) proc)))

;; maybe add general conversion procedure
;; this only works for 0 <= n <= 255 and outputs the byte as a string of two hex numbers
(define (byte->hex n)
  (let ((f (integer-divide n 16)))
    (string-append (char->string (digit->char (car f) 16))
		   (char->string (digit->char (cdr f) 16)))))

;; converts a string of hex numbers into a string of binary numbers
(define (hex->bin x)
  (define (help i p)
    (if (= p 0.5) '()
	(if (>= i p)
	    (cons #\1 (help (- i p) (/ p 2)))
	    (cons #\0 (help i (/ p 2))))))
  (list->string (flatmap (lambda (h) (help (char->digit h 16) 8))
			 (string->list x))))
;; remove all elements in list1 from list2
(define (remove-all list1 list2)
  (remove (lambda (x) (member x list1))
	  list2))

;; returns the first number for which predicate evaluates to true
(define (first-true pred n)
  (if (pred n) n
      (first-true pred (+ n 1))))

;; breadth first search
(define (find-connected open closed universe adjacent)
  (if (null? open) closed
      (let* ((current (car open))
	     (adj (remove-all (append open closed)
			      (adjacent (first current)
					(second current)
					universe))))
	(find-connected (append (cdr open) adj)
			(cons current closed)
			universe
			adjacent))))

(define (nested-coordinates b x y)
  (+ (* y b)
     x))
