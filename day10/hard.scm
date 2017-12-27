(load "../util.scm")
(define input "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62")
(define knot-len 256)
(define base-knot (enumerate-interval 0 knot-len))
(define runs 64)
(define block-size 16)

(define (run list pos skip lengths)
  (if (null? lengths)
      (cons list (cons pos (cons skip '())))
      (begin
	(let* ((len (car lengths))
	       (lst (rotate-list list pos))
	       (sp (split lst len))
	       (newlist (append (reverse (car sp)) (cdr sp)))
	       (newpos (modulo (+ pos len skip) (length list))))
	  (run (rotate-list newlist (- pos))
		newpos
		(+ skip 1)
		(cdr lengths))))))

;; splits the sparse hash into blocks of 16 bytes, foldls the blocks using bitwise xor and 
(define (densify sparse-hash)
  (let* ((blocks (split-per sparse-hash block-size))
	 (xord (map (lambda (block)
		      (reduce-left bitwise-xor 0 block))
		    blocks))
	 (bytes (map byte->hex xord)))
    (fold-left string-append "" bytes)))

(define (parse input)
  (let* ((chars (string->list input))
	 (asciis (map char->ascii chars)))
    (append asciis '(17 31 73 47 23))))
	
(define (hash input)
  (let* ((lengths (parse input))
	 (sparse-hash (first (iter runs
				   (list base-knot 0 0)
				   (lambda (h)
				     (run (first h) (second h) (third h) lengths)))))
	 (dense-hash (densify sparse-hash)))
    dense-hash))
		     
