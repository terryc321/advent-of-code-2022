
(use-modules (ice-9 format))

(define example
  '(
(1 1 3 1 1)
(1 1 5 1 1)

((1) (2 3 4))
((1) 4)

(9)
((8 7 6))

((4 4) 4 4)
((4 4) 4 4 4)

(7  7  7  7)
(7  7  7)

()
(3)

((()))
(())

(1  (2  (3  (4  (5  6  7))))  8  9)
(1  (2  (3  (4  (5  6  0))))  8  9)
))


(define process-recur
  (lambda (exit left right)
    (exit #t)
    (format #t "left ~a : right ~a ~%" left right)
    (cond
     ((and (pair? left)(pair? right))
      1)
     ((and (pair? left)(integer? right)) 2)
     ((and (pair? right)(integer? left)) 3)
     ((and (integer? left)(integer? right))
      (cond
       ((< left right) #t)
       ((> left right) #f)
       (#t #t)))       
     (#t (error "process neither int or list")))))

  



(define process
  (lambda (left right)
    (call/cc (lambda (exit)
	       (format #t "left ~a : right ~a ~%" left right)
	       (cond
		((and (null? left)(null? right)) #t)
		((and (pair? left)(null? right)) (exit #f 'ran-out-items)
		((and (pair? right)(null? left)) (exit #t 'right-order)
		
		((and (pair? left)(pair? right)) (process-recur exit left right))
		((and (pair? left)(integer? right)) (process-recur exit left (list right)))
		((and (pair? right)(integer? left)) (process-recur exit (list left) right))
		((and (integer? left)(integer? right))
		 (cond
		  ((< left right) #t)
		  ((> left right) #f)
		  (#t #t)))       
		(#t (error "process neither int or list")))))))


    
(define pairs
  (lambda ()
    (letrec ((help (lambda (xs n)
		     (cond
		      ((null? xs) #t)
		      (#t (let ((left (car xs))
				(right (car (cdr xs))))
			    (process left right)
			   #t
			   (help (cdr (cdr xs)) (+ n 1)
						   )))))))
      (help example 1))))

(define test-1 (lambda ()
		 (process '(1 1 3 1 1) '(1 1 5 1 1))))






