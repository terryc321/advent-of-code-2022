

#|

helpful stuff to copy & paste

|#

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(use-modules (ice-9 format))

(use-modules (srfi srfi-1))


(define p1 (make-vector 10 0))
(define p2 (make-array 0 10 10))

(array-set! p2 '0-5 0 5)
(array-set! p2 '0-0 0 0)
(array-set! p2 '1-1 1 1)
(array-set! p2 '2-2 2 2)
(array-set! p2 '9-1 9 1)
(array-set! p2 '9-0 9 0)

(pp p2)

;; not much correlation to how array looks on screen to x y coordinates
;; load input file into lines
(define (get-lines filename)
  (let ((lines '()))
    (call-with-input-file filename
      (lambda (port)
	(letrec ((self (lambda ()
			 (let ((obj (read port)))
			   (cond
			    ((eof-object? obj)
			     (reverse lines))
			    (#t (set! lines (cons obj lines))
			       (self)))))))
	  (self))))))

;;(define *lines* (get-lines "input"))



;; generate a 1d stream of bytes
(define (get-bytes filename)
  (let ((bytes '()))
    (call-with-input-file filename
      (lambda (port)
	(letrec ((self (lambda ()
			 (let ((obj (read-char port)))
			   (cond			    
			    ((eof-object? obj) 			     
			     (reverse bytes))			    
			    ((eq? #\newline obj) ; newline reached
			     (self))
			    (#t (set! bytes (cons obj bytes)) ; assuming only digits and newline?
				(self)))))))
	  (self))))))


;; really want to read a character at a time and assign it to corresponding grid square
;; when hit a new-line or end of line character
;; guile help system tab completion not helpful
;; read-byte TAB no help
;; read-char TAB no help
;; read TAB 
(define (get-bytes-2d filename)
  (let ((lines '())
	(line '()))
    (call-with-input-file filename
      (lambda (port)
	(letrec ((self (lambda ()
			 (let ((obj (read-char port)))
			   (cond
			    ((and (eof-object? obj)(null? line)) ; no more data
			     (reverse lines))
			    ((eof-object? obj) ; still data
			     (set! lines (cons (reverse line) lines))
			     (set! line '())
			     (reverse lines))			    
			    ((eq? #\newline obj) ; newline reached
			     (set! lines (cons (reverse line) lines))
			     (set! line '())
			     (self))
			    (#t (set! line (cons obj line)) ; assuming only digits and newline?
				(self)))))))
	  (self))))))

#|
newline in scheme #\newline
scheme@(guile-user)> (string-ref "\n" 0)
$40 = #\newline

$ hexdump -bc input2
0000000 063 060 063 067 063 012 062 065 065 061 062 012 066 065 063 063
0000000   3   0   3   7   3  \n   2   5   5   1   2  \n   6   5   3   3
0000010 062 012 063 063 065 064 071 012 063 065 063 071 060            
0000010   2  \n   3   3   5   4   9  \n   3   5   3   9   0            
000001d

like loop from 0 to size-1 , 0 to size-1

|#

;; roll your own assert macro
(define-macro (assert x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (if ,g ,g (error (list 'assertion 'failed ,g 'expression 'was ',x))))))
;;(assert (= 1 2))


(define (ch->digit c)
  (cond
   ((char=? c #\0) 0)     ((char=? c #\1) 1)     ((char=? c #\2) 2)     ((char=? c #\3) 3)
   ((char=? c #\4) 4)     ((char=? c #\5) 5)     ((char=? c #\6) 6)     ((char=? c #\7) 7)
   ((char=? c #\8) 8)     ((char=? c #\9) 9)
   (#t (error (list 'ch->digit 'c c 'not '0 'to '9)))))


(define (grid-from-file filename sz)
  (let* ((data (get-bytes filename))
	 (size sz)
	 (arr (make-array #f (+ size 2) (+ 2 size))))
    (letrec ((g (lambda (x y)
		  (cond
		   ((> x size) (g 1 (+ y 1)))
		   ((> y size)
		    (if (not (null? data))
			(begin
			  (format #t "warning - grid-from-file :")
			  (format #t "excess data left over ~%~a~%" data)
			  (assert (null? data)))			       
			#f)
		    arr)
		   (#t (let ((n (ch->digit (car data))))
			 (set! data (cdr data))
			 (array-set! arr n x y)
			 (g (+ x 1) y)))))))
      (g 1 1)
      arr)))


(define p2 (grid-from-file "input2" 5))

;; 99 x 99
(define the-grid (grid-from-file "input" 99))





#|
check semantics grid ok
(array-ref p 1 1)
(array-ref p 5 5)
(array-ref p 5 1)
(array-ref p 1 5)
(array-ref p 4 4)
(array-ref p 5 4)
(array-ref p 1 3)
(array-ref p 4 1)
|#

(define grid-ref array-ref)
(grid-ref p2 1 5)


#|
stay on grid 1,1 to 5,5 ok
outside get #f false values

is the tree visible?

given grid of size x size , start position x y
figure out if visible from outside

if tree at x2 y2 is same or greater value , it is hidden

if tree is visible from any angle , it is then visible
|#

(define (tree-viz g x y)
  (letrec ((viz-help (lambda (x2 y2 dx dy height size)
		     (cond
		      ((< x2 1) (list #t #t))
		      ((> x2 size) (list #t #t))
		      ((< y2 1) (list #t #t))
		      ((> y2 size) (list #t #t))
		      ((>= (grid-ref g x2 y2) height)
		       (list #f (list x2 y2 (grid-ref g x2 y2) height)))
		      (#t (viz-help (+ x2 dx) (+ y2 dy) dx dy height size)))))
	   (viz (lambda (dx dy hgt size) ; start search one step off x y
		  (viz-help (+ x dx) (+ y dy) dx dy hgt size))))
    (let* ((tree-height (grid-ref g x y))
	   (size (- (array-length g) 2))
	   (up (viz 0 -1 tree-height size))
	   (down (viz 0 1 tree-height size))
	   (left (viz -1 0 tree-height size))
	   (right (viz 1 0 tree-height size))
	   (results (list 'up  up
			  'down down
			  'right right
			  'left left)))
      (format #t "results = ~A~%" results)
      (or (first up)
	  (first down)
	  (first left)
	  (first right)))))


#|
for entire grid count number of visible trees
|#
(define (count grid)
  (let* ((size (- (array-length grid) 2))
	 (tot 0))
    (letrec ((g (lambda (x y)
		  (cond
		   ((> x size) (g 1 (+ y 1)))
		   ((> y size) tot) ; reached end of grid
		   (#t (if (tree-viz grid x y)
			   (begin
			     (format #t "tot ~a : tree viz at (~a,~a) ~%"
				     tot x y)
			     (set! tot (+ tot 1))
			     (g (+ x 1) y))
			   (begin
			     (g (+ x 1) y))))))))
      (g 1 1))))


#|
scheme@(guile-user)> (count the-grid)
...
...
tot 1532 : tree viz at (99,99) 
$112 = 1533
|#
      


#|
score trees based on view distance

tree at edge has a view distance of zero
tree to another tree as large or greater sets view distance in that direction

given list (4 1 7 4) want x2 bound to 4 , y2 bound to 1

using unhygienic macro to 
|#
(define-macro (bind_4 xs . body)
  `(let ((x2 (first ,xs))
	 (y2 (second ,xs)))
     ,@body))
;;(bind_4 '(4 1 7 4)  (list x2 y2))

(define (tree-score g x y)
  (letrec ((score-help (lambda (x2 y2 dx dy height size)
		     (cond
		      ((< x2 1) (list #t #t))
		      ((> x2 size) (list #t #t))
		      ((< y2 1) (list #t #t))
		      ((> y2 size) (list #t #t))
		      ((>= (grid-ref g x2 y2) height)
		       (list #f (list x2 y2 (grid-ref g x2 y2) height)))
		      (#t (score-help (+ x2 dx) (+ y2 dy) dx dy height size)))))
	   (score (lambda (dx dy hgt size) ; start search one step off x y
		    (score-help (+ x dx) (+ y dy) dx dy hgt size)))
	   (adapt (lambda (dir dx dy hgt size)
		    ;; keep dir constrained to be one of these
		    (assert (member dir '(up down left right)))
		    (let ((res (score dx dy hgt size)))
		      ;;(format #t "adapt.res = ~a ~%" res)
		      (cond
		       ;; no block tree
		       ((and (eq? dir 'up) (car res)) (abs (- y 1)))
		       ((and (eq? dir 'down) (car res))	(abs (- size y)))
		       ((and (eq? dir 'left) (car res)) (abs (- x 1)))
		       ((and (eq? dir 'right) (car res)) (abs (- size x)))
		       ;; block tree
		       ((and (eq? dir 'up)) (bind_4 (second res) (abs(- y y2))))
		       ((and (eq? dir 'down)) (bind_4 (second res) (abs (- y2 y))))
		       ((and (eq? dir 'left)) (bind_4 (second res) (abs (- x x2))))
		       ((and (eq? dir 'right))(bind_4 (second res) (abs (- x2 x))))
		       (#t (error (list 'adapt 'cant 'handle 'this res))))))))
    (second
     (let ((size (- (array-length g) 2)))
      (cond
       ((= x 1) (list 'product  0 'edge-x-1))
       ((= y 1) (list 'product 0 'edge-y-1))
       ((= x size) (list 'product 0 'edge-x-size))
       ((= y size) (list 'product 0 'edge-y-size))
       (#t
	(let* ((tree-height (grid-ref g x y))	   
	       (up (adapt 'up 0 -1 tree-height size))
	       (down (adapt 'down 0 1 tree-height size))
	       (left (adapt 'left -1 0 tree-height size))
	       (right (adapt 'right 1 0 tree-height size))
	       (results (list 'product (* up down left right)
			      'up  up
			      'down down
			      'right right
			      'left left)))
	  (format #t "results = ~A~%" results)
	  results)))))))




;; find best score
(define (best-score grid)
  (let* ((size (- (array-length grid) 2))
	 (best-loc '())
	 (best-score 0))
    (letrec ((g (lambda (x y)
		  (cond
		   ((> x size) (g 1 (+ y 1)))
		   ((> y size) (list 'best-score best-score  ; reached end of grid
				     'best-locations best-loc))
		   (#t (let ((score (tree-score grid x y)))
			 (cond
			  ((> score best-score)
			   (set! best-score score)
			   (set! best-loc (list (list x y)))
			   (format #t "new best score ~a : tree viz at (~a,~a) ~%"
				   score x y)
			   (g (+ x 1) y))
			  ((= score best-score) ; equal best score
			   (set! best-loc (cons (list x y) best-loc))
			   (format #t "equal best score ~a : tree viz at (~a,~a) ~%"
				   score x y)
			   (g (+ x 1) y))			   
			  (#t			     
			   (g (+ x 1) y)))))))))
      (g 1 1))))



#|
results = (product 2 up 2 down 1 right 1 left 1)
$155 = (best-score 345744 best-locations ((50 9)))

scheme@(guile-user) [2]> (tree-score the-grid 50 9)
results = (product 345744 up 8 down 18 right 49 left 49)
$156 = 345744
scheme@(guile-user) [2]> (* 8 18 49 49)
$157 = 345744

|#
    
    
    
		      
  



