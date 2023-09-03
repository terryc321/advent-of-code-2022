
#|

fudge
(chdir "day10")

no idea why guile geiser always loads in wrong directory....jez..

|#

(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

(define pp pretty-print)

;;(use-modules (srfi srfi-1))
(define first car)
(define second (lambda (x) (car (cdr x))))



;; guile error handling??
;; roll your own assert macro
(define-macro (assert x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (if ,g ,g (error (list 'assertion 'failed ,g 'expression 'was ',x))))))

;; ------ debugging macros ----------
;; change (if #f  to (if #t  to see nice diagrams
;; when run tests
(define-macro (snoke p1 p2 p3)
  (if #f
      `(snooker ,p1 ,p2 ,p3)
      `(quiet-snooker ,p1 ,p2 ,p3)))

;; see print logging on some 
(define-macro (dbug . args)
  (if #f
      `(format #t ,@args)
      #t))

;; more print logging for debugging
(define-macro (dbug2 . args)
  (if #f
      `(format #t ,@args)
      #t))


;; n-ary not equal
(define (/= . args)
  (not (apply = args)))


;; dirty test suite ...
(define (expect title a b)
  (let ((ok (equal? a b)))
    (if ok
	ok
	(format #t "failed [~a] : expected ~a : received ~a~%" title a b))))


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


#|

addx N
noop

|#

(define (sane-commands xs)
  (cond
   ((null? xs) #t)
   (#t (let ((word (first xs)))
	 (cond
	  ((eq? word 'addx) (let ((num (second xs)))
			      (assert (integer? num))
			      (sane-commands (cdr (cdr xs)))))
	  ((eq? word 'noop) (sane-commands (cdr xs)))
	  (#t (error (list 'day10 'aoc2022 'command 'unknown xs))))))))


#|

single register X : initial value 1

addx V takes 2 clock cycles after which X register increased by V
V can be negative in which case it decreases X register

noop takes 1 cycle and does nothing

clock : starts at 1 the first clock cycle
reg-x : 1 represent x register

addx V , V already integer by scheme read procedure

interested in 
20th, 60th, 100th, 140th, 180th, and 220th cycles

20 + (n * 40)



|#

(define (model commands)
  (let ((signal-sum 0)
	(reg-x 1)
	(clock 1))      
    (letrec ((tick (lambda ()
		     (format #t "tick ~a : reg-x ~a :" clock reg-x)
		     (when
			 (member clock '(20 60 100 140 180 220))
		       (let ((signal-product (* clock reg-x)))
			 (set! signal-sum (+ signal-sum signal-product))		       
		         (format #t "cycle ~a : reg-x ~a : signal-product ~a : signal-sum ~a "
				 clock
				 reg-x
				 signal-product
				 signal-sum)))
		     (format #t "~%")
		     (set! clock (+ clock 1))
		     ))
	     (execute (lambda (xs)
			(cond
			 ((null? xs) #t)
			 (#t (let ((word (first xs)))
			       (cond
				((eq? word 'addx) (let ((num (second xs)))
						    (tick)
						    (tick)
						    (set! reg-x (+ reg-x num))
						    (execute (cdr (cdr xs)))))
				((eq? word 'noop)
				 (tick)
				 (execute (cdr xs)))
				(#t (error (list 'model 'error 'day10 'aoc2022 'command 'unknown xs))))))))))
      (execute commands))))




(define (run filename)
  (let ((commands (get-lines filename)))
    (sane-commands commands)
    (model commands)))


#|

tick 220 : reg-x 21 :cycle 220 : reg-x 21 : signal-product 4620 : signal-sum 14220 
14220 accepted

crt 40 pixels wide and 6 pixels high

Cycle   1 -> ######################################## <- Cycle  40
Cycle  41 -> ######################################## <- Cycle  80
Cycle  81 -> ######################################## <- Cycle 120
Cycle 121 -> ######################################## <- Cycle 160
Cycle 161 -> ######################################## <- Cycle 200
Cycle 201 -> ######################################## <- Cycle 240

pixels drawn left to right

pixel 0 far left
pixel 39 far right
40 pixels per row
6 rows

x register sets middle pixel of 3 pixel sprite
                         A   B  C
                             .
                            /|\

copied model procedure to cathode-ray as its more complex and likely fall over


|#



(define (cathode-ray commands)
  (let ((signal-sum 0)
	(reg-x 1)
	(clock 1)
	(ray-x 0)
	(ray-y 0))
    (letrec ((tick (lambda ()
		     ;; crt at ray-x ray-y
		     (if (or (= ray-x (- reg-x 1)) ; cute
			     (= ray-x (- reg-x 0))
			     (= ray-x (- reg-x -1)))
			 (format #t "#")
			 (format #t "."))
		     ;; advance cathode ray
		     ;; advance scan line if reached end of scan
		     (set! ray-x (+ ray-x 1))
		     (when (> ray-x 39)
		       (format #t "~%") ;; newline
		       (set! ray-x 0)
		       (set! ray-y (+ 1 ray-y)))
		     ;; advance clock
		     (set! clock (+ clock 1))
		     ))
	     (execute (lambda (xs)
			(cond
			 ((null? xs) #t)
			 (#t (let ((word (first xs)))
			       (cond
				((eq? word 'addx) (let ((num (second xs)))
						    (tick)
						    (tick)
						    (set! reg-x (+ reg-x num))
						    (execute (cdr (cdr xs)))))
				((eq? word 'noop)
				 (tick)
				 (execute (cdr xs)))
				(#t (error (list 'model 'error 'day10 'aoc2022 'command 'unknown xs))))))))))
      (execute commands))))




(define (run2 filename)
  (let ((commands (get-lines filename)))
    (sane-commands commands)
    (cathode-ray commands)))

#|

scheme@(guile-user)> (run2 "input2")
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
$16 = #t

scheme@(guile-user)> (run2 "input")
####.###...##..###..#....####.####.#..#.
...#.#..#.#..#.#..#.#....#.......#.#..#.
..#..#..#.#..#.#..#.#....###....#..#..#.
.#...###..####.###..#....#.....#...#..#.
#....#.#..#..#.#.#..#....#....#....#..#.
####.#..#.#..#.#..#.####.#....####..##..
$17 = #t

ZRARLFZU


|#
