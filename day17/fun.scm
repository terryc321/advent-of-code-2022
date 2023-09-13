


(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...

;; --------------------- macros --------------------------
(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			   ,@body
			   (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- macros --------------------------

(define *debug* #f)


(define input #f)

(define (get-input filename)
  (let ((port (open-input-file filename)))
    (set! input (read port))
    (close-input-port port)
    (string-length input)))

;; for example
;;(define input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(define input #f)

(get-input "input2")





#|
rock begins falling
blown left / right
falls one
blown left /right
falls one
...
rests when falls "into" rock below , does not fall into it though
can still be blown left / right while directly on top of another rock


xs simulating an infinite list

height after 2022 rocks have fallen and stopped , but before 2023 rd rock begins falling

rocks - poor mans tetris

move piece left , right , up , down
no up but.
piece yield a list of squares it occupies

(piece-left piece) -> all coords shift left
(piece-right piece) -> all coords shift right
(piece-down piece) -> all coords shift down
(piece-press piece) -> press piece into board so it becomes rock !

bit fiddling to figure out where piece initial x , y should be

different pieces are simply a result of being given x , y coord to start and generates a list of coords
where rock would be if it was still

dont actually change hash until ready to place the rock

moving window seven units high 

####

.#.
###
.#.

..#
..#
###

#
#
#
#

###
###

rock starts 2 units empty away from left wall
rock can start on the 3rd column only

x=0 x=1 x=2
        ***
rock height begins so that it is clear by 3 units
so it begins on the 4th from highest
then add height of piece itself


|#


(define highest-rock 0)

;; when rock gets stuck becomes affixed , flag rock-fixed set true
(define *rock-fixed* #t)

;; fixed
(define flat-horz-piece  (lambda () (let ((x 2)(y (+ 4 highest-rock)))
					 `((,(+ x 0) ,y) (,(+ x 1) ,y)
					   (,(+ x 2) ,y) (,(+ x 3) ,y)))))

;; fixed
(define cross-piece  (lambda () (let ((x 2)(y (+ 6 highest-rock)))
				     `((,(+ x 1) ,(+ y 0))  (,(+ x 1) ,(+ y -1))  (,(+ x 0) ,(+ y -1))
				      (,(+ x 2) ,(+ y -1)) (,(+ x 1) ,(+ y -2))))))


;; fixed
(define elbow-piece  (lambda () (let ((x 2)(y (+ 6 highest-rock)))
				     `((,(+ x 2) ,(+ y 0))  (,(+ x 2) ,(+ y -1))  (,(+ x 2) ,(+ y -2))
				       (,(+ x 1) ,(+ y -2)) (,(+ x 0) ,(+ y -2))))) )


;; 
(define flat-vert-piece  (lambda () (let ((x 2)(y (+ 7 highest-rock)))
					 `((,(+ x 0) ,(+ y 0)) (,(+ x 0) ,(+ y -1))
					   (,(+ x 0) ,(+ y -2)) (,(+ x 0) ,(+ y -3))))))


;; 
(define box-piece  (lambda () (let ((x 2)(y (+ 5 highest-rock)))
				   `((,(+ x 0) ,(+ y 0)) (,(+ x 1) ,(+ y 0))
				     (,(+ x 1) ,(+ y -1)) (,(+ x 0) ,(+ y -1))))))


(define (height fn)
  (cond
   ((eq? fn flat-horz-piece) 1)
   ((eq? fn cross-piece) 3)
   ((eq? fn elbow-piece) 3)
   ((eq? fn flat-vert-piece) 4)
   ((eq? fn box-piece) 2)
   (#t (error "height:fn not regnised"))))

  

(flat-horz-piece)

(cross-piece)

(elbow-piece)

(flat-vert-piece)

(box-piece)

(define pieces (list flat-horz-piece
		     cross-piece
		     elbow-piece
		     flat-vert-piece
		     box-piece))


;; track highest piece of rock
(define board (make-hash-table))

(hash-set! board (list 0 0) #f)
(hash-ref board (list 0 0))
(hash-ref board (list 0 1))
(hash-ref board (list 1 1))
(hash-ref board (list 0 0))


;;(define current-piece (flat-horz-piece 2 (+ 4 highest-rock)))
;;(define current-piece (cross-piece 2 (+ 5 highest-rock)))
;;(define current-piece (elbow-piece 2 (+ 5 highest-rock)))
;;(define current-piece (flat-vert-piece 2 (+ 6 highest-rock)))
(define current-piece (flat-horz-piece))
;; (define current-piece (cross-piece))
;; (define current-piece (elbow-piece))
;; (define current-piece (flat-vert-piece))
;; (define current-piece (box-piece))

;; current piece is defined as a list of coordinates
;; generated from procedures take no arguments , places piece at correct location
;; as long as global variable highest-rock identifies highest y coordinate
;; currently occupied by rock

;; can current-piece go left , right , down ?
;;
;;  wall -1  >|  0 1 2 3 4 5 6  |< 7 wall
;; 
;;                                   0 lowest can go vertically
;;            ------------------    -1 floor wall
;; yes if all x y are >=  0,0  6 =<

(define current-str-i 0)

;; piece added to board
;; piece floating on board

(define (board-yx y x)
  (let ((res (hash-ref board (list x y))))
    (cond
     (res "#")
     ((member (list x y) current-piece) "@")
     (#t "."))))


(define (show-board)
  (let iterate ((y (+ highest-rock 20)))
    ''(format #t "i = ~a ~%" i)
    (format #t "| ~a ~a ~a ~a ~a ~a ~a |~%" (board-yx y 0) (board-yx y 1) (board-yx y 2) (board-yx y 3) (board-yx y 4) (board-yx y 5) (board-yx y 6) )
    (cond
     ((> y 0) (iterate (- y 1)))
     (#t (format #t "----------------------~%")))))


;; any-conflict? conflict with list coords and board ? ... returns bool
;; no conflict , no rock in way of this rock

;; on-board? list coords ... returns bool
;; is the piece still on the board?

(define (any-conflict? possible)
  (let ((res (filter (lambda (x) x)
		     (map (lambda (p) (hash-ref board p)) possible))))
    (if (null? res) #f #t)))

(define (go-left)
  ;;(format #t "<go left>")
  (let ((possible (map (lambda (p) (list (1- (first p)) (second p)) ) current-piece)))
    (cond
     ((not (null? (filter (lambda (p) (< (first p) 0)) possible))) #f)
     ((not (null? (filter (lambda (p) (> (first p) 6)) possible))) #f) ;;redundant as going left
     ((any-conflict? possible) #f)
     (#t
      ;; went left
     (when *debug* (format #t " Went Left !~%"))
      (set! current-piece possible)))))

(define (go-right)
  ;;(format #t "<go right>")  
  (let ((possible (map (lambda (p) (list (1+ (first p)) (second p)) ) current-piece)))
    (cond
     ((not (null? (filter (lambda (p) (< (first p) 0)) possible))) #f)
     ((not (null? (filter (lambda (p) (> (first p) 6)) possible))) #f) ;;redundant as going left
     ((any-conflict? possible) #f)
     (#t
      ;; went right
      (when *debug* (format #t " Went Right !~%"))
      (set! current-piece possible)))))



(define (drop-down)
  ;;(format #t "<drop down>")  
  (let ((possible (map (lambda (p) (list (first p) (1- (second p)))) current-piece)))
    (cond
     ;;((not (null? (filter (lambda (p) (< (second p) 1)) possible))) (affix-rock))
     ((any-conflict? possible) (affix-rock))
     (#t
      ;; went down
      (when *debug* (format #t "Went down !~%"))
      (set! current-piece possible)))))


(define (affix-rock)
  (map (lambda (p) (hash-set! board p #t)) current-piece)
  (set! *rock-fixed* #t))
  

(define (clear-board)
  (set! board (make-hash-table))
  (map (lambda (p) (hash-set! board p #t))
       `((0 0)(1 0)(2 0)(3 0)(4 0)(5 0)(6 0))))


(clear-board)


;; make xs an infinite list of pieces forever in sequence provided by example
(define game
  (lambda (verbose)
    (call/cc (lambda (exit)
	       (clear-board)
	       (set! highest-rock 0)
    (let* ((str-len (string-length input))
	   (str-i 0)
	   (xs pieces)
	   (ys xs)
	   (wind-n 0)
	   (count 0))
      (set! ys (cdr (cdr (cdr (cdr xs)))))
      (set-cdr! ys xs)
      (letrec ((loop2
		(lambda ()
		  ;; assume highest-rock has been computed
		  ;; somewhere before we create new rock

		  ;; no pieces iterated through
		  (set! count (1+ count))
		  
		  ;; introduced piece
		  (let ((fn (car xs)))
		    ;;(format #t "piece function => ~a ~%" fn)
		    (set! current-piece (fn)))
		  ;; move onto next piece as never read xs again
		  (set! xs (cdr xs))

		  (when verbose (show-board))
		  
		  ;; flip flag that rock is affixed
		  (set! *rock-fixed* #f)
		  		   
		  (letrec ((loop (lambda ()

				   ;;(when verbose (show-board))
				   
				   (set! wind-n (1+ wind-n))

				   (let ((wind (string-ref input str-i)))
				     (cond
				      ((char=? #\> wind)
				       (when verbose (format #t "WIND ~a >> RIGHT~%" wind-n))
				       (go-right))
				      ((char=? #\< wind)
				       (when verbose (format #t "WIND ~a << LEFT~%" wind-n))
				       (go-left))
				      (#t (error "wind char expected #\\< or #\\>"))))

				   ;; next character of input
				   (set! str-i (1+ str-i))
				   (when (>= str-i str-len)
				     (set! str-i 0))
				   
				   ;;(show-board)			 
				   (drop-down)
				   
				   ;;(show-board)
				   ;; keep looping if not affixed
				   (if *rock-fixed* #t (loop)))))
		    (loop)) ;;enter loop
		  
		  ;; rock fixed to board
		  ;; rock might fall down a gap so it's x << Y >> locations will not be maximum
		  (set! highest-rock (max highest-rock (apply max (map second current-piece))))

		  ;; if placed 2022 rocks
		  ;; part II 
		  (when (= count 2022)
		    (format #t "highest rock after 2022 rocks is ~a ~%" highest-rock)
		    (exit highest-rock))		  

		  (loop2)
		  		  
		  )))
	(loop2) ;; enter outer loop
	))))))






;; ;; we better computer highest-rock now
;; ;; before we
;; (format #t "~a : car => ~a : current-piece ~a ~%" i (car x) current-piece)
;; (when (< i 10)
;;   (iterate (cdr x) (+ i 1)))))))

#|
highest rock after 2022 rocks is 3211 
$14 = 3211
|#

#|

part two wants 10 ^ 12 ... is there a repeating pattern then ??
(= count 1000000000000) ;;

|#

