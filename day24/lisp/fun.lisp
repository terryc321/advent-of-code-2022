
#|
advent of code 2022 day 24 blizzard

if blizzard at x,y pointing east > and playfield size is width , height at time step 0
what will be its position at time step N

similarly for other three directions < ^ v

then if we know where all blizzards are at all times , we know where the empty spaces will be
we can then plot out , maybe breadth first approach , find shortest path


|#


(sb-ext:restrict-compiler-policy 'debug 3 3)
(sb-ext:restrict-compiler-policy 'safety 3 3)

(defpackage foo
  (:use :cl))
(in-package :foo)

#|
;; flatten it ? ( "#.######.....##>....##.....##...v.##.....######.#"

#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#

5x5 grid
top opening and bottom opening
(defparameter arr (make-array '(2 3)))

can the wind ever escape out the opening ?
top opening is (1,0)
top left is (1,1)
bottom right is (5,5)
bottom opening is (5,6)

wind at (x=1,y=2,direction= +X)
wind at (x=4,y=4,direction= +Y)

wind > can travel right x=5 ok , when x > 5 replaced with new one at x = 1 still facing right
similarly 
wind v can travel down to y=5 ok , any more gets replaced with x=4 , y = 1, direction v down

grid dimensions 5 5
wind position ( x , y )
wind directions < > ^ v

state of game
 wid hgt  explorer-x explorer-y  wind1-x wind1-y dir    wind2-x wind2-y dir
            wind1 implicit direction east
            wind2 implicit direction south

|#



;; put as a function (put "~%") did not produce a newline like expected
;; so made a macro , not sure its shorter than (terpri) common lisps version of newline
;;(defun put (s) (format t "~a" s))
(defmacro put (s)  
  `(cond
     ((symbolp ,s) (format t "~a" ,s))
     (t (format t ,s))))

;; just assume one explorer , two wind components
(defun show (state)
  (destructuring-bind ((wid hgt)(ex ey) . winds) state
    (loop for y from 0 to (+ hgt 1) do
          (put "~%")
             (loop for x from 0 to (+ wid 1) do
                   (let ((here '()))
                     (when (and (= x ex)(= y ey)) (setq here (cons 'E here)))
		     ;; iterate over winds
		     (dolist (wind winds)
                       (destructuring-bind (wx wy dir) wind
                         ;;(format t "wind => ~a : wx = ~a : wy = ~a ~%" wind wx wy)
                         (when (and (= x wx) (= y wy)) (setq here (cons dir here)))))
		     ;; 
                     (cond 
                       ((> (length here) 1)
                        (put "(")
                        (dolist (h here)  (put h))
                        (put ")"))
                       ((= (length here) 1) (put (car here)))
                       ((and (= x 1)(= y 0)) (put ".")) ;; top opening
                       ((and (= x wid)(= y (+ hgt 1))) (put ".")) ;; bot opening
                       ((and (>= x 1)(<= x wid)(>= y 1)(<= y hgt)) (put "."))
                       (t (put "#") ;; wall
                          ))))))
  (put "~%"))



                     
(defun test ()
  (show `((5 5)(1 0)(1 2 >)(4 4 v))))

(defun advance-wind (wid hgt winds)
  (let ((winds2 (mapcar (lambda (x)
			  (destructuring-bind (wx wy wdir) x
			    (cond
			      ((eq wdir '>)
			       (setq wx (+ wx 1))
			       (if (> wx wid) (setq wx 1)))
			      ((eq wdir '<)
			       (setq wx (- wx 1))
			       (if (< wx 1) (setq wx wid)))
			      ((eq wdir 'v)
			       (setq wy (+ wy 1))
			       (if (> wy hgt) (setq wy 1)))
			      ((eq wdir '^)
			       (setq wy (- wy 1))
			       (if (< wy 1) (setq wy hgt)))
			      (t (error "bad dir")))
			    (list wx wy wdir)))
			winds)))
    winds2))


#|
task : read a file and turn grid into something more usable
[ ] open a file for reading
[ ] read line by line
[ ] determine playfield width height , ignoring external walls
[ ] player at 1 0 assumed
[ ] player exit at width (height + 1)
|#

(defun parse-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines '()))
      (catch 'foo
	(loop while t do
	  (let ((line (read-line stream nil 'eof)))
	    (cond
	      ((eq line 'eof) (throw 'foo t))
	      (t (setq lines (cons line lines)))))))
      (setq lines (reverse lines))
      lines)))

(defun parse-lines (lines)
  (let ((width (- (length (car lines)) 2))
	(height (- (length lines) 2))
	(winds '())
	(explorer-x 1)(explorer-y 0)
	(y -1))
    (dolist (line lines)
      (incf y)
      (let ((len (length line)))
	(loop for x from 0 to (- len 1) do
	  (let ((ch (char line x)))
	    ;; check empty entry to playfield is at (1,0) and (width,height+1)
	    (when (and (= x 1)(= y 0)) (assert (char= ch #\.)))
	    (when (and (= x width)(= y (+ height 1))) (assert (char= ch #\.)))
	  (when (or (not (= y 0)) (not (= y (+ height 1)))
		    (not (= x 0)) (not (= x (+ width 1))))
	      (cond
		((char= ch #\>) (setq winds (cons (list x y '>) winds)))
		((char= ch #\<) (setq winds (cons (list x y '<) winds)))
		((char= ch #\^) (setq winds (cons (list x y '^) winds)))
		((or (char= ch #\v)(char= ch #\V)) (setq winds (cons (list x y 'v) winds)))
		))))))
    `((,width ,height) (,explorer-x ,explorer-y) ,@winds)))

;; get lines and converts lines to a state 
(defun parse (filename)
  (parse-lines (parse-file filename)))

(defun test-parse ()
  (let ((lines (parse-file "example1.txt")))
    (format t "lines => ~a~%" lines)
    (parse-lines lines)))


;; 
;; (defparameter *state* `((5 5)(1 0)(1 2 >)(4 4 v)))
;; (defun next (state) 
;;   (show state)
;;   (setq *state* (advance-wind *state*)))

;; (defun run (state)
;;   (loop for i from 1 to 100 do
;;     (show state)
;;     (setq state (advance-wind state))))

(defparameter *example1* (parse "example1.txt"))
(defparameter *example2* (parse "example2.txt"))
(defparameter *input* (parse "../input.txt"))

#|

#|
solver explorer can move
right - increase X 
left - decrease X 
up - decrease Y
down - increase Y

we have to try to reach bottom 
|#

;;(destructuring-bind (_ a b c) '(ignore 1 2 3) (list a b c))
;;(destructuring-bind (_ (x y) . winds) '(ignore (1 2) (1 2 >) (2 3 <) (4 5 ^)))


(defun explorer-position (state)
  (second state))

(defun winds-of (state)
  (cdr (cdr state)))

(defun playfield-of (state)
  (first state))

(defun player-valid (width height x y)
  (or (and (= x 1)(= y 0))
      (and (= x width) (= y (+ height 1)))
      (and (>= x 1)(<= x width)(>= y 1)(<= y height))))

#|
more than one state
   initial state  ((width height) (explorer-x explorer-y) ... winds .. (wx wy wdir) .. )
   search state   (step (explorer-x explorer-y) winds)  since we need to track number of steps
   if doing breadth first search - we simply 

breadth first search of state
|#

(defun explore (state)
  (let ((working '())
	(todo '())
	(steps 0))
    ;;
    (catch 'solved
      (destructuring-bind (width height) (playfield-of state)
	(format t "playfield is ~a ~a in size ~%" width height)
	;; assume explorer at position (1 , 0)
	;; search state ((explorer-x explorer-y) ... winds ... )
	;; initial working set is a single state = the start state
	(setq working (list `((1 0) (winds-of state))))
	
	(loop while (not (null working)) do
	  (dolist (state working)

	    ;; some mechanism to abort adding next state of this to working set
	    (catch 'blizzard
	      (destructuring-bind (ex ey) (first state)
		(let ((winds (cdr state)))
		  
		  ;; found a solution
		  (when (and (= ex width) (= ey (+ height 1)))
		    (format t "our explorer reached exit after ~a steps ~%" steps)
		    (throw 'solved t))
		  
		  ;; if player somehow not on the playfield for whatever reason - abort this 
		  (when (not (player-valid width height ex ey))
		    (throw 'blizzard t))

		  ;; check all winds that no blizzard and explorer occupy same square
		  (dolist (wind winds)
		    (destructuring-bind (wx wy wdir) wind
		      (when (and (= wx ex)(= wy ey))
			(throw 'blizzard t))))
		  
		  ;; explorer go right and advance winds together
		  ;; go right
		  (let ((ex2 (+ ex 1))(ey2 ey)) (setq todo (cons `((,ex2 ,ey2) ,@(advance-wind width height winds)) todo)))
		  
		  ;; go left
		  (let ((ex2 (- ex 1))(ey2 ey)) (setq todo (cons `((,ex2 ,ey2) ,@(advance-wind width height winds)) todo)))

		  ;; go up
		  (let ((ex2 ex)(ey2 (- ey 1))) (setq todo (cons `((,ex2 ,ey2) ,@(advance-wind width height winds)) todo)))

		  ;; go down
		  (let ((ex2 ex)(ey2 (+ ey 1))) (setq todo (cons `((,ex2 ,ey2) ,@(advance-wind width height winds)) todo)))

		  ;; wait
		  (let ((ex2 ex)(ey2 ey)) (setq todo (cons `((,ex2 ,ey2) ,@(advance-wind width height winds)) todo)))

		  ))))
	  ;; flip working and nullify todo
	  (setq working todo)
	  (setq todo nil)
	  (setq steps (+ steps 1))
	      )))))


|#




