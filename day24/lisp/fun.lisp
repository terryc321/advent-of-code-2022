

;; advent of code 2022 day 24 blizzard

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
(defun show-game (state)
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
  (show-game `((5 5)(1 0)(1 2 >)(4 4 v))))

(defun advance-wind (state)
  (destructuring-bind ((wid hgt)(ex ey) . winds) state
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
    ;; new state
      `((,wid ,hgt)(,ex ,ey) ,@winds2))))


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
;;   (show-game state)
;;   (setq *state* (advance-wind *state*)))

(defun run (state)
  (loop for i from 1 to 100 do
    (show-game state)
    (setq state (advance-wind state))))

(defparameter *example1* (parse "example1.txt"))
(defparameter *example2* (parse "example2.txt"))


