
;; advent of code 2022 day 24 blizzard

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
                     (break) ;; put a breakpoint and see what value of winds is!
                     (when (and (= x wx)(= y wy)) (setq here (cons '> here)))
                     (when (and (= x wx2)(= y wy2)) (setq here (cons 'v here)))
                     (cond 
                       ((> (length here) 1)
                        (put "(")
                        (dolist (h here)  (put h))
                        (put ")"))
                       ((= (length here) 1) (put (car here)))
                       ((and (= x 1)(= y 0)) (put ".")) ;; top opening
                       ((and (= x (- wid 1))(= y hgt)) (put ".")) ;; bot opening
                       ((and (>= x 1)(<= x wid)(>= y 1)(<= y hgt)) (put "."))
                       (t (put "#") ;; wall
                          ))))))
  (put "~%"))


                     
(defun test ()
  (show-game `((5 5)(1 0)(1 2)(4 4))))

(defun advance (state)
  (destructuring-bind ((wid hgt)(ex ey) (wx wy) (wx2 wy2)) state
    (setq wx (+ wx 1))
    (setq wy2 (+ wy2 1))
    (when (> wx wid) (setq wx 1))
    (when (> wy2 hgt) (setq wy2 1))
    ;; new state
   (list (list wid hgt)(list ex ey)(list wx wy)(list wx2 wy2))))


(defparameter *state* `((5 5)(1 0)(1 2)(4 4)))
(defun next () 
  (show-game *state*)
  (setq *state* (advance *state*)))

(defun run ()
  (loop for i from 1 to 11 do
        (next)))


    