

#|

interesting  with a bit of planning can isolate regions in which to avoid as it
leads nowhere

could optimise the finding which ways we can go

visualisation tool be very handy

even if get a glimpse of where it is
no need for 

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi

v..v<<<<
>v.vv<<^
.>vv>E^^
..v>>>^^
..>>>>>^

Interface

at  : board x y -> char
hgt : board x y -> int

allow     : int int -> bool  
left-p    : board x y -> bool
right-p   : board x y -> bool
up-p      : board x y -> bool
down-p    : board x y -> bool

visit-p   : x y path  -> bool
seek      : board int int int int list  int -> ?
                   x   y   xE  yE  path nsteps


|#

(defpackage :aoc22
  (:use :cl))

(in-package :aoc22)

(defun example-board () 
  (list
  "Sabqponm"
  "abcryxxl"
  "accszExk"
  "acctuvwj"
  "abdefghi"))


(defun problem-board ()
  (list
"abaaaaaccccccccaaaaaaaaaaccccaaaaaaaaaccccaacccccccccccccccccccccaaaaaaaaaaaccaaaaaaaaaccccccccccaaaaaaaacaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa"
"abaaaaaaaccccccaaaaaaaaaacccccaaaaaacccccaaaacccccccccccccaacccccaaaaaaaaaaaacaaaaaaaaacccccccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccccaaaa"
"abacaaaaaccccccccaaaaaacccccccaaaaaacccccaaaacccccccccccccaacccaaaaaaaaaaaaaacaaaaaaaaccccccccccaaaaaaaaaaaaaaaaccccccccccccccccccccccccccaaaccccccccccccccaaaa"
"abccaacccccccccccaaaaaaccccccaaaaaaacccccaaaacccaacccccaaaaaaaaaaaaaaaaaaaaacaaaaaaaccccccccccccacaaaaaccaaaaaaaccccccccccccccccccccccccccaaccccccccccccccaaaaa"
"abcaaccccccccaaaaaaaaaaccccccaaacaaaccccccccccccaaaaaccaaaaaaaaaaaaaaaaaaaaaccaccaaacccccccccccccccaaaacccaaaaaaccccccccccccccccccccccccccaaacccccccccccccaaaca"
"abcccccccccccaaaaaaaaaaaaacccccccccacccccccccccaaaaacccccaaaacaaaaaaaaaaaaccccaaaaaaccccccccaaacccccaaccccaacccccccccccccccccccccccaaaaccaaaccccccccccccccccccc"
"abccccccccccaaaaaaccccaaaacccccccccccccccccccccaaaaaaccccaaaaaaaaaaaaaacaaacccaaaaaaaacccccaaaaaacccccccccccccccccccccccccccccccccccaaaaaaaaacccccccccccccccccc"
"abacccccccccaaaaaacccccaaacaaacccccccccacccaaccccaaaacccaaacaaaaaaaaaaacccccccaaaaaaaacccccaaaaaaccccccccccccccccccccccccccccccccjjjjjjjaaaaaaaaaccccaacccccccc"
"abaccccccccccaaaaaccaaaaaaaaaaccaccccccaacaaacccaaccccccaacccaaaaaaaaaaacccccccaaaaaaacccccaaaaaccccccccccccccccccccccccccccccccijjjjjjjjjhhhhhhhhhcaaaaaaccccc"
"abaacccccccccaaaccccaaaaaaaaaaaaaccccccaaaaacccccccccccccccccccccaaaaaaacccccccaaaaaccccccccaaaaacccccccccccccccaaaaccccccccccciijjjjjjjjjhhhhhhhhhhcaaaaaccccc"
"abaaccccccccccccccccccaaaaaacaaaaaacccccaaaaaacccccccccccaaacccccaaaccccccccccaaaaaaccccccccaacaacccccccccccccccaaaaccccccccccciiioooooojjhhhhpphhhhhaaaaaccccc"
"abacccccccccccccccccccaaaaaaccaaaaacccaaaaaaaacccccccccccaaaaaaccaacccccccccccccccaaccccccccacccccccccccccccccccaaaaccccccccccciiioooooooooopppppphiiaaaaaacccc"
"abaccccccccccaaaaaccccaaaaaaaaaaaaccccaaaaaaaacccccccccaaaaaaaaccccccccccccaaaaccccccccccccaaacccccccccccccccccccaaccccccccccciiinnoouuooooopppppppiiaaaaaacccc"
"abcccccccccccaaaaaccccaaacaaaaccaacccccccaaccccccccccccaaaaaaaaccccccccccccaaaaccccccccaaacaaacccccccccccccccccccccccccccccccciiinnotuuuuoopuuuupppiiaaacaccccc"
"abccccccccccaaaaaaccccaccccccccccccccccccaaccccccccccccaaaaaaacccccccaaacccaaaaccccccccaaaaaaaaaacccaacccccccccccccccccccccccciiinntttuuuuuuuuuuuppiiiaaccccccc"
"abaaccccccccaaaaaaccccccccccccccccccaaaacccccccccccccccccaaaaaacccccaaaaccccaaccccccccccaaaaaaaaacccaaacaaacaaaccccccccccaaccciiinntttxxuuuuuuuuuppiiiccccccccc"
"abaacccccccccaaaaaccccccccccccccccccaaaaccccccccaccccccccaaaaaacccccaaaaccccccccccccccccccaaaaacccccaaaaaaacaaaacccccccccaaaaiiiinnttxxxxuuyyyuvvppiiiccccccccc"
"abaacccccccccaaaccccccccccccccccccccaaaaccaaacaaaacccccccaaccccccccccaaacccccccccccccccccaaaaaaccccccaaaaaacaaaacccccccccaaaaiiinnnttxxxxxxyyyvvppqiiiccccccccc"
"abaccccccccccccccccccccaaccccccccccccaacccaaaaaaaacccccccccccccccccccccccccccccccccccaacaaaaaaacccaaaaaaaaccaaaccccccccaaaaahhhinnntttxxxxxyyyvvqqqiiiccccccccc"
"abcccccccccccccccccccaaaaaaccccccccccccccccaaaaaaaaaccccccccccccccccccccccccccccccaaaaaaaaacaaacccaaaaaaaaaccccccccccccaaaaahhhnnnttttxxxxyyyvvvqqqiiiccccccccc"
"SbcccccccccccccccccccaaaaaaccccccccccccccccaaaaaaaaaccccccccccccccccccccccccccccccaaaaacccccccacccaaaaaaaaaacccccccccccccaahhhnnntttxxxEzzzyyyvvvqqqjjjcccccccc"
"abcccccccccccccccccccaaaaacccccccccccccaaaaaaaaaaaacccccccccccccccccccccccccccccccaaaaaaaccccccccccccaaacaaacccccccccccccahhhmmmtttxxxxxyyyyyyyvvvqqqjjjccccccc"
"abccccccccccccccccccccaaaaacccccccccaacaaaaaaaaaaaaccccaccaaaccccccccccccccccccccaaaaaaaaccccccccccccaaacccccccccccccaaccahhhmmmtttxxxyywyyyyyyvvvqqqjjjccccccc"
"abccccccccccccccccccccaaaaacccccccccaaaaaaaacaaacccccccaaaaaaccccaccaaaccccccccccaaaaaaaaccccccccccccaacccccccccccccaaaccchhhmmmsssxxwwwyyywyyvvvvqqqjjjccccccc"
"abccaacccccccccccccccccccccccccccccccaaaaaaccaaacccccccaaaaaaccccaacaaacccccccccccacaaacccccccccccccccccccccccccaaaaaaaccchhhmmmssssswwwwyywwvvvvvqqqjjjdcccccc"
"abccaaaccaaccccccccccccccccccccccccaaaaaaaaccaaccccccccaaaaaaacccaaaaaccccccccccccccaaacccccccccccccccccccccccccaaaaaaaaaahhhmmmmsssssswwywwwrvvqqqqqjjjdddcccc"
"abccaaaaaaacccccaaaccccccccccccccccaaaaacaacccccccccccaaaaaaaaccccaaaaaaccccccccaaaccccccccccccccccccccccccccccccaaaaaaaaahhhgmmmmmsssswwwwwrrrrrqqqjjjjdddcccc"
"abcccaaaaaacccccaaacacccccccccccccccccaaaccaacccccccccaaaaaaaaccaaaaaaaacaaccccaaaacccccccccccccccccccccccccccccccaaaaaaaccggggmmmmmmssswwwwrrrrrkjjjjjddddcccc"
"abaaaaaaaaccccaacaaaaaccccaaacccccccccaaaccaaccccccccccccaaaccccaaaaacaaaaaccccaaaacccccccccccaacccccccccccaaccccaaaaaacccccgggggmmmllssswwrrrkkkkkjjjddddacccc"
"abaaaaaaaaacccaaaaaaaaccccaaaacccccccccaaaaaaccccccccccccaaacccccccaacccaaacaaacaaaccccccccccaaaacccccccaaaaaacccaaaaaaacccccgggggglllsssrrrrrkkkkkkdddddaacccc"
"abaaaaaaaaaacccaaaaaccccccaaaaccccccccccaaaaaaaccccccaaccccccccccccaaaaaaaaaaaaccccccccccccccaaaacccccccaaaaaccccaaccaaacccccccggggglllsrrrrrkkkeeedddddaaccccc"
"abaacaaaaaaaccccaaaaacccccaaaccccccccccccaaaaaaccccaaaaccccccccccccccaaaaaaaaacccccccccccccccaaaacccccccaaaaaaacccccccaacccccccccgggglllrrrrkkkeeeeeddaaaaccccc"
"abaaaaaacccccccaaacaacccccccccccccccccccaaaaaccccccaaaaaaccccccccaaacccaaaaacccccccccccccccccccccccccccaaaaaaaacccccccccccccccccccggfllllllkkkeeeeeccaaaaaacccc"
"abaaaaacccccccccaacccccccccccccccccccccaaaaaacccccccaaaacccccacccaaccccaaaaaaccccccccccccccccccccccccccaaaaaaaacccccccccccaacccccccffflllllkkkeeeccccaaaaaacccc"
"abaaaaacccccccccccccccccccccccccccaacccccccaaccccccaaaaacccccaaaaaaaccaaaaaaaaaaccccccccccccccccccccccccacaaacccccccccaaccaaccccccccfffllllkeeeecccccaacccccccc"
"abaaaacccccccccccccccccccccccccccaaacccccccccccccccaacaacccccaaaaaaccaaaaacaaaaaccccccccccccccccccccccccccaaacccccccccaaaaaaccccccccffffffffeeeeccccccccccccccc"
"abaaaccccccccccccccccccccccccccccaaaaacacccccccccccccccccccccaaaaaaaaaaaaccaaaaaaaaccccccaaccccccccccaaaaacccccccccccccaaaaaaacccccccfffffffeeaaccccccccccccaaa"
"abaacccccaacaacccccccccccccaacaaaaaaaaaacccccccaaccccccccccccaaaaaaaaaaacccaaaaaaaacccccaaacaacccccccaaaaaccccccccccaaccaaaaaaccccccccafffffeaacccccccccccccaaa"
"abaaaccccaaaaacccccccccccccaacaaaaaaaaaaccccccaaaccccccccccccaaaaaaaaaaaccccaaaaaccccccccaaaaaccccccaaaaaacccccccaacaaaaaaaaccccccccccaaacccccccccccccccccccaaa"
"abccccccccaaaaacccccccccaaaaaaaaaaaaaacccccaaaaacaaccccccaaaaaaaaaaaaaaaaaaaaaaaaacccccaaaaaacccccccaaaaaacccccccaaaaaaaacaaccccccccccaaaccccccccccccccccaaaaaa"
"abcccccccaaaaaacccccccccaaaaaaaaaaaaaacccccaaaaaaaaccccccaaaaacaaaaaaaaaaaaaaaaaaacccccaaaaaaaacccccaaaaaaccccccaaaaaaaaccaacccccccccccccccccccccccccccccaaaaaa"
  
))



;; change selected board
(defun board ()
  (problem-board))



(defun board-width (bd)
  (length (car bd)))

(defun board-height (bd)
  (length bd))


;; gets the lisp character in string
(defun at (bd x y)
  (labels ((char-xy (bd x y)
	     (char (nth (- (board-height bd) y) bd)
		   (- x 1))))
    (char-xy bd x y)))



;; gets the height in terms of problem
(defun hgt (bd x y)
  (cond
    ((and (>= x 1) (<= x (board-width bd))
	  (>= y 1) (<= y (board-height bd)))
     (let* ((ch (at bd x y))
	    (code (char-code ch)))
       (cond
	 ((char= ch #\S) (char-code #\a))
	 ((char= ch #\E) (char-code #\z))
	 (t code))))
    (t 99999)))


#|
allowed to go if where are is higher than where want to go
allowed to go if where are is at same level
allowed to go if where are is one level below where to go
cannot go if where want to go is more than one level
|#
(defun allow(here there)
  (cond
    ((> here there) t) 
    ((= here there) t)
    ((= here (- there 1)) t)
    (t nil)))


(defun left-p (bd x y)
  (let ((x2 (- x 1))
	(y2 y))
    (let ((here (hgt bd x y))
	  (there (hgt bd x2 y2)))
      (allow here there))))

(defun up-p (bd x y)
  (let ((x2 x)
	(y2 (+ y 1)))
    (let ((here (hgt bd x y))
	  (there (hgt bd x2 y2)))
      (allow here there))))

(defun right-p (bd x y)
  (let ((x2 (+ x 1))
	(y2 y))
    (let ((here (hgt bd x y))
	  (there (hgt bd x2 y2)))
      ;;(format t "here = ~a : there = ~a ~%" here there)
      (allow here there))))


(defun down-p (bd x y)
  (let ((x2 x)
	(y2 (- y 1)))
    (let ((here (hgt bd x y))
	  (there (hgt bd x2 y2)))
      (allow here there))))


;; #\S is the start
;; problem board - no #\S start position ? top left then
;;; <<<<<<<<<<< find-start
(defun find-start (bd)
  (catch 'found
  (loop for x from 1 to (board-width bd) do
    (loop for y from 1 to (board-height bd) do
      (when (char= (at bd x y) #\S)
	(throw 'found (list x y)))))
  nil))


;; #\E is the finish
(defun find-finish (bd)
  (catch 'found
  (loop for x from 1 to (board-width bd) do
    (loop for y from 1 to (board-height bd) do
      (when (char= (at bd x y) #\E)
	(throw 'found (list x y)))))
  nil))

(defun visit-p (x y path)
  (member (list x y) path :test #'equalp))

(defparameter *solutions* '())
(defparameter *sorted-solutions* '())

(defun seek (bd x y xE yE path steps)
  (format t "~a ~a ~a~%" steps x y)
  ;;(format t "~a ~a : ~a : ~a" x y path steps)
  ;;(format t "at ~a ~a~%" x y)
  (cond
    ((and (= x xE) (= y yE))
     ;;(format t "solution ~a ~a~%" path steps)
     (setq *solutions* (cons (reverse (cons (list x y) path)) *solutions*)))
    ((visit-p x y path)
     ;;(format t "backtrack ... old path..~%")
     t
     )
    (t

     (cond
       ((left-p bd x y)
	;;(format t "moving left ! ~%")
	(seek bd (- x 1) y xE yE (cons (list x y) path) (+ 1 steps)))
       (t
	;;(format t "barred left ! ~a ~a ~%" x y)
	))

     (cond
       ((right-p bd x y)
	;;(format t "moving right ! ~%")
	(seek bd (+ x 1) y xE yE (cons (list x y) path) (+ 1 steps)))
       (t
	;;(format t "barred right ! ~a ~a ~%" x y)
	))

     (cond
       ((down-p bd x y)
	;;(format t "moving down ! ~%")
	(seek bd x (- y 1) xE yE (cons (list x y) path) (+ 1 steps)))
       (t
	;;(format t "barred down ! ~a ~a ~%" x y)
	))

     (cond
       ((up-p bd x y)
	;;(format t "moving up ! ~%")
	(seek bd x (+ y 1) xE yE (cons (list x y) path) (+ 1 steps)))
       (t
	;;(format t "barred up ! ~a ~a ~%" x y)
	))
     
     )))

(defun solve(bd)
  (destructuring-bind (xS yS) (find-start bd)
    (destructuring-bind (xE yE) (find-finish bd)
      ;; (format t "xS yS = ~a ~a ~%" xS yS)
      ;; (format t "xE yE = ~a ~a ~%" xE yE)      
      (let ((path '())
	    (steps 0))
	(setq *solutions* nil)
	(seek bd xS yS xE yE path steps)
	(setq *sorted-solutions*
	      (sort *solutions* (lambda (x y)(< (length x)
						(length y)))))
	(setq *solutions* *sorted-solutions*)
	*solutions*))))



#|
barred up ! 1 5 
32
AOC22> (length (car *solutions*))
40
AOC22> (length *solutions*)
30
AOC22> (defparameter sorted (sort *solutions* #'(lambda (x y)(< (length x)(length y)))))
SORTED
AOC22> 
; No value
AOC22> (length sorted)
30
AOC22> (length *solutions*)
16
AOC22>

destructively damaged *solutions* even though i did not intend for that
to happen

path of 32 locations
START -> loc2 -> loc 3 ... -> FINISH loc 32
there are 31 steps

remove destructive too ??

|#

;;(solve (board))



