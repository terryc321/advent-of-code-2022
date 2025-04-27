
# README

[X] fun2.lisp - solves part1
[X] bar2.lisp - solves part2


# new look at writing code in common lisp 

go back to baby steps 

so this is interesting , running lisp and scheme code together

lisp has great debugging ? well , it has a stepper - injected with sbcl nonsense

scheme - well , your on your own

rather not have to execute code to see if it is correct 

if want debuggable code need to be able to call code with everything it needs to
compute the result
- in general a lot of information
- need some sort of hash map to pass to
- or optional and default and named arguments
either way , sufficiently more complex than a simple fibonacci call

# keyword arguments 

(defun in-play(&key position width height)
  (destructuring-bind (x y) position
    (cond
      ((and (= x 1)(= y 0)) t)
      ((and (= x width)(= y (+ 1 height))) t)
      ((<= x 0) nil)
      ((>= x (+ width 1)) nil)
      ((<= y 0) nil)
      ((>= y (+ height 1)) nil)
      (t t))))

we can call in-play with 3 arguments position width height in any order

(in-play :position '(503 231) :width 5 :height 5)
(in-play :width 5 :position '(503 231) :height 5)
(in-play :width 5 :height 5 :position '(503 231))

# multiline comments 

ability to wrap comments inside other comments so we can comment block out a big chunk of code
in a few keystrokes

multiline comments to comment out large sections of unfinished code
otherwise compiler or interpreter will start complaining about stuff thats not
currently in scope (considered workable)

# code a simple solution first 

get a working simple solution first 

push complexity away for as long as possible

we could of combined all blizzard procedures together , but make them more efficient
perhaps but at expense of introducing bugs

initial correctness and testing we split them and make them as simple as possible

# 

```lisp

;; a wind or blizzard (same thing here) has position wx wy and direction wdir
;; given some winds and some positions remove those positions that are in blizzards
(defun avoid-blizzards2 (&key positions winds)
  (let ((result '()))
    (loop for wind in winds do
      (destructuring-bind (wx wy wdir) wind
	(loop for position in positions do
	  (destructuring-bind (x y) position
	    (cond
	      ((and (= x wx)(= y wy)) ;; discard position
	       nil)
	      (t ;; keep position
	       (setq result (cons position result))))))))
    result))


(let ((positions '((1 0)(2 3)(4 5)))
      (winds (cdr (cdr *example1*))))
  (avoid-blizzards2 :positions positions :winds winds))
;;=>((4 5) (2 3) (1 0) (4 5) (2 3) (1 0))
;; collecting more and more positions , rather than just keeping positions we have
;; we are making duplicates !

```

here is a corrected version 
``` lisp
(defun avoid-blizzards (&key positions winds)
  (let ((result '()))
    (loop for position in positions do
      (destructuring-bind (x y) position
	(catch 'bliz
	  (loop for wind in winds do
	    (destructuring-bind (wx wy wdir) wind
	      (cond
		((and (= x wx)(= y wy)) ;; discard position
		 (throw 'bliz nil)))))
	  ;; no blizzard conflict - keep position
	  (setq result (cons position result)))))
    result))


(let ((positions '((1 0)(2 3)(4 5)))
      (winds (cdr (cdr *example1*))))
  (avoid-blizzards :positions positions :winds winds))
;;=> ((4 5) (2 3) (1 0))

```

# use previously tested code

```lisp
(defun avoid-duplicate-positions-bad (&key positions width height)
  (let ((grid (make-array (list (+ width 4) (+ height 4)) :initial-element nil))
	(result nil))
       (loop for pos in positions do 
         (destructuring-bind (x y) pos
		 ;; ***
	   (when (and (>= x 0)(>= y 0)(<= x (+ width 1))(<= y (+ height 1))) ;; in grid
	     (let ((ge (aref grid x2 y2)))
		 ;; ***
```

we can replace the array grid check with in-play that means a position in addition 
to be being inside the grid area , it is also not buried in the wall
whereas array check that could be true

```lisp
        ;; ***
       (when (in-play :position pos :width width :height height)
	     (let ((ge (aref grid x2 y2)))
		 ;; ***
```

# hidden crevace

```lisp
(let ((width 5)(height)(x 1)(y 1))
  (loop for y from 1 to height do
    (let* ((wind (list x y '^))
	   (wind2 (next-wind :wind wind :width width :height height)))
      (cond
	((= y 1) (assert (equalp wind2 (list x height '^))))
	(t (assert (equalp wind2 (list x (- y 1) '^))))
	))))

```

complaining that height is nil and not a real 

this is a fancy way of saying that height variable is not initialised properly

in a tighter language , we do not allow a default hidden assignment - must be explicit

# avoid the unexpected

avoid functionality not explicitly asked for and/or leads to weird stuff

default capitalisation on reader in common lisp leads to some weird stuff

```lisp
(equal '(1 2 V) (list 1 2 'v))
T
```

leads to some strange behaviour such as left < or right > or up ^ not having a capitalised form , but down vee v does 

this is unexpected , avoid the unexpected



