

#|

Advent of code 2022 - Day 12

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi

2d array replace S with ? ; E with ?
S has elevation a
E has elevation z
can go up right left down
start at S
finish at E
minimize steps


scheme
loops for loop ?
arrays in chicken scheme 2d ?


arr x y => element or Nothing

ingrid arr x y ?
width arr ?
height arr ?
square arr ? - assumed square 2 d grid , may be triangular or circular 

what is top left coordinate 1 1 ? or 0 0 ?

suppose path from S to E in order
.... step1 ; step2 ; step3

without knowing anything position of start to end - (0 20) to (135 20)
135 steps as crow flies horizontally
minimum number of steps can be ignoring movement rules is 135 steps
with movement rules , (impossible) or (possible) more steps required

|#

(import (srfi 63)) ;; generalised arrays
;; (import (srfi 69)) ;; hash tables
(import (chicken format)) ;; format

(define input (list "Sabqponm"
		    "abcryxxl"
		    "accszExk"
		    "acctuvwj"
		    "abdefghi"))


(define input (list
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
"abcccccccaaaaaacccccccccaaaaaaaaaaaaaacccccaaaaaaaaccccccaaaaacaaaaaaaaaaaaaaaaaaacccccaaaaaaaacccccaaaaaaccccccaaaaaaaaccaacccccccccccccccccccccccccccccaaaaaa"))

;; integrity checks
;; only one S in input , atleast one , no more than one S
;; only one E in input , atleast one , no more than one E

;; start end locations 
(define *start* #f)
(define *end* #f)
(define *start-count* 0)
(define *end-count* 0)


(define *width* (string-length (car input)))
(define *height* (length input))
(define *array* (make-array #(#f) *width* *height*))
(define *array2* (make-array #(#f) *width* *height*))



;; given string of some length len ,x y index , iterate over string until no more chars
(define (mlfoo x y str)
  (let ((len (string-length str)))
    (let loop ((si 0))
      (let ((ch (string-ref str si)))
	;; 
	(cond
	 ((char=? ch #\S) ;; S = a 
	  (set! *start* (list (+ x si) y))
	  (set! *start-count*  (+ 1 *start-count*))
	  (format #t "char at ~a ~a is ~a ~%" (+ x si) y #\a)
	  (array-set! *array* #\a (+ x si) y)
	  )
	 ((char=? ch #\E) ;; E = z
	  (format #t "char at ~a ~a is ~a ~%" (+ x si) y #\z)
	  (array-set! *array* #\z (+ x si) y)
	  (set! *end-count*  (+ 1 *end-count*))
	  (set! *end* (list (+ x si) y)))
	 (#t
	  (format #t "char at ~a ~a is ~a ~%" (+ x si) y ch)
	  (array-set! *array* ch (+ x si) y)))
	;;
	(if (>= (+ 1 si) len)
	    #f
	    (loop (+ si 1)))))))


(define (mlgoo xs)
  (define (mlgoo2 y xs)
    (cond
     ((null? xs) #f)
     (#t (mlfoo 0 y (car xs))
	 (mlgoo2 (+ y 1) (cdr xs)))))
  (mlgoo2 0 xs))



(define (show-array)
  (let* ((dim (array-dimensions *array*))
	 (wid (car dim))
	 (hgt (car (cdr dim))))
    (letrec ((show-array2
	      (lambda (x y)
		(cond
		 ((>= y hgt) #f)
		 ((>= x wid) (format #t "~%")
		  (show-array2 0 (+ y 1)))
		 (#t
		  (format #t "~a" (array-ref *array* x y))
		  (show-array2 (+ x 1) y))))))			    
      (show-array2 0 0))))

;;(mlfoo 0 0 (car input))
(mlgoo input)

(show-array)

;; breadth first search , start at some position , if we get to some other position ...
;; do we need the path or just a value , then number of steps

(define (onboard? x y)
  (and (>= x 0)(< x *width*)(>= y 0)(< y *height*)))

(define (elev x y)
  (let ((ch (array-ref *array* x y)))
    (- (char->integer ch)
       (char->integer #\a))))


;; cango from (x y) to (x2 y2) IF
;; (x y) onboard
;; (x2 y2) onboard
;;
;; MOTION itself either VERTICAL or HORIZONTAL not both DIAGONAL
;;
;; only move up down or left right
;; ie change x must be one and y must be zero
;; OR change x must be zero and y change must be one
;; dont allow to sit still - blow up space
;;
;; also only go from (x y) to (x2 y2) IF x2 y2 is at elevation + 1
(define (cango? x y x2 y2)
  (and
   (onboard? x y)
   (onboard? x2 y2)
   (or (and (= 1 (abs (- x2 x))) (= 0 (abs (- y2 y))))
       (and (= 0 (abs (- x2 x))) (= 1 (abs (- y2 y)))))
   (<= (- (elev x2 y2)(elev x y)) 1)))


(define (breadth work todo)
  (cond
   ((null? work) (cond ;; make todo the work , with empty todo
		  ((null? todo) #f)
		  (#t (breadth todo '()))))
   (#t (let* ((state (car work))
	      (x (car state))
	      (y (car (cdr state)))
	      (step (car (cdr (cdr state)))))
	 (letrec ((simp
		   (lambda (x2 y2)
		     (cond
		      ((onboard? x y)
		       (let ((oldstep (array-ref *array2* x y)))
			 (cond
			  ((not oldstep) (array-set! *array2* step x y))
			  ((< oldstep step) #f)
			  (#t ;; allow continue
		       
		     (when (cango? x y x2 y2)
		       (let ((oldstep (array-ref *array2* x2 y2)))
			 (when (or (not oldstep)(< (+ step 1) oldstep))
			   (array-set! *array2* (+ step 1) x2 y2)
			   ;; alert
			   (when (and (= x2 (car *end*))(= y2 (car (cdr *end*))))
			     (format #t "can reach *end* in ~a steps ~%" (+ step 1)))
			   ;; 
			   (set! todo (append todo (list (list x2 y2 (+ step 1))))))))))))))))	   
	   ;; left 
	   (let ((x2 (- x 1))(y2 y)) (simp x2 y2))
	   ;; right
	   (let ((x2 (+ x 1))(y2 y)) (simp x2 y2))
	   ;; up
	   (let ((x2 x)(y2 (- y 1))) (simp x2 y2))
	   ;; down
	   (let ((x2 x)(y2 (+ y 1))) (simp x2 y2))	   
	   ;; next state
	   (breadth (cdr work) todo))))))


(define *end-x* (car *end*))
(define *end-y* (car (cdr *end*)))

(define (visited? x y step)
  (let ((s (array-ref *array2* x y)))
    (cond
     ((not s) #f)
     (#t
      (<= s step)))))



(define (recur x y step)
  (cond
   ((> step 457) #f) ;; know 457 too far
   ((and (= x *end-x*)(= y *end-y*)) (format #t "reached end in ~a steps~%" step))
   (#t
    (letrec ((simp (lambda (x2 y2 step2)
		     (when (and (cango? x y x2 y2) (not (visited? x2 y2 step2)))
		       (array-set! *array2*  step2 x2 y2)
		       (recur x2 y2 step2)))))
    ;; right
      (let ((x2 (+ x 1))(y2 y)(step2 (+ step 1)))
	(simp x2 y2 step2))
      
    ;; left
      (let ((x2 (- x 1))(y2 y)(step2 (+ step 1)))
	(simp x2 y2 step2))
      ;; up
      (let ((x2 x)(y2 (- y 1))(step2 (+ step 1)))
	(simp x2 y2 step2))

      ;; down
      (let ((x2 x)(y2 (+ y 1))(step2 (+ step 1)))
	(simp x2 y2 step2))
      
    ))))


       
       

;; shuvvel a list of states to breadth and let it work through them all
(define (solve)
  (let ((state0 (append *start* (list 0))))
    (breadth (list state0) '())))

(define (solution)
  (solve)
  (array-ref *array2* (car *end*) (car (cdr *end*))))

;; WRONG !!
;; (solution)
;; 457
;;
;; huh ?
;; so where did it go wrong ?
;;
;; tried 456 in case off by one error
;; got a curious message saying was right answer for someone else
;; check logged into right account

(define (solve-r)
  (recur (car *start*) (car (cdr *start*)) 0))

(define (solution-r)
  (solve-r)
  (array-ref *array2* (car *end*) (car (cdr *end*))))


#|

#;2780> (solution-r)
reached end in 457 steps
reached end in 455 steps
reached end in 453 steps
reached end in 451 steps
reached end in 449 steps
reached end in 447 steps
447
#;2792> (solution)
447
#;2794>

ACCEPTED ANSWER !

Interestingly think solved the problem but was using old start coordinate (0 0)
rather than *start* (0 20) so system like scheme allows old data to hang around
on each execution can lead to bad computations

* EXTENSION TASK *

What is the fewest steps required to move starting from any square with elevation a to the location that should get the best signal?

loop over grid , if character is an #\a or elevation is zero equivalent ,
then do search again and look at how many steps it says to get to *end* point

2091 matches against letter a
2092 if we include start symbol S which is also a

|#


(define (find-a)
  (let ((count-a 0)
	(best-step #f))
    (let loop ((x 0)(y 0))
      (cond
       ((>= y *height*) #f)
       ((>= x *width*) (loop 0 (+ y 1)))
       (#t (let ((e (elev x y)))
	     (cond
	      ((= e 0) ;; found a letter a
	       (set! count-a (+ count-a 1))
	       ;; make a new state x y step
	       (let* ((new-start (list x y 0))
		      (new-states (list new-start)))
		 ;; reset *array2*
		 (set! *array2* (make-array #(#f) *width* *height*))
		 ;; breadth first search using new start position
		 (breadth new-states '())
		 ;; look at current steps required reach *end* position
		 (let ((cur-step (array-ref *array2* (car *end*) (car (cdr *end*)))))
		   (cond
		    ;; huh ? cur-step not always possible ?? how come ??
		    ((not (integer? cur-step)) (format #t "not possible(~a ~a)?~%" x y))
		    (#t (when (or (not best-step)(< cur-step best-step))
			  (set! best-step cur-step)
			  (format #t "new best step ~a ~%" best-step)))))))))
	   (loop (+ x 1) y)))
      best-step)))

(define part-2 find-a)

;;
;; (find-a)
;; 446
;;
;; ACCEPTED Answer !








   
    
  



      


