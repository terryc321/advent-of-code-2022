

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...

;; regular expression
(use-modules (ice-9 regex)) 

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

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

(define *debug* #f)

(define input #f)

(define (get-input filename)
  (let ((port (open-input-file filename)))
    (set! input (read port))
    (close-input-port port)))

;; for example
(define input #f)

;;(get-input "input")
(get-input "input2")



#|
1, 2, -3, 3, -2, 0, 4

circular list forwards and backwards
or is it just a vector ?


(define v (make-array 0 7))
(array-set! v 1 0)
(array-set! v 2 1)
(array-set! v -3 2)
(array-set! v 3 3)
(array-set! v -2 4)
(array-set! v 0 5)
(array-set! v 4 6)
v
|#

" need to move a digit left or right a number of places

#(1 2 -3 3 -2 0 4)

#(1 2 -3 3 -2 0 4)
#(0 1  2 3  4 5 6)

#(1 2 -3 3 -2 0 4)
#(0 1  2 3  4 5 6)


"

" circular list ? next + prev ? "
 
" how go backwards through a list ?
 by going forwards in list that is reversed ?

if have a list v (1 2 -3 3 -2 0 4)
have a list rev-v (4 0 -2 3 -3 2 1)

make them circular
give them unique id's 

probably want to copy list 

"
(define (copy xs)
  (define (it ys rs)
    (cond
     ((null? ys) rs)
     (#t (it (cdr ys) (cons (car ys) rs)))))
  (reverse (it xs '())))

(define (mutate p)
  (set-car! p 3))

(define p (list 0 1 2 3))
(define p2 p)
(define p3 (copy p))

p
p2
p3

(mutate p)
p
p2
p3

(define (ident xs)
  (let ((n 0))
    (letrec ((foo (lambda (ys rs)
		    (cond
		     ((null? ys) rs)
		     (#t
		      (set! n (+ n 1))
		      (foo (cdr ys) (cons (list 'id n 'val (car ys)) rs)))))))
      (reverse (foo xs '())))))


(define (doubly xs)
  (let* ((v (copy xs))
	 (v2 (copy (reverse v))))
    (values v v2)))

(doubly p3)

(define pat (doubly input))

pat

"
object position

obj pos
 a  1     b 2   c 3   d  4

 a forward 1
 a  2    b  2   c  3   d 4

 b  1    a  2   c  3   d 4
 
"

"
double singly linked list
first one in sequence for forward numbers

second one in sequence backwards for backwards numbers

scheme@(guile-user) [1]> input
$27 = (1 2 -3 3 -2 0 4)
scheme@(guile-user) [1]> (doubly input)
$28 = ((id 1 val 1) (id 2 val 2) (id 3 val -3) (id 4 val 3) (id 5 val -2) (id 6 val 0) (id 7 val 4))
$29 = ((id 7 val 4) (id 6 val 0) (id 5 val -2) (id 4 val 3) (id 3 val -3) (id 2 val 2) (id 1 val 1))


 make proper list passed in into a circular list
    probably play havoc if it gets evaluated as it will never end if it gets printed
  
  
"

(define (all-unique xs)
  (let ((my-hash (make-hash-table)))
    (letrec ((foo (lambda (ys)
		    (cond
		     ((null? ys) #t)
		     (#t (let ((fred (car ys)))
			   (cond
			    ((hash-ref my-hash fred)
			     (format #t "duplicate entry for ~a ~%" fred)
			     #f)
			    (#t
			     (hash-set! my-hash fred fred)
			     (foo (cdr ys))))))))))		   
      (foo xs))))

#|
scheme@(guile-user)> (all-unique input)
duplicate entry for 4965 
$50 = #f
scheme@(guile-user)>

there is a clash , give each item a unique id 
|#


" make a copy of original list as we will mutate the cons cells in drastic style

  bug in recursion calling make-circular instead of foo
"
(define make-circular
  (lambda (ys)
    (let* ((dup (copy ys)))
      (letrec ((foo (lambda (xs)
		      (cond
		       ((null? (cdr xs)) (set-cdr! xs dup))
		       (#t (foo (cdr xs)))))))
	(foo dup)
	dup))))



	


"
same idea as multiple-value-bind and values in common lisp

call-with-values thunk-makes-values lambda-takes-values 
scheme@(guile-user) [2]> (call-with-values (lambda () (doubly input)) (lambda (a b) (list a b)))
$35 = (((id 1 val 1) (id 2 val 2) (id 3 val -3) (id 4 val 3) (id 5 val -2) (id 6 val 0) (id 7 val 4)) ((id 7 val 4) (id 6 val 0) (id 5 val -2) (id 4 val 3) (id 3 val -3) (id 2 val 2) (id 1 val 1)))

call forward list north
call backwards list south

baz is just (map bar input) except dont collect results ,
so not exactly same i guess...

item is just an integer ... fingers crossed values are all unique ...
we could check this is the case i guess with hash table ...


data has (id N val VALUE) format 
N integer
VALUE integer

input data originally
(-8023
 4822
 4250
 -6738
 3764
.........

problem input looks like this after function ident applied to it

((id 1 val -8023)
 (id 2 val 4822)
 (id 3 val 4250)
 (id 4 val -6738)
 (id 5 val 3764)
.........

call bar on every (id N val VAL) in the original input
 north is 

"

"search forward through xs to find item - possibly an infinite loop if look for something not in list
so we stake at head of list and if we meet it and not first-traversal then know not in list
and should stop and tell teacher... not in list...

wrap result in a lambda as displaying it will go haywire
"
(define (find-item item xs)
  ;;(format #t "find-item: item = ~A : ~a~%" item (pair? xs))
  (let ((stake xs)
	(first-traversal #t))
    (letrec ((foo (lambda (ys)
		    (cond
		     ((null? ys) (error  "find-item null ys - expected ys to be infinite circular list??"))
		     ((and first-traversal (eq? stake ys) (equal? (car ys) item))
		      ys)
		     ((equal? (car ys) item) ys)		     
		     (#t (set! first-traversal #f)
			 (foo (cdr ys)))))))
      (let ((result (foo xs)))
	;;(format #t "find-item: item found ~%")
	result))))


#|
scheme@(guile-user)> (find-item 1 '(5 4 3 2 1 0 -1 -2 -3 -4))
find-item: item = 1 : #t
find-item: item found 
$53 = (1 0 -1 -2 -3 -4)
|#



"may find that gof and gob are exactly the same with arguments reversed"

" turtle graphics in scheme
embed scheme interpreter in a SDL application
- OR -
auto generate sufficiently complex piece of code into C code
compile C code
and use that as the interface to guile scheme

try chickadee seems fast as gui input output ??


go forward

given id  ......... search for that id on circ-north and circ-south

GOF go forward found item that will be moving
     ---->   A 
             C <-----

i do not want to find item itself but where it is on the cons cell rope if will
want parent cons cell , not cons cell CAR contents

$59 = #t
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 3)
$60 = (4 5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 0)
$61 = (1 2 3 4 5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 1)
$62 = (2 3 4 5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 2)
$63 = (3 4 5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 3)
$64 = (4 5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 3)
$65 = (4 5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 4)
$66 = (5)
scheme@(guile-user) [10]> (go-forward '(1 2 3 4 5) 5)
$67 = ()
scheme@(guile-user) [10]> (doubly input)
$68 = (1 2 -3 3 -2 0 4)
$69 = (4 0 -2 3 -3 2 1)
scheme@(guile-user) [10]> (make-circular (doubly input))
$70 = (1 2 -3 3 -2 0 4 . #-6#)
scheme@(guile-user) [10]> (go-forward (make-circular (doubly input)) 20)
$71 = (4 1 2 -3 3 -2 0 . #-6#)
scheme@(guile-user) [10]> (go-forward (make-circular (doubly input)) 30)
$72 = (-3 3 -2 0 4 1 2 . #-6#)
scheme@(guile-user) [10]> (go-forward (make-circular (doubly input)) 40)
$73 = (0 4 1 2 -3 3 -2 . #-6#)
scheme@(guile-user) [10]> (go-forward (make-circular (doubly input)) 4343)
$74 = (3 -2 0 4 1 2 -3 . #-6#)
scheme@(guile-user) [10]>


"
;; go forward
(define go-forward
  (lambda (xs n)
    (cond
     ((zero? n) xs)
     (#t (go-forward (cdr xs) (- n 1))))))

#|

i was swapping item at position A with item at position B

wrong !

should be treating it like a linked list

suppose want to move A two spaces forward

  xs = circular list  A B C D E F ... A B C D E F ... A B C D E F  ... 

circle round until (car (cdr xs)) = A
ie at  "F" where car (F) = "F"
  cdr(F) = [A | ...]

set-cdr! F = B
removes A from connection

set-cdr! C = A
set-cdr! A = D

so list now looks like
      xs = B C A D E F ...B C A D E F ...B C A D E F ...B C A D E F ...


|#

(define gof
  (lambda (item c s)
    (format #t "GOFF ~a : ~a : ~a ~%" item c s)
    (let* ((id (second item))
	   (val (fourth item))
	   (A (find-item item c))
	   (C (find-item item s)))
      (format #t "item = ~a : ~%A = ~a ~%C = ~a ~%"
	      item
	      (map fourth (take (length input) A))
	      (map fourth (take (length input) C)))
      (let* ((target (go-forward c val))
	     (target-item (car target)))
	(format #t "target = ~A ~%" target)

	(set-car! target item)
	(set-car! A target-item)

	(let ((D (find-item target-item s)))
	  (set-car! D item)
	  (set-car! C target-item)
	  #t)))))

	  


;; go backward -- see if can just use symmetry to solve it
(define gob
  (lambda (item c s)
    (format #t "GOFF ~a : ~a : ~a ~%" item c s)
    (let* ((id (second item))
	   (val (fourth item))
	   (A (find-item item s))
	   (C (find-item item c)))
      (format #t "item = ~a : ~%A = ~a ~%C = ~a ~%"
	      item
	      (map fourth (take (length input) A))
	      (map fourth (take (length input) C)))
      ;; val is negative , we want use go forward ...
      (let* ((target (go-forward s (abs val)))
	     (target-item (car target)))
	(format #t "target = ~A ~%" target)

	(set-car! target item)
	(set-car! A target-item)

	(let ((D (find-item target-item c)))
	  (set-car! D item)
	  (set-car! C target-item)
	  #t)))))




(define take
  (letrec ((foo (lambda (xs ys n)
		  (cond
		   ((null? xs) ys)
		   ((> n 0) (foo (cdr xs) (cons (car xs) ys) (- n 1)))
		   (#t ys)))))
    (lambda (n xs)
      (reverse (foo xs '() n)))))
    
     

(define foo
  (lambda ()
    (let* ((data (ident input)))
      (call-with-values (lambda () (doubly data))
	(lambda (north south)
	  (let ((inf-north (make-circular north))
		(inf-south (make-circular south)))
	    (format #t "~%")
	    (format #t "inf-north now : ~a ~%"
		    (map fourth (take (length input) inf-north)))
	    (format #t "inf-south now : ~a ~%"
		    (map fourth (take (length input) inf-south)))	    
	    (letrec ((bar (lambda (item)
			    (let ((val (fourth item)))
			      (format #t "~%~%working on item ~a : val = ~a ~%" item (fourth item))
			      (cond
			       ((not (integer? val)) (error "expected VAL to be integer"))
			       ((positive? val) (gof item inf-north inf-south))
			       ((negative? val) (gob item inf-north inf-south))
			       (#t "its zero. - no action"
				   #t))
			      )))
		     (baz (lambda (xs)
			    (cond
			     ((null? xs) #t)
			     (#t (bar (car xs))
				 (format #t "inf-north now : ~a ~%"
					 (map fourth (take (length input) inf-north)))
				 (format #t "inf-south now : ~a ~%"
					 (map fourth (take (length input) inf-south)))
				 (baz (cdr xs)))))))
	      (baz data)
	      )))))))


#|
(call-with-values (doubly (ident input))
  (lambda (a b)
    (call-with-values (lambda () (values (make-circular a) (make-circular b)))
      (lambda (a b)
	(gof (second a) a b)))))
|#



  

    
	      


(define p (list 1 2 3))

(set-cdr! (cdr (cdr p)) p)
(take 100 p)

(define q (copy input))

q

(set-cdr! (cdr (cdr (cdr (cdr (cdr (cdr q)))))) q)

(take (* 2 (length input)) q)
 '(1 2 -3 3 -2 0 4 1 2 -3 3 -2 0 4)
input
 '(1 2 -3 3 -2 0 4 1 2 -3 3 -2 0 4)


#|


EMACS is woefully slow , if could get another way for Geiser Guile REPL to
Geiser to work then be helpful i think

dasdcasdcsadcin a shell running ,time (ident input) takes 0.15 second
in emacs same command takes 1 second - ikes

text editor repl where cursor can go anywhere not locked to side of screen


 (id 4997 val 308)
 (id 4998 val -1646)
 (id 4999 val -6308)
 (id 5000 val 5838))
;; 0.159592s real time, 0.466364s run time.  0.371803s spent in GC.

 (id 4998 val -1646)
 (id 4999 val -6308)
 (id 5000 val 5838))
;; 1.004088s real time, 0.872569s run time.  0.720367s spent in GC.
scheme@(guile-user)>


|#

#|

test circular lists

scheme@(guile-user)> (define p (list 1 2 3))
scheme@(guile-user)> (define q (make-circular p))
scheme@(guile-user)> q
$38 = (1 2 3 . #-2#)
scheme@(guile-user)> (take 100 q)
$39 = (1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1)
scheme@(guile-user)>

|#
