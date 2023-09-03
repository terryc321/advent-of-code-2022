
#|

errors 

'tag-79-x-not-implemented hx hy hx2 hy2 tx ty dx dy
(tag-79-x-not-implemented 3  3   2   4   2  2  0  2)


---------------------------------------------------------
read file - get motions

rather than simulating all pieces of rope all at once

run the head through motions to get a path

start piece rope at (1 1) like head
given know head current position , head next position , "tail" current postion
compute tail next position

iterate down "heads" path to generate a path for "tail"

then this "tail" is now our head
next "tail" placed at (1 1) see how it behaves

1 ) run head through commands

head ((1 1)(2 1)(3 1) ...)

2 ) using head's path and tail starts at 1 1 compute tails path

tail-1 ((1 1) ....)  its path

3) using tail-1 path and start tail-2 at 1 1 compute tail-2's path

tail-2 ((1 1) ....)  its path

...
...

tail-9 ((1 1) ...) its path ...

remove duplicate squares reached by tail-9

then count them up

your'e done with day 9 aoc 2022

---------------------------------------------------------
testing the motion is correct - 36 motions to do.

. . .
. X .
. . .

Y can be on a dot or on X
Y can move up , down , left , right

9 starting squares
each 4 moves
total 36 outcomes to cater for.

seems kinda obvious now , but gives a robust way to test
going right direction
---------------------------------------------------------

complexity rises it is simple to get distracted from what would
otherwise be a simple problem

trace path by the head itself
right 4 . . .
up 1 .
left 3 . . .
...

get path  '((1 1)(2 1)(3 1)(4 1) ...)

then tail or 2nd item simply has to figure out where it should be
assume everybody starts at a nominal x=1 y=1  '(1 1) position

simply a reduction rotate left ? map reduce?

|#

;;(use-modules (srfi srfi-1))
(define first car)
(define second (lambda (x) (car (cdr x))))

;; guile error handling??
;; roll your own assert macro
(define-macro (assert x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (if ,g ,g (error (list 'assertion 'failed ,g 'expression 'was ',x))))))



#|

as b moves away , 'a' needs to follow it
a  b . .
a .  b .
. a  b . << ans

ab . . .
a  b . .  no action
a  b . . << ans

. . . ab
. . b a   no action
. . b a  << ans

. . b a
. b . a  move a left
. b a .. << ans



. . X . .

.
.
X
.
.

.
. .
. . X
. . . .
. . . . .

, , , , ,
. . . .
. . X
. .
.

|#

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


;; sufficient for our purposes
;; it could be tidy up something rotten i suppose
(define (follow pos1 pos2 pos3)
  (let ((hx (first pos1))
	(hy (second pos1))
	(hx2 (first pos2))
	(hy2 (second pos2))
	(tx (first pos3))
	(ty (second pos3)))
    
    (when (> (abs (- hx hx2)) 1)
      (dbug "WARN : excessive JUMP HX HX2 by HEAD in excess of 1 space : ~a -> ~a " hx hx2)
      (dbug "WARN : pos1 ~a -> pos2 ~a~%" pos1 pos2))
    
    (when (> (abs (- hy hy2)) 1)
      (dbug "WARN : excessive JUMP HY HY2 by HEAD in excess of 1 space : ~a -> ~a " hy hy2)
      (dbug "WARN : pos1  ~a -> pos2 ~a~%" pos1 pos2))
    
    (dbug "follow : hx hy ~a ~a : hx2 hy2 ~a ~a : tx ty ~a ~a ~%" hx hy hx2 hy2 tx ty)
    (let* ((dx (- hx2 tx))
	   (dy (- hy2 ty)))
      (cond  
       ;; horz 
       ((and (= hy hy2 ty) (= dx 2)) ;; all in line but gap needs closing
	(dbug "tag 42-a active~%") (list (+ tx 1) ty))
       ((and (= hy hy2 ty) (= dx -2)) ;; all in line but gap needs closing
	(dbug "tag 42-b active~%") (list (- tx 1) ty))
       ((and (= hy hy2 ty) (< (abs dx) 2)) ;; all in line and still touching - no action
	(dbug "tag 42-c active~%") (list tx ty))
       ((and (= hy hy2 ty)) ;; all in line , not touching , not in reach  EEEKKKK !!!
	(dbug "tag 42-d active ERR~%") (error (list 'tag-42-d-err hx hy hx2 hy2 tx ty dx dy)))
       ;; vert
       ((and (= hx hx2 tx) (= dy 2)) ;; all in line but gap needs closing
	(dbug "tag 43-a active~%") (list tx (+ ty 1)))
       ((and (= hx hx2 tx) (= dy -2))
	(dbug "tag 43-b active~%") (list tx (- ty 1)))
       ((and (= hx hx2 tx) (< (abs dy) 2)) ;; all in line and still touching - no action
	(dbug "tag 43-c active~%") (list tx ty))
       ((and (= hx hx2 tx)) ;; all in line , not touching , not in reach EEEKKK !! 
	(dbug "tag 43-d active ERR~%") (error (list 'tag-43-d-err hx hy hx2 hy2 tx ty dx dy)))
       ;; here on WE always move in diagonal , rope can only move up down left right
       ;; n-e + up - WE can only move four ways +1 +1 , +1 -1 , -1 +1 , -1 -1
       
       ;; 1 1
       ((and (= dx 1)(= dy 2)) ;; all in line but gap needs closing
	(dbug "tag 44-a active~%") (list (+ tx 1) (+ ty 1)))
       ((and (= dx 2)(= dy 1)) ;; all in line but gap needs closing
	(dbug "tag 44-b active~%") (list (+ tx 1) (+ ty 1)))

       ;; dx or dy != 0 since (= hx hx2 tx) checked above
       ;; dx can vary 1 or 2 ??
       
       ((and (= dx 1)(= dy 1)) ;; no action required
	(dbug "tag 45-a active~%") (list tx ty))
       ((and (= dx 1)(= dy -1)) ;; no action required
	(dbug "tag 45-b active~%") (list tx ty))
       ((and (= dx -1)(= dy 1)) ;; no action required
	(dbug "tag 45-c active~%") (list tx ty))       
       ((and (= dx -1)(= dy -1)) ;; no action required
	(dbug "tag 45-d active~%") (list tx ty))       
       ;; centre
       ((and (= dx 0)(= dy 0)) ;; no action required
	(dbug "tag 46-a active~%") (list tx ty))

       ;; 
       ((and (= dx 0)(= dy 1)) ;; no action required
	(dbug "tag 47-a active~%") (list tx ty))
       ((and (= dx 1)(= dy 0)) ;; no action required
	(dbug "tag 47-b active~%") (list tx ty))
       ((and (= dx -1)(= dy 0)) ;; no action required
	(dbug "tag 47-c active~%") (list tx ty))
       ((and (= dx 0)(= dy -1)) ;; no action required
	(dbug "tag 47-d active~%") (list tx ty))
       
       ;; -1 1
       ((and (= dx -1)(= dy 2)) ;; all in line but gap needs closing
	(dbug "tag 48-a active~%") (list (- tx 1) (+ ty 1)))
       ((and (= dx -2)(= dy 1)) ;; all in line but gap needs closing
	(dbug "tag 48-b active~%") (list (- tx 1) (+ ty 1)))
       
       ;; 1 -1
       ((and (= dx 1)(= dy -2)) ;; all in line but gap needs closing
	(dbug "tag 49-a active~%") (list (+ tx 1) (- ty 1)))
       ((and (= dx 2)(= dy -1)) ;; all in line but gap needs closing
	(dbug "tag 49-b active~%") (list (+ tx 1) (- ty 1)))

       ;; -1 -1
       ((and (= dx -1)(= dy -2)) ;; all in line but gap needs closing
	(dbug "tag 50-a active~%") (list (- tx 1) (- ty 1)))
       ((and (= dx -2)(= dy -1)) ;; all in line but gap needs closing
	(dbug "tag 50-b active~%") (list (- tx 1) (- ty 1)))


       ;;  0 2 ?? dunno what do - chase it i guess ...
       ((and (= dx 0)(= dy 2)) ;; no action required
	(dbug "tag 47-a active~%") (list tx (+ ty 1)))
       ((and (= dx 2)(= dy 0)) ;; no action required
	(dbug "tag 47-b active~%") (list (+ tx 1) ty))
       ((and (= dx -2)(= dy 0)) ;; no action required
	(dbug "tag 47-c active~%") (list (- tx 1) ty))
       ((and (= dx 0)(= dy -2)) ;; no action required
	(dbug "tag 47-d active~%") (list tx (- ty 1)))

       
       
       ;; <--------- these undocumented just guessed -------------->
       ;; 2 2
       ;; 2 -2
       ;; -2 2
       ;; -2 -2
       ((and (= dx 2)(= dy 2)) ;; all in line but gap needs closing
	(dbug "tag 61-a active~%") (list (+ tx 1) (+ ty 1)))
       ((and (= dx 2)(= dy -2)) ;; all in line but gap needs closing
	(dbug "tag 62-b active~%") (list (+ tx 1) (- ty 1)))

       ((and (= dx -2)(= dy 2)) ;; all in line but gap needs closing
	(dbug "tag 63-c active~%") (list (- tx 1) (+ ty 1)))
       ((and (= dx -2)(= dy -2)) ;; all in line but gap needs closing
	(dbug "tag 64-d active~%") (list (- tx 1) (- ty 1)))
       
       (else
	(dbug "tag 79-x active ERR~%") (error (list 'tag-79-x-not-implemented hx hy hx2 hy2 tx ty dx dy)))))))




;; dirty visualizer 
(define (snake pos1 pos2 pos3 pos4)
  (let ((hx (first pos1))
	(hy (second pos1))
	(hx2 (first pos2))
	(hy2 (second pos2))
	(tx (first pos3))
	(ty (second pos3))
	(tx2 (first pos4))
	(ty2 (second pos4)))
    
  (letrec ((dig1 (lambda (x y)
		  (dbug2 "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
			  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx)(= y hy)(= x tx)(= y ty)) "HT")
		   ((and (= x hx)(= y hy)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else "."))))
	   (dig2 (lambda (x y)
		  (dbug2 "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"   x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx2)(= y hy2)(= x tx)(= y ty)) "HT")
		   ((and (= x hx2)(= y hy2)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else "."))))
	   (dig3 (lambda (x y)
		  (dbug2 "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
			  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx2)(= y hy2)(= x tx2)(= y ty2)) "HT")
		   ((and (= x hx2)(= y hy2)) "H")
		   ((and (= x tx2)(= y ty2)) "T")
		   (else "."))))	   	   
	   ); letrec-procs    
    ;;use letrec
    (format #t "~%")
    (format #t "~%MOTION : head(~a,~a) -> (~a,~a) " hx hy hx2 hy2)
    (format #t "~%MOTION : tail(~a,~a) -> (~a,~a) " tx ty tx2 ty2)
    (format #t "~%")    
    (format #t "BEFORE ~%")
    (format #t "~%")
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 7) (dig1 2 7) (dig1 3 7) (dig1 4 7) (dig1 5 7) (dig1 6 7) (dig1 7 7))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 6) (dig1 2 6) (dig1 3 6) (dig1 4 6) (dig1 5 6) (dig1 6 6) (dig1 7 6))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 5) (dig1 2 5) (dig1 3 5) (dig1 4 5) (dig1 5 5) (dig1 6 5) (dig1 7 5))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 4) (dig1 2 4) (dig1 3 4) (dig1 4 4) (dig1 5 4) (dig1 6 4) (dig1 7 4))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 3) (dig1 2 3) (dig1 3 3) (dig1 4 3) (dig1 5 3) (dig1 6 3) (dig1 7 3))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 2) (dig1 2 2) (dig1 3 2) (dig1 4 2) (dig1 5 2) (dig1 6 2) (dig1 7 2))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 1) (dig1 2 1) (dig1 3 1) (dig1 4 1) (dig1 5 1) (dig1 6 1) (dig1 7 1))
    
    (format #t "~%")
    (format #t "DURING ~%")
    (format #t "~%")
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 7) (dig2 2 7) (dig2 3 7) (dig2 4 7) (dig2 5 7) (dig2 6 7) (dig2 7 7))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 6) (dig2 2 6) (dig2 3 6) (dig2 4 6) (dig2 5 6) (dig2 6 6) (dig2 7 6))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 5) (dig2 2 5) (dig2 3 5) (dig2 4 5) (dig2 5 5) (dig2 6 5) (dig2 7 5))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 4) (dig2 2 4) (dig2 3 4) (dig2 4 4) (dig2 5 4) (dig2 6 4) (dig2 7 4))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 3) (dig2 2 3) (dig2 3 3) (dig2 4 3) (dig2 5 3) (dig2 6 3) (dig2 7 3))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 2) (dig2 2 2) (dig2 3 2) (dig2 4 2) (dig2 5 2) (dig2 6 2) (dig2 7 2))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 1) (dig2 2 1) (dig2 3 1) (dig2 4 1) (dig2 5 1) (dig2 6 1) (dig2 7 1))
    
    (format #t "~%")
    (format #t "AFTER ~%")
    (format #t "~%")
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 7) (dig3 2 7) (dig3 3 7) (dig3 4 7) (dig3 5 7) (dig3 6 7) (dig3 7 7))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 6) (dig3 2 6) (dig3 3 6) (dig3 4 6) (dig3 5 6) (dig3 6 6) (dig3 7 6))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 5) (dig3 2 5) (dig3 3 5) (dig3 4 5) (dig3 5 5) (dig3 6 5) (dig3 7 5))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 4) (dig3 2 4) (dig3 3 4) (dig3 4 4) (dig3 5 4) (dig3 6 4) (dig3 7 4))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 3) (dig3 2 3) (dig3 3 3) (dig3 4 3) (dig3 5 3) (dig3 6 3) (dig3 7 3))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 2) (dig3 2 2) (dig3 3 2) (dig3 4 2) (dig3 5 2) (dig3 6 2) (dig3 7 2))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 1) (dig3 2 1) (dig3 3 1) (dig3 4 1) (dig3 5 1) (dig3 6 1) (dig3 7 1))    
    )))





(define-macro (quiet-snooker pos1 pos2 pos3)
  (let ((g (gensym "pos")))
    `(let ((,g (follow ',pos1 ',pos2 ',pos3)))
       ;;(snake ',pos1 ',pos2 ',pos3 ,g)
       ,g)))


(define-macro (snooker pos1 pos2 pos3)
  (let ((g (gensym "pos")))
    `(let ((,g (follow ',pos1 ',pos2 ',pos3)))
       (snake ',pos1 ',pos2 ',pos3 ,g)
       ,g)))


;; dirty test suite ...
(define (expect title a b)
  (let ((ok (equal? a b)))
    (if ok
	ok
	(format #t "failed [~a] : expected ~a : received ~a~%" title a b))))
	

#|

5 : . . . . .
4 : . 1 2 3 .
3 ; . 4 5 6 .
2 : . 7 8 9 .
1 : . . . . .
....1 2 3 4 5

1 is (2,4)  : 2 is (3,4) :  3 is (4,4)
4 is (2,3)  : 5 is (3,3) :  6 is (4,3)
7 is (2,2)  : 8 is (3,2) :  9 is (4,2)

only movement of head is u = up
, l = left , r = right , d = down

|#

;;------------------ ups passed --------------------------
(define (test-1u) ;; diag
  (dbug "TESTING 1-UP~%")
  (expect "test-1u" '(2 4) (snoke (2 4) (2 5) (3 3))))

(define (test-2u) ;; up 1
    (dbug "TESTING 2-UP~%")
  (expect "test-2u" '(3 4) (snoke (3 4) (3 5) (3 3))))

(define (test-3u) ;; diag
    (dbug "TESTING 3-UP~%")
  (expect "test-3u" '(4 4) (snoke (4 4) (4 5) (3 3))))

(define (test-4u) ;; nop
    (dbug "TESTING 4-UP~%")
  (expect "test-4u" '(3 3) (snoke (2 3) (2 4) (3 3))))

(define (test-5u) ;; nop
    (dbug "TESTING 5-UP~%")
  (expect "test-5u" '(3 3) (snoke (3 3) (3 4) (3 3))))

(define (test-6u) ;; nop
    (dbug "TESTING 6-UP~%")
  (expect "test-6u" '(3 3) (snoke (4 3) (4 4) (3 3))))

(define (test-7u) ;; nop
    (dbug "TESTING 7-UP~%")
  (expect "test-7u" '(3 3) (snoke (2 2) (2 3) (3 3))))

(define (test-8u) ;; nop
    (dbug "TESTING 8-UP~%")
  (expect "test-8u" '(3 3) (snoke (3 2) (3 3) (3 3))))

(define (test-9u) ;;  nop
    (dbug "TESTING 9-UP~%")
  (expect "test-9u" '(3 3) (snoke (4 2) (4 3) (3 3))))

;;-------------- ups passed -----------------

(define (test-1d) ;; nop
  (dbug "TESTING 1-DOWN~%")
  (expect "test-1d" '(3 3) (snoke (2 4) (2 3) (3 3))))

(define (test-2d) ;; nop
  (dbug "TESTING 2-DOWN~%")
  (expect "test-2d" '(3 3) (snoke (3 4) (3 3) (3 3))))

(define (test-3d) ;; nop
  (dbug "TESTING 3-DOWN~%")
  (expect "test-3d" '(3 3) (snoke (4 4) (4 3) (3 3))))

(define (test-4d) ;; nop
  (dbug "TESTING 4-DOWN~%")  
  (expect "test-4d" '(3 3) (snoke (2 3) (2 2) (3 3))))

(define (test-5d) ;; nop
  (dbug "TESTING 5-DOWN~%")  
  (expect "test-5d" '(3 3) (snoke (3 3) (3 2) (3 3))))

(define (test-6d) ;; nop
  (dbug "TESTING 6-DOWN~%")  
  (expect "test-6d" '(3 3) (snoke (4 3) (4 2) (3 3))))

(define (test-7d) ;; diag
  (dbug "TESTING 7-DOWN~%")  
  (expect "test-7d" '(2 2) (snoke (2 2) (2 1) (3 3))))

(define (test-8d) ;; down 1
  (dbug "TESTING 8-DOWN~%")  
  (expect "test-8d" '(3 2) (snoke (3 2) (3 1) (3 3))))

(define (test-9d) ;; diag
  (dbug "TESTING 9-DOWN~%")  
  (expect "test-9d" '(4 2) (snoke (4 2) (4 1) (3 3))))

;; ------------- downs passed ---------------

;; ------------- rights passed -------------------

(define (test-1r) ;; nop
  (dbug "TESTING 1-RIGHT~%")  
  (expect "test-1r" '(3 3) (snoke (2 4) (3 4) (3 3))))

(define (test-2r) ;; nop
  (dbug "TESTING 2-RIGHT~%")    
  (expect "test-2r" '(3 3) (snoke (3 4) (4 4) (3 3))))

(define (test-3r) ;; diag
  (dbug "TESTING 3-RIGHT~%")    
  (expect "test-3r" '(4 4) (snoke (4 4) (5 4) (3 3))))

(define (test-4r) ;; nop
  (dbug "TESTING 4-RIGHT~%")    
  (expect "test-4r" '(3 3) (snoke (2 3) (3 3) (3 3))))

(define (test-5r) ;; nop
  (dbug "TESTING 5-RIGHT~%")    
  (expect "test-5r" '(3 3) (snoke (3 3) (4 3) (3 3))))

(define (test-6r) ;; right 1
  (dbug "TESTING 6-RIGHT~%")    
  (expect "test-6r" '(4 3) (snoke (4 3) (5 3) (3 3))))

(define (test-7r) ;; nop
  (dbug "TESTING 7-RIGHT~%")    
  (expect "test-7r" '(3 3) (snoke (2 2) (3 2) (3 3))))

(define (test-8r) ;; nop
  (dbug "TESTING 8-RIGHT~%")    
  (expect "test-8r" '(3 3) (snoke (3 2) (4 2) (3 3))))

(define (test-9r) ;; diag
  (dbug "TESTING 9-RIGHT~%")   
  (expect "test-9r" '(4 2) (snoke (4 2) (5 2) (3 3))))

;; -------------------- lefts started -----------------------

(define (test-1l) ;; diag
  (dbug "TESTING 1-LEFT~%")    
  (expect "test-1l" '(2 4) (snoke (2 4) (1 4) (3 3))))

(define (test-2l) ;; nop
  (dbug "TESTING 2-LEFT~%")    
  (expect "test-2l" '(3 3) (snoke (3 4) (2 4) (3 3))))

(define (test-3l) ;; nop
  (dbug "TESTING 3-LEFT~%")      
  (expect "test-3l" '(3 3) (snoke (4 4) (3 4) (3 3))))

(define (test-4l) ;; left 1
  (dbug "TESTING 4-LEFT~%")      
  (expect "test-4l" '(2 3) (snoke (2 3) (1 3) (3 3))))

(define (test-5l) ;; nop
  (dbug "TESTING 5-LEFT~%")      
  (expect "test-5l" '(3 3) (snoke (3 3) (2 3) (3 3))))

(define (test-6l) ;; nop
  (dbug "TESTING 6-LEFT~%")      
  (expect "test-6l" '(3 3) (snoke (4 3) (3 3) (3 3))))

(define (test-7l) ;; diag
  (dbug "TESTING 7-LEFT~%")      
  (expect "test-7l" '(2 2) (snoke (2 2) (1 2) (3 3))))

(define (test-8l) ;; nop
  (dbug "TESTING 8-LEFT~%")      
  (expect "test-8l" '(3 3) (snoke (3 2) (2 2) (3 3))))

(define (test-9l) ;; nop
  (dbug "TESTING 9-LEFT~%")      
  (expect "test-9l" '(3 3) (snoke (4 2) (3 2) (3 3))))

;; ------------------- lefts passed -------------------------


;; some unit tests -- 36 for full coverage ---
(define (run-tests)
  (test-1u)
  (test-2u)
  (test-3u)
  (test-4u)
  (test-5u)
  (test-6u)
  (test-7u)
  (test-8u)
  (test-9u)

  (test-1d)
  (test-2d)
  (test-3d)
  (test-4d)
  (test-5d)
  (test-6d)
  (test-7d)
  (test-8d)
  (test-9d)

  (test-1r)
  (test-2r)
  (test-3r)
  (test-4r)
  (test-5r)
  (test-6r)
  (test-7r)
  (test-8r)
  (test-9r)

  (test-1l)
  (test-2l)
  (test-3l)
  (test-4l)
  (test-5l)
  (test-6l)
  (test-7l)
  (test-8l)
  (test-9l)
  )



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

(define (sane-commands xs)
  (cond
   ((null? xs) #t)
   (#t (let ((letter (first xs))
	     (num (second xs)))
	 (assert (member letter '(R L U D)))
	 (assert (integer? num))
	 (sane-commands (cdr (cdr xs)))))))




(define (model commands)
  (let ((path '())
	(x 1)
	(y 1))    
    (letrec ((report (lambda ()
		       (set! path (cons (list x y) path))))
	     (help
	      (lambda (cmds)
		(cond
		 ((null? cmds) 'done)
		 (#t (let ((dir (first cmds))
			   (dist (second cmds)))
		       (cond
			((eq? dir 'R)
			 (format #t "~%========= Right ~a ==========~%" dist)
			 (right dist))
			((eq? dir 'L)
			 (format #t "~%========= Left ~a ==========~%" dist)
			 (left dist))
			((eq? dir 'U)
			 (format #t "~%========= Up ~a ==========~%" dist)
			 (up dist))
			((eq? dir 'D)
			 (format #t "~%========= Down ~a ==========~%" dist)
			 (down dist))
			(#t (error "model.help DIRECTION required R L U D : dir was " dir )))
		       ;; recurse + drop 2 items DIRECTION DISTANCE
		       (help (cdr (cdr cmds))))))))
	     (right (lambda (dist)
		      (cond
		       ((< dist 1) 'done)  ;; right + model tail response
		       (#t (set! x (+ x 1))
			   (report)
			   (right (- dist 1))))))
	     (left (lambda (dist)
		     (cond
		      ((< dist 1) 'done)  ;; left + model tail response
		      (#t (set! x (- x 1))
			  ;; report before clobber 
			  (report)	
			  (left (- dist 1))))))
	     (up (lambda (dist)
		   (cond
		    ((< dist 1) 'done)  ;; up + model tail response
		    (#t (set! y (+ y 1))
			  ;; report before clobber 
			  (report)	
			  (up (- dist 1))))))
	     (down (lambda (dist)
		     (cond
		      ((< dist 1) 'done)  ;; down + model tail response
		      (#t (set! y (- y 1))
			  ;; report before clobber 
			  (report)	
			  (down (- dist 1)))))))  
      (report)
      (help commands)
      (reverse path))))


;; ------------------------------------

(define (tail-path path-head)
  (let ((path '()))
    (letrec ((help (lambda (xs pos1 pos2 pos3)
		     (dbug "help.xs =~a~%" xs)
		     (dbug "help.pos1 =~a~%" pos1)
		     (dbug "help.pos2 =~a~%" pos2)
		     (dbug "help.pos3 =~a~%" pos3)	  
		     (cond
		      ((null? xs)
		       (dbug "help.xs is null --- ending here ~%")
		       ;; tack on last gasp ?
		       (let ((pos4 (follow pos1 pos2 pos3)))
			 (dbug "help.gasp.pos4 =~a~%" pos4)			      	
			 (set! path (cons pos4 path))
			 (dbug "help.gasp.path =~a~%" path))
		       ;; done
		       path)
		      (else (let ((dummy (car xs)))
			      (dbug "help.dummy =~a~%" dummy)
			      ;; dummy is "head's" next position
			      (let ((pos4 (follow pos1 pos2 pos3)))
				(dbug "help.pos4 =~a~%" pos4)
			      	
				(set! path (cons pos4 path))
				(dbug "help.path =~a~%" path)
			      	
				;; done with pos3 onto pos4
				;; pos1 disgarded
				;; pos2 to dummy is next transition head made
				(if (not (null? dummy))
				    (begin
				      (dbug "help.dummy NOT null ~a ~%" dummy)
				      (help (cdr xs) pos2 dummy pos4))
				    (begin
				      (dbug "help.dummy null -- finished line 698 ~%")
				      path
				      )))))))))
      ;; all start at 1 1
      (set! path (cons '(1 1) path))      
	(dbug "path-head =~a~%" path-head)	
	(let ((pos1 (first path-head))
	      (pos2 (second path-head))
	      (pos3 '(1 1)))
	  (dbug "pos1 =~a~%" pos1)
	  (dbug "pos2 =~a~%" pos2)
	  (dbug "pos3 =~a~%" pos3)	
	  (reverse (help (cdr (cdr path-head)) pos1 pos2 pos3))))))




(define (viz head tail)
  (let ((hx (first head))
	(hy (second head))
	(tx (first tail))
	(ty (second tail)))
  (letrec ((dig1 (lambda (x y)
		  (dbug2 "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
			  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx)(= y hy)(= x tx)(= y ty)) "HT")
		   ((and (= x hx)(= y hy)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else ".")))))
    ;;use letrec
    (format #t "~%")
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 7) (dig1 2 7) (dig1 3 7) (dig1 4 7) (dig1 5 7) (dig1 6 7) )
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 6) (dig1 2 6) (dig1 3 6) (dig1 4 6) (dig1 5 6) (dig1 6 6) )
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 5) (dig1 2 5) (dig1 3 5) (dig1 4 5) (dig1 5 5) (dig1 6 5) )
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 4) (dig1 2 4) (dig1 3 4) (dig1 4 4) (dig1 5 4) (dig1 6 4) )
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 3) (dig1 2 3) (dig1 3 3) (dig1 4 3) (dig1 5 3) (dig1 6 3) )
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 2) (dig1 2 2) (dig1 3 2) (dig1 4 2) (dig1 5 2) (dig1 6 2) )
    (format #t "~a ~a ~a ~a ~a ~a ~%" (dig1 1 1) (dig1 2 1) (dig1 3 1) (dig1 4 1) (dig1 5 1) (dig1 6 1) )
    )))




(define (viz-1)
  (let* ((path-head (model lines2))
	 (path-tail (tail-path path-head)))
    (while (and (not (null? path-head))
		(not (null? path-tail)))
      (let ((pos-head (car path-head))
	    (pos-tail (car path-tail)))
	;;show
	(viz pos-head pos-tail)
	(set! path-head (cdr path-head))
	(set! path-tail (cdr path-tail))))))



;; remove duplicates
(define (process path)
  (let ((visit '()))
    (letrec ((help (lambda (xs)
		     (cond
		      ((null? xs) visit)
		      (#t (let ((pos (car xs)))
			    (cond
			     ((member pos visit) #f)
			     (else (set! visit (cons pos visit))))
			    (help (cdr xs))))))))
      (help path))))

(define lines (get-lines "input"))
(assert (sane-commands lines))

(define lines2 (get-lines "input2"))
(assert (sane-commands lines2))

(define lines3 (get-lines "input3"))
(assert (sane-commands lines3))



;; ;; small example one head one tail
;; (define h-path2 (model lines2))
;; (define t-path2 (tail-path h-path2))
;; (define example-1 (process t-path2))
;; (define example-1-length (length example-1))

;; ;; bigger example one head one tail
;; (define h-path (model lines))
;; (define t-path (tail-path h-path))
;; (define example-2 (process t-path))
;; (define example-2-length (length example-2))


;; ------------- now rope HEAD can now move diagonally whereas before it could not
;;   -------------------------as it was only up , down , left , right ...........
;; smaller example one head nine tails
(define h-path (model lines3)) ;;<<<------- change lines lines2 or lines3
(define t1-path (tail-path h-path))
(define t2-path (tail-path t1-path))
(define t3-path (tail-path t2-path))
(define t4-path (tail-path t3-path))
(define t5-path (tail-path t4-path))
(define t6-path (tail-path t5-path))
(define t7-path (tail-path t6-path))
(define t8-path (tail-path t7-path))
(define t9-path (tail-path t8-path))

(define example-3 (process t9-path))
(define example-3-length (length example-3))

;;(format #t "best guess at t-9 solution is with ~%length of [ ~a ] squares ~%" example-3-length)




(define (viz-tool-show hpos t1pos t2pos t3pos t4pos t5pos t6pos t7pos t8pos t9pos)
  (let ((width 26) ;; 6
	(height 21)) ;; 5
  (letrec ((help (lambda (x y)
		   (cond
		    ((< y 1) 'done)
		    ((> x width)
		     (format #t "~%") ; newline
		     (help 1 (- y 1)))
		    (else
		     (begin
		       (let ((pos (list x y))
			     (nog 0))
			 ;;(format #t " [")
			 (format #t " ")
			 (when (equal? pos hpos) (format #t  "H") (set! nog (+ 1 nog)))
			 (when (equal? pos t1pos) (format #t "1") (set! nog (+ 1 nog)))
			 (when (equal? pos t2pos) (format #t "2") (set! nog (+ 1 nog)))
			 (when (equal? pos t3pos) (format #t "3") (set! nog (+ 1 nog)))
			 (when (equal? pos t4pos) (format #t "4") (set! nog (+ 1 nog)))
			 (when (equal? pos t5pos) (format #t "5") (set! nog (+ 1 nog)))
			 (when (equal? pos t6pos) (format #t "6") (set! nog (+ 1 nog)))
			 (when (equal? pos t7pos) (format #t "7") (set! nog (+ 1 nog)))
			 (when (equal? pos t8pos) (format #t "8") (set! nog (+ 1 nog)))
			 (when (equal? pos t9pos) (format #t "9") (set! nog (+ 1 nog)))

			 (when (= nog 0)
			   (format #t " . "))
			 ;; (while (< nog 10)
			 ;;   (format #t ".")
			 ;;   (set! nog (+ nog 1)))			 
			 ;;(format #t "] ")
			 (format #t " ")
			 (help (+ x 1) y))))))))
    (format #t "~%")
    (help 1 height)
    (format #t "~%"))))
  

(define (viz-tool)
  (let ((hs h-path)
	(t1s t1-path)
	(t2s t2-path)
	(t3s t3-path)
	(t4s t4-path)
	(t5s t5-path)
	(t6s t6-path)
	(t7s t7-path)
	(t8s t8-path)
	(t9s t9-path))
    (while (and (not (null? hs)) (not (null? t1s)) (not (null? t2s)) (not (null? t3s)) (not (null? t4s)) (not (null? t5s))
		(not (null? t6s)) (not (null? t7s)) (not (null? t8s)) (not (null? t9s)))
      (let ((hpos (first hs)) (t1pos (first t1s)) (t2pos (first t2s)) (t3pos (first t3s)) (t4pos (first t4s)) (t5pos (first t5s))
	    (t6pos (first t6s)) (t7pos (first t7s)) (t8pos (first t8s)) (t9pos (first t9s)))

	(viz-tool-show hpos t1pos t2pos t3pos t4pos t5pos t6pos t7pos t8pos t9pos)

	;; next positions everyone ...
	(set! hs (cdr hs))	
	(set! t1s (cdr t1s))
	(set! t2s (cdr t2s))
	(set! t3s (cdr t3s))
	(set! t4s (cdr t4s))
	(set! t5s (cdr t5s))
	(set! t6s (cdr t6s))
	(set! t7s (cdr t7s))
	(set! t8s (cdr t8s))
	(set! t9s (cdr t9s))
	))))


;; use viz-tool on smaller examples , big examples just a blur on screeen
(viz-tool)





	
	
	
	
	

	






