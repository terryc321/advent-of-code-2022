

#|

(getcwd) 
(chdir "day9")

|#

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(use-modules (ice-9 format))

(use-modules (ice-9 optargs))

;; first second third ...
(use-modules (srfi srfi-1))

;; guile error handling??
;; roll your own assert macro
(define-macro (assert x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (if ,g ,g (error (list 'assertion 'failed ,g 'expression 'was ',x))))))


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


(define lines (get-lines "input2"))
(assert (sane-commands lines))

;; (define-macro (dolist args . body)
;;   ...)

#|

R 10

lambda* allows for keyword arguments
sx sy = start at x , y
hx hy = head x y
tx ty = tail x y

assume to start
head at 0,0
start at 0,0
tail at 0,0
<------------ purposes diagram ---------------->

                     . y axis
up +Y               /|\ 
down -Y              |
left -X              |
right +X             ...........> x axis



<------------ 4 inward motions ---------------->
left-in + still touch
. . . .
. (AC) B . 
. . . . 
. . . .
hx,hy B to C
tx,ty A to A  no change

hx-tx = 0
hy-ty = 0  no action 
-----------------------------------
up-in + still touch
. . . .
. (AC) . 
. B . . 
. . . .
hx,hy B to C
tx,ty A to A  no change

hx-tx = 0
hy-ty = 0  no action 
-----------------------------------

right-in + still touch
. . . .
B (AC) . 
. . . . 
. . . .
hx,hy B to C
tx,ty A to A  no change
hx-tx = 0
hy-ty = 0  no action 
-----------------------------------
down-in + still touch
. B . .
. (AC) . 
. . . . 
. . . .
hx,hy B to C
tx,ty A to A  no change
hx-tx = 0
hy-ty = 0  no action 
-----------------------------------


< ------------ 4 outward motions ------------>
right-out + still touch
. . . .
. (AB) C . 
. . . . 
. . . .
hx,hy B to C
tx,ty A to A  no change
hx-tx = 1
hy-ty = 0  no action 
-----------------------------------
down-out + still touch
. . . .
. (AB) . 
. C . . 
. . . .
hx,hy B to C
tx,ty A to A  no change
hx-tx = 0
hy-ty = 1  no action 
-----------------------------------
left-out + still touch
. . . .
C (AB) . 
. . . . 
. . . .
hx,hy B to C
tx,ty A to A  no change
hx-tx = 0
hy-ty = 0  no action 
-----------------------------------
up-out + still touch
. C . .
. (AB) . 
. . . . 
. . . .
hx,hy B to C
tx,ty A to A  no change
hx-tx = 0
hy-ty = 0  no action 
-----------------------------------

< ------------ 4 horizontal motions ------------>
right + disconnect! 
. . . .
. A B C 
. . . . 
. . . .
hx,hy B to C
tx,ty A to B
hx-tx = 2
hy-ty = 0
action tx ++
so hx-tx = 1
so hy-ty = 0
-----------------------------------
down + disconnect
. . A .
. . B . 
. . C . 
. . . .
hx,hy B to C
tx,ty A to B
hx-tx = 0
hy-ty = -2
action ty --;
-----------------------------------
left + disconnect
. . . .
. C B A 
. . . . 
. . . .
hx,hy B to C
tx,ty A to B
hx-tx = -2
hy-ty = 0
action tx --;
-----------------------------------
up + disconnect
. . C .
. . B . 
. . A . 
. . . .
hx,hy B to C
tx,ty A to B
hx-tx = 0
hy-ty = 2
action ty ++;
-----------------------------------

< ------------ 4 diagonal motions ------------>
down-left + disconnect 
. . . A
. . B . 
. C . . 
. . . .

hx,hy B to C
tx,ty A to B
hx-tx = -2
hy-ty = -2
action tx -- ; ty --;
-----------------------------------

up left + disconnect
. . . .
. C . . 
. . B . 
. . . A
hx,hy B to C
tx,ty A to B
hx-tx = 2
hy-ty = 2
action tx ++ ; ty --;
-----------------------------------
up right + disconnect
. . . C
. . B . 
. A . . 
. . . .
hx,hy B to C
tx,ty A to B
hx-tx = 2
hy-ty = 2
action tx ++ ; ty ++;
-----------------------------------
down right+ disconnect

. . . .
. A . . 
. . B . 
. . . C
hx,hy B to C
tx,ty A to B
hx-tx = 2
hy-ty = -2
action tx ++; ty --;
-----------------------------------


<------------ 4 diagonal no motion required ------------->
. . . A     cannot occur because B is too far from A 
. . C .    invalid state
. B . . 
. . . .

B C A .    cannot occur because B is too far from A  again.
. . . .    invalid state
. . . . 
. . . .




|#

;; just trying to show board , but without having to specify hx hy tx ty for every square

(define-macro (follow)
  `(begin
     (let ((dx (- hx tx))
	   (dy (- hy ty)))
       (cond
	((= dx 2)  (set! tx (+ tx 1)))
	((= dx -2) (set! tx (- tx 1)))
	((= dx 1) #f)
	((= dx 0) #f)
	((= dx -1) #f)
	(#t (error (list (format #f "illegal delta X : hd@(~a,~a) tl@(~a,~a) delta(~a,~a)" hx hy tx ty dx dy)))))
       (cond
	((= dy 2)  (set! ty (+ ty 1)))
	((= dy -2) (set! ty (- ty 1)))
	((= dy 1) #f)
	((= dy 0) #f)
	((= dy -1) #f)
	(#t (error (list (format #f "illegal : delta y: hd@(~a,~a) tl@(~a,~a) delta(~a,~a)" hx hy tx ty dx dy))))))))

;;(follow)


(define-macro (dig x y)
  `(begin
     (cond
      ((and (= ,x hx)(= ,y hy)(= ,x tx)(= ,y ty)) "HT")
      ((and (= ,x hx)(= ,y hy)) "H")
      ((and (= ,x tx)(= ,y ty)) "T")
      (else "."))))


(define-macro (snake hx hy tx ty)
  `(begin
     (format #t "~%")
     (format #t "head(~a,~a) : tail(~a,~a) : " ,hx ,hy ,tx ,ty)
     (format #t "~%")
     (format #t "~a g~a ~a ~a~%" (dig 1 5) (dig 2 5) (dig 3 5) (dig 4 5))
     (format #t "~a ~a h~a ~a~%" (dig 1 4) (dig 2 4) (dig 3 4) (dig 4 4))
     (format #t "~a ~a ~a i~a~%" (dig 1 3) (dig 2 3) (dig 3 3) (dig 4 3))
     (format #t "~a ~a j~a ~a~%" (dig 1 2) (dig 2 2) (dig 3 2) (dig 4 2))
     (format #t "~a k~a ~a ~a~%" (dig 1 1) (dig 2 1) (dig 3 1) (dig 4 1))
     (format #t "~%~%hx = ~a : hy = ~a : tx = ~a : ty = ~a~%" ,hx ,hy ,tx ,ty)
     ))

(snake)


    
(define (test)
  (let ((hx 2)(hy 0)(tx 0)(ty 0))    
    (follow)
    (snake hx hy tx ty)))


(let ((hx 2)(hy 0)(tx 0)(ty 0))    
  (follow)
  (format #t "dig 2 0 -> ~a " (dig 2 0))
  (snake hx hy tx ty))






(define (model commands)
  (let ((hx 0)(hy 0)(tx 0)(ty 0)(history '()))
    (letrec ((help
	      (lambda (cmds)
		(cond
		 ((null? cmds) 'done)
		 (#t (let ((dir (first cmds))
			   (dist (second cmds)))
		       (cond
			((eq? dir 'R) (right dist))
			((eq? dir 'L) (left dist))
			((eq? dir 'U) (up dist))
			((eq? dir 'D) (down dist))
			(#t (error "model.help DIRECTION required R L U D : dir was " dir )))
		       ;; recurse + drop 2 items DIRECTION DISTANCE
		       (help (cdr (cdr cmds))))))))
	     (right (lambda (dist)
		      (cond
		       ((< dist 1) 'done)
		       (#t (set! hx (+ hx 1))
			   (follow)
			   (right (- dist 1))))))
	     (left (lambda (dist) #t))
	     (up (lambda (dist) #t))
	     (down (lambda (dist) #t)))
      ;; start letrec
      (help commands))))


(define (small)
  (model (get-lines "input2")))

(define (big)
  (model (get-lines "input")))

	    

