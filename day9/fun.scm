

#|

(getcwd) 
(chdir "day9")

concern 1 : scheme system get out of sync with what is in file ,
in terms of computer hangover from old experiments


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



(define (snake hx hy hx2 hy2 tx ty tx2 ty2)
  ;; constrain snake head from moving at most 1 square at a time in both x and y directions
  (assert (<= (abs (- hx2 hx)) 1))
  (assert (<= (abs (- hy2 hy)) 1))
  ;;(format #t "snake.delta dhx ~a : dhy ~a ~%" (- hx2 hx) (- hy2 hy))
  (letrec ((dig1 (lambda (x y)
		  ;; dig debug
		  ;;(format #t "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
		  ;;	  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx)(= y hy)(= x tx)(= y ty)) "HT")
		   ((and (= x hx)(= y hy)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else "."))))
	   (dig2 (lambda (x y)
		  ;; dig debug
		  ;;(format #t "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
		  ;;	  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx2)(= y hy2)(= x tx)(= y ty)) "HT")
		   ((and (= x hx2)(= y hy2)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else "."))))
	   (dig3 (lambda (x y)
		  ;; dig debug
		  ;;(format #t "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
		  ;;	  x y hx hy (= x hx) (= y hy))
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
    
    )) ;snake



(define (snake2 hx hy hx2 hy2 tx ty tx2 ty2)
  ;; constrain snake head from moving at most 1 square at a time in both x and y directions
  (assert (<= (abs (- hx2 hx)) 1))
  (assert (<= (abs (- hy2 hy)) 1))
  ;;(format #t "snake.delta dhx ~a : dhy ~a ~%" (- hx2 hx) (- hy2 hy))
  (letrec ((dig1 (lambda (x y)
		  ;; dig debug
		  ;;(format #t "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
		  ;;	  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx)(= y hy)(= x tx)(= y ty)) "HT")
		   ((and (= x hx)(= y hy)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else "."))))
	   (dig2 (lambda (x y)
		  ;; dig debug
		  ;;(format #t "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
		  ;;	  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx2)(= y hy2)(= x tx)(= y ty)) "HT")
		   ((and (= x hx2)(= y hy2)) "H")
		   ((and (= x tx)(= y ty)) "T")
		   (else "."))))
	   (dig3 (lambda (x y)
		  ;; dig debug
		  ;;(format #t "DIG x=~a : y=~a : hx =~a : hy = ~a : x=hx ~a: y = hy ~a ~%"
		  ;;	  x y hx hy (= x hx) (= y hy))
		  (cond
		   ((and (= x hx2)(= y hy2)(= x tx2)(= y ty2)) "HT")
		   ((and (= x hx2)(= y hy2)) "H")
		   ((and (= x tx2)(= y ty2)) "T")
		   (else "."))))	   	   
	   ); letrec-procs    
    ;;use letrec

    ;; (format #t "~%")
    ;; (format #t "~%MOTION : head(~a,~a) -> (~a,~a) " hx hy hx2 hy2)
    ;; (format #t "~%MOTION : tail(~a,~a) -> (~a,~a) " tx ty tx2 ty2)
    ;; (format #t "~%")    
    ;; (format #t "BEFORE ~%")
    ;; (format #t "~%")
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 7) (dig1 2 7) (dig1 3 7) (dig1 4 7) (dig1 5 7) (dig1 6 7) (dig1 7 7))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 6) (dig1 2 6) (dig1 3 6) (dig1 4 6) (dig1 5 6) (dig1 6 6) (dig1 7 6))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 5) (dig1 2 5) (dig1 3 5) (dig1 4 5) (dig1 5 5) (dig1 6 5) (dig1 7 5))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 4) (dig1 2 4) (dig1 3 4) (dig1 4 4) (dig1 5 4) (dig1 6 4) (dig1 7 4))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 3) (dig1 2 3) (dig1 3 3) (dig1 4 3) (dig1 5 3) (dig1 6 3) (dig1 7 3))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 2) (dig1 2 2) (dig1 3 2) (dig1 4 2) (dig1 5 2) (dig1 6 2) (dig1 7 2))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig1 1 1) (dig1 2 1) (dig1 3 1) (dig1 4 1) (dig1 5 1) (dig1 6 1) (dig1 7 1))
    
    ;; (format #t "~%")
    ;; (format #t "DURING ~%")
    ;; (format #t "~%")
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 7) (dig2 2 7) (dig2 3 7) (dig2 4 7) (dig2 5 7) (dig2 6 7) (dig2 7 7))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 6) (dig2 2 6) (dig2 3 6) (dig2 4 6) (dig2 5 6) (dig2 6 6) (dig2 7 6))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 5) (dig2 2 5) (dig2 3 5) (dig2 4 5) (dig2 5 5) (dig2 6 5) (dig2 7 5))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 4) (dig2 2 4) (dig2 3 4) (dig2 4 4) (dig2 5 4) (dig2 6 4) (dig2 7 4))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 3) (dig2 2 3) (dig2 3 3) (dig2 4 3) (dig2 5 3) (dig2 6 3) (dig2 7 3))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 2) (dig2 2 2) (dig2 3 2) (dig2 4 2) (dig2 5 2) (dig2 6 2) (dig2 7 2))
    ;; (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig2 1 1) (dig2 2 1) (dig2 3 1) (dig2 4 1) (dig2 5 1) (dig2 6 1) (dig2 7 1))
    
    ;; (format #t "~%")
    ;;(format #t "AFTER ~%")
    (format #t "~%")
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 7) (dig3 2 7) (dig3 3 7) (dig3 4 7) (dig3 5 7) (dig3 6 7) (dig3 7 7))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 6) (dig3 2 6) (dig3 3 6) (dig3 4 6) (dig3 5 6) (dig3 6 6) (dig3 7 6))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 5) (dig3 2 5) (dig3 3 5) (dig3 4 5) (dig3 5 5) (dig3 6 5) (dig3 7 5))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 4) (dig3 2 4) (dig3 3 4) (dig3 4 4) (dig3 5 4) (dig3 6 4) (dig3 7 4))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 3) (dig3 2 3) (dig3 3 3) (dig3 4 3) (dig3 5 3) (dig3 6 3) (dig3 7 3))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 2) (dig3 2 2) (dig3 3 2) (dig3 4 2) (dig3 5 2) (dig3 6 2) (dig3 7 2))
    (format #t "~a ~a ~a ~a ~a ~a ~a~%" (dig3 1 1) (dig3 2 1) (dig3 3 1) (dig3 4 1) (dig3 5 1) (dig3 6 1) (dig3 7 1))
    
    )) ;snake


(define (follow hx hy hx2 hy2 tx ty)
  (format #t "follow : hx hy ~a ~a : hx2 hy2 ~a ~a : tx ty ~a ~a ~%" hx hy hx2 hy2 tx ty)
  (let* ((tx2 tx)
	 (ty2 ty)
	 (dx (- hx2 tx2))
	 (dy (- hy2 ty2)))
    (cond
     ((or (= hx hx2 tx) ;; aligned either vertically or horizontally
	  (= hy hy2 ty))
      (cond
       ((= dx 2)  (set! tx2 (+ tx2 1)))
       ((= dx -2) (set! tx2 (- tx2 1)))
       ((= dx 1) #f)
       ((= dx 0) #f)
       ((= dx -1) #f)
       (#t (error (list (format #f "illegal delta dx[~a]" dx) 'other-parameters hx hy tx ty dx dy))))
      (cond
       ((= dy 2)  (set! ty2 (+ ty2 1)))
       ((= dy -2) (set! ty2 (- ty2 1)))
       ((= dy 1) #f)
       ((= dy 0) #f)
       ((= dy -1) #f)
       (#t (error (list (format #f "illegal delta dy[~a]" dy) 'other-parameters hx hy tx ty dx dy))))
      (list tx2 ty2))
     (else
      (cond
       ;; > dx >dy
       ((and (= dx 1)(= dy 2)) (set! tx2 (+ tx2 1)) (set! ty2 (+ ty2 1)))
       ((and (= dx 2)(= dy 1)) (set! tx2 (+ tx2 1)) (set! ty2 (+ ty2 1)))
       ;; > dx < dy
       ((and (= dx 1)(= dy -2)) (set! tx2 (+ tx2 1)) (set! ty2 (- ty2 1)))
       ((and (= dx 2)(= dy -1)) (set! tx2 (+ tx2 1)) (set! ty2 (- ty2 1)))
       ;; < dx > dy
       ((and (= dx -1)(= dy 2)) (set! tx2 (- tx2 1)) (set! ty2 (+ ty2 1)))
       ((and (= dx -2)(= dy 1)) (set! tx2 (- tx2 1)) (set! ty2 (+ ty2 1)))
       ;; < dx < dy
       ((and (= dx -1)(= dy -2)) (set! tx2 (- tx2 1)) (set! ty2 (- ty2 1)))
       ((and (= dx -2)(= dy -1)) (set! tx2 (- tx2 1)) (set! ty2 (- ty2 1)))
       ;; no action required
       (#t #f))
      (list tx2 ty2)))))



;; right + disconnect
(define (test-1)
  (let ((hx 3)(hy 3)(hx2 4)(hy2 3)(tx 2)(ty 3)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-1 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))

;; left + disconnect
(define (test-2)
  (let ((hx 3)(hy 3)(hx2 2)(hy2 3)(tx 4)(ty 3)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-2 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))      
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))

;; up + disconnect
(define (test-3)
  (let ((hx 3)(hy 3)(hx2 3)(hy2 4)(tx 3)(ty 2)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-3 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))      
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))


;; down + disconnect
(define (test-4)
  (let ((hx 3)(hy 3)(hx2 3)(hy2 2)(tx 3)(ty 4)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-4 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))

;; if head tail are not touching and not in same row or column
;; tail moves diagonally to keep up

;; head moves up , tail should move diagonally
(define (test-11)
  (let ((hx 3)(hy 3)(hx2 3)(hy2 4)(tx 2)(ty 2)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-11 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))


;; head moves right , tail should move diagonally
(define (test-12)
  (let ((hx 3)(hy 3)(hx2 4)(hy2 3)(tx 2)(ty 2)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-12 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))

;; head moves down , tail should move diag
(define (test-13)
  (let ((hx 3)(hy 3)(hx2 3)(hy2 2)(tx 2)(ty 4)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-13 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 3)(= ty2 3)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))

;; head moves left , tail should move diag
(define (test-14)
  (let ((hx 2)(hy 3)(hx2 1)(hy2 3)(tx 3)(ty 2)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-14 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      ;;(assert (and (= tx2 2)(= ty2 3)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))


;; head moves up , tail should move diagonally
(define (test-21)
  (let ((hx 5)(hy 1)(hx2 5)(hy2 2)(tx 4)(ty 1)(tx2 0)(ty2 0))
    (let ((fxy (follow hx hy hx2 hy2 tx ty)))
      (set! tx2 (first fxy))
      (set! ty2 (second fxy))
      (format #t "TEST-21 : TX2 = ~a : TY2 = ~a ~%" tx2 ty2)
      (assert (and (= tx2 4)(= ty2 1)))
      (snake hx hy hx2 hy2 tx ty tx2 ty2))))


;; some unit tests
(define (run-tests)
  (test-1)
  (test-2)
  (test-3)
  (test-4)
  (test-11)
  (test-12)
  (test-13)
  (test-14)
  (test-21)
  )


#|

head position motion
hx,hy        ->   hx2,hy2

tx,ty  current tail position

compute next tail motion
(let ((tx2 0)(ty2 0))
 ...
(let ((fxy (follow hx hy hx2 hy2 tx ty)))
 (set! tx2 (first fxy))
 (set! ty2 (second fxy))
 ...
))

right 

|#


(define (model commands)
  (let ((hx 1)(hy 1)(hx2 1)(hy2 1)(tx 1)(ty 1)(tx2 1)(ty2 1)(step 0)(history '()))
    (letrec ((report (lambda ()
		       (snake2 hx hy hx2 hy2 tx ty tx2 ty2)
		       (set! history (cons (list tx2 ty2) history))
		       ))
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
		       (#t (set! hx2 (+ hx 1)) 
			   (set! hy2 (+ hy 0)) 
			   (let ((fxy (follow hx hy hx2 hy2 tx ty)))
			     (set! tx2 (first fxy))
			     (set! ty2 (second fxy))
			     ;; report before clobber 
			     (report)			    			     
			     ;; shift state
			     (set! hx hx2)
			     (set! hy hy2)
			     (set! tx tx2)
			     (set! ty ty2)
			     (right (- dist 1)))))))    
	     (left (lambda (dist)
		     (cond
		      ((< dist 1) 'done)  ;; left + model tail response
		      (#t (set! hx2 (- hx 1)) 
			  (set! hy2 (+ hy 0)) 
			  (let ((fxy (follow hx hy hx2 hy2 tx ty)))
			    (set! tx2 (first fxy))
			    (set! ty2 (second fxy))
			    ;; report before clobber 
			    (report)			    
			    ;; shift state
			    (set! hx hx2)
			    (set! hy hy2)
			    (set! tx tx2)
			    (set! ty ty2)
			    (left (- dist 1)))))))	     
	     (up (lambda (dist)
		   (cond
		    ((< dist 1) 'done)  ;; up + model tail response
		    (#t (set! hx2 (+ hx 0)) 
			(set! hy2 (+ hy 1)) 
			(let ((fxy (follow hx hy hx2 hy2 tx ty)))
			  (set! tx2 (first fxy))
			  (set! ty2 (second fxy))
			  ;; report before clobber 
			  (report)			    
			  ;; shift state
			  (set! hx hx2)
			  (set! hy hy2)
			  (set! tx tx2)
			  (set! ty ty2)
			  (up (- dist 1)))))))
	     (down (lambda (dist)
		     (cond
		      ((< dist 1) 'done)  ;; down + model tail response
		      (#t (set! hx2 (+ hx 0)) 
			  (set! hy2 (- hy 1)) 
			  (let ((fxy (follow hx hy hx2 hy2 tx ty)))
			    (set! tx2 (first fxy))
			    (set! ty2 (second fxy))
			    ;; report before clobber 
			    (report)			    
			    ;; shift state
			    (set! hx hx2)
			    (set! hy hy2)
			    (set! tx tx2)
			    (set! ty ty2)
			    (down (- dist 1)))))))
	     ); letrec-procs
      ;; start letrec
      (report)
      (help commands)
      history)))




;; visit-map
(define (visit-map visited)
  (letrec ((dig1 (lambda (x y)
		   (cond
		    ((member (list x y) visited) "#")
		    (else ".")))))
    ;;use letrec
    (format #t "~%")
    (format #t "VISITED~%")
    (format #t "~%")
    (format #t "~%")    
    (format #t "~a ~a ~a ~a ~a ~a~%" (dig1 1 5) (dig1 2 5) (dig1 3 5) (dig1 4 5) (dig1 5 5) (dig1 6 5))
    (format #t "~a ~a ~a ~a ~a ~a~%" (dig1 1 4) (dig1 2 4) (dig1 3 4) (dig1 4 4) (dig1 5 4) (dig1 6 4))
    (format #t "~a ~a ~a ~a ~a ~a~%" (dig1 1 3) (dig1 2 3) (dig1 3 3) (dig1 4 3) (dig1 5 3) (dig1 6 3))
    (format #t "~a ~a ~a ~a ~a ~a~%" (dig1 1 2) (dig1 2 2) (dig1 3 2) (dig1 4 2) (dig1 5 2) (dig1 6 2))
    (format #t "~a ~a ~a ~a ~a ~a~%" (dig1 1 1) (dig1 2 1) (dig1 3 1) (dig1 4 1) (dig1 5 1) (dig1 6 1))
    )) ;snake



(define (process hist)
  (let ((visit '()))
    (letrec ((help (lambda (xs)
		     (cond
		      ((null? xs) visit)
		      (#t (let ((pos (car xs)))
			    (cond
			     ((member pos visit) #f)
			     (else (set! visit (cons pos visit))))
			    (help (cdr xs))))))))
      (help hist))))



	   
#|  
(visit-map (process (small)))

. . # # . .
. . . # # .
. # # # # .
. . . . # .
# # # # . .
$55 = #t

(length (process (large)))
. . . . . . .
 5883
squares visited




|#




;; ;; just need locations touched first time , not a re-visit check
;; (define (process hist)
;;   (let ((seen `())
;; 	(visited '()))
;;     (letrec ((help (lambda (xs prev)
;; 		     ;;(format #t "process.help.[car xs] = ~a~%" (car xs))
;; 		     (cond
;; 		      ((null? xs) 'done)
;; 		      (else (let ((pos (car xs)))
;; 			      (format #t "process.pos = ~a ~%" pos)
;; 			      (let ((in-seen (member pos seen))
;; 				    (in-visited (member pos visited)))
;; 				(cond
;; 				 ((not in-seen) (set! seen (cons pos seen)))
;; 				 ((and prev (equal? pos prev)) ; not counted as visited
;; 				  #f)
;; 				 (else
;; 				  (format #t "revisit ~a ~%" pos)
;; 				  (set! visited (cons pos visited))))
				
;; 				(help (cdr xs) pos))))))))
;;       ;; letrec 
;;       (help hist #f)
;;       visited)))



(define (small)
  (let ((history (model (get-lines "input2"))))
    history))

(define (big)
  (let ((history (model (get-lines "input"))))
    history))



#|

now we want to model a long rope , made of smaller ropes ?

head as before ... 1 2 3 4 5 6 7 8 9

hx,hy    hx2,hy2
p1x,p1y  p1x2,p1y2
p2x,p2y  p2x2,p2y2
p3x,p3y  p3x2,p3y2
p4x,p4y  p4x2,p4y2
... what

initially all positions are set to 1,1 
.....................
         now        next
start    1,1       1,1
head     1,1       1,1
p1       1,1       1,1
p2       1,1       1,1
p3       1,1       1,1
p4       1,1       1,1
p5       1,1       1,1
p6       1,1       1,1
p7       1,1       1,1
p8       1,1       1,1
p9       1,1       1,1
.........................

head only moves L R U D
left ,right , up , down


h
t1 x y  x2 y2     : 
t2 x y  x2 y2     : t2x2 t2y2
t3 x y  x2 y2     : 
t4 x y  x2 y2     :
t5 x y  x2 y2     :
t6 x y  x7 y7     :
t7 x y  x2 y2     :
t8 x y  x2 y2     :
t9 x y  x2 y2     : t9x2 t9y2


|#


;; (define (follow hx hy hx2 hy2 tx ty)
;;   (format #t "follow : hx hy ~a ~a : hx2 hy2 ~a ~a : tx ty ~a ~a ~%" hx hy hx2 hy2 tx ty)
;;   (let* ((tx2 tx)
;; 	 (ty2 ty)
;; 	 (dx (- hx2 tx2))
;; 	 (dy (- hy2 ty2)))
;;     (cond
;;      ((or (= hx hx2 tx) ;; aligned either vertically or horizontally
;; 	  (= hy hy2 ty))
;;       (cond
;;        ((= dx 2)  (set! tx2 (+ tx2 1)))
;;        ((= dx -2) (set! tx2 (- tx2 1)))
;;        ((= dx 1) #f)
;;        ((= dx 0) #f)
;;        ((= dx -1) #f)
;;        (#t (error (list (format #f "illegal delta dx[~a]" dx) 'other-parameters hx hy tx ty dx dy))))
;;       (cond
;;        ((= dy 2)  (set! ty2 (+ ty2 1)))
;;        ((= dy -2) (set! ty2 (- ty2 1)))
;;        ((= dy 1) #f)
;;        ((= dy 0) #f)
;;        ((= dy -1) #f)
;;        (#t (error (list (format #f "illegal delta dy[~a]" dy) 'other-parameters hx hy tx ty dx dy))))
;;       (list tx2 ty2))
;;      (else
;;       (cond
;;        ;; > dx >dy
;;        ((and (= dx 1)(= dy 2)) (set! tx2 (+ tx2 1)) (set! ty2 (+ ty2 1)))
;;        ((and (= dx 2)(= dy 1)) (set! tx2 (+ tx2 1)) (set! ty2 (+ ty2 1)))
;;        ;; > dx < dy
;;        ((and (= dx 1)(= dy -2)) (set! tx2 (+ tx2 1)) (set! ty2 (- ty2 1)))
;;        ((and (= dx 2)(= dy -1)) (set! tx2 (+ tx2 1)) (set! ty2 (- ty2 1)))
;;        ;; < dx > dy
;;        ((and (= dx -1)(= dy 2)) (set! tx2 (- tx2 1)) (set! ty2 (+ ty2 1)))
;;        ((and (= dx -2)(= dy 1)) (set! tx2 (- tx2 1)) (set! ty2 (+ ty2 1)))
;;        ;; < dx < dy
;;        ((and (= dx -1)(= dy -2)) (set! tx2 (- tx2 1)) (set! ty2 (- ty2 1)))
;;        ((and (= dx -2)(= dy -1)) (set! tx2 (- tx2 1)) (set! ty2 (- ty2 1)))
;;        ;; no action required
;;        (#t #f))
;;       (list tx2 ty2)))))


;; (define-macro (splash t1x t1y t1x2 t1y2 t2x t2y t2x2 t2y2)
;;   (let ((g (gensym "g")))
;;   `(let ((,g (follow ,t1x ,t1y ,t1x2 ,t1y2 ,t2x ,t2y)))
;;      (set! ,t2x2 (first ,g))
;;      (set! ,t2y2 (second ,g))
;;      (format #t "splash")
;;      (format #t " : [~a ~a] : " ',t1x ',t1y)
;;      (format #t "(~a,~a) -> (~a,~a) : " ,t1x ,t1y ,t1x2 ,t1y2)
;;      (format #t " : [~a ~a] : " ',t2x ',t2y)     
;;      (format #t "(~a,~a) -> (~a,~a) " ,t2x ,t2y ,t2x2 ,t2y2)
;;      (format #t "~%"))))

(define-macro (splash ax ay ax2 ay2 bx by bx2 by2)
  `(begin
     (format #t "follow : ")
     (format #t "~a ~a ; ~a ~a => " ',ax ',ay ,ax ,ay)
     (format #t "~a ~a ; ~a ~a " ',ax2 ',ay2 ,ax2 ,ay2)
     (format #t "~a ~a ; ~a ~a => " ',bx ',by ,bx ,by)
     (format #t "~a ~a ; ~a ~a " ',bx2 ',by2 ,bx2 ,by2)
     ))

     
     
     

  ;; 	     hx2 hy2 ~a ~a : tx ty ~a ~a ~%" 
  ;; 	       hy2 tx ty)
  ;; (let* ((tx2 tx)
  ;; 	 (ty2 ty)
  ;; 	 (dx (- hx2 tx2))
  ;; 	 (dy (- hy2 ty2)))
  ;;   (cond
  ;;    ((or (= hx hx2 tx) ;; aligned either vertically or horizontally
  ;; 	  (= hy hy2 ty))
  ;;     (cond
  ;;      ((= dx 2)  (set! tx2 (+ tx2 1)))
  ;;      ((= dx -2) (set! tx2 (- tx2 1)))
  ;;      ((= dx 1) #f)
  ;;      ((= dx 0) #f)
  ;;      ((= dx -1) #f)
  ;;      (#t (error (list (format #f "illegal delta dx[~a]" dx) 'other-parameters hx hy tx ty dx dy))))
  ;;     (cond
  ;;      ((= dy 2)  (set! ty2 (+ ty2 1)))
  ;;      ((= dy -2) (set! ty2 (- ty2 1)))
  ;;      ((= dy 1) #f)
  ;;      ((= dy 0) #f)
  ;;      ((= dy -1) #f)
  ;;      (#t (error (list (format #f "illegal delta dy[~a]" dy) 'other-parameters hx hy tx ty dx dy))))
  ;;     (list tx2 ty2))
  ;;    (else
  ;;     (cond
  ;;      ;; > dx >dy
  ;;      ((and (= dx 1)(= dy 2)) (set! tx2 (+ tx2 1)) (set! ty2 (+ ty2 1)))
  ;;      ((and (= dx 2)(= dy 1)) (set! tx2 (+ tx2 1)) (set! ty2 (+ ty2 1)))
  ;;      ;; > dx < dy
  ;;      ((and (= dx 1)(= dy -2)) (set! tx2 (+ tx2 1)) (set! ty2 (- ty2 1)))
  ;;      ((and (= dx 2)(= dy -1)) (set! tx2 (+ tx2 1)) (set! ty2 (- ty2 1)))
  ;;      ;; < dx > dy
  ;;      ((and (= dx -1)(= dy 2)) (set! tx2 (- tx2 1)) (set! ty2 (+ ty2 1)))
  ;;      ((and (= dx -2)(= dy 1)) (set! tx2 (- tx2 1)) (set! ty2 (+ ty2 1)))
  ;;      ;; < dx < dy
  ;;      ((and (= dx -1)(= dy -2)) (set! tx2 (- tx2 1)) (set! ty2 (- ty2 1)))
  ;;      ((and (= dx -2)(= dy -1)) (set! tx2 (- tx2 1)) (set! ty2 (- ty2 1)))
  ;;      ;; no action required
  ;;      (#t #f))
  ;;     (list tx2 ty2)))))



(define (train commands)
  (let ((h1x 1)(h1y 1)(h1x2 1)(h1y2 1)
	(t1x 1)(t1y 1)(t1x2 1)(t1y2 1)
	(t2x 1)(t2y 1)(t2x2 1)(t2y2 1)
	(t3x 1)(t3y 1)(t3x2 1)(t3y2 1)
	(t4x 1)(t4y 1)(t4x2 1)(t4y2 1)
	(t5x 1)(t5y 1)(t5x2 1)(t5y2 1)
	(t6x 1)(t6y 1)(t6x2 1)(t6y2 1)
	(t7x 1)(t7y 1)(t7x2 1)(t7y2 1)
	(t8x 1)(t8y 1)(t8x2 1)(t8y2 1)
	(t9x 1)(t9y 1)(t9x2 1)(t9y2 1)
	(step 0)(history '()))
    (letrec ((report (lambda ()
		       
		       ;; whose history are we recording exactly ? 
		       ;;(snake2 hx hy hx2 hy2 tx ty tx2 ty2)
		       (format #t "~%~%")
		       
		       ;; track the t9 item item
		       (set! history (cons (list t9x2 t9y2) history))
		       ))
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
		       (#t (set! h1x2 (+ h1x 1)) 
			   (set! h1y2 (+ h1y 0))

			   (splash h1x h1y h1x2 h1y2 t1x t1y t1x2 t1y2)

			   (splash t1x t1y t1x2 t1y2 t2x t2y t2x2 t2y2)
			   (splash t2x t2y t2x2 t2y2 t3x t3y t3x2 t3y2)
			   (splash t3x t3y t3x2 t3y2 t4x t4y t4x2 t4y2)
			   (splash t4x t4y t4x2 t4y2 t5x t5y t5x2 t5y2)
			   (splash t5x t5y t5x2 t5y2 t6x t6y t6x2 t6y2)
			   (splash t6x t6y t6x2 t6y2 t7x t7y t7x2 t7y2)
			   (splash t7x t7y t7x2 t7y2 t8x t8y t8x2 t8y2)
			   (splash t8x t8y t8x2 t8y2 t9x t9y t9x2 t9y2)

			     ;; report before clobber 
			     (report)			    			     
			     ;; shift state			     
			     (set! h1x h1x2)  (set! h1y h1y2)
			     (set! t1x t1x2)  (set! t1y t1y2)
			     (set! t2x t2x2)  (set! t2y t2y2)
			     (set! t3x t3x2)  (set! t3y t3y2)
			     (set! t4x t4x2)  (set! t4y t4y2)
			     (set! t5x t5x2)  (set! t5y t5y2)
			     (set! t6x t6x2)  (set! t6y t6y2)
			     (set! t7x t7x2)  (set! t7y t7y2)
			     (set! t8x t8x2)  (set! t8y t8y2)
			     (set! t9x t9x2)  (set! t9y t9y2)

			     
			     
			     (right (- dist 1))))))
	     (left (lambda (dist)
		     (cond
		      ((< dist 1) 'done)  ;; left + model tail response
		      (#t (set! h1x2 (- h1x 1)) 
			  (set! h1y2 (+ h1y 0))

			  ;;
			   (splash h1x h1y h1x2 h1y2 t1x t1y t1x2 t1y2)
			   
			   (splash t1x t1y t1x2 t1y2 t2x t2y t2x2 t2y2)
			   (splash t2x t2y t2x2 t2y2 t3x t3y t3x2 t3y2)
			   (splash t3x t3y t3x2 t3y2 t4x t4y t4x2 t4y2)
			   (splash t4x t4y t4x2 t4y2 t5x t5y t5x2 t5y2)
			   (splash t5x t5y t5x2 t5y2 t6x t6y t6x2 t6y2)
			   (splash t6x t6y t6x2 t6y2 t7x t7y t7x2 t7y2)
			   (splash t7x t7y t7x2 t7y2 t8x t8y t8x2 t8y2)
			   (splash t8x t8y t8x2 t8y2 t9x t9y t9x2 t9y2)
			     
			     ;; report before clobber 
			     (report)			    			     
			     ;; shift state			     
			     (set! h1x h1x2)  (set! h1y h1y2)
			     (set! t1x t1x2)  (set! t1y t1y2)
			     (set! t2x t2x2)  (set! t2y t2y2)
			     (set! t3x t3x2)  (set! t3y t3y2)
			     (set! t4x t4x2)  (set! t4y t4y2)
			     (set! t5x t5x2)  (set! t5y t5y2)
			     (set! t6x t6x2)  (set! t6y t6y2)
			     (set! t7x t7x2)  (set! t7y t7y2)
			     (set! t8x t8x2)  (set! t8y t8y2)
			     (set! t9x t9x2)  (set! t9y t9y2)
			  
			    (left (- dist 1))))))
	     (up (lambda (dist)
		   (cond
		    ((< dist 1) 'done)  ;; up + model tail response
		    (#t (set! h1x2 (+ h1x 0)) 
			(set! h1y2 (+ h1y 1))

			;;
			 (splash h1x h1y h1x2 h1y2 t1x t1y t1x2 t1y2)
			   
			   (splash t1x t1y t1x2 t1y2 t2x t2y t2x2 t2y2)
			   (splash t2x t2y t2x2 t2y2 t3x t3y t3x2 t3y2)
			   (splash t3x t3y t3x2 t3y2 t4x t4y t4x2 t4y2)
			   (splash t4x t4y t4x2 t4y2 t5x t5y t5x2 t5y2)
			   (splash t5x t5y t5x2 t5y2 t6x t6y t6x2 t6y2)
			   (splash t6x t6y t6x2 t6y2 t7x t7y t7x2 t7y2)
			   (splash t7x t7y t7x2 t7y2 t8x t8y t8x2 t8y2)
			   (splash t8x t8y t8x2 t8y2 t9x t9y t9x2 t9y2)
			     
			     ;; report before clobber 
			     (report)			    			     
			     ;; shift state			     
			     (set! h1x h1x2)  (set! h1y h1y2)
			     (set! t1x t1x2)  (set! t1y t1y2)
			     (set! t2x t2x2)  (set! t2y t2y2)
			     (set! t3x t3x2)  (set! t3y t3y2)
			     (set! t4x t4x2)  (set! t4y t4y2)
			     (set! t5x t5x2)  (set! t5y t5y2)
			     (set! t6x t6x2)  (set! t6y t6y2)
			     (set! t7x t7x2)  (set! t7y t7y2)
			     (set! t8x t8x2)  (set! t8y t8y2)
			     (set! t9x t9x2)  (set! t9y t9y2)
			  
			  (up (- dist 1))))))
	     (down (lambda (dist)
		     (cond
		      ((< dist 1) 'done)  ;; down + model tail response
		      (#t (set! h1x2 (+ h1x 0)) 
			  (set! h1y2 (- h1y 1))

			  ;;
			   (splash h1x h1y h1x2 h1y2 t1x t1y t1x2 t1y2)
			   
			   (splash t1x t1y t1x2 t1y2 t2x t2y t2x2 t2y2)
			   (splash t2x t2y t2x2 t2y2 t3x t3y t3x2 t3y2)
			   (splash t3x t3y t3x2 t3y2 t4x t4y t4x2 t4y2)
			   (splash t4x t4y t4x2 t4y2 t5x t5y t5x2 t5y2)
			   (splash t5x t5y t5x2 t5y2 t6x t6y t6x2 t6y2)
			   (splash t6x t6y t6x2 t6y2 t7x t7y t7x2 t7y2)
			   (splash t7x t7y t7x2 t7y2 t8x t8y t8x2 t8y2)
			   (splash t8x t8y t8x2 t8y2 t9x t9y t9x2 t9y2)
			     
			     ;; report before clobber 
			     (report)			    			     
			     ;; shift state			     
			     (set! h1x h1x2)  (set! h1y h1y2)
			     (set! t1x t1x2)  (set! t1y t1y2)
			     (set! t2x t2x2)  (set! t2y t2y2)
			     (set! t3x t3x2)  (set! t3y t3y2)
			     (set! t4x t4x2)  (set! t4y t4y2)
			     (set! t5x t5x2)  (set! t5y t5y2)
			     (set! t6x t6x2)  (set! t6y t6y2)
			     (set! t7x t7x2)  (set! t7y t7y2)
			     (set! t8x t8x2)  (set! t8y t8y2)
			     (set! t9x t9x2)  (set! t9y t9y2)
			  
			    (down (- dist 1))))))
	     ); letrec-procs
      ;; start letrec
      (report)
      (help commands)
      history)))




(define (small-train)
  (let ((history (train (get-lines "input2"))))
    history))

(define (big-train)
  (let ((history (train (get-lines "input"))))
    history))





