
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

;; statistical profiler
(use-modules (statprof))

(define pp pretty-print)


;; list utilities
(use-modules (srfi srfi-1))


#|

----------------some basic macros make scheme language usable -------------------------------

|#

;; dolist
;;(dolist (x xs) ...)
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

;;(dolist (x '(1 2 3)) (display "x = ") (display x) (newline))
;; guile when defined

#|

----------------some basic macros make scheme language usable -------------------------------

|#


#|

fun3.scm

example + problem

one thing nice about scheme is functional code , not concerned about whether a procedure is
going to absolutely trash the data structure under feet by mutation - see sort common lisp


|#


(define example
  (lambda ()
    '(  
      (AA 0 DD II BB)
      (BB 13 CC AA)
      (CC 2 DD BB)
      (DD 20 CC AA EE)
      (EE 3 FF DD)
      (FF 0 EE GG)
      (GG 0 FF HH)
      (HH 22 GG)
      (II 0 AA JJ)
      (JJ 21 II)
      )))


#|
moved AA to top of the list

|#
(define problem
  (lambda ()
    '(
      (AA 0  NC  VY  BH  TQ  YJ)
      (AW 0  LG  TL)
      (OM 0  XK  IM)
      (BG 0  MP  SB)
      (XB 0  MA  TL)
      (CD 0  VL  OF)
      (VF 0  CS  XK)
      (HK 0  RL  QB)
      (QN 0  IV  QR)
      (OF 4  TQ  CD  IR  IM  JE)
      (QB 14  HK  XE  CS  VO)
      (ZE 7  JB  NC  SE  OI)
      (OW 0  MB  JB)
      (MA 0  XB  MB)
      (MP 0  VK  BG)
      (UE 9  ZM  RZ  WI  HO  FO)
      (QR 24 QN)
      (TQ 0  OF  AA)
      (SE 0  ZE  ZZ)
      (AQ 20 CX)
      (XE 0  JQ  QB)
      (DC 8  ZD  MJ  RZ)
      (ZM 0  YJ  UE)
      (VK 21 MP)
      (VR 0  WV  PS)
      (BH 0  AA  MB)
      (ZR 0  LG  AI)
      (JE 0  OF  HO)
      (IR 0  IV  OF)
      (FO 0  XQ  UE)
      (ZZ 0  SE  TL)
      (XQ 0  IV  FO)
      (WI 0  UE  VO)
      (VY 0  AA  LG)
      (XK 15  VF  OM  ZD)
      (CX 0  AQ  MB)
      (JQ 0  XE  IV)
      (LG 3  VY  PS  ZR  AW  OI)
      (JB 0  ZE  OW)
      (OI 0  ZE  LG)
      (YJ 0  ZM  AA)
      (NC 0  AA  ZE)
      (KR 0  SB  MJ)
      (MB 17  CX  BH  AI  OW  MA)
      (AI 0  ZR  MB)
      (TL 16  ZZ  XB  AW)
      (RL 0  WV  HK)
      (CS 0  VF  QB)
      (WV 25  RL  VL  VR)
      (ZD 0  XK  DC)
      (IV 23  XQ  IR  JQ  QN)
      (PS 0  VR  LG)
      (RZ 0  DC  UE)
      (VO 0  WI  QB)
      (MJ 0  DC  KR)
      (IM 0  OM  OF)
      (VL 0  CD  WV)
      (SB 18  BG  KR)
      (HO 0  JE  UE)
      )))



(define *problem-sites*  (map car (problem)))

;;encode?
(define (loc->int s)
  (let ((xs *problem-sites*)
	(i 0))
    (call/cc (lambda (exit)
	       (while (not (null? xs))
		 (when (eq? s (car xs))
		   (exit i))
		 (set! xs (cdr xs))
		 (set! i (+ i 1)))
	       (error "loc->int loc not found")))))

(define (int->loc i)
  (let ((xs *problem-sites*))
    (while (> i 0)
      (set! xs (cdr xs))
      (set! i (- i 1)))
    (car xs)))



(define (check-loc-int)
  (dolist (p *problem-sites*)
	  (format #t "~a -> ~a~%" p (loc->int p))))

(define (check-int-loc)
  (dolist (p '( 0 1 2 3 4 5 6 7 8 9
	     10 11 12 13 14 15 16 17 18 19
	     20 21 22 23 24 25 26 27 28 29
	     30 31 32 33 34 35 36 37 38 39
	     40 41 42 43 44 45 46 47 48 49
	     50 51 52 53 54 55 56 57 58
	     ))
	  (format #t "~a -> ~a~%" p (int->loc p))))


(define *encoded-problem*
  (map (lambda (xs)
	 (let ((loc (loc->int (first xs)))
	       (pay (second xs))
	       (tunnels (map loc->int (cdr (cdr xs)))))
	   (cons loc (cons pay tunnels))))
       (problem)))


(define old-flow-rate
  (lambda (s)
    (cadr (assoc s (problem)))))

(define (check-old-flow-rate)
  (dolist (loc *problem-sites*)
	  (display "loc ")
	  (display loc)	
	  (display " ... ")
	  (display (old-flow-rate loc))
	  (newline)))

;; shortest-path
;; flow-rate
(define flow-rate
  (let ((arr (make-array 0 (length *problem-sites*))))
    (dolist (p *problem-sites*)
	    (let ((pi (loc->int p))
		  (flow (old-flow-rate p)))
	      (array-set! arr flow pi)))
    (lambda (s)
      (array-ref arr (loc->int s)))))

(define (check-flow-rate)
  (dolist (loc *problem-sites*)
	  (display "loc ")
	  (display loc)	
	  (display " ... ")
	  (display (flow-rate loc))
	  (newline)))

(define encoded-flow-rate
  (let ((arr (make-array 0 (length *problem-sites*))))
    (dolist (p *problem-sites*)
	    (let ((pi (loc->int p))
		  (flow (old-flow-rate p)))
	      (array-set! arr flow pi)))
    (lambda (i)
      (array-ref arr i))))



(define (check-encoded-flow-rate)
  (do ((i 1 (1+ i)))
    ((> i 58))
    (display "loc ")
    (display (int->loc i))	
    (display " ... ")
    (display (encoded-flow-rate i))
    (newline)))


;; iota 100  0 .. 99 inclusive
;; 
;; (statprof (lambda ()
;;             (map encoded-flow-rate (iota 59))
;;             #f))

;; (statprof (lambda ()
;;             (map flow-rate *problem-sites*)
;;             #f))
;; no samples ?

  

#|
LOC one of locations in problem  AW ... HO
guile scheme uppercase letters important on symbol

can-go : LOC -> LIST of LOC

|#
(define can-go
  (lambda (from)
    (let ((input (problem)))
      (let ((res (cdr (cdr (assoc from input)))))
	res))))

(define (check-can-go)
  (dolist (loc *problem-sites*)
	  (display "loc ")
	  (display loc)	
	  (display " ... ")
	  (display (can-go loc))
	  (newline)))



(define who-pays
  (lambda ()
    (let ((input (problem)))
      (let ((vals 
	     (sort 
	      (filter (lambda (x) (> (car (cdr  x)) 0))
		      (map (lambda (x) (list (car x) (car (cdr x)))) input))
	      (lambda (x y)(> (car (cdr x)) (car (cdr y)))))))
	(values (map car vals)
		(map (lambda (x) (car (cdr x))) vals))))))


(define *pays* (who-pays))







;; (let ((xs '(dolist (x xs) 1 2 3)))
;;   (car (cdr xs)))



;; missing when macro
;; missing while macro ?
(define shortest-path
  (lambda (from to)
    (let ((best 99999999999)
	  (best-path '()))
      (letrec ((trec2
		(lambda (loc seen dist)
		  (cond
		   ((eq? loc to)
		    ;; (display "here ... distance ")
		    ;; (display dist)
		    ;; (display " .. ")
		    ;; (display seen)
		    ;; (newline)
		    (cond
		     ((< dist best)
		      (set! best dist)
		      (set! best-path seen))
		     (#t #t))
		    dist)
		   (#t (let ((reach (can-go loc)))
			(dolist (r reach)
			(cond
			 ((member r seen)
			  ;;(display "already seen ... skipping ") (newline)
			  #t)
			 (#t (trec2 r (cons r seen) (+ dist 1)))))))))))
	(let ((dist 0)
	      (seen (list from)))
	  (trec2 from seen dist)
	  (values best best-path))))))


(define (check-shortest-path)
  (dolist (loc *problem-sites*)
	  (dolist (loc2 *problem-sites*)
		  (when (not (eq? loc loc2))
		    (display "...dist ")
		    (display loc)
		    (display " ... ")
		    (display loc2)
		    (display " ... ")
		    (display (shortest-path loc loc2))
		    (newline)))))

#|
fudge stop repl from printing path-array ??
|#
(define *path-array* #f)

(set! *path-array*
  (let ((arr (make-array 0
			 (length *problem-sites*)
			 (length *problem-sites*))))
    (dolist (p *problem-sites*)
	    (dolist (q *problem-sites*)
		    (let ((pi (loc->int p))
			  (qi (loc->int q))
			  (dist (shortest-path p q)))
		      (array-set! arr dist pi qi))))
    arr))
    

(define fastest-path
  (lambda (a b)
    (array-ref *path-array*
	       (loc->int a)
	       (loc->int b))))











;; could make it a hash table
;; lets see what greedy does

;;(define (benefit loc
#|
benefit from going to A versus B

distance walk in time
one unit to open valve
flow-rate valve

seen
possible to pass-through same gate multiple times ?

|#




(define greedy
  (lambda ()
    (let ((sols '())
	  (best-flow 0)
	  (best-walk '()))
      (letrec ((trec (lambda (loc time seen locs flow opened)
		       (cond
			((> time 0)
		       ;; best flow found
		       (when
			   (> flow best-flow)
			 (set! best-flow flow)
			 (set! best-walk seen)
			 ;;
			 (newline)
			 (display "*********** best flow found *********** ")
			 (newline)
			 (display best-walk)
			 (newline)
			 (display "best flow : ")
			 (display best-flow)
			 (display " : time remain : ")
			 (display time)			 
			 (newline)
			 )
				  
		       (cond
			((>= (length seen) 15)
			 (display "are we done ? flow ")
			 (display flow)
			 (display " : time ")
			 (display time)
			 (newline)
			 (set! sols (cons (list 'flow flow) sols))
			 )
			(#t (dolist (loc2 locs)
				   (cond
				    ((member loc2 opened)
				     ;; (display "have we turned valve [")
				     ;; (display loc2)
				     ;; (display "] off already ?")
				     ;; (newline)
				     #t)
				    (#t (let ((walk (shortest-path loc loc2)))
					  (let ((time2 (- time walk)))
					    (when (> time2 0)
					      (let ((extra-flow (* (flow-rate loc2) (- time2 1))))
						(trec loc2 (- time2 1) (cons loc2 seen) locs
						      (+ flow extra-flow)
						      (cons loc2 opened))))))))))))
			(#t #t)))))
		       
	(let ((start 'AA)
	      (seen '())
	      (flow 0)
	      (opened '())
	      (time 30))
	  (trec start time seen (who-pays) flow opened)
	  sols)))))




#|
given currnet location and valves opened - of those valves not opened

sort them
start at 'AA
no-valves-open

compute next states
 


(let ((pays (who-pays)))
  (define (next-state loc opened time flow)
    (let ((reach (map (lambda (sym) (list sym (cons )
		      (filter (lambda (x) (not (member x opened))) pays))))
      reach)))

|#


;; where can go from given location?
(define f2
  (let ((pays (who-pays)))
    (lambda (loc)
      (let ((reach (map (lambda (to) (list 'dist loc to (shortest-path loc to)))
			pays)))
	reach))))

;; (f2 'AA)
;;$30 = ((dist AA WV 5) (dist AA QR 6) (dist AA IV 4) (dist AA VK 11) (dist AA AQ 4) (dist AA SB 8) (dist AA MB 2) (dist AA TL 4) (dist AA XK 5) (dist AA QB 6) (dist AA UE 3) (dist AA DC 5) (dist AA ZE 2) (dist AA OF 2) (dist AA LG 2))

;; filter out those that are too far away for avail time
;; distance has to be less than time
;; so get there turn on valve
;; 1 turn on valve + walk dist
(define f3
  (lambda (loc time)
    (let ((xs (f2 loc)))	  
      (let ((ys (filter (lambda (x)
			  (let ((dist (fourth x)))
			    (< dist time)))
			xs)))
	ys))))

;; 
(define f4
  (lambda (loc time opened)
    (let ((xs (f3 loc time)))	  
      (let ((ys (filter (lambda (x)
			  (let ((loc2 (fourth x)))
			    (not (member loc2 opened))))
			xs)))
	ys))))



(define f5
  (lambda (loc flow time opened)
    (let ((xs (f4 loc time opened)))
      (map (lambda (y)
	     (let ((dist (fourth y))
		   (loc2 (third y)))
	       (let ((time2 (- time dist 1))
		     (flow2 (flow-rate loc2)))					
		 (list 'state loc2 (+ flow (* time2 flow2)) time2 (cons loc2 opened)))))
	   xs))))


(define stage1 (f5 'AA 0 30 '()))

;; (define stage2 (apply append (map (lambda (x)
;; 				    (let ((loc (second x))
;; 					  (flow (third x))
;; 					  (time (fourth x))
;; 					  (opened (fifth x)))
;; 				      (f5 loc flow time opened)))
;; 				  stage1)))

;; (define stage3 (apply append (map (lambda (x)
;; 				    (let ((loc (second x))
;; 					  (flow (third x))
;; 					  (time (fourth x))
;; 					  (opened (fifth x)))
;; 				      (f5 loc flow time opened)))
;; 				  stage2)))

;; (define stage4 (apply append (map (lambda (x)
;; 				    (let ((loc (second x))
;; 					  (flow (third x))
;; 					  (time (fourth x))
;; 					  (opened (fifth x)))
;; 				      (f5 loc flow time opened)))
;; 				  stage3)))

;; although this is nice idea , on a strict system it tries to evaluate each oone
;; exponential growth size of these , strictness here may be a hinderance

;; (let ((loc1 'aa))
;;   (let ((loc2 

#|

initial start AA is not a paying gate - opening AA valve does not contribute ?
these are ones that pay (WV QR IV VK AQ SB MB TL XK QB UE DC ZE OF LG)
with these amounts      (25 24 23 21 20 18 17 16 15 14 9 8 7 4 3)
|#

	 


(define (trec)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(best-path 0))
    (dolist (loc2 *pays*)
	    (let* ((walk (shortest-path loc1 loc2))
		   (time (- time walk 1)))
	      (when (> time 0)
		(let ((flow (+ flow (* time (flow-rate loc2)))))
		  (when (> flow best)
		    (set! best flow)
		    (set! best-path (list loc1 loc2))
		    (format #t "flow best ~a : ~a ~%" flow (list loc1 loc2))
		    #t
		    )))))))


;; (dolist (loc3 *pays*)
;; 	(dolist (loc4 *pays*)
(define-macro (seek loc1 loc2 condition . body)
  `(dolist (,loc2 *pays*)
	   (when ,condition
	     (let* ((walk (fastest-path ,loc1 ,loc2))
		    (time (- time walk 1)))
	       (when (> time 0)
		 (let ((flow (+ flow (* time (flow-rate ,loc2)))))
		   ,@body
		   ))))))



(define (trec2)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(best-path 0))
    (seek loc1 loc2 #t	 
	  (when (> flow best)
	    (set! best flow)
	    (set! best-path (list loc1 loc2))
	    (format #t "flow best ~a : ~a ~%" flow (list loc1 loc2))
	    #t
	    ))))


(define (trec3)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(when (> flow best)
		  (set! best flow)
		  (set! best-path (list loc1 loc2 loc3))
		  (format #t "flow best ~a : ~a ~%" flow (list loc1 loc2 loc3))
		  #t
		  )))))


(define (differ a b)
  (not (eq? a b)))

(define (trec4)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4) (differ loc3 loc4))
		      (set! cases (+ cases 1))
		      (when (> flow best)
			(set! best flow)
			(set! best-path (list loc1 loc2 loc3 loc4))
			(format #t "flow best ~a : ~a ~%" flow (list loc1 loc2 loc3 loc4))
			cases
			))))
    cases))

;; trec4 was 
;; 34.585907s real time, 37.538752s run time.  3.680404s spent in GC.

(define (trec4b)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4) (differ loc3 loc4))
		      (set! cases (+ cases 1))
		      (when (> flow best)
			(set! best flow)
			(set! best-path (list loc1 loc2 loc3 loc4))
			(format #t "flow best ~a : ~a ~%" flow (list loc1 loc2 loc3 loc4))
			cases
			))))
    cases))


;; 2-3
;; 2   3   4
;;        3-4
;;        2-4
;; 2   3   4   5
;;            4-5
;;            3-5
;;            2-5
;;     
(define (trec5)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4) (differ loc3 loc4))
		      (seek loc4 loc5 (and (differ loc2 loc5) (differ loc3 loc5) (differ loc4 loc5))
			    (set! cases (+ cases 1))
			    (when (> flow best)
			      (set! best flow)
			      (set! best-path (list loc1 loc2 loc3 loc4 loc5))
			      (format #t "flow best ~a : path ~a : time ~a ~%"
				      flow
				      (list loc1 loc2 loc3 loc4 loc5)
				      time)
			      cases
			      )))))
    cases))






(define (trec6)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4)
				     (differ loc3 loc4))
		      (seek loc4 loc5 (and (differ loc2 loc5)
					   (differ loc3 loc5)
					   (differ loc4 loc5))
			    (seek loc5 loc6 (and (differ loc2 loc6)
						 (differ loc3 loc6)
						 (differ loc4 loc6)
						 (differ loc5 loc6))	    
			    (set! cases (+ cases 1))
			    (when (> flow best)
			      (set! best flow)
			      (set! best-path (list loc1 loc2 loc3 loc4 loc5 loc6))
			      (format #t "flow best ~a : path ~a : time ~a ~%"
				      flow
				      (list loc1 loc2 loc3 loc4 loc5 loc6)
				      time)
			      cases
			      ))))))
    cases))



(define (trec7)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4)
				     (differ loc3 loc4))
		      (seek loc4 loc5 (and (differ loc2 loc5)
					   (differ loc3 loc5)
					   (differ loc4 loc5))
			    (seek loc5 loc6 (and (differ loc2 loc6)
						 (differ loc3 loc6)
						 (differ loc4 loc6)
						 (differ loc5 loc6))
				  (seek loc6 loc7 (and (differ loc2 loc7)
						       (differ loc3 loc7)
						       (differ loc4 loc7)
						       (differ loc5 loc7)
						       (differ loc6 loc7))
			    (set! cases (+ cases 1))
			    (when (> flow best)
			      (set! best flow)
			      (set! best-path (list loc1 loc2 loc3 loc4 loc5 loc6 loc7))
			      (format #t "flow best ~a : path ~a : time ~a ~%"
				      flow
				      (list loc1 loc2 loc3 loc4 loc5 loc6 loc7)
				      time)
			      cases
			      )))))))
    cases))


(define (trec8)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4)
				     (differ loc3 loc4))
		      (seek loc4 loc5 (and (differ loc2 loc5)
					   (differ loc3 loc5)
					   (differ loc4 loc5))
			    (seek loc5 loc6 (and (differ loc2 loc6)
						 (differ loc3 loc6)
						 (differ loc4 loc6)
						 (differ loc5 loc6))
				  (seek loc6 loc7 (and (differ loc2 loc7)
						       (differ loc3 loc7)
						       (differ loc4 loc7)
						       (differ loc5 loc7)
						       (differ loc6 loc7))
					(seek loc7 loc8 (and (differ loc2 loc8)
						       (differ loc3 loc8)
						       (differ loc4 loc8)
						       (differ loc5 loc8)
						       (differ loc6 loc8)
						       (differ loc7 loc8))
			    (set! cases (+ cases 1))
			    (when (> flow best)
			      (set! best flow)
			      (set! best-path (list loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8))
			      (format #t "flow best ~a : path ~a : time ~a ~%"
				      flow
				      (list loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8)
				      time)
			      cases
			      ))))))))
    cases))




(define (trec9)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4)
				     (differ loc3 loc4))
		      (seek loc4 loc5 (and (differ loc2 loc5)
					   (differ loc3 loc5)
					   (differ loc4 loc5))
			    (seek loc5 loc6 (and (differ loc2 loc6)
						 (differ loc3 loc6)
						 (differ loc4 loc6)
						 (differ loc5 loc6))
				  (seek loc6 loc7 (and (differ loc2 loc7)
						       (differ loc3 loc7)
						       (differ loc4 loc7)
						       (differ loc5 loc7)
						       (differ loc6 loc7))
					(seek loc7 loc8 (and (differ loc2 loc8)
						       (differ loc3 loc8)
						       (differ loc4 loc8)
						       (differ loc5 loc8)
						       (differ loc6 loc8)
						       (differ loc7 loc8))
					      (seek loc8 loc9 (and (differ loc2 loc9)
						       (differ loc3 loc9)
						       (differ loc4 loc9)
						       (differ loc5 loc9)
						       (differ loc6 loc9)
						       (differ loc7 loc9)
						       (differ loc8 loc9))
			    (set! cases (+ cases 1))
			    (when (> flow best)
			      (set! best flow)
			      (set! best-path (list loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8 loc9))
			      (format #t "flow best ~a : path ~a : time ~a ~%"
				      flow
				      (list loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8 loc9)
				      time)
			      cases
			      )))))))))
    cases))


(define (trec10)
  (let ((loc1 'AA)
	(time 30)
	(dist 0)
	(flow 0)
	(best 0)
	(cases 0)
	(best-path 0))
    (seek loc1 loc2 #t
	  (seek loc2 loc3 (differ loc2 loc3)
		(seek loc3 loc4 (and (differ loc2 loc4)
				     (differ loc3 loc4))
		      (seek loc4 loc5 (and (differ loc2 loc5)
					   (differ loc3 loc5)
					   (differ loc4 loc5))
			    (seek loc5 loc6 (and (differ loc2 loc6)
						 (differ loc3 loc6)
						 (differ loc4 loc6)
						 (differ loc5 loc6))
				  (seek loc6 loc7 (and (differ loc2 loc7)
						       (differ loc3 loc7)
						       (differ loc4 loc7)
						       (differ loc5 loc7)
						       (differ loc6 loc7))
					(seek loc7 loc8 (and (differ loc2 loc8)
						       (differ loc3 loc8)
						       (differ loc4 loc8)
						       (differ loc5 loc8)
						       (differ loc6 loc8)
						       (differ loc7 loc8))
					      (seek loc8 loc9 (and (differ loc2 loc9)
						       (differ loc3 loc9)
						       (differ loc4 loc9)
						       (differ loc5 loc9)
						       (differ loc6 loc9)
						       (differ loc7 loc9)
						       (differ loc8 loc9))
						    (seek loc9 loc10 (and (differ loc2 loc10)
						       (differ loc3 loc10)
						       (differ loc4 loc10)
						       (differ loc5 loc10)
						       (differ loc6 loc10)
						       (differ loc7 loc10)
						       (differ loc8 loc10)
						       (differ loc9 loc10)
						       )
			    (set! cases (+ cases 1))
			    (when (> flow best)
			      (set! best flow)
			      (set! best-path (list loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8 loc9 loc10))
			      (format #t "flow best ~a : path ~a : time ~a ~%"
				      flow
				      (list loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8 loc9 loc10)
				      time)
			      cases
			      ))))))))))
    cases))


#|

scheme@(guile-user)> ,time (trec6)
flow best 1380 : path (AA WV QR IV AQ MB) : time 1 
flow best 1445 : path (AA WV QR IV MB AQ) : time 3 
flow best 1484 : path (AA WV QR IV QB XK) : time 5 
flow best 1536 : path (AA WV IV QR MB AQ) : time 3 
flow best 1575 : path (AA WV IV QR QB XK) : time 5 
flow best 1600 : path (AA WV QB IV QR AQ) : time 2 
flow best 1628 : path (AA WV QB IV QR MB) : time 4 
flow best 1635 : path (AA WV QB IV QR XK) : time 5 
flow best 1652 : path (AA IV QR WV MB AQ) : time 4 
flow best 1683 : path (AA IV QR WV QB XK) : time 6 
flow best 1687 : path (AA IV QR QB WV AQ) : time 3 
flow best 1712 : path (AA IV QR QB WV MB) : time 5 
flow best 1723 : path (AA IV QR QB WV TL) : time 6 
$14 = 72797
;; 1.859492s real time, 6.372284s run time.  5.576332s spent in GC.
scheme@(guile-user)> ,time (trec7)
flow best 1429 : path (AA WV QR IV XK DC UE) : time 1 
flow best 1500 : path (AA WV QR IV QB XK DC) : time 2 
flow best 1520 : path (AA WV IV QR XK DC UE) : time 1 
flow best 1591 : path (AA WV IV QR QB XK DC) : time 2 
flow best 1648 : path (AA WV QB IV QR MB AQ) : time 1 
flow best 1651 : path (AA WV QB IV QR XK DC) : time 2 
flow best 1653 : path (AA WV QB IV QR UE XK) : time 2 
flow best 1655 : path (AA WV QB IV QR UE DC) : time 4 
flow best 1661 : path (AA QR IV QB WV MB AQ) : time 2 
flow best 1666 : path (AA QR IV QB WV TL MB) : time 2 
flow best 1669 : path (AA IV QR WV TL MB AQ) : time 1 
flow best 1692 : path (AA IV QR WV QB XK UE) : time 1 
flow best 1707 : path (AA IV QR WV QB XK DC) : time 3 
flow best 1752 : path (AA IV QR QB WV MB AQ) : time 2 
flow best 1757 : path (AA IV QR QB WV TL MB) : time 2 
flow best 1785 : path (AA MB TL WV QB IV QR) : time 6 
$15 = 80768
;; 6.686968s real time, 22.569868s run time.  19.676802s spent in GC.
scheme@(guile-user)> ,time (trec8)
flow best 1362 : path (AA WV IV OF XK DC UE QB) : time 1 
flow best 1579 : path (AA WV QB QR IV UE DC XK) : time 1 
flow best 1670 : path (AA WV QB IV QR UE DC XK) : time 1 
flow best 1727 : path (AA IV QR QB WV OF XK DC) : time 1 
flow best 1739 : path (AA IV QR QB WV LG MB AQ) : time 1 
flow best 1748 : path (AA IV QR QB WV LG TL MB) : time 1 
flow best 1755 : path (AA MB AQ TL WV QB IV QR) : time 1 
flow best 1789 : path (AA MB TL WV QB IV QR OF) : time 1  <<<<<<<<<<<<<<<<<<<<<<<<< suggests BEST ??
$16 = 17927
;; 11.273690s real time, 37.820474s run time.  32.946900s spent in GC.
scheme@(guile-user)> ,time (trec9)
flow best 1626 : path (AA AQ MB TL LG WV OF IV QR) : time 1 
flow best 1691 : path (AA MB AQ TL LG WV OF IV QR) : time 1 
$17 = 444
;; 12.288415s real time, 41.009531s run time.  35.690303s spent in GC.
scheme@(guile-user)> ,time (trec10)
$18 = 0
;; 12.335859s real time, 41.106476s run time.  35.771559s spent in GC.
scheme@(guile-user)>


! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !
! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !
! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !
! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !

flow best 1789 : path (AA MB TL WV QB IV QR OF) : time 1  <<<<<<<<<<<<<<<<<<<<<<<<< suggests BEST ??
          ^^^^
          ||||
1789 accepted as best answer !!!

! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !! SOLVED !

|#

			  

#|
scheme@(guile-user)> (f5 'AA 30 '())
$75 = ((pay AA WV 600 24) (pay AA QR 552 23) (pay AA IV 575 25) (pay AA VK 378 18) (pay AA AQ 500 25) (pay AA SB 378 21) (pay AA MB 459 27) (pay AA TL 400 25) (pay AA XK 360 24) (pay AA QB 322 23) (pay AA UE 234 26) (pay AA DC 192 24) (pay AA ZE 189 27) (pay AA OF 108 27) (pay AA LG 81 27))
scheme@(guile-user)>

|#






;; 'AA



;; illegal ......
;; (let ((c 3))
;;   (define (f a b)
;;     c))


;; illegal .....
;; (let ((c 3))
;;   (define f (lambda (a b)
;; 	      c)))


	 






#|

*********** best flow found *********** 
(MB TL WV QB QR IV)
best flow : 1757 : time remain : 2

(MB TL WV QB QR IV AA)
reversed

(AA IV QR QB WV TL MB)

(shortest-path 'AA 'IV)
(shortest-path 'IV 'QR)



|#

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t "i = ~a ~%" i )
;;     (set! i (+ i 1))))




(define confirm2  
  (lambda (xs)
    (let ((time 30)
	  (flow 0)
	  (opened '())
	  (loc (car xs)))
      (while (> time 0)

	(format #t "~%time = ~a ~%" time)
	
	;; if opening valve here then open valve here
	(when (and (not (member loc opened)) (> (flow-rate loc) 0))
	  (format #t "starting to open valve ~a at time ~a ~%" loc time)
	  (set! time (- time 1))
	  (when (> time 0)
	    (set! opened (cons loc opened))
	    (format #t "valve open on gate ~a at time ~a ~%" loc time)
	    (format #t "flow rate on gate ~a is ~a ~%" loc (flow-rate loc))
	    (format #t "contributes amount released is ~a ~%" (* time (flow-rate loc)))
	    (set! flow (+ flow (* time (flow-rate loc))))))

	;; can we go elsewhere?
	(cond
	 ((null? xs) (set! time (- time 1)))
	 ((null? (cdr xs)) (set! time (- time 1))) ; at last loc
	 (#t (let ((next (car (cdr xs))))
	       (set! xs (cdr xs))
	       (let ((walk (shortest-path loc next)))
		 (format #t "walking from ~a to ~a took ~a time units~%" loc next walk)
		 (set! time (- time walk))
		 (set! loc next)
		 #t))))

	;;(format #t "loop complete ~%")
	)
      (format #t "flow confirmed as ~a ~%" flow)
      (format #t "confirmed complete ~%"))))




(define confirm
  (lambda (xs)
    (let ((time 30)
	  (flow 0))
      (confirm2 xs))))













;; (defun greedy ()
;;   (let ((sols '()))
;;   (labels ((trec (loc time seen locs flow)
;; 	     (cond
;; 	       ((= 6 (length seen))
;; 		(format t "are we done ? ... flow ~a ~%" flow)
;; 		(setq sols (cons (list 'flow flow) sols))
;; 		)
;; 	       (t (dolist (loc2 locs)
;; 		    (cond
;; 		      ((member loc2 seen)
;; 		       (format t "have we turned off [~a] this valve already ? ~%" loc2)
;; 		       )
;; 		      (t
;; 		       (let ((walk (shortest-path loc loc2)))
;; 			 (let ((time2 (- time walk)))
;; 			   (when (> time2 0)
;; 			     (let ((extra-flow (* (flow-rate3 loc2) (- time2 1))))
;; 			       (trec loc2 (- time2 1) (cons loc2 seen) locs (+ flow extra-flow)))))))))))))
;;     (let ((start 'aa)
;; 	  (seen '())
;; 	  (flow 0)
;; 	  (time 30))
;;       (trec start time seen (who-pays) flow)))
;;     (sort sols (lambda (x y) (> (cadr x) (cadr y))))))




;; (let ((my-hash (make-hash-table :size 60)))
;;   (defun can-go(from)
;;     (let ((val (gethash from my-hash)))
;;       (cond
;; 	(val val)
;; 	(t (let ((input (problem)))
;; 	     (let ((res (cdr (cdr (assoc from input)))))
;; 	       (setf (gethash from my-hash) res)
;; 	       res)))))))
;; ;; prime can-go
;; (dolist (a *problem-sites*)
;;   (can-go a))



;; (defun who-pays()
;;   (let ((input (problem)))
;;     (let ((rs (remove-if (lambda (x) (= 0 (cadr x)))
;; 			 (sort (mapcar (lambda (x) (list (car x) (cadr x))) input)
;; 			       (lambda (x y) (> (cadr x) (cadr y)))))))
;;       (values (mapcar #'car rs) (mapcar #'cadr rs)))))

;; multiple values 






;; (defun old-flow-rate(s)
;;   (let ((res (cadr (assoc s (problem)))))
;;     res))






;; (let ((my-hash (make-hash-table)))
;;   (defun flow-rate(s)
;;     (let ((val (gethash s my-hash)))
;;       (cond
;; 	(val val)
;; 	(t (let ((res (cadr (assoc s (problem)))))
;; 	     (setf (gethash s my-hash) res)
;; 	     res))))))


;; ;; prime flow-rate
;; (dolist (a *problem-sites*)
;;   (flow-rate a))

;; (defun test-1 ()
;;   (time (dolist (a *problem-sites*)
;; 	  (flow-rate a))))

;; (defun test-2 ()
;;   (time (dolist (a *problem-sites*)
;; 	  (old-flow-rate a))))




;; ;; (defun shortest-path (from to)
;; ;;   (assert (member from '(AA BB CC DD EE FF GG HH II JJ)))
;; ;;   (assert (member to '(AA BB CC DD EE FF GG HH II JJ)))
;; ;;   (assert (not (eq from to)))  
;; ;;   ...
;; ;;   the shortest path is ...
;; ;;   ...)


;; ;;
;; ;; (defun trec2(from to seen dist)
;; ;;   (cond
;; ;;     ((eq from to)
;; ;;      (format t "here ... distance ~a ... ~a~%" dist seen)
;; ;;      dist)
;; ;;     (t (let ((reach (can-go from)))
;; ;; 	 (dolist (r reach)
;; ;; 	   (cond
;; ;; 	     ((member r seen)
;; ;; 	      (format t "seen ~a already skipping ~%" r))
;; ;; 	     (t (trec2 r to (cons r seen) (+ dist 1)))))))))



;; ;; does this need speeding up ??
;; (defun old-shortest-path(from to)
;;   (assert (member from *problem-sites*))
;;   (assert (member to *problem-sites*))
;;   (let ((best 999999999999999)
;; 	(best-path '()))
;;     (labels ((trec2 (from to seen dist)
;; 	       (cond
;; 		 ((eq from to)
;; 		  ;;(format t "here ... distance ~a ... ~a~%" dist seen)
;; 		  (when (< dist best)
;; 		    (setq best dist)
;; 		    (setq best-path seen))
;; 		  dist)
;; 		 (t (let ((reach (can-go from)))
;; 		      (dolist (r reach)
;; 			(cond
;; 			  ((member r seen)
;; 			   ;;(format t "seen ~a already skipping ~%" r)
;; 			   )
;; 			  (t (trec2 r to (cons r seen) (+ dist 1))))))))))
;;       (let ((dist 0)
;; 	    (seen (list from)))	
;; 	(trec2 from to seen dist)
;; 	;;(format t "best ~a ... ~a ...~%" best best-path)
;; 	(values best best-path)))))





;; ;; able to do this
;; (let ((my-hash (make-hash-table)))
;;   (defun shortest-path(a b)
;;     (let ((h2 (gethash a my-hash)))
;;       (cond
;; 	(h2 (let ((val (gethash b h2)))
;; 	      (cond
;; 		(val val)
;; 		(t (let ((res (shortest-path a b)))
;; 		     (setf (gethash b h2) res)
;; 		     res)))))
;; 	(t (let ((res (shortest-path a b)))
;; 	     (let ((new-h (make-hash-table)))
;; 	       (setf (gethash b new-h) res)
;; 	       (setf (gethash a my-hash) new-h))))))))

;; ;; prime shortest paths
;; (dolist (a *problem-sites*)
;;   (dolist (b *problem-sites*)
;;     (fast-path a b)))

  

  




;; #|
;; locations

;; at location loc
;; time is 0 .. 30

;; walk take n units of time
;; open valve take 1 unit of time
;; flow = 

;; |#
;; (defun greedy ()
;;   (let ((sols '()))
;;   (labels ((trec (loc time seen locs flow)
;; 	     (cond
;; 	       ((= 6 (length seen))
;; 		(format t "are we done ? ... flow ~a ~%" flow)
;; 		(setq sols (cons (list 'flow flow) sols))
;; 		)
;; 	       (t (dolist (loc2 locs)
;; 		    (cond
;; 		      ((member loc2 seen)
;; 		       (format t "have we turned off [~a] this valve already ? ~%" loc2)
;; 		       )
;; 		      (t
;; 		       (let ((walk (shortest-path loc loc2)))
;; 			 (let ((time2 (- time walk)))
;; 			   (when (> time2 0)
;; 			     (let ((extra-flow (* (flow-rate3 loc2) (- time2 1))))
;; 			       (trec loc2 (- time2 1) (cons loc2 seen) locs (+ flow extra-flow)))))))))))))
;;     (let ((start 'aa)
;; 	  (seen '())
;; 	  (flow 0)
;; 	  (time 30))
;;       (trec start time seen (who-pays) flow)))
;;     (sort sols (lambda (x y) (> (cadr x) (cadr y))))))


	  


;; (defun check ()
;;   (let ((locs (let ((xs (example)))
;; 		(mapcar (lambda (x) (car x) ) xs))))
;;     (dolist (loc locs)
;;       (dolist (loc2 locs)
;; 	(when (not (eq loc loc2))
;; 	  (multiple-value-bind (b p) (shortest-path loc loc2)
;; 	    (format t "~a -> ~a : val ~a : path ~a ~%" loc loc2 b p)))))))

