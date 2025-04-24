
#|
decimal          snafu
        1              1
        2              2
        3             1=
        4             1-
        5             10
        6             11
        7             12
        8             2=
        9             2-
       10             20
       15            1=0
       20            1-0
     2022         1=11-2
    12345        1-0---0
314159265  1121-1110-1=0

convert2 :: String -> Integer -> Integer -> Integer
convert2 [] p s = s
convert2 ('2' : xs) p s = convert2 xs (p * 5) (s + (p * 2))
convert2 ('1' : xs) p s = convert2 xs (p * 5) (s + (p * 1))
convert2 ('0' : xs) p s = convert2 xs (p * 5) (s + (p * 0))
convert2 ('-' : xs) p s = convert2 xs (p * 5) (s + (p * (-1)))
convert2 ('=' : xs) p s = convert2 xs (p * 5) (s + (p * (-2)))
--
snafu :: String -> Integer
snafu x = convert2 (reverse x) 1 0

check = map snafu [ "1","2", "1=", "1-", "10", "11", "12", "2=", "2-", "20", "1=0", "1-0", "1=11-2", "1-0---0", "1121-1110-1=0"] 

check2 = map snafu ["1=-0-2","12111","2=0=","21","2=01","111","20012","112","1=-1=","1-12","12","1=","122"]
expect2 = [ 1747 , 906 ,198 , 11,201, 31,1257,32, 353,107, 7, 3, 37]

counting in snafu

=
-
0
1
2 overflows to next unit and begins at = again

best-str STR [2-=102--02-2111=0=02] with difference 1410365
hopefully this is less than guess

|#

(import (chicken pretty-print)) ;; pretty print
(import (chicken format)) ;; format
(import srfi-1) ;; lists
(import srfi-69) ;; hash
;; macros
(import bindings) ;; bind
(import (chicken process-context))
;;(change-directory "day25/chicken")
(import (chicken base)) ;; assert

(import (chicken random))

(define guess 32969743607087)

(define (snafu str)
  (define (helper xs prod sum)
    (cond
     ((null? xs) sum)
     (#t (let ((ch (car xs))
	       (n 0))
	   (cond
	    ((char=? ch #\=) (set! n -2))
	    ((char=? ch #\-) (set! n -1))
	    ((char=? ch #\0) (set! n 0))
	    ((char=? ch #\1) (set! n 1))
	    ((char=? ch #\2) (set! n 2))
	    (#t (error "snafu character not recognised ~a ~%" ch)))
	    
	   (helper (cdr xs) (* prod 5) (+ sum (* n prod)))))))
  (cond
   ((null? str) '())
   (#t (let ((prod 1)(sum 0))
	 (helper (reverse (string->list str)) prod sum)))))


(define (check)
  (assert (equal?
	   (map snafu (list "1=-0-2" "12111" "2=0=" "21" "2=01" "111" "20012" "112" "1=-1=" "1-12" "12" "1=" "122"))
	   (list 1747   906  198   11 201  31 1257 32  353 107  7  3  37))))

;; generate a random list of 20 or so snafsu
(define (gen lim)
  (define (helper n)
    (cond
     ((= n 0) '())
     (#t (let ((n2 (pseudo-random-integer 5))
	       (ch #\#))
	   (cond
	    ((= n2 0) (set! ch #\=))
	    ((= n2 1) (set! ch #\-))
	    ((= n2 2) (set! ch #\0))
	    ((= n2 3) (set! ch #\1))
	    ((= n2 4) (set! ch #\2))
	    (#t (error "generate pseudo bad integer")))
	   (cons ch (helper (- n 1)))))))
  (apply string (helper lim)))

(define (hunt lim)
  (let ((best #f)
	(best-str #f)
	(best-diff #f))
    (define (hunter lim)
      (let* ((str (gen lim))
	     (val (snafu str))
	     (diff (abs (- guess val))))
	(cond
	 ((or (not best-diff) (< diff best-diff))
	  (set! best-diff diff)
	  (set! best-str str)
	  (format #t "best-str ~a ~a ~%" best-str best-diff)
	  (optimize str)
	  ))	
	(hunter lim)))
    (hunter lim)))


#|
#;949> (hunt 20)
best-str =----002=-=11020011= 75877081022684 
best-str 110-2-1210-201=-001- 10177459613958 
best-str 2000100-==2012-012=2 5207389311830 
best-str 2=10-22=20---12-22-2 1705488194915 
best-str 2--=-12110=10-2-=-== 272733335751 
best-str 2-=2-=1011-=11=-1011 100380698669 
best-str 2-=1==110001=2=0=200 82536316662 
best-str 2-=100---12-01=1020- 12259612663 
best-str 2-=102020=1-1210210= 1918224436 
best-str 2-=102-=0-01-12-1-21 270616976 
best-str 2-=102--1=2=1111--11 15474644 
best-str 2-=102--0010201=1--= 14933244 
best-str 2-=102--020=-==001-0 1589808 
best-str 2-=102--02-2111=0=02 1410365 
|#

;; optimize snafu string str
(define (optimize str)
  (define best-diff #f)
  (define best-str #f)
  (define (vector->string v)
    (list->string (vector->list v)))
  (define (helper)
    ;; record best-diff
    (set! best-str str)
    (set! best-diff (abs (- guess (snafu str))))
    (let ((len (string-length str))
	  (vec (list->vector (string->list str))))
      ;;(let loop ((i 0))
      (let ((i (pseudo-random-integer len)))
	;; i pseudo random index into vector/string
	
	;;try = replace each character as we go if we get a better version use that
	;; and keep
	(let ((ch* (vector-ref vec i)))
	  ;;(format #t "optimizing for index ~a ~% "i )
	  
	(map (lambda (ch)
	       (vector-set! vec i ch)
	       (let* ((str2 (vector->string vec))
		      (val (snafu str2))
		      (diff (abs (- guess val))))
		 (when (or (not best-diff) (< diff best-diff))		   
		   (set! best-diff diff)
		   (set! best-str str2)
		   ;;(format #t "best-str ~a ~a~%" best-str best-diff)
		   (set! str str2)
		   (set! vec (list->vector (string->list str)))
		   )))
	     '(#\= #\- #\0 #\1 #\2))
	
	(helper)))))
  
	;; (when (< i (- len 1))
	;;   (loop (+ i 1)))))))

  (helper)
  (list best-str best-diff))



(define (test)
  (optimize "2-=102--02-2111=0=02"))

(define (sanity)
  (optimize  "22222222222222222222"))

;; "2-=102--02-2111=0=02"

(define (sanity2)
  (optimize  "2222222222222222222"))
      

;;(define best-str "2-=102--02-2111=0=02")
;;(define best-str "2-=102--02-========1")
;;(define best-str "2-=102--02---------2")
;;(define best-str "2-=102--02---======2")
;;(define best-str "2-=102--02--=1=22222")
;;(define best-str "2-=102--02--=1-10000")
;;(define best-str "2-=102--02--=1-12211")
;;(define best-str "2-=102--02--=1-11222")
(define best-str "2-=102--02--=1-12=22") ;; <<< is this it ?? ACCEPTED !

(define best (snafu best-str))
(define best-diff (abs (- guess best)))

;; take best-str and change
(define (g s)
  (let* ((val (snafu s))
	 (diff (abs (- guess val))))
    (cond
     ((< diff best-diff)
      (set! best-str s)
      (set! best val)
      (set! best-diff diff)
      (format #t "~a ~a : better !!~%" best-str best-diff))
     ((and (string=? s best-str) (= diff best-diff))
      (format #t "~a ~a : same string !!~%" best-str best-diff))
     ((= diff best-diff)
      (format #t "~a ~a : same but different string : ~a ~a !!~%" s diff best-str best-diff))
     (#t
      (format #t "~a ~a : worse !! : ~a ~a : ~a~%" s diff best-str best-diff (< val guess))))))



#|
do we have a way to chop the decimal required number up so to then
piece together a solution

think about moving everything up or down
 
4 4 4 4 4 4 4   2  2  2  2  2  2  2 2 
3 3 3 3 3 3 3   1  1  1  1  1  1  1 1
2 2 2 2 2 2 2   0  0  0  0  0  0  0 0 
1 1 1 1 1 1 1  -1 -1 -1 -1 -1 -1 -1 -1
0 0 0 0 0 0 0  -2 -2 -2 -2 -2 -2 -2 -2


LSB least significant bit POS=0

if = at LSB then -2 (expt 5 0) * -2
if - at LSB then -1 (expt 5 0) * -1
if 0 at LSB then 0 (expt 5 0) * 0
if 1 at LSB then 1 (expt 5 0) * 1
if 2 at LSB then 2 (expt 5 0) * 2 

at next BIT position work from lowest bit position POS=1 

target 32969743607087
make 7 <= snafu "12"
make 87 ??

pre-compute e0 ... e30 say
e0 = (expt 5 0)
e1 = (expt 5 1)
...
e30 = (expt 5 30)

can either have -2 * E , -1 * E , 0 * E ie ignore, 1 * E , 2 * E for each E{n}
sum all our chosen E{n} ... can we make 87 ?
50 + 25 + 5 + 2
1 1 1 2 

(E 0) => (define e0 (expt 5 0))

|#

;;(define-syntax E
(define-syntax E
  (er-macro-transformer
   (lambda (expr rename compare?)
     (let* ((n (car (cdr expr)))
	    (sym (string->symbol (string-append "e" (number->string n))))
	    (%define (rename 'define))
	    (%expt (rename 'expt)))
       ;;`(,%define ,sym (,%expt 5 0))
       `(define ,sym (,%expt 5 ,n))
       ))))

;; (cons 'begin (map (lambda (n) `(E ,n)) (iota 23)))

(begin (E 0) (E 1) (E 2) (E 3) (E 4) (E 5) (E 6) (E 7) (E 8) (E 9) (E 10) (E 11) (E 12) (E 13) (E 14) (E 15) (E 16) (E 17) (E 18) (E 19) (E 20) (E 21) (E 22))

#|

then case of if we want to -2 -1 0 1 2

F -2 -2 -2 -2
..
F 2 2 2 2

can i make 87 ??
compute the snafu at same time and make snafu string also - then check snafu verified

|#
;; (rec 3 0 '())   recursive want 3 values considered

(define (rec2 n s letters)
  (cond
   ((< n 0) (let* ((str (apply string (reverse letters)))
		   (sum1 s)
		   (sum2 (snafu str)))
	      (format #t "STR : [~a] : " str)
	      (format #t "LETTERS : [~a] : " letters)	      
	      (cond
	       ((= sum1 sum2) (format #t "~a => ~a agree~%" str sum1))
	       (#t (format #t "~a => ~a : expected ~a : disagree !!~%" str sum1 sum2)))))
   ((> n 4) "please (rec 3 0 '()) or (rec 2 0 '()) or (rec 1 0 '())")
   ((= n 4)
    (rec2 (- n 1) (+ s (* 2 e4)) (cons #\2 letters))
    (rec2 (- n 1) (+ s e4) (cons #\1 letters))
    (rec2 (- n 1) s (cons #\0 letters))
    (rec2 (- n 1) (- s e4) (cons #\- letters))
    (rec2 (- n 1) (- s (* 2 e4)) (cons #\= letters)))
   ((= n 3)
    (rec2 (- n 1) (+ s (* 2 e3)) (cons #\2 letters))
    (rec2 (- n 1) (+ s e3) (cons #\1 letters))
    (rec2 (- n 1) s (cons #\0 letters))
    (rec2 (- n 1) (- s e3) (cons #\- letters))
    (rec2 (- n 1) (- s (* 2 e3)) (cons #\= letters)))
   ((= n 2)
    (rec2 (- n 1) (+ s (* 2 e2)) (cons #\2 letters))
    (rec2 (- n 1) (+ s e2) (cons #\1 letters))
    (rec2 (- n 1) s (cons #\0 letters))
    (rec2 (- n 1) (- s e2) (cons #\- letters))
    (rec2 (- n 1) (- s (* 2 e2)) (cons #\= letters)))
   ((= n 1)
    (rec2 (- n 1) (+ s (* 2 e1)) (cons #\2 letters))
    (rec2 (- n 1) (+ s e1) (cons #\1 letters))
    (rec2 (- n 1) s (cons #\0 letters))
    (rec2 (- n 1) (- s e1) (cons #\- letters))
    (rec2 (- n 1) (- s (* 2 e1)) (cons #\= letters)))
   ((= n 0)
    (rec2 (- n 1) (+ s (* 2 e0)) (cons #\2 letters))
    (rec2 (- n 1) (+ s e0) (cons #\1 letters))
    (rec2 (- n 1) s (cons #\0 letters))
    (rec2 (- n 1) (- s e0) (cons #\- letters))
    (rec2 (- n 1) (- s (* 2 e0)) (cons #\= letters)))
   ))






   
   
   


   
    
    
    


   
  
  


;; (define (brute)
;;   (
    

#|
(define (f n)
  (cond
   ((or (< n 1)(> n 20)) "out-of-range 1 to 20 please")
   (#t (let* ((i (- n 1))
	      (s1 
|#

#|

STR : [22222] : LETTERS : [(2 2 2 2 2)] : 22222 => 1562 agree
STR : [22221] : LETTERS : [(1 2 2 2 2)] : 22221 => 1561 agree
STR : [22220] : LETTERS : [(0 2 2 2 2)] : 22220 => 1560 agree
STR : [2222-] : LETTERS : [(- 2 2 2 2)] : 2222- => 1559 agree
STR : [2222=] : LETTERS : [(= 2 2 2 2)] : 2222= => 1558 agree
STR : [22212] : LETTERS : [(2 1 2 2 2)] : 22212 => 1557 agree
STR : [22211] : LETTERS : [(1 1 2 2 2)] : 22211 => 1556 agree
STR : [22210] : LETTERS : [(0 1 2 2 2)] : 22210 => 1555 agree
STR : [2221-] : LETTERS : [(- 1 2 2 2)] : 2221- => 1554 agree
STR : [2221=] : LETTERS : [(= 1 2 2 2)] : 2221= => 1553 agree
STR : [22202] : LETTERS : [(2 0 2 2 2)] : 22202 => 1552 agree
STR : [22201] : LETTERS : [(1 0 2 2 2)] : 22201 => 1551 agree
STR : [22200] : LETTERS : [(0 0 2 2 2)] : 22200 => 1550 agree
STR : [2220-] : LETTERS : [(- 0 2 2 2)] : 2220- => 1549 agree
STR : [2220=] : LETTERS : [(= 0 2 2 2)] : 2220= => 1548 agree
STR : [222-2] : LETTERS : [(2 - 2 2 2)] : 222-2 => 1547 agree
STR : [222-1] : LETTERS : [(1 - 2 2 2)] : 222-1 => 1546 agree
STR : [222-0] : LETTERS : [(0 - 2 2 2)] : 222-0 => 1545 agree
STR : [222--] : LETTERS : [(- - 2 2 2)] : 222-- => 1544 agree
STR : [222-=] : LETTERS : [(= - 2 2 2)] : 222-= => 1543 agree
STR : [222=2] : LETTERS : [(2 = 2 2 2)] : 222=2 => 1542 agree
STR : [222=1] : LETTERS : [(1 = 2 2 2)] : 222=1 => 1541 agree
STR : [222=0] : LETTERS : [(0 = 2 2 2)] : 222=0 => 1540 agree
STR : [222=-] : LETTERS : [(- = 2 2 2)] : 222=- => 1539 agree
STR : [222==] : LETTERS : [(= = 2 2 2)] : 222== => 1538 agree
STR : [22122] : LETTERS : [(2 2 1 2 2)] : 22122 => 1537 agree
STR : [22121] : LETTERS : [(1 2 1 2 2)] : 22121 => 1536 agree
STR : [22120] : LETTERS : [(0 2 1 2 2)] : 22120 => 1535 agree
STR : [2212-] : LETTERS : [(- 2 1 2 2)] : 2212- => 1534 agree
STR : [2212=] : LETTERS : [(= 2 1 2 2)] : 2212= => 1533 agree
STR : [22112] : LETTERS : [(2 1 1 2 2)] : 22112 => 1532 agree
STR : [22111] : LETTERS : [(1 1 1 2 2)] : 22111 => 1531 agree
STR : [22110] : LETTERS : [(0 1 1 2 2)] : 22110 => 1530 agree
STR : [2211-] : LETTERS : [(- 1 1 2 2)] : 2211- => 1529 agree
STR : [2211=] : LETTERS : [(= 1 1 2 2)] : 2211= => 1528 agree
STR : [22102] : LETTERS : [(2 0 1 2 2)] : 22102 => 1527 agree
STR : [22101] : LETTERS : [(1 0 1 2 2)] : 22101 => 1526 agree
STR : [22100] : LETTERS : [(0 0 1 2 2)] : 22100 => 1525 agree
STR : [2210-] : LETTERS : [(- 0 1 2 2)] : 2210- => 1524 agree
STR : [2210=] : LETTERS : [(= 0 1 2 2)] : 2210= => 1523 agree
STR : [221-2] : LETTERS : [(2 - 1 2 2)] : 221-2 => 1522 agree
STR : [221-1] : LETTERS : [(1 - 1 2 2)] : 221-1 => 1521 agree
STR : [221-0] : LETTERS : [(0 - 1 2 2)] : 221-0 => 1520 agree
STR : [221--] : LETTERS : [(- - 1 2 2)] : 221-- => 1519 agree
STR : [221-=] : LETTERS : [(= - 1 2 2)] : 221-= => 1518 agree
STR : [221=2] : LETTERS : [(2 = 1 2 2)] : 221=2 => 1517 agree
STR : [221=1] : LETTERS : [(1 = 1 2 2)] : 221=1 => 1516 agree
STR : [221=0] : LETTERS : [(0 = 1 2 2)] : 221=0 => 1515 agree
STR : [221=-] : LETTERS : [(- = 1 2 2)] : 221=- => 1514 agree
STR : [221==] : LETTERS : [(= = 1 2 2)] : 221== => 1513 agree
STR : [22022] : LETTERS : [(2 2 0 2 2)] : 22022 => 1512 agree
STR : [22021] : LETTERS : [(1 2 0 2 2)] : 22021 => 1511 agree
STR : [22020] : LETTERS : [(0 2 0 2 2)] : 22020 => 1510 agree
STR : [2202-] : LETTERS : [(- 2 0 2 2)] : 2202- => 1509 agree
STR : [2202=] : LETTERS : [(= 2 0 2 2)] : 2202= => 1508 agree
STR : [22012] : LETTERS : [(2 1 0 2 2)] : 22012 => 1507 agree
STR : [22011] : LETTERS : [(1 1 0 2 2)] : 22011 => 1506 agree
STR : [22010] : LETTERS : [(0 1 0 2 2)] : 22010 => 1505 agree
STR : [2201-] : LETTERS : [(- 1 0 2 2)] : 2201- => 1504 agree
STR : [2201=] : LETTERS : [(= 1 0 2 2)] : 2201= => 1503 agree
STR : [22002] : LETTERS : [(2 0 0 2 2)] : 22002 => 1502 agree
STR : [22001] : LETTERS : [(1 0 0 2 2)] : 22001 => 1501 agree
STR : [22000] : LETTERS : [(0 0 0 2 2)] : 22000 => 1500 agree
STR : [2200-] : LETTERS : [(- 0 0 2 2)] : 2200- => 1499 agree
STR : [2200=] : LETTERS : [(= 0 0 2 2)] : 2200= => 1498 agree
STR : [220-2] : LETTERS : [(2 - 0 2 2)] : 220-2 => 1497 agree
STR : [220-1] : LETTERS : [(1 - 0 2 2)] : 220-1 => 1496 agree
STR : [220-0] : LETTERS : [(0 - 0 2 2)] : 220-0 => 1495 agree
STR : [220--] : LETTERS : [(- - 0 2 2)] : 220-- => 1494 agree
STR : [220-=] : LETTERS : [(= - 0 2 2)] : 220-= => 1493 agree
STR : [220=2] : LETTERS : [(2 = 0 2 2)] : 220=2 => 1492 agree
STR : [220=1] : LETTERS : [(1 = 0 2 2)] : 220=1 => 1491 agree
STR : [220=0] : LETTERS : [(0 = 0 2 2)] : 220=0 => 1490 agree
STR : [220=-] : LETTERS : [(- = 0 2 2)] : 220=- => 1489 agree
STR : [220==] : LETTERS : [(= = 0 2 2)] : 220== => 1488 agree
STR : [22-22] : LETTERS : [(2 2 - 2 2)] : 22-22 => 1487 agree
STR : [22-21] : LETTERS : [(1 2 - 2 2)] : 22-21 => 1486 agree
STR : [22-20] : LETTERS : [(0 2 - 2 2)] : 22-20 => 1485 agree
STR : [22-2-] : LETTERS : [(- 2 - 2 2)] : 22-2- => 1484 agree
STR : [22-2=] : LETTERS : [(= 2 - 2 2)] : 22-2= => 1483 agree
STR : [22-12] : LETTERS : [(2 1 - 2 2)] : 22-12 => 1482 agree
STR : [22-11] : LETTERS : [(1 1 - 2 2)] : 22-11 => 1481 agree
STR : [22-10] : LETTERS : [(0 1 - 2 2)] : 22-10 => 1480 agree
STR : [22-1-] : LETTERS : [(- 1 - 2 2)] : 22-1- => 1479 agree
STR : [22-1=] : LETTERS : [(= 1 - 2 2)] : 22-1= => 1478 agree
STR : [22-02] : LETTERS : [(2 0 - 2 2)] : 22-02 => 1477 agree
STR : [22-01] : LETTERS : [(1 0 - 2 2)] : 22-01 => 1476 agree
STR : [22-00] : LETTERS : [(0 0 - 2 2)] : 22-00 => 1475 agree
STR : [22-0-] : LETTERS : [(- 0 - 2 2)] : 22-0- => 1474 agree
STR : [22-0=] : LETTERS : [(= 0 - 2 2)] : 22-0= => 1473 agree
STR : [22--2] : LETTERS : [(2 - - 2 2)] : 22--2 => 1472 agree
STR : [22--1] : LETTERS : [(1 - - 2 2)] : 22--1 => 1471 agree
STR : [22--0] : LETTERS : [(0 - - 2 2)] : 22--0 => 1470 agree
STR : [22---] : LETTERS : [(- - - 2 2)] : 22--- => 1469 agree
STR : [22--=] : LETTERS : [(= - - 2 2)] : 22--= => 1468 agree
STR : [22-=2] : LETTERS : [(2 = - 2 2)] : 22-=2 => 1467 agree
STR : [22-=1] : LETTERS : [(1 = - 2 2)] : 22-=1 => 1466 agree
STR : [22-=0] : LETTERS : [(0 = - 2 2)] : 22-=0 => 1465 agree
STR : [22-=-] : LETTERS : [(- = - 2 2)] : 22-=- => 1464 agree
STR : [22-==] : LETTERS : [(= = - 2 2)] : 22-== => 1463 agree
STR : [22=22] : LETTERS : [(2 2 = 2 2)] : 22=22 => 1462 agree
STR : [22=21] : LETTERS : [(1 2 = 2 2)] : 22=21 => 1461 agree
STR : [22=20] : LETTERS : [(0 2 = 2 2)] : 22=20 => 1460 agree
STR : [22=2-] : LETTERS : [(- 2 = 2 2)] : 22=2- => 1459 agree
STR : [22=2=] : LETTERS : [(= 2 = 2 2)] : 22=2= => 1458 agree
STR : [22=12] : LETTERS : [(2 1 = 2 2)] : 22=12 => 1457 agree
STR : [22=11] : LETTERS : [(1 1 = 2 2)] : 22=11 => 1456 agree
STR : [22=10] : LETTERS : [(0 1 = 2 2)] : 22=10 => 1455 agree
STR : [22=1-] : LETTERS : [(- 1 = 2 2)] : 22=1- => 1454 agree
STR : [22=1=] : LETTERS : [(= 1 = 2 2)] : 22=1= => 1453 agree
STR : [22=02] : LETTERS : [(2 0 = 2 2)] : 22=02 => 1452 agree
STR : [22=01] : LETTERS : [(1 0 = 2 2)] : 22=01 => 1451 agree
STR : [22=00] : LETTERS : [(0 0 = 2 2)] : 22=00 => 1450 agree
STR : [22=0-] : LETTERS : [(- 0 = 2 2)] : 22=0- => 1449 agree
STR : [22=0=] : LETTERS : [(= 0 = 2 2)] : 22=0= => 1448 agree
STR : [22=-2] : LETTERS : [(2 - = 2 2)] : 22=-2 => 1447 agree
STR : [22=-1] : LETTERS : [(1 - = 2 2)] : 22=-1 => 1446 agree
STR : [22=-0] : LETTERS : [(0 - = 2 2)] : 22=-0 => 1445 agree
STR : [22=--] : LETTERS : [(- - = 2 2)] : 22=-- => 1444 agree
STR : [22=-=] : LETTERS : [(= - = 2 2)] : 22=-= => 1443 agree
STR : [22==2] : LETTERS : [(2 = = 2 2)] : 22==2 => 1442 agree
STR : [22==1] : LETTERS : [(1 = = 2 2)] : 22==1 => 1441 agree
STR : [22==0] : LETTERS : [(0 = = 2 2)] : 22==0 => 1440 agree
STR : [22==-] : LETTERS : [(- = = 2 2)] : 22==- => 1439 agree
STR : [22===] : LETTERS : [(= = = 2 2)] : 22=== => 1438 agree
STR : [21222] : LETTERS : [(2 2 2 1 2)] : 21222 => 1437 agree
STR : [21221] : LETTERS : [(1 2 2 1 2)] : 21221 => 1436 agree
STR : [21220] : LETTERS : [(0 2 2 1 2)] : 21220 => 1435 agree
STR : [2122-] : LETTERS : [(- 2 2 1 2)] : 2122- => 1434 agree
STR : [2122=] : LETTERS : [(= 2 2 1 2)] : 2122= => 1433 agree
STR : [21212] : LETTERS : [(2 1 2 1 2)] : 21212 => 1432 agree
STR : [21211] : LETTERS : [(1 1 2 1 2)] : 21211 => 1431 agree
STR : [21210] : LETTERS : [(0 1 2 1 2)] : 21210 => 1430 agree
STR : [2121-] : LETTERS : [(- 1 2 1 2)] : 2121- => 1429 agree
STR : [2121=] : LETTERS : [(= 1 2 1 2)] : 2121= => 1428 agree
STR : [21202] : LETTERS : [(2 0 2 1 2)] : 21202 => 1427 agree
STR : [21201] : LETTERS : [(1 0 2 1 2)] : 21201 => 1426 agree
STR : [21200] : LETTERS : [(0 0 2 1 2)] : 21200 => 1425 agree
STR : [2120-] : LETTERS : [(- 0 2 1 2)] : 2120- => 1424 agree
STR : [2120=] : LETTERS : [(= 0 2 1 2)] : 2120= => 1423 agree
STR : [212-2] : LETTERS : [(2 - 2 1 2)] : 212-2 => 1422 agree
STR : [212-1] : LETTERS : [(1 - 2 1 2)] : 212-1 => 1421 agree
STR : [212-0] : LETTERS : [(0 - 2 1 2)] : 212-0 => 1420 agree
STR : [212--] : LETTERS : [(- - 2 1 2)] : 212-- => 1419 agree
STR : [212-=] : LETTERS : [(= - 2 1 2)] : 212-= => 1418 agree
STR : [212=2] : LETTERS : [(2 = 2 1 2)] : 212=2 => 1417 agree
STR : [212=1] : LETTERS : [(1 = 2 1 2)] : 212=1 => 1416 agree
STR : [212=0] : LETTERS : [(0 = 2 1 2)] : 212=0 => 1415 agree
STR : [212=-] : LETTERS : [(- = 2 1 2)] : 212=- => 1414 agree
STR : [212==] : LETTERS : [(= = 2 1 2)] : 212== => 1413 agree
STR : [21122] : LETTERS : [(2 2 1 1 2)] : 21122 => 1412 agree
STR : [21121] : LETTERS : [(1 2 1 1 2)] : 21121 => 1411 agree
STR : [21120] : LETTERS : [(0 2 1 1 2)] : 21120 => 1410 agree
STR : [2112-] : LETTERS : [(- 2 1 1 2)] : 2112- => 1409 agree
STR : [2112=] : LETTERS : [(= 2 1 1 2)] : 2112= => 1408 agree
STR : [21112] : LETTERS : [(2 1 1 1 2)] : 21112 => 1407 agree
STR : [21111] : LETTERS : [(1 1 1 1 2)] : 21111 => 1406 agree
STR : [21110] : LETTERS : [(0 1 1 1 2)] : 21110 => 1405 agree
STR : [2111-] : LETTERS : [(- 1 1 1 2)] : 2111- => 1404 agree
STR : [2111=] : LETTERS : [(= 1 1 1 2)] : 2111= => 1403 agree
STR : [21102] : LETTERS : [(2 0 1 1 2)] : 21102 => 1402 agree
STR : [21101] : LETTERS : [(1 0 1 1 2)] : 21101 => 1401 agree
STR : [21100] : LETTERS : [(0 0 1 1 2)] : 21100 => 1400 agree
STR : [2110-] : LETTERS : [(- 0 1 1 2)] : 2110- => 1399 agree
STR : [2110=] : LETTERS : [(= 0 1 1 2)] : 2110= => 1398 agree
STR : [211-2] : LETTERS : [(2 - 1 1 2)] : 211-2 => 1397 agree
STR : [211-1] : LETTERS : [(1 - 1 1 2)] : 211-1 => 1396 agree
STR : [211-0] : LETTERS : [(0 - 1 1 2)] : 211-0 => 1395 agree
STR : [211--] : LETTERS : [(- - 1 1 2)] : 211-- => 1394 agree
STR : [211-=] : LETTERS : [(= - 1 1 2)] : 211-= => 1393 agree
STR : [211=2] : LETTERS : [(2 = 1 1 2)] : 211=2 => 1392 agree
STR : [211=1] : LETTERS : [(1 = 1 1 2)] : 211=1 => 1391 agree
STR : [211=0] : LETTERS : [(0 = 1 1 2)] : 211=0 => 1390 agree
STR : [211=-] : LETTERS : [(- = 1 1 2)] : 211=- => 1389 agree
STR : [211==] : LETTERS : [(= = 1 1 2)] : 211== => 1388 agree
STR : [21022] : LETTERS : [(2 2 0 1 2)] : 21022 => 1387 agree
STR : [21021] : LETTERS : [(1 2 0 1 2)] : 21021 => 1386 agree
STR : [21020] : LETTERS : [(0 2 0 1 2)] : 21020 => 1385 agree
STR : [2102-] : LETTERS : [(- 2 0 1 2)] : 2102- => 1384 agree
STR : [2102=] : LETTERS : [(= 2 0 1 2)] : 2102= => 1383 agree
STR : [21012] : LETTERS : [(2 1 0 1 2)] : 21012 => 1382 agree
STR : [21011] : LETTERS : [(1 1 0 1 2)] : 21011 => 1381 agree
STR : [21010] : LETTERS : [(0 1 0 1 2)] : 21010 => 1380 agree
STR : [2101-] : LETTERS : [(- 1 0 1 2)] : 2101- => 1379 agree
STR : [2101=] : LETTERS : [(= 1 0 1 2)] : 2101= => 1378 agree
STR : [21002] : LETTERS : [(2 0 0 1 2)] : 21002 => 1377 agree
STR : [21001] : LETTERS : [(1 0 0 1 2)] : 21001 => 1376 agree
STR : [21000] : LETTERS : [(0 0 0 1 2)] : 21000 => 1375 agree
STR : [2100-] : LETTERS : [(- 0 0 1 2)] : 2100- => 1374 agree
STR : [2100=] : LETTERS : [(= 0 0 1 2)] : 2100= => 1373 agree
STR : [210-2] : LETTERS : [(2 - 0 1 2)] : 210-2 => 1372 agree
STR : [210-1] : LETTERS : [(1 - 0 1 2)] : 210-1 => 1371 agree
STR : [210-0] : LETTERS : [(0 - 0 1 2)] : 210-0 => 1370 agree
STR : [210--] : LETTERS : [(- - 0 1 2)] : 210-- => 1369 agree
STR : [210-=] : LETTERS : [(= - 0 1 2)] : 210-= => 1368 agree
STR : [210=2] : LETTERS : [(2 = 0 1 2)] : 210=2 => 1367 agree
STR : [210=1] : LETTERS : [(1 = 0 1 2)] : 210=1 => 1366 agree
STR : [210=0] : LETTERS : [(0 = 0 1 2)] : 210=0 => 1365 agree
STR : [210=-] : LETTERS : [(- = 0 1 2)] : 210=- => 1364 agree
STR : [210==] : LETTERS : [(= = 0 1 2)] : 210== => 1363 agree
STR : [21-22] : LETTERS : [(2 2 - 1 2)] : 21-22 => 1362 agree
STR : [21-21] : LETTERS : [(1 2 - 1 2)] : 21-21 => 1361 agree
STR : [21-20] : LETTERS : [(0 2 - 1 2)] : 21-20 => 1360 agree
STR : [21-2-] : LETTERS : [(- 2 - 1 2)] : 21-2- => 1359 agree
STR : [21-2=] : LETTERS : [(= 2 - 1 2)] : 21-2= => 1358 agree
STR : [21-12] : LETTERS : [(2 1 - 1 2)] : 21-12 => 1357 agree
STR : [21-11] : LETTERS : [(1 1 - 1 2)] : 21-11 => 1356 agree
STR : [21-10] : LETTERS : [(0 1 - 1 2)] : 21-10 => 1355 agree
STR : [21-1-] : LETTERS : [(- 1 - 1 2)] : 21-1- => 1354 agree
STR : [21-1=] : LETTERS : [(= 1 - 1 2)] : 21-1= => 1353 agree
STR : [21-02] : LETTERS : [(2 0 - 1 2)] : 21-02 => 1352 agree
STR : [21-01] : LETTERS : [(1 0 - 1 2)] : 21-01 => 1351 agree
STR : [21-00] : LETTERS : [(0 0 - 1 2)] : 21-00 => 1350 agree
STR : [21-0-] : LETTERS : [(- 0 - 1 2)] : 21-0- => 1349 agree
STR : [21-0=] : LETTERS : [(= 0 - 1 2)] : 21-0= => 1348 agree
STR : [21--2] : LETTERS : [(2 - - 1 2)] : 21--2 => 1347 agree
STR : [21--1] : LETTERS : [(1 - - 1 2)] : 21--1 => 1346 agree
STR : [21--0] : LETTERS : [(0 - - 1 2)] : 21--0 => 1345 agree
STR : [21---] : LETTERS : [(- - - 1 2)] : 21--- => 1344 agree
STR : [21--=] : LETTERS : [(= - - 1 2)] : 21--= => 1343 agree
STR : [21-=2] : LETTERS : [(2 = - 1 2)] : 21-=2 => 1342 agree
STR : [21-=1] : LETTERS : [(1 = - 1 2)] : 21-=1 => 1341 agree
STR : [21-=0] : LETTERS : [(0 = - 1 2)] : 21-=0 => 1340 agree
STR : [21-=-] : LETTERS : [(- = - 1 2)] : 21-=- => 1339 agree
STR : [21-==] : LETTERS : [(= = - 1 2)] : 21-== => 1338 agree
STR : [21=22] : LETTERS : [(2 2 = 1 2)] : 21=22 => 1337 agree
STR : [21=21] : LETTERS : [(1 2 = 1 2)] : 21=21 => 1336 agree
STR : [21=20] : LETTERS : [(0 2 = 1 2)] : 21=20 => 1335 agree
STR : [21=2-] : LETTERS : [(- 2 = 1 2)] : 21=2- => 1334 agree
STR : [21=2=] : LETTERS : [(= 2 = 1 2)] : 21=2= => 1333 agree
STR : [21=12] : LETTERS : [(2 1 = 1 2)] : 21=12 => 1332 agree
STR : [21=11] : LETTERS : [(1 1 = 1 2)] : 21=11 => 1331 agree
STR : [21=10] : LETTERS : [(0 1 = 1 2)] : 21=10 => 1330 agree
STR : [21=1-] : LETTERS : [(- 1 = 1 2)] : 21=1- => 1329 agree
STR : [21=1=] : LETTERS : [(= 1 = 1 2)] : 21=1= => 1328 agree
STR : [21=02] : LETTERS : [(2 0 = 1 2)] : 21=02 => 1327 agree
STR : [21=01] : LETTERS : [(1 0 = 1 2)] : 21=01 => 1326 agree
STR : [21=00] : LETTERS : [(0 0 = 1 2)] : 21=00 => 1325 agree
STR : [21=0-] : LETTERS : [(- 0 = 1 2)] : 21=0- => 1324 agree
STR : [21=0=] : LETTERS : [(= 0 = 1 2)] : 21=0= => 1323 agree
STR : [21=-2] : LETTERS : [(2 - = 1 2)] : 21=-2 => 1322 agree
STR : [21=-1] : LETTERS : [(1 - = 1 2)] : 21=-1 => 1321 agree
STR : [21=-0] : LETTERS : [(0 - = 1 2)] : 21=-0 => 1320 agree
STR : [21=--] : LETTERS : [(- - = 1 2)] : 21=-- => 1319 agree
STR : [21=-=] : LETTERS : [(= - = 1 2)] : 21=-= => 1318 agree
STR : [21==2] : LETTERS : [(2 = = 1 2)] : 21==2 => 1317 agree
STR : [21==1] : LETTERS : [(1 = = 1 2)] : 21==1 => 1316 agree
STR : [21==0] : LETTERS : [(0 = = 1 2)] : 21==0 => 1315 agree
STR : [21==-] : LETTERS : [(- = = 1 2)] : 21==- => 1314 agree
STR : [21===] : LETTERS : [(= = = 1 2)] : 21=== => 1313 agree
STR : [20222] : LETTERS : [(2 2 2 0 2)] : 20222 => 1312 agree
STR : [20221] : LETTERS : [(1 2 2 0 2)] : 20221 => 1311 agree
STR : [20220] : LETTERS : [(0 2 2 0 2)] : 20220 => 1310 agree
STR : [2022-] : LETTERS : [(- 2 2 0 2)] : 2022- => 1309 agree
STR : [2022=] : LETTERS : [(= 2 2 0 2)] : 2022= => 1308 agree
STR : [20212] : LETTERS : [(2 1 2 0 2)] : 20212 => 1307 agree
STR : [20211] : LETTERS : [(1 1 2 0 2)] : 20211 => 1306 agree
STR : [20210] : LETTERS : [(0 1 2 0 2)] : 20210 => 1305 agree
STR : [2021-] : LETTERS : [(- 1 2 0 2)] : 2021- => 1304 agree
STR : [2021=] : LETTERS : [(= 1 2 0 2)] : 2021= => 1303 agree
STR : [20202] : LETTERS : [(2 0 2 0 2)] : 20202 => 1302 agree
STR : [20201] : LETTERS : [(1 0 2 0 2)] : 20201 => 1301 agree
STR : [20200] : LETTERS : [(0 0 2 0 2)] : 20200 => 1300 agree
STR : [2020-] : LETTERS : [(- 0 2 0 2)] : 2020- => 1299 agree
STR : [2020=] : LETTERS : [(= 0 2 0 2)] : 2020= => 1298 agree
STR : [202-2] : LETTERS : [(2 - 2 0 2)] : 202-2 => 1297 agree
STR : [202-1] : LETTERS : [(1 - 2 0 2)] : 202-1 => 1296 agree
STR : [202-0] : LETTERS : [(0 - 2 0 2)] : 202-0 => 1295 agree
STR : [202--] : LETTERS : [(- - 2 0 2)] : 202-- => 1294 agree
STR : [202-=] : LETTERS : [(= - 2 0 2)] : 202-= => 1293 agree
STR : [202=2] : LETTERS : [(2 = 2 0 2)] : 202=2 => 1292 agree
STR : [202=1] : LETTERS : [(1 = 2 0 2)] : 202=1 => 1291 agree
STR : [202=0] : LETTERS : [(0 = 2 0 2)] : 202=0 => 1290 agree
STR : [202=-] : LETTERS : [(- = 2 0 2)] : 202=- => 1289 agree
STR : [202==] : LETTERS : [(= = 2 0 2)] : 202== => 1288 agree
STR : [20122] : LETTERS : [(2 2 1 0 2)] : 20122 => 1287 agree
STR : [20121] : LETTERS : [(1 2 1 0 2)] : 20121 => 1286 agree
STR : [20120] : LETTERS : [(0 2 1 0 2)] : 20120 => 1285 agree
STR : [2012-] : LETTERS : [(- 2 1 0 2)] : 2012- => 1284 agree
STR : [2012=] : LETTERS : [(= 2 1 0 2)] : 2012= => 1283 agree
STR : [20112] : LETTERS : [(2 1 1 0 2)] : 20112 => 1282 agree
STR : [20111] : LETTERS : [(1 1 1 0 2)] : 20111 => 1281 agree
STR : [20110] : LETTERS : [(0 1 1 0 2)] : 20110 => 1280 agree
STR : [2011-] : LETTERS : [(- 1 1 0 2)] : 2011- => 1279 agree
STR : [2011=] : LETTERS : [(= 1 1 0 2)] : 2011= => 1278 agree
STR : [20102] : LETTERS : [(2 0 1 0 2)] : 20102 => 1277 agree
STR : [20101] : LETTERS : [(1 0 1 0 2)] : 20101 => 1276 agree
STR : [20100] : LETTERS : [(0 0 1 0 2)] : 20100 => 1275 agree
STR : [2010-] : LETTERS : [(- 0 1 0 2)] : 2010- => 1274 agree
STR : [2010=] : LETTERS : [(= 0 1 0 2)] : 2010= => 1273 agree
STR : [201-2] : LETTERS : [(2 - 1 0 2)] : 201-2 => 1272 agree
STR : [201-1] : LETTERS : [(1 - 1 0 2)] : 201-1 => 1271 agree
STR : [201-0] : LETTERS : [(0 - 1 0 2)] : 201-0 => 1270 agree
STR : [201--] : LETTERS : [(- - 1 0 2)] : 201-- => 1269 agree
STR : [201-=] : LETTERS : [(= - 1 0 2)] : 201-= => 1268 agree
STR : [201=2] : LETTERS : [(2 = 1 0 2)] : 201=2 => 1267 agree
STR : [201=1] : LETTERS : [(1 = 1 0 2)] : 201=1 => 1266 agree
STR : [201=0] : LETTERS : [(0 = 1 0 2)] : 201=0 => 1265 agree
STR : [201=-] : LETTERS : [(- = 1 0 2)] : 201=- => 1264 agree
STR : [201==] : LETTERS : [(= = 1 0 2)] : 201== => 1263 agree
STR : [20022] : LETTERS : [(2 2 0 0 2)] : 20022 => 1262 agree
STR : [20021] : LETTERS : [(1 2 0 0 2)] : 20021 => 1261 agree
STR : [20020] : LETTERS : [(0 2 0 0 2)] : 20020 => 1260 agree
STR : [2002-] : LETTERS : [(- 2 0 0 2)] : 2002- => 1259 agree
STR : [2002=] : LETTERS : [(= 2 0 0 2)] : 2002= => 1258 agree
STR : [20012] : LETTERS : [(2 1 0 0 2)] : 20012 => 1257 agree
STR : [20011] : LETTERS : [(1 1 0 0 2)] : 20011 => 1256 agree
STR : [20010] : LETTERS : [(0 1 0 0 2)] : 20010 => 1255 agree
STR : [2001-] : LETTERS : [(- 1 0 0 2)] : 2001- => 1254 agree
STR : [2001=] : LETTERS : [(= 1 0 0 2)] : 2001= => 1253 agree
STR : [20002] : LETTERS : [(2 0 0 0 2)] : 20002 => 1252 agree
STR : [20001] : LETTERS : [(1 0 0 0 2)] : 20001 => 1251 agree
STR : [20000] : LETTERS : [(0 0 0 0 2)] : 20000 => 1250 agree
STR : [2000-] : LETTERS : [(- 0 0 0 2)] : 2000- => 1249 agree
STR : [2000=] : LETTERS : [(= 0 0 0 2)] : 2000= => 1248 agree
STR : [200-2] : LETTERS : [(2 - 0 0 2)] : 200-2 => 1247 agree
STR : [200-1] : LETTERS : [(1 - 0 0 2)] : 200-1 => 1246 agree
STR : [200-0] : LETTERS : [(0 - 0 0 2)] : 200-0 => 1245 agree
STR : [200--] : LETTERS : [(- - 0 0 2)] : 200-- => 1244 agree
STR : [200-=] : LETTERS : [(= - 0 0 2)] : 200-= => 1243 agree
STR : [200=2] : LETTERS : [(2 = 0 0 2)] : 200=2 => 1242 agree
STR : [200=1] : LETTERS : [(1 = 0 0 2)] : 200=1 => 1241 agree
STR : [200=0] : LETTERS : [(0 = 0 0 2)] : 200=0 => 1240 agree
STR : [200=-] : LETTERS : [(- = 0 0 2)] : 200=- => 1239 agree
STR : [200==] : LETTERS : [(= = 0 0 2)] : 200== => 1238 agree
STR : [20-22] : LETTERS : [(2 2 - 0 2)] : 20-22 => 1237 agree
STR : [20-21] : LETTERS : [(1 2 - 0 2)] : 20-21 => 1236 agree
STR : [20-20] : LETTERS : [(0 2 - 0 2)] : 20-20 => 1235 agree
STR : [20-2-] : LETTERS : [(- 2 - 0 2)] : 20-2- => 1234 agree
STR : [20-2=] : LETTERS : [(= 2 - 0 2)] : 20-2= => 1233 agree
STR : [20-12] : LETTERS : [(2 1 - 0 2)] : 20-12 => 1232 agree
STR : [20-11] : LETTERS : [(1 1 - 0 2)] : 20-11 => 1231 agree
STR : [20-10] : LETTERS : [(0 1 - 0 2)] : 20-10 => 1230 agree
STR : [20-1-] : LETTERS : [(- 1 - 0 2)] : 20-1- => 1229 agree
STR : [20-1=] : LETTERS : [(= 1 - 0 2)] : 20-1= => 1228 agree
STR : [20-02] : LETTERS : [(2 0 - 0 2)] : 20-02 => 1227 agree
STR : [20-01] : LETTERS : [(1 0 - 0 2)] : 20-01 => 1226 agree
STR : [20-00] : LETTERS : [(0 0 - 0 2)] : 20-00 => 1225 agree
STR : [20-0-] : LETTERS : [(- 0 - 0 2)] : 20-0- => 1224 agree
STR : [20-0=] : LETTERS : [(= 0 - 0 2)] : 20-0= => 1223 agree
STR : [20--2] : LETTERS : [(2 - - 0 2)] : 20--2 => 1222 agree
STR : [20--1] : LETTERS : [(1 - - 0 2)] : 20--1 => 1221 agree
STR : [20--0] : LETTERS : [(0 - - 0 2)] : 20--0 => 1220 agree
STR : [20---] : LETTERS : [(- - - 0 2)] : 20--- => 1219 agree
STR : [20--=] : LETTERS : [(= - - 0 2)] : 20--= => 1218 agree
STR : [20-=2] : LETTERS : [(2 = - 0 2)] : 20-=2 => 1217 agree
STR : [20-=1] : LETTERS : [(1 = - 0 2)] : 20-=1 => 1216 agree
STR : [20-=0] : LETTERS : [(0 = - 0 2)] : 20-=0 => 1215 agree
STR : [20-=-] : LETTERS : [(- = - 0 2)] : 20-=- => 1214 agree
STR : [20-==] : LETTERS : [(= = - 0 2)] : 20-== => 1213 agree
STR : [20=22] : LETTERS : [(2 2 = 0 2)] : 20=22 => 1212 agree
STR : [20=21] : LETTERS : [(1 2 = 0 2)] : 20=21 => 1211 agree
STR : [20=20] : LETTERS : [(0 2 = 0 2)] : 20=20 => 1210 agree
STR : [20=2-] : LETTERS : [(- 2 = 0 2)] : 20=2- => 1209 agree
STR : [20=2=] : LETTERS : [(= 2 = 0 2)] : 20=2= => 1208 agree
STR : [20=12] : LETTERS : [(2 1 = 0 2)] : 20=12 => 1207 agree
STR : [20=11] : LETTERS : [(1 1 = 0 2)] : 20=11 => 1206 agree
STR : [20=10] : LETTERS : [(0 1 = 0 2)] : 20=10 => 1205 agree
STR : [20=1-] : LETTERS : [(- 1 = 0 2)] : 20=1- => 1204 agree
STR : [20=1=] : LETTERS : [(= 1 = 0 2)] : 20=1= => 1203 agree
STR : [20=02] : LETTERS : [(2 0 = 0 2)] : 20=02 => 1202 agree
STR : [20=01] : LETTERS : [(1 0 = 0 2)] : 20=01 => 1201 agree
STR : [20=00] : LETTERS : [(0 0 = 0 2)] : 20=00 => 1200 agree
STR : [20=0-] : LETTERS : [(- 0 = 0 2)] : 20=0- => 1199 agree
STR : [20=0=] : LETTERS : [(= 0 = 0 2)] : 20=0= => 1198 agree
STR : [20=-2] : LETTERS : [(2 - = 0 2)] : 20=-2 => 1197 agree
STR : [20=-1] : LETTERS : [(1 - = 0 2)] : 20=-1 => 1196 agree
STR : [20=-0] : LETTERS : [(0 - = 0 2)] : 20=-0 => 1195 agree
STR : [20=--] : LETTERS : [(- - = 0 2)] : 20=-- => 1194 agree
STR : [20=-=] : LETTERS : [(= - = 0 2)] : 20=-= => 1193 agree
STR : [20==2] : LETTERS : [(2 = = 0 2)] : 20==2 => 1192 agree
STR : [20==1] : LETTERS : [(1 = = 0 2)] : 20==1 => 1191 agree
STR : [20==0] : LETTERS : [(0 = = 0 2)] : 20==0 => 1190 agree
STR : [20==-] : LETTERS : [(- = = 0 2)] : 20==- => 1189 agree
STR : [20===] : LETTERS : [(= = = 0 2)] : 20=== => 1188 agree
STR : [2-222] : LETTERS : [(2 2 2 - 2)] : 2-222 => 1187 agree
STR : [2-221] : LETTERS : [(1 2 2 - 2)] : 2-221 => 1186 agree
STR : [2-220] : LETTERS : [(0 2 2 - 2)] : 2-220 => 1185 agree
STR : [2-22-] : LETTERS : [(- 2 2 - 2)] : 2-22- => 1184 agree
STR : [2-22=] : LETTERS : [(= 2 2 - 2)] : 2-22= => 1183 agree
STR : [2-212] : LETTERS : [(2 1 2 - 2)] : 2-212 => 1182 agree
STR : [2-211] : LETTERS : [(1 1 2 - 2)] : 2-211 => 1181 agree
STR : [2-210] : LETTERS : [(0 1 2 - 2)] : 2-210 => 1180 agree
STR : [2-21-] : LETTERS : [(- 1 2 - 2)] : 2-21- => 1179 agree
STR : [2-21=] : LETTERS : [(= 1 2 - 2)] : 2-21= => 1178 agree
STR : [2-202] : LETTERS : [(2 0 2 - 2)] : 2-202 => 1177 agree
STR : [2-201] : LETTERS : [(1 0 2 - 2)] : 2-201 => 1176 agree
STR : [2-200] : LETTERS : [(0 0 2 - 2)] : 2-200 => 1175 agree
STR : [2-20-] : LETTERS : [(- 0 2 - 2)] : 2-20- => 1174 agree
STR : [2-20=] : LETTERS : [(= 0 2 - 2)] : 2-20= => 1173 agree
STR : [2-2-2] : LETTERS : [(2 - 2 - 2)] : 2-2-2 => 1172 agree
STR : [2-2-1] : LETTERS : [(1 - 2 - 2)] : 2-2-1 => 1171 agree
STR : [2-2-0] : LETTERS : [(0 - 2 - 2)] : 2-2-0 => 1170 agree
STR : [2-2--] : LETTERS : [(- - 2 - 2)] : 2-2-- => 1169 agree
STR : [2-2-=] : LETTERS : [(= - 2 - 2)] : 2-2-= => 1168 agree
STR : [2-2=2] : LETTERS : [(2 = 2 - 2)] : 2-2=2 => 1167 agree
STR : [2-2=1] : LETTERS : [(1 = 2 - 2)] : 2-2=1 => 1166 agree
STR : [2-2=0] : LETTERS : [(0 = 2 - 2)] : 2-2=0 => 1165 agree
STR : [2-2=-] : LETTERS : [(- = 2 - 2)] : 2-2=- => 1164 agree
STR : [2-2==] : LETTERS : [(= = 2 - 2)] : 2-2== => 1163 agree
STR : [2-122] : LETTERS : [(2 2 1 - 2)] : 2-122 => 1162 agree
STR : [2-121] : LETTERS : [(1 2 1 - 2)] : 2-121 => 1161 agree
STR : [2-120] : LETTERS : [(0 2 1 - 2)] : 2-120 => 1160 agree
STR : [2-12-] : LETTERS : [(- 2 1 - 2)] : 2-12- => 1159 agree
STR : [2-12=] : LETTERS : [(= 2 1 - 2)] : 2-12= => 1158 agree
STR : [2-112] : LETTERS : [(2 1 1 - 2)] : 2-112 => 1157 agree
STR : [2-111] : LETTERS : [(1 1 1 - 2)] : 2-111 => 1156 agree
STR : [2-110] : LETTERS : [(0 1 1 - 2)] : 2-110 => 1155 agree
STR : [2-11-] : LETTERS : [(- 1 1 - 2)] : 2-11- => 1154 agree
STR : [2-11=] : LETTERS : [(= 1 1 - 2)] : 2-11= => 1153 agree
STR : [2-102] : LETTERS : [(2 0 1 - 2)] : 2-102 => 1152 agree
STR : [2-101] : LETTERS : [(1 0 1 - 2)] : 2-101 => 1151 agree
STR : [2-100] : LETTERS : [(0 0 1 - 2)] : 2-100 => 1150 agree
STR : [2-10-] : LETTERS : [(- 0 1 - 2)] : 2-10- => 1149 agree
STR : [2-10=] : LETTERS : [(= 0 1 - 2)] : 2-10= => 1148 agree
STR : [2-1-2] : LETTERS : [(2 - 1 - 2)] : 2-1-2 => 1147 agree
STR : [2-1-1] : LETTERS : [(1 - 1 - 2)] : 2-1-1 => 1146 agree
STR : [2-1-0] : LETTERS : [(0 - 1 - 2)] : 2-1-0 => 1145 agree
STR : [2-1--] : LETTERS : [(- - 1 - 2)] : 2-1-- => 1144 agree
STR : [2-1-=] : LETTERS : [(= - 1 - 2)] : 2-1-= => 1143 agree
STR : [2-1=2] : LETTERS : [(2 = 1 - 2)] : 2-1=2 => 1142 agree
STR : [2-1=1] : LETTERS : [(1 = 1 - 2)] : 2-1=1 => 1141 agree
STR : [2-1=0] : LETTERS : [(0 = 1 - 2)] : 2-1=0 => 1140 agree
STR : [2-1=-] : LETTERS : [(- = 1 - 2)] : 2-1=- => 1139 agree
STR : [2-1==] : LETTERS : [(= = 1 - 2)] : 2-1== => 1138 agree
STR : [2-022] : LETTERS : [(2 2 0 - 2)] : 2-022 => 1137 agree
STR : [2-021] : LETTERS : [(1 2 0 - 2)] : 2-021 => 1136 agree
STR : [2-020] : LETTERS : [(0 2 0 - 2)] : 2-020 => 1135 agree
STR : [2-02-] : LETTERS : [(- 2 0 - 2)] : 2-02- => 1134 agree
STR : [2-02=] : LETTERS : [(= 2 0 - 2)] : 2-02= => 1133 agree
STR : [2-012] : LETTERS : [(2 1 0 - 2)] : 2-012 => 1132 agree
STR : [2-011] : LETTERS : [(1 1 0 - 2)] : 2-011 => 1131 agree
STR : [2-010] : LETTERS : [(0 1 0 - 2)] : 2-010 => 1130 agree
STR : [2-01-] : LETTERS : [(- 1 0 - 2)] : 2-01- => 1129 agree
STR : [2-01=] : LETTERS : [(= 1 0 - 2)] : 2-01= => 1128 agree
STR : [2-002] : LETTERS : [(2 0 0 - 2)] : 2-002 => 1127 agree
STR : [2-001] : LETTERS : [(1 0 0 - 2)] : 2-001 => 1126 agree
STR : [2-000] : LETTERS : [(0 0 0 - 2)] : 2-000 => 1125 agree
STR : [2-00-] : LETTERS : [(- 0 0 - 2)] : 2-00- => 1124 agree
STR : [2-00=] : LETTERS : [(= 0 0 - 2)] : 2-00= => 1123 agree
STR : [2-0-2] : LETTERS : [(2 - 0 - 2)] : 2-0-2 => 1122 agree
STR : [2-0-1] : LETTERS : [(1 - 0 - 2)] : 2-0-1 => 1121 agree
STR : [2-0-0] : LETTERS : [(0 - 0 - 2)] : 2-0-0 => 1120 agree
STR : [2-0--] : LETTERS : [(- - 0 - 2)] : 2-0-- => 1119 agree
STR : [2-0-=] : LETTERS : [(= - 0 - 2)] : 2-0-= => 1118 agree
STR : [2-0=2] : LETTERS : [(2 = 0 - 2)] : 2-0=2 => 1117 agree
STR : [2-0=1] : LETTERS : [(1 = 0 - 2)] : 2-0=1 => 1116 agree
STR : [2-0=0] : LETTERS : [(0 = 0 - 2)] : 2-0=0 => 1115 agree
STR : [2-0=-] : LETTERS : [(- = 0 - 2)] : 2-0=- => 1114 agree
STR : [2-0==] : LETTERS : [(= = 0 - 2)] : 2-0== => 1113 agree
STR : [2--22] : LETTERS : [(2 2 - - 2)] : 2--22 => 1112 agree
STR : [2--21] : LETTERS : [(1 2 - - 2)] : 2--21 => 1111 agree
STR : [2--20] : LETTERS : [(0 2 - - 2)] : 2--20 => 1110 agree
STR : [2--2-] : LETTERS : [(- 2 - - 2)] : 2--2- => 1109 agree
STR : [2--2=] : LETTERS : [(= 2 - - 2)] : 2--2= => 1108 agree
STR : [2--12] : LETTERS : [(2 1 - - 2)] : 2--12 => 1107 agree
STR : [2--11] : LETTERS : [(1 1 - - 2)] : 2--11 => 1106 agree
STR : [2--10] : LETTERS : [(0 1 - - 2)] : 2--10 => 1105 agree
STR : [2--1-] : LETTERS : [(- 1 - - 2)] : 2--1- => 1104 agree
STR : [2--1=] : LETTERS : [(= 1 - - 2)] : 2--1= => 1103 agree
STR : [2--02] : LETTERS : [(2 0 - - 2)] : 2--02 => 1102 agree
STR : [2--01] : LETTERS : [(1 0 - - 2)] : 2--01 => 1101 agree
STR : [2--00] : LETTERS : [(0 0 - - 2)] : 2--00 => 1100 agree
STR : [2--0-] : LETTERS : [(- 0 - - 2)] : 2--0- => 1099 agree
STR : [2--0=] : LETTERS : [(= 0 - - 2)] : 2--0= => 1098 agree
STR : [2---2] : LETTERS : [(2 - - - 2)] : 2---2 => 1097 agree
STR : [2---1] : LETTERS : [(1 - - - 2)] : 2---1 => 1096 agree
STR : [2---0] : LETTERS : [(0 - - - 2)] : 2---0 => 1095 agree
STR : [2----] : LETTERS : [(- - - - 2)] : 2---- => 1094 agree
STR : [2---=] : LETTERS : [(= - - - 2)] : 2---= => 1093 agree
STR : [2--=2] : LETTERS : [(2 = - - 2)] : 2--=2 => 1092 agree
STR : [2--=1] : LETTERS : [(1 = - - 2)] : 2--=1 => 1091 agree
STR : [2--=0] : LETTERS : [(0 = - - 2)] : 2--=0 => 1090 agree
STR : [2--=-] : LETTERS : [(- = - - 2)] : 2--=- => 1089 agree
STR : [2--==] : LETTERS : [(= = - - 2)] : 2--== => 1088 agree
STR : [2-=22] : LETTERS : [(2 2 = - 2)] : 2-=22 => 1087 agree
STR : [2-=21] : LETTERS : [(1 2 = - 2)] : 2-=21 => 1086 agree
STR : [2-=20] : LETTERS : [(0 2 = - 2)] : 2-=20 => 1085 agree
STR : [2-=2-] : LETTERS : [(- 2 = - 2)] : 2-=2- => 1084 agree
STR : [2-=2=] : LETTERS : [(= 2 = - 2)] : 2-=2= => 1083 agree
STR : [2-=12] : LETTERS : [(2 1 = - 2)] : 2-=12 => 1082 agree
STR : [2-=11] : LETTERS : [(1 1 = - 2)] : 2-=11 => 1081 agree
STR : [2-=10] : LETTERS : [(0 1 = - 2)] : 2-=10 => 1080 agree
STR : [2-=1-] : LETTERS : [(- 1 = - 2)] : 2-=1- => 1079 agree
STR : [2-=1=] : LETTERS : [(= 1 = - 2)] : 2-=1= => 1078 agree
STR : [2-=02] : LETTERS : [(2 0 = - 2)] : 2-=02 => 1077 agree
STR : [2-=01] : LETTERS : [(1 0 = - 2)] : 2-=01 => 1076 agree
STR : [2-=00] : LETTERS : [(0 0 = - 2)] : 2-=00 => 1075 agree
STR : [2-=0-] : LETTERS : [(- 0 = - 2)] : 2-=0- => 1074 agree
STR : [2-=0=] : LETTERS : [(= 0 = - 2)] : 2-=0= => 1073 agree
STR : [2-=-2] : LETTERS : [(2 - = - 2)] : 2-=-2 => 1072 agree
STR : [2-=-1] : LETTERS : [(1 - = - 2)] : 2-=-1 => 1071 agree
STR : [2-=-0] : LETTERS : [(0 - = - 2)] : 2-=-0 => 1070 agree
STR : [2-=--] : LETTERS : [(- - = - 2)] : 2-=-- => 1069 agree
STR : [2-=-=] : LETTERS : [(= - = - 2)] : 2-=-= => 1068 agree
STR : [2-==2] : LETTERS : [(2 = = - 2)] : 2-==2 => 1067 agree
STR : [2-==1] : LETTERS : [(1 = = - 2)] : 2-==1 => 1066 agree
STR : [2-==0] : LETTERS : [(0 = = - 2)] : 2-==0 => 1065 agree
STR : [2-==-] : LETTERS : [(- = = - 2)] : 2-==- => 1064 agree
STR : [2-===] : LETTERS : [(= = = - 2)] : 2-=== => 1063 agree
STR : [2=222] : LETTERS : [(2 2 2 = 2)] : 2=222 => 1062 agree
STR : [2=221] : LETTERS : [(1 2 2 = 2)] : 2=221 => 1061 agree
STR : [2=220] : LETTERS : [(0 2 2 = 2)] : 2=220 => 1060 agree
STR : [2=22-] : LETTERS : [(- 2 2 = 2)] : 2=22- => 1059 agree
STR : [2=22=] : LETTERS : [(= 2 2 = 2)] : 2=22= => 1058 agree
STR : [2=212] : LETTERS : [(2 1 2 = 2)] : 2=212 => 1057 agree
STR : [2=211] : LETTERS : [(1 1 2 = 2)] : 2=211 => 1056 agree
STR : [2=210] : LETTERS : [(0 1 2 = 2)] : 2=210 => 1055 agree
STR : [2=21-] : LETTERS : [(- 1 2 = 2)] : 2=21- => 1054 agree
STR : [2=21=] : LETTERS : [(= 1 2 = 2)] : 2=21= => 1053 agree
STR : [2=202] : LETTERS : [(2 0 2 = 2)] : 2=202 => 1052 agree
STR : [2=201] : LETTERS : [(1 0 2 = 2)] : 2=201 => 1051 agree
STR : [2=200] : LETTERS : [(0 0 2 = 2)] : 2=200 => 1050 agree
STR : [2=20-] : LETTERS : [(- 0 2 = 2)] : 2=20- => 1049 agree
STR : [2=20=] : LETTERS : [(= 0 2 = 2)] : 2=20= => 1048 agree
STR : [2=2-2] : LETTERS : [(2 - 2 = 2)] : 2=2-2 => 1047 agree
STR : [2=2-1] : LETTERS : [(1 - 2 = 2)] : 2=2-1 => 1046 agree
STR : [2=2-0] : LETTERS : [(0 - 2 = 2)] : 2=2-0 => 1045 agree
STR : [2=2--] : LETTERS : [(- - 2 = 2)] : 2=2-- => 1044 agree
STR : [2=2-=] : LETTERS : [(= - 2 = 2)] : 2=2-= => 1043 agree
STR : [2=2=2] : LETTERS : [(2 = 2 = 2)] : 2=2=2 => 1042 agree
STR : [2=2=1] : LETTERS : [(1 = 2 = 2)] : 2=2=1 => 1041 agree
STR : [2=2=0] : LETTERS : [(0 = 2 = 2)] : 2=2=0 => 1040 agree
STR : [2=2=-] : LETTERS : [(- = 2 = 2)] : 2=2=- => 1039 agree
STR : [2=2==] : LETTERS : [(= = 2 = 2)] : 2=2== => 1038 agree
STR : [2=122] : LETTERS : [(2 2 1 = 2)] : 2=122 => 1037 agree
STR : [2=121] : LETTERS : [(1 2 1 = 2)] : 2=121 => 1036 agree
STR : [2=120] : LETTERS : [(0 2 1 = 2)] : 2=120 => 1035 agree
STR : [2=12-] : LETTERS : [(- 2 1 = 2)] : 2=12- => 1034 agree
STR : [2=12=] : LETTERS : [(= 2 1 = 2)] : 2=12= => 1033 agree
STR : [2=112] : LETTERS : [(2 1 1 = 2)] : 2=112 => 1032 agree
STR : [2=111] : LETTERS : [(1 1 1 = 2)] : 2=111 => 1031 agree
STR : [2=110] : LETTERS : [(0 1 1 = 2)] : 2=110 => 1030 agree
STR : [2=11-] : LETTERS : [(- 1 1 = 2)] : 2=11- => 1029 agree
STR : [2=11=] : LETTERS : [(= 1 1 = 2)] : 2=11= => 1028 agree
STR : [2=102] : LETTERS : [(2 0 1 = 2)] : 2=102 => 1027 agree
STR : [2=101] : LETTERS : [(1 0 1 = 2)] : 2=101 => 1026 agree
STR : [2=100] : LETTERS : [(0 0 1 = 2)] : 2=100 => 1025 agree
STR : [2=10-] : LETTERS : [(- 0 1 = 2)] : 2=10- => 1024 agree
STR : [2=10=] : LETTERS : [(= 0 1 = 2)] : 2=10= => 1023 agree
STR : [2=1-2] : LETTERS : [(2 - 1 = 2)] : 2=1-2 => 1022 agree
STR : [2=1-1] : LETTERS : [(1 - 1 = 2)] : 2=1-1 => 1021 agree
STR : [2=1-0] : LETTERS : [(0 - 1 = 2)] : 2=1-0 => 1020 agree
STR : [2=1--] : LETTERS : [(- - 1 = 2)] : 2=1-- => 1019 agree
STR : [2=1-=] : LETTERS : [(= - 1 = 2)] : 2=1-= => 1018 agree
STR : [2=1=2] : LETTERS : [(2 = 1 = 2)] : 2=1=2 => 1017 agree
STR : [2=1=1] : LETTERS : [(1 = 1 = 2)] : 2=1=1 => 1016 agree
STR : [2=1=0] : LETTERS : [(0 = 1 = 2)] : 2=1=0 => 1015 agree
STR : [2=1=-] : LETTERS : [(- = 1 = 2)] : 2=1=- => 1014 agree
STR : [2=1==] : LETTERS : [(= = 1 = 2)] : 2=1== => 1013 agree
STR : [2=022] : LETTERS : [(2 2 0 = 2)] : 2=022 => 1012 agree
STR : [2=021] : LETTERS : [(1 2 0 = 2)] : 2=021 => 1011 agree
STR : [2=020] : LETTERS : [(0 2 0 = 2)] : 2=020 => 1010 agree
STR : [2=02-] : LETTERS : [(- 2 0 = 2)] : 2=02- => 1009 agree
STR : [2=02=] : LETTERS : [(= 2 0 = 2)] : 2=02= => 1008 agree
STR : [2=012] : LETTERS : [(2 1 0 = 2)] : 2=012 => 1007 agree
STR : [2=011] : LETTERS : [(1 1 0 = 2)] : 2=011 => 1006 agree
STR : [2=010] : LETTERS : [(0 1 0 = 2)] : 2=010 => 1005 agree
STR : [2=01-] : LETTERS : [(- 1 0 = 2)] : 2=01- => 1004 agree
STR : [2=01=] : LETTERS : [(= 1 0 = 2)] : 2=01= => 1003 agree
STR : [2=002] : LETTERS : [(2 0 0 = 2)] : 2=002 => 1002 agree
STR : [2=001] : LETTERS : [(1 0 0 = 2)] : 2=001 => 1001 agree
STR : [2=000] : LETTERS : [(0 0 0 = 2)] : 2=000 => 1000 agree
STR : [2=00-] : LETTERS : [(- 0 0 = 2)] : 2=00- => 999 agree
STR : [2=00=] : LETTERS : [(= 0 0 = 2)] : 2=00= => 998 agree
STR : [2=0-2] : LETTERS : [(2 - 0 = 2)] : 2=0-2 => 997 agree
STR : [2=0-1] : LETTERS : [(1 - 0 = 2)] : 2=0-1 => 996 agree
STR : [2=0-0] : LETTERS : [(0 - 0 = 2)] : 2=0-0 => 995 agree
STR : [2=0--] : LETTERS : [(- - 0 = 2)] : 2=0-- => 994 agree
STR : [2=0-=] : LETTERS : [(= - 0 = 2)] : 2=0-= => 993 agree
STR : [2=0=2] : LETTERS : [(2 = 0 = 2)] : 2=0=2 => 992 agree
STR : [2=0=1] : LETTERS : [(1 = 0 = 2)] : 2=0=1 => 991 agree
STR : [2=0=0] : LETTERS : [(0 = 0 = 2)] : 2=0=0 => 990 agree
STR : [2=0=-] : LETTERS : [(- = 0 = 2)] : 2=0=- => 989 agree
STR : [2=0==] : LETTERS : [(= = 0 = 2)] : 2=0== => 988 agree
STR : [2=-22] : LETTERS : [(2 2 - = 2)] : 2=-22 => 987 agree
STR : [2=-21] : LETTERS : [(1 2 - = 2)] : 2=-21 => 986 agree
STR : [2=-20] : LETTERS : [(0 2 - = 2)] : 2=-20 => 985 agree
STR : [2=-2-] : LETTERS : [(- 2 - = 2)] : 2=-2- => 984 agree
STR : [2=-2=] : LETTERS : [(= 2 - = 2)] : 2=-2= => 983 agree
STR : [2=-12] : LETTERS : [(2 1 - = 2)] : 2=-12 => 982 agree
STR : [2=-11] : LETTERS : [(1 1 - = 2)] : 2=-11 => 981 agree
STR : [2=-10] : LETTERS : [(0 1 - = 2)] : 2=-10 => 980 agree
STR : [2=-1-] : LETTERS : [(- 1 - = 2)] : 2=-1- => 979 agree
STR : [2=-1=] : LETTERS : [(= 1 - = 2)] : 2=-1= => 978 agree
STR : [2=-02] : LETTERS : [(2 0 - = 2)] : 2=-02 => 977 agree
STR : [2=-01] : LETTERS : [(1 0 - = 2)] : 2=-01 => 976 agree
STR : [2=-00] : LETTERS : [(0 0 - = 2)] : 2=-00 => 975 agree
STR : [2=-0-] : LETTERS : [(- 0 - = 2)] : 2=-0- => 974 agree
STR : [2=-0=] : LETTERS : [(= 0 - = 2)] : 2=-0= => 973 agree
STR : [2=--2] : LETTERS : [(2 - - = 2)] : 2=--2 => 972 agree
STR : [2=--1] : LETTERS : [(1 - - = 2)] : 2=--1 => 971 agree
STR : [2=--0] : LETTERS : [(0 - - = 2)] : 2=--0 => 970 agree
STR : [2=---] : LETTERS : [(- - - = 2)] : 2=--- => 969 agree
STR : [2=--=] : LETTERS : [(= - - = 2)] : 2=--= => 968 agree
STR : [2=-=2] : LETTERS : [(2 = - = 2)] : 2=-=2 => 967 agree
STR : [2=-=1] : LETTERS : [(1 = - = 2)] : 2=-=1 => 966 agree
STR : [2=-=0] : LETTERS : [(0 = - = 2)] : 2=-=0 => 965 agree
STR : [2=-=-] : LETTERS : [(- = - = 2)] : 2=-=- => 964 agree
STR : [2=-==] : LETTERS : [(= = - = 2)] : 2=-== => 963 agree
STR : [2==22] : LETTERS : [(2 2 = = 2)] : 2==22 => 962 agree
STR : [2==21] : LETTERS : [(1 2 = = 2)] : 2==21 => 961 agree
STR : [2==20] : LETTERS : [(0 2 = = 2)] : 2==20 => 960 agree
STR : [2==2-] : LETTERS : [(- 2 = = 2)] : 2==2- => 959 agree
STR : [2==2=] : LETTERS : [(= 2 = = 2)] : 2==2= => 958 agree
STR : [2==12] : LETTERS : [(2 1 = = 2)] : 2==12 => 957 agree
STR : [2==11] : LETTERS : [(1 1 = = 2)] : 2==11 => 956 agree
STR : [2==10] : LETTERS : [(0 1 = = 2)] : 2==10 => 955 agree
STR : [2==1-] : LETTERS : [(- 1 = = 2)] : 2==1- => 954 agree
STR : [2==1=] : LETTERS : [(= 1 = = 2)] : 2==1= => 953 agree
STR : [2==02] : LETTERS : [(2 0 = = 2)] : 2==02 => 952 agree
STR : [2==01] : LETTERS : [(1 0 = = 2)] : 2==01 => 951 agree
STR : [2==00] : LETTERS : [(0 0 = = 2)] : 2==00 => 950 agree
STR : [2==0-] : LETTERS : [(- 0 = = 2)] : 2==0- => 949 agree
STR : [2==0=] : LETTERS : [(= 0 = = 2)] : 2==0= => 948 agree
STR : [2==-2] : LETTERS : [(2 - = = 2)] : 2==-2 => 947 agree
STR : [2==-1] : LETTERS : [(1 - = = 2)] : 2==-1 => 946 agree
STR : [2==-0] : LETTERS : [(0 - = = 2)] : 2==-0 => 945 agree
STR : [2==--] : LETTERS : [(- - = = 2)] : 2==-- => 944 agree
STR : [2==-=] : LETTERS : [(= - = = 2)] : 2==-= => 943 agree
STR : [2===2] : LETTERS : [(2 = = = 2)] : 2===2 => 942 agree
STR : [2===1] : LETTERS : [(1 = = = 2)] : 2===1 => 941 agree
STR : [2===0] : LETTERS : [(0 = = = 2)] : 2===0 => 940 agree
STR : [2===-] : LETTERS : [(- = = = 2)] : 2===- => 939 agree
STR : [2====] : LETTERS : [(= = = = 2)] : 2==== => 938 agree
STR : [12222] : LETTERS : [(2 2 2 2 1)] : 12222 => 937 agree
STR : [12221] : LETTERS : [(1 2 2 2 1)] : 12221 => 936 agree
STR : [12220] : LETTERS : [(0 2 2 2 1)] : 12220 => 935 agree
STR : [1222-] : LETTERS : [(- 2 2 2 1)] : 1222- => 934 agree
STR : [1222=] : LETTERS : [(= 2 2 2 1)] : 1222= => 933 agree
STR : [12212] : LETTERS : [(2 1 2 2 1)] : 12212 => 932 agree
STR : [12211] : LETTERS : [(1 1 2 2 1)] : 12211 => 931 agree
STR : [12210] : LETTERS : [(0 1 2 2 1)] : 12210 => 930 agree
STR : [1221-] : LETTERS : [(- 1 2 2 1)] : 1221- => 929 agree
STR : [1221=] : LETTERS : [(= 1 2 2 1)] : 1221= => 928 agree
STR : [12202] : LETTERS : [(2 0 2 2 1)] : 12202 => 927 agree
STR : [12201] : LETTERS : [(1 0 2 2 1)] : 12201 => 926 agree
STR : [12200] : LETTERS : [(0 0 2 2 1)] : 12200 => 925 agree
STR : [1220-] : LETTERS : [(- 0 2 2 1)] : 1220- => 924 agree
STR : [1220=] : LETTERS : [(= 0 2 2 1)] : 1220= => 923 agree
STR : [122-2] : LETTERS : [(2 - 2 2 1)] : 122-2 => 922 agree
STR : [122-1] : LETTERS : [(1 - 2 2 1)] : 122-1 => 921 agree
STR : [122-0] : LETTERS : [(0 - 2 2 1)] : 122-0 => 920 agree
STR : [122--] : LETTERS : [(- - 2 2 1)] : 122-- => 919 agree
STR : [122-=] : LETTERS : [(= - 2 2 1)] : 122-= => 918 agree
STR : [122=2] : LETTERS : [(2 = 2 2 1)] : 122=2 => 917 agree
STR : [122=1] : LETTERS : [(1 = 2 2 1)] : 122=1 => 916 agree
STR : [122=0] : LETTERS : [(0 = 2 2 1)] : 122=0 => 915 agree
STR : [122=-] : LETTERS : [(- = 2 2 1)] : 122=- => 914 agree
STR : [122==] : LETTERS : [(= = 2 2 1)] : 122== => 913 agree
STR : [12122] : LETTERS : [(2 2 1 2 1)] : 12122 => 912 agree
STR : [12121] : LETTERS : [(1 2 1 2 1)] : 12121 => 911 agree
STR : [12120] : LETTERS : [(0 2 1 2 1)] : 12120 => 910 agree
STR : [1212-] : LETTERS : [(- 2 1 2 1)] : 1212- => 909 agree
STR : [1212=] : LETTERS : [(= 2 1 2 1)] : 1212= => 908 agree
STR : [12112] : LETTERS : [(2 1 1 2 1)] : 12112 => 907 agree
STR : [12111] : LETTERS : [(1 1 1 2 1)] : 12111 => 906 agree
STR : [12110] : LETTERS : [(0 1 1 2 1)] : 12110 => 905 agree
STR : [1211-] : LETTERS : [(- 1 1 2 1)] : 1211- => 904 agree
STR : [1211=] : LETTERS : [(= 1 1 2 1)] : 1211= => 903 agree
STR : [12102] : LETTERS : [(2 0 1 2 1)] : 12102 => 902 agree
STR : [12101] : LETTERS : [(1 0 1 2 1)] : 12101 => 901 agree
STR : [12100] : LETTERS : [(0 0 1 2 1)] : 12100 => 900 agree
STR : [1210-] : LETTERS : [(- 0 1 2 1)] : 1210- => 899 agree
STR : [1210=] : LETTERS : [(= 0 1 2 1)] : 1210= => 898 agree
STR : [121-2] : LETTERS : [(2 - 1 2 1)] : 121-2 => 897 agree
STR : [121-1] : LETTERS : [(1 - 1 2 1)] : 121-1 => 896 agree
STR : [121-0] : LETTERS : [(0 - 1 2 1)] : 121-0 => 895 agree
STR : [121--] : LETTERS : [(- - 1 2 1)] : 121-- => 894 agree
STR : [121-=] : LETTERS : [(= - 1 2 1)] : 121-= => 893 agree
STR : [121=2] : LETTERS : [(2 = 1 2 1)] : 121=2 => 892 agree
STR : [121=1] : LETTERS : [(1 = 1 2 1)] : 121=1 => 891 agree
STR : [121=0] : LETTERS : [(0 = 1 2 1)] : 121=0 => 890 agree
STR : [121=-] : LETTERS : [(- = 1 2 1)] : 121=- => 889 agree
STR : [121==] : LETTERS : [(= = 1 2 1)] : 121== => 888 agree
STR : [12022] : LETTERS : [(2 2 0 2 1)] : 12022 => 887 agree
STR : [12021] : LETTERS : [(1 2 0 2 1)] : 12021 => 886 agree
STR : [12020] : LETTERS : [(0 2 0 2 1)] : 12020 => 885 agree
STR : [1202-] : LETTERS : [(- 2 0 2 1)] : 1202- => 884 agree
STR : [1202=] : LETTERS : [(= 2 0 2 1)] : 1202= => 883 agree
STR : [12012] : LETTERS : [(2 1 0 2 1)] : 12012 => 882 agree
STR : [12011] : LETTERS : [(1 1 0 2 1)] : 12011 => 881 agree
STR : [12010] : LETTERS : [(0 1 0 2 1)] : 12010 => 880 agree
STR : [1201-] : LETTERS : [(- 1 0 2 1)] : 1201- => 879 agree
STR : [1201=] : LETTERS : [(= 1 0 2 1)] : 1201= => 878 agree
STR : [12002] : LETTERS : [(2 0 0 2 1)] : 12002 => 877 agree
STR : [12001] : LETTERS : [(1 0 0 2 1)] : 12001 => 876 agree
STR : [12000] : LETTERS : [(0 0 0 2 1)] : 12000 => 875 agree
STR : [1200-] : LETTERS : [(- 0 0 2 1)] : 1200- => 874 agree
STR : [1200=] : LETTERS : [(= 0 0 2 1)] : 1200= => 873 agree
STR : [120-2] : LETTERS : [(2 - 0 2 1)] : 120-2 => 872 agree
STR : [120-1] : LETTERS : [(1 - 0 2 1)] : 120-1 => 871 agree
STR : [120-0] : LETTERS : [(0 - 0 2 1)] : 120-0 => 870 agree
STR : [120--] : LETTERS : [(- - 0 2 1)] : 120-- => 869 agree
STR : [120-=] : LETTERS : [(= - 0 2 1)] : 120-= => 868 agree
STR : [120=2] : LETTERS : [(2 = 0 2 1)] : 120=2 => 867 agree
STR : [120=1] : LETTERS : [(1 = 0 2 1)] : 120=1 => 866 agree
STR : [120=0] : LETTERS : [(0 = 0 2 1)] : 120=0 => 865 agree
STR : [120=-] : LETTERS : [(- = 0 2 1)] : 120=- => 864 agree
STR : [120==] : LETTERS : [(= = 0 2 1)] : 120== => 863 agree
STR : [12-22] : LETTERS : [(2 2 - 2 1)] : 12-22 => 862 agree
STR : [12-21] : LETTERS : [(1 2 - 2 1)] : 12-21 => 861 agree
STR : [12-20] : LETTERS : [(0 2 - 2 1)] : 12-20 => 860 agree
STR : [12-2-] : LETTERS : [(- 2 - 2 1)] : 12-2- => 859 agree
STR : [12-2=] : LETTERS : [(= 2 - 2 1)] : 12-2= => 858 agree
STR : [12-12] : LETTERS : [(2 1 - 2 1)] : 12-12 => 857 agree
STR : [12-11] : LETTERS : [(1 1 - 2 1)] : 12-11 => 856 agree
STR : [12-10] : LETTERS : [(0 1 - 2 1)] : 12-10 => 855 agree
STR : [12-1-] : LETTERS : [(- 1 - 2 1)] : 12-1- => 854 agree
STR : [12-1=] : LETTERS : [(= 1 - 2 1)] : 12-1= => 853 agree
STR : [12-02] : LETTERS : [(2 0 - 2 1)] : 12-02 => 852 agree
STR : [12-01] : LETTERS : [(1 0 - 2 1)] : 12-01 => 851 agree
STR : [12-00] : LETTERS : [(0 0 - 2 1)] : 12-00 => 850 agree
STR : [12-0-] : LETTERS : [(- 0 - 2 1)] : 12-0- => 849 agree
STR : [12-0=] : LETTERS : [(= 0 - 2 1)] : 12-0= => 848 agree
STR : [12--2] : LETTERS : [(2 - - 2 1)] : 12--2 => 847 agree
STR : [12--1] : LETTERS : [(1 - - 2 1)] : 12--1 => 846 agree
STR : [12--0] : LETTERS : [(0 - - 2 1)] : 12--0 => 845 agree
STR : [12---] : LETTERS : [(- - - 2 1)] : 12--- => 844 agree
STR : [12--=] : LETTERS : [(= - - 2 1)] : 12--= => 843 agree
STR : [12-=2] : LETTERS : [(2 = - 2 1)] : 12-=2 => 842 agree
STR : [12-=1] : LETTERS : [(1 = - 2 1)] : 12-=1 => 841 agree
STR : [12-=0] : LETTERS : [(0 = - 2 1)] : 12-=0 => 840 agree
STR : [12-=-] : LETTERS : [(- = - 2 1)] : 12-=- => 839 agree
STR : [12-==] : LETTERS : [(= = - 2 1)] : 12-== => 838 agree
STR : [12=22] : LETTERS : [(2 2 = 2 1)] : 12=22 => 837 agree
STR : [12=21] : LETTERS : [(1 2 = 2 1)] : 12=21 => 836 agree
STR : [12=20] : LETTERS : [(0 2 = 2 1)] : 12=20 => 835 agree
STR : [12=2-] : LETTERS : [(- 2 = 2 1)] : 12=2- => 834 agree
STR : [12=2=] : LETTERS : [(= 2 = 2 1)] : 12=2= => 833 agree
STR : [12=12] : LETTERS : [(2 1 = 2 1)] : 12=12 => 832 agree
STR : [12=11] : LETTERS : [(1 1 = 2 1)] : 12=11 => 831 agree
STR : [12=10] : LETTERS : [(0 1 = 2 1)] : 12=10 => 830 agree
STR : [12=1-] : LETTERS : [(- 1 = 2 1)] : 12=1- => 829 agree
STR : [12=1=] : LETTERS : [(= 1 = 2 1)] : 12=1= => 828 agree
STR : [12=02] : LETTERS : [(2 0 = 2 1)] : 12=02 => 827 agree
STR : [12=01] : LETTERS : [(1 0 = 2 1)] : 12=01 => 826 agree
STR : [12=00] : LETTERS : [(0 0 = 2 1)] : 12=00 => 825 agree
STR : [12=0-] : LETTERS : [(- 0 = 2 1)] : 12=0- => 824 agree
STR : [12=0=] : LETTERS : [(= 0 = 2 1)] : 12=0= => 823 agree
STR : [12=-2] : LETTERS : [(2 - = 2 1)] : 12=-2 => 822 agree
STR : [12=-1] : LETTERS : [(1 - = 2 1)] : 12=-1 => 821 agree
STR : [12=-0] : LETTERS : [(0 - = 2 1)] : 12=-0 => 820 agree
STR : [12=--] : LETTERS : [(- - = 2 1)] : 12=-- => 819 agree
STR : [12=-=] : LETTERS : [(= - = 2 1)] : 12=-= => 818 agree
STR : [12==2] : LETTERS : [(2 = = 2 1)] : 12==2 => 817 agree
STR : [12==1] : LETTERS : [(1 = = 2 1)] : 12==1 => 816 agree
STR : [12==0] : LETTERS : [(0 = = 2 1)] : 12==0 => 815 agree
STR : [12==-] : LETTERS : [(- = = 2 1)] : 12==- => 814 agree
STR : [12===] : LETTERS : [(= = = 2 1)] : 12=== => 813 agree
STR : [11222] : LETTERS : [(2 2 2 1 1)] : 11222 => 812 agree
STR : [11221] : LETTERS : [(1 2 2 1 1)] : 11221 => 811 agree
STR : [11220] : LETTERS : [(0 2 2 1 1)] : 11220 => 810 agree
STR : [1122-] : LETTERS : [(- 2 2 1 1)] : 1122- => 809 agree
STR : [1122=] : LETTERS : [(= 2 2 1 1)] : 1122= => 808 agree
STR : [11212] : LETTERS : [(2 1 2 1 1)] : 11212 => 807 agree
STR : [11211] : LETTERS : [(1 1 2 1 1)] : 11211 => 806 agree
STR : [11210] : LETTERS : [(0 1 2 1 1)] : 11210 => 805 agree
STR : [1121-] : LETTERS : [(- 1 2 1 1)] : 1121- => 804 agree
STR : [1121=] : LETTERS : [(= 1 2 1 1)] : 1121= => 803 agree
STR : [11202] : LETTERS : [(2 0 2 1 1)] : 11202 => 802 agree
STR : [11201] : LETTERS : [(1 0 2 1 1)] : 11201 => 801 agree
STR : [11200] : LETTERS : [(0 0 2 1 1)] : 11200 => 800 agree
STR : [1120-] : LETTERS : [(- 0 2 1 1)] : 1120- => 799 agree
STR : [1120=] : LETTERS : [(= 0 2 1 1)] : 1120= => 798 agree
STR : [112-2] : LETTERS : [(2 - 2 1 1)] : 112-2 => 797 agree
STR : [112-1] : LETTERS : [(1 - 2 1 1)] : 112-1 => 796 agree
STR : [112-0] : LETTERS : [(0 - 2 1 1)] : 112-0 => 795 agree
STR : [112--] : LETTERS : [(- - 2 1 1)] : 112-- => 794 agree
STR : [112-=] : LETTERS : [(= - 2 1 1)] : 112-= => 793 agree
STR : [112=2] : LETTERS : [(2 = 2 1 1)] : 112=2 => 792 agree
STR : [112=1] : LETTERS : [(1 = 2 1 1)] : 112=1 => 791 agree
STR : [112=0] : LETTERS : [(0 = 2 1 1)] : 112=0 => 790 agree
STR : [112=-] : LETTERS : [(- = 2 1 1)] : 112=- => 789 agree
STR : [112==] : LETTERS : [(= = 2 1 1)] : 112== => 788 agree
STR : [11122] : LETTERS : [(2 2 1 1 1)] : 11122 => 787 agree
STR : [11121] : LETTERS : [(1 2 1 1 1)] : 11121 => 786 agree
STR : [11120] : LETTERS : [(0 2 1 1 1)] : 11120 => 785 agree
STR : [1112-] : LETTERS : [(- 2 1 1 1)] : 1112- => 784 agree
STR : [1112=] : LETTERS : [(= 2 1 1 1)] : 1112= => 783 agree
STR : [11112] : LETTERS : [(2 1 1 1 1)] : 11112 => 782 agree
STR : [11111] : LETTERS : [(1 1 1 1 1)] : 11111 => 781 agree
STR : [11110] : LETTERS : [(0 1 1 1 1)] : 11110 => 780 agree
STR : [1111-] : LETTERS : [(- 1 1 1 1)] : 1111- => 779 agree
STR : [1111=] : LETTERS : [(= 1 1 1 1)] : 1111= => 778 agree
STR : [11102] : LETTERS : [(2 0 1 1 1)] : 11102 => 777 agree
STR : [11101] : LETTERS : [(1 0 1 1 1)] : 11101 => 776 agree
STR : [11100] : LETTERS : [(0 0 1 1 1)] : 11100 => 775 agree
STR : [1110-] : LETTERS : [(- 0 1 1 1)] : 1110- => 774 agree
STR : [1110=] : LETTERS : [(= 0 1 1 1)] : 1110= => 773 agree
STR : [111-2] : LETTERS : [(2 - 1 1 1)] : 111-2 => 772 agree
STR : [111-1] : LETTERS : [(1 - 1 1 1)] : 111-1 => 771 agree
STR : [111-0] : LETTERS : [(0 - 1 1 1)] : 111-0 => 770 agree
STR : [111--] : LETTERS : [(- - 1 1 1)] : 111-- => 769 agree
STR : [111-=] : LETTERS : [(= - 1 1 1)] : 111-= => 768 agree
STR : [111=2] : LETTERS : [(2 = 1 1 1)] : 111=2 => 767 agree
STR : [111=1] : LETTERS : [(1 = 1 1 1)] : 111=1 => 766 agree
STR : [111=0] : LETTERS : [(0 = 1 1 1)] : 111=0 => 765 agree
STR : [111=-] : LETTERS : [(- = 1 1 1)] : 111=- => 764 agree
STR : [111==] : LETTERS : [(= = 1 1 1)] : 111== => 763 agree
STR : [11022] : LETTERS : [(2 2 0 1 1)] : 11022 => 762 agree
STR : [11021] : LETTERS : [(1 2 0 1 1)] : 11021 => 761 agree
STR : [11020] : LETTERS : [(0 2 0 1 1)] : 11020 => 760 agree
STR : [1102-] : LETTERS : [(- 2 0 1 1)] : 1102- => 759 agree
STR : [1102=] : LETTERS : [(= 2 0 1 1)] : 1102= => 758 agree
STR : [11012] : LETTERS : [(2 1 0 1 1)] : 11012 => 757 agree
STR : [11011] : LETTERS : [(1 1 0 1 1)] : 11011 => 756 agree
STR : [11010] : LETTERS : [(0 1 0 1 1)] : 11010 => 755 agree
STR : [1101-] : LETTERS : [(- 1 0 1 1)] : 1101- => 754 agree
STR : [1101=] : LETTERS : [(= 1 0 1 1)] : 1101= => 753 agree
STR : [11002] : LETTERS : [(2 0 0 1 1)] : 11002 => 752 agree
STR : [11001] : LETTERS : [(1 0 0 1 1)] : 11001 => 751 agree
STR : [11000] : LETTERS : [(0 0 0 1 1)] : 11000 => 750 agree
STR : [1100-] : LETTERS : [(- 0 0 1 1)] : 1100- => 749 agree
STR : [1100=] : LETTERS : [(= 0 0 1 1)] : 1100= => 748 agree
STR : [110-2] : LETTERS : [(2 - 0 1 1)] : 110-2 => 747 agree
STR : [110-1] : LETTERS : [(1 - 0 1 1)] : 110-1 => 746 agree
STR : [110-0] : LETTERS : [(0 - 0 1 1)] : 110-0 => 745 agree
STR : [110--] : LETTERS : [(- - 0 1 1)] : 110-- => 744 agree
STR : [110-=] : LETTERS : [(= - 0 1 1)] : 110-= => 743 agree
STR : [110=2] : LETTERS : [(2 = 0 1 1)] : 110=2 => 742 agree
STR : [110=1] : LETTERS : [(1 = 0 1 1)] : 110=1 => 741 agree
STR : [110=0] : LETTERS : [(0 = 0 1 1)] : 110=0 => 740 agree
STR : [110=-] : LETTERS : [(- = 0 1 1)] : 110=- => 739 agree
STR : [110==] : LETTERS : [(= = 0 1 1)] : 110== => 738 agree
STR : [11-22] : LETTERS : [(2 2 - 1 1)] : 11-22 => 737 agree
STR : [11-21] : LETTERS : [(1 2 - 1 1)] : 11-21 => 736 agree
STR : [11-20] : LETTERS : [(0 2 - 1 1)] : 11-20 => 735 agree
STR : [11-2-] : LETTERS : [(- 2 - 1 1)] : 11-2- => 734 agree
STR : [11-2=] : LETTERS : [(= 2 - 1 1)] : 11-2= => 733 agree
STR : [11-12] : LETTERS : [(2 1 - 1 1)] : 11-12 => 732 agree
STR : [11-11] : LETTERS : [(1 1 - 1 1)] : 11-11 => 731 agree
STR : [11-10] : LETTERS : [(0 1 - 1 1)] : 11-10 => 730 agree
STR : [11-1-] : LETTERS : [(- 1 - 1 1)] : 11-1- => 729 agree
STR : [11-1=] : LETTERS : [(= 1 - 1 1)] : 11-1= => 728 agree
STR : [11-02] : LETTERS : [(2 0 - 1 1)] : 11-02 => 727 agree
STR : [11-01] : LETTERS : [(1 0 - 1 1)] : 11-01 => 726 agree
STR : [11-00] : LETTERS : [(0 0 - 1 1)] : 11-00 => 725 agree
STR : [11-0-] : LETTERS : [(- 0 - 1 1)] : 11-0- => 724 agree
STR : [11-0=] : LETTERS : [(= 0 - 1 1)] : 11-0= => 723 agree
STR : [11--2] : LETTERS : [(2 - - 1 1)] : 11--2 => 722 agree
STR : [11--1] : LETTERS : [(1 - - 1 1)] : 11--1 => 721 agree
STR : [11--0] : LETTERS : [(0 - - 1 1)] : 11--0 => 720 agree
STR : [11---] : LETTERS : [(- - - 1 1)] : 11--- => 719 agree
STR : [11--=] : LETTERS : [(= - - 1 1)] : 11--= => 718 agree
STR : [11-=2] : LETTERS : [(2 = - 1 1)] : 11-=2 => 717 agree
STR : [11-=1] : LETTERS : [(1 = - 1 1)] : 11-=1 => 716 agree
STR : [11-=0] : LETTERS : [(0 = - 1 1)] : 11-=0 => 715 agree
STR : [11-=-] : LETTERS : [(- = - 1 1)] : 11-=- => 714 agree
STR : [11-==] : LETTERS : [(= = - 1 1)] : 11-== => 713 agree
STR : [11=22] : LETTERS : [(2 2 = 1 1)] : 11=22 => 712 agree
STR : [11=21] : LETTERS : [(1 2 = 1 1)] : 11=21 => 711 agree
STR : [11=20] : LETTERS : [(0 2 = 1 1)] : 11=20 => 710 agree
STR : [11=2-] : LETTERS : [(- 2 = 1 1)] : 11=2- => 709 agree
STR : [11=2=] : LETTERS : [(= 2 = 1 1)] : 11=2= => 708 agree
STR : [11=12] : LETTERS : [(2 1 = 1 1)] : 11=12 => 707 agree
STR : [11=11] : LETTERS : [(1 1 = 1 1)] : 11=11 => 706 agree
STR : [11=10] : LETTERS : [(0 1 = 1 1)] : 11=10 => 705 agree
STR : [11=1-] : LETTERS : [(- 1 = 1 1)] : 11=1- => 704 agree
STR : [11=1=] : LETTERS : [(= 1 = 1 1)] : 11=1= => 703 agree
STR : [11=02] : LETTERS : [(2 0 = 1 1)] : 11=02 => 702 agree
STR : [11=01] : LETTERS : [(1 0 = 1 1)] : 11=01 => 701 agree
STR : [11=00] : LETTERS : [(0 0 = 1 1)] : 11=00 => 700 agree
STR : [11=0-] : LETTERS : [(- 0 = 1 1)] : 11=0- => 699 agree
STR : [11=0=] : LETTERS : [(= 0 = 1 1)] : 11=0= => 698 agree
STR : [11=-2] : LETTERS : [(2 - = 1 1)] : 11=-2 => 697 agree
STR : [11=-1] : LETTERS : [(1 - = 1 1)] : 11=-1 => 696 agree
STR : [11=-0] : LETTERS : [(0 - = 1 1)] : 11=-0 => 695 agree
STR : [11=--] : LETTERS : [(- - = 1 1)] : 11=-- => 694 agree
STR : [11=-=] : LETTERS : [(= - = 1 1)] : 11=-= => 693 agree
STR : [11==2] : LETTERS : [(2 = = 1 1)] : 11==2 => 692 agree
STR : [11==1] : LETTERS : [(1 = = 1 1)] : 11==1 => 691 agree
STR : [11==0] : LETTERS : [(0 = = 1 1)] : 11==0 => 690 agree
STR : [11==-] : LETTERS : [(- = = 1 1)] : 11==- => 689 agree
STR : [11===] : LETTERS : [(= = = 1 1)] : 11=== => 688 agree
STR : [10222] : LETTERS : [(2 2 2 0 1)] : 10222 => 687 agree
STR : [10221] : LETTERS : [(1 2 2 0 1)] : 10221 => 686 agree
STR : [10220] : LETTERS : [(0 2 2 0 1)] : 10220 => 685 agree
STR : [1022-] : LETTERS : [(- 2 2 0 1)] : 1022- => 684 agree
STR : [1022=] : LETTERS : [(= 2 2 0 1)] : 1022= => 683 agree
STR : [10212] : LETTERS : [(2 1 2 0 1)] : 10212 => 682 agree
STR : [10211] : LETTERS : [(1 1 2 0 1)] : 10211 => 681 agree
STR : [10210] : LETTERS : [(0 1 2 0 1)] : 10210 => 680 agree
STR : [1021-] : LETTERS : [(- 1 2 0 1)] : 1021- => 679 agree
STR : [1021=] : LETTERS : [(= 1 2 0 1)] : 1021= => 678 agree
STR : [10202] : LETTERS : [(2 0 2 0 1)] : 10202 => 677 agree
STR : [10201] : LETTERS : [(1 0 2 0 1)] : 10201 => 676 agree
STR : [10200] : LETTERS : [(0 0 2 0 1)] : 10200 => 675 agree
STR : [1020-] : LETTERS : [(- 0 2 0 1)] : 1020- => 674 agree
STR : [1020=] : LETTERS : [(= 0 2 0 1)] : 1020= => 673 agree
STR : [102-2] : LETTERS : [(2 - 2 0 1)] : 102-2 => 672 agree
STR : [102-1] : LETTERS : [(1 - 2 0 1)] : 102-1 => 671 agree
STR : [102-0] : LETTERS : [(0 - 2 0 1)] : 102-0 => 670 agree
STR : [102--] : LETTERS : [(- - 2 0 1)] : 102-- => 669 agree
STR : [102-=] : LETTERS : [(= - 2 0 1)] : 102-= => 668 agree
STR : [102=2] : LETTERS : [(2 = 2 0 1)] : 102=2 => 667 agree
STR : [102=1] : LETTERS : [(1 = 2 0 1)] : 102=1 => 666 agree
STR : [102=0] : LETTERS : [(0 = 2 0 1)] : 102=0 => 665 agree
STR : [102=-] : LETTERS : [(- = 2 0 1)] : 102=- => 664 agree
STR : [102==] : LETTERS : [(= = 2 0 1)] : 102== => 663 agree
STR : [10122] : LETTERS : [(2 2 1 0 1)] : 10122 => 662 agree
STR : [10121] : LETTERS : [(1 2 1 0 1)] : 10121 => 661 agree
STR : [10120] : LETTERS : [(0 2 1 0 1)] : 10120 => 660 agree
STR : [1012-] : LETTERS : [(- 2 1 0 1)] : 1012- => 659 agree
STR : [1012=] : LETTERS : [(= 2 1 0 1)] : 1012= => 658 agree
STR : [10112] : LETTERS : [(2 1 1 0 1)] : 10112 => 657 agree
STR : [10111] : LETTERS : [(1 1 1 0 1)] : 10111 => 656 agree
STR : [10110] : LETTERS : [(0 1 1 0 1)] : 10110 => 655 agree
STR : [1011-] : LETTERS : [(- 1 1 0 1)] : 1011- => 654 agree
STR : [1011=] : LETTERS : [(= 1 1 0 1)] : 1011= => 653 agree
STR : [10102] : LETTERS : [(2 0 1 0 1)] : 10102 => 652 agree
STR : [10101] : LETTERS : [(1 0 1 0 1)] : 10101 => 651 agree
STR : [10100] : LETTERS : [(0 0 1 0 1)] : 10100 => 650 agree
STR : [1010-] : LETTERS : [(- 0 1 0 1)] : 1010- => 649 agree
STR : [1010=] : LETTERS : [(= 0 1 0 1)] : 1010= => 648 agree
STR : [101-2] : LETTERS : [(2 - 1 0 1)] : 101-2 => 647 agree
STR : [101-1] : LETTERS : [(1 - 1 0 1)] : 101-1 => 646 agree
STR : [101-0] : LETTERS : [(0 - 1 0 1)] : 101-0 => 645 agree
STR : [101--] : LETTERS : [(- - 1 0 1)] : 101-- => 644 agree
STR : [101-=] : LETTERS : [(= - 1 0 1)] : 101-= => 643 agree
STR : [101=2] : LETTERS : [(2 = 1 0 1)] : 101=2 => 642 agree
STR : [101=1] : LETTERS : [(1 = 1 0 1)] : 101=1 => 641 agree
STR : [101=0] : LETTERS : [(0 = 1 0 1)] : 101=0 => 640 agree
STR : [101=-] : LETTERS : [(- = 1 0 1)] : 101=- => 639 agree
STR : [101==] : LETTERS : [(= = 1 0 1)] : 101== => 638 agree
STR : [10022] : LETTERS : [(2 2 0 0 1)] : 10022 => 637 agree
STR : [10021] : LETTERS : [(1 2 0 0 1)] : 10021 => 636 agree
STR : [10020] : LETTERS : [(0 2 0 0 1)] : 10020 => 635 agree
STR : [1002-] : LETTERS : [(- 2 0 0 1)] : 1002- => 634 agree
STR : [1002=] : LETTERS : [(= 2 0 0 1)] : 1002= => 633 agree
STR : [10012] : LETTERS : [(2 1 0 0 1)] : 10012 => 632 agree
STR : [10011] : LETTERS : [(1 1 0 0 1)] : 10011 => 631 agree
STR : [10010] : LETTERS : [(0 1 0 0 1)] : 10010 => 630 agree
STR : [1001-] : LETTERS : [(- 1 0 0 1)] : 1001- => 629 agree
STR : [1001=] : LETTERS : [(= 1 0 0 1)] : 1001= => 628 agree
STR : [10002] : LETTERS : [(2 0 0 0 1)] : 10002 => 627 agree
STR : [10001] : LETTERS : [(1 0 0 0 1)] : 10001 => 626 agree
STR : [10000] : LETTERS : [(0 0 0 0 1)] : 10000 => 625 agree
STR : [1000-] : LETTERS : [(- 0 0 0 1)] : 1000- => 624 agree
STR : [1000=] : LETTERS : [(= 0 0 0 1)] : 1000= => 623 agree
STR : [100-2] : LETTERS : [(2 - 0 0 1)] : 100-2 => 622 agree
STR : [100-1] : LETTERS : [(1 - 0 0 1)] : 100-1 => 621 agree
STR : [100-0] : LETTERS : [(0 - 0 0 1)] : 100-0 => 620 agree
STR : [100--] : LETTERS : [(- - 0 0 1)] : 100-- => 619 agree
STR : [100-=] : LETTERS : [(= - 0 0 1)] : 100-= => 618 agree
STR : [100=2] : LETTERS : [(2 = 0 0 1)] : 100=2 => 617 agree
STR : [100=1] : LETTERS : [(1 = 0 0 1)] : 100=1 => 616 agree
STR : [100=0] : LETTERS : [(0 = 0 0 1)] : 100=0 => 615 agree
STR : [100=-] : LETTERS : [(- = 0 0 1)] : 100=- => 614 agree
STR : [100==] : LETTERS : [(= = 0 0 1)] : 100== => 613 agree
STR : [10-22] : LETTERS : [(2 2 - 0 1)] : 10-22 => 612 agree
STR : [10-21] : LETTERS : [(1 2 - 0 1)] : 10-21 => 611 agree
STR : [10-20] : LETTERS : [(0 2 - 0 1)] : 10-20 => 610 agree
STR : [10-2-] : LETTERS : [(- 2 - 0 1)] : 10-2- => 609 agree
STR : [10-2=] : LETTERS : [(= 2 - 0 1)] : 10-2= => 608 agree
STR : [10-12] : LETTERS : [(2 1 - 0 1)] : 10-12 => 607 agree
STR : [10-11] : LETTERS : [(1 1 - 0 1)] : 10-11 => 606 agree
STR : [10-10] : LETTERS : [(0 1 - 0 1)] : 10-10 => 605 agree
STR : [10-1-] : LETTERS : [(- 1 - 0 1)] : 10-1- => 604 agree
STR : [10-1=] : LETTERS : [(= 1 - 0 1)] : 10-1= => 603 agree
STR : [10-02] : LETTERS : [(2 0 - 0 1)] : 10-02 => 602 agree
STR : [10-01] : LETTERS : [(1 0 - 0 1)] : 10-01 => 601 agree
STR : [10-00] : LETTERS : [(0 0 - 0 1)] : 10-00 => 600 agree
STR : [10-0-] : LETTERS : [(- 0 - 0 1)] : 10-0- => 599 agree
STR : [10-0=] : LETTERS : [(= 0 - 0 1)] : 10-0= => 598 agree
STR : [10--2] : LETTERS : [(2 - - 0 1)] : 10--2 => 597 agree
STR : [10--1] : LETTERS : [(1 - - 0 1)] : 10--1 => 596 agree
STR : [10--0] : LETTERS : [(0 - - 0 1)] : 10--0 => 595 agree
STR : [10---] : LETTERS : [(- - - 0 1)] : 10--- => 594 agree
STR : [10--=] : LETTERS : [(= - - 0 1)] : 10--= => 593 agree
STR : [10-=2] : LETTERS : [(2 = - 0 1)] : 10-=2 => 592 agree
STR : [10-=1] : LETTERS : [(1 = - 0 1)] : 10-=1 => 591 agree
STR : [10-=0] : LETTERS : [(0 = - 0 1)] : 10-=0 => 590 agree
STR : [10-=-] : LETTERS : [(- = - 0 1)] : 10-=- => 589 agree
STR : [10-==] : LETTERS : [(= = - 0 1)] : 10-== => 588 agree
STR : [10=22] : LETTERS : [(2 2 = 0 1)] : 10=22 => 587 agree
STR : [10=21] : LETTERS : [(1 2 = 0 1)] : 10=21 => 586 agree
STR : [10=20] : LETTERS : [(0 2 = 0 1)] : 10=20 => 585 agree
STR : [10=2-] : LETTERS : [(- 2 = 0 1)] : 10=2- => 584 agree
STR : [10=2=] : LETTERS : [(= 2 = 0 1)] : 10=2= => 583 agree
STR : [10=12] : LETTERS : [(2 1 = 0 1)] : 10=12 => 582 agree
STR : [10=11] : LETTERS : [(1 1 = 0 1)] : 10=11 => 581 agree
STR : [10=10] : LETTERS : [(0 1 = 0 1)] : 10=10 => 580 agree
STR : [10=1-] : LETTERS : [(- 1 = 0 1)] : 10=1- => 579 agree
STR : [10=1=] : LETTERS : [(= 1 = 0 1)] : 10=1= => 578 agree
STR : [10=02] : LETTERS : [(2 0 = 0 1)] : 10=02 => 577 agree
STR : [10=01] : LETTERS : [(1 0 = 0 1)] : 10=01 => 576 agree
STR : [10=00] : LETTERS : [(0 0 = 0 1)] : 10=00 => 575 agree
STR : [10=0-] : LETTERS : [(- 0 = 0 1)] : 10=0- => 574 agree
STR : [10=0=] : LETTERS : [(= 0 = 0 1)] : 10=0= => 573 agree
STR : [10=-2] : LETTERS : [(2 - = 0 1)] : 10=-2 => 572 agree
STR : [10=-1] : LETTERS : [(1 - = 0 1)] : 10=-1 => 571 agree
STR : [10=-0] : LETTERS : [(0 - = 0 1)] : 10=-0 => 570 agree
STR : [10=--] : LETTERS : [(- - = 0 1)] : 10=-- => 569 agree
STR : [10=-=] : LETTERS : [(= - = 0 1)] : 10=-= => 568 agree
STR : [10==2] : LETTERS : [(2 = = 0 1)] : 10==2 => 567 agree
STR : [10==1] : LETTERS : [(1 = = 0 1)] : 10==1 => 566 agree
STR : [10==0] : LETTERS : [(0 = = 0 1)] : 10==0 => 565 agree
STR : [10==-] : LETTERS : [(- = = 0 1)] : 10==- => 564 agree
STR : [10===] : LETTERS : [(= = = 0 1)] : 10=== => 563 agree
STR : [1-222] : LETTERS : [(2 2 2 - 1)] : 1-222 => 562 agree
STR : [1-221] : LETTERS : [(1 2 2 - 1)] : 1-221 => 561 agree
STR : [1-220] : LETTERS : [(0 2 2 - 1)] : 1-220 => 560 agree
STR : [1-22-] : LETTERS : [(- 2 2 - 1)] : 1-22- => 559 agree
STR : [1-22=] : LETTERS : [(= 2 2 - 1)] : 1-22= => 558 agree
STR : [1-212] : LETTERS : [(2 1 2 - 1)] : 1-212 => 557 agree
STR : [1-211] : LETTERS : [(1 1 2 - 1)] : 1-211 => 556 agree
STR : [1-210] : LETTERS : [(0 1 2 - 1)] : 1-210 => 555 agree
STR : [1-21-] : LETTERS : [(- 1 2 - 1)] : 1-21- => 554 agree
STR : [1-21=] : LETTERS : [(= 1 2 - 1)] : 1-21= => 553 agree
STR : [1-202] : LETTERS : [(2 0 2 - 1)] : 1-202 => 552 agree
STR : [1-201] : LETTERS : [(1 0 2 - 1)] : 1-201 => 551 agree
STR : [1-200] : LETTERS : [(0 0 2 - 1)] : 1-200 => 550 agree
STR : [1-20-] : LETTERS : [(- 0 2 - 1)] : 1-20- => 549 agree
STR : [1-20=] : LETTERS : [(= 0 2 - 1)] : 1-20= => 548 agree
STR : [1-2-2] : LETTERS : [(2 - 2 - 1)] : 1-2-2 => 547 agree
STR : [1-2-1] : LETTERS : [(1 - 2 - 1)] : 1-2-1 => 546 agree
STR : [1-2-0] : LETTERS : [(0 - 2 - 1)] : 1-2-0 => 545 agree
STR : [1-2--] : LETTERS : [(- - 2 - 1)] : 1-2-- => 544 agree
STR : [1-2-=] : LETTERS : [(= - 2 - 1)] : 1-2-= => 543 agree
STR : [1-2=2] : LETTERS : [(2 = 2 - 1)] : 1-2=2 => 542 agree
STR : [1-2=1] : LETTERS : [(1 = 2 - 1)] : 1-2=1 => 541 agree
STR : [1-2=0] : LETTERS : [(0 = 2 - 1)] : 1-2=0 => 540 agree
STR : [1-2=-] : LETTERS : [(- = 2 - 1)] : 1-2=- => 539 agree
STR : [1-2==] : LETTERS : [(= = 2 - 1)] : 1-2== => 538 agree
STR : [1-122] : LETTERS : [(2 2 1 - 1)] : 1-122 => 537 agree
STR : [1-121] : LETTERS : [(1 2 1 - 1)] : 1-121 => 536 agree
STR : [1-120] : LETTERS : [(0 2 1 - 1)] : 1-120 => 535 agree
STR : [1-12-] : LETTERS : [(- 2 1 - 1)] : 1-12- => 534 agree
STR : [1-12=] : LETTERS : [(= 2 1 - 1)] : 1-12= => 533 agree
STR : [1-112] : LETTERS : [(2 1 1 - 1)] : 1-112 => 532 agree
STR : [1-111] : LETTERS : [(1 1 1 - 1)] : 1-111 => 531 agree
STR : [1-110] : LETTERS : [(0 1 1 - 1)] : 1-110 => 530 agree
STR : [1-11-] : LETTERS : [(- 1 1 - 1)] : 1-11- => 529 agree
STR : [1-11=] : LETTERS : [(= 1 1 - 1)] : 1-11= => 528 agree
STR : [1-102] : LETTERS : [(2 0 1 - 1)] : 1-102 => 527 agree
STR : [1-101] : LETTERS : [(1 0 1 - 1)] : 1-101 => 526 agree
STR : [1-100] : LETTERS : [(0 0 1 - 1)] : 1-100 => 525 agree
STR : [1-10-] : LETTERS : [(- 0 1 - 1)] : 1-10- => 524 agree
STR : [1-10=] : LETTERS : [(= 0 1 - 1)] : 1-10= => 523 agree
STR : [1-1-2] : LETTERS : [(2 - 1 - 1)] : 1-1-2 => 522 agree
STR : [1-1-1] : LETTERS : [(1 - 1 - 1)] : 1-1-1 => 521 agree
STR : [1-1-0] : LETTERS : [(0 - 1 - 1)] : 1-1-0 => 520 agree
STR : [1-1--] : LETTERS : [(- - 1 - 1)] : 1-1-- => 519 agree
STR : [1-1-=] : LETTERS : [(= - 1 - 1)] : 1-1-= => 518 agree
STR : [1-1=2] : LETTERS : [(2 = 1 - 1)] : 1-1=2 => 517 agree
STR : [1-1=1] : LETTERS : [(1 = 1 - 1)] : 1-1=1 => 516 agree
STR : [1-1=0] : LETTERS : [(0 = 1 - 1)] : 1-1=0 => 515 agree
STR : [1-1=-] : LETTERS : [(- = 1 - 1)] : 1-1=- => 514 agree
STR : [1-1==] : LETTERS : [(= = 1 - 1)] : 1-1== => 513 agree
STR : [1-022] : LETTERS : [(2 2 0 - 1)] : 1-022 => 512 agree
STR : [1-021] : LETTERS : [(1 2 0 - 1)] : 1-021 => 511 agree
STR : [1-020] : LETTERS : [(0 2 0 - 1)] : 1-020 => 510 agree
STR : [1-02-] : LETTERS : [(- 2 0 - 1)] : 1-02- => 509 agree
STR : [1-02=] : LETTERS : [(= 2 0 - 1)] : 1-02= => 508 agree
STR : [1-012] : LETTERS : [(2 1 0 - 1)] : 1-012 => 507 agree
STR : [1-011] : LETTERS : [(1 1 0 - 1)] : 1-011 => 506 agree
STR : [1-010] : LETTERS : [(0 1 0 - 1)] : 1-010 => 505 agree
STR : [1-01-] : LETTERS : [(- 1 0 - 1)] : 1-01- => 504 agree
STR : [1-01=] : LETTERS : [(= 1 0 - 1)] : 1-01= => 503 agree
STR : [1-002] : LETTERS : [(2 0 0 - 1)] : 1-002 => 502 agree
STR : [1-001] : LETTERS : [(1 0 0 - 1)] : 1-001 => 501 agree
STR : [1-000] : LETTERS : [(0 0 0 - 1)] : 1-000 => 500 agree
STR : [1-00-] : LETTERS : [(- 0 0 - 1)] : 1-00- => 499 agree
STR : [1-00=] : LETTERS : [(= 0 0 - 1)] : 1-00= => 498 agree
STR : [1-0-2] : LETTERS : [(2 - 0 - 1)] : 1-0-2 => 497 agree
STR : [1-0-1] : LETTERS : [(1 - 0 - 1)] : 1-0-1 => 496 agree
STR : [1-0-0] : LETTERS : [(0 - 0 - 1)] : 1-0-0 => 495 agree
STR : [1-0--] : LETTERS : [(- - 0 - 1)] : 1-0-- => 494 agree
STR : [1-0-=] : LETTERS : [(= - 0 - 1)] : 1-0-= => 493 agree
STR : [1-0=2] : LETTERS : [(2 = 0 - 1)] : 1-0=2 => 492 agree
STR : [1-0=1] : LETTERS : [(1 = 0 - 1)] : 1-0=1 => 491 agree
STR : [1-0=0] : LETTERS : [(0 = 0 - 1)] : 1-0=0 => 490 agree
STR : [1-0=-] : LETTERS : [(- = 0 - 1)] : 1-0=- => 489 agree
STR : [1-0==] : LETTERS : [(= = 0 - 1)] : 1-0== => 488 agree
STR : [1--22] : LETTERS : [(2 2 - - 1)] : 1--22 => 487 agree
STR : [1--21] : LETTERS : [(1 2 - - 1)] : 1--21 => 486 agree
STR : [1--20] : LETTERS : [(0 2 - - 1)] : 1--20 => 485 agree
STR : [1--2-] : LETTERS : [(- 2 - - 1)] : 1--2- => 484 agree
STR : [1--2=] : LETTERS : [(= 2 - - 1)] : 1--2= => 483 agree
STR : [1--12] : LETTERS : [(2 1 - - 1)] : 1--12 => 482 agree
STR : [1--11] : LETTERS : [(1 1 - - 1)] : 1--11 => 481 agree
STR : [1--10] : LETTERS : [(0 1 - - 1)] : 1--10 => 480 agree
STR : [1--1-] : LETTERS : [(- 1 - - 1)] : 1--1- => 479 agree
STR : [1--1=] : LETTERS : [(= 1 - - 1)] : 1--1= => 478 agree
STR : [1--02] : LETTERS : [(2 0 - - 1)] : 1--02 => 477 agree
STR : [1--01] : LETTERS : [(1 0 - - 1)] : 1--01 => 476 agree
STR : [1--00] : LETTERS : [(0 0 - - 1)] : 1--00 => 475 agree
STR : [1--0-] : LETTERS : [(- 0 - - 1)] : 1--0- => 474 agree
STR : [1--0=] : LETTERS : [(= 0 - - 1)] : 1--0= => 473 agree
STR : [1---2] : LETTERS : [(2 - - - 1)] : 1---2 => 472 agree
STR : [1---1] : LETTERS : [(1 - - - 1)] : 1---1 => 471 agree
STR : [1---0] : LETTERS : [(0 - - - 1)] : 1---0 => 470 agree
STR : [1----] : LETTERS : [(- - - - 1)] : 1---- => 469 agree
STR : [1---=] : LETTERS : [(= - - - 1)] : 1---= => 468 agree
STR : [1--=2] : LETTERS : [(2 = - - 1)] : 1--=2 => 467 agree
STR : [1--=1] : LETTERS : [(1 = - - 1)] : 1--=1 => 466 agree
STR : [1--=0] : LETTERS : [(0 = - - 1)] : 1--=0 => 465 agree
STR : [1--=-] : LETTERS : [(- = - - 1)] : 1--=- => 464 agree
STR : [1--==] : LETTERS : [(= = - - 1)] : 1--== => 463 agree
STR : [1-=22] : LETTERS : [(2 2 = - 1)] : 1-=22 => 462 agree
STR : [1-=21] : LETTERS : [(1 2 = - 1)] : 1-=21 => 461 agree
STR : [1-=20] : LETTERS : [(0 2 = - 1)] : 1-=20 => 460 agree
STR : [1-=2-] : LETTERS : [(- 2 = - 1)] : 1-=2- => 459 agree
STR : [1-=2=] : LETTERS : [(= 2 = - 1)] : 1-=2= => 458 agree
STR : [1-=12] : LETTERS : [(2 1 = - 1)] : 1-=12 => 457 agree
STR : [1-=11] : LETTERS : [(1 1 = - 1)] : 1-=11 => 456 agree
STR : [1-=10] : LETTERS : [(0 1 = - 1)] : 1-=10 => 455 agree
STR : [1-=1-] : LETTERS : [(- 1 = - 1)] : 1-=1- => 454 agree
STR : [1-=1=] : LETTERS : [(= 1 = - 1)] : 1-=1= => 453 agree
STR : [1-=02] : LETTERS : [(2 0 = - 1)] : 1-=02 => 452 agree
STR : [1-=01] : LETTERS : [(1 0 = - 1)] : 1-=01 => 451 agree
STR : [1-=00] : LETTERS : [(0 0 = - 1)] : 1-=00 => 450 agree
STR : [1-=0-] : LETTERS : [(- 0 = - 1)] : 1-=0- => 449 agree
STR : [1-=0=] : LETTERS : [(= 0 = - 1)] : 1-=0= => 448 agree
STR : [1-=-2] : LETTERS : [(2 - = - 1)] : 1-=-2 => 447 agree
STR : [1-=-1] : LETTERS : [(1 - = - 1)] : 1-=-1 => 446 agree
STR : [1-=-0] : LETTERS : [(0 - = - 1)] : 1-=-0 => 445 agree
STR : [1-=--] : LETTERS : [(- - = - 1)] : 1-=-- => 444 agree
STR : [1-=-=] : LETTERS : [(= - = - 1)] : 1-=-= => 443 agree
STR : [1-==2] : LETTERS : [(2 = = - 1)] : 1-==2 => 442 agree
STR : [1-==1] : LETTERS : [(1 = = - 1)] : 1-==1 => 441 agree
STR : [1-==0] : LETTERS : [(0 = = - 1)] : 1-==0 => 440 agree
STR : [1-==-] : LETTERS : [(- = = - 1)] : 1-==- => 439 agree
STR : [1-===] : LETTERS : [(= = = - 1)] : 1-=== => 438 agree
STR : [1=222] : LETTERS : [(2 2 2 = 1)] : 1=222 => 437 agree
STR : [1=221] : LETTERS : [(1 2 2 = 1)] : 1=221 => 436 agree
STR : [1=220] : LETTERS : [(0 2 2 = 1)] : 1=220 => 435 agree
STR : [1=22-] : LETTERS : [(- 2 2 = 1)] : 1=22- => 434 agree
STR : [1=22=] : LETTERS : [(= 2 2 = 1)] : 1=22= => 433 agree
STR : [1=212] : LETTERS : [(2 1 2 = 1)] : 1=212 => 432 agree
STR : [1=211] : LETTERS : [(1 1 2 = 1)] : 1=211 => 431 agree
STR : [1=210] : LETTERS : [(0 1 2 = 1)] : 1=210 => 430 agree
STR : [1=21-] : LETTERS : [(- 1 2 = 1)] : 1=21- => 429 agree
STR : [1=21=] : LETTERS : [(= 1 2 = 1)] : 1=21= => 428 agree
STR : [1=202] : LETTERS : [(2 0 2 = 1)] : 1=202 => 427 agree
STR : [1=201] : LETTERS : [(1 0 2 = 1)] : 1=201 => 426 agree
STR : [1=200] : LETTERS : [(0 0 2 = 1)] : 1=200 => 425 agree
STR : [1=20-] : LETTERS : [(- 0 2 = 1)] : 1=20- => 424 agree
STR : [1=20=] : LETTERS : [(= 0 2 = 1)] : 1=20= => 423 agree
STR : [1=2-2] : LETTERS : [(2 - 2 = 1)] : 1=2-2 => 422 agree
STR : [1=2-1] : LETTERS : [(1 - 2 = 1)] : 1=2-1 => 421 agree
STR : [1=2-0] : LETTERS : [(0 - 2 = 1)] : 1=2-0 => 420 agree
STR : [1=2--] : LETTERS : [(- - 2 = 1)] : 1=2-- => 419 agree
STR : [1=2-=] : LETTERS : [(= - 2 = 1)] : 1=2-= => 418 agree
STR : [1=2=2] : LETTERS : [(2 = 2 = 1)] : 1=2=2 => 417 agree
STR : [1=2=1] : LETTERS : [(1 = 2 = 1)] : 1=2=1 => 416 agree
STR : [1=2=0] : LETTERS : [(0 = 2 = 1)] : 1=2=0 => 415 agree
STR : [1=2=-] : LETTERS : [(- = 2 = 1)] : 1=2=- => 414 agree
STR : [1=2==] : LETTERS : [(= = 2 = 1)] : 1=2== => 413 agree
STR : [1=122] : LETTERS : [(2 2 1 = 1)] : 1=122 => 412 agree
STR : [1=121] : LETTERS : [(1 2 1 = 1)] : 1=121 => 411 agree
STR : [1=120] : LETTERS : [(0 2 1 = 1)] : 1=120 => 410 agree
STR : [1=12-] : LETTERS : [(- 2 1 = 1)] : 1=12- => 409 agree
STR : [1=12=] : LETTERS : [(= 2 1 = 1)] : 1=12= => 408 agree
STR : [1=112] : LETTERS : [(2 1 1 = 1)] : 1=112 => 407 agree
STR : [1=111] : LETTERS : [(1 1 1 = 1)] : 1=111 => 406 agree
STR : [1=110] : LETTERS : [(0 1 1 = 1)] : 1=110 => 405 agree
STR : [1=11-] : LETTERS : [(- 1 1 = 1)] : 1=11- => 404 agree
STR : [1=11=] : LETTERS : [(= 1 1 = 1)] : 1=11= => 403 agree
STR : [1=102] : LETTERS : [(2 0 1 = 1)] : 1=102 => 402 agree
STR : [1=101] : LETTERS : [(1 0 1 = 1)] : 1=101 => 401 agree
STR : [1=100] : LETTERS : [(0 0 1 = 1)] : 1=100 => 400 agree
STR : [1=10-] : LETTERS : [(- 0 1 = 1)] : 1=10- => 399 agree
STR : [1=10=] : LETTERS : [(= 0 1 = 1)] : 1=10= => 398 agree
STR : [1=1-2] : LETTERS : [(2 - 1 = 1)] : 1=1-2 => 397 agree
STR : [1=1-1] : LETTERS : [(1 - 1 = 1)] : 1=1-1 => 396 agree
STR : [1=1-0] : LETTERS : [(0 - 1 = 1)] : 1=1-0 => 395 agree
STR : [1=1--] : LETTERS : [(- - 1 = 1)] : 1=1-- => 394 agree
STR : [1=1-=] : LETTERS : [(= - 1 = 1)] : 1=1-= => 393 agree
STR : [1=1=2] : LETTERS : [(2 = 1 = 1)] : 1=1=2 => 392 agree
STR : [1=1=1] : LETTERS : [(1 = 1 = 1)] : 1=1=1 => 391 agree
STR : [1=1=0] : LETTERS : [(0 = 1 = 1)] : 1=1=0 => 390 agree
STR : [1=1=-] : LETTERS : [(- = 1 = 1)] : 1=1=- => 389 agree
STR : [1=1==] : LETTERS : [(= = 1 = 1)] : 1=1== => 388 agree
STR : [1=022] : LETTERS : [(2 2 0 = 1)] : 1=022 => 387 agree
STR : [1=021] : LETTERS : [(1 2 0 = 1)] : 1=021 => 386 agree
STR : [1=020] : LETTERS : [(0 2 0 = 1)] : 1=020 => 385 agree
STR : [1=02-] : LETTERS : [(- 2 0 = 1)] : 1=02- => 384 agree
STR : [1=02=] : LETTERS : [(= 2 0 = 1)] : 1=02= => 383 agree
STR : [1=012] : LETTERS : [(2 1 0 = 1)] : 1=012 => 382 agree
STR : [1=011] : LETTERS : [(1 1 0 = 1)] : 1=011 => 381 agree
STR : [1=010] : LETTERS : [(0 1 0 = 1)] : 1=010 => 380 agree
STR : [1=01-] : LETTERS : [(- 1 0 = 1)] : 1=01- => 379 agree
STR : [1=01=] : LETTERS : [(= 1 0 = 1)] : 1=01= => 378 agree
STR : [1=002] : LETTERS : [(2 0 0 = 1)] : 1=002 => 377 agree
STR : [1=001] : LETTERS : [(1 0 0 = 1)] : 1=001 => 376 agree
STR : [1=000] : LETTERS : [(0 0 0 = 1)] : 1=000 => 375 agree
STR : [1=00-] : LETTERS : [(- 0 0 = 1)] : 1=00- => 374 agree
STR : [1=00=] : LETTERS : [(= 0 0 = 1)] : 1=00= => 373 agree
STR : [1=0-2] : LETTERS : [(2 - 0 = 1)] : 1=0-2 => 372 agree
STR : [1=0-1] : LETTERS : [(1 - 0 = 1)] : 1=0-1 => 371 agree
STR : [1=0-0] : LETTERS : [(0 - 0 = 1)] : 1=0-0 => 370 agree
STR : [1=0--] : LETTERS : [(- - 0 = 1)] : 1=0-- => 369 agree
STR : [1=0-=] : LETTERS : [(= - 0 = 1)] : 1=0-= => 368 agree
STR : [1=0=2] : LETTERS : [(2 = 0 = 1)] : 1=0=2 => 367 agree
STR : [1=0=1] : LETTERS : [(1 = 0 = 1)] : 1=0=1 => 366 agree
STR : [1=0=0] : LETTERS : [(0 = 0 = 1)] : 1=0=0 => 365 agree
STR : [1=0=-] : LETTERS : [(- = 0 = 1)] : 1=0=- => 364 agree
STR : [1=0==] : LETTERS : [(= = 0 = 1)] : 1=0== => 363 agree
STR : [1=-22] : LETTERS : [(2 2 - = 1)] : 1=-22 => 362 agree
STR : [1=-21] : LETTERS : [(1 2 - = 1)] : 1=-21 => 361 agree
STR : [1=-20] : LETTERS : [(0 2 - = 1)] : 1=-20 => 360 agree
STR : [1=-2-] : LETTERS : [(- 2 - = 1)] : 1=-2- => 359 agree
STR : [1=-2=] : LETTERS : [(= 2 - = 1)] : 1=-2= => 358 agree
STR : [1=-12] : LETTERS : [(2 1 - = 1)] : 1=-12 => 357 agree
STR : [1=-11] : LETTERS : [(1 1 - = 1)] : 1=-11 => 356 agree
STR : [1=-10] : LETTERS : [(0 1 - = 1)] : 1=-10 => 355 agree
STR : [1=-1-] : LETTERS : [(- 1 - = 1)] : 1=-1- => 354 agree
STR : [1=-1=] : LETTERS : [(= 1 - = 1)] : 1=-1= => 353 agree
STR : [1=-02] : LETTERS : [(2 0 - = 1)] : 1=-02 => 352 agree
STR : [1=-01] : LETTERS : [(1 0 - = 1)] : 1=-01 => 351 agree
STR : [1=-00] : LETTERS : [(0 0 - = 1)] : 1=-00 => 350 agree
STR : [1=-0-] : LETTERS : [(- 0 - = 1)] : 1=-0- => 349 agree
STR : [1=-0=] : LETTERS : [(= 0 - = 1)] : 1=-0= => 348 agree
STR : [1=--2] : LETTERS : [(2 - - = 1)] : 1=--2 => 347 agree
STR : [1=--1] : LETTERS : [(1 - - = 1)] : 1=--1 => 346 agree
STR : [1=--0] : LETTERS : [(0 - - = 1)] : 1=--0 => 345 agree
STR : [1=---] : LETTERS : [(- - - = 1)] : 1=--- => 344 agree
STR : [1=--=] : LETTERS : [(= - - = 1)] : 1=--= => 343 agree
STR : [1=-=2] : LETTERS : [(2 = - = 1)] : 1=-=2 => 342 agree
STR : [1=-=1] : LETTERS : [(1 = - = 1)] : 1=-=1 => 341 agree
STR : [1=-=0] : LETTERS : [(0 = - = 1)] : 1=-=0 => 340 agree
STR : [1=-=-] : LETTERS : [(- = - = 1)] : 1=-=- => 339 agree
STR : [1=-==] : LETTERS : [(= = - = 1)] : 1=-== => 338 agree
STR : [1==22] : LETTERS : [(2 2 = = 1)] : 1==22 => 337 agree
STR : [1==21] : LETTERS : [(1 2 = = 1)] : 1==21 => 336 agree
STR : [1==20] : LETTERS : [(0 2 = = 1)] : 1==20 => 335 agree
STR : [1==2-] : LETTERS : [(- 2 = = 1)] : 1==2- => 334 agree
STR : [1==2=] : LETTERS : [(= 2 = = 1)] : 1==2= => 333 agree
STR : [1==12] : LETTERS : [(2 1 = = 1)] : 1==12 => 332 agree
STR : [1==11] : LETTERS : [(1 1 = = 1)] : 1==11 => 331 agree
STR : [1==10] : LETTERS : [(0 1 = = 1)] : 1==10 => 330 agree
STR : [1==1-] : LETTERS : [(- 1 = = 1)] : 1==1- => 329 agree
STR : [1==1=] : LETTERS : [(= 1 = = 1)] : 1==1= => 328 agree
STR : [1==02] : LETTERS : [(2 0 = = 1)] : 1==02 => 327 agree
STR : [1==01] : LETTERS : [(1 0 = = 1)] : 1==01 => 326 agree
STR : [1==00] : LETTERS : [(0 0 = = 1)] : 1==00 => 325 agree
STR : [1==0-] : LETTERS : [(- 0 = = 1)] : 1==0- => 324 agree
STR : [1==0=] : LETTERS : [(= 0 = = 1)] : 1==0= => 323 agree
STR : [1==-2] : LETTERS : [(2 - = = 1)] : 1==-2 => 322 agree
STR : [1==-1] : LETTERS : [(1 - = = 1)] : 1==-1 => 321 agree
STR : [1==-0] : LETTERS : [(0 - = = 1)] : 1==-0 => 320 agree
STR : [1==--] : LETTERS : [(- - = = 1)] : 1==-- => 319 agree
STR : [1==-=] : LETTERS : [(= - = = 1)] : 1==-= => 318 agree
STR : [1===2] : LETTERS : [(2 = = = 1)] : 1===2 => 317 agree
STR : [1===1] : LETTERS : [(1 = = = 1)] : 1===1 => 316 agree
STR : [1===0] : LETTERS : [(0 = = = 1)] : 1===0 => 315 agree
STR : [1===-] : LETTERS : [(- = = = 1)] : 1===- => 314 agree
STR : [1====] : LETTERS : [(= = = = 1)] : 1==== => 313 agree
STR : [02222] : LETTERS : [(2 2 2 2 0)] : 02222 => 312 agree
STR : [02221] : LETTERS : [(1 2 2 2 0)] : 02221 => 311 agree
STR : [02220] : LETTERS : [(0 2 2 2 0)] : 02220 => 310 agree
STR : [0222-] : LETTERS : [(- 2 2 2 0)] : 0222- => 309 agree
STR : [0222=] : LETTERS : [(= 2 2 2 0)] : 0222= => 308 agree
STR : [02212] : LETTERS : [(2 1 2 2 0)] : 02212 => 307 agree
STR : [02211] : LETTERS : [(1 1 2 2 0)] : 02211 => 306 agree
STR : [02210] : LETTERS : [(0 1 2 2 0)] : 02210 => 305 agree
STR : [0221-] : LETTERS : [(- 1 2 2 0)] : 0221- => 304 agree
STR : [0221=] : LETTERS : [(= 1 2 2 0)] : 0221= => 303 agree
STR : [02202] : LETTERS : [(2 0 2 2 0)] : 02202 => 302 agree
STR : [02201] : LETTERS : [(1 0 2 2 0)] : 02201 => 301 agree
STR : [02200] : LETTERS : [(0 0 2 2 0)] : 02200 => 300 agree
STR : [0220-] : LETTERS : [(- 0 2 2 0)] : 0220- => 299 agree
STR : [0220=] : LETTERS : [(= 0 2 2 0)] : 0220= => 298 agree
STR : [022-2] : LETTERS : [(2 - 2 2 0)] : 022-2 => 297 agree
STR : [022-1] : LETTERS : [(1 - 2 2 0)] : 022-1 => 296 agree
STR : [022-0] : LETTERS : [(0 - 2 2 0)] : 022-0 => 295 agree
STR : [022--] : LETTERS : [(- - 2 2 0)] : 022-- => 294 agree
STR : [022-=] : LETTERS : [(= - 2 2 0)] : 022-= => 293 agree
STR : [022=2] : LETTERS : [(2 = 2 2 0)] : 022=2 => 292 agree
STR : [022=1] : LETTERS : [(1 = 2 2 0)] : 022=1 => 291 agree
STR : [022=0] : LETTERS : [(0 = 2 2 0)] : 022=0 => 290 agree
STR : [022=-] : LETTERS : [(- = 2 2 0)] : 022=- => 289 agree
STR : [022==] : LETTERS : [(= = 2 2 0)] : 022== => 288 agree
STR : [02122] : LETTERS : [(2 2 1 2 0)] : 02122 => 287 agree
STR : [02121] : LETTERS : [(1 2 1 2 0)] : 02121 => 286 agree
STR : [02120] : LETTERS : [(0 2 1 2 0)] : 02120 => 285 agree
STR : [0212-] : LETTERS : [(- 2 1 2 0)] : 0212- => 284 agree
STR : [0212=] : LETTERS : [(= 2 1 2 0)] : 0212= => 283 agree
STR : [02112] : LETTERS : [(2 1 1 2 0)] : 02112 => 282 agree
STR : [02111] : LETTERS : [(1 1 1 2 0)] : 02111 => 281 agree
STR : [02110] : LETTERS : [(0 1 1 2 0)] : 02110 => 280 agree
STR : [0211-] : LETTERS : [(- 1 1 2 0)] : 0211- => 279 agree
STR : [0211=] : LETTERS : [(= 1 1 2 0)] : 0211= => 278 agree
STR : [02102] : LETTERS : [(2 0 1 2 0)] : 02102 => 277 agree
STR : [02101] : LETTERS : [(1 0 1 2 0)] : 02101 => 276 agree
STR : [02100] : LETTERS : [(0 0 1 2 0)] : 02100 => 275 agree
STR : [0210-] : LETTERS : [(- 0 1 2 0)] : 0210- => 274 agree
STR : [0210=] : LETTERS : [(= 0 1 2 0)] : 0210= => 273 agree
STR : [021-2] : LETTERS : [(2 - 1 2 0)] : 021-2 => 272 agree
STR : [021-1] : LETTERS : [(1 - 1 2 0)] : 021-1 => 271 agree
STR : [021-0] : LETTERS : [(0 - 1 2 0)] : 021-0 => 270 agree
STR : [021--] : LETTERS : [(- - 1 2 0)] : 021-- => 269 agree
STR : [021-=] : LETTERS : [(= - 1 2 0)] : 021-= => 268 agree
STR : [021=2] : LETTERS : [(2 = 1 2 0)] : 021=2 => 267 agree
STR : [021=1] : LETTERS : [(1 = 1 2 0)] : 021=1 => 266 agree
STR : [021=0] : LETTERS : [(0 = 1 2 0)] : 021=0 => 265 agree
STR : [021=-] : LETTERS : [(- = 1 2 0)] : 021=- => 264 agree
STR : [021==] : LETTERS : [(= = 1 2 0)] : 021== => 263 agree
STR : [02022] : LETTERS : [(2 2 0 2 0)] : 02022 => 262 agree
STR : [02021] : LETTERS : [(1 2 0 2 0)] : 02021 => 261 agree
STR : [02020] : LETTERS : [(0 2 0 2 0)] : 02020 => 260 agree
STR : [0202-] : LETTERS : [(- 2 0 2 0)] : 0202- => 259 agree
STR : [0202=] : LETTERS : [(= 2 0 2 0)] : 0202= => 258 agree
STR : [02012] : LETTERS : [(2 1 0 2 0)] : 02012 => 257 agree
STR : [02011] : LETTERS : [(1 1 0 2 0)] : 02011 => 256 agree
STR : [02010] : LETTERS : [(0 1 0 2 0)] : 02010 => 255 agree
STR : [0201-] : LETTERS : [(- 1 0 2 0)] : 0201- => 254 agree
STR : [0201=] : LETTERS : [(= 1 0 2 0)] : 0201= => 253 agree
STR : [02002] : LETTERS : [(2 0 0 2 0)] : 02002 => 252 agree
STR : [02001] : LETTERS : [(1 0 0 2 0)] : 02001 => 251 agree
STR : [02000] : LETTERS : [(0 0 0 2 0)] : 02000 => 250 agree
STR : [0200-] : LETTERS : [(- 0 0 2 0)] : 0200- => 249 agree
STR : [0200=] : LETTERS : [(= 0 0 2 0)] : 0200= => 248 agree
STR : [020-2] : LETTERS : [(2 - 0 2 0)] : 020-2 => 247 agree
STR : [020-1] : LETTERS : [(1 - 0 2 0)] : 020-1 => 246 agree
STR : [020-0] : LETTERS : [(0 - 0 2 0)] : 020-0 => 245 agree
STR : [020--] : LETTERS : [(- - 0 2 0)] : 020-- => 244 agree
STR : [020-=] : LETTERS : [(= - 0 2 0)] : 020-= => 243 agree
STR : [020=2] : LETTERS : [(2 = 0 2 0)] : 020=2 => 242 agree
STR : [020=1] : LETTERS : [(1 = 0 2 0)] : 020=1 => 241 agree
STR : [020=0] : LETTERS : [(0 = 0 2 0)] : 020=0 => 240 agree
STR : [020=-] : LETTERS : [(- = 0 2 0)] : 020=- => 239 agree
STR : [020==] : LETTERS : [(= = 0 2 0)] : 020== => 238 agree
STR : [02-22] : LETTERS : [(2 2 - 2 0)] : 02-22 => 237 agree
STR : [02-21] : LETTERS : [(1 2 - 2 0)] : 02-21 => 236 agree
STR : [02-20] : LETTERS : [(0 2 - 2 0)] : 02-20 => 235 agree
STR : [02-2-] : LETTERS : [(- 2 - 2 0)] : 02-2- => 234 agree
STR : [02-2=] : LETTERS : [(= 2 - 2 0)] : 02-2= => 233 agree
STR : [02-12] : LETTERS : [(2 1 - 2 0)] : 02-12 => 232 agree
STR : [02-11] : LETTERS : [(1 1 - 2 0)] : 02-11 => 231 agree
STR : [02-10] : LETTERS : [(0 1 - 2 0)] : 02-10 => 230 agree
STR : [02-1-] : LETTERS : [(- 1 - 2 0)] : 02-1- => 229 agree
STR : [02-1=] : LETTERS : [(= 1 - 2 0)] : 02-1= => 228 agree
STR : [02-02] : LETTERS : [(2 0 - 2 0)] : 02-02 => 227 agree
STR : [02-01] : LETTERS : [(1 0 - 2 0)] : 02-01 => 226 agree
STR : [02-00] : LETTERS : [(0 0 - 2 0)] : 02-00 => 225 agree
STR : [02-0-] : LETTERS : [(- 0 - 2 0)] : 02-0- => 224 agree
STR : [02-0=] : LETTERS : [(= 0 - 2 0)] : 02-0= => 223 agree
STR : [02--2] : LETTERS : [(2 - - 2 0)] : 02--2 => 222 agree
STR : [02--1] : LETTERS : [(1 - - 2 0)] : 02--1 => 221 agree
STR : [02--0] : LETTERS : [(0 - - 2 0)] : 02--0 => 220 agree
STR : [02---] : LETTERS : [(- - - 2 0)] : 02--- => 219 agree
STR : [02--=] : LETTERS : [(= - - 2 0)] : 02--= => 218 agree
STR : [02-=2] : LETTERS : [(2 = - 2 0)] : 02-=2 => 217 agree
STR : [02-=1] : LETTERS : [(1 = - 2 0)] : 02-=1 => 216 agree
STR : [02-=0] : LETTERS : [(0 = - 2 0)] : 02-=0 => 215 agree
STR : [02-=-] : LETTERS : [(- = - 2 0)] : 02-=- => 214 agree
STR : [02-==] : LETTERS : [(= = - 2 0)] : 02-== => 213 agree
STR : [02=22] : LETTERS : [(2 2 = 2 0)] : 02=22 => 212 agree
STR : [02=21] : LETTERS : [(1 2 = 2 0)] : 02=21 => 211 agree
STR : [02=20] : LETTERS : [(0 2 = 2 0)] : 02=20 => 210 agree
STR : [02=2-] : LETTERS : [(- 2 = 2 0)] : 02=2- => 209 agree
STR : [02=2=] : LETTERS : [(= 2 = 2 0)] : 02=2= => 208 agree
STR : [02=12] : LETTERS : [(2 1 = 2 0)] : 02=12 => 207 agree
STR : [02=11] : LETTERS : [(1 1 = 2 0)] : 02=11 => 206 agree
STR : [02=10] : LETTERS : [(0 1 = 2 0)] : 02=10 => 205 agree
STR : [02=1-] : LETTERS : [(- 1 = 2 0)] : 02=1- => 204 agree
STR : [02=1=] : LETTERS : [(= 1 = 2 0)] : 02=1= => 203 agree
STR : [02=02] : LETTERS : [(2 0 = 2 0)] : 02=02 => 202 agree
STR : [02=01] : LETTERS : [(1 0 = 2 0)] : 02=01 => 201 agree
STR : [02=00] : LETTERS : [(0 0 = 2 0)] : 02=00 => 200 agree
STR : [02=0-] : LETTERS : [(- 0 = 2 0)] : 02=0- => 199 agree
STR : [02=0=] : LETTERS : [(= 0 = 2 0)] : 02=0= => 198 agree
STR : [02=-2] : LETTERS : [(2 - = 2 0)] : 02=-2 => 197 agree
STR : [02=-1] : LETTERS : [(1 - = 2 0)] : 02=-1 => 196 agree
STR : [02=-0] : LETTERS : [(0 - = 2 0)] : 02=-0 => 195 agree
STR : [02=--] : LETTERS : [(- - = 2 0)] : 02=-- => 194 agree
STR : [02=-=] : LETTERS : [(= - = 2 0)] : 02=-= => 193 agree
STR : [02==2] : LETTERS : [(2 = = 2 0)] : 02==2 => 192 agree
STR : [02==1] : LETTERS : [(1 = = 2 0)] : 02==1 => 191 agree
STR : [02==0] : LETTERS : [(0 = = 2 0)] : 02==0 => 190 agree
STR : [02==-] : LETTERS : [(- = = 2 0)] : 02==- => 189 agree
STR : [02===] : LETTERS : [(= = = 2 0)] : 02=== => 188 agree
STR : [01222] : LETTERS : [(2 2 2 1 0)] : 01222 => 187 agree
STR : [01221] : LETTERS : [(1 2 2 1 0)] : 01221 => 186 agree
STR : [01220] : LETTERS : [(0 2 2 1 0)] : 01220 => 185 agree
STR : [0122-] : LETTERS : [(- 2 2 1 0)] : 0122- => 184 agree
STR : [0122=] : LETTERS : [(= 2 2 1 0)] : 0122= => 183 agree
STR : [01212] : LETTERS : [(2 1 2 1 0)] : 01212 => 182 agree
STR : [01211] : LETTERS : [(1 1 2 1 0)] : 01211 => 181 agree
STR : [01210] : LETTERS : [(0 1 2 1 0)] : 01210 => 180 agree
STR : [0121-] : LETTERS : [(- 1 2 1 0)] : 0121- => 179 agree
STR : [0121=] : LETTERS : [(= 1 2 1 0)] : 0121= => 178 agree
STR : [01202] : LETTERS : [(2 0 2 1 0)] : 01202 => 177 agree
STR : [01201] : LETTERS : [(1 0 2 1 0)] : 01201 => 176 agree
STR : [01200] : LETTERS : [(0 0 2 1 0)] : 01200 => 175 agree
STR : [0120-] : LETTERS : [(- 0 2 1 0)] : 0120- => 174 agree
STR : [0120=] : LETTERS : [(= 0 2 1 0)] : 0120= => 173 agree
STR : [012-2] : LETTERS : [(2 - 2 1 0)] : 012-2 => 172 agree
STR : [012-1] : LETTERS : [(1 - 2 1 0)] : 012-1 => 171 agree
STR : [012-0] : LETTERS : [(0 - 2 1 0)] : 012-0 => 170 agree
STR : [012--] : LETTERS : [(- - 2 1 0)] : 012-- => 169 agree
STR : [012-=] : LETTERS : [(= - 2 1 0)] : 012-= => 168 agree
STR : [012=2] : LETTERS : [(2 = 2 1 0)] : 012=2 => 167 agree
STR : [012=1] : LETTERS : [(1 = 2 1 0)] : 012=1 => 166 agree
STR : [012=0] : LETTERS : [(0 = 2 1 0)] : 012=0 => 165 agree
STR : [012=-] : LETTERS : [(- = 2 1 0)] : 012=- => 164 agree
STR : [012==] : LETTERS : [(= = 2 1 0)] : 012== => 163 agree
STR : [01122] : LETTERS : [(2 2 1 1 0)] : 01122 => 162 agree
STR : [01121] : LETTERS : [(1 2 1 1 0)] : 01121 => 161 agree
STR : [01120] : LETTERS : [(0 2 1 1 0)] : 01120 => 160 agree
STR : [0112-] : LETTERS : [(- 2 1 1 0)] : 0112- => 159 agree
STR : [0112=] : LETTERS : [(= 2 1 1 0)] : 0112= => 158 agree
STR : [01112] : LETTERS : [(2 1 1 1 0)] : 01112 => 157 agree
STR : [01111] : LETTERS : [(1 1 1 1 0)] : 01111 => 156 agree
STR : [01110] : LETTERS : [(0 1 1 1 0)] : 01110 => 155 agree
STR : [0111-] : LETTERS : [(- 1 1 1 0)] : 0111- => 154 agree
STR : [0111=] : LETTERS : [(= 1 1 1 0)] : 0111= => 153 agree
STR : [01102] : LETTERS : [(2 0 1 1 0)] : 01102 => 152 agree
STR : [01101] : LETTERS : [(1 0 1 1 0)] : 01101 => 151 agree
STR : [01100] : LETTERS : [(0 0 1 1 0)] : 01100 => 150 agree
STR : [0110-] : LETTERS : [(- 0 1 1 0)] : 0110- => 149 agree
STR : [0110=] : LETTERS : [(= 0 1 1 0)] : 0110= => 148 agree
STR : [011-2] : LETTERS : [(2 - 1 1 0)] : 011-2 => 147 agree
STR : [011-1] : LETTERS : [(1 - 1 1 0)] : 011-1 => 146 agree
STR : [011-0] : LETTERS : [(0 - 1 1 0)] : 011-0 => 145 agree
STR : [011--] : LETTERS : [(- - 1 1 0)] : 011-- => 144 agree
STR : [011-=] : LETTERS : [(= - 1 1 0)] : 011-= => 143 agree
STR : [011=2] : LETTERS : [(2 = 1 1 0)] : 011=2 => 142 agree
STR : [011=1] : LETTERS : [(1 = 1 1 0)] : 011=1 => 141 agree
STR : [011=0] : LETTERS : [(0 = 1 1 0)] : 011=0 => 140 agree
STR : [011=-] : LETTERS : [(- = 1 1 0)] : 011=- => 139 agree
STR : [011==] : LETTERS : [(= = 1 1 0)] : 011== => 138 agree
STR : [01022] : LETTERS : [(2 2 0 1 0)] : 01022 => 137 agree
STR : [01021] : LETTERS : [(1 2 0 1 0)] : 01021 => 136 agree
STR : [01020] : LETTERS : [(0 2 0 1 0)] : 01020 => 135 agree
STR : [0102-] : LETTERS : [(- 2 0 1 0)] : 0102- => 134 agree
STR : [0102=] : LETTERS : [(= 2 0 1 0)] : 0102= => 133 agree
STR : [01012] : LETTERS : [(2 1 0 1 0)] : 01012 => 132 agree
STR : [01011] : LETTERS : [(1 1 0 1 0)] : 01011 => 131 agree
STR : [01010] : LETTERS : [(0 1 0 1 0)] : 01010 => 130 agree
STR : [0101-] : LETTERS : [(- 1 0 1 0)] : 0101- => 129 agree
STR : [0101=] : LETTERS : [(= 1 0 1 0)] : 0101= => 128 agree
STR : [01002] : LETTERS : [(2 0 0 1 0)] : 01002 => 127 agree
STR : [01001] : LETTERS : [(1 0 0 1 0)] : 01001 => 126 agree
STR : [01000] : LETTERS : [(0 0 0 1 0)] : 01000 => 125 agree
STR : [0100-] : LETTERS : [(- 0 0 1 0)] : 0100- => 124 agree
STR : [0100=] : LETTERS : [(= 0 0 1 0)] : 0100= => 123 agree
STR : [010-2] : LETTERS : [(2 - 0 1 0)] : 010-2 => 122 agree
STR : [010-1] : LETTERS : [(1 - 0 1 0)] : 010-1 => 121 agree
STR : [010-0] : LETTERS : [(0 - 0 1 0)] : 010-0 => 120 agree
STR : [010--] : LETTERS : [(- - 0 1 0)] : 010-- => 119 agree
STR : [010-=] : LETTERS : [(= - 0 1 0)] : 010-= => 118 agree
STR : [010=2] : LETTERS : [(2 = 0 1 0)] : 010=2 => 117 agree
STR : [010=1] : LETTERS : [(1 = 0 1 0)] : 010=1 => 116 agree
STR : [010=0] : LETTERS : [(0 = 0 1 0)] : 010=0 => 115 agree
STR : [010=-] : LETTERS : [(- = 0 1 0)] : 010=- => 114 agree
STR : [010==] : LETTERS : [(= = 0 1 0)] : 010== => 113 agree
STR : [01-22] : LETTERS : [(2 2 - 1 0)] : 01-22 => 112 agree
STR : [01-21] : LETTERS : [(1 2 - 1 0)] : 01-21 => 111 agree
STR : [01-20] : LETTERS : [(0 2 - 1 0)] : 01-20 => 110 agree
STR : [01-2-] : LETTERS : [(- 2 - 1 0)] : 01-2- => 109 agree
STR : [01-2=] : LETTERS : [(= 2 - 1 0)] : 01-2= => 108 agree
STR : [01-12] : LETTERS : [(2 1 - 1 0)] : 01-12 => 107 agree
STR : [01-11] : LETTERS : [(1 1 - 1 0)] : 01-11 => 106 agree
STR : [01-10] : LETTERS : [(0 1 - 1 0)] : 01-10 => 105 agree
STR : [01-1-] : LETTERS : [(- 1 - 1 0)] : 01-1- => 104 agree
STR : [01-1=] : LETTERS : [(= 1 - 1 0)] : 01-1= => 103 agree
STR : [01-02] : LETTERS : [(2 0 - 1 0)] : 01-02 => 102 agree
STR : [01-01] : LETTERS : [(1 0 - 1 0)] : 01-01 => 101 agree
STR : [01-00] : LETTERS : [(0 0 - 1 0)] : 01-00 => 100 agree
STR : [01-0-] : LETTERS : [(- 0 - 1 0)] : 01-0- => 99 agree
STR : [01-0=] : LETTERS : [(= 0 - 1 0)] : 01-0= => 98 agree
STR : [01--2] : LETTERS : [(2 - - 1 0)] : 01--2 => 97 agree
STR : [01--1] : LETTERS : [(1 - - 1 0)] : 01--1 => 96 agree
STR : [01--0] : LETTERS : [(0 - - 1 0)] : 01--0 => 95 agree
STR : [01---] : LETTERS : [(- - - 1 0)] : 01--- => 94 agree
STR : [01--=] : LETTERS : [(= - - 1 0)] : 01--= => 93 agree
STR : [01-=2] : LETTERS : [(2 = - 1 0)] : 01-=2 => 92 agree
STR : [01-=1] : LETTERS : [(1 = - 1 0)] : 01-=1 => 91 agree
STR : [01-=0] : LETTERS : [(0 = - 1 0)] : 01-=0 => 90 agree
STR : [01-=-] : LETTERS : [(- = - 1 0)] : 01-=- => 89 agree
STR : [01-==] : LETTERS : [(= = - 1 0)] : 01-== => 88 agree
STR : [01=22] : LETTERS : [(2 2 = 1 0)] : 01=22 => 87 agree   <<< here is our 87 
STR : [01=21] : LETTERS : [(1 2 = 1 0)] : 01=21 => 86 agree
STR : [01=20] : LETTERS : [(0 2 = 1 0)] : 01=20 => 85 agree
STR : [01=2-] : LETTERS : [(- 2 = 1 0)] : 01=2- => 84 agree
STR : [01=2=] : LETTERS : [(= 2 = 1 0)] : 01=2= => 83 agree
STR : [01=12] : LETTERS : [(2 1 = 1 0)] : 01=12 => 82 agree
STR : [01=11] : LETTERS : [(1 1 = 1 0)] : 01=11 => 81 agree
STR : [01=10] : LETTERS : [(0 1 = 1 0)] : 01=10 => 80 agree
STR : [01=1-] : LETTERS : [(- 1 = 1 0)] : 01=1- => 79 agree
STR : [01=1=] : LETTERS : [(= 1 = 1 0)] : 01=1= => 78 agree
STR : [01=02] : LETTERS : [(2 0 = 1 0)] : 01=02 => 77 agree
STR : [01=01] : LETTERS : [(1 0 = 1 0)] : 01=01 => 76 agree
STR : [01=00] : LETTERS : [(0 0 = 1 0)] : 01=00 => 75 agree
STR : [01=0-] : LETTERS : [(- 0 = 1 0)] : 01=0- => 74 agree
STR : [01=0=] : LETTERS : [(= 0 = 1 0)] : 01=0= => 73 agree
STR : [01=-2] : LETTERS : [(2 - = 1 0)] : 01=-2 => 72 agree
STR : [01=-1] : LETTERS : [(1 - = 1 0)] : 01=-1 => 71 agree
STR : [01=-0] : LETTERS : [(0 - = 1 0)] : 01=-0 => 70 agree
STR : [01=--] : LETTERS : [(- - = 1 0)] : 01=-- => 69 agree
STR : [01=-=] : LETTERS : [(= - = 1 0)] : 01=-= => 68 agree
STR : [01==2] : LETTERS : [(2 = = 1 0)] : 01==2 => 67 agree
STR : [01==1] : LETTERS : [(1 = = 1 0)] : 01==1 => 66 agree
STR : [01==0] : LETTERS : [(0 = = 1 0)] : 01==0 => 65 agree
STR : [01==-] : LETTERS : [(- = = 1 0)] : 01==- => 64 agree
STR : [01===] : LETTERS : [(= = = 1 0)] : 01=== => 63 agree
STR : [00222] : LETTERS : [(2 2 2 0 0)] : 00222 => 62 agree
STR : [00221] : LETTERS : [(1 2 2 0 0)] : 00221 => 61 agree
STR : [00220] : LETTERS : [(0 2 2 0 0)] : 00220 => 60 agree
STR : [0022-] : LETTERS : [(- 2 2 0 0)] : 0022- => 59 agree
STR : [0022=] : LETTERS : [(= 2 2 0 0)] : 0022= => 58 agree
STR : [00212] : LETTERS : [(2 1 2 0 0)] : 00212 => 57 agree
STR : [00211] : LETTERS : [(1 1 2 0 0)] : 00211 => 56 agree
STR : [00210] : LETTERS : [(0 1 2 0 0)] : 00210 => 55 agree
STR : [0021-] : LETTERS : [(- 1 2 0 0)] : 0021- => 54 agree
STR : [0021=] : LETTERS : [(= 1 2 0 0)] : 0021= => 53 agree
STR : [00202] : LETTERS : [(2 0 2 0 0)] : 00202 => 52 agree
STR : [00201] : LETTERS : [(1 0 2 0 0)] : 00201 => 51 agree
STR : [00200] : LETTERS : [(0 0 2 0 0)] : 00200 => 50 agree
STR : [0020-] : LETTERS : [(- 0 2 0 0)] : 0020- => 49 agree
STR : [0020=] : LETTERS : [(= 0 2 0 0)] : 0020= => 48 agree
STR : [002-2] : LETTERS : [(2 - 2 0 0)] : 002-2 => 47 agree
STR : [002-1] : LETTERS : [(1 - 2 0 0)] : 002-1 => 46 agree
STR : [002-0] : LETTERS : [(0 - 2 0 0)] : 002-0 => 45 agree
STR : [002--] : LETTERS : [(- - 2 0 0)] : 002-- => 44 agree
STR : [002-=] : LETTERS : [(= - 2 0 0)] : 002-= => 43 agree
STR : [002=2] : LETTERS : [(2 = 2 0 0)] : 002=2 => 42 agree
STR : [002=1] : LETTERS : [(1 = 2 0 0)] : 002=1 => 41 agree
STR : [002=0] : LETTERS : [(0 = 2 0 0)] : 002=0 => 40 agree
STR : [002=-] : LETTERS : [(- = 2 0 0)] : 002=- => 39 agree
STR : [002==] : LETTERS : [(= = 2 0 0)] : 002== => 38 agree
STR : [00122] : LETTERS : [(2 2 1 0 0)] : 00122 => 37 agree
STR : [00121] : LETTERS : [(1 2 1 0 0)] : 00121 => 36 agree
STR : [00120] : LETTERS : [(0 2 1 0 0)] : 00120 => 35 agree
STR : [0012-] : LETTERS : [(- 2 1 0 0)] : 0012- => 34 agree
STR : [0012=] : LETTERS : [(= 2 1 0 0)] : 0012= => 33 agree
STR : [00112] : LETTERS : [(2 1 1 0 0)] : 00112 => 32 agree
STR : [00111] : LETTERS : [(1 1 1 0 0)] : 00111 => 31 agree
STR : [00110] : LETTERS : [(0 1 1 0 0)] : 00110 => 30 agree
STR : [0011-] : LETTERS : [(- 1 1 0 0)] : 0011- => 29 agree
STR : [0011=] : LETTERS : [(= 1 1 0 0)] : 0011= => 28 agree
STR : [00102] : LETTERS : [(2 0 1 0 0)] : 00102 => 27 agree
STR : [00101] : LETTERS : [(1 0 1 0 0)] : 00101 => 26 agree
STR : [00100] : LETTERS : [(0 0 1 0 0)] : 00100 => 25 agree
STR : [0010-] : LETTERS : [(- 0 1 0 0)] : 0010- => 24 agree
STR : [0010=] : LETTERS : [(= 0 1 0 0)] : 0010= => 23 agree
STR : [001-2] : LETTERS : [(2 - 1 0 0)] : 001-2 => 22 agree
STR : [001-1] : LETTERS : [(1 - 1 0 0)] : 001-1 => 21 agree
STR : [001-0] : LETTERS : [(0 - 1 0 0)] : 001-0 => 20 agree
STR : [001--] : LETTERS : [(- - 1 0 0)] : 001-- => 19 agree
STR : [001-=] : LETTERS : [(= - 1 0 0)] : 001-= => 18 agree
STR : [001=2] : LETTERS : [(2 = 1 0 0)] : 001=2 => 17 agree
STR : [001=1] : LETTERS : [(1 = 1 0 0)] : 001=1 => 16 agree
STR : [001=0] : LETTERS : [(0 = 1 0 0)] : 001=0 => 15 agree
STR : [001=-] : LETTERS : [(- = 1 0 0)] : 001=- => 14 agree
STR : [001==] : LETTERS : [(= = 1 0 0)] : 001== => 13 agree
STR : [00022] : LETTERS : [(2 2 0 0 0)] : 00022 => 12 agree
STR : [00021] : LETTERS : [(1 2 0 0 0)] : 00021 => 11 agree
STR : [00020] : LETTERS : [(0 2 0 0 0)] : 00020 => 10 agree
STR : [0002-] : LETTERS : [(- 2 0 0 0)] : 0002- => 9 agree
STR : [0002=] : LETTERS : [(= 2 0 0 0)] : 0002= => 8 agree
STR : [00012] : LETTERS : [(2 1 0 0 0)] : 00012 => 7 agree
STR : [00011] : LETTERS : [(1 1 0 0 0)] : 00011 => 6 agree
STR : [00010] : LETTERS : [(0 1 0 0 0)] : 00010 => 5 agree
STR : [0001-] : LETTERS : [(- 1 0 0 0)] : 0001- => 4 agree
STR : [0001=] : LETTERS : [(= 1 0 0 0)] : 0001= => 3 agree
STR : [00002] : LETTERS : [(2 0 0 0 0)] : 00002 => 2 agree
STR : [00001] : LETTERS : [(1 0 0 0 0)] : 00001 => 1 agree
STR : [00000] : LETTERS : [(0 0 0 0 0)] : 00000 => 0 agree
STR : [0000-] : LETTERS : [(- 0 0 0 0)] : 0000- => -1 agree
STR : [0000=] : LETTERS : [(= 0 0 0 0)] : 0000= => -2 agree
STR : [000-2] : LETTERS : [(2 - 0 0 0)] : 000-2 => -3 agree
STR : [000-1] : LETTERS : [(1 - 0 0 0)] : 000-1 => -4 agree
STR : [000-0] : LETTERS : [(0 - 0 0 0)] : 000-0 => -5 agree
STR : [000--] : LETTERS : [(- - 0 0 0)] : 000-- => -6 agree
STR : [000-=] : LETTERS : [(= - 0 0 0)] : 000-= => -7 agree
STR : [000=2] : LETTERS : [(2 = 0 0 0)] : 000=2 => -8 agree
STR : [000=1] : LETTERS : [(1 = 0 0 0)] : 000=1 => -9 agree
STR : [000=0] : LETTERS : [(0 = 0 0 0)] : 000=0 => -10 agree
STR : [000=-] : LETTERS : [(- = 0 0 0)] : 000=- => -11 agree
STR : [000==] : LETTERS : [(= = 0 0 0)] : 000== => -12 agree
STR : [00-22] : LETTERS : [(2 2 - 0 0)] : 00-22 => -13 agree
STR : [00-21] : LETTERS : [(1 2 - 0 0)] : 00-21 => -14 agree
STR : [00-20] : LETTERS : [(0 2 - 0 0)] : 00-20 => -15 agree
STR : [00-2-] : LETTERS : [(- 2 - 0 0)] : 00-2- => -16 agree
STR : [00-2=] : LETTERS : [(= 2 - 0 0)] : 00-2= => -17 agree
STR : [00-12] : LETTERS : [(2 1 - 0 0)] : 00-12 => -18 agree
STR : [00-11] : LETTERS : [(1 1 - 0 0)] : 00-11 => -19 agree
STR : [00-10] : LETTERS : [(0 1 - 0 0)] : 00-10 => -20 agree
STR : [00-1-] : LETTERS : [(- 1 - 0 0)] : 00-1- => -21 agree
STR : [00-1=] : LETTERS : [(= 1 - 0 0)] : 00-1= => -22 agree
STR : [00-02] : LETTERS : [(2 0 - 0 0)] : 00-02 => -23 agree
STR : [00-01] : LETTERS : [(1 0 - 0 0)] : 00-01 => -24 agree
STR : [00-00] : LETTERS : [(0 0 - 0 0)] : 00-00 => -25 agree
STR : [00-0-] : LETTERS : [(- 0 - 0 0)] : 00-0- => -26 agree
STR : [00-0=] : LETTERS : [(= 0 - 0 0)] : 00-0= => -27 agree
STR : [00--2] : LETTERS : [(2 - - 0 0)] : 00--2 => -28 agree
STR : [00--1] : LETTERS : [(1 - - 0 0)] : 00--1 => -29 agree
STR : [00--0] : LETTERS : [(0 - - 0 0)] : 00--0 => -30 agree
STR : [00---] : LETTERS : [(- - - 0 0)] : 00--- => -31 agree
STR : [00--=] : LETTERS : [(= - - 0 0)] : 00--= => -32 agree
STR : [00-=2] : LETTERS : [(2 = - 0 0)] : 00-=2 => -33 agree
STR : [00-=1] : LETTERS : [(1 = - 0 0)] : 00-=1 => -34 agree
STR : [00-=0] : LETTERS : [(0 = - 0 0)] : 00-=0 => -35 agree
STR : [00-=-] : LETTERS : [(- = - 0 0)] : 00-=- => -36 agree
STR : [00-==] : LETTERS : [(= = - 0 0)] : 00-== => -37 agree
STR : [00=22] : LETTERS : [(2 2 = 0 0)] : 00=22 => -38 agree
STR : [00=21] : LETTERS : [(1 2 = 0 0)] : 00=21 => -39 agree
STR : [00=20] : LETTERS : [(0 2 = 0 0)] : 00=20 => -40 agree
STR : [00=2-] : LETTERS : [(- 2 = 0 0)] : 00=2- => -41 agree
STR : [00=2=] : LETTERS : [(= 2 = 0 0)] : 00=2= => -42 agree
STR : [00=12] : LETTERS : [(2 1 = 0 0)] : 00=12 => -43 agree
STR : [00=11] : LETTERS : [(1 1 = 0 0)] : 00=11 => -44 agree
STR : [00=10] : LETTERS : [(0 1 = 0 0)] : 00=10 => -45 agree
STR : [00=1-] : LETTERS : [(- 1 = 0 0)] : 00=1- => -46 agree
STR : [00=1=] : LETTERS : [(= 1 = 0 0)] : 00=1= => -47 agree
STR : [00=02] : LETTERS : [(2 0 = 0 0)] : 00=02 => -48 agree
STR : [00=01] : LETTERS : [(1 0 = 0 0)] : 00=01 => -49 agree
STR : [00=00] : LETTERS : [(0 0 = 0 0)] : 00=00 => -50 agree
STR : [00=0-] : LETTERS : [(- 0 = 0 0)] : 00=0- => -51 agree
STR : [00=0=] : LETTERS : [(= 0 = 0 0)] : 00=0= => -52 agree
STR : [00=-2] : LETTERS : [(2 - = 0 0)] : 00=-2 => -53 agree
STR : [00=-1] : LETTERS : [(1 - = 0 0)] : 00=-1 => -54 agree
STR : [00=-0] : LETTERS : [(0 - = 0 0)] : 00=-0 => -55 agree
STR : [00=--] : LETTERS : [(- - = 0 0)] : 00=-- => -56 agree
STR : [00=-=] : LETTERS : [(= - = 0 0)] : 00=-= => -57 agree
STR : [00==2] : LETTERS : [(2 = = 0 0)] : 00==2 => -58 agree
STR : [00==1] : LETTERS : [(1 = = 0 0)] : 00==1 => -59 agree
STR : [00==0] : LETTERS : [(0 = = 0 0)] : 00==0 => -60 agree
STR : [00==-] : LETTERS : [(- = = 0 0)] : 00==- => -61 agree
STR : [00===] : LETTERS : [(= = = 0 0)] : 00=== => -62 agree
STR : [0-222] : LETTERS : [(2 2 2 - 0)] : 0-222 => -63 agree
STR : [0-221] : LETTERS : [(1 2 2 - 0)] : 0-221 => -64 agree
STR : [0-220] : LETTERS : [(0 2 2 - 0)] : 0-220 => -65 agree
STR : [0-22-] : LETTERS : [(- 2 2 - 0)] : 0-22- => -66 agree
STR : [0-22=] : LETTERS : [(= 2 2 - 0)] : 0-22= => -67 agree
STR : [0-212] : LETTERS : [(2 1 2 - 0)] : 0-212 => -68 agree
STR : [0-211] : LETTERS : [(1 1 2 - 0)] : 0-211 => -69 agree
STR : [0-210] : LETTERS : [(0 1 2 - 0)] : 0-210 => -70 agree
STR : [0-21-] : LETTERS : [(- 1 2 - 0)] : 0-21- => -71 agree
STR : [0-21=] : LETTERS : [(= 1 2 - 0)] : 0-21= => -72 agree
STR : [0-202] : LETTERS : [(2 0 2 - 0)] : 0-202 => -73 agree
STR : [0-201] : LETTERS : [(1 0 2 - 0)] : 0-201 => -74 agree
STR : [0-200] : LETTERS : [(0 0 2 - 0)] : 0-200 => -75 agree
STR : [0-20-] : LETTERS : [(- 0 2 - 0)] : 0-20- => -76 agree
STR : [0-20=] : LETTERS : [(= 0 2 - 0)] : 0-20= => -77 agree
STR : [0-2-2] : LETTERS : [(2 - 2 - 0)] : 0-2-2 => -78 agree
STR : [0-2-1] : LETTERS : [(1 - 2 - 0)] : 0-2-1 => -79 agree
STR : [0-2-0] : LETTERS : [(0 - 2 - 0)] : 0-2-0 => -80 agree
STR : [0-2--] : LETTERS : [(- - 2 - 0)] : 0-2-- => -81 agree
STR : [0-2-=] : LETTERS : [(= - 2 - 0)] : 0-2-= => -82 agree
STR : [0-2=2] : LETTERS : [(2 = 2 - 0)] : 0-2=2 => -83 agree
STR : [0-2=1] : LETTERS : [(1 = 2 - 0)] : 0-2=1 => -84 agree
STR : [0-2=0] : LETTERS : [(0 = 2 - 0)] : 0-2=0 => -85 agree
STR : [0-2=-] : LETTERS : [(- = 2 - 0)] : 0-2=- => -86 agree
STR : [0-2==] : LETTERS : [(= = 2 - 0)] : 0-2== => -87 agree
STR : [0-122] : LETTERS : [(2 2 1 - 0)] : 0-122 => -88 agree
STR : [0-121] : LETTERS : [(1 2 1 - 0)] : 0-121 => -89 agree
STR : [0-120] : LETTERS : [(0 2 1 - 0)] : 0-120 => -90 agree
STR : [0-12-] : LETTERS : [(- 2 1 - 0)] : 0-12- => -91 agree
STR : [0-12=] : LETTERS : [(= 2 1 - 0)] : 0-12= => -92 agree
STR : [0-112] : LETTERS : [(2 1 1 - 0)] : 0-112 => -93 agree
STR : [0-111] : LETTERS : [(1 1 1 - 0)] : 0-111 => -94 agree
STR : [0-110] : LETTERS : [(0 1 1 - 0)] : 0-110 => -95 agree
STR : [0-11-] : LETTERS : [(- 1 1 - 0)] : 0-11- => -96 agree
STR : [0-11=] : LETTERS : [(= 1 1 - 0)] : 0-11= => -97 agree
STR : [0-102] : LETTERS : [(2 0 1 - 0)] : 0-102 => -98 agree
STR : [0-101] : LETTERS : [(1 0 1 - 0)] : 0-101 => -99 agree
STR : [0-100] : LETTERS : [(0 0 1 - 0)] : 0-100 => -100 agree
STR : [0-10-] : LETTERS : [(- 0 1 - 0)] : 0-10- => -101 agree
STR : [0-10=] : LETTERS : [(= 0 1 - 0)] : 0-10= => -102 agree
STR : [0-1-2] : LETTERS : [(2 - 1 - 0)] : 0-1-2 => -103 agree
STR : [0-1-1] : LETTERS : [(1 - 1 - 0)] : 0-1-1 => -104 agree
STR : [0-1-0] : LETTERS : [(0 - 1 - 0)] : 0-1-0 => -105 agree
STR : [0-1--] : LETTERS : [(- - 1 - 0)] : 0-1-- => -106 agree
STR : [0-1-=] : LETTERS : [(= - 1 - 0)] : 0-1-= => -107 agree
STR : [0-1=2] : LETTERS : [(2 = 1 - 0)] : 0-1=2 => -108 agree
STR : [0-1=1] : LETTERS : [(1 = 1 - 0)] : 0-1=1 => -109 agree
STR : [0-1=0] : LETTERS : [(0 = 1 - 0)] : 0-1=0 => -110 agree
STR : [0-1=-] : LETTERS : [(- = 1 - 0)] : 0-1=- => -111 agree
STR : [0-1==] : LETTERS : [(= = 1 - 0)] : 0-1== => -112 agree
STR : [0-022] : LETTERS : [(2 2 0 - 0)] : 0-022 => -113 agree
STR : [0-021] : LETTERS : [(1 2 0 - 0)] : 0-021 => -114 agree
STR : [0-020] : LETTERS : [(0 2 0 - 0)] : 0-020 => -115 agree
STR : [0-02-] : LETTERS : [(- 2 0 - 0)] : 0-02- => -116 agree
STR : [0-02=] : LETTERS : [(= 2 0 - 0)] : 0-02= => -117 agree
STR : [0-012] : LETTERS : [(2 1 0 - 0)] : 0-012 => -118 agree
STR : [0-011] : LETTERS : [(1 1 0 - 0)] : 0-011 => -119 agree
STR : [0-010] : LETTERS : [(0 1 0 - 0)] : 0-010 => -120 agree
STR : [0-01-] : LETTERS : [(- 1 0 - 0)] : 0-01- => -121 agree
STR : [0-01=] : LETTERS : [(= 1 0 - 0)] : 0-01= => -122 agree
STR : [0-002] : LETTERS : [(2 0 0 - 0)] : 0-002 => -123 agree
STR : [0-001] : LETTERS : [(1 0 0 - 0)] : 0-001 => -124 agree
STR : [0-000] : LETTERS : [(0 0 0 - 0)] : 0-000 => -125 agree
STR : [0-00-] : LETTERS : [(- 0 0 - 0)] : 0-00- => -126 agree
STR : [0-00=] : LETTERS : [(= 0 0 - 0)] : 0-00= => -127 agree
STR : [0-0-2] : LETTERS : [(2 - 0 - 0)] : 0-0-2 => -128 agree
STR : [0-0-1] : LETTERS : [(1 - 0 - 0)] : 0-0-1 => -129 agree
STR : [0-0-0] : LETTERS : [(0 - 0 - 0)] : 0-0-0 => -130 agree
STR : [0-0--] : LETTERS : [(- - 0 - 0)] : 0-0-- => -131 agree
STR : [0-0-=] : LETTERS : [(= - 0 - 0)] : 0-0-= => -132 agree
STR : [0-0=2] : LETTERS : [(2 = 0 - 0)] : 0-0=2 => -133 agree
STR : [0-0=1] : LETTERS : [(1 = 0 - 0)] : 0-0=1 => -134 agree
STR : [0-0=0] : LETTERS : [(0 = 0 - 0)] : 0-0=0 => -135 agree
STR : [0-0=-] : LETTERS : [(- = 0 - 0)] : 0-0=- => -136 agree
STR : [0-0==] : LETTERS : [(= = 0 - 0)] : 0-0== => -137 agree
STR : [0--22] : LETTERS : [(2 2 - - 0)] : 0--22 => -138 agree
STR : [0--21] : LETTERS : [(1 2 - - 0)] : 0--21 => -139 agree
STR : [0--20] : LETTERS : [(0 2 - - 0)] : 0--20 => -140 agree
STR : [0--2-] : LETTERS : [(- 2 - - 0)] : 0--2- => -141 agree
STR : [0--2=] : LETTERS : [(= 2 - - 0)] : 0--2= => -142 agree
STR : [0--12] : LETTERS : [(2 1 - - 0)] : 0--12 => -143 agree
STR : [0--11] : LETTERS : [(1 1 - - 0)] : 0--11 => -144 agree
STR : [0--10] : LETTERS : [(0 1 - - 0)] : 0--10 => -145 agree
STR : [0--1-] : LETTERS : [(- 1 - - 0)] : 0--1- => -146 agree
STR : [0--1=] : LETTERS : [(= 1 - - 0)] : 0--1= => -147 agree
STR : [0--02] : LETTERS : [(2 0 - - 0)] : 0--02 => -148 agree
STR : [0--01] : LETTERS : [(1 0 - - 0)] : 0--01 => -149 agree
STR : [0--00] : LETTERS : [(0 0 - - 0)] : 0--00 => -150 agree
STR : [0--0-] : LETTERS : [(- 0 - - 0)] : 0--0- => -151 agree
STR : [0--0=] : LETTERS : [(= 0 - - 0)] : 0--0= => -152 agree
STR : [0---2] : LETTERS : [(2 - - - 0)] : 0---2 => -153 agree
STR : [0---1] : LETTERS : [(1 - - - 0)] : 0---1 => -154 agree
STR : [0---0] : LETTERS : [(0 - - - 0)] : 0---0 => -155 agree
STR : [0----] : LETTERS : [(- - - - 0)] : 0---- => -156 agree
STR : [0---=] : LETTERS : [(= - - - 0)] : 0---= => -157 agree
STR : [0--=2] : LETTERS : [(2 = - - 0)] : 0--=2 => -158 agree
STR : [0--=1] : LETTERS : [(1 = - - 0)] : 0--=1 => -159 agree
STR : [0--=0] : LETTERS : [(0 = - - 0)] : 0--=0 => -160 agree
STR : [0--=-] : LETTERS : [(- = - - 0)] : 0--=- => -161 agree
STR : [0--==] : LETTERS : [(= = - - 0)] : 0--== => -162 agree
STR : [0-=22] : LETTERS : [(2 2 = - 0)] : 0-=22 => -163 agree
STR : [0-=21] : LETTERS : [(1 2 = - 0)] : 0-=21 => -164 agree
STR : [0-=20] : LETTERS : [(0 2 = - 0)] : 0-=20 => -165 agree
STR : [0-=2-] : LETTERS : [(- 2 = - 0)] : 0-=2- => -166 agree
STR : [0-=2=] : LETTERS : [(= 2 = - 0)] : 0-=2= => -167 agree
STR : [0-=12] : LETTERS : [(2 1 = - 0)] : 0-=12 => -168 agree
STR : [0-=11] : LETTERS : [(1 1 = - 0)] : 0-=11 => -169 agree
STR : [0-=10] : LETTERS : [(0 1 = - 0)] : 0-=10 => -170 agree
STR : [0-=1-] : LETTERS : [(- 1 = - 0)] : 0-=1- => -171 agree
STR : [0-=1=] : LETTERS : [(= 1 = - 0)] : 0-=1= => -172 agree
STR : [0-=02] : LETTERS : [(2 0 = - 0)] : 0-=02 => -173 agree
STR : [0-=01] : LETTERS : [(1 0 = - 0)] : 0-=01 => -174 agree
STR : [0-=00] : LETTERS : [(0 0 = - 0)] : 0-=00 => -175 agree
STR : [0-=0-] : LETTERS : [(- 0 = - 0)] : 0-=0- => -176 agree
STR : [0-=0=] : LETTERS : [(= 0 = - 0)] : 0-=0= => -177 agree
STR : [0-=-2] : LETTERS : [(2 - = - 0)] : 0-=-2 => -178 agree
STR : [0-=-1] : LETTERS : [(1 - = - 0)] : 0-=-1 => -179 agree
STR : [0-=-0] : LETTERS : [(0 - = - 0)] : 0-=-0 => -180 agree
STR : [0-=--] : LETTERS : [(- - = - 0)] : 0-=-- => -181 agree
STR : [0-=-=] : LETTERS : [(= - = - 0)] : 0-=-= => -182 agree
STR : [0-==2] : LETTERS : [(2 = = - 0)] : 0-==2 => -183 agree
STR : [0-==1] : LETTERS : [(1 = = - 0)] : 0-==1 => -184 agree
STR : [0-==0] : LETTERS : [(0 = = - 0)] : 0-==0 => -185 agree
STR : [0-==-] : LETTERS : [(- = = - 0)] : 0-==- => -186 agree
STR : [0-===] : LETTERS : [(= = = - 0)] : 0-=== => -187 agree
STR : [0=222] : LETTERS : [(2 2 2 = 0)] : 0=222 => -188 agree
STR : [0=221] : LETTERS : [(1 2 2 = 0)] : 0=221 => -189 agree
STR : [0=220] : LETTERS : [(0 2 2 = 0)] : 0=220 => -190 agree
STR : [0=22-] : LETTERS : [(- 2 2 = 0)] : 0=22- => -191 agree
STR : [0=22=] : LETTERS : [(= 2 2 = 0)] : 0=22= => -192 agree
STR : [0=212] : LETTERS : [(2 1 2 = 0)] : 0=212 => -193 agree
STR : [0=211] : LETTERS : [(1 1 2 = 0)] : 0=211 => -194 agree
STR : [0=210] : LETTERS : [(0 1 2 = 0)] : 0=210 => -195 agree
STR : [0=21-] : LETTERS : [(- 1 2 = 0)] : 0=21- => -196 agree
STR : [0=21=] : LETTERS : [(= 1 2 = 0)] : 0=21= => -197 agree
STR : [0=202] : LETTERS : [(2 0 2 = 0)] : 0=202 => -198 agree
STR : [0=201] : LETTERS : [(1 0 2 = 0)] : 0=201 => -199 agree
STR : [0=200] : LETTERS : [(0 0 2 = 0)] : 0=200 => -200 agree
STR : [0=20-] : LETTERS : [(- 0 2 = 0)] : 0=20- => -201 agree
STR : [0=20=] : LETTERS : [(= 0 2 = 0)] : 0=20= => -202 agree
STR : [0=2-2] : LETTERS : [(2 - 2 = 0)] : 0=2-2 => -203 agree
STR : [0=2-1] : LETTERS : [(1 - 2 = 0)] : 0=2-1 => -204 agree
STR : [0=2-0] : LETTERS : [(0 - 2 = 0)] : 0=2-0 => -205 agree
STR : [0=2--] : LETTERS : [(- - 2 = 0)] : 0=2-- => -206 agree
STR : [0=2-=] : LETTERS : [(= - 2 = 0)] : 0=2-= => -207 agree
STR : [0=2=2] : LETTERS : [(2 = 2 = 0)] : 0=2=2 => -208 agree
STR : [0=2=1] : LETTERS : [(1 = 2 = 0)] : 0=2=1 => -209 agree
STR : [0=2=0] : LETTERS : [(0 = 2 = 0)] : 0=2=0 => -210 agree
STR : [0=2=-] : LETTERS : [(- = 2 = 0)] : 0=2=- => -211 agree
STR : [0=2==] : LETTERS : [(= = 2 = 0)] : 0=2== => -212 agree
STR : [0=122] : LETTERS : [(2 2 1 = 0)] : 0=122 => -213 agree
STR : [0=121] : LETTERS : [(1 2 1 = 0)] : 0=121 => -214 agree
STR : [0=120] : LETTERS : [(0 2 1 = 0)] : 0=120 => -215 agree
STR : [0=12-] : LETTERS : [(- 2 1 = 0)] : 0=12- => -216 agree
STR : [0=12=] : LETTERS : [(= 2 1 = 0)] : 0=12= => -217 agree
STR : [0=112] : LETTERS : [(2 1 1 = 0)] : 0=112 => -218 agree
STR : [0=111] : LETTERS : [(1 1 1 = 0)] : 0=111 => -219 agree
STR : [0=110] : LETTERS : [(0 1 1 = 0)] : 0=110 => -220 agree
STR : [0=11-] : LETTERS : [(- 1 1 = 0)] : 0=11- => -221 agree
STR : [0=11=] : LETTERS : [(= 1 1 = 0)] : 0=11= => -222 agree
STR : [0=102] : LETTERS : [(2 0 1 = 0)] : 0=102 => -223 agree
STR : [0=101] : LETTERS : [(1 0 1 = 0)] : 0=101 => -224 agree
STR : [0=100] : LETTERS : [(0 0 1 = 0)] : 0=100 => -225 agree
STR : [0=10-] : LETTERS : [(- 0 1 = 0)] : 0=10- => -226 agree
STR : [0=10=] : LETTERS : [(= 0 1 = 0)] : 0=10= => -227 agree
STR : [0=1-2] : LETTERS : [(2 - 1 = 0)] : 0=1-2 => -228 agree
STR : [0=1-1] : LETTERS : [(1 - 1 = 0)] : 0=1-1 => -229 agree
STR : [0=1-0] : LETTERS : [(0 - 1 = 0)] : 0=1-0 => -230 agree
STR : [0=1--] : LETTERS : [(- - 1 = 0)] : 0=1-- => -231 agree
STR : [0=1-=] : LETTERS : [(= - 1 = 0)] : 0=1-= => -232 agree
STR : [0=1=2] : LETTERS : [(2 = 1 = 0)] : 0=1=2 => -233 agree
STR : [0=1=1] : LETTERS : [(1 = 1 = 0)] : 0=1=1 => -234 agree
STR : [0=1=0] : LETTERS : [(0 = 1 = 0)] : 0=1=0 => -235 agree
STR : [0=1=-] : LETTERS : [(- = 1 = 0)] : 0=1=- => -236 agree
STR : [0=1==] : LETTERS : [(= = 1 = 0)] : 0=1== => -237 agree
STR : [0=022] : LETTERS : [(2 2 0 = 0)] : 0=022 => -238 agree
STR : [0=021] : LETTERS : [(1 2 0 = 0)] : 0=021 => -239 agree
STR : [0=020] : LETTERS : [(0 2 0 = 0)] : 0=020 => -240 agree
STR : [0=02-] : LETTERS : [(- 2 0 = 0)] : 0=02- => -241 agree
STR : [0=02=] : LETTERS : [(= 2 0 = 0)] : 0=02= => -242 agree
STR : [0=012] : LETTERS : [(2 1 0 = 0)] : 0=012 => -243 agree
STR : [0=011] : LETTERS : [(1 1 0 = 0)] : 0=011 => -244 agree
STR : [0=010] : LETTERS : [(0 1 0 = 0)] : 0=010 => -245 agree
STR : [0=01-] : LETTERS : [(- 1 0 = 0)] : 0=01- => -246 agree
STR : [0=01=] : LETTERS : [(= 1 0 = 0)] : 0=01= => -247 agree
STR : [0=002] : LETTERS : [(2 0 0 = 0)] : 0=002 => -248 agree
STR : [0=001] : LETTERS : [(1 0 0 = 0)] : 0=001 => -249 agree
STR : [0=000] : LETTERS : [(0 0 0 = 0)] : 0=000 => -250 agree
STR : [0=00-] : LETTERS : [(- 0 0 = 0)] : 0=00- => -251 agree
STR : [0=00=] : LETTERS : [(= 0 0 = 0)] : 0=00= => -252 agree
STR : [0=0-2] : LETTERS : [(2 - 0 = 0)] : 0=0-2 => -253 agree
STR : [0=0-1] : LETTERS : [(1 - 0 = 0)] : 0=0-1 => -254 agree
STR : [0=0-0] : LETTERS : [(0 - 0 = 0)] : 0=0-0 => -255 agree
STR : [0=0--] : LETTERS : [(- - 0 = 0)] : 0=0-- => -256 agree
STR : [0=0-=] : LETTERS : [(= - 0 = 0)] : 0=0-= => -257 agree
STR : [0=0=2] : LETTERS : [(2 = 0 = 0)] : 0=0=2 => -258 agree
STR : [0=0=1] : LETTERS : [(1 = 0 = 0)] : 0=0=1 => -259 agree
STR : [0=0=0] : LETTERS : [(0 = 0 = 0)] : 0=0=0 => -260 agree
STR : [0=0=-] : LETTERS : [(- = 0 = 0)] : 0=0=- => -261 agree
STR : [0=0==] : LETTERS : [(= = 0 = 0)] : 0=0== => -262 agree
STR : [0=-22] : LETTERS : [(2 2 - = 0)] : 0=-22 => -263 agree
STR : [0=-21] : LETTERS : [(1 2 - = 0)] : 0=-21 => -264 agree
STR : [0=-20] : LETTERS : [(0 2 - = 0)] : 0=-20 => -265 agree
STR : [0=-2-] : LETTERS : [(- 2 - = 0)] : 0=-2- => -266 agree
STR : [0=-2=] : LETTERS : [(= 2 - = 0)] : 0=-2= => -267 agree
STR : [0=-12] : LETTERS : [(2 1 - = 0)] : 0=-12 => -268 agree
STR : [0=-11] : LETTERS : [(1 1 - = 0)] : 0=-11 => -269 agree
STR : [0=-10] : LETTERS : [(0 1 - = 0)] : 0=-10 => -270 agree
STR : [0=-1-] : LETTERS : [(- 1 - = 0)] : 0=-1- => -271 agree
STR : [0=-1=] : LETTERS : [(= 1 - = 0)] : 0=-1= => -272 agree
STR : [0=-02] : LETTERS : [(2 0 - = 0)] : 0=-02 => -273 agree
STR : [0=-01] : LETTERS : [(1 0 - = 0)] : 0=-01 => -274 agree
STR : [0=-00] : LETTERS : [(0 0 - = 0)] : 0=-00 => -275 agree
STR : [0=-0-] : LETTERS : [(- 0 - = 0)] : 0=-0- => -276 agree
STR : [0=-0=] : LETTERS : [(= 0 - = 0)] : 0=-0= => -277 agree
STR : [0=--2] : LETTERS : [(2 - - = 0)] : 0=--2 => -278 agree
STR : [0=--1] : LETTERS : [(1 - - = 0)] : 0=--1 => -279 agree
STR : [0=--0] : LETTERS : [(0 - - = 0)] : 0=--0 => -280 agree
STR : [0=---] : LETTERS : [(- - - = 0)] : 0=--- => -281 agree
STR : [0=--=] : LETTERS : [(= - - = 0)] : 0=--= => -282 agree
STR : [0=-=2] : LETTERS : [(2 = - = 0)] : 0=-=2 => -283 agree
STR : [0=-=1] : LETTERS : [(1 = - = 0)] : 0=-=1 => -284 agree
STR : [0=-=0] : LETTERS : [(0 = - = 0)] : 0=-=0 => -285 agree
STR : [0=-=-] : LETTERS : [(- = - = 0)] : 0=-=- => -286 agree
STR : [0=-==] : LETTERS : [(= = - = 0)] : 0=-== => -287 agree
STR : [0==22] : LETTERS : [(2 2 = = 0)] : 0==22 => -288 agree
STR : [0==21] : LETTERS : [(1 2 = = 0)] : 0==21 => -289 agree
STR : [0==20] : LETTERS : [(0 2 = = 0)] : 0==20 => -290 agree
STR : [0==2-] : LETTERS : [(- 2 = = 0)] : 0==2- => -291 agree
STR : [0==2=] : LETTERS : [(= 2 = = 0)] : 0==2= => -292 agree
STR : [0==12] : LETTERS : [(2 1 = = 0)] : 0==12 => -293 agree
STR : [0==11] : LETTERS : [(1 1 = = 0)] : 0==11 => -294 agree
STR : [0==10] : LETTERS : [(0 1 = = 0)] : 0==10 => -295 agree
STR : [0==1-] : LETTERS : [(- 1 = = 0)] : 0==1- => -296 agree
STR : [0==1=] : LETTERS : [(= 1 = = 0)] : 0==1= => -297 agree
STR : [0==02] : LETTERS : [(2 0 = = 0)] : 0==02 => -298 agree
STR : [0==01] : LETTERS : [(1 0 = = 0)] : 0==01 => -299 agree
STR : [0==00] : LETTERS : [(0 0 = = 0)] : 0==00 => -300 agree
STR : [0==0-] : LETTERS : [(- 0 = = 0)] : 0==0- => -301 agree
STR : [0==0=] : LETTERS : [(= 0 = = 0)] : 0==0= => -302 agree
STR : [0==-2] : LETTERS : [(2 - = = 0)] : 0==-2 => -303 agree
STR : [0==-1] : LETTERS : [(1 - = = 0)] : 0==-1 => -304 agree
STR : [0==-0] : LETTERS : [(0 - = = 0)] : 0==-0 => -305 agree
STR : [0==--] : LETTERS : [(- - = = 0)] : 0==-- => -306 agree
STR : [0==-=] : LETTERS : [(= - = = 0)] : 0==-= => -307 agree
STR : [0===2] : LETTERS : [(2 = = = 0)] : 0===2 => -308 agree
STR : [0===1] : LETTERS : [(1 = = = 0)] : 0===1 => -309 agree
STR : [0===0] : LETTERS : [(0 = = = 0)] : 0===0 => -310 agree
STR : [0===-] : LETTERS : [(- = = = 0)] : 0===- => -311 agree
STR : [0====] : LETTERS : [(= = = = 0)] : 0==== => -312 agree
STR : [-2222] : LETTERS : [(2 2 2 2 -)] : -2222 => -313 agree
STR : [-2221] : LETTERS : [(1 2 2 2 -)] : -2221 => -314 agree
STR : [-2220] : LETTERS : [(0 2 2 2 -)] : -2220 => -315 agree
STR : [-222-] : LETTERS : [(- 2 2 2 -)] : -222- => -316 agree
STR : [-222=] : LETTERS : [(= 2 2 2 -)] : -222= => -317 agree
STR : [-2212] : LETTERS : [(2 1 2 2 -)] : -2212 => -318 agree
STR : [-2211] : LETTERS : [(1 1 2 2 -)] : -2211 => -319 agree
STR : [-2210] : LETTERS : [(0 1 2 2 -)] : -2210 => -320 agree
STR : [-221-] : LETTERS : [(- 1 2 2 -)] : -221- => -321 agree
STR : [-221=] : LETTERS : [(= 1 2 2 -)] : -221= => -322 agree
STR : [-2202] : LETTERS : [(2 0 2 2 -)] : -2202 => -323 agree
STR : [-2201] : LETTERS : [(1 0 2 2 -)] : -2201 => -324 agree
STR : [-2200] : LETTERS : [(0 0 2 2 -)] : -2200 => -325 agree
STR : [-220-] : LETTERS : [(- 0 2 2 -)] : -220- => -326 agree
STR : [-220=] : LETTERS : [(= 0 2 2 -)] : -220= => -327 agree
STR : [-22-2] : LETTERS : [(2 - 2 2 -)] : -22-2 => -328 agree
STR : [-22-1] : LETTERS : [(1 - 2 2 -)] : -22-1 => -329 agree
STR : [-22-0] : LETTERS : [(0 - 2 2 -)] : -22-0 => -330 agree
STR : [-22--] : LETTERS : [(- - 2 2 -)] : -22-- => -331 agree
STR : [-22-=] : LETTERS : [(= - 2 2 -)] : -22-= => -332 agree
STR : [-22=2] : LETTERS : [(2 = 2 2 -)] : -22=2 => -333 agree
STR : [-22=1] : LETTERS : [(1 = 2 2 -)] : -22=1 => -334 agree
STR : [-22=0] : LETTERS : [(0 = 2 2 -)] : -22=0 => -335 agree
STR : [-22=-] : LETTERS : [(- = 2 2 -)] : -22=- => -336 agree
STR : [-22==] : LETTERS : [(= = 2 2 -)] : -22== => -337 agree
STR : [-2122] : LETTERS : [(2 2 1 2 -)] : -2122 => -338 agree
STR : [-2121] : LETTERS : [(1 2 1 2 -)] : -2121 => -339 agree
STR : [-2120] : LETTERS : [(0 2 1 2 -)] : -2120 => -340 agree
STR : [-212-] : LETTERS : [(- 2 1 2 -)] : -212- => -341 agree
STR : [-212=] : LETTERS : [(= 2 1 2 -)] : -212= => -342 agree
STR : [-2112] : LETTERS : [(2 1 1 2 -)] : -2112 => -343 agree
STR : [-2111] : LETTERS : [(1 1 1 2 -)] : -2111 => -344 agree
STR : [-2110] : LETTERS : [(0 1 1 2 -)] : -2110 => -345 agree
STR : [-211-] : LETTERS : [(- 1 1 2 -)] : -211- => -346 agree
STR : [-211=] : LETTERS : [(= 1 1 2 -)] : -211= => -347 agree
STR : [-2102] : LETTERS : [(2 0 1 2 -)] : -2102 => -348 agree
STR : [-2101] : LETTERS : [(1 0 1 2 -)] : -2101 => -349 agree
STR : [-2100] : LETTERS : [(0 0 1 2 -)] : -2100 => -350 agree
STR : [-210-] : LETTERS : [(- 0 1 2 -)] : -210- => -351 agree
STR : [-210=] : LETTERS : [(= 0 1 2 -)] : -210= => -352 agree
STR : [-21-2] : LETTERS : [(2 - 1 2 -)] : -21-2 => -353 agree
STR : [-21-1] : LETTERS : [(1 - 1 2 -)] : -21-1 => -354 agree
STR : [-21-0] : LETTERS : [(0 - 1 2 -)] : -21-0 => -355 agree
STR : [-21--] : LETTERS : [(- - 1 2 -)] : -21-- => -356 agree
STR : [-21-=] : LETTERS : [(= - 1 2 -)] : -21-= => -357 agree
STR : [-21=2] : LETTERS : [(2 = 1 2 -)] : -21=2 => -358 agree
STR : [-21=1] : LETTERS : [(1 = 1 2 -)] : -21=1 => -359 agree
STR : [-21=0] : LETTERS : [(0 = 1 2 -)] : -21=0 => -360 agree
STR : [-21=-] : LETTERS : [(- = 1 2 -)] : -21=- => -361 agree
STR : [-21==] : LETTERS : [(= = 1 2 -)] : -21== => -362 agree
STR : [-2022] : LETTERS : [(2 2 0 2 -)] : -2022 => -363 agree
STR : [-2021] : LETTERS : [(1 2 0 2 -)] : -2021 => -364 agree
STR : [-2020] : LETTERS : [(0 2 0 2 -)] : -2020 => -365 agree
STR : [-202-] : LETTERS : [(- 2 0 2 -)] : -202- => -366 agree
STR : [-202=] : LETTERS : [(= 2 0 2 -)] : -202= => -367 agree
STR : [-2012] : LETTERS : [(2 1 0 2 -)] : -2012 => -368 agree
STR : [-2011] : LETTERS : [(1 1 0 2 -)] : -2011 => -369 agree
STR : [-2010] : LETTERS : [(0 1 0 2 -)] : -2010 => -370 agree
STR : [-201-] : LETTERS : [(- 1 0 2 -)] : -201- => -371 agree
STR : [-201=] : LETTERS : [(= 1 0 2 -)] : -201= => -372 agree
STR : [-2002] : LETTERS : [(2 0 0 2 -)] : -2002 => -373 agree
STR : [-2001] : LETTERS : [(1 0 0 2 -)] : -2001 => -374 agree
STR : [-2000] : LETTERS : [(0 0 0 2 -)] : -2000 => -375 agree
STR : [-200-] : LETTERS : [(- 0 0 2 -)] : -200- => -376 agree
STR : [-200=] : LETTERS : [(= 0 0 2 -)] : -200= => -377 agree
STR : [-20-2] : LETTERS : [(2 - 0 2 -)] : -20-2 => -378 agree
STR : [-20-1] : LETTERS : [(1 - 0 2 -)] : -20-1 => -379 agree
STR : [-20-0] : LETTERS : [(0 - 0 2 -)] : -20-0 => -380 agree
STR : [-20--] : LETTERS : [(- - 0 2 -)] : -20-- => -381 agree
STR : [-20-=] : LETTERS : [(= - 0 2 -)] : -20-= => -382 agree
STR : [-20=2] : LETTERS : [(2 = 0 2 -)] : -20=2 => -383 agree
STR : [-20=1] : LETTERS : [(1 = 0 2 -)] : -20=1 => -384 agree
STR : [-20=0] : LETTERS : [(0 = 0 2 -)] : -20=0 => -385 agree
STR : [-20=-] : LETTERS : [(- = 0 2 -)] : -20=- => -386 agree
STR : [-20==] : LETTERS : [(= = 0 2 -)] : -20== => -387 agree
STR : [-2-22] : LETTERS : [(2 2 - 2 -)] : -2-22 => -388 agree
STR : [-2-21] : LETTERS : [(1 2 - 2 -)] : -2-21 => -389 agree
STR : [-2-20] : LETTERS : [(0 2 - 2 -)] : -2-20 => -390 agree
STR : [-2-2-] : LETTERS : [(- 2 - 2 -)] : -2-2- => -391 agree
STR : [-2-2=] : LETTERS : [(= 2 - 2 -)] : -2-2= => -392 agree
STR : [-2-12] : LETTERS : [(2 1 - 2 -)] : -2-12 => -393 agree
STR : [-2-11] : LETTERS : [(1 1 - 2 -)] : -2-11 => -394 agree
STR : [-2-10] : LETTERS : [(0 1 - 2 -)] : -2-10 => -395 agree
STR : [-2-1-] : LETTERS : [(- 1 - 2 -)] : -2-1- => -396 agree
STR : [-2-1=] : LETTERS : [(= 1 - 2 -)] : -2-1= => -397 agree
STR : [-2-02] : LETTERS : [(2 0 - 2 -)] : -2-02 => -398 agree
STR : [-2-01] : LETTERS : [(1 0 - 2 -)] : -2-01 => -399 agree
STR : [-2-00] : LETTERS : [(0 0 - 2 -)] : -2-00 => -400 agree
STR : [-2-0-] : LETTERS : [(- 0 - 2 -)] : -2-0- => -401 agree
STR : [-2-0=] : LETTERS : [(= 0 - 2 -)] : -2-0= => -402 agree
STR : [-2--2] : LETTERS : [(2 - - 2 -)] : -2--2 => -403 agree
STR : [-2--1] : LETTERS : [(1 - - 2 -)] : -2--1 => -404 agree
STR : [-2--0] : LETTERS : [(0 - - 2 -)] : -2--0 => -405 agree
STR : [-2---] : LETTERS : [(- - - 2 -)] : -2--- => -406 agree
STR : [-2--=] : LETTERS : [(= - - 2 -)] : -2--= => -407 agree
STR : [-2-=2] : LETTERS : [(2 = - 2 -)] : -2-=2 => -408 agree
STR : [-2-=1] : LETTERS : [(1 = - 2 -)] : -2-=1 => -409 agree
STR : [-2-=0] : LETTERS : [(0 = - 2 -)] : -2-=0 => -410 agree
STR : [-2-=-] : LETTERS : [(- = - 2 -)] : -2-=- => -411 agree
STR : [-2-==] : LETTERS : [(= = - 2 -)] : -2-== => -412 agree
STR : [-2=22] : LETTERS : [(2 2 = 2 -)] : -2=22 => -413 agree
STR : [-2=21] : LETTERS : [(1 2 = 2 -)] : -2=21 => -414 agree
STR : [-2=20] : LETTERS : [(0 2 = 2 -)] : -2=20 => -415 agree
STR : [-2=2-] : LETTERS : [(- 2 = 2 -)] : -2=2- => -416 agree
STR : [-2=2=] : LETTERS : [(= 2 = 2 -)] : -2=2= => -417 agree
STR : [-2=12] : LETTERS : [(2 1 = 2 -)] : -2=12 => -418 agree
STR : [-2=11] : LETTERS : [(1 1 = 2 -)] : -2=11 => -419 agree
STR : [-2=10] : LETTERS : [(0 1 = 2 -)] : -2=10 => -420 agree
STR : [-2=1-] : LETTERS : [(- 1 = 2 -)] : -2=1- => -421 agree
STR : [-2=1=] : LETTERS : [(= 1 = 2 -)] : -2=1= => -422 agree
STR : [-2=02] : LETTERS : [(2 0 = 2 -)] : -2=02 => -423 agree
STR : [-2=01] : LETTERS : [(1 0 = 2 -)] : -2=01 => -424 agree
STR : [-2=00] : LETTERS : [(0 0 = 2 -)] : -2=00 => -425 agree
STR : [-2=0-] : LETTERS : [(- 0 = 2 -)] : -2=0- => -426 agree
STR : [-2=0=] : LETTERS : [(= 0 = 2 -)] : -2=0= => -427 agree
STR : [-2=-2] : LETTERS : [(2 - = 2 -)] : -2=-2 => -428 agree
STR : [-2=-1] : LETTERS : [(1 - = 2 -)] : -2=-1 => -429 agree
STR : [-2=-0] : LETTERS : [(0 - = 2 -)] : -2=-0 => -430 agree
STR : [-2=--] : LETTERS : [(- - = 2 -)] : -2=-- => -431 agree
STR : [-2=-=] : LETTERS : [(= - = 2 -)] : -2=-= => -432 agree
STR : [-2==2] : LETTERS : [(2 = = 2 -)] : -2==2 => -433 agree
STR : [-2==1] : LETTERS : [(1 = = 2 -)] : -2==1 => -434 agree
STR : [-2==0] : LETTERS : [(0 = = 2 -)] : -2==0 => -435 agree
STR : [-2==-] : LETTERS : [(- = = 2 -)] : -2==- => -436 agree
STR : [-2===] : LETTERS : [(= = = 2 -)] : -2=== => -437 agree
STR : [-1222] : LETTERS : [(2 2 2 1 -)] : -1222 => -438 agree
STR : [-1221] : LETTERS : [(1 2 2 1 -)] : -1221 => -439 agree
STR : [-1220] : LETTERS : [(0 2 2 1 -)] : -1220 => -440 agree
STR : [-122-] : LETTERS : [(- 2 2 1 -)] : -122- => -441 agree
STR : [-122=] : LETTERS : [(= 2 2 1 -)] : -122= => -442 agree
STR : [-1212] : LETTERS : [(2 1 2 1 -)] : -1212 => -443 agree
STR : [-1211] : LETTERS : [(1 1 2 1 -)] : -1211 => -444 agree
STR : [-1210] : LETTERS : [(0 1 2 1 -)] : -1210 => -445 agree
STR : [-121-] : LETTERS : [(- 1 2 1 -)] : -121- => -446 agree
STR : [-121=] : LETTERS : [(= 1 2 1 -)] : -121= => -447 agree
STR : [-1202] : LETTERS : [(2 0 2 1 -)] : -1202 => -448 agree
STR : [-1201] : LETTERS : [(1 0 2 1 -)] : -1201 => -449 agree
STR : [-1200] : LETTERS : [(0 0 2 1 -)] : -1200 => -450 agree
STR : [-120-] : LETTERS : [(- 0 2 1 -)] : -120- => -451 agree
STR : [-120=] : LETTERS : [(= 0 2 1 -)] : -120= => -452 agree
STR : [-12-2] : LETTERS : [(2 - 2 1 -)] : -12-2 => -453 agree
STR : [-12-1] : LETTERS : [(1 - 2 1 -)] : -12-1 => -454 agree
STR : [-12-0] : LETTERS : [(0 - 2 1 -)] : -12-0 => -455 agree
STR : [-12--] : LETTERS : [(- - 2 1 -)] : -12-- => -456 agree
STR : [-12-=] : LETTERS : [(= - 2 1 -)] : -12-= => -457 agree
STR : [-12=2] : LETTERS : [(2 = 2 1 -)] : -12=2 => -458 agree
STR : [-12=1] : LETTERS : [(1 = 2 1 -)] : -12=1 => -459 agree
STR : [-12=0] : LETTERS : [(0 = 2 1 -)] : -12=0 => -460 agree
STR : [-12=-] : LETTERS : [(- = 2 1 -)] : -12=- => -461 agree
STR : [-12==] : LETTERS : [(= = 2 1 -)] : -12== => -462 agree
STR : [-1122] : LETTERS : [(2 2 1 1 -)] : -1122 => -463 agree
STR : [-1121] : LETTERS : [(1 2 1 1 -)] : -1121 => -464 agree
STR : [-1120] : LETTERS : [(0 2 1 1 -)] : -1120 => -465 agree
STR : [-112-] : LETTERS : [(- 2 1 1 -)] : -112- => -466 agree
STR : [-112=] : LETTERS : [(= 2 1 1 -)] : -112= => -467 agree
STR : [-1112] : LETTERS : [(2 1 1 1 -)] : -1112 => -468 agree
STR : [-1111] : LETTERS : [(1 1 1 1 -)] : -1111 => -469 agree
STR : [-1110] : LETTERS : [(0 1 1 1 -)] : -1110 => -470 agree
STR : [-111-] : LETTERS : [(- 1 1 1 -)] : -111- => -471 agree
STR : [-111=] : LETTERS : [(= 1 1 1 -)] : -111= => -472 agree
STR : [-1102] : LETTERS : [(2 0 1 1 -)] : -1102 => -473 agree
STR : [-1101] : LETTERS : [(1 0 1 1 -)] : -1101 => -474 agree
STR : [-1100] : LETTERS : [(0 0 1 1 -)] : -1100 => -475 agree
STR : [-110-] : LETTERS : [(- 0 1 1 -)] : -110- => -476 agree
STR : [-110=] : LETTERS : [(= 0 1 1 -)] : -110= => -477 agree
STR : [-11-2] : LETTERS : [(2 - 1 1 -)] : -11-2 => -478 agree
STR : [-11-1] : LETTERS : [(1 - 1 1 -)] : -11-1 => -479 agree
STR : [-11-0] : LETTERS : [(0 - 1 1 -)] : -11-0 => -480 agree
STR : [-11--] : LETTERS : [(- - 1 1 -)] : -11-- => -481 agree
STR : [-11-=] : LETTERS : [(= - 1 1 -)] : -11-= => -482 agree
STR : [-11=2] : LETTERS : [(2 = 1 1 -)] : -11=2 => -483 agree
STR : [-11=1] : LETTERS : [(1 = 1 1 -)] : -11=1 => -484 agree
STR : [-11=0] : LETTERS : [(0 = 1 1 -)] : -11=0 => -485 agree
STR : [-11=-] : LETTERS : [(- = 1 1 -)] : -11=- => -486 agree
STR : [-11==] : LETTERS : [(= = 1 1 -)] : -11== => -487 agree
STR : [-1022] : LETTERS : [(2 2 0 1 -)] : -1022 => -488 agree
STR : [-1021] : LETTERS : [(1 2 0 1 -)] : -1021 => -489 agree
STR : [-1020] : LETTERS : [(0 2 0 1 -)] : -1020 => -490 agree
STR : [-102-] : LETTERS : [(- 2 0 1 -)] : -102- => -491 agree
STR : [-102=] : LETTERS : [(= 2 0 1 -)] : -102= => -492 agree
STR : [-1012] : LETTERS : [(2 1 0 1 -)] : -1012 => -493 agree
STR : [-1011] : LETTERS : [(1 1 0 1 -)] : -1011 => -494 agree
STR : [-1010] : LETTERS : [(0 1 0 1 -)] : -1010 => -495 agree
STR : [-101-] : LETTERS : [(- 1 0 1 -)] : -101- => -496 agree
STR : [-101=] : LETTERS : [(= 1 0 1 -)] : -101= => -497 agree
STR : [-1002] : LETTERS : [(2 0 0 1 -)] : -1002 => -498 agree
STR : [-1001] : LETTERS : [(1 0 0 1 -)] : -1001 => -499 agree
STR : [-1000] : LETTERS : [(0 0 0 1 -)] : -1000 => -500 agree
STR : [-100-] : LETTERS : [(- 0 0 1 -)] : -100- => -501 agree
STR : [-100=] : LETTERS : [(= 0 0 1 -)] : -100= => -502 agree
STR : [-10-2] : LETTERS : [(2 - 0 1 -)] : -10-2 => -503 agree
STR : [-10-1] : LETTERS : [(1 - 0 1 -)] : -10-1 => -504 agree
STR : [-10-0] : LETTERS : [(0 - 0 1 -)] : -10-0 => -505 agree
STR : [-10--] : LETTERS : [(- - 0 1 -)] : -10-- => -506 agree
STR : [-10-=] : LETTERS : [(= - 0 1 -)] : -10-= => -507 agree
STR : [-10=2] : LETTERS : [(2 = 0 1 -)] : -10=2 => -508 agree
STR : [-10=1] : LETTERS : [(1 = 0 1 -)] : -10=1 => -509 agree
STR : [-10=0] : LETTERS : [(0 = 0 1 -)] : -10=0 => -510 agree
STR : [-10=-] : LETTERS : [(- = 0 1 -)] : -10=- => -511 agree
STR : [-10==] : LETTERS : [(= = 0 1 -)] : -10== => -512 agree
STR : [-1-22] : LETTERS : [(2 2 - 1 -)] : -1-22 => -513 agree
STR : [-1-21] : LETTERS : [(1 2 - 1 -)] : -1-21 => -514 agree
STR : [-1-20] : LETTERS : [(0 2 - 1 -)] : -1-20 => -515 agree
STR : [-1-2-] : LETTERS : [(- 2 - 1 -)] : -1-2- => -516 agree
STR : [-1-2=] : LETTERS : [(= 2 - 1 -)] : -1-2= => -517 agree
STR : [-1-12] : LETTERS : [(2 1 - 1 -)] : -1-12 => -518 agree
STR : [-1-11] : LETTERS : [(1 1 - 1 -)] : -1-11 => -519 agree
STR : [-1-10] : LETTERS : [(0 1 - 1 -)] : -1-10 => -520 agree
STR : [-1-1-] : LETTERS : [(- 1 - 1 -)] : -1-1- => -521 agree
STR : [-1-1=] : LETTERS : [(= 1 - 1 -)] : -1-1= => -522 agree
STR : [-1-02] : LETTERS : [(2 0 - 1 -)] : -1-02 => -523 agree
STR : [-1-01] : LETTERS : [(1 0 - 1 -)] : -1-01 => -524 agree
STR : [-1-00] : LETTERS : [(0 0 - 1 -)] : -1-00 => -525 agree
STR : [-1-0-] : LETTERS : [(- 0 - 1 -)] : -1-0- => -526 agree
STR : [-1-0=] : LETTERS : [(= 0 - 1 -)] : -1-0= => -527 agree
STR : [-1--2] : LETTERS : [(2 - - 1 -)] : -1--2 => -528 agree
STR : [-1--1] : LETTERS : [(1 - - 1 -)] : -1--1 => -529 agree
STR : [-1--0] : LETTERS : [(0 - - 1 -)] : -1--0 => -530 agree
STR : [-1---] : LETTERS : [(- - - 1 -)] : -1--- => -531 agree
STR : [-1--=] : LETTERS : [(= - - 1 -)] : -1--= => -532 agree
STR : [-1-=2] : LETTERS : [(2 = - 1 -)] : -1-=2 => -533 agree
STR : [-1-=1] : LETTERS : [(1 = - 1 -)] : -1-=1 => -534 agree
STR : [-1-=0] : LETTERS : [(0 = - 1 -)] : -1-=0 => -535 agree
STR : [-1-=-] : LETTERS : [(- = - 1 -)] : -1-=- => -536 agree
STR : [-1-==] : LETTERS : [(= = - 1 -)] : -1-== => -537 agree
STR : [-1=22] : LETTERS : [(2 2 = 1 -)] : -1=22 => -538 agree
STR : [-1=21] : LETTERS : [(1 2 = 1 -)] : -1=21 => -539 agree
STR : [-1=20] : LETTERS : [(0 2 = 1 -)] : -1=20 => -540 agree
STR : [-1=2-] : LETTERS : [(- 2 = 1 -)] : -1=2- => -541 agree
STR : [-1=2=] : LETTERS : [(= 2 = 1 -)] : -1=2= => -542 agree
STR : [-1=12] : LETTERS : [(2 1 = 1 -)] : -1=12 => -543 agree
STR : [-1=11] : LETTERS : [(1 1 = 1 -)] : -1=11 => -544 agree
STR : [-1=10] : LETTERS : [(0 1 = 1 -)] : -1=10 => -545 agree
STR : [-1=1-] : LETTERS : [(- 1 = 1 -)] : -1=1- => -546 agree
STR : [-1=1=] : LETTERS : [(= 1 = 1 -)] : -1=1= => -547 agree
STR : [-1=02] : LETTERS : [(2 0 = 1 -)] : -1=02 => -548 agree
STR : [-1=01] : LETTERS : [(1 0 = 1 -)] : -1=01 => -549 agree
STR : [-1=00] : LETTERS : [(0 0 = 1 -)] : -1=00 => -550 agree
STR : [-1=0-] : LETTERS : [(- 0 = 1 -)] : -1=0- => -551 agree
STR : [-1=0=] : LETTERS : [(= 0 = 1 -)] : -1=0= => -552 agree
STR : [-1=-2] : LETTERS : [(2 - = 1 -)] : -1=-2 => -553 agree
STR : [-1=-1] : LETTERS : [(1 - = 1 -)] : -1=-1 => -554 agree
STR : [-1=-0] : LETTERS : [(0 - = 1 -)] : -1=-0 => -555 agree
STR : [-1=--] : LETTERS : [(- - = 1 -)] : -1=-- => -556 agree
STR : [-1=-=] : LETTERS : [(= - = 1 -)] : -1=-= => -557 agree
STR : [-1==2] : LETTERS : [(2 = = 1 -)] : -1==2 => -558 agree
STR : [-1==1] : LETTERS : [(1 = = 1 -)] : -1==1 => -559 agree
STR : [-1==0] : LETTERS : [(0 = = 1 -)] : -1==0 => -560 agree
STR : [-1==-] : LETTERS : [(- = = 1 -)] : -1==- => -561 agree
STR : [-1===] : LETTERS : [(= = = 1 -)] : -1=== => -562 agree
STR : [-0222] : LETTERS : [(2 2 2 0 -)] : -0222 => -563 agree
STR : [-0221] : LETTERS : [(1 2 2 0 -)] : -0221 => -564 agree
STR : [-0220] : LETTERS : [(0 2 2 0 -)] : -0220 => -565 agree
STR : [-022-] : LETTERS : [(- 2 2 0 -)] : -022- => -566 agree
STR : [-022=] : LETTERS : [(= 2 2 0 -)] : -022= => -567 agree
STR : [-0212] : LETTERS : [(2 1 2 0 -)] : -0212 => -568 agree
STR : [-0211] : LETTERS : [(1 1 2 0 -)] : -0211 => -569 agree
STR : [-0210] : LETTERS : [(0 1 2 0 -)] : -0210 => -570 agree
STR : [-021-] : LETTERS : [(- 1 2 0 -)] : -021- => -571 agree
STR : [-021=] : LETTERS : [(= 1 2 0 -)] : -021= => -572 agree
STR : [-0202] : LETTERS : [(2 0 2 0 -)] : -0202 => -573 agree
STR : [-0201] : LETTERS : [(1 0 2 0 -)] : -0201 => -574 agree
STR : [-0200] : LETTERS : [(0 0 2 0 -)] : -0200 => -575 agree
STR : [-020-] : LETTERS : [(- 0 2 0 -)] : -020- => -576 agree
STR : [-020=] : LETTERS : [(= 0 2 0 -)] : -020= => -577 agree
STR : [-02-2] : LETTERS : [(2 - 2 0 -)] : -02-2 => -578 agree
STR : [-02-1] : LETTERS : [(1 - 2 0 -)] : -02-1 => -579 agree
STR : [-02-0] : LETTERS : [(0 - 2 0 -)] : -02-0 => -580 agree
STR : [-02--] : LETTERS : [(- - 2 0 -)] : -02-- => -581 agree
STR : [-02-=] : LETTERS : [(= - 2 0 -)] : -02-= => -582 agree
STR : [-02=2] : LETTERS : [(2 = 2 0 -)] : -02=2 => -583 agree
STR : [-02=1] : LETTERS : [(1 = 2 0 -)] : -02=1 => -584 agree
STR : [-02=0] : LETTERS : [(0 = 2 0 -)] : -02=0 => -585 agree
STR : [-02=-] : LETTERS : [(- = 2 0 -)] : -02=- => -586 agree
STR : [-02==] : LETTERS : [(= = 2 0 -)] : -02== => -587 agree
STR : [-0122] : LETTERS : [(2 2 1 0 -)] : -0122 => -588 agree
STR : [-0121] : LETTERS : [(1 2 1 0 -)] : -0121 => -589 agree
STR : [-0120] : LETTERS : [(0 2 1 0 -)] : -0120 => -590 agree
STR : [-012-] : LETTERS : [(- 2 1 0 -)] : -012- => -591 agree
STR : [-012=] : LETTERS : [(= 2 1 0 -)] : -012= => -592 agree
STR : [-0112] : LETTERS : [(2 1 1 0 -)] : -0112 => -593 agree
STR : [-0111] : LETTERS : [(1 1 1 0 -)] : -0111 => -594 agree
STR : [-0110] : LETTERS : [(0 1 1 0 -)] : -0110 => -595 agree
STR : [-011-] : LETTERS : [(- 1 1 0 -)] : -011- => -596 agree
STR : [-011=] : LETTERS : [(= 1 1 0 -)] : -011= => -597 agree
STR : [-0102] : LETTERS : [(2 0 1 0 -)] : -0102 => -598 agree
STR : [-0101] : LETTERS : [(1 0 1 0 -)] : -0101 => -599 agree
STR : [-0100] : LETTERS : [(0 0 1 0 -)] : -0100 => -600 agree
STR : [-010-] : LETTERS : [(- 0 1 0 -)] : -010- => -601 agree
STR : [-010=] : LETTERS : [(= 0 1 0 -)] : -010= => -602 agree
STR : [-01-2] : LETTERS : [(2 - 1 0 -)] : -01-2 => -603 agree
STR : [-01-1] : LETTERS : [(1 - 1 0 -)] : -01-1 => -604 agree
STR : [-01-0] : LETTERS : [(0 - 1 0 -)] : -01-0 => -605 agree
STR : [-01--] : LETTERS : [(- - 1 0 -)] : -01-- => -606 agree
STR : [-01-=] : LETTERS : [(= - 1 0 -)] : -01-= => -607 agree
STR : [-01=2] : LETTERS : [(2 = 1 0 -)] : -01=2 => -608 agree
STR : [-01=1] : LETTERS : [(1 = 1 0 -)] : -01=1 => -609 agree
STR : [-01=0] : LETTERS : [(0 = 1 0 -)] : -01=0 => -610 agree
STR : [-01=-] : LETTERS : [(- = 1 0 -)] : -01=- => -611 agree
STR : [-01==] : LETTERS : [(= = 1 0 -)] : -01== => -612 agree
STR : [-0022] : LETTERS : [(2 2 0 0 -)] : -0022 => -613 agree
STR : [-0021] : LETTERS : [(1 2 0 0 -)] : -0021 => -614 agree
STR : [-0020] : LETTERS : [(0 2 0 0 -)] : -0020 => -615 agree
STR : [-002-] : LETTERS : [(- 2 0 0 -)] : -002- => -616 agree
STR : [-002=] : LETTERS : [(= 2 0 0 -)] : -002= => -617 agree
STR : [-0012] : LETTERS : [(2 1 0 0 -)] : -0012 => -618 agree
STR : [-0011] : LETTERS : [(1 1 0 0 -)] : -0011 => -619 agree
STR : [-0010] : LETTERS : [(0 1 0 0 -)] : -0010 => -620 agree
STR : [-001-] : LETTERS : [(- 1 0 0 -)] : -001- => -621 agree
STR : [-001=] : LETTERS : [(= 1 0 0 -)] : -001= => -622 agree
STR : [-0002] : LETTERS : [(2 0 0 0 -)] : -0002 => -623 agree
STR : [-0001] : LETTERS : [(1 0 0 0 -)] : -0001 => -624 agree
STR : [-0000] : LETTERS : [(0 0 0 0 -)] : -0000 => -625 agree
STR : [-000-] : LETTERS : [(- 0 0 0 -)] : -000- => -626 agree
STR : [-000=] : LETTERS : [(= 0 0 0 -)] : -000= => -627 agree
STR : [-00-2] : LETTERS : [(2 - 0 0 -)] : -00-2 => -628 agree
STR : [-00-1] : LETTERS : [(1 - 0 0 -)] : -00-1 => -629 agree
STR : [-00-0] : LETTERS : [(0 - 0 0 -)] : -00-0 => -630 agree
STR : [-00--] : LETTERS : [(- - 0 0 -)] : -00-- => -631 agree
STR : [-00-=] : LETTERS : [(= - 0 0 -)] : -00-= => -632 agree
STR : [-00=2] : LETTERS : [(2 = 0 0 -)] : -00=2 => -633 agree
STR : [-00=1] : LETTERS : [(1 = 0 0 -)] : -00=1 => -634 agree
STR : [-00=0] : LETTERS : [(0 = 0 0 -)] : -00=0 => -635 agree
STR : [-00=-] : LETTERS : [(- = 0 0 -)] : -00=- => -636 agree
STR : [-00==] : LETTERS : [(= = 0 0 -)] : -00== => -637 agree
STR : [-0-22] : LETTERS : [(2 2 - 0 -)] : -0-22 => -638 agree
STR : [-0-21] : LETTERS : [(1 2 - 0 -)] : -0-21 => -639 agree
STR : [-0-20] : LETTERS : [(0 2 - 0 -)] : -0-20 => -640 agree
STR : [-0-2-] : LETTERS : [(- 2 - 0 -)] : -0-2- => -641 agree
STR : [-0-2=] : LETTERS : [(= 2 - 0 -)] : -0-2= => -642 agree
STR : [-0-12] : LETTERS : [(2 1 - 0 -)] : -0-12 => -643 agree
STR : [-0-11] : LETTERS : [(1 1 - 0 -)] : -0-11 => -644 agree
STR : [-0-10] : LETTERS : [(0 1 - 0 -)] : -0-10 => -645 agree
STR : [-0-1-] : LETTERS : [(- 1 - 0 -)] : -0-1- => -646 agree
STR : [-0-1=] : LETTERS : [(= 1 - 0 -)] : -0-1= => -647 agree
STR : [-0-02] : LETTERS : [(2 0 - 0 -)] : -0-02 => -648 agree
STR : [-0-01] : LETTERS : [(1 0 - 0 -)] : -0-01 => -649 agree
STR : [-0-00] : LETTERS : [(0 0 - 0 -)] : -0-00 => -650 agree
STR : [-0-0-] : LETTERS : [(- 0 - 0 -)] : -0-0- => -651 agree
STR : [-0-0=] : LETTERS : [(= 0 - 0 -)] : -0-0= => -652 agree
STR : [-0--2] : LETTERS : [(2 - - 0 -)] : -0--2 => -653 agree
STR : [-0--1] : LETTERS : [(1 - - 0 -)] : -0--1 => -654 agree
STR : [-0--0] : LETTERS : [(0 - - 0 -)] : -0--0 => -655 agree
STR : [-0---] : LETTERS : [(- - - 0 -)] : -0--- => -656 agree
STR : [-0--=] : LETTERS : [(= - - 0 -)] : -0--= => -657 agree
STR : [-0-=2] : LETTERS : [(2 = - 0 -)] : -0-=2 => -658 agree
STR : [-0-=1] : LETTERS : [(1 = - 0 -)] : -0-=1 => -659 agree
STR : [-0-=0] : LETTERS : [(0 = - 0 -)] : -0-=0 => -660 agree
STR : [-0-=-] : LETTERS : [(- = - 0 -)] : -0-=- => -661 agree
STR : [-0-==] : LETTERS : [(= = - 0 -)] : -0-== => -662 agree
STR : [-0=22] : LETTERS : [(2 2 = 0 -)] : -0=22 => -663 agree
STR : [-0=21] : LETTERS : [(1 2 = 0 -)] : -0=21 => -664 agree
STR : [-0=20] : LETTERS : [(0 2 = 0 -)] : -0=20 => -665 agree
STR : [-0=2-] : LETTERS : [(- 2 = 0 -)] : -0=2- => -666 agree
STR : [-0=2=] : LETTERS : [(= 2 = 0 -)] : -0=2= => -667 agree
STR : [-0=12] : LETTERS : [(2 1 = 0 -)] : -0=12 => -668 agree
STR : [-0=11] : LETTERS : [(1 1 = 0 -)] : -0=11 => -669 agree
STR : [-0=10] : LETTERS : [(0 1 = 0 -)] : -0=10 => -670 agree
STR : [-0=1-] : LETTERS : [(- 1 = 0 -)] : -0=1- => -671 agree
STR : [-0=1=] : LETTERS : [(= 1 = 0 -)] : -0=1= => -672 agree
STR : [-0=02] : LETTERS : [(2 0 = 0 -)] : -0=02 => -673 agree
STR : [-0=01] : LETTERS : [(1 0 = 0 -)] : -0=01 => -674 agree
STR : [-0=00] : LETTERS : [(0 0 = 0 -)] : -0=00 => -675 agree
STR : [-0=0-] : LETTERS : [(- 0 = 0 -)] : -0=0- => -676 agree
STR : [-0=0=] : LETTERS : [(= 0 = 0 -)] : -0=0= => -677 agree
STR : [-0=-2] : LETTERS : [(2 - = 0 -)] : -0=-2 => -678 agree
STR : [-0=-1] : LETTERS : [(1 - = 0 -)] : -0=-1 => -679 agree
STR : [-0=-0] : LETTERS : [(0 - = 0 -)] : -0=-0 => -680 agree
STR : [-0=--] : LETTERS : [(- - = 0 -)] : -0=-- => -681 agree
STR : [-0=-=] : LETTERS : [(= - = 0 -)] : -0=-= => -682 agree
STR : [-0==2] : LETTERS : [(2 = = 0 -)] : -0==2 => -683 agree
STR : [-0==1] : LETTERS : [(1 = = 0 -)] : -0==1 => -684 agree
STR : [-0==0] : LETTERS : [(0 = = 0 -)] : -0==0 => -685 agree
STR : [-0==-] : LETTERS : [(- = = 0 -)] : -0==- => -686 agree
STR : [-0===] : LETTERS : [(= = = 0 -)] : -0=== => -687 agree
STR : [--222] : LETTERS : [(2 2 2 - -)] : --222 => -688 agree
STR : [--221] : LETTERS : [(1 2 2 - -)] : --221 => -689 agree
STR : [--220] : LETTERS : [(0 2 2 - -)] : --220 => -690 agree
STR : [--22-] : LETTERS : [(- 2 2 - -)] : --22- => -691 agree
STR : [--22=] : LETTERS : [(= 2 2 - -)] : --22= => -692 agree
STR : [--212] : LETTERS : [(2 1 2 - -)] : --212 => -693 agree
STR : [--211] : LETTERS : [(1 1 2 - -)] : --211 => -694 agree
STR : [--210] : LETTERS : [(0 1 2 - -)] : --210 => -695 agree
STR : [--21-] : LETTERS : [(- 1 2 - -)] : --21- => -696 agree
STR : [--21=] : LETTERS : [(= 1 2 - -)] : --21= => -697 agree
STR : [--202] : LETTERS : [(2 0 2 - -)] : --202 => -698 agree
STR : [--201] : LETTERS : [(1 0 2 - -)] : --201 => -699 agree
STR : [--200] : LETTERS : [(0 0 2 - -)] : --200 => -700 agree
STR : [--20-] : LETTERS : [(- 0 2 - -)] : --20- => -701 agree
STR : [--20=] : LETTERS : [(= 0 2 - -)] : --20= => -702 agree
STR : [--2-2] : LETTERS : [(2 - 2 - -)] : --2-2 => -703 agree
STR : [--2-1] : LETTERS : [(1 - 2 - -)] : --2-1 => -704 agree
STR : [--2-0] : LETTERS : [(0 - 2 - -)] : --2-0 => -705 agree
STR : [--2--] : LETTERS : [(- - 2 - -)] : --2-- => -706 agree
STR : [--2-=] : LETTERS : [(= - 2 - -)] : --2-= => -707 agree
STR : [--2=2] : LETTERS : [(2 = 2 - -)] : --2=2 => -708 agree
STR : [--2=1] : LETTERS : [(1 = 2 - -)] : --2=1 => -709 agree
STR : [--2=0] : LETTERS : [(0 = 2 - -)] : --2=0 => -710 agree
STR : [--2=-] : LETTERS : [(- = 2 - -)] : --2=- => -711 agree
STR : [--2==] : LETTERS : [(= = 2 - -)] : --2== => -712 agree
STR : [--122] : LETTERS : [(2 2 1 - -)] : --122 => -713 agree
STR : [--121] : LETTERS : [(1 2 1 - -)] : --121 => -714 agree
STR : [--120] : LETTERS : [(0 2 1 - -)] : --120 => -715 agree
STR : [--12-] : LETTERS : [(- 2 1 - -)] : --12- => -716 agree
STR : [--12=] : LETTERS : [(= 2 1 - -)] : --12= => -717 agree
STR : [--112] : LETTERS : [(2 1 1 - -)] : --112 => -718 agree
STR : [--111] : LETTERS : [(1 1 1 - -)] : --111 => -719 agree
STR : [--110] : LETTERS : [(0 1 1 - -)] : --110 => -720 agree
STR : [--11-] : LETTERS : [(- 1 1 - -)] : --11- => -721 agree
STR : [--11=] : LETTERS : [(= 1 1 - -)] : --11= => -722 agree
STR : [--102] : LETTERS : [(2 0 1 - -)] : --102 => -723 agree
STR : [--101] : LETTERS : [(1 0 1 - -)] : --101 => -724 agree
STR : [--100] : LETTERS : [(0 0 1 - -)] : --100 => -725 agree
STR : [--10-] : LETTERS : [(- 0 1 - -)] : --10- => -726 agree
STR : [--10=] : LETTERS : [(= 0 1 - -)] : --10= => -727 agree
STR : [--1-2] : LETTERS : [(2 - 1 - -)] : --1-2 => -728 agree
STR : [--1-1] : LETTERS : [(1 - 1 - -)] : --1-1 => -729 agree
STR : [--1-0] : LETTERS : [(0 - 1 - -)] : --1-0 => -730 agree
STR : [--1--] : LETTERS : [(- - 1 - -)] : --1-- => -731 agree
STR : [--1-=] : LETTERS : [(= - 1 - -)] : --1-= => -732 agree
STR : [--1=2] : LETTERS : [(2 = 1 - -)] : --1=2 => -733 agree
STR : [--1=1] : LETTERS : [(1 = 1 - -)] : --1=1 => -734 agree
STR : [--1=0] : LETTERS : [(0 = 1 - -)] : --1=0 => -735 agree
STR : [--1=-] : LETTERS : [(- = 1 - -)] : --1=- => -736 agree
STR : [--1==] : LETTERS : [(= = 1 - -)] : --1== => -737 agree
STR : [--022] : LETTERS : [(2 2 0 - -)] : --022 => -738 agree
STR : [--021] : LETTERS : [(1 2 0 - -)] : --021 => -739 agree
STR : [--020] : LETTERS : [(0 2 0 - -)] : --020 => -740 agree
STR : [--02-] : LETTERS : [(- 2 0 - -)] : --02- => -741 agree
STR : [--02=] : LETTERS : [(= 2 0 - -)] : --02= => -742 agree
STR : [--012] : LETTERS : [(2 1 0 - -)] : --012 => -743 agree
STR : [--011] : LETTERS : [(1 1 0 - -)] : --011 => -744 agree
STR : [--010] : LETTERS : [(0 1 0 - -)] : --010 => -745 agree
STR : [--01-] : LETTERS : [(- 1 0 - -)] : --01- => -746 agree
STR : [--01=] : LETTERS : [(= 1 0 - -)] : --01= => -747 agree
STR : [--002] : LETTERS : [(2 0 0 - -)] : --002 => -748 agree
STR : [--001] : LETTERS : [(1 0 0 - -)] : --001 => -749 agree
STR : [--000] : LETTERS : [(0 0 0 - -)] : --000 => -750 agree
STR : [--00-] : LETTERS : [(- 0 0 - -)] : --00- => -751 agree
STR : [--00=] : LETTERS : [(= 0 0 - -)] : --00= => -752 agree
STR : [--0-2] : LETTERS : [(2 - 0 - -)] : --0-2 => -753 agree
STR : [--0-1] : LETTERS : [(1 - 0 - -)] : --0-1 => -754 agree
STR : [--0-0] : LETTERS : [(0 - 0 - -)] : --0-0 => -755 agree
STR : [--0--] : LETTERS : [(- - 0 - -)] : --0-- => -756 agree
STR : [--0-=] : LETTERS : [(= - 0 - -)] : --0-= => -757 agree
STR : [--0=2] : LETTERS : [(2 = 0 - -)] : --0=2 => -758 agree
STR : [--0=1] : LETTERS : [(1 = 0 - -)] : --0=1 => -759 agree
STR : [--0=0] : LETTERS : [(0 = 0 - -)] : --0=0 => -760 agree
STR : [--0=-] : LETTERS : [(- = 0 - -)] : --0=- => -761 agree
STR : [--0==] : LETTERS : [(= = 0 - -)] : --0== => -762 agree
STR : [---22] : LETTERS : [(2 2 - - -)] : ---22 => -763 agree
STR : [---21] : LETTERS : [(1 2 - - -)] : ---21 => -764 agree
STR : [---20] : LETTERS : [(0 2 - - -)] : ---20 => -765 agree
STR : [---2-] : LETTERS : [(- 2 - - -)] : ---2- => -766 agree
STR : [---2=] : LETTERS : [(= 2 - - -)] : ---2= => -767 agree
STR : [---12] : LETTERS : [(2 1 - - -)] : ---12 => -768 agree
STR : [---11] : LETTERS : [(1 1 - - -)] : ---11 => -769 agree
STR : [---10] : LETTERS : [(0 1 - - -)] : ---10 => -770 agree
STR : [---1-] : LETTERS : [(- 1 - - -)] : ---1- => -771 agree
STR : [---1=] : LETTERS : [(= 1 - - -)] : ---1= => -772 agree
STR : [---02] : LETTERS : [(2 0 - - -)] : ---02 => -773 agree
STR : [---01] : LETTERS : [(1 0 - - -)] : ---01 => -774 agree
STR : [---00] : LETTERS : [(0 0 - - -)] : ---00 => -775 agree
STR : [---0-] : LETTERS : [(- 0 - - -)] : ---0- => -776 agree
STR : [---0=] : LETTERS : [(= 0 - - -)] : ---0= => -777 agree
STR : [----2] : LETTERS : [(2 - - - -)] : ----2 => -778 agree
STR : [----1] : LETTERS : [(1 - - - -)] : ----1 => -779 agree
STR : [----0] : LETTERS : [(0 - - - -)] : ----0 => -780 agree
STR : [-----] : LETTERS : [(- - - - -)] : ----- => -781 agree
STR : [----=] : LETTERS : [(= - - - -)] : ----= => -782 agree
STR : [---=2] : LETTERS : [(2 = - - -)] : ---=2 => -783 agree
STR : [---=1] : LETTERS : [(1 = - - -)] : ---=1 => -784 agree
STR : [---=0] : LETTERS : [(0 = - - -)] : ---=0 => -785 agree
STR : [---=-] : LETTERS : [(- = - - -)] : ---=- => -786 agree
STR : [---==] : LETTERS : [(= = - - -)] : ---== => -787 agree
STR : [--=22] : LETTERS : [(2 2 = - -)] : --=22 => -788 agree
STR : [--=21] : LETTERS : [(1 2 = - -)] : --=21 => -789 agree
STR : [--=20] : LETTERS : [(0 2 = - -)] : --=20 => -790 agree
STR : [--=2-] : LETTERS : [(- 2 = - -)] : --=2- => -791 agree
STR : [--=2=] : LETTERS : [(= 2 = - -)] : --=2= => -792 agree
STR : [--=12] : LETTERS : [(2 1 = - -)] : --=12 => -793 agree
STR : [--=11] : LETTERS : [(1 1 = - -)] : --=11 => -794 agree
STR : [--=10] : LETTERS : [(0 1 = - -)] : --=10 => -795 agree
STR : [--=1-] : LETTERS : [(- 1 = - -)] : --=1- => -796 agree
STR : [--=1=] : LETTERS : [(= 1 = - -)] : --=1= => -797 agree
STR : [--=02] : LETTERS : [(2 0 = - -)] : --=02 => -798 agree
STR : [--=01] : LETTERS : [(1 0 = - -)] : --=01 => -799 agree
STR : [--=00] : LETTERS : [(0 0 = - -)] : --=00 => -800 agree
STR : [--=0-] : LETTERS : [(- 0 = - -)] : --=0- => -801 agree
STR : [--=0=] : LETTERS : [(= 0 = - -)] : --=0= => -802 agree
STR : [--=-2] : LETTERS : [(2 - = - -)] : --=-2 => -803 agree
STR : [--=-1] : LETTERS : [(1 - = - -)] : --=-1 => -804 agree
STR : [--=-0] : LETTERS : [(0 - = - -)] : --=-0 => -805 agree
STR : [--=--] : LETTERS : [(- - = - -)] : --=-- => -806 agree
STR : [--=-=] : LETTERS : [(= - = - -)] : --=-= => -807 agree
STR : [--==2] : LETTERS : [(2 = = - -)] : --==2 => -808 agree
STR : [--==1] : LETTERS : [(1 = = - -)] : --==1 => -809 agree
STR : [--==0] : LETTERS : [(0 = = - -)] : --==0 => -810 agree
STR : [--==-] : LETTERS : [(- = = - -)] : --==- => -811 agree
STR : [--===] : LETTERS : [(= = = - -)] : --=== => -812 agree
STR : [-=222] : LETTERS : [(2 2 2 = -)] : -=222 => -813 agree
STR : [-=221] : LETTERS : [(1 2 2 = -)] : -=221 => -814 agree
STR : [-=220] : LETTERS : [(0 2 2 = -)] : -=220 => -815 agree
STR : [-=22-] : LETTERS : [(- 2 2 = -)] : -=22- => -816 agree
STR : [-=22=] : LETTERS : [(= 2 2 = -)] : -=22= => -817 agree
STR : [-=212] : LETTERS : [(2 1 2 = -)] : -=212 => -818 agree
STR : [-=211] : LETTERS : [(1 1 2 = -)] : -=211 => -819 agree
STR : [-=210] : LETTERS : [(0 1 2 = -)] : -=210 => -820 agree
STR : [-=21-] : LETTERS : [(- 1 2 = -)] : -=21- => -821 agree
STR : [-=21=] : LETTERS : [(= 1 2 = -)] : -=21= => -822 agree
STR : [-=202] : LETTERS : [(2 0 2 = -)] : -=202 => -823 agree
STR : [-=201] : LETTERS : [(1 0 2 = -)] : -=201 => -824 agree
STR : [-=200] : LETTERS : [(0 0 2 = -)] : -=200 => -825 agree
STR : [-=20-] : LETTERS : [(- 0 2 = -)] : -=20- => -826 agree
STR : [-=20=] : LETTERS : [(= 0 2 = -)] : -=20= => -827 agree
STR : [-=2-2] : LETTERS : [(2 - 2 = -)] : -=2-2 => -828 agree
STR : [-=2-1] : LETTERS : [(1 - 2 = -)] : -=2-1 => -829 agree
STR : [-=2-0] : LETTERS : [(0 - 2 = -)] : -=2-0 => -830 agree
STR : [-=2--] : LETTERS : [(- - 2 = -)] : -=2-- => -831 agree
STR : [-=2-=] : LETTERS : [(= - 2 = -)] : -=2-= => -832 agree
STR : [-=2=2] : LETTERS : [(2 = 2 = -)] : -=2=2 => -833 agree
STR : [-=2=1] : LETTERS : [(1 = 2 = -)] : -=2=1 => -834 agree
STR : [-=2=0] : LETTERS : [(0 = 2 = -)] : -=2=0 => -835 agree
STR : [-=2=-] : LETTERS : [(- = 2 = -)] : -=2=- => -836 agree
STR : [-=2==] : LETTERS : [(= = 2 = -)] : -=2== => -837 agree
STR : [-=122] : LETTERS : [(2 2 1 = -)] : -=122 => -838 agree
STR : [-=121] : LETTERS : [(1 2 1 = -)] : -=121 => -839 agree
STR : [-=120] : LETTERS : [(0 2 1 = -)] : -=120 => -840 agree
STR : [-=12-] : LETTERS : [(- 2 1 = -)] : -=12- => -841 agree
STR : [-=12=] : LETTERS : [(= 2 1 = -)] : -=12= => -842 agree
STR : [-=112] : LETTERS : [(2 1 1 = -)] : -=112 => -843 agree
STR : [-=111] : LETTERS : [(1 1 1 = -)] : -=111 => -844 agree
STR : [-=110] : LETTERS : [(0 1 1 = -)] : -=110 => -845 agree
STR : [-=11-] : LETTERS : [(- 1 1 = -)] : -=11- => -846 agree
STR : [-=11=] : LETTERS : [(= 1 1 = -)] : -=11= => -847 agree
STR : [-=102] : LETTERS : [(2 0 1 = -)] : -=102 => -848 agree
STR : [-=101] : LETTERS : [(1 0 1 = -)] : -=101 => -849 agree
STR : [-=100] : LETTERS : [(0 0 1 = -)] : -=100 => -850 agree
STR : [-=10-] : LETTERS : [(- 0 1 = -)] : -=10- => -851 agree
STR : [-=10=] : LETTERS : [(= 0 1 = -)] : -=10= => -852 agree
STR : [-=1-2] : LETTERS : [(2 - 1 = -)] : -=1-2 => -853 agree
STR : [-=1-1] : LETTERS : [(1 - 1 = -)] : -=1-1 => -854 agree
STR : [-=1-0] : LETTERS : [(0 - 1 = -)] : -=1-0 => -855 agree
STR : [-=1--] : LETTERS : [(- - 1 = -)] : -=1-- => -856 agree
STR : [-=1-=] : LETTERS : [(= - 1 = -)] : -=1-= => -857 agree
STR : [-=1=2] : LETTERS : [(2 = 1 = -)] : -=1=2 => -858 agree
STR : [-=1=1] : LETTERS : [(1 = 1 = -)] : -=1=1 => -859 agree
STR : [-=1=0] : LETTERS : [(0 = 1 = -)] : -=1=0 => -860 agree
STR : [-=1=-] : LETTERS : [(- = 1 = -)] : -=1=- => -861 agree
STR : [-=1==] : LETTERS : [(= = 1 = -)] : -=1== => -862 agree
STR : [-=022] : LETTERS : [(2 2 0 = -)] : -=022 => -863 agree
STR : [-=021] : LETTERS : [(1 2 0 = -)] : -=021 => -864 agree
STR : [-=020] : LETTERS : [(0 2 0 = -)] : -=020 => -865 agree
STR : [-=02-] : LETTERS : [(- 2 0 = -)] : -=02- => -866 agree
STR : [-=02=] : LETTERS : [(= 2 0 = -)] : -=02= => -867 agree
STR : [-=012] : LETTERS : [(2 1 0 = -)] : -=012 => -868 agree
STR : [-=011] : LETTERS : [(1 1 0 = -)] : -=011 => -869 agree
STR : [-=010] : LETTERS : [(0 1 0 = -)] : -=010 => -870 agree
STR : [-=01-] : LETTERS : [(- 1 0 = -)] : -=01- => -871 agree
STR : [-=01=] : LETTERS : [(= 1 0 = -)] : -=01= => -872 agree
STR : [-=002] : LETTERS : [(2 0 0 = -)] : -=002 => -873 agree
STR : [-=001] : LETTERS : [(1 0 0 = -)] : -=001 => -874 agree
STR : [-=000] : LETTERS : [(0 0 0 = -)] : -=000 => -875 agree
STR : [-=00-] : LETTERS : [(- 0 0 = -)] : -=00- => -876 agree
STR : [-=00=] : LETTERS : [(= 0 0 = -)] : -=00= => -877 agree
STR : [-=0-2] : LETTERS : [(2 - 0 = -)] : -=0-2 => -878 agree
STR : [-=0-1] : LETTERS : [(1 - 0 = -)] : -=0-1 => -879 agree
STR : [-=0-0] : LETTERS : [(0 - 0 = -)] : -=0-0 => -880 agree
STR : [-=0--] : LETTERS : [(- - 0 = -)] : -=0-- => -881 agree
STR : [-=0-=] : LETTERS : [(= - 0 = -)] : -=0-= => -882 agree
STR : [-=0=2] : LETTERS : [(2 = 0 = -)] : -=0=2 => -883 agree
STR : [-=0=1] : LETTERS : [(1 = 0 = -)] : -=0=1 => -884 agree
STR : [-=0=0] : LETTERS : [(0 = 0 = -)] : -=0=0 => -885 agree
STR : [-=0=-] : LETTERS : [(- = 0 = -)] : -=0=- => -886 agree
STR : [-=0==] : LETTERS : [(= = 0 = -)] : -=0== => -887 agree
STR : [-=-22] : LETTERS : [(2 2 - = -)] : -=-22 => -888 agree
STR : [-=-21] : LETTERS : [(1 2 - = -)] : -=-21 => -889 agree
STR : [-=-20] : LETTERS : [(0 2 - = -)] : -=-20 => -890 agree
STR : [-=-2-] : LETTERS : [(- 2 - = -)] : -=-2- => -891 agree
STR : [-=-2=] : LETTERS : [(= 2 - = -)] : -=-2= => -892 agree
STR : [-=-12] : LETTERS : [(2 1 - = -)] : -=-12 => -893 agree
STR : [-=-11] : LETTERS : [(1 1 - = -)] : -=-11 => -894 agree
STR : [-=-10] : LETTERS : [(0 1 - = -)] : -=-10 => -895 agree
STR : [-=-1-] : LETTERS : [(- 1 - = -)] : -=-1- => -896 agree
STR : [-=-1=] : LETTERS : [(= 1 - = -)] : -=-1= => -897 agree
STR : [-=-02] : LETTERS : [(2 0 - = -)] : -=-02 => -898 agree
STR : [-=-01] : LETTERS : [(1 0 - = -)] : -=-01 => -899 agree
STR : [-=-00] : LETTERS : [(0 0 - = -)] : -=-00 => -900 agree
STR : [-=-0-] : LETTERS : [(- 0 - = -)] : -=-0- => -901 agree
STR : [-=-0=] : LETTERS : [(= 0 - = -)] : -=-0= => -902 agree
STR : [-=--2] : LETTERS : [(2 - - = -)] : -=--2 => -903 agree
STR : [-=--1] : LETTERS : [(1 - - = -)] : -=--1 => -904 agree
STR : [-=--0] : LETTERS : [(0 - - = -)] : -=--0 => -905 agree
STR : [-=---] : LETTERS : [(- - - = -)] : -=--- => -906 agree
STR : [-=--=] : LETTERS : [(= - - = -)] : -=--= => -907 agree
STR : [-=-=2] : LETTERS : [(2 = - = -)] : -=-=2 => -908 agree
STR : [-=-=1] : LETTERS : [(1 = - = -)] : -=-=1 => -909 agree
STR : [-=-=0] : LETTERS : [(0 = - = -)] : -=-=0 => -910 agree
STR : [-=-=-] : LETTERS : [(- = - = -)] : -=-=- => -911 agree
STR : [-=-==] : LETTERS : [(= = - = -)] : -=-== => -912 agree
STR : [-==22] : LETTERS : [(2 2 = = -)] : -==22 => -913 agree
STR : [-==21] : LETTERS : [(1 2 = = -)] : -==21 => -914 agree
STR : [-==20] : LETTERS : [(0 2 = = -)] : -==20 => -915 agree
STR : [-==2-] : LETTERS : [(- 2 = = -)] : -==2- => -916 agree
STR : [-==2=] : LETTERS : [(= 2 = = -)] : -==2= => -917 agree
STR : [-==12] : LETTERS : [(2 1 = = -)] : -==12 => -918 agree
STR : [-==11] : LETTERS : [(1 1 = = -)] : -==11 => -919 agree
STR : [-==10] : LETTERS : [(0 1 = = -)] : -==10 => -920 agree
STR : [-==1-] : LETTERS : [(- 1 = = -)] : -==1- => -921 agree
STR : [-==1=] : LETTERS : [(= 1 = = -)] : -==1= => -922 agree
STR : [-==02] : LETTERS : [(2 0 = = -)] : -==02 => -923 agree
STR : [-==01] : LETTERS : [(1 0 = = -)] : -==01 => -924 agree
STR : [-==00] : LETTERS : [(0 0 = = -)] : -==00 => -925 agree
STR : [-==0-] : LETTERS : [(- 0 = = -)] : -==0- => -926 agree
STR : [-==0=] : LETTERS : [(= 0 = = -)] : -==0= => -927 agree
STR : [-==-2] : LETTERS : [(2 - = = -)] : -==-2 => -928 agree
STR : [-==-1] : LETTERS : [(1 - = = -)] : -==-1 => -929 agree
STR : [-==-0] : LETTERS : [(0 - = = -)] : -==-0 => -930 agree
STR : [-==--] : LETTERS : [(- - = = -)] : -==-- => -931 agree
STR : [-==-=] : LETTERS : [(= - = = -)] : -==-= => -932 agree
STR : [-===2] : LETTERS : [(2 = = = -)] : -===2 => -933 agree
STR : [-===1] : LETTERS : [(1 = = = -)] : -===1 => -934 agree
STR : [-===0] : LETTERS : [(0 = = = -)] : -===0 => -935 agree
STR : [-===-] : LETTERS : [(- = = = -)] : -===- => -936 agree
STR : [-====] : LETTERS : [(= = = = -)] : -==== => -937 agree
STR : [=2222] : LETTERS : [(2 2 2 2 =)] : =2222 => -938 agree
STR : [=2221] : LETTERS : [(1 2 2 2 =)] : =2221 => -939 agree
STR : [=2220] : LETTERS : [(0 2 2 2 =)] : =2220 => -940 agree
STR : [=222-] : LETTERS : [(- 2 2 2 =)] : =222- => -941 agree
STR : [=222=] : LETTERS : [(= 2 2 2 =)] : =222= => -942 agree
STR : [=2212] : LETTERS : [(2 1 2 2 =)] : =2212 => -943 agree
STR : [=2211] : LETTERS : [(1 1 2 2 =)] : =2211 => -944 agree
STR : [=2210] : LETTERS : [(0 1 2 2 =)] : =2210 => -945 agree
STR : [=221-] : LETTERS : [(- 1 2 2 =)] : =221- => -946 agree
STR : [=221=] : LETTERS : [(= 1 2 2 =)] : =221= => -947 agree
STR : [=2202] : LETTERS : [(2 0 2 2 =)] : =2202 => -948 agree
STR : [=2201] : LETTERS : [(1 0 2 2 =)] : =2201 => -949 agree
STR : [=2200] : LETTERS : [(0 0 2 2 =)] : =2200 => -950 agree
STR : [=220-] : LETTERS : [(- 0 2 2 =)] : =220- => -951 agree
STR : [=220=] : LETTERS : [(= 0 2 2 =)] : =220= => -952 agree
STR : [=22-2] : LETTERS : [(2 - 2 2 =)] : =22-2 => -953 agree
STR : [=22-1] : LETTERS : [(1 - 2 2 =)] : =22-1 => -954 agree
STR : [=22-0] : LETTERS : [(0 - 2 2 =)] : =22-0 => -955 agree
STR : [=22--] : LETTERS : [(- - 2 2 =)] : =22-- => -956 agree
STR : [=22-=] : LETTERS : [(= - 2 2 =)] : =22-= => -957 agree
STR : [=22=2] : LETTERS : [(2 = 2 2 =)] : =22=2 => -958 agree
STR : [=22=1] : LETTERS : [(1 = 2 2 =)] : =22=1 => -959 agree
STR : [=22=0] : LETTERS : [(0 = 2 2 =)] : =22=0 => -960 agree
STR : [=22=-] : LETTERS : [(- = 2 2 =)] : =22=- => -961 agree
STR : [=22==] : LETTERS : [(= = 2 2 =)] : =22== => -962 agree
STR : [=2122] : LETTERS : [(2 2 1 2 =)] : =2122 => -963 agree
STR : [=2121] : LETTERS : [(1 2 1 2 =)] : =2121 => -964 agree
STR : [=2120] : LETTERS : [(0 2 1 2 =)] : =2120 => -965 agree
STR : [=212-] : LETTERS : [(- 2 1 2 =)] : =212- => -966 agree
STR : [=212=] : LETTERS : [(= 2 1 2 =)] : =212= => -967 agree
STR : [=2112] : LETTERS : [(2 1 1 2 =)] : =2112 => -968 agree
STR : [=2111] : LETTERS : [(1 1 1 2 =)] : =2111 => -969 agree
STR : [=2110] : LETTERS : [(0 1 1 2 =)] : =2110 => -970 agree
STR : [=211-] : LETTERS : [(- 1 1 2 =)] : =211- => -971 agree
STR : [=211=] : LETTERS : [(= 1 1 2 =)] : =211= => -972 agree
STR : [=2102] : LETTERS : [(2 0 1 2 =)] : =2102 => -973 agree
STR : [=2101] : LETTERS : [(1 0 1 2 =)] : =2101 => -974 agree
STR : [=2100] : LETTERS : [(0 0 1 2 =)] : =2100 => -975 agree
STR : [=210-] : LETTERS : [(- 0 1 2 =)] : =210- => -976 agree
STR : [=210=] : LETTERS : [(= 0 1 2 =)] : =210= => -977 agree
STR : [=21-2] : LETTERS : [(2 - 1 2 =)] : =21-2 => -978 agree
STR : [=21-1] : LETTERS : [(1 - 1 2 =)] : =21-1 => -979 agree
STR : [=21-0] : LETTERS : [(0 - 1 2 =)] : =21-0 => -980 agree
STR : [=21--] : LETTERS : [(- - 1 2 =)] : =21-- => -981 agree
STR : [=21-=] : LETTERS : [(= - 1 2 =)] : =21-= => -982 agree
STR : [=21=2] : LETTERS : [(2 = 1 2 =)] : =21=2 => -983 agree
STR : [=21=1] : LETTERS : [(1 = 1 2 =)] : =21=1 => -984 agree
STR : [=21=0] : LETTERS : [(0 = 1 2 =)] : =21=0 => -985 agree
STR : [=21=-] : LETTERS : [(- = 1 2 =)] : =21=- => -986 agree
STR : [=21==] : LETTERS : [(= = 1 2 =)] : =21== => -987 agree
STR : [=2022] : LETTERS : [(2 2 0 2 =)] : =2022 => -988 agree
STR : [=2021] : LETTERS : [(1 2 0 2 =)] : =2021 => -989 agree
STR : [=2020] : LETTERS : [(0 2 0 2 =)] : =2020 => -990 agree
STR : [=202-] : LETTERS : [(- 2 0 2 =)] : =202- => -991 agree
STR : [=202=] : LETTERS : [(= 2 0 2 =)] : =202= => -992 agree
STR : [=2012] : LETTERS : [(2 1 0 2 =)] : =2012 => -993 agree
STR : [=2011] : LETTERS : [(1 1 0 2 =)] : =2011 => -994 agree
STR : [=2010] : LETTERS : [(0 1 0 2 =)] : =2010 => -995 agree
STR : [=201-] : LETTERS : [(- 1 0 2 =)] : =201- => -996 agree
STR : [=201=] : LETTERS : [(= 1 0 2 =)] : =201= => -997 agree
STR : [=2002] : LETTERS : [(2 0 0 2 =)] : =2002 => -998 agree
STR : [=2001] : LETTERS : [(1 0 0 2 =)] : =2001 => -999 agree
STR : [=2000] : LETTERS : [(0 0 0 2 =)] : =2000 => -1000 agree
STR : [=200-] : LETTERS : [(- 0 0 2 =)] : =200- => -1001 agree
STR : [=200=] : LETTERS : [(= 0 0 2 =)] : =200= => -1002 agree
STR : [=20-2] : LETTERS : [(2 - 0 2 =)] : =20-2 => -1003 agree
STR : [=20-1] : LETTERS : [(1 - 0 2 =)] : =20-1 => -1004 agree
STR : [=20-0] : LETTERS : [(0 - 0 2 =)] : =20-0 => -1005 agree
STR : [=20--] : LETTERS : [(- - 0 2 =)] : =20-- => -1006 agree
STR : [=20-=] : LETTERS : [(= - 0 2 =)] : =20-= => -1007 agree
STR : [=20=2] : LETTERS : [(2 = 0 2 =)] : =20=2 => -1008 agree
STR : [=20=1] : LETTERS : [(1 = 0 2 =)] : =20=1 => -1009 agree
STR : [=20=0] : LETTERS : [(0 = 0 2 =)] : =20=0 => -1010 agree
STR : [=20=-] : LETTERS : [(- = 0 2 =)] : =20=- => -1011 agree
STR : [=20==] : LETTERS : [(= = 0 2 =)] : =20== => -1012 agree
STR : [=2-22] : LETTERS : [(2 2 - 2 =)] : =2-22 => -1013 agree
STR : [=2-21] : LETTERS : [(1 2 - 2 =)] : =2-21 => -1014 agree
STR : [=2-20] : LETTERS : [(0 2 - 2 =)] : =2-20 => -1015 agree
STR : [=2-2-] : LETTERS : [(- 2 - 2 =)] : =2-2- => -1016 agree
STR : [=2-2=] : LETTERS : [(= 2 - 2 =)] : =2-2= => -1017 agree
STR : [=2-12] : LETTERS : [(2 1 - 2 =)] : =2-12 => -1018 agree
STR : [=2-11] : LETTERS : [(1 1 - 2 =)] : =2-11 => -1019 agree
STR : [=2-10] : LETTERS : [(0 1 - 2 =)] : =2-10 => -1020 agree
STR : [=2-1-] : LETTERS : [(- 1 - 2 =)] : =2-1- => -1021 agree
STR : [=2-1=] : LETTERS : [(= 1 - 2 =)] : =2-1= => -1022 agree
STR : [=2-02] : LETTERS : [(2 0 - 2 =)] : =2-02 => -1023 agree
STR : [=2-01] : LETTERS : [(1 0 - 2 =)] : =2-01 => -1024 agree
STR : [=2-00] : LETTERS : [(0 0 - 2 =)] : =2-00 => -1025 agree
STR : [=2-0-] : LETTERS : [(- 0 - 2 =)] : =2-0- => -1026 agree
STR : [=2-0=] : LETTERS : [(= 0 - 2 =)] : =2-0= => -1027 agree
STR : [=2--2] : LETTERS : [(2 - - 2 =)] : =2--2 => -1028 agree
STR : [=2--1] : LETTERS : [(1 - - 2 =)] : =2--1 => -1029 agree
STR : [=2--0] : LETTERS : [(0 - - 2 =)] : =2--0 => -1030 agree
STR : [=2---] : LETTERS : [(- - - 2 =)] : =2--- => -1031 agree
STR : [=2--=] : LETTERS : [(= - - 2 =)] : =2--= => -1032 agree
STR : [=2-=2] : LETTERS : [(2 = - 2 =)] : =2-=2 => -1033 agree
STR : [=2-=1] : LETTERS : [(1 = - 2 =)] : =2-=1 => -1034 agree
STR : [=2-=0] : LETTERS : [(0 = - 2 =)] : =2-=0 => -1035 agree
STR : [=2-=-] : LETTERS : [(- = - 2 =)] : =2-=- => -1036 agree
STR : [=2-==] : LETTERS : [(= = - 2 =)] : =2-== => -1037 agree
STR : [=2=22] : LETTERS : [(2 2 = 2 =)] : =2=22 => -1038 agree
STR : [=2=21] : LETTERS : [(1 2 = 2 =)] : =2=21 => -1039 agree
STR : [=2=20] : LETTERS : [(0 2 = 2 =)] : =2=20 => -1040 agree
STR : [=2=2-] : LETTERS : [(- 2 = 2 =)] : =2=2- => -1041 agree
STR : [=2=2=] : LETTERS : [(= 2 = 2 =)] : =2=2= => -1042 agree
STR : [=2=12] : LETTERS : [(2 1 = 2 =)] : =2=12 => -1043 agree
STR : [=2=11] : LETTERS : [(1 1 = 2 =)] : =2=11 => -1044 agree
STR : [=2=10] : LETTERS : [(0 1 = 2 =)] : =2=10 => -1045 agree
STR : [=2=1-] : LETTERS : [(- 1 = 2 =)] : =2=1- => -1046 agree
STR : [=2=1=] : LETTERS : [(= 1 = 2 =)] : =2=1= => -1047 agree
STR : [=2=02] : LETTERS : [(2 0 = 2 =)] : =2=02 => -1048 agree
STR : [=2=01] : LETTERS : [(1 0 = 2 =)] : =2=01 => -1049 agree
STR : [=2=00] : LETTERS : [(0 0 = 2 =)] : =2=00 => -1050 agree
STR : [=2=0-] : LETTERS : [(- 0 = 2 =)] : =2=0- => -1051 agree
STR : [=2=0=] : LETTERS : [(= 0 = 2 =)] : =2=0= => -1052 agree
STR : [=2=-2] : LETTERS : [(2 - = 2 =)] : =2=-2 => -1053 agree
STR : [=2=-1] : LETTERS : [(1 - = 2 =)] : =2=-1 => -1054 agree
STR : [=2=-0] : LETTERS : [(0 - = 2 =)] : =2=-0 => -1055 agree
STR : [=2=--] : LETTERS : [(- - = 2 =)] : =2=-- => -1056 agree
STR : [=2=-=] : LETTERS : [(= - = 2 =)] : =2=-= => -1057 agree
STR : [=2==2] : LETTERS : [(2 = = 2 =)] : =2==2 => -1058 agree
STR : [=2==1] : LETTERS : [(1 = = 2 =)] : =2==1 => -1059 agree
STR : [=2==0] : LETTERS : [(0 = = 2 =)] : =2==0 => -1060 agree
STR : [=2==-] : LETTERS : [(- = = 2 =)] : =2==- => -1061 agree
STR : [=2===] : LETTERS : [(= = = 2 =)] : =2=== => -1062 agree
STR : [=1222] : LETTERS : [(2 2 2 1 =)] : =1222 => -1063 agree
STR : [=1221] : LETTERS : [(1 2 2 1 =)] : =1221 => -1064 agree
STR : [=1220] : LETTERS : [(0 2 2 1 =)] : =1220 => -1065 agree
STR : [=122-] : LETTERS : [(- 2 2 1 =)] : =122- => -1066 agree
STR : [=122=] : LETTERS : [(= 2 2 1 =)] : =122= => -1067 agree
STR : [=1212] : LETTERS : [(2 1 2 1 =)] : =1212 => -1068 agree
STR : [=1211] : LETTERS : [(1 1 2 1 =)] : =1211 => -1069 agree
STR : [=1210] : LETTERS : [(0 1 2 1 =)] : =1210 => -1070 agree
STR : [=121-] : LETTERS : [(- 1 2 1 =)] : =121- => -1071 agree
STR : [=121=] : LETTERS : [(= 1 2 1 =)] : =121= => -1072 agree
STR : [=1202] : LETTERS : [(2 0 2 1 =)] : =1202 => -1073 agree
STR : [=1201] : LETTERS : [(1 0 2 1 =)] : =1201 => -1074 agree
STR : [=1200] : LETTERS : [(0 0 2 1 =)] : =1200 => -1075 agree
STR : [=120-] : LETTERS : [(- 0 2 1 =)] : =120- => -1076 agree
STR : [=120=] : LETTERS : [(= 0 2 1 =)] : =120= => -1077 agree
STR : [=12-2] : LETTERS : [(2 - 2 1 =)] : =12-2 => -1078 agree
STR : [=12-1] : LETTERS : [(1 - 2 1 =)] : =12-1 => -1079 agree
STR : [=12-0] : LETTERS : [(0 - 2 1 =)] : =12-0 => -1080 agree
STR : [=12--] : LETTERS : [(- - 2 1 =)] : =12-- => -1081 agree
STR : [=12-=] : LETTERS : [(= - 2 1 =)] : =12-= => -1082 agree
STR : [=12=2] : LETTERS : [(2 = 2 1 =)] : =12=2 => -1083 agree
STR : [=12=1] : LETTERS : [(1 = 2 1 =)] : =12=1 => -1084 agree
STR : [=12=0] : LETTERS : [(0 = 2 1 =)] : =12=0 => -1085 agree
STR : [=12=-] : LETTERS : [(- = 2 1 =)] : =12=- => -1086 agree
STR : [=12==] : LETTERS : [(= = 2 1 =)] : =12== => -1087 agree
STR : [=1122] : LETTERS : [(2 2 1 1 =)] : =1122 => -1088 agree
STR : [=1121] : LETTERS : [(1 2 1 1 =)] : =1121 => -1089 agree
STR : [=1120] : LETTERS : [(0 2 1 1 =)] : =1120 => -1090 agree
STR : [=112-] : LETTERS : [(- 2 1 1 =)] : =112- => -1091 agree
STR : [=112=] : LETTERS : [(= 2 1 1 =)] : =112= => -1092 agree
STR : [=1112] : LETTERS : [(2 1 1 1 =)] : =1112 => -1093 agree
STR : [=1111] : LETTERS : [(1 1 1 1 =)] : =1111 => -1094 agree
STR : [=1110] : LETTERS : [(0 1 1 1 =)] : =1110 => -1095 agree
STR : [=111-] : LETTERS : [(- 1 1 1 =)] : =111- => -1096 agree
STR : [=111=] : LETTERS : [(= 1 1 1 =)] : =111= => -1097 agree
STR : [=1102] : LETTERS : [(2 0 1 1 =)] : =1102 => -1098 agree
STR : [=1101] : LETTERS : [(1 0 1 1 =)] : =1101 => -1099 agree
STR : [=1100] : LETTERS : [(0 0 1 1 =)] : =1100 => -1100 agree
STR : [=110-] : LETTERS : [(- 0 1 1 =)] : =110- => -1101 agree
STR : [=110=] : LETTERS : [(= 0 1 1 =)] : =110= => -1102 agree
STR : [=11-2] : LETTERS : [(2 - 1 1 =)] : =11-2 => -1103 agree
STR : [=11-1] : LETTERS : [(1 - 1 1 =)] : =11-1 => -1104 agree
STR : [=11-0] : LETTERS : [(0 - 1 1 =)] : =11-0 => -1105 agree
STR : [=11--] : LETTERS : [(- - 1 1 =)] : =11-- => -1106 agree
STR : [=11-=] : LETTERS : [(= - 1 1 =)] : =11-= => -1107 agree
STR : [=11=2] : LETTERS : [(2 = 1 1 =)] : =11=2 => -1108 agree
STR : [=11=1] : LETTERS : [(1 = 1 1 =)] : =11=1 => -1109 agree
STR : [=11=0] : LETTERS : [(0 = 1 1 =)] : =11=0 => -1110 agree
STR : [=11=-] : LETTERS : [(- = 1 1 =)] : =11=- => -1111 agree
STR : [=11==] : LETTERS : [(= = 1 1 =)] : =11== => -1112 agree
STR : [=1022] : LETTERS : [(2 2 0 1 =)] : =1022 => -1113 agree
STR : [=1021] : LETTERS : [(1 2 0 1 =)] : =1021 => -1114 agree
STR : [=1020] : LETTERS : [(0 2 0 1 =)] : =1020 => -1115 agree
STR : [=102-] : LETTERS : [(- 2 0 1 =)] : =102- => -1116 agree
STR : [=102=] : LETTERS : [(= 2 0 1 =)] : =102= => -1117 agree
STR : [=1012] : LETTERS : [(2 1 0 1 =)] : =1012 => -1118 agree
STR : [=1011] : LETTERS : [(1 1 0 1 =)] : =1011 => -1119 agree
STR : [=1010] : LETTERS : [(0 1 0 1 =)] : =1010 => -1120 agree
STR : [=101-] : LETTERS : [(- 1 0 1 =)] : =101- => -1121 agree
STR : [=101=] : LETTERS : [(= 1 0 1 =)] : =101= => -1122 agree
STR : [=1002] : LETTERS : [(2 0 0 1 =)] : =1002 => -1123 agree
STR : [=1001] : LETTERS : [(1 0 0 1 =)] : =1001 => -1124 agree
STR : [=1000] : LETTERS : [(0 0 0 1 =)] : =1000 => -1125 agree
STR : [=100-] : LETTERS : [(- 0 0 1 =)] : =100- => -1126 agree
STR : [=100=] : LETTERS : [(= 0 0 1 =)] : =100= => -1127 agree
STR : [=10-2] : LETTERS : [(2 - 0 1 =)] : =10-2 => -1128 agree
STR : [=10-1] : LETTERS : [(1 - 0 1 =)] : =10-1 => -1129 agree
STR : [=10-0] : LETTERS : [(0 - 0 1 =)] : =10-0 => -1130 agree
STR : [=10--] : LETTERS : [(- - 0 1 =)] : =10-- => -1131 agree
STR : [=10-=] : LETTERS : [(= - 0 1 =)] : =10-= => -1132 agree
STR : [=10=2] : LETTERS : [(2 = 0 1 =)] : =10=2 => -1133 agree
STR : [=10=1] : LETTERS : [(1 = 0 1 =)] : =10=1 => -1134 agree
STR : [=10=0] : LETTERS : [(0 = 0 1 =)] : =10=0 => -1135 agree
STR : [=10=-] : LETTERS : [(- = 0 1 =)] : =10=- => -1136 agree
STR : [=10==] : LETTERS : [(= = 0 1 =)] : =10== => -1137 agree
STR : [=1-22] : LETTERS : [(2 2 - 1 =)] : =1-22 => -1138 agree
STR : [=1-21] : LETTERS : [(1 2 - 1 =)] : =1-21 => -1139 agree
STR : [=1-20] : LETTERS : [(0 2 - 1 =)] : =1-20 => -1140 agree
STR : [=1-2-] : LETTERS : [(- 2 - 1 =)] : =1-2- => -1141 agree
STR : [=1-2=] : LETTERS : [(= 2 - 1 =)] : =1-2= => -1142 agree
STR : [=1-12] : LETTERS : [(2 1 - 1 =)] : =1-12 => -1143 agree
STR : [=1-11] : LETTERS : [(1 1 - 1 =)] : =1-11 => -1144 agree
STR : [=1-10] : LETTERS : [(0 1 - 1 =)] : =1-10 => -1145 agree
STR : [=1-1-] : LETTERS : [(- 1 - 1 =)] : =1-1- => -1146 agree
STR : [=1-1=] : LETTERS : [(= 1 - 1 =)] : =1-1= => -1147 agree
STR : [=1-02] : LETTERS : [(2 0 - 1 =)] : =1-02 => -1148 agree
STR : [=1-01] : LETTERS : [(1 0 - 1 =)] : =1-01 => -1149 agree
STR : [=1-00] : LETTERS : [(0 0 - 1 =)] : =1-00 => -1150 agree
STR : [=1-0-] : LETTERS : [(- 0 - 1 =)] : =1-0- => -1151 agree
STR : [=1-0=] : LETTERS : [(= 0 - 1 =)] : =1-0= => -1152 agree
STR : [=1--2] : LETTERS : [(2 - - 1 =)] : =1--2 => -1153 agree
STR : [=1--1] : LETTERS : [(1 - - 1 =)] : =1--1 => -1154 agree
STR : [=1--0] : LETTERS : [(0 - - 1 =)] : =1--0 => -1155 agree
STR : [=1---] : LETTERS : [(- - - 1 =)] : =1--- => -1156 agree
STR : [=1--=] : LETTERS : [(= - - 1 =)] : =1--= => -1157 agree
STR : [=1-=2] : LETTERS : [(2 = - 1 =)] : =1-=2 => -1158 agree
STR : [=1-=1] : LETTERS : [(1 = - 1 =)] : =1-=1 => -1159 agree
STR : [=1-=0] : LETTERS : [(0 = - 1 =)] : =1-=0 => -1160 agree
STR : [=1-=-] : LETTERS : [(- = - 1 =)] : =1-=- => -1161 agree
STR : [=1-==] : LETTERS : [(= = - 1 =)] : =1-== => -1162 agree
STR : [=1=22] : LETTERS : [(2 2 = 1 =)] : =1=22 => -1163 agree
STR : [=1=21] : LETTERS : [(1 2 = 1 =)] : =1=21 => -1164 agree
STR : [=1=20] : LETTERS : [(0 2 = 1 =)] : =1=20 => -1165 agree
STR : [=1=2-] : LETTERS : [(- 2 = 1 =)] : =1=2- => -1166 agree
STR : [=1=2=] : LETTERS : [(= 2 = 1 =)] : =1=2= => -1167 agree
STR : [=1=12] : LETTERS : [(2 1 = 1 =)] : =1=12 => -1168 agree
STR : [=1=11] : LETTERS : [(1 1 = 1 =)] : =1=11 => -1169 agree
STR : [=1=10] : LETTERS : [(0 1 = 1 =)] : =1=10 => -1170 agree
STR : [=1=1-] : LETTERS : [(- 1 = 1 =)] : =1=1- => -1171 agree
STR : [=1=1=] : LETTERS : [(= 1 = 1 =)] : =1=1= => -1172 agree
STR : [=1=02] : LETTERS : [(2 0 = 1 =)] : =1=02 => -1173 agree
STR : [=1=01] : LETTERS : [(1 0 = 1 =)] : =1=01 => -1174 agree
STR : [=1=00] : LETTERS : [(0 0 = 1 =)] : =1=00 => -1175 agree
STR : [=1=0-] : LETTERS : [(- 0 = 1 =)] : =1=0- => -1176 agree
STR : [=1=0=] : LETTERS : [(= 0 = 1 =)] : =1=0= => -1177 agree
STR : [=1=-2] : LETTERS : [(2 - = 1 =)] : =1=-2 => -1178 agree
STR : [=1=-1] : LETTERS : [(1 - = 1 =)] : =1=-1 => -1179 agree
STR : [=1=-0] : LETTERS : [(0 - = 1 =)] : =1=-0 => -1180 agree
STR : [=1=--] : LETTERS : [(- - = 1 =)] : =1=-- => -1181 agree
STR : [=1=-=] : LETTERS : [(= - = 1 =)] : =1=-= => -1182 agree
STR : [=1==2] : LETTERS : [(2 = = 1 =)] : =1==2 => -1183 agree
STR : [=1==1] : LETTERS : [(1 = = 1 =)] : =1==1 => -1184 agree
STR : [=1==0] : LETTERS : [(0 = = 1 =)] : =1==0 => -1185 agree
STR : [=1==-] : LETTERS : [(- = = 1 =)] : =1==- => -1186 agree
STR : [=1===] : LETTERS : [(= = = 1 =)] : =1=== => -1187 agree
STR : [=0222] : LETTERS : [(2 2 2 0 =)] : =0222 => -1188 agree
STR : [=0221] : LETTERS : [(1 2 2 0 =)] : =0221 => -1189 agree
STR : [=0220] : LETTERS : [(0 2 2 0 =)] : =0220 => -1190 agree
STR : [=022-] : LETTERS : [(- 2 2 0 =)] : =022- => -1191 agree
STR : [=022=] : LETTERS : [(= 2 2 0 =)] : =022= => -1192 agree
STR : [=0212] : LETTERS : [(2 1 2 0 =)] : =0212 => -1193 agree
STR : [=0211] : LETTERS : [(1 1 2 0 =)] : =0211 => -1194 agree
STR : [=0210] : LETTERS : [(0 1 2 0 =)] : =0210 => -1195 agree
STR : [=021-] : LETTERS : [(- 1 2 0 =)] : =021- => -1196 agree
STR : [=021=] : LETTERS : [(= 1 2 0 =)] : =021= => -1197 agree
STR : [=0202] : LETTERS : [(2 0 2 0 =)] : =0202 => -1198 agree
STR : [=0201] : LETTERS : [(1 0 2 0 =)] : =0201 => -1199 agree
STR : [=0200] : LETTERS : [(0 0 2 0 =)] : =0200 => -1200 agree
STR : [=020-] : LETTERS : [(- 0 2 0 =)] : =020- => -1201 agree
STR : [=020=] : LETTERS : [(= 0 2 0 =)] : =020= => -1202 agree
STR : [=02-2] : LETTERS : [(2 - 2 0 =)] : =02-2 => -1203 agree
STR : [=02-1] : LETTERS : [(1 - 2 0 =)] : =02-1 => -1204 agree
STR : [=02-0] : LETTERS : [(0 - 2 0 =)] : =02-0 => -1205 agree
STR : [=02--] : LETTERS : [(- - 2 0 =)] : =02-- => -1206 agree
STR : [=02-=] : LETTERS : [(= - 2 0 =)] : =02-= => -1207 agree
STR : [=02=2] : LETTERS : [(2 = 2 0 =)] : =02=2 => -1208 agree
STR : [=02=1] : LETTERS : [(1 = 2 0 =)] : =02=1 => -1209 agree
STR : [=02=0] : LETTERS : [(0 = 2 0 =)] : =02=0 => -1210 agree
STR : [=02=-] : LETTERS : [(- = 2 0 =)] : =02=- => -1211 agree
STR : [=02==] : LETTERS : [(= = 2 0 =)] : =02== => -1212 agree
STR : [=0122] : LETTERS : [(2 2 1 0 =)] : =0122 => -1213 agree
STR : [=0121] : LETTERS : [(1 2 1 0 =)] : =0121 => -1214 agree
STR : [=0120] : LETTERS : [(0 2 1 0 =)] : =0120 => -1215 agree
STR : [=012-] : LETTERS : [(- 2 1 0 =)] : =012- => -1216 agree
STR : [=012=] : LETTERS : [(= 2 1 0 =)] : =012= => -1217 agree
STR : [=0112] : LETTERS : [(2 1 1 0 =)] : =0112 => -1218 agree
STR : [=0111] : LETTERS : [(1 1 1 0 =)] : =0111 => -1219 agree
STR : [=0110] : LETTERS : [(0 1 1 0 =)] : =0110 => -1220 agree
STR : [=011-] : LETTERS : [(- 1 1 0 =)] : =011- => -1221 agree
STR : [=011=] : LETTERS : [(= 1 1 0 =)] : =011= => -1222 agree
STR : [=0102] : LETTERS : [(2 0 1 0 =)] : =0102 => -1223 agree
STR : [=0101] : LETTERS : [(1 0 1 0 =)] : =0101 => -1224 agree
STR : [=0100] : LETTERS : [(0 0 1 0 =)] : =0100 => -1225 agree
STR : [=010-] : LETTERS : [(- 0 1 0 =)] : =010- => -1226 agree
STR : [=010=] : LETTERS : [(= 0 1 0 =)] : =010= => -1227 agree
STR : [=01-2] : LETTERS : [(2 - 1 0 =)] : =01-2 => -1228 agree
STR : [=01-1] : LETTERS : [(1 - 1 0 =)] : =01-1 => -1229 agree
STR : [=01-0] : LETTERS : [(0 - 1 0 =)] : =01-0 => -1230 agree
STR : [=01--] : LETTERS : [(- - 1 0 =)] : =01-- => -1231 agree
STR : [=01-=] : LETTERS : [(= - 1 0 =)] : =01-= => -1232 agree
STR : [=01=2] : LETTERS : [(2 = 1 0 =)] : =01=2 => -1233 agree
STR : [=01=1] : LETTERS : [(1 = 1 0 =)] : =01=1 => -1234 agree
STR : [=01=0] : LETTERS : [(0 = 1 0 =)] : =01=0 => -1235 agree
STR : [=01=-] : LETTERS : [(- = 1 0 =)] : =01=- => -1236 agree
STR : [=01==] : LETTERS : [(= = 1 0 =)] : =01== => -1237 agree
STR : [=0022] : LETTERS : [(2 2 0 0 =)] : =0022 => -1238 agree
STR : [=0021] : LETTERS : [(1 2 0 0 =)] : =0021 => -1239 agree
STR : [=0020] : LETTERS : [(0 2 0 0 =)] : =0020 => -1240 agree
STR : [=002-] : LETTERS : [(- 2 0 0 =)] : =002- => -1241 agree
STR : [=002=] : LETTERS : [(= 2 0 0 =)] : =002= => -1242 agree
STR : [=0012] : LETTERS : [(2 1 0 0 =)] : =0012 => -1243 agree
STR : [=0011] : LETTERS : [(1 1 0 0 =)] : =0011 => -1244 agree
STR : [=0010] : LETTERS : [(0 1 0 0 =)] : =0010 => -1245 agree
STR : [=001-] : LETTERS : [(- 1 0 0 =)] : =001- => -1246 agree
STR : [=001=] : LETTERS : [(= 1 0 0 =)] : =001= => -1247 agree
STR : [=0002] : LETTERS : [(2 0 0 0 =)] : =0002 => -1248 agree
STR : [=0001] : LETTERS : [(1 0 0 0 =)] : =0001 => -1249 agree
STR : [=0000] : LETTERS : [(0 0 0 0 =)] : =0000 => -1250 agree
STR : [=000-] : LETTERS : [(- 0 0 0 =)] : =000- => -1251 agree
STR : [=000=] : LETTERS : [(= 0 0 0 =)] : =000= => -1252 agree
STR : [=00-2] : LETTERS : [(2 - 0 0 =)] : =00-2 => -1253 agree
STR : [=00-1] : LETTERS : [(1 - 0 0 =)] : =00-1 => -1254 agree
STR : [=00-0] : LETTERS : [(0 - 0 0 =)] : =00-0 => -1255 agree
STR : [=00--] : LETTERS : [(- - 0 0 =)] : =00-- => -1256 agree
STR : [=00-=] : LETTERS : [(= - 0 0 =)] : =00-= => -1257 agree
STR : [=00=2] : LETTERS : [(2 = 0 0 =)] : =00=2 => -1258 agree
STR : [=00=1] : LETTERS : [(1 = 0 0 =)] : =00=1 => -1259 agree
STR : [=00=0] : LETTERS : [(0 = 0 0 =)] : =00=0 => -1260 agree
STR : [=00=-] : LETTERS : [(- = 0 0 =)] : =00=- => -1261 agree
STR : [=00==] : LETTERS : [(= = 0 0 =)] : =00== => -1262 agree
STR : [=0-22] : LETTERS : [(2 2 - 0 =)] : =0-22 => -1263 agree
STR : [=0-21] : LETTERS : [(1 2 - 0 =)] : =0-21 => -1264 agree
STR : [=0-20] : LETTERS : [(0 2 - 0 =)] : =0-20 => -1265 agree
STR : [=0-2-] : LETTERS : [(- 2 - 0 =)] : =0-2- => -1266 agree
STR : [=0-2=] : LETTERS : [(= 2 - 0 =)] : =0-2= => -1267 agree
STR : [=0-12] : LETTERS : [(2 1 - 0 =)] : =0-12 => -1268 agree
STR : [=0-11] : LETTERS : [(1 1 - 0 =)] : =0-11 => -1269 agree
STR : [=0-10] : LETTERS : [(0 1 - 0 =)] : =0-10 => -1270 agree
STR : [=0-1-] : LETTERS : [(- 1 - 0 =)] : =0-1- => -1271 agree
STR : [=0-1=] : LETTERS : [(= 1 - 0 =)] : =0-1= => -1272 agree
STR : [=0-02] : LETTERS : [(2 0 - 0 =)] : =0-02 => -1273 agree
STR : [=0-01] : LETTERS : [(1 0 - 0 =)] : =0-01 => -1274 agree
STR : [=0-00] : LETTERS : [(0 0 - 0 =)] : =0-00 => -1275 agree
STR : [=0-0-] : LETTERS : [(- 0 - 0 =)] : =0-0- => -1276 agree
STR : [=0-0=] : LETTERS : [(= 0 - 0 =)] : =0-0= => -1277 agree
STR : [=0--2] : LETTERS : [(2 - - 0 =)] : =0--2 => -1278 agree
STR : [=0--1] : LETTERS : [(1 - - 0 =)] : =0--1 => -1279 agree
STR : [=0--0] : LETTERS : [(0 - - 0 =)] : =0--0 => -1280 agree
STR : [=0---] : LETTERS : [(- - - 0 =)] : =0--- => -1281 agree
STR : [=0--=] : LETTERS : [(= - - 0 =)] : =0--= => -1282 agree
STR : [=0-=2] : LETTERS : [(2 = - 0 =)] : =0-=2 => -1283 agree
STR : [=0-=1] : LETTERS : [(1 = - 0 =)] : =0-=1 => -1284 agree
STR : [=0-=0] : LETTERS : [(0 = - 0 =)] : =0-=0 => -1285 agree
STR : [=0-=-] : LETTERS : [(- = - 0 =)] : =0-=- => -1286 agree
STR : [=0-==] : LETTERS : [(= = - 0 =)] : =0-== => -1287 agree
STR : [=0=22] : LETTERS : [(2 2 = 0 =)] : =0=22 => -1288 agree
STR : [=0=21] : LETTERS : [(1 2 = 0 =)] : =0=21 => -1289 agree
STR : [=0=20] : LETTERS : [(0 2 = 0 =)] : =0=20 => -1290 agree
STR : [=0=2-] : LETTERS : [(- 2 = 0 =)] : =0=2- => -1291 agree
STR : [=0=2=] : LETTERS : [(= 2 = 0 =)] : =0=2= => -1292 agree
STR : [=0=12] : LETTERS : [(2 1 = 0 =)] : =0=12 => -1293 agree
STR : [=0=11] : LETTERS : [(1 1 = 0 =)] : =0=11 => -1294 agree
STR : [=0=10] : LETTERS : [(0 1 = 0 =)] : =0=10 => -1295 agree
STR : [=0=1-] : LETTERS : [(- 1 = 0 =)] : =0=1- => -1296 agree
STR : [=0=1=] : LETTERS : [(= 1 = 0 =)] : =0=1= => -1297 agree
STR : [=0=02] : LETTERS : [(2 0 = 0 =)] : =0=02 => -1298 agree
STR : [=0=01] : LETTERS : [(1 0 = 0 =)] : =0=01 => -1299 agree
STR : [=0=00] : LETTERS : [(0 0 = 0 =)] : =0=00 => -1300 agree
STR : [=0=0-] : LETTERS : [(- 0 = 0 =)] : =0=0- => -1301 agree
STR : [=0=0=] : LETTERS : [(= 0 = 0 =)] : =0=0= => -1302 agree
STR : [=0=-2] : LETTERS : [(2 - = 0 =)] : =0=-2 => -1303 agree
STR : [=0=-1] : LETTERS : [(1 - = 0 =)] : =0=-1 => -1304 agree
STR : [=0=-0] : LETTERS : [(0 - = 0 =)] : =0=-0 => -1305 agree
STR : [=0=--] : LETTERS : [(- - = 0 =)] : =0=-- => -1306 agree
STR : [=0=-=] : LETTERS : [(= - = 0 =)] : =0=-= => -1307 agree
STR : [=0==2] : LETTERS : [(2 = = 0 =)] : =0==2 => -1308 agree
STR : [=0==1] : LETTERS : [(1 = = 0 =)] : =0==1 => -1309 agree
STR : [=0==0] : LETTERS : [(0 = = 0 =)] : =0==0 => -1310 agree
STR : [=0==-] : LETTERS : [(- = = 0 =)] : =0==- => -1311 agree
STR : [=0===] : LETTERS : [(= = = 0 =)] : =0=== => -1312 agree
STR : [=-222] : LETTERS : [(2 2 2 - =)] : =-222 => -1313 agree
STR : [=-221] : LETTERS : [(1 2 2 - =)] : =-221 => -1314 agree
STR : [=-220] : LETTERS : [(0 2 2 - =)] : =-220 => -1315 agree
STR : [=-22-] : LETTERS : [(- 2 2 - =)] : =-22- => -1316 agree
STR : [=-22=] : LETTERS : [(= 2 2 - =)] : =-22= => -1317 agree
STR : [=-212] : LETTERS : [(2 1 2 - =)] : =-212 => -1318 agree
STR : [=-211] : LETTERS : [(1 1 2 - =)] : =-211 => -1319 agree
STR : [=-210] : LETTERS : [(0 1 2 - =)] : =-210 => -1320 agree
STR : [=-21-] : LETTERS : [(- 1 2 - =)] : =-21- => -1321 agree
STR : [=-21=] : LETTERS : [(= 1 2 - =)] : =-21= => -1322 agree
STR : [=-202] : LETTERS : [(2 0 2 - =)] : =-202 => -1323 agree
STR : [=-201] : LETTERS : [(1 0 2 - =)] : =-201 => -1324 agree
STR : [=-200] : LETTERS : [(0 0 2 - =)] : =-200 => -1325 agree
STR : [=-20-] : LETTERS : [(- 0 2 - =)] : =-20- => -1326 agree
STR : [=-20=] : LETTERS : [(= 0 2 - =)] : =-20= => -1327 agree
STR : [=-2-2] : LETTERS : [(2 - 2 - =)] : =-2-2 => -1328 agree
STR : [=-2-1] : LETTERS : [(1 - 2 - =)] : =-2-1 => -1329 agree
STR : [=-2-0] : LETTERS : [(0 - 2 - =)] : =-2-0 => -1330 agree
STR : [=-2--] : LETTERS : [(- - 2 - =)] : =-2-- => -1331 agree
STR : [=-2-=] : LETTERS : [(= - 2 - =)] : =-2-= => -1332 agree
STR : [=-2=2] : LETTERS : [(2 = 2 - =)] : =-2=2 => -1333 agree
STR : [=-2=1] : LETTERS : [(1 = 2 - =)] : =-2=1 => -1334 agree
STR : [=-2=0] : LETTERS : [(0 = 2 - =)] : =-2=0 => -1335 agree
STR : [=-2=-] : LETTERS : [(- = 2 - =)] : =-2=- => -1336 agree
STR : [=-2==] : LETTERS : [(= = 2 - =)] : =-2== => -1337 agree
STR : [=-122] : LETTERS : [(2 2 1 - =)] : =-122 => -1338 agree
STR : [=-121] : LETTERS : [(1 2 1 - =)] : =-121 => -1339 agree
STR : [=-120] : LETTERS : [(0 2 1 - =)] : =-120 => -1340 agree
STR : [=-12-] : LETTERS : [(- 2 1 - =)] : =-12- => -1341 agree
STR : [=-12=] : LETTERS : [(= 2 1 - =)] : =-12= => -1342 agree
STR : [=-112] : LETTERS : [(2 1 1 - =)] : =-112 => -1343 agree
STR : [=-111] : LETTERS : [(1 1 1 - =)] : =-111 => -1344 agree
STR : [=-110] : LETTERS : [(0 1 1 - =)] : =-110 => -1345 agree
STR : [=-11-] : LETTERS : [(- 1 1 - =)] : =-11- => -1346 agree
STR : [=-11=] : LETTERS : [(= 1 1 - =)] : =-11= => -1347 agree
STR : [=-102] : LETTERS : [(2 0 1 - =)] : =-102 => -1348 agree
STR : [=-101] : LETTERS : [(1 0 1 - =)] : =-101 => -1349 agree
STR : [=-100] : LETTERS : [(0 0 1 - =)] : =-100 => -1350 agree
STR : [=-10-] : LETTERS : [(- 0 1 - =)] : =-10- => -1351 agree
STR : [=-10=] : LETTERS : [(= 0 1 - =)] : =-10= => -1352 agree
STR : [=-1-2] : LETTERS : [(2 - 1 - =)] : =-1-2 => -1353 agree
STR : [=-1-1] : LETTERS : [(1 - 1 - =)] : =-1-1 => -1354 agree
STR : [=-1-0] : LETTERS : [(0 - 1 - =)] : =-1-0 => -1355 agree
STR : [=-1--] : LETTERS : [(- - 1 - =)] : =-1-- => -1356 agree
STR : [=-1-=] : LETTERS : [(= - 1 - =)] : =-1-= => -1357 agree
STR : [=-1=2] : LETTERS : [(2 = 1 - =)] : =-1=2 => -1358 agree
STR : [=-1=1] : LETTERS : [(1 = 1 - =)] : =-1=1 => -1359 agree
STR : [=-1=0] : LETTERS : [(0 = 1 - =)] : =-1=0 => -1360 agree
STR : [=-1=-] : LETTERS : [(- = 1 - =)] : =-1=- => -1361 agree
STR : [=-1==] : LETTERS : [(= = 1 - =)] : =-1== => -1362 agree
STR : [=-022] : LETTERS : [(2 2 0 - =)] : =-022 => -1363 agree
STR : [=-021] : LETTERS : [(1 2 0 - =)] : =-021 => -1364 agree
STR : [=-020] : LETTERS : [(0 2 0 - =)] : =-020 => -1365 agree
STR : [=-02-] : LETTERS : [(- 2 0 - =)] : =-02- => -1366 agree
STR : [=-02=] : LETTERS : [(= 2 0 - =)] : =-02= => -1367 agree
STR : [=-012] : LETTERS : [(2 1 0 - =)] : =-012 => -1368 agree
STR : [=-011] : LETTERS : [(1 1 0 - =)] : =-011 => -1369 agree
STR : [=-010] : LETTERS : [(0 1 0 - =)] : =-010 => -1370 agree
STR : [=-01-] : LETTERS : [(- 1 0 - =)] : =-01- => -1371 agree
STR : [=-01=] : LETTERS : [(= 1 0 - =)] : =-01= => -1372 agree
STR : [=-002] : LETTERS : [(2 0 0 - =)] : =-002 => -1373 agree
STR : [=-001] : LETTERS : [(1 0 0 - =)] : =-001 => -1374 agree
STR : [=-000] : LETTERS : [(0 0 0 - =)] : =-000 => -1375 agree
STR : [=-00-] : LETTERS : [(- 0 0 - =)] : =-00- => -1376 agree
STR : [=-00=] : LETTERS : [(= 0 0 - =)] : =-00= => -1377 agree
STR : [=-0-2] : LETTERS : [(2 - 0 - =)] : =-0-2 => -1378 agree
STR : [=-0-1] : LETTERS : [(1 - 0 - =)] : =-0-1 => -1379 agree
STR : [=-0-0] : LETTERS : [(0 - 0 - =)] : =-0-0 => -1380 agree
STR : [=-0--] : LETTERS : [(- - 0 - =)] : =-0-- => -1381 agree
STR : [=-0-=] : LETTERS : [(= - 0 - =)] : =-0-= => -1382 agree
STR : [=-0=2] : LETTERS : [(2 = 0 - =)] : =-0=2 => -1383 agree
STR : [=-0=1] : LETTERS : [(1 = 0 - =)] : =-0=1 => -1384 agree
STR : [=-0=0] : LETTERS : [(0 = 0 - =)] : =-0=0 => -1385 agree
STR : [=-0=-] : LETTERS : [(- = 0 - =)] : =-0=- => -1386 agree
STR : [=-0==] : LETTERS : [(= = 0 - =)] : =-0== => -1387 agree
STR : [=--22] : LETTERS : [(2 2 - - =)] : =--22 => -1388 agree
STR : [=--21] : LETTERS : [(1 2 - - =)] : =--21 => -1389 agree
STR : [=--20] : LETTERS : [(0 2 - - =)] : =--20 => -1390 agree
STR : [=--2-] : LETTERS : [(- 2 - - =)] : =--2- => -1391 agree
STR : [=--2=] : LETTERS : [(= 2 - - =)] : =--2= => -1392 agree
STR : [=--12] : LETTERS : [(2 1 - - =)] : =--12 => -1393 agree
STR : [=--11] : LETTERS : [(1 1 - - =)] : =--11 => -1394 agree
STR : [=--10] : LETTERS : [(0 1 - - =)] : =--10 => -1395 agree
STR : [=--1-] : LETTERS : [(- 1 - - =)] : =--1- => -1396 agree
STR : [=--1=] : LETTERS : [(= 1 - - =)] : =--1= => -1397 agree
STR : [=--02] : LETTERS : [(2 0 - - =)] : =--02 => -1398 agree
STR : [=--01] : LETTERS : [(1 0 - - =)] : =--01 => -1399 agree
STR : [=--00] : LETTERS : [(0 0 - - =)] : =--00 => -1400 agree
STR : [=--0-] : LETTERS : [(- 0 - - =)] : =--0- => -1401 agree
STR : [=--0=] : LETTERS : [(= 0 - - =)] : =--0= => -1402 agree
STR : [=---2] : LETTERS : [(2 - - - =)] : =---2 => -1403 agree
STR : [=---1] : LETTERS : [(1 - - - =)] : =---1 => -1404 agree
STR : [=---0] : LETTERS : [(0 - - - =)] : =---0 => -1405 agree
STR : [=----] : LETTERS : [(- - - - =)] : =---- => -1406 agree
STR : [=---=] : LETTERS : [(= - - - =)] : =---= => -1407 agree
STR : [=--=2] : LETTERS : [(2 = - - =)] : =--=2 => -1408 agree
STR : [=--=1] : LETTERS : [(1 = - - =)] : =--=1 => -1409 agree
STR : [=--=0] : LETTERS : [(0 = - - =)] : =--=0 => -1410 agree
STR : [=--=-] : LETTERS : [(- = - - =)] : =--=- => -1411 agree
STR : [=--==] : LETTERS : [(= = - - =)] : =--== => -1412 agree
STR : [=-=22] : LETTERS : [(2 2 = - =)] : =-=22 => -1413 agree
STR : [=-=21] : LETTERS : [(1 2 = - =)] : =-=21 => -1414 agree
STR : [=-=20] : LETTERS : [(0 2 = - =)] : =-=20 => -1415 agree
STR : [=-=2-] : LETTERS : [(- 2 = - =)] : =-=2- => -1416 agree
STR : [=-=2=] : LETTERS : [(= 2 = - =)] : =-=2= => -1417 agree
STR : [=-=12] : LETTERS : [(2 1 = - =)] : =-=12 => -1418 agree
STR : [=-=11] : LETTERS : [(1 1 = - =)] : =-=11 => -1419 agree
STR : [=-=10] : LETTERS : [(0 1 = - =)] : =-=10 => -1420 agree
STR : [=-=1-] : LETTERS : [(- 1 = - =)] : =-=1- => -1421 agree
STR : [=-=1=] : LETTERS : [(= 1 = - =)] : =-=1= => -1422 agree
STR : [=-=02] : LETTERS : [(2 0 = - =)] : =-=02 => -1423 agree
STR : [=-=01] : LETTERS : [(1 0 = - =)] : =-=01 => -1424 agree
STR : [=-=00] : LETTERS : [(0 0 = - =)] : =-=00 => -1425 agree
STR : [=-=0-] : LETTERS : [(- 0 = - =)] : =-=0- => -1426 agree
STR : [=-=0=] : LETTERS : [(= 0 = - =)] : =-=0= => -1427 agree
STR : [=-=-2] : LETTERS : [(2 - = - =)] : =-=-2 => -1428 agree
STR : [=-=-1] : LETTERS : [(1 - = - =)] : =-=-1 => -1429 agree
STR : [=-=-0] : LETTERS : [(0 - = - =)] : =-=-0 => -1430 agree
STR : [=-=--] : LETTERS : [(- - = - =)] : =-=-- => -1431 agree
STR : [=-=-=] : LETTERS : [(= - = - =)] : =-=-= => -1432 agree
STR : [=-==2] : LETTERS : [(2 = = - =)] : =-==2 => -1433 agree
STR : [=-==1] : LETTERS : [(1 = = - =)] : =-==1 => -1434 agree
STR : [=-==0] : LETTERS : [(0 = = - =)] : =-==0 => -1435 agree
STR : [=-==-] : LETTERS : [(- = = - =)] : =-==- => -1436 agree
STR : [=-===] : LETTERS : [(= = = - =)] : =-=== => -1437 agree
STR : [==222] : LETTERS : [(2 2 2 = =)] : ==222 => -1438 agree
STR : [==221] : LETTERS : [(1 2 2 = =)] : ==221 => -1439 agree
STR : [==220] : LETTERS : [(0 2 2 = =)] : ==220 => -1440 agree
STR : [==22-] : LETTERS : [(- 2 2 = =)] : ==22- => -1441 agree
STR : [==22=] : LETTERS : [(= 2 2 = =)] : ==22= => -1442 agree
STR : [==212] : LETTERS : [(2 1 2 = =)] : ==212 => -1443 agree
STR : [==211] : LETTERS : [(1 1 2 = =)] : ==211 => -1444 agree
STR : [==210] : LETTERS : [(0 1 2 = =)] : ==210 => -1445 agree
STR : [==21-] : LETTERS : [(- 1 2 = =)] : ==21- => -1446 agree
STR : [==21=] : LETTERS : [(= 1 2 = =)] : ==21= => -1447 agree
STR : [==202] : LETTERS : [(2 0 2 = =)] : ==202 => -1448 agree
STR : [==201] : LETTERS : [(1 0 2 = =)] : ==201 => -1449 agree
STR : [==200] : LETTERS : [(0 0 2 = =)] : ==200 => -1450 agree
STR : [==20-] : LETTERS : [(- 0 2 = =)] : ==20- => -1451 agree
STR : [==20=] : LETTERS : [(= 0 2 = =)] : ==20= => -1452 agree
STR : [==2-2] : LETTERS : [(2 - 2 = =)] : ==2-2 => -1453 agree
STR : [==2-1] : LETTERS : [(1 - 2 = =)] : ==2-1 => -1454 agree
STR : [==2-0] : LETTERS : [(0 - 2 = =)] : ==2-0 => -1455 agree
STR : [==2--] : LETTERS : [(- - 2 = =)] : ==2-- => -1456 agree
STR : [==2-=] : LETTERS : [(= - 2 = =)] : ==2-= => -1457 agree
STR : [==2=2] : LETTERS : [(2 = 2 = =)] : ==2=2 => -1458 agree
STR : [==2=1] : LETTERS : [(1 = 2 = =)] : ==2=1 => -1459 agree
STR : [==2=0] : LETTERS : [(0 = 2 = =)] : ==2=0 => -1460 agree
STR : [==2=-] : LETTERS : [(- = 2 = =)] : ==2=- => -1461 agree
STR : [==2==] : LETTERS : [(= = 2 = =)] : ==2== => -1462 agree
STR : [==122] : LETTERS : [(2 2 1 = =)] : ==122 => -1463 agree
STR : [==121] : LETTERS : [(1 2 1 = =)] : ==121 => -1464 agree
STR : [==120] : LETTERS : [(0 2 1 = =)] : ==120 => -1465 agree
STR : [==12-] : LETTERS : [(- 2 1 = =)] : ==12- => -1466 agree
STR : [==12=] : LETTERS : [(= 2 1 = =)] : ==12= => -1467 agree
STR : [==112] : LETTERS : [(2 1 1 = =)] : ==112 => -1468 agree
STR : [==111] : LETTERS : [(1 1 1 = =)] : ==111 => -1469 agree
STR : [==110] : LETTERS : [(0 1 1 = =)] : ==110 => -1470 agree
STR : [==11-] : LETTERS : [(- 1 1 = =)] : ==11- => -1471 agree
STR : [==11=] : LETTERS : [(= 1 1 = =)] : ==11= => -1472 agree
STR : [==102] : LETTERS : [(2 0 1 = =)] : ==102 => -1473 agree
STR : [==101] : LETTERS : [(1 0 1 = =)] : ==101 => -1474 agree
STR : [==100] : LETTERS : [(0 0 1 = =)] : ==100 => -1475 agree
STR : [==10-] : LETTERS : [(- 0 1 = =)] : ==10- => -1476 agree
STR : [==10=] : LETTERS : [(= 0 1 = =)] : ==10= => -1477 agree
STR : [==1-2] : LETTERS : [(2 - 1 = =)] : ==1-2 => -1478 agree
STR : [==1-1] : LETTERS : [(1 - 1 = =)] : ==1-1 => -1479 agree
STR : [==1-0] : LETTERS : [(0 - 1 = =)] : ==1-0 => -1480 agree
STR : [==1--] : LETTERS : [(- - 1 = =)] : ==1-- => -1481 agree
STR : [==1-=] : LETTERS : [(= - 1 = =)] : ==1-= => -1482 agree
STR : [==1=2] : LETTERS : [(2 = 1 = =)] : ==1=2 => -1483 agree
STR : [==1=1] : LETTERS : [(1 = 1 = =)] : ==1=1 => -1484 agree
STR : [==1=0] : LETTERS : [(0 = 1 = =)] : ==1=0 => -1485 agree
STR : [==1=-] : LETTERS : [(- = 1 = =)] : ==1=- => -1486 agree
STR : [==1==] : LETTERS : [(= = 1 = =)] : ==1== => -1487 agree
STR : [==022] : LETTERS : [(2 2 0 = =)] : ==022 => -1488 agree
STR : [==021] : LETTERS : [(1 2 0 = =)] : ==021 => -1489 agree
STR : [==020] : LETTERS : [(0 2 0 = =)] : ==020 => -1490 agree
STR : [==02-] : LETTERS : [(- 2 0 = =)] : ==02- => -1491 agree
STR : [==02=] : LETTERS : [(= 2 0 = =)] : ==02= => -1492 agree
STR : [==012] : LETTERS : [(2 1 0 = =)] : ==012 => -1493 agree
STR : [==011] : LETTERS : [(1 1 0 = =)] : ==011 => -1494 agree
STR : [==010] : LETTERS : [(0 1 0 = =)] : ==010 => -1495 agree
STR : [==01-] : LETTERS : [(- 1 0 = =)] : ==01- => -1496 agree
STR : [==01=] : LETTERS : [(= 1 0 = =)] : ==01= => -1497 agree
STR : [==002] : LETTERS : [(2 0 0 = =)] : ==002 => -1498 agree
STR : [==001] : LETTERS : [(1 0 0 = =)] : ==001 => -1499 agree
STR : [==000] : LETTERS : [(0 0 0 = =)] : ==000 => -1500 agree
STR : [==00-] : LETTERS : [(- 0 0 = =)] : ==00- => -1501 agree
STR : [==00=] : LETTERS : [(= 0 0 = =)] : ==00= => -1502 agree
STR : [==0-2] : LETTERS : [(2 - 0 = =)] : ==0-2 => -1503 agree
STR : [==0-1] : LETTERS : [(1 - 0 = =)] : ==0-1 => -1504 agree
STR : [==0-0] : LETTERS : [(0 - 0 = =)] : ==0-0 => -1505 agree
STR : [==0--] : LETTERS : [(- - 0 = =)] : ==0-- => -1506 agree
STR : [==0-=] : LETTERS : [(= - 0 = =)] : ==0-= => -1507 agree
STR : [==0=2] : LETTERS : [(2 = 0 = =)] : ==0=2 => -1508 agree
STR : [==0=1] : LETTERS : [(1 = 0 = =)] : ==0=1 => -1509 agree
STR : [==0=0] : LETTERS : [(0 = 0 = =)] : ==0=0 => -1510 agree
STR : [==0=-] : LETTERS : [(- = 0 = =)] : ==0=- => -1511 agree
STR : [==0==] : LETTERS : [(= = 0 = =)] : ==0== => -1512 agree
STR : [==-22] : LETTERS : [(2 2 - = =)] : ==-22 => -1513 agree
STR : [==-21] : LETTERS : [(1 2 - = =)] : ==-21 => -1514 agree
STR : [==-20] : LETTERS : [(0 2 - = =)] : ==-20 => -1515 agree
STR : [==-2-] : LETTERS : [(- 2 - = =)] : ==-2- => -1516 agree
STR : [==-2=] : LETTERS : [(= 2 - = =)] : ==-2= => -1517 agree
STR : [==-12] : LETTERS : [(2 1 - = =)] : ==-12 => -1518 agree
STR : [==-11] : LETTERS : [(1 1 - = =)] : ==-11 => -1519 agree
STR : [==-10] : LETTERS : [(0 1 - = =)] : ==-10 => -1520 agree
STR : [==-1-] : LETTERS : [(- 1 - = =)] : ==-1- => -1521 agree
STR : [==-1=] : LETTERS : [(= 1 - = =)] : ==-1= => -1522 agree
STR : [==-02] : LETTERS : [(2 0 - = =)] : ==-02 => -1523 agree
STR : [==-01] : LETTERS : [(1 0 - = =)] : ==-01 => -1524 agree
STR : [==-00] : LETTERS : [(0 0 - = =)] : ==-00 => -1525 agree
STR : [==-0-] : LETTERS : [(- 0 - = =)] : ==-0- => -1526 agree
STR : [==-0=] : LETTERS : [(= 0 - = =)] : ==-0= => -1527 agree
STR : [==--2] : LETTERS : [(2 - - = =)] : ==--2 => -1528 agree
STR : [==--1] : LETTERS : [(1 - - = =)] : ==--1 => -1529 agree
STR : [==--0] : LETTERS : [(0 - - = =)] : ==--0 => -1530 agree
STR : [==---] : LETTERS : [(- - - = =)] : ==--- => -1531 agree
STR : [==--=] : LETTERS : [(= - - = =)] : ==--= => -1532 agree
STR : [==-=2] : LETTERS : [(2 = - = =)] : ==-=2 => -1533 agree
STR : [==-=1] : LETTERS : [(1 = - = =)] : ==-=1 => -1534 agree
STR : [==-=0] : LETTERS : [(0 = - = =)] : ==-=0 => -1535 agree
STR : [==-=-] : LETTERS : [(- = - = =)] : ==-=- => -1536 agree
STR : [==-==] : LETTERS : [(= = - = =)] : ==-== => -1537 agree
STR : [===22] : LETTERS : [(2 2 = = =)] : ===22 => -1538 agree
STR : [===21] : LETTERS : [(1 2 = = =)] : ===21 => -1539 agree
STR : [===20] : LETTERS : [(0 2 = = =)] : ===20 => -1540 agree
STR : [===2-] : LETTERS : [(- 2 = = =)] : ===2- => -1541 agree
STR : [===2=] : LETTERS : [(= 2 = = =)] : ===2= => -1542 agree
STR : [===12] : LETTERS : [(2 1 = = =)] : ===12 => -1543 agree
STR : [===11] : LETTERS : [(1 1 = = =)] : ===11 => -1544 agree
STR : [===10] : LETTERS : [(0 1 = = =)] : ===10 => -1545 agree
STR : [===1-] : LETTERS : [(- 1 = = =)] : ===1- => -1546 agree
STR : [===1=] : LETTERS : [(= 1 = = =)] : ===1= => -1547 agree
STR : [===02] : LETTERS : [(2 0 = = =)] : ===02 => -1548 agree
STR : [===01] : LETTERS : [(1 0 = = =)] : ===01 => -1549 agree
STR : [===00] : LETTERS : [(0 0 = = =)] : ===00 => -1550 agree
STR : [===0-] : LETTERS : [(- 0 = = =)] : ===0- => -1551 agree
STR : [===0=] : LETTERS : [(= 0 = = =)] : ===0= => -1552 agree
STR : [===-2] : LETTERS : [(2 - = = =)] : ===-2 => -1553 agree
STR : [===-1] : LETTERS : [(1 - = = =)] : ===-1 => -1554 agree
STR : [===-0] : LETTERS : [(0 - = = =)] : ===-0 => -1555 agree
STR : [===--] : LETTERS : [(- - = = =)] : ===-- => -1556 agree
STR : [===-=] : LETTERS : [(= - = = =)] : ===-= => -1557 agree
STR : [====2] : LETTERS : [(2 = = = =)] : ====2 => -1558 agree
STR : [====1] : LETTERS : [(1 = = = =)] : ====1 => -1559 agree
STR : [====0] : LETTERS : [(0 = = = =)] : ====0 => -1560 agree
STR : [====-] : LETTERS : [(- = = = =)] : ====- => -1561 agree
STR : [=====] : LETTERS : [(= = = = =)] : ===== => -1562 agree


#;3469> (g "2-=102--02--=1-12211")
2-=102--02--=1-12211 94 : same string !!
#;3480> (g best-str)
2-=102--02--=1-12211 94 : same string !!
#;3482> best-str
"2-=102--02--=1-12211"
#;3484> (g best-str)
2-=102--02--=1-12211 94 : same string !!
#;3485> (g "2-=102--02--=1-12221")
2-=102--02--=1-12221 99 : worse !! : 2-=102--02--=1-12211 94 : #f
#;3493> (g "2-=102--02--=1-12201")
2-=102--02--=1-12201 89 : better !!
#;3497> (g "2-=102--02--=1-10201")
2-=102--02--=1-10201 161 : worse !! : 2-=102--02--=1-12201 89 : #t
#;3502> (g "2-=102--02--=1-12201")
2-=102--02--=1-12201 89 : same string !!
#;3505> (g "2-=102--02--=1-12001")
2-=102--02--=1-12001 39 : better !!
#;3509> (g "2-=102--02--=1-12000")
2-=102--02--=1-12000 38 : better !!
#;3512> (g "2-=102--02--=1-11000")
2-=102--02--=1-11000 87 : worse !! : 2-=102--02--=1-12000 38 : #t
#;3516> (g "2-=102--02--=1-11020")
2-=102--02--=1-11020 77 : worse !! : 2-=102--02--=1-12000 38 : #t
#;3518> (g "2-=102--02--=1-11022")
2-=102--02--=1-11022 75 : worse !! : 2-=102--02--=1-12000 38 : #t
#;3519> (g "2-=102--02--=1-11222")
2-=102--02--=1-11222 25 : better !!
#;3523> (g "2-=102--02--=1-11222")
2-=102--02--=1-11222 25 : same string !!
#;3538> (g "2-=102--02--=1-11221")
2-=102--02--=1-11221 26 : worse !! : 2-=102--02--=1-11222 25 : #t
#;3542> (g "2-=102--02--=1-22222")
2-=102--02--=1-22222 725 : worse !! : 2-=102--02--=1-11222 25 : #f
#;3549> (g "2-=102--02--=1-12222")
2-=102--02--=1-12222 100 : worse !! : 2-=102--02--=1-11222 25 : #f
#;3552> (g "2-=102--02--=1-11222")
2-=102--02--=1-11222 25 : same string !!
#;3554> (g "2-=102--02--=1-11122")
2-=102--02--=1-11122 50 : worse !! : 2-=102--02--=1-11222 25 : #t
#;3558> (g "2-=102--02--=1-11022")
2-=102--02--=1-11022 75 : worse !! : 2-=102--02--=1-11222 25 : #t
#;3562> (g "2-=102--02--=1-11222")
2-=102--02--=1-11222 25 : same string !!
#;3566> (g "2-=102--02--=1-11212")
2-=102--02--=1-11212 30 : worse !! : 2-=102--02--=1-11222 25 : #t
#;3568> (g "2-=102--02--=1-11222")
2-=102--02--=1-11222 25 : same string !!
#;3571> (g "2-=102--02--=1-12000")
2-=102--02--=1-12000 38 : worse !! : 2-=102--02--=1-11222 25 : #f
#;3576> (g "2-=102--02--=1-12=00")
2-=102--02--=1-12=00 12 : better !!
#;3580> (g "2-=102--02--=1-12=-0")
2-=102--02--=1-12=-0 17 : worse !! : 2-=102--02--=1-12=00 12 : #t
#;3583> (g "2-=102--02--=1-12=20")
2-=102--02--=1-12=20 2 : better !!
#;3587> (g "2-=102--02--=1-12=21")
2-=102--02--=1-12=21 1 : better !!
#;3588> (g "2-=102--02--=1-12=22")
2-=102--02--=1-12=22 0 : better !!
#;3590> (g "2-=102--02--=1-12=22")
2-=102--02--=1-12=22 0 : same string !!
#;3591> (g "2-=102--02--=1-12=22")
2-=102--02--=1-12=22 0 : same string !!
#;3592> (g "2-=102--02--=1-12=22")
2-=102--02--=1-12=22 0 : same string !!
#;3604> (snafu "2-=102--02--=1-12=22")
32969743607087
#;3608> guess
32969743607087
#;3609>


ACCEPTED answer was  "2-=102--02--=1-12=22" 
answer inside quotes " "  excluding quotes

we can count in snarfu using = - 0 1 2 then overflow

counting up we do ...

====
===-
===0
===1
===2
==-=
==--
==-1
==-2
==0=
==0-
==01
==02
==1=
etc ....

almost impossible to code an algorithm to do a good search ie solve the
problem if do not know how to do the search

blind coding in vain hope somehow find a "shangri la" fictional
romantic paradise


|#






	    
	    


