
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
      (let loop ((i 0))
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
	
	(when (< i (- len 1))
	  (loop (+ i 1)))))))

  (helper)
  (list best-str best-diff))



(define (test)
  (optimize "2-=102--02-2111=0=02"))

(define (sanity)
  (optimize  "22222222222222222222"))

;; "2-=102--02-2111=0=02"

(define (sanity2)
  (optimize  "2222222222222222222"))
      





	    
	    


