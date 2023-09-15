

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

;; apply the encryption key for part I I
(let ((magic-key 811589153))
  (set! input (map (lambda (x) (* x magic-key)) input)))

;; idea here is to find better algorithm than scooting
;; round and round one at a time



(define (ident xs)
  (let ((n 0))
    (letrec ((foo (lambda (ys rs)
		    (cond
		     ((null? ys) rs)
		     (#t
		      (set! n (+ n 1))
		      (foo (cdr ys) (cons (list 'id n 'val (car ys)) rs)))))))
      (reverse (foo xs '())))))


(define arr (make-array 0 (length input)))


(let ((i 0)
      (xs (ident input)))
  (while (not (null? xs))
    (array-set! arr (car xs) i)
    (set! i (1+ i))
    (set! xs (cdr xs))))

arr

(define arr-len (array-length arr))
arr-len

(define (find x)
  (call/cc (lambda (exit)
	   (let ((i 0))
	     (while (< i arr-len)
	       (when (equal? x (array-ref arr i))
		 (exit i))
	       (set! i (+ i 1)))))))

;; final index if start at i go forward k 
(define (gof i k)
  (letrec ((lup (lambda (a b)
		  (cond
		   ((= b 0) a)
		   ((>= (+ a 1) arr-len)
		    (lup 1 (- b 1)))
		   (#t (lup (+ a 1) (- b 1)))))))
    (lup i k)))

  
;; final index if start at i go backward k
;; make k positive to be sure
(define (gob i k)
  (letrec ((lup (lambda (a b)
		  (cond
		   ((= b 0) a)
		   ((< (- a 1) 0)
		    (lup (- arr-len 2) (- b 1)))
		   (#t (lup (- a 1) (- b 1)))))))
    (lup i (abs k))))


#|

forward 129349243 places and at index 3 and array is size 7 what index end up at ?

pick i and place at k where is empty slot ?
i = k nothing to do shift to itself no empty gap created

case i > k
       k >>>>>> i-1
 ....[ k ] ....[ i ]....
move all those from i-1 down to k , up by 1
i , k array indexs 0 to (1- arr-len)

case i < k
      < i+1 <<< k    
....[ i ]..... [ k ]....
|#
(define (go-shift i k)
  (cond
   ((= i k) #t)
   ((> i k) ;; move items up one into gap
    (let ((j (- i 1))
	  (pick (array-ref arr i)))
      (while (>= j k)
	(array-set! arr (array-ref arr j) (+ j 1))
	(set! j (- j 1)))
      (array-set! arr pick k)
      #t))
   (#t ;(< i k ) ;; move items all down one into gap
    (let ((j (+ i 1))
	  (pick (array-ref arr i)))
      (while (<= j k)
	(array-set! arr (array-ref arr j) (- j 1))
	(set! j (+ j 1)))
      (array-set! arr pick k)
      #t))
   ))



(define (show)
  (let ((i 0 ))
    (format #t "[")
    (while (< i arr-len)
      (format #t "~a " (fourth (array-ref arr i)))
      (set! i (+ i 1)))
    (format #t "]~%")))


(define (find-zero)
  (call/cc (lambda (exit)
	     (let ((i 0 ))
	       (while (< i arr-len)
		 (when (zero? (fourth (array-ref arr i)))
		   (exit i))
		 (set! i (+ i 1)))
	       (error "zero not found !")))))




;; item moves and all other items shift either up
;; or down to take space empty slot
(define (fuz)
  (let ((i 0)
	(xs (ident input)))
    (while (not (null? xs))
      ;;(format #t "~a : " i)
      ;;(pretty-print arr)
      ;;(show)
      ;;(newline)
      (let* ((a (car xs))
	     (id (second a))
	     (val (fourth a)))
	;; find id in arr
	(let ((index (find a)))
	  (when (not (integer? index))
	    (error "index not found !"))	
	  ;;(format #t "processing ~a : id ~a : val ~a : @index ~a  ~%" a id val index)
	  (cond
	   ((positive? val)
	    (let ((index2 (gof index val)))
	      (go-shift index index2)))
	   ((negative? val)
	    (let ((index2 (gob index val)))
	      (go-shift index index2)))
	   (#t #t)))
	(set! xs (cdr xs))
	(set! i (+ i 1))))))




(define (fwd i n)
  (cond
   ((= n 0) i)
   (#t (let ((i2 (+ i 1)))
	 (cond
	  ((>= (+ i 1) arr-len)
	   (fwd 0 (- n 1)))
	  (#t (fwd (+ i 1) (- n 1))))))))


(define (fiz)
  (fuz)
  (show)
  (let* ((zi (find-zero))
	 (za (fwd zi 1000))
	 (zb (fwd za 1000))
	 (zc (fwd zb 1000)))
    (let ((n1 (fourth (array-ref arr za)))
	  (n2 (fourth (array-ref arr zb)))
	  (n3 (fourth (array-ref arr zc))))
      (format #t "numbers 1000th = ~a ~%" n1)
      (format #t "numbers 2000th = ~a ~%" n2)
      (format #t "numbers 3000th = ~a ~%" n3)
      (format #t "sum = ~a ~%" (+ n1 n2 n3)))))

#|

aoc 2022 day 20 

numbers 1000th = 9573 
numbers 2000th = 1824 
numbers 3000th = 7673 
sum = 19070 
$10 = #t

part two multiply each number by encrpytion key

811589153

|#

