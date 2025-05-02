#|

read in characters from a file and make a "grid" in a hash map so
given a coordinate - say what should be the square above or below , left or right...

|#

(import (chicken format))
(import (chicken pretty-print))
(import srfi-69)
(import srfi-1)

(import (chicken process-context))
;; (current-directory)
;; (change-directory "day22/chicken")
(import (chicken io))

(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read-line p) (cons line result)))))))


(define *input* (readlines "input.txt"))

(define *max-x* 0)
(define *max-y* 0)
(define *min-x* 1)
(define *min-y* 1)

(define *map* (make-hash-table #:test equal?))

(define *left-map* (make-hash-table #:test equal?))
(define *right-map* (make-hash-table #:test equal?))

(define *up-map* (make-hash-table #:test equal?))
(define *down-map* (make-hash-table #:test equal?))


;; lines without last 2 lines (empty line + code line)
(define (process in)  
  (let ((lines (reverse (cdr (cdr (reverse in))))))
    (process2 lines 0 (length lines) '())))

(define (process2 in i nlines acc)
  (cond
   ((>= i (- nlines 1)) acc)
   (#t (let ((str (list-ref in i)))
	 (let ((strlen (string-length str)))
	   (let loop ((j 0))
	     (cond
	      ((< j strlen)	       
	       (let ((ch (string-ref str j)))
		 (format #t "(i ~a, j ~a) =~a ~%" (+ i 1) (+ j 1) ch)
		 (when (or (char>? ch #\space))
		   (hash-table-set! *map* (list (+ j 1) (+ i 1) ) ch)

		   ;; record max x y
		   (when (> (+ i 1) *max-y*)
		     (set! *max-y* (+ i 1)))
		   
		   (when (> (+ j 1) *max-x*)
		     (set! *max-x* (+ j 1)))
		   
		   ;;(set! acc (cons (list (+ i 1) (+ j 1) ch) acc))
		   )		   
	       (loop (+ j 1))))
	      (#t
	       ;; move onto next line
	       (process2 in (+ i 1) nlines acc)
	       ))))))))



(set! *map* (make-hash-table))
(process *input*)

(define (lookup x y)
  (hash-table-ref/default *map* (list x y) #f))



;; scope of puzzle
;; *max-x* *max-y* *min-x* *min-y*
;; 199     150        1       1

;; x-1 y is in the hash then job done else keep going right until cannot go right no more
;; unless x y is not a valid square 


;; find left either directly left or keep going right until nothing then backup one
(define (find-left x y)
  (let ((out (hash-table-ref/default *map* (list x y) #f)))
    (when out
      (let ((tmp (hash-table-ref/default *map* (list (- x 1) y) #f)))
	(cond
	 (tmp (hash-table-set! *left-map* (list x y) (list (- x 1) y))
	      (list (- x 1) y))
	 (#t (let loop ((i x))
	       (let ((out2 (hash-table-ref/default *map* (list i y) #f)))
		 (cond
		  (out2 (loop (+ i 1)))
		  (#t 
		   (hash-table-set! *left-map* (list x y) (list (- i 1) y))
		   (list (- i 1) y)))))))))))

(define (find-right x y)
  (let ((out (hash-table-ref/default *map* (list x y) #f)))
    (when out
      (let ((tmp (hash-table-ref/default *map* (list (+ x 1) y) #f)))
	(cond
	 (tmp (hash-table-set! *right-map* (list x y) (list (+ x 1) y))
	      (list (+ x 1) y))
	 (#t (let loop ((i x))
	       (let ((out2 (hash-table-ref/default *map* (list i y) #f)))
		 (cond
		  (out2 (loop (- i 1)))
		  (#t
		   ;; backup one square
		   (hash-table-set! *right-map* (list x y) (list (+ i 1) y))
		   (list (+ i 1) y)))))))))))

;; forms a self check LEFT - RIGHT 
;; find-lefts 150 1 -> 149 1
;; find-rights 149 1 -> 150 1

;; (x,y) position has to be a valid position before find - down makes sense
(define (find-down x y) 
  (let ((out (hash-table-ref/default *map* (list x y) #f)))
    (when out
      (let ((tmp (hash-table-ref/default *map* (list x (+ y 1)) #f)))
	(cond
	 (tmp (hash-table-set! *down-map* (list x y) (list x (+ y 1)))
	      (list x (+ y 1)))
	 (#t (let loop ((i y))
	       ;; (format #t "find-down x = ~a : y = ~a ~%" x y)
	       (let ((out2 (hash-table-ref/default *map* (list x i) #f)))
		 (cond
		  (out2 (loop (- i 1)))
		  (#t 
		   (hash-table-set! *down-map* (list x y) (list x (+ i 1)))
		   (list x (+ i 1))))))))))))


(define (find-up x y)
  (let ((out (hash-table-ref/default *map* (list x y) #f)))
    (when out
      (let ((tmp (hash-table-ref/default *map* (list x (- y 1)) #f)))
	(cond
	 (tmp (hash-table-set! *up-map* (list x y) (list x (- y 1)))
	      (list x (- y 1)))
	 (#t (let loop ((i y))
	       (let ((out2 (hash-table-ref/default *map* (list x i) #f)))
		 (cond
		  (out2 (loop (+ i 1)))
		  (#t 
		   (hash-table-set! *up-map* (list x y) (list x (- i 1)))
		   (list x (- i 1))))))))))))



(define (up x y) (hash-table-ref/default *up-map* (list x y) #f))
(define (left x y) (hash-table-ref/default *left-map* (list x y) #f))
(define (right x y) (hash-table-ref/default *right-map* (list x y) #f))
(define (down x y) (hash-table-ref/default *down-map* (list x y) #f))

(define (all-directions)
  (let loop ((x 1)(y 1))
    (cond
     ((> x *max-x*) (loop 1 (+ y 1)))
     ((> y *max-y*) #f)
     (#t (find-left x y)
	 (find-right x y)
	 (find-up x y)
	 (find-down x y)
	 (loop (+ x 1) y)))))

(all-directions)


(define *codes*
  '(5 L 37 R 32 R 38 R 28 L 18 R 11 R 37 R 41 R 42 L 8 L 3 R 14 L 22
      L 28 R 4 L 32 R 44 R 3 R 45 L 44 L 39 L 27 L 25 L 7 R 32 L 36 R
      21 L 29 R 33 R 37 L 37 R 19 L 44 R 36 L 14 R 45 R 13 R 32 L 9 R
      16 L 23 L 19 R 1 L 18 R 9 R 48 L 38 L 49 L 9 L 43 R 30 L 13 L 20
      R 46 L 46 R 38 R 34 R 9 L 16 R 1 R 8 L 36 L 14 R 9 L 17 R 18 L 19
      L 23 R 42 R 43 L 3 R 27 R 9 R 25 R 39 L 40 R 47 L 1 R 17 L 34 R
      50 R 43 L 16 L 10 R 28 R 34 R 47 R 33 R 18 R 41 R 44 L 26 L 3 L
      40 L 2 L 50 L 7 L 28 R 8 L 21 R 36 R 45 R 16 R 41 L 33 R 34 L 37
      R 31 L 40 R 36 R 4 R 5 R 47 R 11 R 6 L 25 L 6 L 9 R 22 R 42 L 4 L
      41 R 39 R 8 R 50 R 10 L 38 R 45 R 7 R 36 L 43 R 20 L 18 R 45 L 40
      R 30 L 10 L 15 L 44 R 24 R 16 R 36 L 24 L 32 R 47 R 43 R 12 R 31
      R 26 R 4 L 38 R 37 L 16 L 7 L 42 L 28 L 42 R 19 R 29 R 4 L 11 L
      40 L 8 R 21 L 31 R 5 R 26 R 33 L 44 R 34 R 3 L 6 R 9 R 43 L 36 R
      29 L 30 R 32 L 29 R 35 R 28 L 27 R 10 L 28 R 6 R 23 L 29 L 26 R
      32 R 28 R 31 L 46 R 30 R 36 L 29 L 7 R 39 R 5 L 11 L 29 R 3 L 3 R
      29 R 9 R 11 L 50 R 15 R 6 R 35 R 50 L 18 L 49 R 26 R 24 L 33 L 48
      L 30 L 10 R 44 R 1 R 31 L 35 L 35 L 2 L 27 L 6 R 24 R 10 L 34 R
      14 R 9 R 26 R 3 L 46 L 7 R 11 R 37 R 1 R 43 R 35 L 48 R 36 L 1 R
      13 L 21 L 8 L 31 R 43 R 40 L 1 R 49 L 12 R 33 R 32 R 19 R 38 L 12
      R 12 L 47 L 5 L 9 L 29 L 18 R 7 R 10 L 24 L 42 R 47 R 44 R 3 L 22
      L 23 R 5 L 2 L 17 R 44 R 30 L 7 L 17 R 26 L 41 L 17 L 37 R 1 L 46
      R 26 R 9 R 40 L 50 L 6 R 3 R 48 L 40 L 20 L 34 R 47 R 46 R 19 R
      32 L 35 R 22 L 7 R 34 R 8 L 4 R 49 R 47 L 32 R 26 R 12 R 11 R 47
      R 4 L 14 R 1 L 24 L 30 L 21 L 5 R 10 L 28 R 5 L 31 L 46 R 33 R 42
      R 27 L 31 L 14 L 35 R 41 R 18 L 43 R 45 R 31 R 8 R 32 R 39 L 45 L
      14 L 38 L 49 L 34 R 26 L 49 R 37 L 27 L 47 R 7 L 14 L 22 R 22 R
      42 L 20 R 36 R 21 R 35 R 10 L 37 L 45 R 35 L 14 R 48 R 10 R 21 L
      30 R 27 L 34 R 1 L 46 L 34 R 47 R 22 L 7 L 46 R 41 R 33 L 41 L 39
      R 50 L 29 L 50 R 33 L 46 L 36 R 7 R 48 L 1 L 3 R 35 R 24 L 25 R
      43 L 7 R 50 L 13 L 41 R 22 R 49 L 23 L 6 R 49 L 12 R 2 R 34 L 29
      L 11 L 36 R 8 R 21 L 10 R 50 L 10 L 49 L 42 R 31 L 18 R 25 L 30 R
      13 R 40 L 20 L 35 L 17 R 47 L 13 L 27 L 42 L 42 R 5 L 16 L 22 R
      30 L 4 L 28 L 18 R 23 L 19 L 16 L 40 L 35 L 6 R 37 L 30 R 14 L 31
      L 50 R 1 R 19 L 9 L 10 L 50 R 40 R 23 R 34 R 50 L 8 L 30 L 28 R
      10 R 19 L 26 L 13 L 44 R 28 L 19 R 11 L 19 L 30 L 7 R 10 R 46 L
      17 R 15 R 27 R 36 R 19 L 19 R 49 R 11 L 37 R 1 R 16 L 36 L 48 L
      31 R 41 R 30 R 16 L 18 R 7 R 35 L 34 L 43 L 38 L 34 L 11 L 48 R
      17 L 40 R 8 L 31 L 47 R 4 L 36 L 28 R 48 R 34 R 29 L 38 R 21 L 22
      L 23 R 3 R 34 R 17 L 32 L 42 L 34 R 29 L 46 L 19 R 32 R 48 L 41 R
      46 R 9 L 40 L 49 R 41 R 36 R 23 L 49 L 30 R 1 L 28 L 10 L 14 R 35
      R 27 R 7 R 45 L 49 L 35 R 42 R 31 R 17 L 5 L 15 R 21 L 22 R 5 L
      16 R 2 L 3 L 19 R 24 L 26 R 31 R 10 L 32 R 10 R 17 R 17 L 20 L 42
      R 31 R 11 R 11 R 25 R 22 L 39 R 19 L 4 L 46 L 34 R 44 R 38 L 13 R
      40 R 23 L 2 R 16 L 21 R 8 L 7 R 30 L 2 R 45 R 21 L 32 L 42 L 2 L
      10 L 45 L 46 R 21 R 31 L 22 L 10 R 42 L 7 R 20 L 12 L 39 R 37 R
      37 L 43 L 45 L 1 L 8 L 14 R 44 R 19 R 23 L 37 L 6 L 12 R 27 R 26
      R 8 R 12 L 3 R 47 R 35 L 25 R 41 L 39 R 46 R 10 L 32 L 35 L 31 R
      12 L 20 R 8 L 19 L 26 L 18 L 5 L 22 L 15 R 24 L 2 R 1 R 38 R 50 L
      4 L 45 L 4 L 16 R 16 R 31 R 41 R 5 L 29 R 49 R 25 L 21 R 20 L 15
      R 33 R 47 L 7 R 32 L 35 R 5 L 31 L 18 R 42 L 12 R 9 L 21 L 4 L 36
      L 14 R 33 L 7 R 21 L 27 R 18 L 20 L 47 L 14 L 37 R 35 L 42 L 22 L
      11 R 33 R 13 L 11 R 11 L 7 R 7 R 23 L 47 R 4 R 24 L 10 R 45 L 16
      R 45 L 35 R 27 R 43 L 47 R 3 L 42 R 14 R 2 L 44 R 1 L 45 L 25 R
      23 R 1 L 47 R 38 L 46 L 18 R 46 R 42 L 1 L 34 R 48 R 42 L 42 L 50
      R 39 R 25 R 43 R 19 L 48 R 38 L 20 L 45 L 40 R 28 L 39 R 4 R 16 L
      46 R 5 L 41 L 25 L 4 R 50 L 11 R 6 R 34 L 19 L 33 L 33 L 27 L 24
      L 16 L 23 L 15 R 22 R 32 R 19 R 7 R 4 R 30 L 1 R 5 R 23 L 46 R 38
      L 23 L 42 R 40 L 21 R 45 L 27 L 1 L 41 R 44 R 26 L 47 R 34 R 44 L
      37 L 38 L 37 L 38 L 1 R 50 R 30 R 38 R 28 R 5 R 13 L 31 L 11 L 30
      R 34 L 16 R 8 R 27 L 19 L 10 R 33 R 49 L 36 R 43 L 5 L 19 L 1 R
      24 L 29 R 13 R 19 R 24 R 10 L 48 R 36 R 18 L 40 L 28 R 13 L 36 L
      1 R 18 R 27 L 42 R 11 R 16 L 26 L 20 L 46 R 27 R 29 R 26 R 40 R 5
      L 31 L 22 L 33 R 37 R 33 R 28 R 22 L 42 R 5 R 4 L 19 R 34 R 26 L
      13 R 8 L 46 L 25 R 40 R 39 R 36 L 6 R 9 R 22 R 42 L 25 R 38 L 18
      L 10 L 11 R 15 R 25 L 17 L 9 R 36 R 47 L 31 L 5 L 33 R 6 R 2 R 45
      R 33 R 26 L 16 R 9 L 21 L 9 L 18 L 4 R 30 L 36 L 35 R 6 R 19 L 42
      L 38 L 5 R 9 L 39 L 13 R 3 L 20 R 16 L 48 L 47 R 16 L 15 L 13 R
      43 L 17 R 30 R 27 R 16 R 32 R 7 L 11 L 50 R 50 R 20 L 3 L 24 L 44
      R 42 L 50 L 22 L 42 R 16 R 37 L 37 R 8 L 41 R 50 R 47 R 5 L 7 L 2
      L 38 R 39 R 9 R 36 L 24 L 23 R 21 L 49 L 42 R 18 R 37 L 49 L 37 L
      38 L 2 R 48 R 41 L 4 R 28 R 1 R 36 L 6 R 23 R 22 R 22 L 13 L 18 L
      35 L 11 R 27 L 16 L 19 R 7 L 5 R 22 L 26 L 23 L 43 R 17 L 1 R 36
      L 34 R 45 R 47 R 12 R 34 R 14 L 39 L 16 L 47 L 19 L 12 R 21 L 28
      L 14 R 27 R 36 L 3 L 38 R 11 L 23 R 29 R 10 R 34 L 32 L 26 R 7 L
      10 L 27 L 10 R 23 R 27 R 49 R 42 R 27 L 49 L 14 R 49 R 38 R 16 R
      41 R 25 L 35 L 12 L 9 L 18 R 25 L 13 R 35 L 1 R 8 L 49 L 19 R 39
      R 38 R 49 R 32 R 34 L 28 R 15 L 10 R 1 R 23 R 9 L 39 L 29 R 26 L
      28 L 27 L 10 L 27 L 1 L 36 R 14 R 48 R 33 R 24 R 37 L 42 R 42 R
      32 R 40 R 16 R 2 R 1 L 37 L 32 R 17 L 13 L 41 L 19 R 33 R 18 L 7
      L 2 R 3 R 43 L 30 L 21 L 48 R 3 R 31 R 5 R 38 L 47 L 36 L 47 L 30
      R 24 R 26 R 37 L 24 R 24 L 39 L 19 L 27 R 9 R 33 R 11 R 37 L 31 L
      25 R 8 R 41 L 24 R 10 L 41 L 35 R 16 L 7 L 26 R 42 R 30 L 49 R 34
      L 49 R 19 R 43 L 24 L 9 R 11 R 34 L 10 L 21 R 39 L 37 R 6 L 16 R
      8 R 1 R 11 L 17 R 14 L 14 R 10 R 35 R 37 R 29 L 10 R 21 L 37 R 41
      L 11 L 48 R 33 R 16 L 43 R 43 L 24 L 17 L 12 L 20 R 43 R 25 R 45
      R 19 R 45 R 49 L 21 L 42 L 31 L 36 R 16 R 41 R 50 R 43 L 15 L 30
      L 26 L 25 R 40 R 34 R 18 R 46 R 24 R 14 L 20 R 12 L 19 R 49 L 1 R
      27 L 42 R 11 L 24 R 34 R 35 R 2 L 20 R 26 R 41 R 18 L 42 R 18 R 7
      L 4 R 33 L 21 R 13 L 24 R 10 R 15 L 41 L 47 R 34 R 28 L 35 L 8 L
      3 R 10 R 14 R 6 R 15 R 2 R 12 R 44 R 41 L 45 L 7 R 3 R 32 R 29 L
      26 R 13 R 4 L 38 R 20 L 12 L 26 R 8 L 6 L 15 L 10 R 6 L 45 R 28 L
      10 L 26 L 33 L 47 L 19 R 14 R 7 L 13 R 21 R 18 L 17 L 24 L 21 L
      24 R 22 R 7 R 40 L 22 R 27 R 37 L 17 R 35 L 22 R 37 R 33 R 6 L 4
      L 38 L 4 L 36 L 6 L 5 L 27 L 1 R 49 R 33 R 31 L 3 L 35 L 45 L 43
      R 41 R 42 R 44 L 4 R 18 L 29 L 15 L 46 R 14 R 7 R 37 L 16 L 5 R
      45 L 41 R 36 L 11 R 18 L 17 L 12 R 29 L 12 L 40 R 28 R 44 L 26 L
      9 L 19 R 39 L 36 L 33 L 20 R 42 L 17 L 36 L 21 R 19 R 38 R 33 L
      19 R 33 L 25 R 2 L 24 R 6 R 33 R 8 R 43 L 35 L 26 L 23 L 25 L 12
      L 10 L 17 R 2 R 45 L 10 R 17 L 7 R 38 L 1 R 2 L 9 R 14 R 35 L 38
      L 39 L 38 R 46 R 10 R 28 R 40 L 31 L 34 L 30 R 45 L 2 L 42 L 49 R
      28 R 46 R 44 R 11 R 48 R 1 L 38 L 20 L 25 R 8 R 26 L 34 R 3 L 39
      L 25 L 48 L 12 R 34 L 25 R 14 R 36 R 39 L 45 R 34 R 25 L 26 L 33
      R 22 R 9 L 13 R 26 R 11 L 20 R 20 R 27 R 22 L 26 R 30 R 41 R 38 L
      30 R 46 R 25 L 46 R 21 L 11 L 38 L 45 L 47 R 38 R 1 L 15 R 29 R
      42 R 6 R 2 L 6 L 32 R 36 R 32 R 22 R 10 L 42 L 31 L 30 R 18 L 21
      R 25 L 16 L 9 R 20 L 8 R 38 R 50 L 18 R 3 R 18 R 16 R 16 R 10 L
      46 L 14 R 2 R 4 L 48 L 46 L 9 L 18 L 38 L 27 R 29 L 47 R 7 R 13 R
      9 R 6 L 26 L 46 R 40 R 36 R 37 R 38 R 12 R 9 L 28 R 36 R 41 L 37
      L 35 R 48 L 18 R 17 L 25 L 39 L 12 R 26 L 8 R 31 L 15 R 24 R 43 L
      40 R 19 L 32 R 17 R 30 R 24 L 41 R 5 L 23 L 36 R 21 L 39 R 43 L 2
      L 33 L 11 R 10 L 38 R 40 R 6 L 11 R 33 L 19 R 24 R 32 L 46 R 18 L
      4 R 22 R 32 R 13 L 47 R 8 L 9 R 12 L 18 L 32 L 24 L 19 R 37 R 15
      R 32 L 6 R 45 R 31 L 29 L 37 R 26 L 36 L 48 L 46 L 7 L 1 L 2 L 27
      R 4 L 45 L 37 R 30 L 49 L 34 L 39 R 36 R 26 R 3 R 1 R 20 R 48 R
      35 R 9 L 38 R 14 L 38 L 24 R 7 L 1 R 47 L 14 R 9 L 44 L 11 L 11 R
      38 R 14 R 41 R 41 R 1 R 8 R 20 R 46 L 3 R 43 L 49 R 8 R 22 R 27 L
      8 R 48 R 7 L 24 R 50 R 23 L 11 R 21 R 14 L 39 L 14 R 3 R 6 R 45 L
      42 R 39 L 6 R 25 L 16 L 21 R 44 R 35 L 10 L 30 L 7 L 22 L 13 R 44
      R 50 R 5 L 14 L 35 L 19 R 26 R 12 L 26 L 42 R 3 L 27 R 11 R 1 L
      10 R 43 L 20 L 28 L 30 L 25 L 21 L 18 R 26 L 19 R 27 L 22 R 36 R
      42 R 4 R 42 R 39 R 30 R 5 R 6 L 45 R 10 L 19 R 1 R 18 R 50 L 34 L
      2 L 35 R 22 R 45 R 28 R 8 R 45 L 37 L 38 R 7 L 44 L 26 L 36 L 29
      R 21 L 29 R 16 R 34 L 40 R 48 R 46 R 30 R 32 R 22 R 40 L 7 R 24 R
      24 R 11 R 4 R 5 R 28 R 35 R 29 R 17 L 2 R 42 R 45 L 35 R 14 R 33
      L 16 R 45 L 28 R 16 R 23 L 33 R 33 R 41 R 30 L 41 L 25 R 22 R 48
      R 42 L 47 R 41 L 17 L 5 R 42 L 27 L 26 R 42 L 5 R 3 R 1 L 16 L 36
      R 23 L 31 R 29 L 1 L 4 R 41 R 13 L 3 L 22 R 39 R 17 R 21 L 39 R
      29 L 31 L 18 R 31 R 30 L 18 L 11 L 49 L 42 R 21 L 31 L 50 R 50 R
      27 R 4 L 42 L 21 R 16 R 13 R 7 L 48 L 41 L 18 R 27 L 10 R 10 R 14
      L 31 L 28 L 15 R 37 L 41 L 41 L 13 L 9 L 18 L 25 R 11 R 9 L 1 L
      43 R 2 L 7 L 10 L 5 R 15 L 48 L 27 L 19 L 34 R 47 R 21 L 36 R 50
      R 49 R 12 R 49 L 25 R 6 R 44 L 45 R 40 R 32 R 3 L 49 L 37 L 31 R
      6 L 37 L 32 R 2 R 35 L 49 L 50 R 43 R 37 R 24 L 42 R 43 L 13 L 10
      L 38 L 4 L 36 R 41 L 21 R 44 R 17 R 18 R 40 R 9 R 15 L 2 L 31 R
      23 R 40 R 33 R 40 L 26 L 1 L 33 R 35 L 24 R 10 R 32 L 10 R 33 L
      45 R 2 R 37 R 47 L 30 L 28 R 49 R 21 L 24 R 25 R 46 R 44 R 25 R
      40 R 7 R 9 R 41 R 33 L 15 L 12 L 37 R 15 L 4 L 19 L 13 R 2 L 11 L
      10 L 41 R 2 R 1 L 15 L 26 L 19 R 45 R 41 L 45 L 18 L 1 R 30 R 37
      R 42 L 40 L 50 L 31 R 2 R 36 L 31 L 15 R 19 L 36 R 22 L 11 L 5 R
      22 L 50 R 33 R 42 L 42 R 32 L 40 R 17 L 10 R 9 L 14 R 16 L 9 L 22
      R 33 L 49 R 14 R 4 R 41 L 17 R 8 R 25 L 41 L 22 L 46 L 32 R 40 L
      28 L 48 R 38 L 41 R 26 R 24 R 23 R 25 R 47 R 22 R 45 R 27 R 46 L
      4 L 24 L 1 L 49 L 20 L 7 R 11 R 38 R 30 R 7 L 16 L 2 R 16 L 46 L
      41 L 16 R 31 R 7 R 5 R 14 R 17 L 21 R 8 L 30 R 27 R 17 R 38 L 15
      R 39 L 1 R 45 R 2 L 49 R 10 R 23 L 38 R 22 R 44 R 41 L 25 R 13 L
      11 L 17 L 32 L 32 R 21 L 17 L 43 R 35 R 10 L 29 L 32 L 36 R 26 L
      4 R 50 L 32 R 6 L 16 L 10 L 16 L 21 R 45 L 35 L 39 L 43 L 4 R 42
      R 38 L 37 R 34 R 32 L 9 L 26 R 50 L 47 L 25 R 18 R 46 R 47 R 42 L
      35 R 8))


;; by visual inspection and neovim (row column) reverse line1,51
(define *start-x* 51)
(define *start-y* 1)
(define *start-direction* 'east)

(define *x* *start-x*)
(define *y* *start-y*)
(define *direction* *start-direction*)


(define (turn-left)
  (cond
   ((eq? *direction* 'north) (set! *direction* 'west))
   ((eq? *direction* 'east) (set! *direction* 'north))
   ((eq? *direction* 'south) (set! *direction* 'east))
   ((eq? *direction* 'west) (set! *direction* 'south))
   (#t (error "turn-left"))))


(define (turn-right)
  (cond
   ((eq? *direction* 'north) (set! *direction* 'east))
   ((eq? *direction* 'east) (set! *direction* 'south))
   ((eq? *direction* 'south) (set! *direction* 'west))
   ((eq? *direction* 'west) (set! *direction* 'north))
   (#t (error "turn-right"))))


(define (forward-north n)
  (cond
   ((< n 1) #f)
   (#t (let* ((tmp (up *x* *y*))
	      (x (car tmp))
	      (y (car (cdr tmp)))
	      (out (lookup x y)))
	 (cond
	  ((equal? out #\.) (set! *x* x) (set! *y* y) (forward-north (- n 1)))
	  (#t #f))))))

	  
(define (forward-south n)
  (cond
   ((< n 1) #f)
   (#t (let* ((tmp (down *x* *y*))
	      (x (car tmp))
	      (y (car (cdr tmp)))
	      (out (lookup x y)))
	 (cond
	  ((equal? out #\.) (set! *x* x) (set! *y* y) (forward-south (- n 1)))
	  (#t #f))))))


(define (forward-east n)
  (cond
   ((< n 1) #f)
   (#t (let* ((tmp (right *x* *y*))
	      (x (car tmp))
	      (y (car (cdr tmp)))
	      (out (lookup x y)))
	 (cond
	  ((equal? out #\.) (set! *x* x) (set! *y* y) (forward-east (- n 1)))
	  (#t #f))))))

	  
(define (forward-west n)
  (cond
   ((< n 1) #f)
   (#t (let* ((tmp (left *x* *y*))
	      (x (car tmp))
	      (y (car (cdr tmp)))
	      (out (lookup x y)))
	 (cond
	  ((equal? out #\.) (set! *x* x) (set! *y* y) (forward-west (- n 1)))
	  (#t #f))))))

(define (facing)
  (cond
   ((eq? *direction* 'east) 0) ;; 0 for right
   ((eq? *direction* 'north) 3) ;; 3 for up 
   ((eq? *direction* 'south) 1) ;; 1 for down
   ((eq? *direction* 'west) 2) ;; 2 for left 
   (#t (error "facing"))))


   


(define (run)
  (let loop ((codes *codes*))
    (cond
     ((null? codes) (list *x* *y*))
     (#t (let ((instruction (car codes)))
	   (cond
	    ((eq? instruction 'L) (turn-left)(loop (cdr codes)))
	    ((eq? instruction 'R) (turn-right)(loop (cdr codes)))
	    ((integer? instruction)
	     (let ((forward #f))
	       (cond
		((eq? *direction* 'north) (set! forward forward-north))
		((eq? *direction* 'south) (set! forward forward-south))
		((eq? *direction* 'east) (set! forward forward-east))
		((eq? *direction* 'west) (set! forward forward-west))
		(#t (error "instruction+int+dir")))
	       (forward instruction)
	       (loop (cdr codes))))
	    (#t (error "instruction+run"))))))))


(define (part1)
  (run)
  (+ (* 1000 *y*)
     (* 4 *x*)
     (facing)))


;; thats not the right answer - boo hoo ...
;; 11582













		   
	  
     
  




	      

