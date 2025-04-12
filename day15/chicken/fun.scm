

#|
chicken scheme
^Sensor at x=\([0-9]+\), y=\([0-9]\)+: closest beacon is at x=\([0-9]+\), y=\([0-9]+\)
|#

(import (chicken syntax))

(import (chicken format))

(import (srfi-1))

(import (chicken pretty-print))

(import (chicken process-context))
;; (current-directory)
;; (change-directory "day15/chicken")

(import (chicken string))
;; string-split on newlines

(import (srfi-69))
;; hash tables

(import regex)

(import matchable)

(define input-text
  "
Sensor at x=193758, y=2220950: closest beacon is at x=652350, y=2000000
Sensor at x=3395706, y=3633894: closest beacon is at x=3404471, y=3632467
Sensor at x=3896022, y=3773818: closest beacon is at x=3404471, y=3632467
Sensor at x=1442554, y=1608100: closest beacon is at x=652350, y=2000000
Sensor at x=803094, y=813675: closest beacon is at x=571163, y=397470
Sensor at x=3491072, y=3408908: closest beacon is at x=3404471, y=3632467
Sensor at x=1405010, y=486446: closest beacon is at x=571163, y=397470
Sensor at x=3369963, y=3641076: closest beacon is at x=3404471, y=3632467
Sensor at x=3778742, y=2914974: closest beacon is at x=4229371, y=3237483
Sensor at x=1024246, y=3626229: closest beacon is at x=2645627, y=3363491
Sensor at x=3937091, y=2143160: closest beacon is at x=4229371, y=3237483
Sensor at x=2546325, y=2012887: closest beacon is at x=2645627, y=3363491
Sensor at x=3505386, y=3962087: closest beacon is at x=3404471, y=3632467
Sensor at x=819467, y=239010: closest beacon is at x=571163, y=397470
Sensor at x=2650614, y=595151: closest beacon is at x=3367919, y=-1258
Sensor at x=3502942, y=6438: closest beacon is at x=3367919, y=-1258
Sensor at x=3924022, y=634379: closest beacon is at x=3367919, y=-1258
Sensor at x=2935977, y=2838245: closest beacon is at x=2645627, y=3363491
Sensor at x=1897626, y=7510: closest beacon is at x=3367919, y=-1258
Sensor at x=151527, y=640680: closest beacon is at x=571163, y=397470
Sensor at x=433246, y=1337298: closest beacon is at x=652350, y=2000000
Sensor at x=2852855, y=3976978: closest beacon is at x=3282750, y=3686146
Sensor at x=3328398, y=3645875: closest beacon is at x=3282750, y=3686146
Sensor at x=3138934, y=3439134: closest beacon is at x=3282750, y=3686146
Sensor at x=178, y=2765639: closest beacon is at x=652350, y=2000000
Sensor at x=3386231, y=3635056: closest beacon is at x=3404471, y=3632467
Sensor at x=3328074, y=1273456: closest beacon is at x=3367919, y=-1258
Sensor at x=268657, y=162438: closest beacon is at x=571163, y=397470
")

(set! input-text (string-split input-text "\n"))

(define example-text
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
")

(set! example-text (string-split example-text "\n"))


(define re (regexp ".*x=([+-]?[0-9]+).*y=([+-]?[0-9]+):.*x=([+-]?[0-9]+),.y=([+-]?[0-9]+)"))
;; (define m1 (string-match re (car text)))
;; (match m1
;;       [(str x1 y1 x2 y2) (list 'x1 x1 'y1 y1 'x2 x2 'y2 y2)]
;;       [_ 'no-match-found] )

(define (coords text)
  (map (lambda (str)
	 (let ((m1 (string-match re str)))
	   (match m1
	     [(str x1 y1 x2 y2) (map string->number (list x1 y1 x2 y2))]
	     [_ 'no-match])))
       text))

(define input-coords (coords input-text))
(define example-coords (coords example-text))

;; convert string to integer
#|
example-coords
((2 18 -2 15)
 (9 16 10 16)
 (13 2 15 3)
 (12 14 10 16)
 (10 20 10 16)
 (14 17 10 16)
 (8 7 2 10)
 (2 0 2 10)
 (0 11 2 10)
 (20 14 25 17)
 (17 20 21 22)
 (16 7 15 3)
 (14 3 15 3)
 (20 1 15 3))

coordinate of sensor X,Y and nearest beacon X,Y
input-coords
((193758 2220950 652350 2000000)
 (3395706 3633894 3404471 3632467)
 (3896022 3773818 3404471 3632467)
 (1442554 1608100 652350 2000000)
 (803094 813675 571163 397470)
 (3491072 3408908 3404471 3632467)
 (1405010 486446 571163 397470)
 (3369963 3641076 3404471 3632467)
 (3778742 2914974 4229371 3237483)
 (1024246 3626229 2645627 3363491)
 (3937091 2143160 4229371 3237483)
 (2546325 2012887 2645627 3363491)
 (3505386 3962087 3404471 3632467)
 (819467 239010 571163 397470)
 (2650614 595151 3367919 -1258)
 (3502942 6438 3367919 -1258)
 (3924022 634379 3367919 -1258)
 (2935977 2838245 2645627 3363491)
 (1897626 7510 3367919 -1258)
 (151527 640680 571163 397470)
 (433246 1337298 652350 2000000)
 (2852855 3976978 3282750 3686146)
 (3328398 3645875 3282750 3686146)
 (3138934 3439134 3282750 3686146)
 (178 2765639 652350 2000000)
 (3386231 3635056 3404471 3632467)
 (3328074 1273456 3367919 -1258)
 (268657 162438 571163 397470))

|#

(define (manhattan p1 p2)
  (let ((x1 (car p1))
	(y1 (car (cdr p1)))
	(x2 (car p2))
	(y2 (car (cdr p2))))
    (+ (abs (- x2 x1)) (abs (- y2 y1)))))


;; two beacons cannot be at the same distance to sensor
;; return a function says either no cannot be here since inside distance or maybe cannot ascertain
(define make-f
  (lambda (ic)
    (match ic
      ((x y x2 y2) (let ((man (manhattan (list x y) (list x2 y2))))
		     (lambda (x3 y3)
		       (let ((man2 (manhattan (list x y) (list x3 y3))))
			 (not (<= man2 man))))))
      (_ (error "no match creating f")))))



;;(define problem-coords example-coords)
(define problem-coords input-coords)


(define funs (map make-f problem-coords))

(define (beacon-possible? x y)
  (let ((out (filter (lambda (o) (eq? o #f)) (map (lambda (fn) (fn x y)) funs))))
    (cond
     ((null? out) #t)
     (#t #f))))


;; is there definitely a sensor here
(define sensor?
  (let ((sensors (make-hash-table)))
    (map (lambda (ic)
	   (match ic
	     ((x y _ _)  (hash-table-set! sensors (list x y) #t))
	     (_ (error "no match creating sensor?"))))
	 problem-coords)
    (lambda (x y)
      (hash-table-ref/default sensors (list x y) #f))))

(define beacon?
  (let ((beacons (make-hash-table)))
    (map (lambda (ic)
	   (match ic
	     ((_ _ x y)  (hash-table-set! beacons (list x y) #t))
	     (_ (error "no match creating sensor?"))))
	 problem-coords)
    (lambda (x y)
      (hash-table-ref/default beacons (list x y) #f))))



(define (viz x y)
  (cond
   ((sensor? x y) #\S)
   ((beacon? x y) #\B)
   ((not (beacon-possible? x y)) #\#)
   (#t #\.)))

;; x y where a new beacon cannot be
(define (new-beacon-cannot x y)
  (cond
   ((sensor? x y) #t)
   ((beacon? x y) #f)
   ((not (beacon-possible? x y)) #t)
   (#t #f)))

;; is there definitely a beacon here
;; 
;; x [-4 ..+26] 
;; y [ +9 .. +10]
(define (show)
  (newline)
  (let loop ((x -4)(y 9))
    (cond
     ((> y 11) #f)
     ((> x 26) (newline)  (loop -4 (+ y 1)))
     ((= x -4)
      (format #t "~a :" y)
      (format #t "~a" (viz x y))
      (loop (+ x 1) y))
     (#t (format #t "~a" (viz x y))			  
	 (loop (+ x 1) y))))
  (newline))


(define (count-beacon-not-present ygiven minx maxx)
  (let ((count 0))
    (let loop ((x minx)(y ygiven))
      (cond
       ((not (= y ygiven)) #f)
       ((> x maxx) (newline) #f)
       (#t (when (new-beacon-cannot x y)
	     (set! count (+ 1 count)))
	   (loop (+ x 1) y))))
    count))


(define demo
  (let ((y 10)(minx -2)(maxx 27))
    (count-beacon-not-present y minx maxx)))

(define (demo2 iter multiple)
  (let* ((y 2000000)
	 (minx (- (inexact->exact (floor (* y multiple)))))
	 (maxx (+ (inexact->exact (floor (* y multiple)))))
	 (rng (abs (- maxx minx))))
    (time (format #t "(iter ~a : rng ~a : count : ~a) : ~%" iter rng (count-beacon-not-present y minx maxx)))
    (demo2 (+ iter 1) (* multiple 2))))


;; (incf s)
(define-syntax incf!
  (er-macro-transformer
   (lambda (x r c)
     (let ((sym (car (cdr x))))
       `(set! ,sym (+ ,sym 1))))))

;; (let ((x 1))
;;   (incf! x)
;;   x)


(define (part1)
  (let* ((y 2000000)
	 (x1 -1)
	 (x2 1)
	 (x0 0)
	 (count 0))
    ;; one at 0
    (when (new-beacon-cannot x0 y) (incf! count))
    (let loop ((x1 -1)(x2 1)(iter 0))
      (when (zero? (modulo iter 10000)) (format #t "(count = ~a) ~%" count))
      (when (new-beacon-cannot x1 y) (incf! count))
      (when (new-beacon-cannot x2 y) (incf! count))
      (loop (- x1 1) (+ x2 1)(+ iter 1)))))


(part1)

#|

5832528

ANSWER ACCEPTED !



|#

    






;; FASTER TO COMPUTE rather than use hash to store results !

;; ;; memoize result for a given y - makes a fun such contains a hash table by lexical scope ,
;; ;; updates on each request for x if not in hash
;; ;; if in hash , simply gives stored result back
;; (define (make-count-beacon-not-present y)  
;;   (let ((hash (make-hash-table)))
;;     (lambda (x)
;;       (let ((out (hash-table-ref/default hash x 'unknown)))
;; 	(cond
;; 	 ((eq? out 'unknown) (let ((result (new-beacon-cannot x y)))
;; 			       (hash-table-set! hash x result)
;; 			       result))
;; 	 (#t out))))))


;; (define (fast-count-beacon-not-present ygiven)
;;   (let ((beacon-fn (make-count-beacon-not-present ygiven)))
;;     (lambda (minx maxx)
;;       (let ((count 0))
;; 	(let loop ((x minx))
;; 	  (cond
;; 	   ((> x maxx) #f)
;; 	   (#t (when (beacon-fn x)
;; 		 (set! count (+ 1 count)))
;; 	       (loop (+ x 1)))))
;; 	count))))




;; (define (brute)
;;   (define (demo2 multiple)
;;     (let* ((y 2000000)
;; 	   (minx (- (inexact->exact (floor (* multiple y)))))
;; 	   (maxx (+ (inexact->exact (* multiple y))))
;; 	   (beacon-fn (fast-count-beacon-not-present y)))
;;       (time (format #t "~a ~a -> [~a] :: " multiple (abs (- maxx maxx)) (beacon-fn minx maxx)))
;;       (demo2 (* multiple 2))))
;;   (demo2 0.01))




;; dunno how to configure minimum x to maximum 






    






