
;; chicken scheme 
(import (chicken format))

;; functional hash table , simply cons id-value onto front of a list
;; original list untouched
;; or COPY vector and do that


#|

robots vs resources

     resources 
     ore  cly obs
ore  4
cly  2
obs  3    14
geo  2        7

time limit 24 mins completed

|#

(define run-config
  (lambda ()
    #( 4
       2
       3 14
       2    7)))

(define run-ore (lambda (c) (vector-ref c 0)))
(define run-cly (lambda (c) (vector-ref c 1)))
(define run-obs-ore (lambda (c) (vector-ref c 2)))
(define run-obs-cly (lambda (c) (vector-ref c 3)))
(define run-geo-ore (lambda (c) (vector-ref c 4)))
(define run-geo-obs (lambda (c) (vector-ref c 5)))

(define dummy-run-config
  (lambda ()
    #( run-ore
       run-cly
       run-obs-ore run-obs-cly
       run-geo-ore run-geo-obs)))



#|
macro expression
   (robot-ore <state>)

|#
;; (define-syntax robot-ore
;;   (er-macro-transformer
;;    (lambda (exp rename compare)
;;      (let ((state (car (cdr exp))))
;;        `(vector-ref ,state 0)
;;        ;;(list 'quote state)
;;        ))))

(define initial-state
  (lambda ()
    #(  1 0 0 0
	0 0 0 0	
	0
	0 0 0 0)))

(define robot-ore (lambda (s)  (vector-ref s 0)))
(define robot-clay (lambda (s) (vector-ref s 1)))
(define robot-obs (lambda (s)  (vector-ref s 2)))
(define robot-geo (lambda (s)  (vector-ref s 3)))

(define resource-ore (lambda (s)  (vector-ref s 4)))
(define resource-clay (lambda (s) (vector-ref s 5)))
(define resource-obs (lambda (s)  (vector-ref s 6)))
(define resource-geo (lambda (s)  (vector-ref s 7)))

(define sim-time (lambda (s) (vector-ref s 8)))

(define future-robot-ore (lambda (s)  (vector-ref s 9)))
(define future-robot-clay (lambda (s) (vector-ref s 10)))
(define future-robot-obs (lambda (s)  (vector-ref s 11)))
(define future-robot-geo (lambda (s)  (vector-ref s 12)))



;;------------




(define dummy-state
  (lambda ()
    #( ore clay obs geo
       n-ore n-clay n-obs n-geo
       stime
       f-ore f-clay f-obs f-geo)))

