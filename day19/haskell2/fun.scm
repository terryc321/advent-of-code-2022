

#|

TODO TODO TODO /////..... some way to write these routines so its not so much a ball ache
perhaps using some macros and eval ...



scheme for the win
aoc2022 day 19 

robots themselves ore clay obsidian geode
rocks  ore clay obsidian geode

24 days 


|#

;; seems like have slib availables...
;; (use-modules (ice-9 slib))
;; (require 'primes)
;; (prime? 13)


(use-modules (ice-9 optargs))
;; (lambda*
;; (ice-9 optargs)

(define* (my-function #:key (param1 1) (param2 "default"))
  (format #t "param1: ~A, param2: ~A~%" param1 param2))

;; calling / using the procedure with named parameters
;;(my-function #:param1 1 #:param2 2)

(define* (my-function2 #:key
		       (param1 (error "badparam 1"))
		       (param2 (error "badparam2 default")))
  (format #t "param1: ~A, param2: ~A~%" param1 param2))

(my-function2 #:param1 1 #:param2 2)
(my-function2 #:param1 1 )



;; records
(use-modules (srfi srfi-9))

(define-record-type <employee>
  (make-employee name age salary)
  employee?
  (name    employee-name)
  (age     employee-age    set-employee-age!)
  (salary  employee-salary set-employee-salary!))

<employee>

(define fred (make-employee "Fred" 45 20000.00))

(employee? fred)  ;;      ⇒ #t
(employee-age fred) ;;    ⇒ 45
(set-employee-salary! fred 25000.00)  ;; pay rise

#|
rocks

robot-ore robot-clay robot-obsidian robot-geode
rock-ore rock-clay rock-obsidian rock-g

time robots rocks

if start with n robots then after time step , i will have +n rocks of that type
(map + '(1 2 3 4) '(10 20 30 40))

costs = ore robot
        clay robot
        obsidian robot
        geode robot

Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.


|#

;; specialise a specific set of costs
;; implicit costs we know
;;  r- are robots
;; otherwise rocks

(define cost-robot-ore-ore 4)

(define cost-robot-clay-ore 2)

(define cost-robot-obs-ore 3)
(define cost-robot-obs-clay 14)

(define cost-robot-geo-ore 2)
(define cost-robot-geo-obs 7)

(define max-geo-produced 0)

(define (blueprint1)
  (set! cost-robot-ore-ore 4)

  (set! cost-robot-clay-ore 2)

  (set! cost-robot-obs-ore 3)
  (set! cost-robot-obs-clay 14)

  (set! cost-robot-geo-ore 2)
  (set! cost-robot-geo-obs 7)

  (set! max-geo-produced 0)
  (run))

(define (blueprint2)
  (set! cost-robot-ore-ore 2)
  (set! cost-robot-clay-ore 3)
  (set! cost-robot-obs-ore 3)
  (set! cost-robot-obs-clay 8)
  (set! cost-robot-geo-ore 3)
  (set! cost-robot-geo-obs 12)
  (set! max-geo-produced 0)
  (run))

(define (report	geo)
  (when (> geo max-geo-produced)
    (set! max-geo-produced geo)
    (format #t "max geo produced ~a ~%" max-geo-produced)))


(define (run)
  (let ((time 0)
	(ore 0)	(clay 0)(obs 0)	(geo 0)
	(robot-ore 1)(robot-clay 0)(robot-obs 0)(robot-geo 0)
	(buy-ore 0)(buy-clay 0)(buy-obs 0)(buy-geo 0))
    (decide time ore clay obs geo
	    robot-ore robot-clay robot-obs robot-geo
	    buy-ore buy-clay buy-obs buy-geo)))

;; end condition is when time expires
;; (define (decide time ore clay obs geo robot-ore robot-clay robot-obs robot-geo)
;;   (cond
;;    ((> time 24) (report geo))
;;    (#t
    
;;     )))

(define* (decide #:key
		 (time (error "badparam"))
		 (ore (error "badparam"))
		 (clay (error "badparam"))
		 (obs (error "badparam"))
		 (geo (error "badparam"))
		 (robot-ore (error "badparam"))
		 (robot-clay (error "badparam"))
		 (robot-obs (error "badparam"))
		 (robot-geo (error "badparam")))
  (cond
   ((> time 25) (report geo))
   (#t ;;




    
    

(my-function2 #:param1 1 #:param2 2)
(my-function2 #:param1 1 )

  
#|
(define decide
  (lambda (time ore clay obs geo 
  		robot-ore robot-clay robot-obs robot-geo
		buy-ore buy-clay buy-obs buy-geo)
    (cond
     ((> time 25) (report geo))
     (#t
      ;; buy one geode robot if we can 
      (when (and (>= ore cost-robot-geo-ore)(>= obs cost-robot-geo-obs))
	(decide time (- ore cost-robot-geo-ore) clay (- obs cost-robot-geo-obs) geo
		robot-ore robot-clay robot-obs robot-geo
		buy-ore buy-clay buy-obs (+ 1 buy-geo)))
      
      ;; buy as many obsidian robots we can
      (when (and (>= ore cost-robot-obs-ore)(>= clay cost-robot-obs-clay))
	(decide time (- ore cost-robot-obs-ore) (- clay cost-robot-obs-clay) obs geo
		robot-ore robot-clay robot-obs robot-geo
		buy-ore buy-clay (+ 1 buy-obs) buy-geo))
      
      ;; buy as many clay robots we can
      (when (>= ore cost-robot-clay-ore)
	(decide time (- ore cost-robot-clay-ore) clay obs geo
		robot-ore robot-clay robot-obs robot-geo
		buy-ore (+ 1 buy-clay) buy-obs buy-geo))
      
      ;; buy as many ore robots we can
      (when (>= ore cost-robot-ore-ore)
	(decide time (- ore cost-robot-ore-ore) clay obs geo
		robot-ore robot-clay robot-obs robot-geo
		(+ 1 buy-ore) buy-clay buy-obs buy-geo))

      ;; do nothing - commit to this course action - reset buy robot counts !
      (let ((buy-ore2 0)(buy-clay2 0)(buy-obs2 0)(buy-geo2 0))
	(decide (+ time 1)
		(+ ore robot-ore)
		(+ clay robot-clay)
		(+ obs robot-obs)
		(+ geo robot-geo)
		(+ robot-ore buy-ore) (+ robot-clay buy-clay) (+ robot-obs buy-obs)
		(+ robot-geo buy-geo)
		buy-ore2 buy-clay2 buy-obs2 buy-geo2))
      ))))
|#

#|
(define next
  (lambda (time robots rocks costs)
    (cond
     ((> time 25) 'done-report-your-rocks)
     (#t
      ;; current robots generate me rocks
      (let ((future-rocks robots)) 
      
	;; future robots = current robots + bought robots
	(let ((future-rocks2 (map + future-rocks spent-rocks))) 
	  (next (+ time 1) future-robots future-rocks2 costs)
	  ))))))
|#

#|
;; how figure out what did was sub optimal ? 
(define next
  (lambda (time robots rocks costs)
    (cond
     ((> time 25) 'done-report-your-rocks)
     (#t
      ;; current robots generate me rocks
      (let ((future-rocks robots)) 
      
	;; future robots = current robots + bought robots
	(let ((future-rocks2 (map + future-rocks spent-rocks))) 
	  (next (+ time 1) future-robots future-rocks2 costs)
	  ))))))
|#







