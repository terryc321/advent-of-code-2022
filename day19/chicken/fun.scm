
;; -*- geiser-scheme-implementation: chicken -*-

#|

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

is it more a math formula than needs to be solved than a search problem

2 ore take 2 steps
7 obsidian takes 3 ore and 14 clay
3 ore takes 3 steps with 1 ore robot
14 clay take 28 ore

breadth first search rather than depth first search


|#


(import (chicken format))
;; (format #t "~a~%" 123)

(import (chicken process-context))
;;(current-directory)
;;(change-directory "day19/chicken")
;;geiser : C-c C-k : chicken compile-file

(import bindings)
;;(bind ((a b)(c d)(e f)) '((1 2)(3 4)(5 6)) (format #t "a b c d e f ~a ~a ~a ~a ~a ~%" a b c d e f))



;; maximum number of geodes achievable 
;; time limit 24 steps
;; prefix r means number of robots of ro : number of ore robots
;;
;; o ore
;; c clay
;; ob obsidian
;; g geo
;; (define (blue1b rore rclay robsidian rgeode r1 r2 r3 r4 step)
;;   (bind ((robot-ore ore)(robot-clay clay)(robot-obsidian obsidian)(robot-geode geode))
;; 	(list r1 r2 r3 r4)
;; 	;;(format #t "ore:~a clay:~a obs:~a geo:~a step:~a~%" r1 r2 r3 r4 step)
;; 	(blue1 (list robot-ore (+ rore ore))
;; 	       (list robot-clay (+ rclay clay))
;; 	       (list robot-obsidian (+ robsidian obsidian))
;; 	       (list robot-geode (+ rgeode geode))
;; 	       (+ step 1))))

(define max-geo 0)

#|
strategy 1 : do nothing at end have 24 ores
strategy 2 : buy only ore robots at end have ? ores
step r o
0    1 0
1    1 1
2    1 2
3    1 3
4    1 4
5    2 1
6    2 3
7    2 5

Each ore robot costs 4 ore.
Each clay robot costs 2 ore.
Each obsidian robot costs 3 ore and 14 clay.
Each geode robot costs 2 ore and 7 obsidian.
  2 ore + 7 ( 3 ore + 14 clay )
                      14 * 2 ore
   2 ore + 21 ore + 28 ore
  (+ 2 21 28 )  51

last state = end of minute N

possible next states .... 
next state = end of minute N + 1 

leave on the table ? would buy as many as could ?
how debug ?
how reason about it ?

what does an optimal buying strategy look like ?

delay buying ... until never ... never bought anything ... cannot make geodes

|#




(define (blue1 ro rc rob rg robot-ore ore robot-clay clay robot-obs obs robot-geo geo step)
  (cond
   ((< ore 0) #f)
   ((< clay 0) #f)
   ((< obs 0) #f)
   ((< geo 0) #f)
   ((and (> geo 0) (> step 24))
    (when (> geo max-geo)
      (set! max-geo geo)
      (format #t "we have ~a geode~%" max-geo)))
   ((> step 24) #f)
   (#t
      (format #t "~%step ~a : robots (ore ~a ~a) (clay ~a ~a) : " step robot-ore ore robot-clay clay)
      
      ;; buy an obsidian
      (when (and (>= ore 3)(>= clay 14))
	(blue1 ro rc (+ 1 rob) rg
	       robot-ore  (- ore 3)
	       robot-clay (- clay 14)
	       robot-obs  obs
	       robot-geo  geo
	       step))

      ;; buy a clay robot for 2 ore
      (when (>= ore 2) 
	  (blue1 ro (+ 1 rc) rob rg
	   robot-ore (- ore 2)
	   robot-clay clay
	   robot-obs  obs
	   robot-geo geo
	   step))

      ;; buy an ore robot for 4 ore
      (when (>= ore 4)
	(blue1
	 (+ ro 1) rc rob rg
	 robot-ore (- ore 4)
	 robot-clay clay
	 robot-obs  obs
	 robot-geo geo
	 step))

      ;; do nothing
      (let ((robot-ore (+ ro robot-ore))
	    (robot-clay (+ rc robot-clay))
	    (robot-obs (+ rob robot-obs))
	    (robot-geo (+ rg robot-geo)))    
	(blue1 0 0 0 0       
	       robot-ore (+ robot-ore ore)
	       robot-clay (+ robot-clay clay)
	       robot-obs  (+ robot-obs obs)
	       robot-geo (+ robot-geo geo)
	       (+ 1 step))))))




(define (blue1-run)
  (let* ((ro 0)
	 (rc 0)
	 (rob 0)
	 (rg 0)
	 (robot-ore 1)(ore 0)
	 (robot-clay 0)(clay 0)
	 (robot-obs 0)(obs 0)
	 (robot-geo 0)(geo 0)
	 (step 0))
    (blue1 ro rc rob rg
	   robot-ore ore
	   robot-clay clay
	   robot-obs obs
	   robot-geo geo
	   step)))

;;(blue1-run)

