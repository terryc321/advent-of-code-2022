
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

(import srfi-1)

(import (chicken pretty-print))


(import (chicken format))
;; (format #t "~a~%" 123)

(import (chicken process-context))
;;(current-directory)
;;(change-directory "day19/chicken")
;;geiser : C-c C-k : chicken compile-file

(import bindings)
;;(bind ((a b)(c d)(e f)) '((1 2)(3 4)(5 6)) (format #t "a b c d e f ~a ~a ~a ~a ~a ~%" a b c d e f))

(import (regex))


(define inputs (list
"Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 9 clay. Each geode robot costs 3 ore and 9 obsidian."
"Blueprint 2: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 20 clay. Each geode robot costs 4 ore and 8 obsidian."
"Blueprint 3: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 16 clay. Each geode robot costs 2 ore and 9 obsidian."
"Blueprint 4: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 20 clay. Each geode robot costs 4 ore and 16 obsidian."
"Blueprint 5: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 16 clay. Each geode robot costs 2 ore and 15 obsidian."
"Blueprint 6: Each ore robot costs 2 ore. Each clay robot costs 2 ore. Each obsidian robot costs 2 ore and 20 clay. Each geode robot costs 2 ore and 14 obsidian."
"Blueprint 7: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 7 clay. Each geode robot costs 3 ore and 20 obsidian."
"Blueprint 8: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 4 ore and 15 obsidian."
"Blueprint 9: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 7 clay. Each geode robot costs 2 ore and 7 obsidian."
"Blueprint 10: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 11 clay. Each geode robot costs 2 ore and 19 obsidian."
"Blueprint 11: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 20 clay. Each geode robot costs 2 ore and 12 obsidian."
"Blueprint 12: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 20 clay. Each geode robot costs 2 ore and 8 obsidian."
"Blueprint 13: Each ore robot costs 2 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 4 ore and 9 obsidian."
"Blueprint 14: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 18 clay. Each geode robot costs 3 ore and 8 obsidian."
"Blueprint 15: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 9 clay. Each geode robot costs 3 ore and 15 obsidian."
"Blueprint 16: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 11 clay. Each geode robot costs 2 ore and 16 obsidian."
"Blueprint 17: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 13 clay. Each geode robot costs 3 ore and 15 obsidian."
"Blueprint 18: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 16 clay. Each geode robot costs 3 ore and 20 obsidian."
"Blueprint 19: Each ore robot costs 2 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 19 clay. Each geode robot costs 4 ore and 8 obsidian."
"Blueprint 20: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 16 clay. Each geode robot costs 2 ore and 15 obsidian."
"Blueprint 21: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 7 clay. Each geode robot costs 2 ore and 19 obsidian."
"Blueprint 22: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 14 clay. Each geode robot costs 3 ore and 17 obsidian."
"Blueprint 23: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 8 clay. Each geode robot costs 2 ore and 8 obsidian."
"Blueprint 24: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 7 clay. Each geode robot costs 4 ore and 17 obsidian."
"Blueprint 25: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 16 clay. Each geode robot costs 3 ore and 9 obsidian."
"Blueprint 26: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 15 clay. Each geode robot costs 4 ore and 9 obsidian."
"Blueprint 27: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 20 clay. Each geode robot costs 4 ore and 7 obsidian."
"Blueprint 28: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 17 clay. Each geode robot costs 4 ore and 8 obsidian."
"Blueprint 29: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 12 clay. Each geode robot costs 3 ore and 17 obsidian."
"Blueprint 30: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 5 clay. Each geode robot costs 2 ore and 10 obsidian."
))


;; (string-match (regexp "^Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay.*") "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 9 clay. Each geode robot costs 3 ore and 9 obsidian.")
(define input-values
  (let ((xs (map (lambda (str)
	 (string-match (regexp "^Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.$") str))
		 inputs)))
    (map (lambda (x)
	   (cons (car x) (map string->number (cdr x))))
	 xs)))




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


what want to happen here is a multiple parallel blueprint procedures executing
continuation




|#


(define continuations (make-vector 40))

;; see if we can run this generic routine for one particular blueprint
;; like to run all blueprints in parallel if at all possible 
(define (make-blue-search blueprint-str
			  blueprint-no
			  cost-ore
			  cost-clay
			  cost-obs-ore cost-obs-clay
			  cost-geo-ore cost-geo-obs)
  (define max-geo 0)
  
  (define (blue1a robot-ore ore robot-clay clay robot-obs obs robot-geo geo step path)
    (cond
     ((< ore 0) #f)
     ((< clay 0) #f)
     ((< obs 0) #f)
     ((< geo 0) #f)
     ((and (> geo 0) (> step 23))
      (when (> geo max-geo)
	(set! max-geo geo)
	(format #t "we have blueprint ~a => ~a geode~%" blueprint-no max-geo)
	;;(format #t "~a~%" (reverse path))
	))
     ((> step 23) #f)
     (#t
      ;; feedback
      ;;(format #t "step ~a : ore:(~a ~a) clay:(~a ~a) obs:(~a ~a) geo:(~a ~a)~%" step robot-ore ore robot-clay clay robot-obs obs robot-geo geo)    
      (blue1b 0 0 0 0 robot-ore ore robot-clay clay robot-obs obs robot-geo geo step path))))

  (define (blue1b ro rc rob rg robot-ore ore robot-clay clay robot-obs obs robot-geo geo step path)
    
    ;; buy a geode
    (when (and (>= ore 2)(>= obs 7))
      (blue1b ro rc rob (+ 1 rg)
	      robot-ore  (- ore 2)
	      robot-clay clay
	      robot-obs  (- obs 7)
	      robot-geo  geo
	      step
	      (cons 'buy-geode path)
	      ))
    
    ;; buy an obsidian
    (when (and (>= ore 3)(>= clay 14))
      (blue1b ro rc (+ 1 rob) rg
	      robot-ore  (- ore 3)
	      robot-clay (- clay 14)
	      robot-obs  obs
	      robot-geo  geo
	      step
	      (cons 'buy-obsidian path)
	      ))

    ;; buy a clay robot for 2 ore
    (when (>= ore 2) 
      (blue1b ro (+ 1 rc) rob rg
	      robot-ore (- ore 2)
	      robot-clay clay
	      robot-obs  obs
	      robot-geo geo
	      step
	      (cons 'buy-clay path)
	      ))

    ;; buy an ore robot for 4 ore
    (when (>= ore 4)
      (blue1b
       (+ ro 1) rc rob rg
       robot-ore (- ore 4)
       robot-clay clay
       robot-obs  obs
       robot-geo geo
       step
       (cons 'buy-ore path)
       ))

    ;; do nothing - advance with current layout
    (blue1a 
     (+ ro robot-ore) (+ robot-ore ore)
     (+ rc robot-clay) (+ robot-clay clay)
     (+ rob robot-obs)  (+ robot-obs obs)
     (+ rg robot-geo) (+ robot-geo geo)
     (+ 1 step)
     (cons 'next path)
     ))

  (define (blue1-run)
    (let* ((ro 0)
	   (rc 0)
	   (rob 0)
	   (rg 0)
	   (robot-ore 1)(ore 0)
	   (robot-clay 0)(clay 0)
	   (robot-obs 0)(obs 0)
	   (robot-geo 0)(geo 0)
	   (step 0)
	   (path '()))
      (blue1a 
       robot-ore ore
       robot-clay clay
       robot-obs obs
       robot-geo geo
       step
       path
       )))

  (vector-set! continuations blueprint-no (lambda (v) (blue1-run)))
  (list blueprint-str blue1-run))


(define funcs (map (lambda (x) (apply make-blue-search x)) input-values))

;; here continuations should be fully defined

(define (run)
  (let ((v (car (command-line-arguments))))
    (cond
     ((string? v) (set! v (string->number v))))
    (when (integer? v)
      (format #t "running blueprint ~a~%" v)
      (let ((fn (vector-ref continuations v)))
	(fn 'go)))))

(run)




#|
(make-blue-search "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 9 clay. Each geode robot costs 3 ore and 9 obsidian."
  1
  4
  4
  4
  9
  3
  9)


(length '("Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 9 clay. Each geode robot costs 3 ore and 9 obsidian."
  1
  4
  4
  4
  9
  3
  9))

(make-blue-search (length '(blueprint-str blueprint-no
			  cost-ore
			  cost-clay
			  cost-obs-ore cost-obs-clay
			  cost-geo-ore cost-geo-obs))

;;(filter (lambda (x) (not (= (length x) 8))) input-values)
|#		   
		  
