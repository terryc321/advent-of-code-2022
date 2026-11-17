

#| 

advent code 2022 day 19 robot puzzle 

start with one ore robot

costs for corresponding robots ore , clay , obsidian then geode robot

blueprint 1 

4 0 0 
2 0 0
3 14 0
2 0 7  

blueprint 2 

2 0 0 
3 0 0 
3 8 0 
3 0 12 

we have to figure out given appropriate resources at any time , 
what are the possible choices of robots to build ?

intuition do we build as soon as possible ? or wait until some future time ?

we have to build a list of possible choices available , then try each one i guess if we wish to 
be sure of exhaustion 

there may be several different paths to a solution of maximum of N geodes after 24 minute time limit

* ideas *

- distinct buying decisions 

how many ore robots can we buy ? 
none 
one 
two 
three - too expensive {{ now we have a set of possible ore robots with respective costs }}
call this { ORE SET }

how many clay robots can we buy 
for each possible set of ore robots X cross product with { none .. one .. two ... until too expensive }}
call this { CLAY SET } 

makes a new set of possibilities

how many obsidian robots can we buy ? 
for each in clay set X cross product with { none ... one ... two until too expensive }
call this { OBSIDIAN SET }

how many geode robots can we buy ? 
for each in obsidian set X cross product with { none ... one ... two until too expensive }
call this { GEODE SET }

(import (chicken format))
;; fmt yas snippet snippets in ~/.emacs.d/snippets/scheme-mode
;; (format #t "a= ~a~%" a)

(import (chicken pretty-print))
;; pp pretty print

;; macros ?

{{ 
if we have a structure to hold current mineral counts 
ore clay obsidian geode  
0   0    0        0 : minerals
1   0    0        0 : working robots
0   1    0        1 : building robots

we begin with 1 ore robot
}}

 |#

(import (chicken format))
(import (chicken pretty-print))

;; 1. Define the record type with its fields
;; m minerals
;; r robots have
;; b building
(define-record state m1 m2 m3 m4 r1 r2 r3 r4 b1 b2 b3 b4)

;; the first starting state
(define state1 (make-state 0 0 0 0 ;; no minerals 
                           1 0 0 0 ;; 1 ore robot
                           0 0 0 0 ;; not building any robots yet
                           ))

(define-record blueprint o1 o2 o3 ;; ore robot costs
                         c1 c2 c3 ;; clay robot costs
                         p1 p2 p3 ;; obsidian robot costs
                         g1 g2 g3 ;; geode robot costs
                         )

(define blue1 (make-blueprint 4 0 0 
                              2 0 0 
                              3 14 0
                              2 0 7))

(define blue2 (make-blueprint 2 0 0 
                              3 0 0
                              3 8 0
                              3 0 12))

;; from a given state -- derive the next possible states 
;; hopefully they should be all unique

(define (show-state s)
  (format #t "* state * ~%")
  (format #t "* minerals : ~a ~a ~a ~a~%" (state-m1 s) (state-m2 s) (state-m3 s) (state-m4 s))
  (format #t "* robots   : ~a ~a ~a ~a~%" (state-r1 s) (state-r2 s) (state-r3 s) (state-r4 s))
  (format #t "* building : ~a ~a ~a ~a~%" (state-b1 s) (state-b2 s) (state-b3 s) (state-b4 s)))

(define (show-blueprint s)
  (format #t "* blueprint * ~%")
  (format #t "* ore      : ~a ~a ~a ~%" (blueprint-o1 s) (blueprint-o2 s) (blueprint-o3 s))
  (format #t "* clay     : ~a ~a ~a ~%" (blueprint-c1 s) (blueprint-c2 s) (blueprint-c3 s))
  (format #t "* obsidian : ~a ~a ~a ~%" (blueprint-p1 s) (blueprint-p2 s) (blueprint-p3 s))
  (format #t "* geode    : ~a ~a ~a ~%" (blueprint-g1 s) (blueprint-g2 s) (blueprint-g3 s)))

;; debug flag 
(define (show-state-change s s2 blue msg)
  (when #f 
    (format #t "state change diagram : ~a ~%" msg)
    (show-state s)
    (format #t " ==> ~%")
    (show-state s2)
    (format #t " ==> ~%")
    (show-blueprint blue)
    (format #t " ****** ~%")))

#| 
do we need to determine which state we have already seen before ?

|#  


;; ORE
;; look at blueprint of ore robots o1 o2 o3 
(define (find-next-states-1 state blue)
  ;;(format #t "entering find-next-states-1~%")
  (let ((o1 (blueprint-o1 blue)) (o2 (blueprint-o2 blue)) (o3 (blueprint-o3 blue))
        (m1 (state-m1 state)) (m2 (state-m2 state)) (m3 (state-m3 state)) (m4 (state-m4 state))
        (r1 (state-r1 state)) (r2 (state-r2 state)) (r3 (state-r3 state)) (r4 (state-r4 state))
        (b1 0) (b2 0) (b3 0) (b4 0)
        (next '()))
    ;; 
    (letrec ((foo (lambda (n)                     
                      (cond 
                       ((and (<= (* n o1) m1)
                             (<= (* n o2) m2)
                             (<= (* n o3) m3))
                        (let ((em1 (- m1 (* n o1)))
                              (em2 (- m2 (* n o2)))
                              (em3 (- m3 (* n o3)))
                              (em4 m4))
                          (let ((state2 (make-state em1 em2 em3 em4
                                                    r1 r2 r3 r4
                                                    n 0 0 0)))
                            (set! next (cons state2 next))
                            ;; (let ((msg (format #f "we can afford to buy ~a ore robots~%" n)))
                            ;;   (show-state-change state state2 blue msg))
                            (foo (+ n 1)))))
                       (#t #f)))))
      (foo 0)
      next
      )))



;; CLAY
(define (find-next-states-2 state blue)
  ;;(format #t "entering find-next-states-2~%")
  (let ((c1 (blueprint-c1 blue)) (c2 (blueprint-c2 blue)) (c3 (blueprint-c3 blue))
        (m1 (state-m1 state)) (m2 (state-m2 state)) (m3 (state-m3 state)) (m4 (state-m4 state))
        (r1 (state-r1 state)) (r2 (state-r2 state)) (r3 (state-r3 state)) (r4 (state-r4 state))
        (b1 (state-b1 state)) (b2 0) (b3 0) (b4 0)
        (next '()))
    ;;
    (letrec ((foo (lambda (n)                     
                      (cond 
                       ((and (<= (* n c1) m1)
                             (<= (* n c2) m2)
                             (<= (* n c3) m3))
                        ;; 
                        (let ((em1 (- m1 (* n c1)))
                              (em2 (- m2 (* n c2)))
                              (em3 (- m3 (* n c3)))
                              (em4 m4))
                        (let ((state2 (make-state em1 em2 em3 em4
                                                  r1 r2 r3 r4
                                                  b1 n 0 0)))
                          (set! next (cons state2 next))
                          ;; (let ((msg (format #f "we can afford to buy ~a clay robots~%" n)))
                          ;;   (show-state-change state state2 blue msg))
                          (foo (+ n 1)))))
                       (#t #f)))))
      (foo 0)
      next
      )))




;; OBSIDIAN 
(define (find-next-states-3 state blue)
  ;;(format #t "entering find-next-states-3~%")
  (let ((p1 (blueprint-p1 blue)) (p2 (blueprint-p2 blue)) (p3 (blueprint-p3 blue))
        (m1 (state-m1 state)) (m2 (state-m2 state)) (m3 (state-m3 state)) (m4 (state-m4 state))
        (r1 (state-r1 state)) (r2 (state-r2 state)) (r3 (state-r3 state)) (r4 (state-r4 state))
        (b1 (state-b1 state)) (b2 (state-b2 state)) (b3 0) (b4 0)
        (next '()))
    ;;
    (letrec ((foo (lambda (n)                     
                      (cond 
                       ((and (<= (* n p1) m1)
                             (<= (* n p2) m2)
                             (<= (* n p3) m3))
                        ;; 
                        (let ((em1 (- m1 (* n p1)))
                              (em2 (- m2 (* n p2)))
                              (em3 (- m3 (* n p3)))
                              (em4 m4))
                        (let ((state2 (make-state em1 em2 em3 em4
                                                  r1 r2 r3 r4
                                                  b1 b2 n 0)))
                          (set! next (cons state2 next))
                          ;; (let ((msg (format #f "we can afford to buy ~a obsidian robots~%" n)))
                          ;;   (show-state-change state state2 blue msg))
                          (foo (+ n 1)))))
                       (#t #f)))))
      (foo 0)
      next
      )))

   
;; GEODE                              
;; look at blueprint of geode robots g1 g2 g3
(define (find-next-states-4 state blue)
  ;;(format #t "entering find-next-states-4~%")
  (let ((g1 (blueprint-g1 blue)) (g2 (blueprint-g2 blue)) (g3 (blueprint-g3 blue))
        (m1 (state-m1 state)) (m2 (state-m2 state)) (m3 (state-m3 state)) (m4 (state-m4 state))
        (r1 (state-r1 state)) (r2 (state-r2 state)) (r3 (state-r3 state)) (r4 (state-r4 state))
        (b1 (state-b1 state)) (b2 (state-b2 state)) (b3 (state-b3 state)) (b4 0)
        (next '()))
    ;;
    (letrec ((foo (lambda (n)                     
                      (cond 
                       ((and (<= (* n g1) m1)
                             (<= (* n g2) m2)
                             (<= (* n g3) m3))
                        ;;
                        (let ((em1 (- m1 (* n g1)))
                              (em2 (- m2 (* n g2)))
                              (em3 (- m3 (* n g3)))
                              (em4 m4))
                        (let ((state2 (make-state em1 em2 em3 em4
                                                  r1 r2 r3 r4
                                                  b1 b2 b3 n)))
                          (set! next (cons state2 next))
                          ;; (let ((msg (format #f "we can afford to buy ~a geode robots~%" n)))
                          ;;   (show-state-change state state2 blue msg))                          
                          (foo (+ n 1)))))
                       (#t #f)))))
      (foo 0)
      next
      )))
                                     

;; (define (compactor xxs)
;;   (foreach xxs (lambda (xs)
;;                  (foreach xs (lambda (x)
;;                                (insert! x)))))
;;   ;; retrieve all entries 



;; are we concerned about duplicate states ?
(define (find-next-states clock limit states blue)
  (format #t "~% * TICK : Clock State ~a * ~%" clock)
  (cond 
   ((<= clock limit)
    ;; when clock <= 24 we keep iterating
    (let* ((phase1 (map (lambda (s) (find-next-states-1 s blue)) states)) 
           ;; we need to glue them as find-next-states-1..4 generates a list per s (state) ie a list of lists
           ;; we need a flat list of potential states
           (phase2 (map (lambda (s) (find-next-states-2 s blue)) (apply append phase1))) 
           (phase3 (map (lambda (s) (find-next-states-3 s blue)) (apply append phase2)))
           (phase4 (map (lambda (s) (find-next-states-4 s blue)) (apply append phase3)))
           (premature (apply append phase4)))
      ;; phase4 represents a set of possible pre-built states before the minute has ticked
      
      ;; all states in phase4
      ;; deduct build of robots from the minerals -- verify all mineral values are >= 0 and not negative
      (let ((next-states (map (lambda (s) (tick-tock s blue)) premature)))
        ;; we have n states -- potentially not unique as we do not iterate through phases to check no duplicates
        ;; whether we should?
        (format #t "length of next-states = ~a~%" (length next-states))
        ;; advance clock
        (find-next-states (+ clock 1) limit next-states blue))))
   (#t 
    ;; when clock > limit we have completed X cycles 
    ;; determine the maximum number of geode minerals we have mined
    ;; report maximum number of geodes found
    (format #t "max-number of geodes found = ~a~%" (max-number-of-geodes states))
    #t)))


(define (max-number-of-geodes xs)
  (let ((biggest 0 ))
    (letrec ((foo (lambda (ys)
                    (cond 
                     ((null? ys) biggest)
                     (#t (let ((y (car ys)))
                           (when (> y biggest) (set! biggest y))
                           (foo (cdr ys))))))))
      (foo xs)
      biggest)))


;; o = ore : c = clay : p = obsidian : g = geode == costs of building those robots (1)ore (2)clay (3)obsidian
;; 
;; m = minerals : r = robots actively working : b = building robots which come online
;; %m1 number of ore minerals after the tick 
;; sum :  number minerals now (m1) + (r1) number robots mine ore (1)
;;    subtract  : cost ore robot in ore (o1) * (b1) number ore robots  you build 
;;    subtract  : cost ore robot in clay (o2) * (b1) number ore robots you build
;;    subtract  : cost ore robot in obsidian (o3) * (b1) number ore robots you build
;; ** subtracts already accounted for in find-next-states1..4 **
(define (tick-tock state blue)
  (let ((o1 (blueprint-o1 blue)) (o2 (blueprint-o2 blue)) (o3 (blueprint-o3 blue))
        (c1 (blueprint-c1 blue)) (c2 (blueprint-c2 blue)) (c3 (blueprint-c3 blue))
        (p1 (blueprint-p1 blue)) (p2 (blueprint-p2 blue)) (p3 (blueprint-p3 blue))
        (g1 (blueprint-g1 blue)) (g2 (blueprint-g2 blue)) (g3 (blueprint-g3 blue))
        (m1 (state-m1 state)) (m2 (state-m2 state)) (m3 (state-m3 state)) (m4 (state-m4 state))
        (r1 (state-r1 state)) (r2 (state-r2 state)) (r3 (state-r3 state)) (r4 (state-r4 state))
        (b1 (state-b1 state)) (b2 (state-b2 state)) (b3 (state-b3 state)) (b4 (state-b4 state)))
    
    ;; state = m1 m2 m3 m4 r1 r2 r3 r4 b1 b2 b3 b4
    (let ((em1 (+ m1 r1))
          (em2 (+ m2 r2))
          (em3 (+ m3 r3))
          (em4 (+ m4 r4)) ;; number geode minerals is before plus number of geode robots
          (er1 (+ r1 b1)) ;; number robots is current robots plus number we build of this type
          (er2 (+ r2 b2))
          (er3 (+ r3 b3))
          (er4 (+ r4 b3)))
      ;; default built robots to zero as not used
      (let ((new-state (make-state em1 em2 em3 em4
                                   er1 er2 er3 er4
                                   0   0   0   0)))
        (when (or (< em1 0)(< em2 0)(< em3 0)(< em4 0))
          (error "bad state - negative mineral amounts??"))
        new-state))))
  

;; entry point to explore a certain blueprint
(define (explore-blueprint blue)
  (let ((clock 0)
        (clock-limit 24)
        (states (list state1)))
    (find-next-states clock clock-limit states blue)))

(define (example1) (explore-blueprint blue1))
(define (example2) (explore-blueprint blue2))



;; next states derivable from this initial state
;; do not build any robots and 

;; pipeline 
;; 
 
;; ;; 2. Create a new rectangle
;; (define rect1 (make-rectangle 10 20))

;; ;; 3. Check the type
;; (rectangle? rect1) ; => #t

;; ;; 4. Get the properties
;; (rectangle-width rect1)  ; => 10
;; (rectangle-height rect1) ; => 20

;; ;; 5. Set the properties (mutators)
;; (rectangle-width-set! rect1 15)
;; (rectangle-width rect1) ; => 15

;; 6. Copy the rectangle (defining a simple copy function)
;; (define (copy-state s)
;;   (make-state (rectangle-width r) 
;;               (rectangle-height r)))

;; (define rect2 (copy-rectangle rect1))
;; (rectangle-width rect2)  ; => 15
;; (rectangle-height rect2) ; => 20

;; try compiling this maximum balls to the wall , but gets stuck at clock state 13 ie 
;; 13 iterations and too many cases to consider
;;(example1)
;; > csc -O5 -o fun1 fun.scm
;; ----- clearly ineffective way to search brute force ------------- ??













