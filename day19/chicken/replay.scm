









(define (replay xs)
  (let ((ore-robot 1)(ore 0)
	(clay-robot 0)(clay 0)
	(obsidian-robot 0)(obsidian 0)
	(geode-robot 0)(geode 0)
	(o-r 0)(c-r 0)(obs-r 0)(g-r 0)
	(step 0))
    (replay2 xs
	     step
	     o-r c-r obs-r g-r
	     ore-robot ore
	     clay-robot clay
	     obsidian-robot obsidian
	     geode-robot geode)))


;; check replay 
;; Each ore robot costs 4 ore.
;; Each clay robot costs 2 ore.
;; Each obsidian robot costs 3 ore and 14 clay.
;; Each geode robot costs 2 ore and 7 obsidian.
(define (replay2 xs
		 step
		 o-r c-r obs-r g-r
		 ore-robot ore
		 clay-robot clay
		 obsidian-robot obsidian
		 geode-robot geode)
  (format #t "step ~a : ~a ~a ~a ~a : ore(~a ~a) clay(~a ~a) obs(~a ~a) geo(~a ~a) ~%"
	  step
	  o-r c-r obs-r g-r
	  ore-robot ore
	  clay-robot clay
	  obsidian-robot obsidian
	  geode-robot geode
	  )
  (cond
   ((null? xs) (list geode))
   (#t (let ((op (car xs)))
	 (cond
	  ((eq? op 'next)
	   (replay2 (cdr xs)
		    (+ step 1)
		    0 0 0 0
		    (+ o-r ore-robot) (+ ore ore-robot)
		    (+ c-r clay-robot) (+ clay clay-robot)
		    (+ obs-r obsidian-robot) (+ obsidian obsidian-robot)
		    (+ g-r geode-robot) (+ geode geode-robot)))
	  ((eq? op 'buy-ore) ;; ore costs 4 ... do we have enough ore
	   (when (< ore 4) (error "cannot buy ore robot do not have enough!~%"))
	   (format #t "bought ore robot for 4 ore ~%")
	   (replay2 (cdr xs)
		    step
		    (+ o-r 1) c-r obs-r g-r
		    ore-robot (- ore 4)
		    clay-robot clay
		    obsidian-robot obsidian
		    geode-robot geode))
	  ((eq? op 'buy-clay) ;; clay costs 2 ore
	   (when (< ore 2) (error "cannot buy clay robot do not have enough!~%"))
	   (format #t "bought clay robot for 2 ore ~%")
	   (replay2 (cdr xs)
		    step
		    o-r (+ 1 c-r) obs-r g-r
		    ore-robot (- ore 2)
		    clay-robot clay
		    obsidian-robot obsidian
		    geode-robot geode))
	  ((eq? op 'buy-obsidian) ;; obsidian cost 3 ore and 14 clay
	   (when (or (< ore 3)(< clay 14)) (error "cannot buy obsidian robot do not have enough!~%"))
	   (format #t "bought obsidian robot for 3 ore and 14 clay ~%")
	   (replay2 (cdr xs)
		    step
		    o-r c-r (+ 1 obs-r) g-r
		    ore-robot (- ore 3)
		    clay-robot (- clay 14)
		    obsidian-robot obsidian
		    geode-robot geode))
	  ((eq? op 'buy-geode) ;; geode cost 2 ore and 7 obsidian
	   (when (or (< ore 2)(< obsidian 7 )) (error "cannot buy geode robot do not have enough!~%"))
	   (format #t "bought geode robot for 2 ore and 7 obsidian ~%")
	   (replay2 (cdr xs)
		    step
		    o-r c-r obs-r (+ 1 g-r)
		    ore-robot (- ore 2)
		    clay-robot clay
		    obsidian-robot (- obsidian 7)
		    geode-robot geode))
	  (#t
	   (error (format #f "unrecog operator ~a ~%" op))))))))


(define (check)
  (replay '(next next buy-clay next next buy-clay next next buy-clay next next buy-clay next next next buy-obsidian next next next buy-obsidian next next next buy-geode next next buy-obsidian next next buy-geode next next buy-geode next next)))


;; (blue1-run)

