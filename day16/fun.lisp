
(defpackage :aoc22
  (:use :cl))

(in-package :aoc22)

(defun example ()
  '(  
    (AA 0 DD II BB)
    (BB 13 CC AA)
    (CC 2 DD BB)
    (DD 20 CC AA EE)
    (EE 3 FF DD)
    (FF 0 EE GG)
    (GG 0 FF HH)
    (HH 22 GG)
    (II 0 AA JJ)
    (JJ 21 II)
    ))




(defun can-go(from)
  (cdr (cdr (assoc from (example)))))



(defun who-pays()  
  (let ((rs (remove-if (lambda (x) (= 0 (cadr x)))
		       (sort (mapcar (lambda (x) (list (car x) (cadr x))) (example))
			     (lambda (x y) (> (cadr x) (cadr y)))))))
    (values (mapcar #'car rs) (mapcar #'cadr rs))))


(defun flow-rate3(s)
  (cadr (assoc s (example))))


;; (defun shortest-path (from to)
;;   (assert (member from '(AA BB CC DD EE FF GG HH II JJ)))
;;   (assert (member to '(AA BB CC DD EE FF GG HH II JJ)))
;;   (assert (not (eq from to)))  
;;   ...
;;   the shortest path is ...
;;   ...)


;;
;; (defun trec2(from to seen dist)
;;   (cond
;;     ((eq from to)
;;      (format t "here ... distance ~a ... ~a~%" dist seen)
;;      dist)
;;     (t (let ((reach (can-go from)))
;; 	 (dolist (r reach)
;; 	   (cond
;; 	     ((member r seen)
;; 	      (format t "seen ~a already skipping ~%" r))
;; 	     (t (trec2 r to (cons r seen) (+ dist 1)))))))))



(defun shortest-path(from to)
  (assert (member from '(AA BB CC DD EE FF GG HH II JJ)))
  (assert (member to '(AA BB CC DD EE FF GG HH II JJ)))
  (let ((best 99999)
	(best-path '()))
    (labels ((trec2 (from to seen dist)
	       (cond
		 ((eq from to)
		  ;;(format t "here ... distance ~a ... ~a~%" dist seen)
		  (when (< dist best)
		    (setq best dist)
		    (setq best-path seen))
		  dist)
		 (t (let ((reach (can-go from)))
		      (dolist (r reach)
			(cond
			  ((member r seen)
			   ;;(format t "seen ~a already skipping ~%" r)
			   )
			  (t (trec2 r to (cons r seen) (+ dist 1))))))))))
      (let ((dist 0)
	    (seen (list from)))	
	(trec2 from to seen dist)
	;;(format t "best ~a ... ~a ...~%" best best-path)
	(values best best-path)))))


#|
locations

at location loc
time is 0 .. 30

walk take n units of time
open valve take 1 unit of time
flow = 

|#
(defun greedy ()
  (let ((sols '()))
  (labels ((trec (loc time seen locs flow)
	     (cond
	       ((= 6 (length seen))
		(format t "are we done ? ... flow ~a ~%" flow)
		(setq sols (cons (list 'flow flow) sols))
		)
	       (t (dolist (loc2 locs)
		    (cond
		      ((member loc2 seen)
		       (format t "have we turned off [~a] this valve already ? ~%" loc2)
		       )
		      (t
		       (let ((walk (shortest-path loc loc2)))
			 (let ((time2 (- time walk)))
			   (when (> time2 0)
			     (let ((extra-flow (* (flow-rate3 loc2) (- time2 1))))
			       (trec loc2 (- time2 1) (cons loc2 seen) locs (+ flow extra-flow)))))))))))))
    (let ((start 'aa)
	  (seen '())
	  (flow 0)
	  (time 30))
      (trec start time seen (who-pays) flow)))
    (sort sols (lambda (x y) (> (cadr x) (cadr y))))))



	  


(defun check ()
  (let ((locs (let ((xs (example)))
		(mapcar (lambda (x) (car x) ) xs))))
    (dolist (loc locs)
      (dolist (loc2 locs)
	(when (not (eq loc loc2))
	  (multiple-value-bind (b p) (shortest-path loc loc2)
	    (format t "~a -> ~a : val ~a : path ~a ~%" loc loc2 b p)))))))



(defun dot ()
  (format t "graph {~%")
  (dolist (e (example))
    (let ((options (cdr (cdr e)))
	  (loc (car e)))
      (dolist (op options)
	(format t "~a -- ~a ~%" loc op))))
  (format t "}~%"))




#|

maximum theortical
ignore paths
start aa
hh 22 1 min open               22 * 28
1 min tunnel jj 1 min open     21 * 26
1 min tunnel 1 min open dd 20  20 * 24 

given at flow rate X and best so far is Y
by what measure should continue ?


do spend time open valve ?
or keep going until reach best ?
what cut off point ?

1 min open valve
1 min travel to next destination
sstart at aa
search is a protected word?
loc = location
open = valves open + when opened
closed = valves still to open
path taken


|#


(defun flow-rate(loc data)
  (car (cdr (assoc loc data))))

(defun all-open(xs)
  (catch 'nope
    (dolist (loc '(BB CC DD EE HH JJ))
      (when (not (member loc xs))
	(throw 'nope nil)))
    t))


(defparameter *max-flow* 0)
(defparameter *max-path* '())



(defun rec (tick loc flow path data open nopen)
  (let ((options '()))
    (cond
      ((< tick 1)
       (when (> flow *max-flow*)
	 (setq *max-flow* flow)
	 (format t "new max flow at timeout ~a ~%" *max-flow*)
	 )
       ;;(format t "done ~a ~%"  path)
       'done)
      ;; ((all-open open)
      ;;  (format t "all open ~a ~%" path)
      ;;  'done)
      ((> nopen 5)
       ;;(format t "all open ~a ~%~a~%~a~%~%" path open flow)
       (when (> flow *max-flow*)
	 (setq *max-flow* flow)
	 (format t "new max flow at run ~a : ~a~%" *max-flow* tick)
	 (setq *max-path* path)
	 )
       
        'done)      
      (t
       (setq options (cdr (cdr (assoc loc data))))
       ;;(format t "options =~a ~%" options)
       (when (not (member loc open))
	 (dolist (op options)
	   (when (> (- tick 2) 0)
	   (rec (- tick 2) op (+ (* tick (flow-rate loc data)) flow) (cons loc path) data (cons loc open) (+ nopen 1)))))
       (progn  ;; skipped
	 (dolist (op options)
	   (rec (- tick 1) op flow (cons loc path) data open nopen)))))))



(defun seek ()
  (setq *max-flow* 0)
  (setq *max-path* '())
  
  (let ((tick 30)
	(loc 'AA)
	(flow 0)
	(data (example)))
    (rec tick loc flow '() data '() 0)
    ))





