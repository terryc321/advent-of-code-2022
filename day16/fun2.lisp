
#|

hit recursive limit when ran (greedy)

see if scheme can do this 

|#


(defpackage :aoc22
  (:use :cl))

(in-package :aoc22)

(declaim (optimize (speed 3)(space 0)(safety 0)(debug 0)(compilation-speed 0)))


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


(defun problem ()
  '(
(AW 0  LG  TL)
(OM 0  XK  IM)
(BG 0  MP  SB)
(XB 0  MA  TL)
(CD 0  VL  OF)
(VF 0  CS  XK)
(HK 0  RL  QB)
(QN 0  IV  QR)
(OF 4  TQ  CD  IR  IM  JE)
(QB 14  HK  XE  CS  VO)
(ZE 7  JB  NC  SE  OI)
(OW 0  MB  JB)
(MA 0  XB  MB)
(MP 0  VK  BG)
(UE 9  ZM  RZ  WI  HO  FO)
(QR 24 QN)
(TQ 0  OF  AA)
(SE 0  ZE  ZZ)
(AQ 20 CX)
(XE 0  JQ  QB)
(DC 8  ZD  MJ  RZ)
(ZM 0  YJ  UE)
(VK 21 MP)
(VR 0  WV  PS)
(BH 0  AA  MB)
(ZR 0  LG  AI)
(JE 0  OF  HO)
(IR 0  IV  OF)
(FO 0  XQ  UE)
(AA 0  NC  VY  BH  TQ  YJ)
(ZZ 0  SE  TL)
(XQ 0  IV  FO)
(WI 0  UE  VO)
(VY 0  AA  LG)
(XK 15  VF  OM  ZD)
(CX 0  AQ  MB)
(JQ 0  XE  IV)
(LG 3  VY  PS  ZR  AW  OI)
(JB 0  ZE  OW)
(OI 0  ZE  LG)
(YJ 0  ZM  AA)
(NC 0  AA  ZE)
(KR 0  SB  MJ)
(MB 17  CX  BH  AI  OW  MA)
(AI 0  ZR  MB)
(TL 16  ZZ  XB  AW)
(RL 0  WV  HK)
(CS 0  VF  QB)
(WV 25  RL  VL  VR)
(ZD 0  XK  DC)
(IV 23  XQ  IR  JQ  QN)
(PS 0  VR  LG)
(RZ 0  DC  UE)
(VO 0  WI  QB)
(MJ 0  DC  KR)
(IM 0  OM  OF)
(VL 0  CD  WV)
(SB 18  BG  KR)
(HO 0  JE  UE)
    ))


(defparameter *problem-sites*  (mapcar #'car (problem)))


(defun old-can-go(from)
  (let ((input (problem)))
    (let ((res (cdr (cdr (assoc from input)))))
      res)))

(let ((my-hash (make-hash-table :size 60)))
  (defun can-go(from)
    (let ((val (gethash from my-hash)))
      (cond
	(val val)
	(t (let ((input (problem)))
	     (let ((res (cdr (cdr (assoc from input)))))
	       (setf (gethash from my-hash) res)
	       res)))))))

;; prime can-go
(dolist (a *problem-sites*)
  (can-go a))





(defun who-pays()
  (let ((input (problem)))
    (let ((rs (remove-if (lambda (x) (= 0 (cadr x)))
			 (sort (mapcar (lambda (x) (list (car x) (cadr x))) input)
			       (lambda (x y) (> (cadr x) (cadr y)))))))
      (values (mapcar #'car rs) (mapcar #'cadr rs)))))


(defun old-flow-rate(s)
  (let ((res (cadr (assoc s (problem)))))
    res))



(let ((my-hash (make-hash-table)))
  (defun flow-rate(s)
    (let ((val (gethash s my-hash)))
      (cond
	(val val)
	(t (let ((res (cadr (assoc s (problem)))))
	     (setf (gethash s my-hash) res)
	     res))))))


;; prime flow-rate
(dolist (a *problem-sites*)
  (flow-rate a))

(defun test-1 ()
  (time (dolist (a *problem-sites*)
	  (flow-rate a))))

(defun test-2 ()
  (time (dolist (a *problem-sites*)
	  (old-flow-rate a))))




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



;; does this need speeding up ??
(defun old-shortest-path(from to)
  (assert (member from *problem-sites*))
  (assert (member to *problem-sites*))
  (let ((best 999999999999999)
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





;; able to do this
(let ((my-hash (make-hash-table)))
  (defun shortest-path(a b)
    (let ((h2 (gethash a my-hash)))
      (cond
	(h2 (let ((val (gethash b h2)))
	      (cond
		(val val)
		(t (let ((res (shortest-path a b)))
		     (setf (gethash b h2) res)
		     res)))))
	(t (let ((res (shortest-path a b)))
	     (let ((new-h (make-hash-table)))
	       (setf (gethash b new-h) res)
	       (setf (gethash a my-hash) new-h))))))))

;; prime shortest paths
(dolist (a *problem-sites*)
  (dolist (b *problem-sites*)
    (fast-path a b)))

  

  




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

