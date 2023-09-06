


;; (defun check-71-0 ()
;;   (let ((control (control-71-0))
;; 	(predict (predictor-71-0))
;; 	(j 1))
;;     (format t "control = ~a ~%" control)
;;     (format t "predict = ~a ~%" predict)
;;     (format t "predict 1st value = ~a ~%" (funcall predict 'val))
    
;;     (loop for i from 1 to 10000 do
;;       (let ((c (funcall control 'val))
;; 	    (p (funcall predict 'val)))
;; 	;;(format t "~a : control [~a] -> predict [~a] ~%" i c p)
;; 	(cond
;; 	  ((/= c p)
;; 	      (format t ".....mismatch ~%")
;; 	   (error (format nil "mismatch index ~a for 71 - 0 ~%" i)))
;; 	  (t
;; 	   ;; either control value or predictor value as same c p
;; 	   (format t "~a " c)))
	
;; 	(setq j (+ j 1))
;; 	(when (> j 11)
;; 	  (setq j 1)
;; 	  (format t "~%"))
;; 	(funcall control 'next)
;; 	(funcall predict 'next)))))
;; (defparameter p (let ((i 0)) (lambda (op) (cond ((eq op 'a) i) (t (setq i(+ i 1))i)))))


;; given n and i
;; make a pathname
;; 
(defun control-data (n i)
  (let ((path (pathname (concatenate 'string
				  "/home/terry/advent-of-code/2022/day11/fix/dat/"
				  (format nil "~a-~a.dat" n i)))))
    (with-open-file (port path :direction :input)
      (let ((vals '())
	    (x 0))
	(handler-case (loop (setq x (read port))
			    (setq vals (cons x vals)))
	  (end-of-file (c)
	    ;;(format t "caught end of file condition.~%")
	    t
	    ))
	(coerce (reverse vals) 'vector)))))




;; give procedure to 
(defun record-data-predictions (pp count)
  "record preedictions to file"
  (let ((path (pathname (concatenate 'string
				  "/home/terry/advent-of-code/2022/day11/fix/pred/"
				  (format nil "~a-~a.dat"
					  (funcall pp 'worry)
					  (funcall pp 'loc))))))
    
    (format t "creating or writing to file ~a~%" path)

    (with-open-file (port path :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
      ;; reset generator
      (funcall pp 'reset)

      ;; dump values from generator
      ;; use eleven numbers per line 
      (let ((n 1))
	(loop for i from 1 to count do
	  (format port "~a " (funcall pp 'val))
	  (funcall pp 'next)
	  (incf n)
	  (when (> n 11)
	    (format port "~%")
	    (setf n 1))))

      ;; reset generator
      (funcall pp 'reset)

      ;; done
      t)))

      








#|
controller given worry value and start location - reads that data set
makes itself an iterator

(defparameter p (controller 71 0))
(funcall p 'val)
(funcall p 'next)

able to step through the data sequentially
eventually it will error out due to no data being present

give controller a size option to see how much data got obtained

|#
(defun controller (n i)    
  (let* ((i 0)	 
	 (pat (control-data n i))
	 (lim (length pat)))
    (lambda (op)
      (cond
	((eq op 'val) (aref pat i))
	((eq op 'size) lim)	
	((eq op 'next)
	 (setq i (+ i 1))
	 (aref pat i))
	(t (error 'controller 'op-unknown op))))))




(defun make-predictor (worry loc dat special-index all-dat all-len)
  (let ((i 0)
	(lim (length dat)))
    (lambda (op)
      (cond
	((eq op 'val) (aref dat i))
	((eq op 'reset) (setq i 0) (aref dat i))
	((eq op 'dat) dat)
	((eq op 'all-dat) all-dat)
	((eq op 'all-len) all-len)	
	((eq op 'worry) worry)
	((eq op 'loc) loc)	
	((eq op 'special) special-index)
	((eq op 'next)
	 (setq i (+ i 1))
	 (cond
	   ((>= i lim)
	    (setq i (- i special-index))
	    (aref dat i))
	   (t (aref dat i))))
	(t (error 'generated-predictor 'op-unknown op))))))





;; control takes data and iterates over it
;; called too many times it will blow up
(defun make-control (dat)
  (let ((i 0)
	(lim (length dat)))
    (lambda (op)
      (cond
	((eq op 'val) (aref dat i))
	((eq op 'reset) (setq i 0) (aref dat i))
	((eq op 'next)
	 (setq i (+ i 1)))
	(t (error 'generated-predictor 'op-unknown op))))))





;; search
(defun predictor-search (worry loc)
  (let* ((pat (control-data worry loc))
	 (lim-original (length pat))
	 (lim lim-original)
	 (eleven 11))	 
    
    ;; (format t "control data length.length pat = ~a ~%" (length pat))
    ;; (format t "control data length.lem        = ~a ~%" lim)

    ;; decided not to chop data off as it may be important ,
    ;; although i dont think make a difference ....
    ;; chop tail ends off pattern mod 11 length
    ;; (multiple-value-bind (a r) (truncate lim-original 11)
    ;;   (setq pat (subseq pat 0 (- lim-original r)))
    ;;   (setq lim (length pat)))
    
    ;; (format t "control data length.length pat = ~a ~%" (length pat))
    ;; (format t "control data length.lem        = ~a ~%" lim)

    (catch 'solution
    ;; from some subset of control data - make a predictor
     ;;(loop for ss from 33 to lim by 11 do
    (loop for ss from 1 to lim do
      (let* ((pre (subseq pat 0 ss))
	     (pre-lim (length pre)))
	
	;; (format t "trying with ~a ~%" pre)
	;; (format t "trying with length ~a ~%" pre-lim)

	;;(loop for si from 1 to (- (floor pre-lim 11) 1) do
	(loop for si from 1 to (- pre-lim 1) do
	  ;;(let ((pp (make-predictor worry loc pre (* si 11) pat lim))
	  (let ((pp (make-predictor worry loc pre si pat lim))
		(cp (make-control pat))
		(errs 0))

	    ;; (format t "using segment length (~a) ~%" ss)
	    ;; (format t "special index (~a)~%" si)
	    
	    (catch 'trash-it
	    (loop for i from 1 to lim do
	      (let ((c (funcall cp 'val))
		    (p (funcall pp 'val)))
		
		;;(format t "control = ~a : " c)
		;;(format t "predict = ~a : " p)
		(when (/= c p)
		  (incf errs)
		  ;;(format t "... mismatch at index ~a ~%" i)
		  (throw 'trash-it t)
		  )
		
		(funcall pp 'next)
		(funcall cp 'next)
		))
	    );; trash-it loop
	    ;; check over
	    ;; (format t "errs total (~a)~%" errs)

	    (funcall pp 'reset)
	    
	    (when (zerop errs)
	      (format t "solution ~a-~a : tot@ ~a pre@ ~a : ss ~a si ~a (back si*11 = ~a) : proc ~a ~%"
		      worry loc lim pre-lim ss si (* si 11) pp)

	      ;; ------- check ----------
	      (funcall pp 'reset)
	      (record-data-predictions pp lim-original)

	      (funcall pp 'reset)
	      
	      (throw 'solution pp)
	      )
	    
	    )))))))










;; for all worry and locations
;; can we
(defun brute ()
  (let ((sols '())
	(sol 0))
    (mapcar (lambda (ni) (destructuring-bind (worry loc) ni
			   (format t "~a ~a ~%" worry loc)
			   (setq sol (predictor-search worry loc))
			   (setq sols (cons sol sols))
			   sol))
	    '((71 0) (56 0) (50 0) (73 0)
	      (70 1) (89 1) (82 1)
	      (52 2) (95 2)
	      (94 3) (64 3) (69 3) (87 3) (70 3)
	      (98 4) (72 4) (98 4) (53 4) (97 4) (51 4)
	      (79 5)
	      (77 6) (55 6) (63 6) (93 6) (66 6) (90 6) (88 6) (71 6)
	      (54 7) (97 7) (87 7) (70 7) (59 7) (82 7) (59 7)))))





(defun game2 (n-rounds)
  (let ((procs (brute))
	(tot-vec (make-array 8)))
    ;; reset procs if by any stretch brute used a few test values
    (dolist (p procs)
      (funcall p 'reset))
    ;; loop for each round ... for each monkey
    ;; .. apply any matching worry number that monkey has
    ;; ...throw it to another monkey
    ;; ...add interaction monkey number by 1 incf
    (loop for n from 1 to n-rounds do
      ;; the nth-round
      (loop for monkey from 0 to 7 do
	(dolist (p procs)
	  (when (= monkey (funcall p 'val))
	    (incf (aref tot-vec monkey))
	    (funcall p 'next)
	    

	    )))
      (format t "round ~a : ~a ~%" n tot-vec))))




      
    ;;   (format t "~%~%round ~a ~%" n)
    ;;   (let* ((rs (mapcar (lambda (x) (list
    ;; 				      (second x)
    ;; 				      (nth (- n 1) (fourth x))))
    ;; 			 routes)))
    ;; 	(loop for m from 0 to 7 do
    ;; 	  (let* ((hold (mapcar #'first (remove-if-not (lambda (x)(= (second x)m)) rs)))
    ;; 		 (hlen (length hold)))
    ;; 	    (format t "~a : ~a : ~a ~%" m hold hlen)
    ;; 	    (incf (aref tot-vec m) hlen)))
	
    ;; 	(format t "~%AFTER round ~a : ~a ~%" n tot-vec)
      		    
    ;; 	(format t "~%")	  
    ;; 	(format t "~a~%" rs)
    ;; 	rs))	  
    ;; tot-vec))




(defun expose (n-rounds)
  (let ((sols (brute)))
    (mapcar (lambda (p)
	      (format t "~%")
	      (format t "worry ~a ~%" (funcall p 'worry))
	      (format t "location ~a ~%" (funcall p 'loc))
	      (format t "dat ~a ~%" (funcall p 'dat))
	      (format t "special ~a ~%" (funcall p 'special))
	      (format t "dat ratio ~a / " (length (funcall p 'dat)))
	      (format t " ~a ~%" (length (funcall p 'all-dat)))

	      (funcall p 'reset)
	      (let ((path '())
		    (loc 0))
		(loop for i from 1 to n-rounds do
		  (setq loc (funcall p 'val))
		  (format t "~a " loc)
		  (setq path (cons loc path))
		  (funcall p 'next))
		(setq path (reverse path))
		
		(format t "~%~%")
		(format t "~%")
		(list 'loc (funcall p 'worry) 'path path)))
	    sols)))

 ;; default 20 rounds
(defun game (n-rounds)
  (let ((routes (expose n-rounds))
	(tot-vec (make-array 8)))
    (loop for n from 1 to n-rounds do
      ;; the nth-round
      (format t "~%~%round ~a ~%" n)
      (let* ((rs (mapcar (lambda (x) (list
				      (second x)
				      (nth (- n 1) (fourth x))))
			 routes)))
	(loop for m from 0 to 7 do
	  (let* ((hold (mapcar #'first (remove-if-not (lambda (x)(= (second x)m)) rs)))
		 (hlen (length hold)))
	    (format t "~a : ~a : ~a ~%" m hold hlen)
	    (incf (aref tot-vec m) hlen)))
	
	(format t "~%AFTER round ~a : ~a ~%" n tot-vec)
      		    
	(format t "~%")	  
	(format t "~a~%" rs)
	rs))	  
    tot-vec))




#|


round 10000 
0 : (71 70 69 70 66) : 5 
1 : NIL : 0 
2 : (82) : 1 
3 : (56 87 77 88 54 70) : 6 
4 : (89 72 53 79 55 93) : 6 
5 : (73 82 64 98 98 90) : 6 
6 : (50 52 95 59 59) : 5 
7 : (94 97 51 63 71 97 87) : 7 

AFTER round 10000 : #(37350 12778 4428 55552 68323 61992 63913 55664) 

((71 0) (56 3) (50 6) (73 5) (70 0) (89 4) (82 5) (52 6) (95 6) (94 7) (64 5)
 (69 0) (87 3) (70 0) (98 5) (72 4) (98 5) (53 4) (97 7) (51 7) (79 4) (77 3)
 (55 4) (63 7) (93 4) (66 0) (90 5) (88 3) (71 7) (54 3) (97 7) (87 7) (70 3)
 (59 6) (82 2) (59 6))
#(37350 12778 4428 55552 68323 61992 63913 55664)
CL-USER> (max 37350 12778 4428 55552 68323 61992 63913 55664)
68323
CL-USER> (max 37350 12778 4428 55552 61992 63913 55664)
63913
CL-USER> (* 68323 63913)
4366727899
CL-USER> 
4366727899 .... too low?


maybe an off by 1 error ??????

#(37355 12779 4429 55559 68329 61997 63918 55670)
CL-USER> (max 37355 12779 4429 55559 68329 61997 63918 55670) 
68329
CL-USER> (max 37355 12779 4429 55559 61997 63918 55670)
63918
CL-USER> (* 68329 63918)
4367453022
CL-USER>


|#


#|
round 10000 : #(185312 113 21328 274669 317346 1 8 7) 
NIL

CL-USER> (max 185312 113 21328 274669 317346 1 8 7)
317346
CL-USER> (max 185312 113 21328 274669 1 8 7)
274669
CL-USER> (* ** *)
87165108474
CL-USER> (* 317346 274669)
87165108474
CL-USER> 

|#
















      



    


  




