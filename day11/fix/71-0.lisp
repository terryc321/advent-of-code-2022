
;; actual data
(defun control-71-0-data ()
  #(
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 2 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    0 7 3 6 4 5 7 3 6 4 5 
    ))


;; control group
(defun control-71-0 ()    
  (let* ((i 0)	 
	 (pat (control-71-0-data))
	 (lim (length pat)))
    (lambda (op)
      (cond
	((eq op 'val) (aref pat i))
	((eq op 'next)
	 (setq i (+ i 1))
	 (if (> i lim)
	     (setq i 41)))	 
	(t (error 'control-71-0 'op-unknown op))))))


;; sequential generator
(defun predictor-71-0 ()
  (let* ((pat #(0 7 3 6 4 5 7 3 6 4 5 
		0 7 3 6 4 5 7 3 2 4 5 
		0 7 3 6 4 5 7 3 6 4 5 
		0 7 3 6 4 5 7 3 6 4 5
		;; << index 44 begins here
		0 7 3 6 4 5 7 3 2 4 5 
		0 7 3 6 4 5 7 3 6 4 5 
		0 7 3 6 4 5 7 3 6 4 5))
	 (lim (length pat))
	 (special-index 44)
	 (i 0))
    ;; repeat on 4th row
    ;; index i from 0 to k
    ;; n from 1 to ...
    ;; off by one n = 1 then i = 0
    ;;        flip back to i = 41 the '0 on line 5    
    (lambda (op)
      (cond
	((eq op 'val) (aref pat i))
	((eq op 'next)
	 (setq i (+ i 1))
	 (if (>= i lim)	     
	     (setq i special-index)))	 
	(t (error 'predictor-71-0 'op-unknown op))))))



(defun check-71-0 ()
  (let ((control (control-71-0))
	(predict (predictor-71-0))
	(j 1))
    (format t "control = ~a ~%" control)
    (format t "predict = ~a ~%" predict)
    (format t "predict 1st value = ~a ~%" (funcall predict 'val))
    
    (loop for i from 1 to 10000 do
      (let ((c (funcall control 'val))
	    (p (funcall predict 'val)))
	;;(format t "~a : control [~a] -> predict [~a] ~%" i c p)
	(cond
	  ((/= c p)
	      (format t ".....mismatch ~%")
	   (error (format nil "mismatch index ~a for 71 - 0 ~%" i)))
	  (t
	   ;; either control value or predictor value as same c p
	   (format t "~a " c)))
	
	(setq j (+ j 1))
	(when (> j 11)
	  (setq j 1)
	  (format t "~%"))
	(funcall control 'next)
	(funcall predict 'next)))))





(defparameter p (let ((i 0)) (lambda (op) (cond ((eq op 'a) i) (t (setq i(+ i 1))i)))))


    


  




