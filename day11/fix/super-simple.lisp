
#|
corrected monkey algorithm

|#

(defun run (num-start init-state)
  (let ((state init-state)
	(num num-start)
	(i 0))
    (loop for tick from 1 to 10000 do
      ;;(format t "tick = ~a : num = ~a ~%" tick num)
      ;;(format t "tick = ~a : state = ~a ~%" tick state)
      (format t "~a " state)
      
      (incf i)
      (when (>= i 11) ;; guess
	;;(format s "~%")
	(format t "~%")	
	(setq i 0))
      ;;(force-output s)
      (finish-output t)
      
      (when (= state 0)
	(setq num (* num 11))
	(if (zerop (mod num 13))
	    (setq state 1)
	    (setq state 7)))

      (when (= state 1)
	(setq num (+ num 1))
	(if (zerop (mod num 7))
	    (setq state 3)
	    (setq state 6)))

      (when (= state 2)
	(setq num (* num num))
	(if (zerop (mod num 3))
	    (setq state 5) (setq state 4)))

      (when (= state 3)
	(setq num (+ num 2))
	(if (zerop (mod num 19))
	    (setq state 2) (setq state 6)))

      (when (= state 4)
	(setq num (+ num 6))
	(if (zerop (mod num 5))
	    (setq state 0)
	    (setq state 5)))

      (when (= state 5)
	(setq num (+ num 7))
	(if (zerop (mod num 2))
	    (setq state 7)
	    (setq state 0)))

      (when (= state 6)
	(setq num (* num 7))
	(if (zerop (mod num 11))
	    (setq state 2)
	    (setq state 4)))

      (when (= state 7)
	(setq num (+ num 8))
	(if (zerop (mod num 17))
            (setq state 1)
	    (setq state 3))))))



