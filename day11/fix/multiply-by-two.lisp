

(defun run ()
  (let ((n 1)
	(tick 1))
    (loop while t do
      (format t "tick = ~a ~%" tick)
      (if (zerop (mod tick 10000))
	  (format t "tick =~a : num = ~a ~%" tick n))
      (incf tick)
      (setq n (*  n 2)))))




  
