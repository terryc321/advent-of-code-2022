


(declaim (optimize (speed 3)(space 0)(safety 0)(debug 0)(compilation-speed 0)))

(ql:quickload :osicat)


;; (describe (osicat-posix:stat #P"/tmp/file"))

;; (describe (osicat-posix:stat #P"/home/terry/advent-of-code/2022/day11/fix/dat/50-0.dat"))

;; (osicat-posix:stat-size (osicat-posix:stat #P"/home/terry/advent-of-code/2022/day11/fix/dat/50-0.dat"))


#|
state machine for monkey puzzle
advent of code 2022 day 11 - done on 4th sept 2023 @ t.cadd
breakthrough - repeating code !!

number 71
0 1 2 3 4 5 6 7 8 9 10

  0 7 3 6 4 5 7 3 6 4 ........then enters a repeating cycle of ......
5 0 7 3 6 4 5 7 3 2 4
5 0 7 3 6 4 5 7 3 2 4
5 0 7 3 6 4 5 7 3 2 4
5 0 7 3 6 4 5 7 3 2 4
5 0 7 3 6 4 5 7 3 2 4
5 0 7 3 6 4 5 7 3 2 4
5 0 7 3 6 4 5 7 3 2 4

weird ??



|#

(defun predict-71 (tick)
  (let ((sol #(5 0 7 3 6 4 5 7 3 6 4))
	(i 1)
	(line 1))
    (loop for n from 1 to tick do
      (format t "~a " (aref sol i))
      (when (= i 10) (format t "~%"))
      (incf i)
      (when (> i 10) (setq i 0)))))

;; try start program
;; may have got half way and quit
;; got a target file size of 20909 where upon believe numbers get too large
;; for computer to compute effectively...

  
;; (run "71-0.dat" 71 0)
;; (run "56-0.dat" 56 0)
;; (run "50-0.dat" 50 0)
;; (run "73-0.dat" 73 0)
(defun run (num-start init-state)
  (let ((state init-state)
	(num num-start)
	(tick 0)
	(then (get-internal-run-time))
	(now (get-internal-run-time))
	(i 0))
    ;; (declare (type (fixnum) state))
    ;; (declare (type (fixnum) tick))
    (declare (type (integer) num))


    (let ((p (pathname (concatenate 'string
				    "/home/terry/advent-of-code/2022/day11/fix/dat/"
				    (format nil "~a-~a.dat" num state)))))
      (let ((sz (ignore-errors (osicat-posix:stat-size (osicat-posix:stat p)))))
	(format t "file found and has size ~a ~%" sz)
	(when (and sz (>= sz 20909))
	  (throw 'hold-the-phone-george-i-believe-it-has-already-been-done t)))

      (with-open-file (s p :direction :output :if-exists :supersede)
    (loop for tick from 1 to 10000 do
      ;;(format t "tick = ~a : num = ~a ~%" tick num)
      ;;(format t "tick = ~a : state = ~a ~%" tick state)
      (format s "~a " state)
      
      (incf i)
      (when (>= i 11) ;; guess
	(format s "~%")
	(setq i 0))
      (force-output s)
      
      (let ((now (get-internal-run-time)))
	(when (> (- now then) 200000)
	  (setq then now)
	  (format t "~a : checking file size ...~%" tick)
	  ;; check if file is over-size every 10 seconds or so ?? 20 k
	  ;; get-internal-run-time
	  (let ((sz (ignore-errors (osicat-posix:stat-size (osicat-posix:stat p)))))
	    (when (and sz (>= sz 20909))
	      (throw 'get-up-and-run-out-the-door-and-do-not-look-back-gone-skies t)))
	  ))

      (cond
	((= state 0) (setq num (* num 11)) (if (zerop (mod num 13))
					       (setq state 1)
					       (setq state 7)))
	((= state 1) (setq num (+ num 1)) (if (zerop (mod num 7))
					       (setq state 3)
					       (setq state 6)))
	((= state 2) (setq num (* num num)) (if (zerop (mod num 3))
					       (setq state 5)
					       (setq state 4)))
	((= state 3) (setq num (+ num 2)) (if (zerop (mod num 19))
					       (setq state 2)
					       (setq state 6)))
	((= state 4) (setq num (+ num 6)) (if (zerop (mod num 5))
					       (setq state 0)
					       (setq state 5)))
	((= state 5) (setq num (+ num 7)) (if (zerop (mod num 2))
					       (setq state 7)
					       (setq state 0)))
	((= state 6) (setq num (* num 7)) (if (zerop (mod num 11))
					       (setq state 2)
					       (setq state 4)))
	(t  (setq num (+ num 8)) (if (zerop (mod num 17))
					       (setq state 1)
					       (setq state 3)))))))))

















    
