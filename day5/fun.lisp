
(ql:quickload :cl-ppcre)
(ql:quickload :uiop)

;; resorted to using cl-ppcre regex package now for regex matching
;; (register-groups-bind (a b)
;;     ("([0-9]+)-([0-9]+)" "123-456")
;;   (list a b))

;; (uiop:chdir "/home/terry/advent-of-code/2022/day5/")
;; 0
;; AOC22> (uiop:getcwd)
;; none of this works
;; (setf *default-pathname-defaults* (truename "./subdir"))
;; (setf *default-pathname-defaults* (truename "/absolute/path"))
;;
;; works in sbcl , setting pathname defaults , load file works and finds file
;; *default-pathname-defaults*
;;

(defpackage :aoc22
  (:use :cl))

(in-package :aoc22)  


;; [Q] [J]                         [H]
;; [G] [S] [Q]     [Z]             [P]
;; [P] [F] [M]     [F]     [F]     [S]
;; [R] [R] [P] [F] [V]     [D]     [L]
;; [L] [W] [W] [D] [W] [S] [V]     [G]
;; [C] [H] [H] [T] [D] [L] [M] [B] [B]
;; [T] [Q] [B] [S] [L] [C] [B] [J] [N]
;; [F] [N] [F] [V] [Q] [Z] [Z] [T] [Q]
;;  1   2   3   4   5   6   7   8   9 

(defun initial-state ()
  (list
   (list)
   (list 'q 'g 'p 'r 'l 'c 't 'f)
   (list 'j 's 'f 'r 'w 'h 'q 'n)
   (list 'q 'm 'p 'w 'h 'b 'f)
   (list 'f 'd 't 's 'v)
   (list 'z 'f 'v 'w 'd 'l 'q)
   (list 's 'l 'c 'z)
   (list 'f 'd 'v 'm 'b 'z)
   (list 'b 'j 't)
   (list 'h 'p 's 'l 'g 'b 'n 'q)
   (list)
   ))


(defun micro-step (state command)
  (destructuring-bind (a b c) command
    (assert (integerp a))
    (assert (integerp b))
    (assert (integerp c))
    (assert (>= a 0))
    (assert (>= b 1))
    (assert (<= b 9))
    (assert (>= c 1))
    (assert (<= c 9))
    (assert (not (null (nth b state))))
    (cond
      ((< a 1) state)
      (t (let ((mov (car (nth b state))))
	   (setf (nth b state) (cdr (nth b state)))
	   (setf (nth c state) (cons mov (nth c state)))
	   state)))))

;; not tail-call itself yet
;;(micro-step state (- a 1) b c)))))

 
  


;; read lines of file
(defun get-lines ()
  (with-open-file (stream "commands" :direction :input)
    (let ((got t)
	  (lines '())
	  (line 1))
    (loop until (not got) do
      (setq got (read-line stream nil nil))
      (when got
	  (setq lines (cons `(line ,line ,got)
			    lines))
	  (format t "line ~a : got = \"~a\" ~%" line (car lines))
	  (incf line)))
      ;;outside loop
      (reverse lines))))

;; ppcre regex matching groups extraction example
(defun extract-ranges (str)
  (cl-ppcre:register-groups-bind (sa sb sc)
      ("move ([0-9]+) from ([0-9]+) to ([0-9]+)" str)
    (let ((a (parse-integer sa))
	  (b (parse-integer sb))
	  (c (parse-integer sc)))
      ;; ensure not missing anything
      (assert (string= str (format nil "move ~a from ~a to ~a" a b c)))
      (list a b c))))


(defun get-commands()
  (let ((commands (mapcar (lambda (xs)
			    (let ((str (nth 2 xs)))
			      (format t "attempt mathch on [~a] :~%" str)
			      (let ((found (extract-ranges str)))
				(format t "success ~a ~%" found)
				found)))
			  (get-lines))))
    commands))


;; find longest-stack
(defun viz(state)
  (let ((n 0))
    (dolist (s state)
      (format t "~a : ~a~%" n s)
      (incf n))))


(defun drop (n xs)
  (assert (integerp n))
  (cond
    ((< n 1) xs)
    (t
     (assert (not (null xs)))
     (drop (- n 1) (cdr xs)))))

;; (drop 0 '(a b c))
;; (drop 1 '(a b c))
;; (drop 2 '(a b c))
;; (drop 3 '(a b c))
;; (drop 4 '(a b c))

(defun take (n xs)
  (assert (integerp n))
  (cond
    ((< n 1) '())
    (t
     (assert (not (null xs)))
     (cons (car xs) (take (- n 1) (cdr xs))))))

;; (take 1 '(a b c))
;; (take 2 '(a b c))
;; (take 3 '(a b c))
;; (take 4 '(a b c))

;; part 2 -- big step --
(defun mega-step (state command)
  (destructuring-bind (a b c) command
    (assert (integerp a))
    (assert (integerp b))
    (assert (integerp c))
    (assert (>= a 0))
    (assert (>= b 1))
    (assert (<= b 9))
    (assert (>= c 1))
    (assert (<= c 9))
    (assert (not (null (nth b state))))
    (cond
      ((< a 1) state)
      (t (let ((mov (take a (nth b state))))
	   (setf (nth b state) (drop a (nth b state)))
	   (setf (nth c state) (append mov (nth c state)))
	   state)))))

    




(defun part1()
  (let ((commands (get-commands))
	(steps 0)
	(count 0)
	(state (initial-state)))
    (loop while commands do
      (let ((command (car commands)))
	(format t "~%~%processing command ~a ~%" command)
	(destructuring-bind (lim from to) command
	  (loop for i from 1 to lim do
	    (incf count)
	    (format t "before[~a]:~%state~%" count)
	    (viz state)
	    (format t "command~a~%" command)
	    (micro-step state command)
	    (format t "after[~a]:~%" count)
	    (viz state)
	    ))
	(incf steps (car command))
	(setq commands (cdr commands))))
    (format t "there are ~a state transitions ~%" steps)
    (format t "final state ~%")
    (viz state)
    (format t "~%")
    (let ((tops (mapcar #'car state)))
      (format t "tops of stacks ~a ~%" tops)
      tops)))



;; final state 
;; 0 : NIL
;; 1 : (V L)
;; 2 : (G C Q M Q B F D G)
;; 3 : (B)
;; 4 : (B)
;; 5 : (J F F S P S D V W W Z L)
;; 6 : (C W)
;; 7 : (R N J Q B H H)
;; 8 : (M P F Z T T P Z F L H V D)
;; 9 : (N R S Q T L Q F S)
;; 10 : NIL

;; tops after 2638 state transitions
;;(NIL V G B B J C R M N NIL)
;;
;; message VGBBJCRMN


;;(defun part2() t)

;; command ( 3 1 2 )
;; moves all 3 in one step from stack 1 to stack 2 
(defun part2()
  (let ((commands (get-commands))
	(steps 0)
	(count 0)
	(state (initial-state)))
    (loop while commands do
      (let ((command (car commands)))
	(format t "~%~%processing command ~a ~%" command)
	(destructuring-bind (lim from to) command
	  (format t "before[~a]:~%state~%" count)
	  (viz state)
	  (format t "command~a~%" command)
	  (mega-step state command)
	  (format t "after[~a]:~%" count)
	  (viz state))	  
	(incf steps (car command))
	(setq commands (cdr commands))))
    (format t "there are ~a state transitions ~%" steps)
    (format t "final state ~%")
    (viz state)
    (format t "~%")
    (let ((tops (mapcar #'car state)))
      (format t "tops of stacks ~a ~%" tops)
      tops)))





    




	      


