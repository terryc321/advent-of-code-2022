
(defpackage :aoc22 (:use :cl))

(in-package :aoc22)  


;; rock paper scissors
;; A = rock
;; B = paper
;; C = scissors

;; X = rock
;; Y = paper
;; Z = scissors

;; shape 
;; score 
;; 1 rock
;; 2 paper
;; 3 scissors

;; outcome round
;; 0 lost
;; 3 draw
;; 6 win


;; empty string
;; A B C followed by X Y Z

(defun game-score (a b)
  (assert (member a '(rock paper scissors)))
  (assert (member b '(rock paper scissors)))  
  (cond
    ((eq a b) 3)
    ((and (eq b 'rock) (eq a 'scissors)) 6)
    ((and (eq b 'paper) (eq a 'rock)) 6)
    ((and (eq b 'scissors) (eq a 'paper)) 6)
    (t 0)))

(defun shape-score (a)
  (assert (member a '(rock paper scissors)))  
  (cond
    ((eq a 'rock) 1)
    ((eq a 'paper) 2)
    ((eq a 'scissors) 3)
    (t (error "shape not recognised"))))


(defun get-games ()
  (with-open-file (stream "input" :direction :input)
    (let ((got t)
	  (lines '())
	  (line 1))
    (loop until (not got) do
      (setq got (read-line stream nil nil))
      (when got
	(let ((alpha (char got 0))
	      (beta (char got 2))
	      (play1 nil)
	      (play2 nil))	      
	  (cond
	    ((char= alpha #\A) (setq play1 'rock))
	    ((char= alpha #\B) (setq play1 'paper))
	    ((char= alpha #\C) (setq play1 'scissors))
	    (t (error "char for player 1 move not recognised ")))

	  (cond
	    ((char= beta #\X) (setq play2 'rock))
	    ((char= beta #\Y) (setq play2 'paper))
	    ((char= beta #\Z) (setq play2 'scissors))
	    (t (error "char for player 2 move not recognised ")))

	  (let* ((gs (game-score play1 play2))
		 (ss (shape-score play2))
		 (tot (+ gs ss)))
	    	  
	  (setq lines (cons `(line ,line ,got ,play1 ,play2 (game-score ,gs) (shape-score ,ss) (total-score ,tot)) lines))
	  (format t "line ~a : got = \"~a\" ~%" line (car lines))
	  (incf line)))))
      
      (reverse lines))))



;; opponent vs you
;;
;; score based on win = 6 , draw = 3 , lose = 0
;; plus shape chosen
;;
;; A X   rock vs rock     : draw 3 + shape rock 1  = 4 tot
;; A Y   rock vs paper    : win 6 + shape 2        = 8 tot
;; A Z   rock vs scissors : lose 0 + shape 3       = 3 tot 
;;
;; B X   paper vs rock       lose 0 + shape 1     = 1 tot
;; B Y   paper vs paper      draw 3 + shape 2     = 5 tot
;; B Z   paper vs scissors : win 6 + shape scissors 3 = 9 tot

;; C X   scissors vs rock     : win 6  + shape 1 = 7 tot
;; C Y   scissors vs paper    : lose 0 + shape 2 = 2 tot
;; C Z   scissors vs scissors : draw 3 + shape 3 = 6 tot

(defun extract-scores ()
  (mapcar (lambda (x) (elt (elt x 7) 1)) (get-games)))

(defun tot-scores ()
  (apply #'+ (extract-scores)))




;; x need lose
;; y need draw
;; z need win
(defun need-lose(a)  
  (assert (member a '(rock paper scissors)))
  (cond
    ((eq a 'rock) 'scissors)
    ((eq a 'paper) 'rock)
    ((eq a 'scissors) 'paper)
    (t (error "shape not recognised"))))

(defun need-win(a)  
  (assert (member a '(rock paper scissors)))
  (cond
    ((eq a 'rock) 'paper)
    ((eq a 'paper) 'scissors)
    ((eq a 'scissors) 'rock)
    (t (error "shape not recognised"))))

(defun need-draw(a)  
  (assert (member a '(rock paper scissors)))
  (cond
    ((eq a 'rock) 'rock)
    ((eq a 'paper) 'paper)
    ((eq a 'scissors) 'scissors)
    (t (error "shape not recognised"))))
 
(defun decode-abc (a)
  (assert (eq 'standard-char (type-of a)))  
  (cond
    ((char= a #\A) 'rock)
    ((char= a #\B) 'paper)
    ((char= a #\C) 'scissors)
    (t (error "decode-abc character not recognised"))))

    

(defun get-games2 ()
  (with-open-file (stream "input" :direction :input)
    (let ((got t)
	  (lines '())
	  (line 1))
    (loop until (not got) do
      (setq got (read-line stream nil nil))
      (when got
	(let ((alpha (char got 0))
	      (beta (char got 2))
	      (play1 nil)
	      (play2 nil))	      
	  (cond
	    ((char= alpha #\A) (setq play1 'rock))
	    ((char= alpha #\B) (setq play1 'paper))
	    ((char= alpha #\C) (setq play1 'scissors))
	    (t (error "char for player 1 move not recognised ")))

	  (cond
	    ((char= beta #\X) (setq play2 (need-lose (decode-abc alpha))))
	    ((char= beta #\Y) (setq play2 (need-draw (decode-abc alpha))))
	    ((char= beta #\Z) (setq play2 (need-win (decode-abc alpha))))
	    (t (error "char for player 2 move not recognised ")))

	  (let* ((gs (game-score play1 play2))
		 (ss (shape-score play2))
		 (tot (+ gs ss)))
	    	  
	  (setq lines (cons `(line ,line ,got ,play1 ,play2 (game-score ,gs) (shape-score ,ss) (total-score ,tot)) lines))
	  (format t "line ~a : got = \"~a\" ~%" line (car lines))
	  (incf line)))))
      
      (reverse lines))))

  
(defun extract-scores2 ()
  (mapcar (lambda (x) (elt (elt x 7) 1)) (get-games2)))

(defun tot-scores2 ()
  (apply #'+ (extract-scores2)))





