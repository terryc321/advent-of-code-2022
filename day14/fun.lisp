
#|

input first few numbers

498,32 -> 503,32
538,63 -> 538,65 -> 534,65 -> 534,70 -> 551,70 -> 551,65 -> 544,65 -> 544,63
525,56 -> 530,56


|#

(defpackage :aoc22
  (:use :cl))

(in-package :aoc22)

;; (defun digit-char-p(x)
;;   (member x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defparameter coords '())
(defparameter min-x 999999999999)
(defparameter min-y 99999999999)
(defparameter max-x -9999999999)
(defparameter max-y -99999999999)

(defparameter my-hash nil)

(defun reset ()
  (setq my-hash (make-hash-table :test #'equalp))
  (setq coords '())
  (setq min-x 999999999999)
  (setq min-y 99999999999)
  (setq max-x -9999999999)
  (setq max-y -99999999999)
  )




(defun add-coord(x y)
  (when (< x min-x)
    (setq min-x x))
  (when (< y min-y)
    (setq min-y y))
  (when (> x max-x)
    (setq max-x x))
  (when (> y max-y)
    (setq max-y y))
  (setf (gethash (list x y) my-hash) t)
  (setq coords (cons (list x y) coords)))




;; (x1 y1 x2 y2 ...
;; x increasing / x decreasing...
;; y increasing / y decreasing
(defun process-list(xs)
  (let ((p xs))
    (loop while (not (null (cdr (cdr p)))) do
      (let ((x1 (car p))
	    (y1 (car (cdr p)))
	    (x2 (car (cdr (cdr p))))
	    (y2 (car (cdr (cdr (cdr p))))))
	(format t "~a , ~a  = > ~a , ~a ~%" x1 y1 x2 y2)
	(cond
	  ((< x2 x1)
	   (loop for x from x2 to x1 do
	     (add-coord x y1)))
	  ((> x2 x1)
	   (loop for x from x1 to x2 do
	     (add-coord x y1)))	   
	  ((< y2 y1)
	   (loop for y from y2 to y1 do
	     (add-coord x1 y)))
	  ((> y2 y1)
	   (loop for y from y1 to y2 do
	     (add-coord x1 y)))
	  (t (error "unhandled case")))
	(setq p (cdr (cdr p)))
	))))


;; parsing / lexing numbers is a ball ache
;; fudge last integer as it always gets lost when loop finishes line
(defun process-line (str)
  (let ((len (length str))
	(i 0)
	(live nil)
	(x nil)
	(xs '())
	(last 0))
    (loop while (< i len) do
      (cond
	((and live (= i (- len 1)) (digit-char-p (char str i)))
	 (setq x (subseq str last (+ i 1)))
	 (setq i (+ i 1))
	 (setq xs (cons x xs)))
	((and (not live) (digit-char-p (char str i)))
	 (setq live t)
	 (setq last i)
	 (setq i (+ i 1)))
	((digit-char-p (char str i)) 
	 (setq i (+ i 1)))
	(live (setq x (subseq str last i))
	      (setq xs (cons x xs))
	      (setq live nil)
	      (setq i (+ i 1))
	      (setq last i))
	(t (setq i (+ i 1)))))
    (setq xs (reverse xs))
    (setq xs (mapcar #'parse-integer xs))
    (process-list xs)
    (format t "xs = ~a ~%" xs)
    t))




(defun data ()
  (reset)
  (let ((in (open "input" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do
	      (process-line line)
	      (format t "[~a]~%~%" line))
      (close in))))



;; my-hash (list x y)
;; no hash entry then air
(defun drop (n)
  (let ((x 500)
	(y 0))
    (catch 'rest
      (loop while t do
	(cond
	  ((not (gethash (list x (+ y 1)) my-hash))
	   (setq y (+ y 1)))
	  ((not (gethash (list (- x 1) (+ y 1)) my-hash))
	   (setq y (+ y 1))
	   (setq x (- x 1)))
	  ((not (gethash (list (+ x 1) (+ y 1)) my-hash))
	   (setq y (+ y 1))
	   (setq x (+ x 1)))
	  (t (setf (gethash (list x y) my-hash) n)
	     (throw 'rest n)))))))


(defun wait ()
  (data)
  (let ((n 1))
    (loop while t do
      (format t "drop ~a : ~a ~%" n (drop n))
      (incf n))))


#|
task 1
drop 1330 last entry to return 1330
drop 1331 falls into the abysss - signalled by never returning to repl

|#
(defun task-1 ()
  (wait))


#|
so assume the floor is an infinite horizontal line with
a y coordinate equal to two plus the highest y coordinate
of any point in your scan.

max-y 168
floor at y = 170 
|#


(defun drop2 (n)
  (let ((x 500)
	(y 0))
    (catch 'rest
      (loop while (not (gethash (list 500 0) my-hash)) do	
	(cond ;; infinite floor
	  ((= y 169)
	   (setf (gethash (list (- x 1) 170) my-hash) t)
	   (setf (gethash (list x 170) my-hash) t)
	   (setf (gethash (list (+ x 1) 170) my-hash) t))
	  (t t))
	(cond ;; 
	  ((not (gethash (list x (+ y 1)) my-hash))
	   (setq y (+ y 1)))
	  ((not (gethash (list (- x 1) (+ y 1)) my-hash))
	   (setq y (+ y 1))
	   (setq x (- x 1)))
	  ((not (gethash (list (+ x 1) (+ y 1)) my-hash))
	   (setq y (+ y 1))
	   (setq x (+ x 1)))
	  (t (setf (gethash (list x y) my-hash) n)
	     (throw 'rest n)))))))


(defun wait2()
  (data)
  (let ((n 1))
    (loop while (not (gethash (list 500 0) my-hash)) do
      (format t "drop ~a : ~a ~%" n (drop2 n))
      (incf n))))


(defun task-2 ()
  (wait2))

#|
AOC22> (gethash (list 500 0) my-hash)
26139
T
we find there is sand at 500 0 in the hash table
so we dropped 26,139 pieces of sand until it plugged the hole
|#
