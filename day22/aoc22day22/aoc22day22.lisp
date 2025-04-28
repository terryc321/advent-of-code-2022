;;;; aoc22day22.lisp

(in-package #:aoc22day22)

;; define something patently false
(defun twice (x)  (* x 3))

(defun fib (n)
  (cond
    ((<= n 2) 1)
    (t (+ (fib (- n 1))
	  (fib (- n 2))))))
;; fib : 1 , 1 , 2 , 3 , 5 , 8 
;;   n   1   2   3   4   5   6   


(defun fac (n)
  (cond
    ((< n 2) 1)
    (t (* n (fac (- n 1))))))

;; (fac 10) => 120

;;; parsing phase 

;; each line should be same length
;; read-line chomps newline i presume
;; (parse-file "../../input.txt")
(defun parse-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines '()))
      (catch 'foo
	(loop while t do
	  (let ((line (read-line stream nil 'eof)))
	    (cond
	      ((eq line 'eof) (throw 'foo t))
	      (t (setq lines (cons line lines)))))))
      (setq lines (reverse lines))
      lines)))

;; instructions R or L or INTEGER
(defun parse-code (s)
  (let ((slist (coerce s 'list)))
    (parse-code-list slist)))

(defmacro add-right-instruction ()
  `(setq instructions (cons 'r instructions)))

(defmacro add-left-instruction ()
  `(setq instructions (cons 'l instructions)))

(defmacro add-forward-instruction (n)
  `(setq instructions (cons ,n instructions)))

(defun parse-code-list (slist)
  (let ((i 0)
	(len (length slist))
	(instructions nil))
    (loop while (< i len) do
      (let ((ch (nth i slist)))
	(cond
	  ((or (char= ch #\R)(char= ch #\r)) (add-right-instruction)(incf i))
	  ((or (char= ch #\L)(char= ch #\l)) (add-left-instruction)(incf i))
	  (t (let ((revdigits (list ch)))
	       (incf i)
	       (catch 'not-number
		 (loop while (< i len) do
		   (let ((ch (nth i slist)))
		     (cond
		       ((or (char= ch #\R)(char= ch #\r)) (throw 'not-number t))
		       ((or (char= ch #\L)(char= ch #\l)) (throw 'not-number t))
		       (t (setq revdigits (cons ch revdigits))
			  (incf i)))))
		 ;; no more input - but number still in revdigits
		 )
	       (setq revdigits (coerce (reverse revdigits) 'string))
	       (add-forward-instruction (parse-integer revdigits)))))))
    (reverse instructions)))

;; irregular shape
(defun parse-lines (lines)
  (let* ((width (length (car lines)))
	 (height (length lines))
	 (x 0)
	 (y 0)
	 (line nil)
	 (grid nil))
	 
    (catch 'grid-over
      (loop while t do
	     (setq line (car lines))
	     (incf y)
	     (setq x 0)
	     (let ((len (length line)))
	       (when (zerop len)
		 (setq lines (cdr lines))
		 (throw 'grid-over t))
	       (loop for ch in (coerce line 'list) do
		 (incf x)
		 (cond
		   ((char= ch #\#)
		    (setq grid (cons (list x y ch) grid)))
		   ((char= ch #\.)
		    (setq grid (cons (list x y ch) grid))))))
	     (setq lines (cdr lines))))
    (let ((code (parse-code (car lines))))
      (list code grid))))


(defun parse (filename)
  (parse-lines (parse-file filename)))

(defun test-parse (filename)
  (let ((lines (parse-file filename)))
    (format t "lines => ~a~%" lines)
    (parse-lines lines)))

(defparameter *example1* (test-parse "grids/example1.txt"))
(defparameter *input* (test-parse "grids/input.txt"))


