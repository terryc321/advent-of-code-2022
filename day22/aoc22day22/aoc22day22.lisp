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
		   ((char= ch #\space)
		    (setq grid (cons (list x y ch) grid)))
		   ((char= ch #\#)
		    (setq grid (cons (list x y ch) grid)))
		   ((char= ch #\.)
		    (setq grid (cons (list x y ch) grid)))
		   (t
		    (setq grid (cons (list x y ch) grid)))
		   )))
	     (setq lines (cdr lines))))
    
    (setq grid (reverse grid))

    (let ((code (parse-code (car lines))))
      (cons code grid))))


(defun parse (filename)
  (parse-lines (parse-file filename)))

(defun test-parse (filename)
  (let ((lines (parse-file filename)))
    (format t "lines => ~a~%" lines)
    (parse-lines lines)))

;; make use of dynamic scope of start-square we can influence show-grid 
(defparameter *start-square* nil)
(defparameter *test1* (test-parse "grids/test1.txt"))
(defparameter *example1* (test-parse "grids/example1.txt"))
(defparameter *input* (test-parse "grids/input.txt"))

;; this unique in determines max-x on a line per line basis
;; determine size of grid to show
;; find min max x y , show as a grid 
(defun show-grid (input)
  (let ((squares (cdr input))
	(min-x nil)
	(max-x nil)
	(min-y nil)
	(max-y nil))
    
    ;; determine min max for grid diagram
    (loop for square in squares do
      (destructuring-bind (x y ch) square
      	(when (or (not min-x)(< x min-x)) (setq min-x x))
	(when (or (not min-y)(< y min-y)) (setq min-y y))	
	(when (or (not max-x)(> x max-x)) (setq max-x x))
	(when (or (not max-y)(> y max-y)) (setq max-y y))))
    
    ;; only one square per x y so when seen it jump to next square
    (loop for y from min-y to max-y do       
    ;;   (let ((max-x 0))
    ;; 	(loop for square in squares do
    ;; 	  (destructuring-bind (x2 y2 ch) square
    ;; 	    (cond
    ;; 	      ((= y2 y) (when (> x2 max-x) (setq max-x x2))))))
      
      (loop for x from min-x to max-x do
	(catch 'seen
	  (loop for square in squares do
	    (destructuring-bind (x2 y2 ch) square
	      (cond
		((and (= x2 x)(= y2 y))
		 (cond
		   ((equalp *start-square* (list x y)) (format t "(~a)" ch))
		   (t (format t "~a" ch)))
		 (throw 'seen t)))))
	  ;; no char registered must mean this is a blank square
	  ;;(format t " ")
	  ))
      (format t "~%"))))


;; show code
(defun show-code (code)
  (loop for x in code do
    (format t "~a" x)))

;; check input sanity
;; show grid then newline then codes
(defun sanity-input ()
  (with-open-file (*standard-output* "grids/input.dat"
                                   :direction :output
                                   :if-exists :supersede)
    (let ((grid (cdr *input*))
	  (code (car *input*)))
      (show-grid *input*)
      (format t "~%")
      (show-code code)
      (format t "~%")
      )))

;; hash table of it
(defun make-input-hash (input)
  (let ((hash (make-hash-table :test #'equalp))
	(squares (cdr input))
	(min-x nil)
	(max-x nil)
	(min-y nil)
	(max-y nil))
    
    ;; determine min max for grid diagram
    (loop for square in squares do
      (destructuring-bind (x y ch) square
      	(when (or (not min-x)(< x min-x)) (setq min-x x))
	(when (or (not min-y)(< y min-y)) (setq min-y y))	
	(when (or (not max-x)(> x max-x)) (setq max-x x))
	(when (or (not max-y)(> y max-y)) (setq max-y y))
	(setf (gethash (list x y) hash) ch)))
    (lambda (op &rest args)
      (cond
	;; every square will return either # gate or . open or #\? nowhere
	((eq op 'look) (let* ((x (first args))
			     (y (second args))
			     (ch (gethash (list x y) hash nil)))
			 (cond
			   ((null ch) #\?)
			   ((char= ch #\#) ch)
			   ((char= ch #\.) ch)
			   ((char= ch #\space) #\?))))
	((eq op 'width) (- max-x min-x))
	((eq op 'height) (- max-y min-y))
	(t (error "input-hash"))))))



;; for my data 
;; start square should be (51,1) meaning 1st row and 51 characters across

;; find start position
(defun start-position (input)
  (declare (optimize (debug 3)));;
  (let ((squares (cdr input))
	(start-x nil)
	(start-y 1))
    
    ;; determine min max for grid diagram
    (loop for square in squares do
      (destructuring-bind (x y ch) square
	(cond
	  ((= y 1)
	   (when (char= ch #\.)
	     (cond	      
	       ((not start-x) (setq start-x x))
	       ((< x start-x) (setq start-x x))))))))
    
    (let* ((start (list start-x start-y))
	   (*start-square* start))
      ;;(show-grid input)
      start)))

;; 200
(defparameter *max-height* 200)


;; 150
(defparameter *max-width* 150)

    
#|

initially facing right

get position from start-position

simplify problem by making a hash table - have a character as value - key (x,y)

|#

(defmacro %%move-right%% ()
  `(catch 'stop
     (loop for i from 1 to n do
       (format t "i = ~a ~%" i)
      (let* ((x2 (+ x 1))(y2 y)(ch (funcall hfn 'look x2 y2)))
	(cond
	  ((char= ch #\#) 'stop! (throw 'stop t))
	  ((char= ch #\.) 'ok (setq x (+ x 1)))
	  (t ;; wrap
	   (format t "RIGHT: char got ~a ~%" ch)
	   (catch 'wrapped
	     (loop for px from 1 do
	       (format t "px = ~a ~%" px)
	       (let ((ch (funcall hfn 'look px y)))
		 (cond
		   ((char= ch #\#) 'stop! (throw 'stop t))
		   ((char= ch #\.) 'ok (setq x px)(throw 'wrapped t))
		   (t 'keep-looking)))))))))))


(defmacro %%move-down%% ()
  `(catch 'stop
    (loop for i from 1 to n do 
      (let* ((x2 x)(y2 (+ y 1))(ch (funcall hfn 'look x2 y2)))
	(cond
	  ((char= ch #\#) 'stop! (throw 'stop t))
	  ((char= ch #\.) 'ok (setq y (+ y 1)))
	  (t ;; wrap
	   (catch 'wrapped
	     (loop for py from 1 do
	       (let ((ch (funcall hfn 'look x py)))
		 (cond
		   ((char= ch #\#) 'stop! (throw 'stop t))
		   ((char= ch #\.) 'ok (setq y py)(throw 'wrapped t))
		   (t 'keep-looking)))))))))))


(defmacro %%move-up%% ()
  `(catch 'stop
    (loop for i from 1 to n do 
      (let* ((x2 x)(y2 (- y 1))(ch (funcall hfn 'look x2 y2)))
	(cond
	  ((char= ch #\#) 'stop! (throw 'stop t))
	  ((char= ch #\.) 'ok (setq y (+ y 1)))
	  (t ;; wrap
	   (catch 'wrapped
	     (loop for py from *max-height* downto 1 do
	       (let ((ch (funcall hfn 'look x py)))
		 (cond
		   ((char= ch #\#) 'stop! (throw 'stop t))
		   ((char= ch #\.) 'ok (setq y py)(throw 'wrapped t))
		   (t 'keep-looking)))))))))))


(defmacro %%move-left%% ()
  `(catch 'stop
    (loop for i from 1 to n do 
      (let* ((x2 (- x 1))(y2 y)(ch (funcall hfn 'look x2 y2)))
	(cond
	  ((char= ch #\#) 'stop! (throw 'stop t))
	  ((char= ch #\.) 'ok (setq x (+ x 1)))
	  (t ;; wrap
	   (catch 'wrapped
	     (loop for px from *max-width* downto 1 do
	       (let ((ch (funcall hfn 'look px y)))
		 (cond
		   ((char= ch #\#) 'stop! (throw 'stop t))
		   ((char= ch #\.) 'ok (setq x px)(throw 'wrapped t))
		   (t 'keep-looking)))))))))))


(defun fun ()
  (let* ((input *input*)
	 (codes (car input))
	 (hfn (make-input-hash input))
	 (pos (start-position input))
	 (dir 'right))
    (destructuring-bind (x y) pos
      (let ((i 1))
	(format t "codes (~a) => ~a~%" i codes)
	(incf i))
      (labels ((turn-right ()
		 (cond
		   ((eq dir 'right) (setq dir 'down))
		   ((eq dir 'down) (setq dir 'left))
		   ((eq dir 'left) (setq dir 'up))
		   ((eq dir 'up) (setq dir 'right))
		   (t (error "turn-right"))))
	       (turn-left ()
		 (cond
		   ((eq dir 'right) (setq dir 'up))
		   ((eq dir 'down) (setq dir 'right))
		   ((eq dir 'left) (setq dir 'down))
		   ((eq dir 'up) (setq dir 'left))
		   (t (error "turn-left"))))
	       (forward (n)
		 (cond
		   ((eq dir 'right) (format t "(~a,~a,~a)~%" x y dir) (%%move-right%%))
		   ((eq dir 'down) (%%move-down%%))
		   ((eq dir 'left) (%%move-left%%))
		   ((eq dir 'up) (%%move-up%%))
		   (t (error "forward")))))
	(let ((i 0))
	  (dolist (code codes)
	    (incf i)
	    (cond
	      ((eq code 'r) (format t "code (~a) => turn right~%" i) (turn-right))
	      ((eq code 'l) (format t "code (~a) => turn left~%" i)(turn-left))
	      ((integerp code)
	       (format t "code (~a) => moving ~a steps forward in direction ~a~%" i code dir)
	       (forward code))
	      (t (error "fun")))))))))








      
	     




























