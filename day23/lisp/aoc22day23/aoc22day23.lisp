;;;; aoc22day23.lisp

(sb-ext:restrict-compiler-policy 'debug 3 3)
(sb-ext:restrict-compiler-policy 'safety 3 3)

(in-package #:aoc22day23)

;;; parsing phase

#|
task : read a file and turn grid into something more usable
[ ] open a file for reading
[ ] read line by line
[ ] determine playfield width height , ignoring external walls
[ ] player at 1 0 assumed
[ ] player exit at width (height + 1)
|#

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


;; plain square grid
(defun parse-lines (lines)
  (let* ((width (length (car lines)))
	 (height (length lines))
	 (x 0)
	 (y 0)
	 (grid (make-array (list (+ width 1)(+ height 1)) :initial-element #\.)))    
    (dolist (line lines)
      (incf y)
      (setq x 0)
      (let ((len (length line)))
	;; each line has same length
	(assert (= len width))
	(loop for ch in (coerce line 'list) do
	  (incf x)
	  (assert (or (char= ch #\.)(char= ch #\#)))
	  (setf (aref grid x y) ch))))
    grid))

(defun parse (filename)
  (parse-lines (parse-file filename)))

(defun test-parse ()
  (let ((lines (parse-file "../../input.txt")))
    (format t "lines => ~a~%" lines)
    (parse-lines lines)))

(defun grid-width (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (- width 1)))

(defun grid-height (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (- height 1)))

(defun grid-dimensions (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (list (- width 1)(- height 1))))


(defun show (grid)
  (let ((width (grid-width grid))
	(height (grid-height grid)))
    (loop for y from 1 to height do
      (format t "~%")
      (loop for x from 1 to width do
	(let ((ch (aref grid x y)))
	  (format t "~a" ch))))
    (format t "~%")))









