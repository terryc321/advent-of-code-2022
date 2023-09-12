
(ql:quickload :cl-ppcre)
(ql:quickload :uiop)

(declaim (optimize (speed 0)(space 0)(safety 3)(debug 3)(compilation-speed 0)))

(defpackage :aoc22
  (:use :cl))

(in-package :aoc22)  

;; read lines from file
(defun get-lines (filename)
  (with-open-file (stream filename :direction :input)
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


(defun process(filename)
  (let ((lines (get-lines filename)))
    lines))


	
