
(defpackage :aoc22 (:use :cl))

(in-package :aoc22)  

;; open input file input.txt
;; read until no more input
;; digits 0 to 9 

(defun num-string (str)
  (let ((result "")
	(any-digits nil))
    t))


(defun get-elves (filename)
  (with-open-file (stream filename :direction :input)
    (let ((got t)
	  (elves '())
	  (elf '())
	  (line 1))
    (loop until (not got) do
      (setq got (read-line stream nil nil))
      ;; if empty line
      (cond
	((or (string= got "")
	     (string= got "NIL"))
	 (setq elves (cons elf elves))
	 (setq elf '()))
	(t (let ((num (parse-integer got)))
	     (setq elf (cons num elf)))))
      (format t "line ~a : got = \"~a\" ~%" line got)
      (incf line))
      elves)))

(defvar elves nil)
(defvar sorted-elves nil)

(defun f ()
  (setq elves (get-elves "input.txt"))
  (format t "elves = ~a ~%" elves)
  (format t "there were ~a elves ~%" (length elves)))


(defun g ()
  (setq elves (get-elves "all-input.txt"))
  (setq elves (reverse (mapcar #'reverse elves)))
  (format t "elves = ~a ~%" elves)
  (let ((n 1)	
	(elf-n 0)
	(res '())
	(high 0))
    (dolist (elf elves nil)
      (let ((tot (apply #'+ elf)))
	(setq res (cons `(elf ,n ,elf ,tot) res))
	(format t "elf ~a =~a : sum ~a : high =~a ~%" n elf tot high)
	(when (>= tot high)
	  (setq high tot)
	  (setq elf-n n)
	  (format t "another best =~a : tot =~a ~%" elf tot))
      (incf n)
      ))    
    (format t "there were ~a elves ~%" (length elves))
    (setq elves res)
    res))


;; elf 21 had these calories for a total of 68802
;; (7661 2803 7796 7485 6268 7251 7346 8521 5165 8506)

  ;; ;; (let ((vec (make-array (length elves) :initial-contents elves)))
  ;; ;;   vec))
  ;; )

(defun part2 ()
  (g)
  (let ((se nil))
    (setq sorted-elves (sort elves (lambda (xs ys)(> (elt xs 3)(elt ys 3)))))
    (setq se sorted-elves)
    (let ((top-three (list (elt se 0)
			   (elt se 1)
			   (elt se 2))))
      (format t "top three ~a ~%" top-three)
      (format t "sum ~a ~%" (mapcar (lambda (xs) (elt xs 3)) top-three))
      (format t "sum ~a ~%" (apply #'+ (mapcar (lambda (xs) (elt xs 3)) top-three)))
      )))

;; 205370



      
    







  







      







