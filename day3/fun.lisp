
(defpackage :aoc22 (:use :cl))

(in-package :aoc22)  

;; is this character in this sequence ?
(defun string->list (s1)
  (let ((result nil)
	(len-s1 (length s1)))
    (loop for i from 0 upto (- len-s1 1) do
      (setq result (cons (char s1 i) result)))
    (reverse result)))


(defun common-letters (xs1 xs2)
  (let ((common '()))
    (dolist (x1 xs1 common)
      (if (member x1 xs2 :test #'char=)
	  (if (member x1 common :test #'char=)
	      nil
	      (setq common (cons x1 common)))
	  nil))
    common))



(defun common-triple (s1 s2 s3)
  (common-letters s1 (string (common-letters s2 s3))))



(defun code (a)
  (assert (eq (type-of a) 'standard-char))
  (let ((pair (assoc a `((#\A . 27)			 (#\B . 28)			 (#\C . 29)
			 (#\D . 30) 			 (#\E . 31)			 (#\F . 32)
			 (#\G . 33)			 (#\H . 34)			 (#\I . 35) 
			 (#\J . 36)			 (#\K . 37)			 (#\L . 38)
			 (#\M . 39)			 (#\N . 40)			 (#\O . 41)
			 (#\P . 42)			 (#\Q . 43)			 (#\R . 44)
			 (#\S . 45)			 (#\T . 46)			 (#\U . 47)
			 (#\V . 48)			 (#\W . 49)			 (#\X . 50)
			 (#\Y . 51)			 (#\Z . 52)			 (#\a . 1)
			 (#\b . 2)			 (#\c . 3)			 (#\d . 4)
			 (#\e . 5)			 (#\f . 6)			 (#\g . 7)
			 (#\h . 8)			 (#\i . 9)			 (#\j . 10)	     
			 (#\k . 11)			 (#\l . 12)			 (#\m . 13)
			 (#\n . 14)			 (#\o . 15)			 (#\p . 16)
			 (#\q . 17)			 (#\r . 18)			 (#\s . 19)
			 (#\t . 20)			 (#\u . 21)			 (#\v . 22)
			 (#\w . 23)			 (#\x . 24)			 (#\y . 25)
			 (#\z . 26)))))
    (assert pair)
    (cdr pair)))
	



(defun get-lines ()
  (with-open-file (stream "input" :direction :input)
    (let ((got t)
	  (lines '())
	  (line 1))
    (loop until (not got) do
      (setq got (read-line stream nil nil))
      (when got

	;; all even 
	(assert (= 0 (mod (length got) 2)))

	(let* ((len-got (length got))
	       (halfway (/ len-got 2))
	       (alpha (subseq got 0 halfway))
	       (beta (subseq got halfway (* 2 halfway)))
	       (com (common-letters (string->list alpha)
				    (string->list beta)))
	       (ct (code (car com))))

	  (assert (= len-got (+ (length alpha)(length beta))))
	       	
	  (setq lines (cons `(line ,line ,got ,alpha ,beta (common ,com) ,ct)
			    lines))
	  (format t "line ~a : got = \"~a\" ~%" line (car lines))
	  (incf line))))      
      (reverse lines))))



(defun part1()
  (apply #'+ (mapcar #'(lambda (x) (elt x 6) ) (get-lines))))


;; organise into triples 
(defun get-triples ()
  (let ((lines (get-lines))
	(result nil))  
    (loop while lines do
      (let ((a (car lines))
	    (b (car (cdr lines)))
	    (c (car (cdr (cdr lines)))))
	(setq result (cons
		      `(,a ,b ,c)		    
		      result))
	;; skip 3 lots
	(setq lines (cdr (cdr (cdr lines))))
	))
    (reverse result)))

  
;;(defvar triples (get-triples))
(defparameter triples (get-triples))

(defun part2 ()
  (let ((tot 0))
    (dolist (triple triples)
      (let* ((the-strings (mapcar #'(lambda (x) (elt x 2)) triple))
	     (a (string->list (elt the-strings 0)))
	     (b (string->list (elt the-strings 1)))
	     (c (string->list (elt the-strings 2))))
	(format t "a : ~a ~%" a)
	(format t "b : ~a ~%" b)
	(format t "c : ~a ~%" c)

	(format t "c-ab : ~a ~%" (common-letters a b))
	(format t "c-bc : ~a ~%" (common-letters b c))
	(format t "c-abc : ~a ~%" (common-letters a (common-letters b c)))
	(format t "c-abc : ~a ~%" (common-letters (common-letters a b) c))

	(let ((elf-code (code (car (common-letters (common-letters a b) c)))))
	  (setq tot (+ tot elf-code))
	  (format t "c-abc elf-code : ~a : total ~a ~%" elf-code tot))
	
	(format t "~%~%")
	tot))))



    
	 
    ;; (format t "triple strings: ~a~%" the-strings)
    ;; (format t "triple common: ~a~%" (apply #'common-triple the-strings))))

    
;;c-abc elf-code : 16 : total 2689 












