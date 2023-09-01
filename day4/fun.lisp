
(ql:quickload :cl-ppcre)

(defpackage :aoc22
  (:use :cl :cl-ppcre))

(in-package :aoc22)  

;; resorted to using cl-ppcre regex package now for regex matching
;; (register-groups-bind (a b)
;;     ("([0-9]+)-([0-9]+)" "123-456")
;;   (list a b))

;; a-b inside c-d
;; c-d inside a-b
(defun fully-contains(xs)
  (destructuring-bind (a b c d) xs
    (or (and (>= a c)(<= b d))
	(and (>= c a)(<= d b)))))


;; 0 1 2 3 4 5 6 7 8 9
;;     2 3
;;       3 4 5 6
;;
;;           5 6 7 8 9
;;               7 8
;; a,b < c
;;          d < a,b
;;      c < d
;;      c < d < a,b
;; no overlap

;; (defun range-outside (xs)
;;   (destructuring-bind (a b c d) xs
;;     (or (and (< a b)(< b c)
;; 	(and (> b a)(> a d))))))

;; if one end of range is between other range then its an overlap
(defun overlap (xs)
  (destructuring-bind (a b c d) xs
    (let ((does (or (and (>= a c)(<= a d))
		    (and (>= b c)(<= b d))
		    (and (>= c a)(<= c b))
		    (and (>= d a)(<= d b)))))
      `(abcd ,xs a ,a b ,b c ,c d ,d does? ,does)
      does)))



(defun get-lines ()
  (with-open-file (stream "input" :direction :input)
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


(defun extract-ranges (str)
  (register-groups-bind (salo sahi sblo sbhi)
      ("([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" str)
    (let ((alo (parse-integer salo))
	  (ahi (parse-integer sahi))
	  (blo (parse-integer sblo))
	  (bhi (parse-integer sbhi)))	  	    
      ;; fully-contains alo ahi blo bhi
      (list alo ahi blo bhi))))

(defun part1 ()
  (length (remove nil (mapcar #'fully-contains (mapcar #'extract-ranges (mapcar #'(lambda (x) (elt x 2)) (get-lines)))))))

;; 1000 lines
;; how many is there any overlap at all ?

(defun part2 ()
  (let* ((ranges (mapcar #'extract-ranges
			 (mapcar #'(lambda (x) (elt x 2)) (get-lines))))
	 (overlaps (mapcar (lambda (x) (overlap x))
			   ranges)))
    (format t "ranges ~a~%" ranges)
    (format t "overlap? ~a~%" overlaps)
    (format t "without nils ~%~a~%" (remove nil overlaps))
    (format t "without nils count~%~a~%" (length (remove nil overlaps)))))

;; (part2)
;; without nils count
;; 876

    




	      


