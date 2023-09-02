
;; how represent a file system
;; advent of code - day 7 
;;
;; 1 - machine
;; 2 - file count
;; 

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


(defun subset-p(f fs)
  (and (<= (length f)(length fs))
       (equalp f (subseq fs 0 (length f)))))

(assert (eq t (subset-p '("/") '("/" "a"))))
(assert (eq t (subset-p '("/") '("/"))))
(assert (eq nil (subset-p '("/" "a") '("/"))))
(assert (eq nil (subset-p '("/" "a" "b") '("/" "a" "c"))))
(assert (eq nil (subset-p '("/" "a" "b") '("/" "a"))))
(assert (eq t (subset-p '("/" "a" "b") '("/" "a" "b"))))
(assert (eq t (subset-p '("/" "a" "b") '("/" "a" "b" "c"))))

(defconstant seventy-million 70000000)

(defconstant thirty-million 30000000)

;; assume process is nice , at some point it will cd to root directory
;; and we can assume this also
(defun process(filename)
  (let ((lines (get-lines filename))
	(known-files nil)
	(known-directories (list (list "/")))
	(current-directory nil)
	(my-hash (make-hash-table :test #'equalp))
	(my-hash2 (make-hash-table :test #'equalp))
	(flat-sum nil)
	(sorted-sum nil)
	(n 0))
    (labels ((ls-command-p (str)
	       (cl-ppcre:register-groups-bind (all) ("(^\\$ ls)$" str)
		 (format t "matched all : ~a ~%" all)
		 t))
	     (cd-command-root-p (str)
	       (cl-ppcre:register-groups-bind (all) ("(^\\$ cd /)$" str)
		 (format t "matched all : ~a : dir [the-root-directory]~%" all)
		 (setq current-directory (list "/"))
		 t))
	     (out-of-directory ()
	       (assert (not (null current-directory)))
	       (setq current-directory (cdr current-directory)))
	     (cd-command-outer-p (str)
	       (cl-ppcre:register-groups-bind (all)  ("(^\\$ cd \\.\\.)$" str)
		 (format t "matched all : ~a : dir [higher-level]~%" all)
		 (out-of-directory)
		 t))
	     (enter-directory (str)
	       (setq current-directory (cons str current-directory)))
	     (cd-command-generic-p (str)
	       (cl-ppcre:register-groups-bind (all dir)  ("(^\\$ cd (.*))$" str)
		 (format t "matched all : ~a : dir [~a]~%" all dir)
		 (enter-directory dir)
		 t))
	     (add-known-directory (dir)
	       (setq known-directories (cons (reverse (cons dir current-directory))
					     known-directories)))
	     (dir-command-p (str)
	       (cl-ppcre:register-groups-bind (all dir)  ("(^dir (.*))$" str)
		 (format t "matched all : ~a : [~a] ~%" all dir)
		 (add-known-directory dir)
		 t))
	     (add-known-file (file size)
	       (setq known-files (cons (list (reverse current-directory) file size)
				       known-files)))
	     (file-command-p (str)
	       (cl-ppcre:register-groups-bind (all size file) ("((^[0-9]+) (.*))$" str)
		 (format t "matched all : ~a : size [~a] : file [~a]~%" all size file)
		 (add-known-file file (parse-integer size))
		 t)))
      ;; now do something
      (loop while lines do
	(let ((line (nth 2 (car lines))))
	  (incf n)
	  (format t "~%~%input [~a] ~%" line)
	  (format t "current-directory [~a] ~%" current-directory)	
	  (cond
	    ((ls-command-p line)  (format t "[~a] : LS command.~%" n))
	    ((cd-command-root-p line) (format t "[~a] : CD to TOPLEVEL ROOT.~%" n))
	    ((cd-command-outer-p line) (format t "[~a] : CD OUT ONE LEVEL.~%" n))
	    ((cd-command-generic-p line) (format t "[~a] : CD GENERIC.~%" n))
	    ((dir-command-p line) (format t "[~a] : DIR command.~%" n))
	    ((file-command-p line) (format t "[~a] : FILE command.~%" n))
	    (t (format t "[~a] : unknown command. : ~a ~%" n line)
	       (error line)))
	  (setq lines (cdr lines)))))
    ;; remove duplicate directories
    (setq known-directories (remove-duplicates known-directories :test #'equalp))
    ;; 
    (values known-files
	    known-directories)
    ;; if for some kd directory , there is a file that sits at or below this level
    ;; include it 
    (dolist (kdir known-directories) ;; for some dir
      (setf (gethash kdir my-hash) nil)
      (dolist (kf known-files) ;; for some file = [dir name size]
	(destructuring-bind (fdir fname fsize) kf
	  (when (subset-p kdir fdir)
	    (format t "subset ~a ~a ~%" kdir fdir)
	    (setf (gethash kdir my-hash)
		  (cons (list fname fsize)
			(gethash kdir my-hash)))))))
    ;; so iterated over all known directories , all files should have been touched
    (dolist (kdir known-directories) ;; for some dir
      (setf (gethash kdir my-hash2) 0)
      (format t "hash ~a~%~a~%" kdir (gethash kdir my-hash))
      (let ((sum-hash-1 (apply #'+ (mapcar #'second (gethash kdir my-hash)))))
	(setf (gethash kdir my-hash2) sum-hash-1)
	(format t "sum ~a ~%~%" sum-hash-1)))
    ;;
    (dolist (kdir known-directories)
      (setq flat-sum (cons (list kdir (gethash kdir my-hash2)) flat-sum)))
    ;;
    (setq sorted-sum (stable-sort flat-sum (lambda (x y)(< (second x)(second y)))))

    ;; largest first?
    (setq sorted-sum (reverse sorted-sum))

    (let ((root-size (second (first sorted-sum))))
      (format t "root size / consumes ~a ~%" root-size)
      (let ((shortfall (- thirty-million (- seventy-million root-size))))
	(format t "shortfall ~a ~%" shortfall)
	(let ((difference 999999999999999999))
	  (dolist (ss sorted-sum)
	    (destructuring-bind (dir fsize) ss
	      (when (and (>= fsize shortfall)
			 (< (- fsize shortfall) difference))
		(setq difference (- fsize shortfall))
		(format t "found new best ~%~a @ ~a ~%~%" difference ss)))))))
    ;;
    sorted-sum
    ))



;; 70000000
;; seventy-million


;; used space
(defconstant used-space 44274331)

(defconstant avail-space (- seventy-million used-space))

(defconstant required-space thirty-million)


;;(defparameter shortfall (- required-space avail-space))


;; used-space                   44274331
;; avail-space                  25725669
;; required-space               30000000
;; shortfall of                  4274331
;; (+ shortfall avail-space)    30000000
;;
;; (("/" "wlqhpwqv" "lpzgcrd" "gcwg" "vqp" "rlbhdgm") 5025657)
;; (("/" "wlqhpwqv" "lpzgcrd" "gcwg" "vqp" "rlbhdgm" "bzd") 4215343)
;;

;;
;; found new best 
;; 751326 @ ((/ wlqhpwqv lpzgcrd gcwg vqp rlbhdgm) 5025657) 
;;
;; was correct , just wanted the size of directory rather than the directory name
;; dumb. just dumb.

	

#|
 shows 187 unique known-directories
 shows 270 unique known-files
|#






  
