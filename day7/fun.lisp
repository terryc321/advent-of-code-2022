
;; how represent a file system
;; advent of code - day 7 
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


;; read lines of file
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

;; ppcre regex matching groups extraction example
(defun extract-ranges (str)
  (cl-ppcre:register-groups-bind (sa sb sc)
      ("move ([0-9]+) from ([0-9]+) to ([0-9]+)" str)
    (let ((a (parse-integer sa))
	  (b (parse-integer sb))
	  (c (parse-integer sc)))
      ;; ensure not missing anything
      (assert (string= str (format nil "move ~a from ~a to ~a" a b c)))
      (list a b c))))



(defun drop (n xs)
  (assert (integerp n))
  (cond
    ((< n 1) xs)
    (t
     (assert (not (null xs)))
     (drop (- n 1) (cdr xs)))))

;; (drop 0 '(a b c))
;; (drop 1 '(a b c))
;; (drop 2 '(a b c))
;; (drop 3 '(a b c))
;; (drop 4 '(a b c))

(defun take (n xs)
  (assert (integerp n))
  (cond
    ((< n 1) '())
    (t
     (assert (not (null xs)))
     (cons (car xs) (take (- n 1) (cdr xs))))))

;; (take 1 '(a b c))
;; (take 2 '(a b c))
;; (take 3 '(a b c))
;; (take 4 '(a b c))

(defparameter *lines* nil) 

(defun example()


  ;; need double slash for $
;; $ ls
(cl-ppcre:register-groups-bind (all)
      ("(^\\$ ls)$" (nth 2 (nth 1 *lines*)))
  (format t "matched all : ~a ~%" all)
  t)


;; dir DIRNAME
(cl-ppcre:register-groups-bind (all dir)
      ("(^dir (.*))$" (nth 2 (nth 7 *lines*)))
  (format t "matched all : ~a : [~a] ~%" all dir)
  t)


;; XYZ filename
(cl-ppcre:register-groups-bind (all size file)
      ("((^[0-9]+) (.*))$" (nth 2 (nth 6 *lines*)))
  (format t "matched all : ~a : size [~a] : file [~a]~%" all size file)
  t)


;; need double slash for $
;;
;; 3 cases into , out-of , jump to root
;; $ cd DIR
;; $ cd ..
;; $ cd /
(cl-ppcre:register-groups-bind (all dir)
      ("(^\\$ cd (.*))$" (nth 2 (nth 20 *lines*)))
  (format t "matched all : ~a : dir [~a]~%" all dir)
  
  t)
);; ------ example ------


(defparameter *current-directory* nil)
(defparameter *known-files* nil)
(defparameter *sorted-files* nil)

(defun ls-command-p (str)
  (cl-ppcre:register-groups-bind (all)
      ("(^\\$ ls)$" str)
    (format t "matched all : ~a ~%" all)
    t))

(defun to-root-directory()
  (setq *current-directory* (list "/")))

(defun cd-command-root-p (str)
  (cl-ppcre:register-groups-bind (all)
      ("(^\\$ cd /)$" str)
    (format t "matched all : ~a : dir [the-root-directory]~%" all)
    (to-root-directory)
    t))

;; (cd-command-root-p "$ cd ..")
;; (cd-command-root-p "$ cd /")
;; (cd-command-root-p "$ cd asdf")

(defun out-of-directory()
  (assert (not (null *current-directory*)))
  (setq *current-directory* (cdr *current-directory*)))


;; only ?
;; $ cd ..
;;
(defun cd-command-outer-p (str)
  (cl-ppcre:register-groups-bind (all)
      ("(^\\$ cd \\.\\.)$" str)
    (format t "matched all : ~a : dir [higher-level]~%" all)
    (out-of-directory)
    t))

;; (cd-command-outer-p "$ cd ..")
;; (cd-command-outer-p "$ cd /")
;; (cd-command-outer-p "$ cd asdf")

(defun enter-directory(str)
  (setq *current-directory* (cons str *current-directory*)))


;; when we CD into something , we automatically add the "/" slash
(defun cd-command-generic-p (str)
  (cl-ppcre:register-groups-bind (all dir)
      ("(^\\$ cd (.*))$" str)
    (format t "matched all : ~a : dir [~a]~%" all dir)
    (enter-directory (concatenate 'string "/" dir))
    t))

;; match against root , out before generic
;; (cd-command-generic-p "$ cd ..")
;; (cd-command-generic-p "$ cd /")
;; (cd-command-generic-p "$ cd asdf")



(defun dir-command-p (str)
  (cl-ppcre:register-groups-bind (all dir)
      ("(^dir (.*))$" str)
    (format t "matched all : ~a : [~a] ~%" all dir)
    t))


(defun slurp(&rest args)
  (let ((result ""))
    (labels ((self (xs)
	       (cond
		 ((null xs) result)
		 (t (setq result (concatenate 'string result (car xs)))
		    (self (cdr xs))))))
      (apply #'self args))
    result))

;; *current-directory* is a list of directories we have traversed into
;; in reverse order
;; 
(defun generate-string-path-from-current-directory()
  (let ((dir (reverse *current-directory*)))
    (slurp dir)))


(defun add-known-file (file size)
  (setq *known-files* (cons (list (reverse *current-directory*) file size)
			    *known-files*)))


(defun file-command-p (str)
  (cl-ppcre:register-groups-bind (all size file)
      ("((^[0-9]+) (.*))$" str)
    (format t "matched all : ~a : size [~a] : file [~a]~%" all size file)
    (add-known-file file (parse-integer size))
    t))


;; assume process is nice , at some point it will cd to root directory
;; and we can assume this also
(defun process(filename)
  (setq *known-files* nil)
  (setq *current-directory* nil)
  (let ((lines (get-lines filename))
	(n 0))
    (loop while (not (null lines)) do
      (let ((line (nth 2 (car lines))))
	(incf n)
	(format t "~%~%input [~a] ~%" line)
	(format t "current-directory [~a] ~%" *current-directory*)	
	(cond
	  ((ls-command-p line)
	   (format t "[~a] : LS command.~%" n)
	   )
	  ((cd-command-root-p line)
	   (format t "[~a] : CD to TOPLEVEL ROOT.~%" n)
	   )
	  ((cd-command-outer-p line)
	   (format t "[~a] : CD OUT ONE LEVEL.~%" n)
	   )
	  ((cd-command-generic-p line)
	   (format t "[~a] : CD GENERIC.~%" n)
	   )
	  ((dir-command-p line)
	   (format t "[~a] : DIR command.~%" n)
	   )
	  ((file-command-p line)
	   (format t "[~a] : FILE command.~%" n)
	   )
	  (t (format t "[~a] : unknown command. : ~a ~%" n line)
	     (error line))))
      (setq lines (cdr lines)))))



(defun part1a()
  (process "input2"))

;; AOC22> *known-files*
;; ((("/d") "k" "7214296") (("/d") "d.ext" "5626152") (("/d") "d.log" "8033020")
;;  (("/d") "j" "4060174") (("/a" "/e") "i" "584") (("/a") "h.lst" "62596")
;;  (("/a") "g" "2557") (("/a") "f" "29116") (NIL "c.dat" "8504156")
;;  (NIL "b.txt" "14848514"))
;;
;; directory grows down , if 1st element is /a
;; then anything further on that directory is under /a
;;


;; hand coded a sample interaction so generate *known-files*
;;
;; $ cd /
;; $ ls
;; dir a
;; dir d
;; 14848514 b.txt
;; 8504156 c.dat
;; $ cd a
;; $ ls
;; dir e
;; 29116 f
;; 2557 g
;; 62596 h.lst
;; $ cd e
;; 584 i
;; $ cd /
;; $ cd d
;; $ ls
;; 4060174 j
;; 8033020 d.log
;; 5626152 d.ext
;; 7214296 k
;;
;;

;; for given *known-files*
;; find directories with less than 100,000
(defun extract-dirs ()
  (reverse (remove-duplicates (mapcar #'car *known-files*) :test #'equal)))

;; AOC22> (extract-dirs)
;; (NIL ("/a") ("/a" "/e") ("/d"))

;;(remove-duplicates *known-files*)

(defun part1()
  (process "input"))


;;
;; (directory "name" PTR)
;; (file "name" size)
;;
;; root is top a.k.a nil
;;
;; (defparameter *structure* (list 'directory 'root (list)))
;; ;; add directory
;; (defparameter *curdir* (nth 2 *structure*))
;; (setf *curdir* (append (list 'directory 'a (list)) *curdir*))

;;(defparameter *structure* (make-hash-table :test #'equalp))

;; sort *known-files*

(defun file-compare (x y)
  (or (< (length x) (length y))
      (string< (slurp x) (slurp y))))




;;(file-compare '(("/") "b.txt" "14848514") '(("/" "/a" "/e") "i" "584"))


;; ((("/" "/d") "k" "7214296") (("/" "/d") "d.ext" "5626152")
;;  (("/" "/d") "d.log" "8033020") (("/" "/d") "j" "4060174")
;;  (("/" "/a" "/e") "i" "584") (("/" "/a") "h.lst" "62596")
;;  (("/" "/a") "g" "2557") (("/" "/a") "f" "29116") (("/") "c.dat" "8504156")
;;  (("/") "b.txt" "14848514"))

(defun test ()
  (part1a))

;;  (setq *sorted-files* (sort *known-files* #'file-compare)))


;; (defparameter *my-hash* (make-hash-table :test #'equalp))

;; ;; zero out
;; (setf (gethash '("/" "/d") *my-hash*) (list))

;; ;; add entry
;; (let ((dir `("/" "/d")))
;;   (setf (gethash dir *my-hash*)
;; 	(cons `(1 2 3) (gethash dir *my-hash*)))

;;   (setf (gethash dir *my-hash*)
;; 	(cons `(4 5 6) (gethash dir *my-hash*)))

;;   (setf (gethash dir *my-hash*)
;; 	(cons `(7 8 9) (gethash dir *my-hash*))))

;; (gethash `("/" "/d") *my-hash*)

;; (setf (gethash `("/" "/d") *my-hash*)
;; 	(cons `(a b c) (gethash `("/" "/d") *my-hash*)))
  

;; for each directory-subdirectory in sorted
;; does the parent directory exist
;; ie /  /a /b /c /d ... exists ...
;; then
;;    /  /a /b /c has to exist
;;    /  /a /b has to exist
;;    /  /a has to exist
;;    / has to exist .
;;
;;
;; for each directory (s) identified in sorted
;; reverse it - iterate over until reach root "/"
;; node
;; if not member of sorted or extras
;; add it in 
(defun init-set ()
  (part1) ;; or part1  
  (let* ((sorted (sort (extract-dirs) #'file-compare))
	 (extras sorted))    
    (dolist (s sorted)
      (let ((lim (- (length s) 1)))	
	(loop for n from 2 to lim do
	  (let ((poss (subseq s 0 n)))
	    ;;(format t "possible dir~%~a~%~a~%~%" poss s)
	    (unless (or (member poss sorted :test #'equalp)
			(member poss extras :test #'equalp))
	      (setq extras (cons poss extras))
	      (format t "added dir ~a~%" poss)
	      )))))
    (append sorted extras)))

;; all directories full-set
;; all files *known-set*
(defun full-set ()
  (remove-duplicates (init-set) 
		     :test #'equalp))


(defun subset-p(f fs)
  (and (<= (length f)(length fs))
       (equalp f (subseq fs 0 (length f)))))


(defun cross-ref ()
  (let ((fs (full-set))
	(ks *known-files*)
	(hash (make-hash-table :test #'equalp)))
    (dolist (f fs) ;; f = dir
      (dolist (k ks) ;; k = (dir "filename" "size?")
	(when (subset-p f (car k))
	  (if (gethash f hash)
	      ;; add to known files in hash
	      (setf (gethash f hash)
		    (cons k (gethash f hash)))
	      ;; make this the first in hash
	      (setf (gethash f hash)
		    (list k))))))
    hash))


(defun viz ()
  (let ((h (cross-ref))
	(h2 (make-hash-table :test #'equalp))
	(mylist nil)
	(final-sum 0))
    (loop for key being the hash-keys of h
        using (hash-value value)
          do (format t "The value associated with the key ~S is ~S~%" key value))
    (format t "~%~%")
    (let ((root (gethash `("/") h)))
      (format t "for root node / we get total ~a files ~%"
	      root)
      (format t "for root node / we get total of ~a files ~%"
	      (length root)))

    (loop for key being the hash-keys of h
          do
	     (let ((val (gethash key h)))
	       (format t "key = ~a ~%" key)
	       ;;(format t "val = ~a ~%" val)
	       (format t "vals = ~a~%" (mapcar (lambda (x)(nth 2 x)) val))
	       (format t "sum = ~a ~%~%" (apply #'+ (mapcar (lambda (x)(nth 2 x)) val)))
	       (let ((mysum (apply #'+ (mapcar (lambda (x)(nth 2 x)) val))))
		 (setf (gethash key h2) mysum)
		 (setq mylist (cons (list key mysum) mylist))
		 (when (<= mysum 100000)
		   (incf final-sum mysum)
		   (format t "under 100k : adding ~a : final ~a~%" mysum final-sum)
		   ))))
    (format t "sum total of all under 100k is ~a ~%" final-sum)

    (let ((val (gethash `("/") h)))
      (format t "root.vals = ~a~%" (mapcar (lambda (x)(nth 2 x)) val))
      (format t "root.sum = ~a ~%~%" (apply #'+ (mapcar (lambda (x)(nth 2 x)) val))))

    (sort mylist #'(lambda (x y)(< (second x)(second y))))
    
    ))

      
    

(defconstant seventy-million 70000000)

(defconstant thirty-million 30000000)

;; given input 
;; root.sum   = 44274331 
;; used space = 44274331 
;; AOC22> (- seventy-million 44274331)
;; 25725669
;; required free space = 30000000
;; actual free space   = 25725669
;; deficit             =  4274331
;;                                                
;; check (+ 25725669 4274331) ; 30 million
;; ok
;;
;; target 4274331
;;                                                         4274331
;; (("/" "/wlqhpwqv" "/lpzgcrd" "/gcwg" "/vqp" "/rlbhdgm") 5025657)
;;
;; guess if put "rlbhdgm" as answer should work
;;
;; "/" "/wlqhpwqv" "/lpzgcrd" "/gcwg" "/vqp" "/rlbhdgm"
;; "/wlqhpwqv/lpzgcrd/gcwg/vqp/rlbhdgm"
;; "/wlqhpwqv/lpzgcrd/gcwg/vqp/rlbhdgm/"

;; for root node / we get total of 10 files 
;; key = (/) 
;; vals = (14848514 8504156 29116 2557 62596 584 4060174 8033020 5626152 7214296)
;; sum = 48381165 

;; key = (/ /a) 
;; vals = (29116 2557 62596 584)
;; sum = 94853 

;; key = (/ /d) 
;; vals = (4060174 8033020 5626152 7214296)
;; sum = 24933642 

;; key = (/ /a /e) 
;; vals = (584)
;; sum = 584 

;; NIL
;; AOC22> 
;; The total sizes of the directories above can be found as follows:
;;
;;     The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.
;;     The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).
;;     Directory d has total size 24933642.
;;     As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.
;;
;;

;; ... okay ...
;; under 100k : adding 17467 : final 1915606
;; sum total of all under 100k is 1915606 
;; NIL

;;; 70,000,000 total space on drive
;;; 30,000,000 free space required for update


	
    











