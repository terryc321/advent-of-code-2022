


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


(defun micro-step (state command)
  (destructuring-bind (a b c) command
    (assert (integerp a))
    (assert (integerp b))
    (assert (integerp c))
    (assert (>= a 0))
    (assert (>= b 1))
    (assert (<= b 9))
    (assert (>= c 1))
    (assert (<= c 9))
    (assert (not (null (nth b state))))
    (cond
      ((< a 1) state)
      (t (let ((mov (car (nth b state))))
	   (setf (nth b state) (cdr (nth b state)))
	   (setf (nth c state) (cons mov (nth c state)))
	   state)))))


;; read lines of file
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


(defun part1()
  (let ((input (nth 2 (car (get-lines)))))
    (find-marker input)))


(defun find-marker (str)
  (let ((len-str (length str)))
    (catch 'found-marker
      (loop for i from 0 to (- len-str 1) do
	(let ((ai (- i 3))
	      (bi (- i 2))
	      (ci (- i 1))
	      (di i))
	  (when (and (>= ai 0)(<= di (- len-str 1)))
	    (let ((i1 (char-code (char str ai)))
		  (i2 (char-code (char str bi)))
		  (i3 (char-code (char str ci)))
		  (i4 (char-code (char str di))))
	      (when (/= i1 i2 i3 i4)
		(throw 'found-marker (+ i 1)))))))
      -1)))



(list 
(find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz") ;;: first marker after character 5
(find-marker "nppdvjthqldpwncqszvftbrmjlhg") ;; : first marker after character 6
(find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;;: first marker after character 10
(find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ;;: first marker after character 11
)

;; (5 6 10 11)


;; ;; arbitrary number of 
;; (defun find-message (str)
;;   (let ((len-str (length str)))
;;     (catch 'found-marker
;;       (loop for i from 0 to (- len-str 1) do
;; 	(let ((ai (- i 14))
;; 	      (bi (- i 13))
;; 	      (ci (- i 12))
;; 	      (di i))
;; 	  (when (and (>= ai 0)(<= di (- len-str 1)))
;; 	    (let ((i1 (char-code (char str ai)))
;; 		  (i2 (char-code (char str bi)))
;; 		  (i3 (char-code (char str ci)))
;; 		  (i4 (char-code (char str di))))
;; 	      (when (/= i1 i2 i3 i4)
;; 		(throw 'found-marker (+ i 1)))))))
;;       -1)))



;; (defmacro find-message-macro-14 ()
;;   `(let ((len-str (length str)))
;;     (catch 'found-marker
;;       (loop for i from 0 to (- len-str 1) do
;; 	(let ((ai (- i 13))
;; 	      (bi (- i 12))
;; 	      (ci (- i 11))
;; 	      (di i))
;; 	  (when (and (>= ai 0)(<= di (- len-str 1)))
;; 	    (let ((i1 (char-code (char str ai)))
;; 		  (i2 (char-code (char str bi)))
;; 		  (i3 (char-code (char str ci)))
;; 		  (i4 (char-code (char str di))))
;; 	      (when (/= i1 i2 i3 i4)
;; 		(throw 'found-marker (+ i 1)))))))
;;       -1)))



;; (find-message-macro-14)  
;; (defparameter p 1)
;; (inc p)


;; write some code to generate the required s expression
(defun find-message-generator ()
  (progn
    (format t "(defun find-message(str)~%")  
    (format t "(let ((len-str (length str)))
    (catch 'found-message
      (loop for i from 0 to (- len-str 1) do~%")
    (format t "(let (~%")  
    (loop for i from 0 to 13 do
      (format t "(~ai (- i ~a)) ~%" (code-char (+ i (char-code #\a))) (- 13 i)))
    (format t ")~%")    
    (format t "(when (and (>= ai 0)(<= ni (- len-str 1)))~%")
    (format t "(let (~%")
    (loop for i from 0 to 13 do
      (format t "(i~a (char-code (char str ~ai)))~%" (+ i 1) (code-char (+ i (char-code #\a)))))
    (format t ")~%")  
    (format t "(when (/= ")
    (loop for i from 0 to 13 do
      (format t " i~a " (+ i 1)))  
    (format t ")~%")
    (format t "(throw 'found-message (+ i 1))~%")
    (format t "))))) ~% -1 ~%)))~%")  
    ))



(defun find-message(str)
  (let ((len-str (length str)))
    (catch 'found-message
      (loop for i from 0 to (- len-str 1) do
	(let (
	      (ai (- i 13)) 
	      (bi (- i 12)) 
	      (ci (- i 11)) 
	      (di (- i 10)) 
	      (ei (- i 9)) 
	      (fi (- i 8)) 
	      (gi (- i 7)) 
	      (hi (- i 6)) 
	      (ii (- i 5)) 
	      (ji (- i 4)) 
	      (ki (- i 3)) 
	      (li (- i 2)) 
	      (mi (- i 1)) 
	      (ni (- i 0)) 
	      )
	  (when (and (>= ai 0)(<= ni (- len-str 1)))
	    (let (
		  (i1 (char-code (char str ai)))
		  (i2 (char-code (char str bi)))
		  (i3 (char-code (char str ci)))
		  (i4 (char-code (char str di)))
		  (i5 (char-code (char str ei)))
		  (i6 (char-code (char str fi)))
		  (i7 (char-code (char str gi)))
		  (i8 (char-code (char str hi)))
		  (i9 (char-code (char str ii)))
		  (i10 (char-code (char str ji)))
		  (i11 (char-code (char str ki)))
		  (i12 (char-code (char str li)))
		  (i13 (char-code (char str mi)))
		  (i14 (char-code (char str ni)))
		  )
	      (when (/=  i1  i2  i3  i4  i5  i6  i7  i8  i9  i10  i11  i12  i13  i14 )
		(throw 'found-message (+ i 1))
		))))) 
      -1 
      )))




(list
(find-message "mjqjpqmgbljsphdztnvjfqwrcgsmlb") ;;: first marker after character 19
(find-message "bvwbjplbgvbhsrlpgdmjqwftvncz") ;;: first marker after character 23
(find-message "nppdvjthqldpwncqszvftbrmjlhg") ;;: first marker after character 23
(find-message "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;;: first marker after character 29
(find-message "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ;;: first marker after character 26
)

;; (19 23 23 29 26)


(defun part2()
  (let ((input (nth 2 (car (get-lines)))))
    (find-message input)))


;;3708


