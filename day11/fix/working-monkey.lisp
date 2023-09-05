
;; array size 10 , 0 to 9 index inclusive
;; (defparameter p (make-array 10 :initial-element 0))
;; (aref p 0)
;; (setf (aref p 0) 123)
;; p

;;(declaim (optimize (speed 0) (space 0) (debug 3)))
(declaim (optimize (speed 3) (space 0) (debug 0)))



;; length 8
(defparameter monkees 
  #(((MONKEY-ID 0) (STARTING-ITEMS (71 56 50 73)) (FORMULA * 11) (DIVISIBLE-BY 13)
     (THROW 1 7))
    ((MONKEY-ID 1) (STARTING-ITEMS (70 89 82)) (FORMULA + 1) (DIVISIBLE-BY 7)
     (THROW 3 6))
    ((MONKEY-ID 2) (STARTING-ITEMS (52 95)) (FORMULA * OLD) (DIVISIBLE-BY 3)
     (THROW 5 4))
    ((MONKEY-ID 3) (STARTING-ITEMS (94 64 69 87 70)) (FORMULA + 2)
     (DIVISIBLE-BY 19) (THROW 2 6))
    ((MONKEY-ID 4) (STARTING-ITEMS (98 72 98 53 97 51)) (FORMULA + 6)
     (DIVISIBLE-BY 5) (THROW 0 5))
    ((MONKEY-ID 5) (STARTING-ITEMS (79)) (FORMULA + 7) (DIVISIBLE-BY 2)
     (THROW 7 0))
    ((MONKEY-ID 6) (STARTING-ITEMS (77 55 63 93 66 90 88 71)) (FORMULA * 7)
     (DIVISIBLE-BY 11) (THROW 2 4))
    ((MONKEY-ID 7) (STARTING-ITEMS (54 97 87 70 59 82 59)) (FORMULA + 8)
     (DIVISIBLE-BY 17) (THROW 1 3))))

;; length 4
(defparameter monkees2 
  #(((MONKEY-ID 0) (STARTING-ITEMS (79 98)) (FORMULA * 19) (DIVISIBLE-BY 23)
     (THROW 2 3))
    ((MONKEY-ID 1) (STARTING-ITEMS (54 65 75 74)) (FORMULA + 6) (DIVISIBLE-BY 19)
     (THROW 2 0))
    ((MONKEY-ID 2) (STARTING-ITEMS (79 60 97)) (FORMULA * OLD) (DIVISIBLE-BY 13)
     (THROW 1 3))
    ((MONKEY-ID 3) (STARTING-ITEMS (74)) (FORMULA + 3) (DIVISIBLE-BY 17)
     (THROW 0 1))))





;; fell into trap naming monkey-round as round
;; then tried round value told was not a vector??
(defun worry-less (n)
  (truncate (/ n 3)))




(defmacro monkey-formula! ()
  `(progn
     (let ((op (second (assoc 'formula monkey)))
	   (value (third (assoc 'formula monkey))))
       ;;(format t "operator op = ~a  : value = ~a ~%" op value)
       (cond
	 ((and (eq op '+) (eq value 'old))
	  (setq worry (+ worry worry)))	 
	 ((eq op '+)
	  (setq worry (+ worry value)))
	 ((and (eq op '*) (eq value 'old))
	  (setq worry (* worry worry)))
	 ((eq op '*)
	  (setq worry (* worry value)))
	 (t (error (format nil "op ~a should be plus + or multiply * " op)))))))




(defmacro monkey-worry! ()
  `(progn
     (when worryless
       (setq worry (truncate (/ worry 3))))))


(defmacro monkey-div! ()
  `(progn
     (let ((divisible-by (second (assoc 'divisible-by monkey)))
	   (true-throw (second (assoc 'throw monkey)))
	   (false-throw (third (assoc 'throw monkey))))
       ;;
       (if (zerop (mod worry divisible-by))
	   (progn ;; throw to true-throw
	     (let* ((catcher (aref monkees true-throw)))
	       (let ((new-monkey  (list (assoc 'monkey-id catcher)
					(list 'starting-items
					      (append (second (assoc 'starting-items catcher))
						      (list worry)))
					(assoc 'formula catcher)
					(assoc 'divisible-by catcher)
					(assoc 'throw catcher))))
		 (setf (aref monkees true-throw) new-monkey))))
	   (progn ;; throw to false throw
	     (let* ((catcher (aref monkees false-throw)))
	       (let ((new-monkey 
		       (list (assoc 'monkey-id catcher)
			     (list 'starting-items
				   (append (second (assoc 'starting-items catcher))
					   (list worry)))
			     (assoc 'formula catcher)
			     (assoc 'divisible-by catcher)
			     (assoc 'throw catcher))))
		 (setf (aref monkees false-throw) new-monkey))))))))





(defmacro monkey-report! ()
  `(progn
     ;; show user monkeys
     (format t "monkeys =~A~%" monkees)

     ;; show monkey inspect
     (format t "monkey-inspect ~A~%" monkey-inspect)

     ;; compute monkey-ness
     (let ((h (sort (coerce monkey-inspect 'list) #'>)))
       (format t "top ape = ~a ~%" (first h))
       (format t "second ape = ~a ~%" (second h))      
       (format t "product of shenanigans = ~a ~%"
	       (* (first h)
		  (second h))))
     ))


(defmacro monkey-heart! ()
  `(progn
     ;; inspecting an item - increase monkey-inspect slot
     ;; add 1 to its slot
     ;; hopefully its zero ' d 
     (incf (aref monkey-inspect i))

     ;; << ---- formula macro ------ >>
     (monkey-formula!)
     
     ;; <<------ worry macro ----------- >>
     (monkey-worry!)

     ;; <<------ divisibility macro -------->>
     (monkey-div!)
     ))

(defmacro monkey-hello! ()
  `(progn
     ;;(format t "~%~%Monkey ~a:~%" i)
     ;;(format t "monkey ~a -> ~a ~%" i monkey)
     t
     ))

;; zero out this monkey 's starting items
(defmacro monkey-clear-items! ()
  `(progn
     (let ((new-monkey  (list (assoc 'monkey-id monkey)
			      (list 'starting-items '())
			      (assoc 'formula monkey)
			      (assoc 'divisible-by monkey)
			      (assoc 'throw monkey))))
       (setf (aref monkees i) new-monkey))))




;; every time monkey inspects , we increment it's monkey ness
;; 10,000 rounds now ... with no worry division ...
(defun monkey-round (monkees n-rounds worryless)
  (let ((nth-round 1)
	(monkey-inspect (make-array (length monkees))))
    (loop while (<= nth-round n-rounds) do
	   (format t "~%~%NTH-ROUND ~a ~%" nth-round)
	   (let ((i 0))
	     (loop while (< i (length monkees)) do
		    ;;(format t "monkey i variable = ~a ~%" i)
		    
		    (let ((monkey (aref monkees i)))
		      ;; <<--------- monkey hello --------------->
		      (monkey-hello!)
		      
		      (let ((items (second (assoc 'starting-items monkey))))
			(loop while (not (null items)) do
			       
			       (let* ((item (car items))
				      (worry item))
				 ;;(format t "~%~%processing item ~a ~%" item)
				 
				 ;; <<---------- monkey heart ------------->>
				 (monkey-heart!)
				 )
			       ;; next item -- match while loop
			       (setq items (cdr items))))

		      ;; <<----------- monkey zero ---------------->>
		      (monkey-clear-items!)
		      
		      ;; next monkey
		      (setq i (+ i 1))
		      ;; done
		      ;; for each monkey
		      )))
	   ;; increment round counter
	   (setq nth-round (+ nth-round 1)))
    ;;<<-------- monkey-report macro---------->>
    (monkey-report!)))






#|
exxample one
20 runs 
|#
(defun example-1 ()
  (monkey-round monkees2 20 t))



#|
task one

monkees

|#
(defun task-1 ()
  (monkey-round monkees 20 t))

#|
task 2

10,000  or 10k runs
nil meaning we do not wish to divide down worry by 3
then truncate
or round to nearest integer
|#
(defun  task-2 ()
  (monkey-round monkees 10000 nil))












