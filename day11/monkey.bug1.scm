
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

;; lists
(use-modules (srfi srfi-1))

;; readline is for prompt like a repl 
;;(use-modules (ice-9 readline))
(use-modules (ice-9 textual-ports))
;;(chdir "day11")
;; C-c C-d C-d  document at point

;; assuming utf-8 ?
(define (get-lines filename)
  (call-with-input-file filename
    (lambda (port)
      ;;(set-readline-input-port! port)
      ;;(format #t "~a~%" (readline ))
      (let ((line 1)
	    (lines '())
	    (next-line (get-line port)))      
	(while (not (eof-object? next-line))
	  (format #t "~a : [~a] " line next-line)

	  (if (= 0 (string-length next-line))
	      (format #t " <<------ empty !!~%")
	      (format #t "~%"))
	  
	  (set! lines (cons next-line lines))
	  ;; fill up next line
	  (set! next-line (get-line port))
	  (set! line (+ line 1)))
	(reverse lines)))))

;; given a multi-line string - extract required data to make a cheeky monkey

(define (get-monkees filename)
  (let ((lines (list->vector (get-lines filename)))
	(r1 (make-regexp "Monkey ([0-9]+):"))
	(lineno 0)
	(monkees '()))
    (letrec ((next-line (lambda () (set! lineno (+ lineno 1))))
	     (cur-line (lambda () (vector-ref lines lineno)))
	     (get-monkey (lambda (nth-monkey)
			   (let* ((r (make-regexp "Monkey ([0-9]+):"))
				  (rm (regexp-exec r (cur-line)))
				  (str (match:substring rm 1))
				  (monkey-id (string->number str)))
			     (format #t "monkey id = [~a] : ~a ~%" monkey-id (integer? monkey-id))
			     (next-line)
			     (let ((starting-items (map string->number (map match:substring (list-matches "[0-9]+" (cur-line))))))
			       (format #t "starting items = [~a] ~%" starting-items)
			       (next-line)

			       (let* ((r (make-regexp "new = old (.) (.*)"))
				      (m (regexp-exec r (cur-line)))
				      (formula-operator (string->symbol (match:substring m 1)))
				      (formula-value (let ((got (match:substring m 2)))
					       (if (string= got "old")
						     ;; make it a symbolic value
						     (string->symbol got)
						     ;;otherwise set num
						     (string->number got)))))
				 
				 (format #t  "operator = [~a] and value = [~a] ~%" formula-operator formula-value)
				 (next-line)	    
				 
				 (let* ((r (make-regexp "([0-9]+)"))
					(m (regexp-exec r (cur-line)))
					(test-value (string->number (match:substring m 0))))
				   (format #t  "test divisible by [~a] ~%" test-value)				   
				   (next-line)
				   
				   (let* ((r (make-regexp "([0-9]+)"))
					  (m (regexp-exec r (cur-line)))
					  (true-throw-value (string->number (match:substring m 0))))
				     (format #t  "if true throw to monkey [~a] ~%" true-throw-value)	     
				     (next-line)

				     (let* ((r (make-regexp "([0-9]+)"))
					    (m (regexp-exec r (cur-line)))
					    (false-throw-value (string->number (match:substring m 0))))
				       (format #t  "if false throw to monkey [~a] ~%" false-throw-value)     
				       (next-line)

				       (format #t "~%~%")

				       ;; skip empty line				       
				       (when (string= (cur-line) "")
					 (next-line))

				       (format #t "monkey ~a ~%" monkey-id)
				       (format #t "starting items ~a ~%" starting-items)
				       (format #t "formula operator ~a : value ~a ~%" formula-operator formula-value)
				       (format #t "test divisible by ~a ~%" test-value)
				       (format #t "throw true to monkey ~a ~%" true-throw-value)
				       (format #t "throw false to monkey ~a  ~%" false-throw-value)

				       (set! monkees (cons
						      (list (list 'monkey-id monkey-id)
							    (list 'starting-items starting-items)
							    (list 'formula formula-operator formula-value)
							    (list 'divisible-by test-value)
							    (list 'throw true-throw-value false-throw-value))
						      monkees))
				       
				       ;;
				       )))))))))
      

      (get-monkey 0)
      (get-monkey 1)
      (get-monkey 2)
      (get-monkey 3)
      (get-monkey 4)
      (get-monkey 5)
      (get-monkey 6)
      (get-monkey 7)      

      monkees
	;; 
      )))


(define monkees (list->vector (reverse (get-monkees "input"))))

#|
;; pretty printed
(pp monkees)
;; gets us this

(((monkey-id 7)
  (starting-items (54 97 87 70 59 82 59))
  (formula + 8)
  (divisible-by 17)
  (throw 1 3))
 ((monkey-id 6)
  (starting-items (77 55 63 93 66 90 88 71))
  (formula * 7)
  (divisible-by 11)
  (throw 2 4))
 ((monkey-id 5)
  (starting-items (79))
  (formula + 7)
  (divisible-by 2)
  (throw 7 0))
 ((monkey-id 4)
  (starting-items (98 72 98 53 97 51))
  (formula + 6)
  (divisible-by 5)
  (throw 0 5))
 ((monkey-id 3)
  (starting-items (94 64 69 87 70))
  (formula + 2)
  (divisible-by 19)
  (throw 2 6))
 ((monkey-id 2)
  (starting-items (52 95))
  (formula * old)
  (divisible-by 3)
  (throw 5 4))
 ((monkey-id 1)
  (starting-items (70 89 82))
  (formula + 1)
  (divisible-by 7)
  (throw 3 6))
 ((monkey-id 0)
  (starting-items (71 56 50 73))
  (formula * 11)
  (divisible-by 13)
  (throw 1 7)))
|#

(define worry-less
  (lambda (n)
    (round (/ n 3))))


;; complete one round of 
(define round
  (lambda (monkees)

    (let ((i 0))
      (while (< i 8)
	(let ((monkey (vector-ref monkees i)))
	  (format #t "monkey ~a -> ~a ~%" i monkey)

	  (let ((items (second (assoc 'starting-items monkey))))
	    (while (not (null? items))
	      (let* ((item (car items))
		     (worry item))
		(format #t "processing item ~a ~%" item)

		;; apply formula
		(let ((op (second (assoc 'formula monkey)))
		      (value (third (assoc 'formula monkey))))
		  (format #t "operator op = ~a  : value = ~a ~%" op value)
		  (cond
		   ((eq? op '+)
		    (if (eq? value 'old)
			(begin
			  (format #t "new = old + old ~%")
			  (set! worry (+ worry worry))
			  )
			(begin
			  (format #t "new = old + ~a ~%" value)			  
			  (set! worry (+ worry value)))
			))
		   ((eq? op '*)
		    (if (eq? value 'old)
			(begin
			  
			  (format #t "new = old * old ~%")			  
			  (set! worry (* worry worry))
			  )
			(begin
			  (format #t "new = old * ~a ~%" value)			  			  
			  (set! worry (* worry value)))
			))
		   (else (error (format #f "op ~a should be plus + or multiply * " op)))))

		;; computed worry value
		;; bored monkey
		(format #t "worry now = ~a ~%" worry)

		;; not breakpoint thinks its a break from routine loop
		;;(break) ;; breakpoint??

		(format #t "sanity.line 231 . worry now = [~a] ~%" worry)
		
		(set! worry (worry-less worry))
		(format #t "monkey got bored~%")
		(format #t ". worry less now = ~a ~%" worry)

		;; (break) ;; breakpoint ??
		
		
		;; divisible by test
		(let ((divisible-by (second (assoc 'divisible-by monkey)))
		      (true-throw (second (assoc 'throw monkey)))
		      (false-throw (third (assoc 'throw monkey))))

		  (format #t "divisible by [~a] ~%" divisible-by)
		  (format #t "true throw [~a] ~%" true-throw)
		  (format #t "false-throw [~a] ~%" false-throw)
		  
		  (if (= 0 (modulo worry divisible-by))
		      (begin ;; throw to true-throw
			#t
			)
		      (begin ;; throw to false throw
			#f
			))
		  
		  ;; next item
		  (set! items (cdr items))

		  ;;
		  ))))
	  ;; next monkey
	  (set! i (+ i 1))
	  ;; done
	  )))))



