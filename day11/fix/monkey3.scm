
#|

PART DUO

now worry no longer divided by 3

|#

(chdir "/home/terry/advent-of-code/2022")

(chdir "./day11/fix")

;; check 
(getcwd)

;; setup guile so it loads in current directory ?
;; Enter `,help' for help.
;; scheme@(guile-user)> (chdir "/home/terry/advent-of-code/2022/day11")
;; scheme@(guile-user)> (getcwd)
;; $5 = "/home/terry/advent-of-code/2022/day11"
;; scheme@(guile-user)> 

(use-modules (ice-9 slib))

(require 'factor)


(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(define pp pretty-print)

;; lists
(use-modules (srfi srfi-1))

;; prime factorize - from slib
;; PRIME FACTOREd number
(define pf (lambda (n)
	     `(pfnum ,(factor n))))


;; convert starting items to prime factored numbers
;; see if make any difference

;; just a quick dirty check , doesnt actually
;; perform an in-depth check to see if num is actually
;; prime factored
(define pf-num? (lambda (xs)
		  (and (pair? xs)
		       (pair? (cdr xs))
		       (pair? (car (cdr xs)))
		       (eq? (car xs) 'pfnum))))
		       


(define (monkees) (list->vector `(((monkey-id 0)
				   (starting-items ,(map pf '(71 56 50 73)))
				   (formula * 11)
				   (divisible-by 13)
				   (throw 1 7))
				  ((monkey-id 1)
				   (starting-items ,(map pf '(70 89 82)))
				   (formula + 1)
				   (divisible-by 7)
				   (throw 3 6))
				  ((monkey-id 2)
				   (starting-items ,(map pf '(52 95)))
				   (formula * old)
				   (divisible-by 3)
				   (throw 5 4))
				  ((monkey-id 3)
				   (starting-items ,(map pf '(94 64 69 87 70)))
				   (formula + 2)
				   (divisible-by 19)
				   (throw 2 6))
				  ((monkey-id 4)
				   (starting-items ,(map pf '(98 72 98 53 97 51)))
				   (formula + 6)
				   (divisible-by 5)
				   (throw 0 5))
				  ((monkey-id 5)
				   (starting-items ,(map pf '(79)))
				   (formula + 7)
				   (divisible-by 2)
				   (throw 7 0))
				  ((monkey-id 6)
				   (starting-items ,(map pf '(77 55 63 93 66 90 88 71)))
				   (formula * 7)
				   (divisible-by 11)
				   (throw 2 4))
				  ((monkey-id 7)
				   (starting-items ,(map pf '(54 97 87 70 59 82 59)))
				   (formula + 8)
				   (divisible-by 17)
				   (throw 1 3)))))



(define (monkees2) (list->vector `(((monkey-id 0)
				   (starting-items ,(map pf '(79 98)))
				   (formula * 19)
				   (divisible-by 23)
				   (throw 2 3))
				  ((monkey-id 1)
				   (starting-items ,(map pf '(54 65 75 74)))
				   (formula + 6)
				   (divisible-by 19)
				   (throw 2 0))
				  ((monkey-id 2)
				   (starting-items ,(map pf '(79 60 97)))
				   (formula * old)
				   (divisible-by 13)
				   (throw 1 3))
				  ((monkey-id 3)
				   (starting-items ,(map pf '(74)))
				   (formula + 3)
				   (divisible-by 17)
				   (throw 0 1)))))


;; fell into trap naming monkey-round as round
;; then tried round value told was not a vector??
(define worry-less
  (lambda (n)
    (truncate (/ n 3))
    ))



;; every time monkey inspects , we increment it's monkey ness
;; 10,000 rounds now ... with no worry division ...
(define monkey-round2
  (lambda (monkees)
        
    (let ((nth-round 1)
	  (monkey-inspect (make-vector (vector-length monkees) 0)))
      (while (<= nth-round 10000)

	(format #t "~%~%NTH-ROUND ~a ~%" nth-round)
	
    (let ((i 0))
      (while (< i (vector-length monkees))
	;;(< i 8) ;; <-- change this for larger problem 
	(let ((monkey (vector-ref monkees i)))
	  
	  ;(format #t "~%~%Monkey ~a:~%" i)
	  ;(format #t "monkey ~a -> ~a ~%" i monkey)

	  ;; hopefully we do not need to process items given to ourselves
	  ;; like carpet moving from under us ...
	  ;; no monkey throws an object to itself
	  ;; so once all items processed , we can set that monkey's items to empty list
	  (let ((items (second (assoc 'starting-items monkey))))
	    (while (not (null? items))
	      (let* ((item (car items))
		     (worry item))
		;;(format #t "~%~%processing item ~a ~%" item)

		;; inspecting an item - increase monkey-inspect slot
		;; add 1 to its slot
		;; hopefully its zero ' d 
		(vector-set! monkey-inspect i (+ 1 (vector-ref monkey-inspect i)))
		
		;; apply formula
		(let ((op (second (assoc 'formula monkey)))
		      (value (third (assoc 'formula monkey))))
		  ;;(format #t "operator op = ~a  : value = ~a ~%" op value)
		  (cond
		   ((eq? op '+)
		    (if (eq? value 'old)
			(begin
			  ;; adding itself to itself same as doubling by 2
			  
			  ;;(format #t "new = old + old ~%")
			  ;;(set! worry (+ worry worry))
			  (set! worry
				(cons 'pfnum (cons 2 (second worry))))
			  
			  )
			(begin
			  ;; expand worry , add value , then refactor ?
			  ;;(let ((expansion (apply * (second worry)))

			  ;; compute expanded factor then re-factor?
			  (set! worry (cons 'pfnum
					    (factor (+ value
						       (apply * (second worry))))))
				
			  ;;(format #t "new = old + ~a ~%" value)			  
			  ;;(set! worry (+ worry value))
			  
			  )
			))
		   ((eq? op '*)
		    (if (eq? value 'old)
			(begin

			  ;; square number is a combination of two lists of its prime factors
			  ;; 6 = 2 x 3  (2 3)
			  ;; 9 = 3 x 3  (3 3)
			  ;; 54 = 2 x 3 x 3 x 3   (2 3 3 3)
			  (set! worry
				(cons 'pfnum (append (second worry) (second worry))))
			  			  
			  ;;(format #t "new = old * old ~%")			  
			  ;;(set! worry (* worry worry))
			  
			  )
			(begin
			  ;; multiplying by value same as adding that values prime factors to worries
			  (set! worry
				(cons 'pfnum (append (factor value) (second worry))))
			  
			  ;;(format #t "new = old * ~a ~%" value)			  			  
			  ;;(set! worry (* worry value)))
			
			))
		   (else (error (format #f "op ~a should be plus + or multiply * " op)))))
		
		;; computed worry value
		;; bored monkey
		;;(format #t "worry now = ~a ~%" worry)

		;;(format #t "sanity.line 231 . worry now = [~a] ~%" worry)
		;;(format #t "type check worry ~a ~%" worry)

		
		;;(set! worry (worry-less worry))
		;;(format #t "monkey got bored~%")
		;;(format #t ". worry less now = ~a ~%" worry)
		
		;; divisible by test
		(let ((divisible-by (second (assoc 'divisible-by monkey)))
		      (true-throw (second (assoc 'throw monkey)))
		      (false-throw (third (assoc 'throw monkey))))

		  ;;(format #t "divisible by [~a] ~%" divisible-by)
		  ;;(format #t "true throw [~a] ~%" true-throw)
		  ;;(format #t "false-throw [~a] ~%" false-throw)

		  ;; helpfully all divisible-by are singularly prime
		  ;; so just case search through list on worry to find 
		  (let ((divisibility-result (member divisible-by (second worry))))
		    ;; search if number is divisible by its entry in prime factors ...
		  (if divisibility-result
		      (begin ;; throw to true-throw
			;;(format #t "throwing to monkey ~a ~%" true-throw)

			(let* ((catcher (vector-ref monkees true-throw)))
			  ;;
			  (let ((new-monkey  (list (assoc 'monkey-id catcher)
						   (list 'starting-items
							 (append (second (assoc 'starting-items catcher))
								 (list worry)))
						   (assoc 'formula catcher)
						   (assoc 'divisible-by catcher)
						   (assoc 'throw catcher))))
			    (vector-set! monkees true-throw new-monkey)
			    ;; ...
			    ;;(format #t " monkey ~a -> ~a ~%" true-throw (vector-ref monkees true-throw))
			    
			    )))
		      (begin ;; throw to false throw
			#f
			;;(format #t "throwing to monkey ~a ~%" false-throw)

			(let* ((catcher (vector-ref monkees false-throw)))
			  ;; get monkey throw to , extract all info , build new monkey , install that
			  ;; where catcher monkey was , with item at end of starting items
			  (let ((new-monkey 
					     (list (assoc 'monkey-id catcher)
						   (list 'starting-items
							 (append (second (assoc 'starting-items catcher))
								 (list worry)))
						   (assoc 'formula catcher)
						   (assoc 'divisible-by catcher)
						   (assoc 'throw catcher))))
			    
			    (vector-set! monkees false-throw new-monkey)

			    ;;(format #t " monkey ~a -> ~a ~%" false-throw (vector-ref monkees false-throw))
			    
			    ;;   ...			    
			   ))))))))
		       
		;; next item -- match while loop
		(set! items (cdr items))

		;;
		))

	  
	  ;; zero out this monkey 's starting items
	  (let ((new-monkey  (list (assoc 'monkey-id monkey)
						   (list 'starting-items '())
						   (assoc 'formula monkey)
						   (assoc 'divisible-by monkey)
						   (assoc 'throw monkey))))
	    (vector-set! monkees i new-monkey)
	    ;;(format #t " monkey reset ~% monkey ~a -> ~a ~%" i (vector-ref monkees i))
	    )
	    
	    ;; ...
	    
	  
	;; next monkey
	(set! i (+ i 1))
	;; done
	;; for each monkey
	
    ;; all monkeys have had a turn

    ;; ;; show user monkeys
    ;; (pp monkees)

    ;; ;; show monkey inspect
    ;; (pp monkey-inspect)

    ;; ;; compute monkey-ness
    ;; (let ((h (sort (vector->list monkey-inspect) >)))
    ;;   (format #t "top ape = ~a ~%" (first h))
    ;;   (format #t "second ape = ~a ~%" (second h))      
    ;;   (format #t "product of shenanigans = ~a ~%"
    ;; 	      (* (first h)
    ;; 		 (second h))))

    
    ;; increment round counter
    (set! nth-round (+ nth-round 1))
    )

    ;; show user monkeys
    (pp monkees)

    ;; show monkey inspect
    (pp monkey-inspect)

    ;; compute monkey-ness
    (let ((h (sort (vector->list monkey-inspect) >)))
      (format #t "top ape = ~a ~%" (first h))
      (format #t "second ape = ~a ~%" (second h))      
      (format #t "product of shenanigans = ~a ~%"
	      (* (first h)
		 (second h))))

      

      )))



      
