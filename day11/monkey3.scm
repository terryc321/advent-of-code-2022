
#|

PART DUO

now worry no longer divided by 3


|#

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


;;
;; is a prime factorisation of a number , smaller than processing the actual number?
;;
;; our worry will now be
;; (factor XXXXXXX) -> prime factors
;;
;; divisible ??
;; what do we know about primality and if a number is divisible by another
;; in other words 
;;      given a number with prime factors p1 p2 p3 p4 p5 ...
;; is this number divisible by a smaller number with prime factors ?
;;     
;; when we add to prime number , we must in general compute and refactor that number correct ?
;; 

(define  working-set (monkees))

;; zero 'd out monkey-inspect counters
(define monkey-inspect (make-vector (vector-length working-set) 0))

#|
  (let ((nth-round 1)
	  (monkey-inspect (make-vector (* 2 (vector-length monkees)) 0)))
      (while (<= nth-round 10000)

	(format #t "~%~%NTH-ROUND ~a ~%" nth-round)
|#
;; initial round 1 
(define nth-round 1)

;; for each monkey in working-set do this ...
#|
    (let ((i 0))
      (while (< i (vector-length monkees))
	;;(< i 8) ;; <-- change this for larger problem 
	(let ((monkey (vector-ref monkees i)))
|#
(define cycle (lambda () #t))


#|
for this particular monkey
say monkey id 0
|#

(define monkey (vector-ref working-set 0))

(define items (assoc 'starting-items monkey))

(define p (car (second items)))

(define h (make-hash-table))

(hashq-set! h 'foo "bar")

h

(hash-ref h 'foo)
#f

(hashq-ref h 'foo)
"bar"


;; apply this to every element in hash ?
(hash-fold (lambda (key value seed) (+ 1 seed)) 0 h)

;; count number of string value elements in hash
(hash-count (lambda (key value) (string? value)) h)

(hash-count (const #t) h)

;; hash q for values ?
(hashq-set! h 5 45)
(hashq-ref h 5)

;; returns key .value pair if found , else #f false
(hashq-get-handle h 'foo)






