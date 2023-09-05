
park code here ............


				     
(regexp-exec r "   Monkey 0:  ")
 #("   Monkey 0:  " (3 . 12) (10 . 11))
(match:substring (regexp-exec r "   Monkey 0:  ") 0)
"Monkey 0:"
(match:substring (regexp-exec r "   Monkey 0:  ") 1)
 "0"
string->integer?
(string->number "0")


;; regex R 
(define r (make-regexp "([0-9]+)"))

(regexp-exec r " Starting items: ([0-9]+)(, ([0-9]+))?  ")
(regexp-exec r ".*Starting items:.*([0-9]+).*")
(regexp-exec r "Starting items: 71, 56, 50, 73 ")
(map string->number (map match:substring (list-matches "[0-9]+" "Starting items: 71, 56, 50, 73 ")))

(define m (regexp-exec (make-regexp "new = old (.) ([0-9]+)") "   Operation: new = old + 1 "))

(match:substring m 0)

(match:substring m 1)
(string->symbol (match:substring m 1))
(eq? '+ (string->symbol (match:substring m 1)))

(match:substring m 2)
(string->number (match:substring m 2))


(define r (make-regexp "([0-9]+)"))
(define m (regexp-exec r "  Test: divisible by 7  "))

(match:substring m 0)
(string->number (match:substring m 0))
7


    If true: throw to monkey 3
(define r (make-regexp "([0-9]+)"))
(define m (regexp-exec r "     If true: throw to monkey 3   "))
(match:substring m 0)
(string->number (match:substring m 0))
3


(define r (make-regexp "([0-9]+)"))
(define m (regexp-exec r "  If false: throw to monkey 6   "))
(match:substring m 0)
(string->number (match:substring m 0))


;;----------------------------------------------------------------------

(let* ((r (make-regexp "new = old (.) ([0-9]+)"))
       (m (regexp-exec r "  Operation: new = old * 7 ")))
  m)

 #("  Operation: new = old * 7 " (13 . 26) (23 . 24) (25 . 26))


(let* ((r (make-regexp "new = old (.) ([0-9]+)"))
       (m (regexp-exec r "  Operation: new = old * old ")))
  m)
 #f


   

