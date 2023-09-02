
#|

complexity rises it is simple to get distracted from what would
otherwise be a simple problem

trace path by the head itself
right 4 . . .
up 1 .
left 3 . . .
...

get path  '((1 1)(2 1)(3 1)(4 1) ...)

then tail or 2nd item simply has to figure out where it should be
assume everybody starts at a nominal x=1 y=1  '(1 1) position

simply a reduction rotate left ? map reduce?

|#


(define (follow pos1 pos2 pos3)
  (let ((hx (first pos1))
	 (hy (second pos1))
	 (hx2 (first pos2))
	 (hy2 (second pos2))
	 (tx (first pos3))
	 (ty (second pos3)))
  (format #t "follow : hx hy ~a ~a : hx2 hy2 ~a ~a : tx ty ~a ~a ~%" hx hy hx2 hy2 tx ty)
  (let* ((tx2 tx)
	 (ty2 ty)
	 (dx (- hx2 tx2))
	 (dy (- hy2 ty2)))
    (cond
     ((or (= hx hx2 tx) ;; aligned either vertically or horizontally
	  (= hy hy2 ty))
      (cond
       ((= dx 2)  (list (+ tx 1) ty)) 
       ((= dx -2) (list (- tx 1) ty)) 
       ((= dx 1)  (list tx ty))
       ((= dx 0)  (list tx ty))
       ((= dx -1) (list tx ty))
       (#t (error (list (format #f "illegal delta dx[~a]" dx) 'other-parameters hx hy tx ty dx dy))))
      (cond
       ((= dy 2)  (list tx (+ ty 1))) 
       ((= dy -2) (list tx (- ty 1))) 
       ((= dy 1)  (list tx ty))
       ((= dy 0)  (list tx ty))
       ((= dy -1) (list tx ty))
       (#t (error (list (format #f "illegal delta dy[~a]" dy) 'other-parameters hx hy tx ty dx dy)))))
     (else
      (cond  ;; must be diagonal moves
       ((and (= dx 1)(= dy 2)) (list (+ tx 1) (+ ty 1)))
       ((and (= dx 2)(= dy 1)) (list (+ tx 1) (+ ty 1)))
       ((and (= dx 1)(= dy -2)) (list (+ tx 1) (- ty 1)))
       ((and (= dx 2)(= dy -1)) (list (+ tx 1) (- ty 1)))
       ((and (= dx -1)(= dy 2)) (list (- tx 1) (+ ty 1)))
       ((and (= dx -2)(= dy 1)) (list (- tx 1) (+ ty 1)))
       ((and (= dx -1)(= dy -2)) (list (- tx 1)(- ty 1)))
       ((and (= dx -2)(= dy -1)) (list (- tx 1)(- ty 1)))
       (#t (list tx ty))))))))


(define (expect title a b)
  (let ((ok (equal? a b)))
    (if ok
	ok
	(format #t "failed [~a] : ~a : ~a~%" title a b))))
	

(define (test-1)  (expect "test-1" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-2)  (expect "test-2" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-3)  (expect "test-3" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-4)  (expect "test-4" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-11)  (expect "test-11" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-12)  (expect "test-12" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-13)  (expect "test-13" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-14)  (expect "test-14" '(3 3) (follow '(3 3) '(4 3) '(2 3))))
;; (define (test-21)  (expect "test-21" '(3 3) (follow '(3 3) '(4 3) '(2 3))))



;; some unit tests
(define (run-tests)
  (test-1)
  ;; (test-2)
  ;; (test-3)
  ;; (test-4)
  ;; (test-11)
  ;; (test-12)
  ;; (test-13)
  ;; (test-14)
  ;; (test-21)
  )




