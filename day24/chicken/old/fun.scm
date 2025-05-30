

(import (chicken pretty-print)) ;; pretty print
(import (chicken format)) ;; format
(import srfi-1) ;; lists
(import srfi-69) ;; hash
;; macros
(import bindings) ;; bind
(import (chicken process-context))
;;(change-directory "day24/chicken")
(import (chicken base)) ;; assert
(import (chicken random))

;; generalized-arrays does not yet compile - chicken 5 - 
(import srfi-179)

;;(import arrays)


;; if get expansion errors not defined at compile time
;; (import-for-syntax (only bindings bind))
;; we can import bind functionality from bindings egg , then use bind in our macro
;; to simplify 

;; er-macro-transformer : explicit macro transformer
;; macro example 
;; (E 0) => (define e0 (expt 5 0))
;; (E n) => (define en (expt 5 n)) where n is an integer

;; (define-syntax E
;;   (er-macro-transformer
;;    (lambda (expr rename compare?)
;;      (let* ((n (car (cdr expr)))
;; 	    (sym (string->symbol (string-append "e" (number->string n))))
;; 	    (%define (rename 'define))
;; 	    (%expt (rename 'expt)))
;;        ;;`(,%define ,sym (,%expt 5 0))
;;        `(define ,sym (,%expt 5 ,n))
;;        ))))

;; ;; (cons 'begin (map (lambda (n) `(E ,n)) (iota 23)))
;; (begin (E 0) (E 1) (E 2) (E 3) (E 4) (E 5) (E 6) (E 7) (E 8) (E 9) (E 10) (E 11) (E 12) (E 13) (E 14) (E 15) (E 16) (E 17) (E 18) (E 19) (E 20) (E 21) (E 22))

(define example-1
  (list
   "#.#####"
   "#.....#"
   "#>....#"
   "#.....#"
   "#...v.#"
   "#.....#"
   "#####.#"))

;; what do when scheme working on does not support the feature want ? like 2d arrays

#|
;; array (1 1) to (11 11) inclusive , if i = j then put 1 else 
(define a (make-array (make-interval #(1 1) #(11 11))
		      (lambda (i j)
                        (if (= i j)
                            (list i j 1)
                            (list i j 0)))))

(define a_ (array-getter a))

(a_ 3 3) ;; => 1
(a_ 2 3) ;; => 0
(a_ 11 0) ;; => is an error ... (a_ 11 0)  (11 0 0)


(define b (make-array (make-interval #(1 1) #(11 11))
		      (lambda (i j)
                        (if (= i j)
                            (list i j 1)
                            (list i j 0)))
		      (array-setter array)))

(define a_ (array-getter a))

(a_ 3 3) ;; => 1
(a_ 2 3) ;; => 0
(a_ 11 0) ;; => is an error ... (a_ 11 0)  (11 0 0)

|#




