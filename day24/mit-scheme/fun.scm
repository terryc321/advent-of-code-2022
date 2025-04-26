
;; -*- geiser-scheme-implementation: mit -*-
(declare (usual-integrations))

;; start mit-scheme
;; alias mit='rlwrap mit-scheme'
;; then run edwin 
;; otherwise mit scheme will not load scheme.init and no slib
;; no format 


;; do we have slib? ... apparently we do ...
(require 'format)
(format #t "~%~%hello world from slib ~%~%")

;; does slib have destructuring bind or some mechanism pattern match
(require 'pretty-print)
;; pp should now be defined (the pretty printer)

;; try a simple swap macro using explicit renaming

;; assuming (swap a b) that a b are symbols ! otherwise multiple eval problem
;; we also have explicit renaming macro transformer
;; x expression
;; r rename
;; c compare 
(define-syntax swap!
  (er-macro-transformer
   (lambda (x r c)
     (let ((tmp (gensym "tmp"))
	   (a (car (cdr x)))
	   (b (car (cdr (cdr x))))
	   (%set! (r 'set!))
	   (%let (r 'let)))
       `(,%let ((,tmp ,a))
 	       (,%set! ,a ,b)
               (,%set! ,b ,tmp))))))

;; usage of swap! 
(let ((a 1)(b 2))
  (swap! a b)
  (format #t "a swapped with b is ~a ~%"(list 'a a 'b b)))

;; can we try a little more complex macro using something like bind in chicken scheme
;; where we can pull apart x expression of the macro
(defmacro while (con . body)
  (let ((loop (gentemp "loop")))
    `(let ,loop ()
	  (if ,con (begin ,@body (,loop))))))


;; we can evaluate an expression in current environment
https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_14.html
(eval 3 (the-environment))






;; finally can we make a 2d array of some sort
;; ignore out of bounds accesses ?
;; 




;; how do i load something at 

;; do we have 


