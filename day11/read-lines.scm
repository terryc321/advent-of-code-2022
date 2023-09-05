


(use-modules (ice-9 format))

;; readline is for prompt like a repl 
;;(use-modules (ice-9 readline))

(use-modules (ice-9 textual-ports))

;;(chdir "day11")

;; C-c C-d C-d  document at point


;; assuming utf-8 ?
(call-with-input-file "input"
  (lambda (port)
    ;;(set-readline-input-port! port)
    ;;(format #t "~a~%" (readline ))
    (let ((line 1)
	  (next-line (get-line port)))      
    (while (not (eof-object? next-line))
      (format #t "~a : [~a]~%" line next-line)
      (set! next-line (get-line port))
      (set! line (+ line 1))))))
   

(call-with-input-file "input"
  (lambda (port)
    (format #t "read = ~a ~%" (read port))))


