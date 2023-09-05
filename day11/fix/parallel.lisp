
;; programming emacs lisp
;; 

(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Use sudo to save the current buffer."
  (interactive "p")
  (if (and (buffer-file-name) (not (file-writable-p (buffer-file-name))))
      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
    ad-do-it)
    ad-do-it))

able to check if data files exist

now know data files exist - have all data required to move to next step
building a predictor for these

for particular monkey start position and worry level



(defparameter monkees 
  '(((MONKEY-ID 0) (STARTING-ITEMS (71 56 50 73)) (FORMULA * 11) (DIVISIBLE-BY 13)
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

(mapcar (lambda (x y) (list x y)) '(0 1 2 3 4 5 6 7)
	(mapcar (lambda (x) (second (assoc 'starting-items x))) monkees))

((0 (71 56 50 73)) (1 (70 89 82)) (2 (52 95)) (3 (94 64 69 87 70))
 (4 (98 72 98 53 97 51)) (5 (79)) (6 (77 55 63 93 66 90 88 71))
		   (7 (54 97 87 70 59 82 59)))


(let ((ls '()))
(loop for i from 0 to 7 do
  (let ((xs (nth i '((71 56 50 73) (70 89 82) (52 95) (94 64 69 87 70) (98 72 98 53 97 51) (79) (77 55 63 93 66 90 88 71) (54 97 87 70 59 82 59)))))
    (setq ls (append ls (mapcar (lambda (x) (list x i)) xs)))))
  ls)

(let ((before "sbcl --noinform --load \"state.lisp\" --eval \"(run ")
      (after (format nil ")\"~%")))
  (mapcar (lambda (ni) (destructuring-bind (n i) ni
			   (format t "~a ~a ~a ~a" before n i after)))
	  '((71 0) (56 0) (50 0) (73 0) (70 1) (89 1) (82 1) (52 2) (95 2) (94 3) (64 3)
	    (69 3) (87 3) (70 3) (98 4) (72 4) (98 4) (53 4) (97 4) (51 4) (79 5) (77 6)
	    (55 6) (63 6) (93 6) (66 6) (90 6) (88 6) (71 6) (54 7) (97 7) (87 7) (70 7)
	    (59 7) (82 7) (59 7))))

sbcl --noinform --load "state.lisp" --eval "(run  71 0 )"
sbcl --noinform --load "state.lisp" --eval "(run  56 0 )"
sbcl --noinform --load "state.lisp" --eval "(run  50 0 )"
sbcl --noinform --load "state.lisp" --eval "(run  73 0 )"
sbcl --noinform --load "state.lisp" --eval "(run  70 1 )"
sbcl --noinform --load "state.lisp" --eval "(run  89 1 )"
sbcl --noinform --load "state.lisp" --eval "(run  82 1 )"
sbcl --noinform --load "state.lisp" --eval "(run  52 2 )"
sbcl --noinform --load "state.lisp" --eval "(run  95 2 )"
sbcl --noinform --load "state.lisp" --eval "(run  94 3 )"
sbcl --noinform --load "state.lisp" --eval "(run  64 3 )"
sbcl --noinform --load "state.lisp" --eval "(run  69 3 )"
sbcl --noinform --load "state.lisp" --eval "(run  87 3 )"
sbcl --noinform --load "state.lisp" --eval "(run  70 3 )"
sbcl --noinform --load "state.lisp" --eval "(run  98 4 )"
sbcl --noinform --load "state.lisp" --eval "(run  72 4 )"
sbcl --noinform --load "state.lisp" --eval "(run  98 4 )"
sbcl --noinform --load "state.lisp" --eval "(run  53 4 )"
sbcl --noinform --load "state.lisp" --eval "(run  97 4 )"
sbcl --noinform --load "state.lisp" --eval "(run  51 4 )"
sbcl --noinform --load "state.lisp" --eval "(run  79 5 )"
sbcl --noinform --load "state.lisp" --eval "(run  77 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  55 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  63 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  93 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  66 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  90 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  88 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  71 6 )"
sbcl --noinform --load "state.lisp" --eval "(run  54 7 )"
sbcl --noinform --load "state.lisp" --eval "(run  97 7 )"
sbcl --noinform --load "state.lisp" --eval "(run  87 7 )"
sbcl --noinform --load "state.lisp" --eval "(run  70 7 )"
sbcl --noinform --load "state.lisp" --eval "(run  59 7 )"
sbcl --noinform --load "state.lisp" --eval "(run  82 7 )"
sbcl --noinform --load "state.lisp" --eval "(run  59 7 )"


;; need osicat for osicat posix
(ql:quickload :osicat)

  (format t "file found and has size ~a ~%" sz)
  (when (and sz (>= sz 20909))
    (throw 'hold-the-phone-george-i-believe-it-has-already-been-done t)))


;; (let ((before "sbcl --noinform --load \"state.lisp\" --eval \"(run ")
;;       (after (format nil ")\"~%")))

(defun check-data-files-present ()
(mapcar (lambda (ni) (destructuring-bind (n i) ni
		       (let ((p (pathname (concatenate 'string
						       "/home/terry/advent-of-code/2022/day11/fix/dat/"
						       (format nil "~a-~a.dat" n i)))))
			 (let ((sz (ignore-errors (osicat-posix:stat-size (osicat-posix:stat p)))))
			   (if sz
			       (format t "present ~a-~a.dat size = ~a~%" n i sz) 
			       (format t "missing data file ~a ~a" n i))))))
	'((71 0) (56 0) (50 0) (73 0) (70 1) (89 1)
	  (82 1) (52 2) (95 2) (94 3) (64 3)
	  (69 3) (87 3) (70 3) (98 4) (72 4)
	  (98 4) (53 4) (97 4) (51 4) (79 5)
	  (77 6) (55 6) (63 6) (93 6) (66 6)
	  (90 6) (88 6) (71 6) (54 7) (97 7)
	  (87 7) (70 7) (59 7) (82 7) (59 7))))


CHECK-DATA-FILES-PRESENT

(check-data-files-present)
(OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK OK
 OK OK OK OK OK OK OK OK OK OK)

(check-data-files-present)
present 71-0.dat size = 1265
present 56-0.dat size = 1808
present 50-0.dat size = 20909
present 73-0.dat size = 20909
present 70-1.dat size = 1651
present 89-1.dat size = 20909
present 82-1.dat size = 20909
present 52-2.dat size = 20909
present 95-2.dat size = 20909
present 94-3.dat size = 20909
present 64-3.dat size = 2849
present 69-3.dat size = 1651
present 87-3.dat size = 1325
present 70-3.dat size = 20909
present 98-4.dat size = 20909
present 72-4.dat size = 20909
present 98-4.dat size = 20909
present 53-4.dat size = 1334
present 97-4.dat size = 20909
present 51-4.dat size = 20909
present 79-5.dat size = 20909
present 77-6.dat size = 2590
present 55-6.dat size = 20909
present 63-6.dat size = 1672
present 93-6.dat size = 1656
present 66-6.dat size = 2847
present 90-6.dat size = 20909
present 88-6.dat size = 20909
present 71-6.dat size = 1649
present 54-7.dat size = 1591
present 97-7.dat size = 1304
present 87-7.dat size = 20909
present 70-7.dat size = 20909
present 59-7.dat size = 20909
present 82-7.dat size = 2210
present 59-7.dat size = 20909



((71 0) (56 0) (50 0) (73 0) (70 1) (89 1)
	(82 1) (52 2) (95 2) (94 3) (64 3)
	(69 3) (87 3) (70 3) (98 4) (72 4)
	(98 4) (53 4) (97 4) (51 4) (79 5)
	(77 6) (55 6) (63 6) (93 6) (66 6)
	(90 6) (88 6) (71 6) (54 7) (97 7)
	(87 7) (70 7) (59 7) (82 7) (59 7))))


(mapcar (lambda (ni) (destructuring-bind (n i) ni
		       (let ((before "sbcl --noinform --load \"simple-state.lisp\" --eval \"(run ")
			     (after (format nil ")\" | tee dat/~a-~a.dat~%" n i)))
			 (format t "~a ~a ~a ~a" before n i after))))
'(
(70 1)
(64 3)
(69 3)
(87 3)  
(53 4)  
(77 6)
(63 6)
(93 6)
(66 6)
(71 6)
(54 7)
(97 7)
(82 7) 
  ))



sbcl --noinform --load "simple-state.lisp" --eval "(run  70 1 )" | tee dat/70-1.dat &
sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  64 3 )" | tee dat/64-3.dat &
sleep 60
killall sbcl
sleep 10


sbcl --noinform --load "simple-state.lisp" --eval "(run  69 3 )" | tee dat/69-3.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  87 3 )" | tee dat/87-3.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  53 4 )" | tee dat/53-4.dat &
sleep 60
killall sbcl
sleep 10


sbcl --noinform --load "simple-state.lisp" --eval "(run  77 6 )" | tee dat/77-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  63 6 )" | tee dat/63-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  93 6 )" | tee dat/93-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  66 6 )" | tee dat/66-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  71 6 )" | tee dat/71-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  54 7 )" | tee dat/54-7.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  97 7 )" | tee dat/97-7.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  82 7 )" | tee dat/82-7.dat &

sleep 60
killall sbcl
sleep 10



