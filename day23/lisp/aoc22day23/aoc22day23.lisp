;;;; aoc22day23.lisp

(sb-ext:restrict-compiler-policy 'debug 3 3)
(sb-ext:restrict-compiler-policy 'safety 3 3)

(in-package #:aoc22day23)

;;; parsing phase

;; each line should be same length
;; read-line chomps newline i presume
;; (parse-file "../../input.txt")
(defun parse-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines '()))
      (catch 'foo
	(loop while t do
	  (let ((line (read-line stream nil 'eof)))
	    (cond
	      ((eq line 'eof) (throw 'foo t))
	      (t (setq lines (cons line lines)))))))
      (setq lines (reverse lines))
      lines)))


;; plain square grid
(defun parse-lines (lines)
  (let* ((width (length (car lines)))
	 (height (length lines))
	 (x 0)
	 (y 0)
	 (grid (make-array (list (+ width 1)(+ height 1)) :initial-element #\.)))    
    (dolist (line lines)
      (incf y)
      (setq x 0)
      (let ((len (length line)))
	;; each line has same length
	(assert (= len width))
	(loop for ch in (coerce line 'list) do
	  (incf x)
	  (assert (or (char= ch #\.)(char= ch #\#)))
	  (setf (aref grid x y) ch))))
    grid))

(defun parse (filename)
  (parse-lines (parse-file filename)))

(defun test-parse ()
  (let ((lines (parse-file "../../input.txt")))
    (format t "lines => ~a~%" lines)
    (parse-lines lines)))

(defun grid-width (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (- width 1)))

(defun grid-height (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (- height 1)))

(defun grid-dimensions (grid)
  (destructuring-bind (width height) (array-dimensions grid)
    (list (- width 1)(- height 1))))

(defun show-textgrid (grid)
  (let ((width (grid-width grid))
	(height (grid-height grid)))
    (loop for y from 1 to height do
      (format t "~%")
      (loop for x from 1 to width do
	(let ((ch (aref grid x y)))
	  (format t "~a" ch))))
    (format t "~%")))

;;; puzzle begins here 
;;; structure to represent an elf , quick refresh 

(defstruct elf
  (id 0)
  (neighbours nil)
  (pos-x 0)
  (pos-y 0)
  (proposed nil))

;; constructor with keyword arguments to override defaults
(defparameter elf nil)
(setq elf (make-elf :pos-x 7))
;; type predicates
(typep elf 'elf)
(type-of elf)

;; selectors
(elf-pos-x elf)
(elf-pos-y elf)

;; change values 
(setf (elf-pos-x elf) 5)
(elf-pos-x elf)

;;; the real work begins

;;; process : string -> list of elfs
(defun process (filename)
  (let* ((grid (parse filename))
	 (width (grid-width grid))
	 (height (grid-height grid))
	 (elfs '())
	 (id 1))
    (loop for y from 1 to height do
      (loop for x from 1 to width do
	(let ((ch (aref grid x y)))
	  (when (char= ch #\#)
	    (setq elfs (cons (make-elf :id id :pos-x x :pos-y (- height y)) elfs))
	    (incf id)))))
    elfs))

;;(length (process "grids/input.txt"))
;;2578 elfs 

(defun show-elfs (elfs)
  (progn
    (mapcar (lambda (e) (format t "~a~%" e)) elfs)
    nil))

;; Orientation +X east , +Y north


#|
simple solution first
for E in elfs
 for E2 in elfs
  if different elfs is E2 in N E S W NE SW NW SE direction 

add-neighbour E E2
a neighbour of E to E2 also E2 has neighbour E

we want to add E2 to neighbours of E , E is modified
look though neighbours of E , if neighbour id same as elf2 id then already been added

|#

;; symmetric relationship neighbour
(defun add-neighbour (elf elf2)
  ;; add E2 to E
  (catch 'already-in
    (loop for neighbour in (elf-neighbours elf) do
			   (when (= (elf-id neighbour) (elf-id elf2))
			     (throw 'already-in t)))
    (setf (elf-neighbours elf) (cons elf2 (elf-neighbours elf))))
  ;; add E to E2
  (catch 'already-in
    (loop for neighbour in (elf-neighbours elf2) do
			   (when (= (elf-id neighbour) (elf-id elf))
			     (throw 'already-in t)))
    (setf (elf-neighbours elf2) (cons elf (elf-neighbours elf2)))))


;; going from src to dest did we go north
(defun has-elf-n (src dest)
  (and (= 0 (- (elf-pos-x dest) (elf-pos-x src)))
       (= 1 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-ne (src dest)
  (and (= 1 (- (elf-pos-x dest) (elf-pos-x src)))
       (= 1 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-e (src dest)
  (and (= 1 (- (elf-pos-x dest) (elf-pos-x src)))
       (= 0 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-se (src dest)
  (and (= 1 (- (elf-pos-x dest) (elf-pos-x src)))
       (= -1 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-s (src dest)
  (and (= 0 (- (elf-pos-x dest) (elf-pos-x src)))
       (= -1 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-sw (src dest)
  (and (= -1 (- (elf-pos-x dest) (elf-pos-x src)))
       (= -1 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-w (src dest)
  (and (= -1 (- (elf-pos-x dest) (elf-pos-x src)))
       (= 0 (- (elf-pos-y dest) (elf-pos-y src)))))

(defun has-elf-nw (src dest)
  (and (= -1 (- (elf-pos-x dest) (elf-pos-x src)))
       (= 1 (- (elf-pos-y dest) (elf-pos-y src)))))

;;(has-elf-n (make-elf :pos-y 1) (make-elf :pos-y 2))
;;(has-elf-e (make-elf :pos-x 1) (make-elf :pos-x 2))





;; If there is no Elf in the N, NE, or NW adjacent positions,
;; the Elf proposes moving north one step.
(defun has-elf-n-ne-nw (elf)
  (catch 'decision
    (loop for elf2 in (elf-neighbours elf) do
      (cond
	((has-elf-n elf elf2) (throw 'decision t))
	((has-elf-ne elf elf2) (throw 'decision t))
	((has-elf-nw elf elf2) (throw 'decision t))))
    nil))

(defun no-elf-n-ne-nw (elf)
  (not (has-elf-n-ne-nw elf)))


;; If there is no Elf in the S, SE, or SW adjacent positions,
;; the Elf proposes moving south one step.
(defun has-elf-s-se-sw (elf)
  (catch 'decision
    (loop for elf2 in (elf-neighbours elf) do
      (cond
	((has-elf-s elf elf2) (throw 'decision t))
	((has-elf-se elf elf2) (throw 'decision t))
	((has-elf-sw elf elf2) (throw 'decision t))))
    nil))

(defun no-elf-s-se-sw (elf)
  (not (has-elf-s-se-sw elf)))



;; If there is no Elf in the W, NW, or SW adjacent positions,
;; the Elf proposes moving west one step.
(defun has-elf-w-nw-sw (elf)
  (catch 'decision
    (loop for elf2 in (elf-neighbours elf) do
      (cond
	((has-elf-w elf elf2) (throw 'decision t))
	((has-elf-nw elf elf2) (throw 'decision t))
	((has-elf-sw elf elf2) (throw 'decision t))))
    nil))

(defun no-elf-w-nw-sw (elf)
  (not (has-elf-w-nw-sw elf)))


;; If there is no Elf in the E, NE, or SE adjacent positions,
;; the Elf proposes moving east one step.
(defun has-elf-e-ne-se (elf)
  (catch 'decision
    (loop for elf2 in (elf-neighbours elf) do
      (cond
	((has-elf-e elf elf2) (throw 'decision t))
	((has-elf-ne elf elf2) (throw 'decision t))
	((has-elf-se elf elf2) (throw 'decision t))))
    nil))

(defun no-elf-e-ne-se (elf)
  (not (has-elf-e-ne-se elf)))


	


;;; First Half of Round
(defun first-half (elfs)
  ;; clear all neighbours 
  (loop for elf in elfs do
    (setf (elf-neighbours elf) nil)
    (setf (elf-proposed elf) nil))
  ;; 
  (loop for elf in elfs do
    (loop for elf2 in elfs do
      (when (/= (elf-id elf) (elf-id elf2)) ;; different elfs	  
	(let ((dx (- (elf-pos-x elf) (elf-pos-x elf2)))
	      (dy (- (elf-pos-y elf) (elf-pos-y elf2))))
	  (cond
	    ((and (= dx 0)(= dy 1)) "n"   (add-neighbour elf elf2))
	    ((and (= dx 1)(= dy 1)) "ne"  (add-neighbour elf elf2))
	    ((and (= dx 1)(= dy 0)) "e"   (add-neighbour elf elf2))
	    ((and (= dx 1)(= dy -1)) "se" (add-neighbour elf elf2))
	    ((and (= dx 0)(= dy -1)) "s"  (add-neighbour elf elf2))
	    ((and (= dx -1)(= dy -1)) "sw"(add-neighbour elf elf2))
	    ((and (= dx -1)(= dy 0)) "w"  (add-neighbour elf elf2))
	    ((and (= dx -1)(= dy 1)) "nw" (add-neighbour elf elf2))
	    ))))
    (format t "elf ~a has ~a neighbours ~%"
	    (elf-id elf) (mapcar #'elf-id (elf-neighbours elf))))
  
  ;; all elfs have found their neighbours 
  ;; compute proposals
  
  (loop for elf in elfs do
    (when (> (length (elf-neighbours elf)) 0)
      (catch 'proposed
	;; If there is no Elf in the N, NE, or NW adjacent positions,
	;; the Elf proposes moving north one step.
	(when (no-elf-n-ne-nw elf)
	  (setf (elf-proposed elf) (list (elf-pos-x elf) (+ 1 (elf-pos-y elf))))
	  (throw 'proposed t))
	
	;; If there is no Elf in the S, SE, or SW adjacent positions,
	;; the Elf proposes moving south one step.
	(when (no-elf-s-se-sw elf)
	  (setf (elf-proposed elf) (list (elf-pos-x elf) (+ -1 (elf-pos-y elf))))
	  (throw 'proposed t))
	
	;; If there is no Elf in the W, NW, or SW adjacent positions,
	;; the Elf proposes moving west one step.
	(when (no-elf-w-nw-sw elf)
	  (setf (elf-proposed elf) (list (+ -1 (elf-pos-x elf)) (elf-pos-y elf)))
	  (throw 'proposed t))	
	
	;; If there is no Elf in the E, NE, or SE adjacent positions,
	;; the Elf proposes moving east one step.
	(when (no-elf-e-ne-se elf)
	  (setf (elf-proposed elf) (list (+ 1 (elf-pos-x elf)) (elf-pos-y elf)))
	  (throw 'proposed t))	

	))) ;; for elfs

  ;; proposals computed 
  (loop for elf in elfs do
    (format t "elf ~a proposes moving to ~a ~%" (elf-id elf) (elf-proposed elf)))
  

  
  );;defun
  
;; test first half
(defun test-first-half ()
  (first-half (process "grids/input.txt")))





	      
	      
	    

	

















