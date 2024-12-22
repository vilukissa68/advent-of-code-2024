;;; day6.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;  Description
;;; Code:

(load-file "./day4.el")

(defun print-array (arr)
  "Print 2D array ARR."
  (mapc (lambda (x) (message "%s " (mapcar 'char-to-string x))) arr))


(defun get-coords (mat elem)
  "Find all coordinates for instances of ELEM in MAT."
  (let ((found-coords '()))
    (dotimes (y (length mat))
      (let ((line (aref mat y)))
	(dotimes (x (length line))
	  (when (string= (char-to-string (aref line x)) elem)
	    (push (list y x) found-coords)))))
    (nreverse found-coords)))


(defun set-matrix-value (matrix y x value)
  "Set the value at X and Y in MATRIX to VALUE."
  (aset (aref matrix y) x value))

(defun move (dir pos mat blocks)
  "Move guard to the given DIR on MAT. If move into BLOCKS fail."
  (let* ((y (car pos))
	 (x (cadr pos))
	 (new-pos (cond
		   ((string= "u" dir) (list (- y 1) x))
		   ((string= "d" dir) (list (+ y 1) x))
		   ((string= "l" dir) (list y (- x 1)))
		   ((string= "r" dir) (list y (+ x 1)))
		   (t pos))))
    (if (cl-member new-pos blocks :test #'equal)
	(cl-values nil mat)  ; If the new position is in blocks, return mat and nil.
      (progn
	(set-matrix-value mat y x ?X)
	(set-matrix-value mat (car new-pos) (cadr new-pos) ?^)
	(cl-values t mat)))))

(defun turn-right (dir)
  "Return new DIR of movement after a right turn."
  (cond
   ((string= "u" dir) "r")
   ((string= "d" dir) "l")
   ((string= "l" dir) "u")
   ((string= "r" dir) "d")
   ))

(defun process-mat (mat blocks)
  "Find way out of MAT from START while turning on BLOCKS."
  (let* ((current-pos (car (get-coords mat "^")))
	 (steps 0)
	 (dir "u"))
    (catch 'loop-break
      (while t
	(let (result new-pos)
	  ;; Perform the move
	  (cl-multiple-value-bind (success new-mat)
	      (move dir current-pos mat blocks)
	    (setq result success)
	    (setq mat new-mat)
	    (when success
	      (setq new-pos (car (get-coords mat "^")))))
	  ;; If move was successful
	  (if result
	      (setq steps (1+ steps)
		    current-pos new-pos)
	    ;; If blocked, turn right
	    (setq dir (turn-right dir)))
	  ;; Break condition
	  (when (or (<= (car current-pos) 0)
		    (<= (cadr current-pos) 0)
		    (>= (car current-pos) (- (length mat) 1))
		    (>= (cadr current-pos) (- (length (aref mat 0)) 1)))
	    (throw 'loop-break steps)))))
    (+ 1 (length (get-coords mat "X")))))


(defun day6-1 ()
  "Solution for Day 4 part 1."
  (interactive)
  (let* ((input (s-trim (read-string "Enter input: ")))
	 (lines (vconcat (mapcar 'string-to-vector (s-split "\n" input))))
	 (blocks (get-coords lines "#"))
	 (results (process-mat lines blocks))
	 )
    (message "Result: %d" results)))


(provide 'day6)
;;; day6.el ends here
