;;; day4.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun print-matrix (mat)
  "Print 2D matrix MAT."
  (mapc (lambda (x) (message "%s" x)) mat))

(defun pad-matrix-strings (matrix pad-value pad-rows pad-cols)
  "Pad a 2D MATRIX of strings with PAD-VALUE.
PAD-ROWS is the number of rows to add at the top and bottom.
PAD-COLS is the number of columns to add to the left and right."
  (let* ((row-length (length (car matrix)))  ;; Length of each row
	 (padded-row (make-string (+ row-length (* 2 pad-cols)) pad-value))
	 (top-padding (make-list pad-rows padded-row))
	 (bottom-padding (make-list pad-rows padded-row))
	 (padded-matrix
	  (mapcar (lambda (row)
		    (concat (make-string pad-cols pad-value) ;; Left padding
			    row
			    (make-string pad-cols pad-value))) ;; Right padding
		  matrix)))
    (append top-padding padded-matrix bottom-padding)))

(defun get-elem (mat y x)
  "Get element by coordinate X Y from MAT."
  (char-to-string (aref (nth y mat) x)))

(defun is-elem (mat predicate y x)
  "Check if coord X Y in MAT corresponds with PREDICATE."
  (eq predicate (get-elem mat y x)))

(defun get-pattern-horizontal (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (get-elem mat y x)
    (concat (get-elem mat y x) (get-pattern-horizontal mat y (+ x 1) (- len 1)))))

(defun get-pattern-horizontal-reverse (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (get-elem mat y x)
    (concat (get-elem mat y x) (get-pattern-horizontal-reverse mat y (- x 1) (- len 1)))))

(defun get-pattern-vertical (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (get-elem mat y x)
    (concat (get-elem mat y x) (get-pattern-vertical mat (+ y 1) x (- len 1)))))

(defun get-pattern-vertical-reverse (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (get-elem mat y x)
    (concat (get-elem mat y x) (get-pattern-vertical-reverse mat (- y 1) x (- len 1)))))

(defun get-pattern-tl-br (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (get-elem mat y x)
    (concat (get-elem mat y x) (get-pattern-tl-br mat (- y 1) (+ x 1) (- len 1)))))

(defun get-pattern-br-tl (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (get-elem mat y x)
    (concat (get-elem mat y x) (get-pattern-br-tl mat (+ y 1) (- x 1) (- len 1)))))

(defun get-pattern-bl-tr (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (char-to-string (get-elem mat y x))
    (concat (get-elem mat y x) (get-pattern-bl-tr mat (+ y 1) (+ x 1) (- len 1)))))

(defun get-pattern-tr-bl (mat y x len)
  "Extact pattern of LEN length from MAT starting from coord X Y.."
  (if (= len 1)
      (char-to-string (get-elem mat y x))
    (concat (get-elem mat y x) (get-pattern-tr-bl mat (- y 1) (- x 1) (- len 1)))))

(defun day4-1 ()
  "Solution for Day 1 part 1."
  (interactive)
  (let* ((input (s-trim (read-string "Enter input: ")))
	 (lines (s-split "\n" input))
         (padded-mat (pad-matrix-strings lines ?0 4 4))
	 (patterns '()))
    (let ((row 0))
      (dolist (line padded-mat)
	(let ((col 0))
	  (dotimes (i (length line))
	    (let ((element (aref line i)))
	      (if (= element ?X)
		  (progn
		    (setq patterns (append patterns (list (get-pattern-horizontal padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-horizontal-reverse padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-vertical padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-vertical-reverse padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-tl-br padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-br-tl padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-bl-tr padded-mat row col 4))))
		    (setq patterns (append patterns (list (get-pattern-tr-bl padded-mat row col 4))))
		    ))
	      (setq col (1+ col))))
	  (setq row (1+ row))))
      (let ((result (length (seq-filter (lambda (x) (string= "XMAS" x)) patterns))))
	(message "Results %d" result))
      )))

(defun day4-2 ()
  "Solution for Day 1 part 2."
  (interactive)
  (let* ((input (s-trim (read-string "Enter input: ")))
	 (lines (s-split "\n" input))
         (padded-mat (pad-matrix-strings lines ?0 4 4))
	 (patterns '()))
    (let ((row 0))
      (dolist (line padded-mat)
	(let ((col 0))
	  (dotimes (i (length line))
	    (let ((element (aref line i)))
	      (if (= element ?A)
		  (progn
		    (setq patterns (append patterns (list (concat (get-elem padded-mat (+ row 1) (+ col 1))
								  (get-elem padded-mat (- row 1) (- col 1))
								  (get-elem padded-mat (+ row 1) (- col 1))
								  (get-elem padded-mat (- row 1) (+ col 1))
								  ))))))
	      (setq col (1+ col))))
	  (setq row (1+ row))))
      (let ((result (length (seq-filter (lambda (x) (or
						     (string= "SMMS" x)
						     (string= "MSSM" x)
						     (string= "SMSM" x)
						     (string= "MSMS" x)))
					patterns))))
	(message "Results %d" result))
      (message "Patterns %s" patterns)
      )))


(provide 'day4)
;;; day4.el ends here
