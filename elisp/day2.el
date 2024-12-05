;;; day2.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun traverse-list-rec (lst)
  (if (or (apply '< lst) (apply '> lst))
      (cond ((eq (length lst) 1) 1)
	    ((<= (abs (- (nth 0 lst) (nth 1 lst))) 3) (traverse-list-rec (cdr lst)))
	    (t 0))
    0))

(defun permute-traverse-list (lst)
  (let ((permutations nil))
    (dotimes (i (length lst) permutations)
      (let ((permutation (append (seq-take lst i) (nthcdr (+ 1 i) lst))))
	(push permutation permutations)))
    (if (> (apply '+ (mapcar #'traverse-list-rec permutations)) 0)
	1 0)))

(defun day2-1 ()
  (interactive)
  (let* ((input (read-string "Enter input: "))
	 (lines (s-split "\n" (s-trim input)))
	 (splits (mapcar (lambda (x)
			   (mapcar #'string-to-number (s-split " " x))) lines))
	 (result (apply '+ (mapcar #'traverse-list-rec splits))))
    (message "The results: %d" result)))

(defun day2-2 ()
  (interactive)
  (let* ((input (read-string "Enter input: "))
	 (lines (s-split "\n" (s-trim input)))
	 (splits (mapcar (lambda (x)
			   (mapcar #'string-to-number (s-split " " (s-trim x))))
			 lines))
	 (result (apply '+ (mapcar #'permute-traverse-list splits))))
    (message "The results: %d" result)))


(provide 'day2)
;;; day2.el ends here
