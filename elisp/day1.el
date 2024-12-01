;;; day1.el --- Solves AOC2024 day 1.
;;; Commentary:
;;; run by evaluating buffer with M-x eval-buffer and calling day1-x after.
;;; Code:

(defun distance (a b)
  "Get distance score between A and B in part 1."
  (cond ((> a b) (- a b))
	(t (- b a))))

(defun parse-line (line)
  "Construct pair from a LINE by remove whitespace."
  (mapcar #'string-to-number
	  (s-split "   " (s-trim lint))))

(defun unzip (pairs)
  "Separate list of PAIRS in to two lists."
  (let ((list1 (mapcar #'car pairs))
	(list2 (mapcar #'cadr pairs)))
    (list list1 list2)))

(defun similarity-score (a list)
  "Find similarity score for one element A in the left LIST."
  (let ((score (* a (length
		     (seq-filter (lambda (x) (= x a)) list)))))
    score))

(defun day1-1 ()
  "Solution for day 1 part 1."
  (interactive)
  (let* ((input (read-string "Enter input: "))
	 (lines (s-split "\n" (s-trim input)))
	 (pairs (mapcar #'parse-line lines))
	 (unzipped (unzip pairs))
	 (list1 (sort (nth 0 unzipped) #'<))
	 (list2 (sort (nth 1 unzipped) #'<))
	 (result (apply '+ (mapcar #'distance list1 list2))))
    (message "The sum of distances is: %d" result)))

(defun day1-2 ()
  "Solution for day 1 part 2."
  (interactive)
  (let* ((input (read-string "Enter input: "))
	 (lines (s-split "\n" (s-trim input)))
         (pairs (mapcar #'parse-line lines))
         (unzipped (unzip pairs))
         (list1 (nth 0 unzipped))
         (list2 (nth 1 unzipped))
	 (result (apply '+ (mapcar
			    (lambda (x)
			      (let ((score (similarity-score x list2)))
				(message "Element: %d, Score: %d" x score)
				score))
			    list1))))
    (message "The sum of distances is: %d" result)))

(provide 'day1)
