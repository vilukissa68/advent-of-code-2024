;;; day7.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun generate-permutations (functions length)
  "Generate all permutations of FUNCTIONS with LENGTH."
  (if (= length 0)
      (list '())
    (let (result)
      (dolist (fn functions)
	(dolist (perm (generate-permutations functions (1- length)))
	  (push (cons fn perm) result)))
      result)))

(defun apply-functions-to-values (functions values)
  "Apply all the FUNCTIONS in a list between VALUES."
  (if (or (null functions) (= (length values) 1))
      (car values)
    (let* ((result (funcall (car functions) (car values) (cadr values)))
	   (remaining-values (cons result (cddr values))))
      (apply-functions-to-values (cdr functions) remaining-values))))


(defun concatenate-nums (a b)
  "Concatenate numbers A and B."
  (string-to-number (concat
		     (number-to-string a)
		     (number-to-string b))))

(defun day7-2 ()
  "Solution for Day 7 part 2."
  (interactive)
  (let* ((input (s-trim (read-string "Enter input: ")))
	 (lines (s-split "\n" input))
	 (result (mapcar (lambda (line)
			   (let* ((split (s-split ": " line))
				  (target (string-to-number (car split)))
				  (numbers (mapcar 'string-to-number (s-split " " (cadr split))))
				  (no-operations (- (length numbers) 1))
				  (operations (generate-permutations '(+ * concatenate-nums) no-operations))
				  (results (mapcar (lambda (operation)
						     (apply-functions-to-values operation numbers))
						   operations)))
			     (if (member target results)
				 target 0)))
			 lines)))
    (message "Result: %d" (apply '+ result))))


(defun day7-1 ()
  "Solution for Day 7 part 1."
  (interactive)
  (let* ((input (s-trim (read-string "Enter input: ")))
	 (lines (s-split "\n" input))
	 (result (mapcar (lambda (line)
			   (let* ((split (s-split ": " line))
				  (target (string-to-number (car split)))
				  (numbers (mapcar 'string-to-number (s-split " " (cadr split))))
				  (no-operations (- (length numbers) 1))
				  (operations (generate-permutations '(+ *) no-operations))
				  (results (mapcar (lambda (operation)
						     (apply-functions-to-values operation numbers))
						   operations)))
			     (if (member target results)
				 target 0)))
			 lines)))
    (message "Result: %d" (apply '+ result))))

(provide 'day7)
