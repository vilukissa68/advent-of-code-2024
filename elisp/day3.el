;;; day3.el --- Description -*- lexical-binding wj wj: t; -*-
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(setq-local sample-input-1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(setq-local sample-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(3,3)mul(32,64](mul(11,8)undo()?mul(8,5)")
(setq-local sample-input-3 "_000_don't()XXXdo()YYYYdo()YYYYdon't()XXXdon't()XXXXXXdo()YYYYYdon't()XXXX")
(setq-local day3-1-operation-regex "mul(\\([0-9]\\{1,3\\},[0-9]\\{1,3\\}\\))")
(setq-local day3-2-operation-regex "mul(\\([0-9]\\{1,3\\},[0-9]\\{1,3\\}\\))")

(defun remove-line-breaks (input)
  "Remove all line breaks from the given INPUT string."
  (replace-regexp-in-string "\n" "" input))

(defun multiply-or-zero (numbers)
  "Multiply the numbers in the list. Return 0 if the list is empty."
  (if (null numbers)
      0
    (apply '* numbers)))

(defun capture-substrings-between-do-and-dont (input)
  "Capture all substrings between 'do()' and 'don't' in the given INPUT string."
  (let ((pos 0)
        (matches nil))
    (while (string-match "do()\\(.*?\\)don't()" input pos)
      (push (match-string 0 input) matches)
      (setq pos (match-end 0)))  ;; Move position after the last match
    (nreverse matches)))  ;; Reverse the list since we used 'push'

(defun find-regex-patterns (regexp str)
  "Find all matches of REGEXP in STR and return them as a list."
  (let ((pos 0)
        (matches nil))
    (while (string-match regexp str pos)
      (push (match-string 0 str) matches)
      (setq pos (match-end 0)))
    (nreverse matches)))

(defun day3-1 ()
  (interactive)
  (let* (
	 (input (read-string "Enter input: "))
	 (operations (find-regex-patterns day3-1-operation-regex input))
	 (mult-res (mapcar (lambda (x)
			     (apply '*
				    (mapcar #'string-to-number
					    (find-regex-patterns "\\([0-9]\\{1,3\\}\\)" x)))) operations))
	 (res (apply '+ mult-res)))
    (message "Result :%s" res)
    ))

(defun day3-2 ()
  (interactive)
  (let* (
	 (input (concat "do()" (remove-line-breaks (read-string "Enter input: ")) "don't()"))
	 (substrings (string-join (capture-substrings-between-do-and-dont input)))
	 (operations (find-regex-patterns day3-2-operation-regex substrings))
	 (mult-res (mapcar (lambda (x)
			     (multiply-or-zero
			      (mapcar #'string-to-number
				      (find-regex-patterns "\\([0-9]\\{1,3\\}\\)" x)))) operations))
	 (res (apply '+ mult-res))
	 )

    (message "input :%s" input)
    (message "substrings :%s" substrings)
    (message "operations :%s" operations)
    (message "mult-res :%s" mult-res)
    (message "Result :%s" res)
    ))

(message "Result: %s" (find-regex-patterns day3-2-operation-regex (concat "do()" sample-input-2)))
(capture-substrings-between-do-and-dont (concat "do()" sample-input-3 "don't()"))

(provide 'day3)
;;; day3.el ends here
