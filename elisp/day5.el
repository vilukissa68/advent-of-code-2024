;;; day5.el --- Description -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;; Package-Requires: ((dash "2.19.1"))

(require 'dash)
(require 's)

(defun print-hash-table (hash-table)
  "Print all key-value pairs in HASH-TABLE."
  (maphash (lambda (key value)
	     (message "Key: %s, Value: %s" key value))
	   hash-table))

(defun split-list-at (lst sep)
  "split LST at SEP."
  (-split-at (-find-index (lambda (x) (string= x sep)) lst ) lst))

(defun add-to-hash-list (hmap key value)
  "Add VALUE to the list of values associated with KEY in HMAP."
  (let ((existing (gethash key hmap)))
    (if existing
        (puthash key (append existing (list value)) hmap)
      (puthash key (list value) hmap))))

(defun swap-list-elements (lst idx1 idx2)
  "Swap elements at IDX1 and IDX2 in LST."
  (let ((new-lst '())
	(cur-idx 0))
    (dolist (item lst new-lst)
      (cond
       ((= cur-idx idx1) (push (nth idx2 lst) new-lst))
       ((= cur-idx idx2) (push (nth idx1 lst) new-lst))
       (t (push item new-lst)))
      (setq cur-idx (1+ cur-idx)))
    (nreverse new-lst)))

(defun is-valid-update (rule-map update-lst)
  "Check if UPDATE-LST follow rules in RULE-MAP."
  (if (= 1 (length update-lst))
      t ;; Base case, last elem always correct
    (if (-all-p 'identity (mapcar (lambda (x)
				    ;; Check if car is member in any list of cdr,
				    ;; if yes => non-valid update
				    (if (member (car update-lst) (gethash x rule-map))
					nil t))
				  (cdr update-lst)))
	;; If valid so far, move to next cdr
	(is-valid-update rule-map (cdr update-lst))
      nil)))

(defun fix-order (rule-map update-lst cur-idx)
  "Fix order of UPDATE-LST based on rules in RULE-MAP starting from CUR-IDX."
  (if (or (is-valid-update rule-map update-lst) (>= cur-idx (1- (length update-lst))))
      update-lst  ;; Return the list if valid or end of list reached
    (let* ((conflict-values (delq nil (mapcar (lambda (x)
						(if (member (nth cur-idx update-lst) (gethash x rule-map))
						    nil x))
					      (nthcdr (1+ cur-idx) update-lst)))))
      (if (> (length conflict-values) 0)
	  (let* ((conflict-key (car conflict-values))
		 (conflict-idx (and conflict-key (cl-position conflict-key update-lst)))
		 (new-lst (swap-list-elements update-lst cur-idx conflict-idx)))
	    (message "New list: %s" new-lst)
	    (fix-order rule-map new-lst cur-idx))
	(fix-order rule-map update-lst (1+ cur-idx))))))

(defun day5-1 ()
  "Solution for Day 5 part 1."
  (interactive)
  (let* ((input (read-string "Enter input: "))
	 (lines (s-split "\n" (s-trim input)))
	 (sections (split-list-at lines ""))
	 (rule-map (make-hash-table :test 'eql))
	 (rules (car sections))
	 (updates (cadr sections)))
    (message "Rules: %s" rules)
    (message "Updates: %s" updates)
    ;; Fill map with rules
    (mapc (lambda (x)
	    (let* ((pair (s-split "|" x))
	      	   (key (string-to-number (car pair)))
		   (value (string-to-number (cadr pair))))
	      (add-to-hash-list rule-map key value)))
	  rules)
    (print-hash-table rule-map)
    ;; Test updates
    (let* ((result (mapcar (lambda (x)
			     ;; Split update to list
			     (let* ((update-lst (mapcar 'string-to-number (s-split "," x))))
			       (if (is-valid-update rule-map update-lst)
				   (nth (floor (/ (length update-lst) 2)) update-lst) 0)))
			   updates)))
      (message "Results: %s" (apply '+ result)))))

(defun day5-2 ()
  "Solution for Day 5 part 2."
  (interactive)
  (let* ((input (read-string "Enter input: "))
	 (lines (s-split "\n" (s-trim input)))
	 (sections (split-list-at lines ""))
	 (rule-map (make-hash-table :test 'eql))
	 (rules (car sections))
	 (updates (cadr sections)))
    (message "Rules: %s" rules)
    (message "Updates: %s" updates)
    ;; Fill map with rules
    (mapc (lambda (x)
	    (let* ((pair (s-split "|" x))
	      	   (key (string-to-number (car pair)))
		   (value (string-to-number (cadr pair))))
	      (add-to-hash-list rule-map key value)))
	  rules)
    (print-hash-table rule-map)
    ;; Test updates
    (let* ((result (mapcar (lambda (x)
			     ;; Split update to list
			     (let* ((update-lst (mapcar 'string-to-number (s-split "," x))))
			       (if (is-valid-update rule-map update-lst)
				   0 (nth (floor (/ (length update-lst) 2)) (fix-order rule-map update-lst 0)))))
			   updates)))
      (message "Results: %s" result)
      (message "Results: %s" (apply '+ result)))))


(swap-list-elements '(1 2 3 4 5 6) 2 4)
(provide 'day5)
;;; day5.el ends here
