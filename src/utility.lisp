(in-package #:alea)

(defun flatten (l)
  "Thanks to stack overflow: https://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp
This bad boy flattens a list, and I didn't want to get the alexandria dependency just for this one function."
  (cond ((null l) nil)
	((atom l) (list l))
	(t (loop for a in l appending (flatten a)))))
