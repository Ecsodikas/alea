(in-package #:alea)

(defparameter *one*
  " ----- 
|     |
|  O  |
|     |
 ----- ")

(defparameter *two*
  " ----- 
|   O |
|     |
| O   |
 ----- ")

(defparameter *three*
  " ----- 
|   O |
|  O  |
| O   |
 ----- ")

(defparameter *four*
  " ----- 
| O O |
|     |
| O O |
 ----- ")

(defparameter *five*
  " ----- 
| O O |
|  O  |
| O O |
 ----- ")

(defparameter *six*
  " ----- 
| O O |
| O O |
| O O |
 ----- ")

(defun draw-dice (n &optional always-numeric)
  "Draws ASCII 'art' to the standard output. If ALWAYS-NUMERIC is t, the 1 to 6-sided dice will not be treated differently. Otherwise 1 to 6-sided dice will be drawn with pips instead of numbers.
Standard behaviour:
Draws the given dice roll to standard output. Returns the value of the dice roll N.
Exceptional behaviour:
- If the numbers reach a fairly high value, the numeric representation of the value will break the artwork."
  (if (and (<= 1 n 6) (not always-numeric))
      (draw-six-sided n)
      (draw-n-sided n)))

(defun draw-n-sided (n)
  (format t
	  " -------- 
|        |
    ~a   
|        |
 -------- "
	  n)
  n)

(defun draw-six-sided (n)
  (format t "~a" (cond ((= n 1) *one*)
		       ((= n 2) *two*)
		       ((= n 3) *three*)
		       ((= n 4) *four*)
		       ((= n 5) *five*)
		       ((= n 6) *six*)))
  n)
