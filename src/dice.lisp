(in-package #:alea)

(defun roll-dice (sides)
  "Roll a dice with SIDES sides. 
Standard behaviour:
Returns an integer in the interval [1, SIDES], where SIDES always gets floored.
Exceptional behaviour: 
- If SIDES is 0, the result is also 0.
- If SIDES is less than 0, the result is 0."
  (let ((fsides (floor sides)))
    (cond ((<= fsides 0) 0)
	  (t (1+ (random fsides))))))

(defun roll-dice-range (from to)
  "Roll a dice with TO - FROM + 1 sides. Returns an integer in the interval [FROM, TO].
Standard behaviour:
Returns an integer in the interval [FROM, TO], where both inputs always get floored.
Exceptional behaviour:
- If TO < FROM, the result is 0.
- The interval is allowed to contain negative numbers as long as FROM < TO."
  (let* ((ffrom (floor from))
	 (fto (floor to))
	 (f-1 (1- ffrom)))
    (cond ((< fto ffrom) 0)
	  (t (+ (roll-dice (- to f-1)) f-1)))))

(defun roll-n-dice (n sides)
  "Roll N dice with SIDES sides.
Standard behaviour:
Returns a list with all results from N calls to the (ROLL-DICE SIDES) function.
Exceptional behaviour:
- The same as (ROLL-DICE SIDES), but multiple times.
- If N is less than 1, the empty list is returned."
  (loop for i from 0 below n collect (roll-dice sides)))

(defun roll-n-dice-range (n from to)
  "Roll N dice with TO - FROM + 1 sides.
Standard behaviour:
Returns a list with all results from N calls to the (ROLL-DICE-RANGE FROM TO) function.
Exceptional behaviour:
- The same as (ROLL-DICE-RANGE FROM TO), but multiple times.
- If N is less than 1, the empty list is returned."
  (loop for i from 0 below n collect (roll-dice-range from to)))

(defun count-hits (rolls target)
  "Given a list of dice rolls ROLLS and a target TARGET. Counts the number of occurences of TARGET in ROLLS.
Standard behaviour:
Return the number of occurences of TARGET in ROLLS.
Exceptional behaviour:
- If ROLLS is NIL, 0 is returned."
  (count target rolls))

(defun sum-hits (rolls target)
  "Given a list of dice rolls ROLLS and a target TARGET. Sums the value of all TARGET in ROLLS.
Standard behaviour:
Return the sum of all TARGET in ROLLS.
Exceptional behaviour:
- If ROLLS is NIL, 0 is returned."
  (* target (count-hits rolls target)))

(defun average (rolls)
  "Given a list of dice rolls or a single dice roll ROLLS, calculate the average of all rolls.
Standard behaviour:
Return the average of all rolls in ROLLS.
Exceptional behaviour:
- If ROLLS is NIL, 0 is returned.
- If ROLLS is an integer, ROLLS is returned."
  (if (integerp rolls)
      rolls
      (if rolls
	  (/ (apply '+ rolls) (length rolls))
	  0)))

(defun n-highest (rolls n)
  "Given a list of dice rolls ROLLS, return the N highest rolls of them.
Standard behaviour:
Return the N highest rolls in ROLLS.
Exceptional behaviour:
- If N is bigger than (length ROLLS), ROLLS is returned.
- If ROLLS is NIL, NIL is returned."
  (let ((n-bound (min n (length rolls))))
    (subseq (sort rolls '>) 0 n-bound)))

(defun n-lowest (rolls n)
  "Given a list of dice rolls ROLLS, return the N lowest rolls of them.
Standard behaviour:
Return the N lowest rolls in ROLLS.
Exceptional behaviour:
- If N is bigger than (length ROLLS), ROLLS is returned.
- If ROLLS is NIL, NIL is returned."
  (let ((n-bound (min n (length rolls))))
    (subseq (sort rolls '<) 0 n-bound)))

(defun roll-dice-parse (dice-string &optional unmute)
  "Given a string DICE-STRING in the form 'XDY', where X and Y are positive integers, roll X dice with Y sides.
Those terms can be concatenated via the letter 'a', for example '3D6a2D8' which would result in 3 6-sided and 2 8-sided dice rolls. 
The optional UNMUTE parameter can be set to T to get insight on the standard-output why the parse has failed.
Standard behaviour:
Return a list with all the results from the specified dice rolls.
Exceptional behaviour:
- If there is a term with a 0 in front of the D, like '0DY', the term results in no rolls.
- If there is a term with a 0 after the D, like 'XD0', the term results in X 0 rolls.
- If there is a negative number in front of the D, like '-1DY', the term results in no rolls.
- If there is a negative number after the D, like 'XD-4', the term results in X 0 rolls.
- If the string is not parsable, NIL is returned, and if UNMUTE is T, the reason is printed."
  (let* ((terms (uiop:split-string dice-string :separator '(#\a)))
	 (x-y (handler-case (mapcar (lambda (terms)
				      (mapcar 'parse-integer (uiop:split-string terms :separator '(#\D)))) terms)
		(t (c)
		  (when unmute
		    (format t "DICE-STRING ~a not parsable: ~a" dice-string c))
		  nil))))
    (when x-y
      (apply #'append (loop for xy in x-y collect (apply #'roll-n-dice xy))))))
