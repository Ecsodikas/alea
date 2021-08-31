(in-package #:alea)

(defclass dice-engine ()
  ((dice-engine-history
    :accessor de-history
    :initform '())))

(defun make-dice-engine ()
  "Creates a new dice-engine with an empty history."
  (make-instance 'dice-engine))

(defmethod de-update-stats ((de dice-engine))
  "Updates all the statistics about the dice rolls in the lifetime of the dice-engine DE."
  (let ((average-of-rolls 5))
    average-of-rolls))

(defmethod de-roll-dice ((de dice-engine) (sides integer))
    "Roll a dice with SIDES sides. And adds the result to the history of the dice-engine DE.
Standard behaviour:
Returns an integer in the interval [1, SIDES], where SIDES always gets floored.
Exceptional behaviour: 
- If SIDES is 0, the result is also 0.
- If SIDES is less than 0, the result is 0."
  (let ((result (roll-dice sides)))
    (push result (de-history de))
    result))

(defmethod de-roll-dice-range ((de dice-engine) (from integer) (to integer))
    "Roll a dice with TO - FROM + 1 sides. Returns an integer in the interval [FROM, TO]. And adds the result to the history of the dice-engine DE.
Standard behaviour:
Returns an integer in the interval [FROM, TO], where both inputs always get floored.
Exceptional behaviour:
- If TO < FROM, the result is 0.
- The interval is allowed to contain negative numbers as long as FROM < TO."
  (let ((result (roll-dice-range from to)))
    (push result (de-history de))
    result))

(defmethod de-roll-n-dice ((de dice-engine) (n integer) (sides integer))
    "Roll N dice with SIDES sides. And adds the result to the history of the dice-engine DE.
Standard behaviour:
Returns a list with all results from N calls to the (ROLL-DICE SIDES) function.
Exceptional behaviour:
- The same as (ROLL-DICE SIDES), but multiple times.
- If N is less than 1, the empty list is returned."
  (let ((result (roll-n-dice n sides)))
    (push result (de-history de))
    result))

(defmethod de-roll-n-dice-range ((de dice-engine) (n integer) (from integer) (to integer))
    "Roll N dice with TO - FROM + 1 sides. And adds the result to the history of the dice-engine DE.
Standard behaviour:
Returns a list with all results from N calls to the (ROLL-DICE-RANGE FROM TO) function.
Exceptional behaviour:
- The same as (ROLL-DICE-RANGE FROM TO), but multiple times.
- If N is less than 1, the empty list is returned."
  (let ((result (roll-n-dice-range n from to)))
    (push result (de-history de))
    result))

(defmethod de-roll-dice-parse ((de dice-engine) (dice-string string))
    "Given a string DICE-STRING in the form 'XDY', where X and Y are positive integers, roll X dice with Y sides. And adds the result to the history of the dice-engine DE.
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
  (let ((result (roll-dice-parse dice-string)))
    (push result (de-history de))
    result))
