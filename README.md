# Alea
Alea is a object oriented library to roll some dice and get statistics for said dice rolls. The bare functionality of rolling dice and getting results can be done in a functional way, though. It is just not possible to calculate statistics and other neat numbers without a dice-engine object which holds a history of past throws to calculate statistics.

This library was created because I use it for a small rogue-like game I'm building and to generate randomness I use this library.

## Roadmap
The following features shall be implemented in the final library (if I, or someone else, finds new useful features, we can implement them aswell).

- [x] Roll dice with any number of sides
- [x] Roll as many dice as we want at the same time
- [x] Get n highest and m lowest results of rolls
- [x] Get average of rolls
- [x] Hold a history of the last n rolls
- [x] Parse a specific string like "4D12a8D8" to get 4 12 sided dice and 8 8 sided dice rolls
- [x] Draw nice ASCII pictures in console to see results

## Tutorial

To start rolling dem dice we just have to load the library. Probably soon available in the [Ultralisp.org](https://ultralisp.org) repository. 

The following functionalities (API) are provided:

```lisp
	;(roll-dice sides)
	(roll-dice 5) ; => [1, 6]
	
	;(roll-dice-range from to)
	(roll-dice-range 3 6) ; => [3, 6]
	
	;(roll-n-dice n sides)
	(roll-n-dice 5 6) ; => ([1,6] [1,6] [1,6] [1,6] [1,6] [1,6])
	
	;(roll-n-dice-range n from to)
	(roll-n-dice-range 3 9 12) ; => ([9, 12] [9,12] [9,12])
	
	;(count-hits rolls target)
	(count-hits '(1 2 2 2 3) 2) ; => 3
	
	;(sum-hits rolls target)
	(sum-hits '(1 2 2 2 3) 2) ; => 6
	
	;(average rolls)
	(average '(1 2 3 4 5 6)) ; => 7/2
	
	;(n-higest rolls n)
	(n-highest '(2 2 3 3 4 4 5) 4) ; => (5 4 4 3)
	
	;(n-lowest rolls n)
	(n-lowest '(2 2 3 3 4 4 5) 4) ; => (2 2 3 3)
	
	;(roll-dice-parse dice-string)
	(roll-dice-parse "4D20") ; => ([1,20] [1,20] [1,20] [1,20])
	(roll-dice-parse "2D20a4D6") ; => ([1,20] [1,20] [1,6] [1,6] [1,6] [1,6])
```

If there is something unclear about how those functions work, every function got a very detailed documentation string `(describe 'fun-name)`. If something is still unclear, let me know in an issue, I'll do my best to clear things up.

The whole API is also usable in an object oriented way.
First we have to create a dice engine. Then we can use all the dice rolling functions mentioned above to roll dice with the engine. To do so, just prefix the function with 'de-' and add the dice-engine as the first parameter. For example:

```lisp
(defvar *de* (make-dice-engine))
(de-roll-dice-parse *de* "4D20") ; => ([1,20] [1,20] [1,20] [1,20])
```

The results are the same as without the engine, but the dice-engine now holds a roll history, the lowest, the highest and the average of the past rolls. You can access those values with:

```lisp
(de-history *de*) ; => history
(de-highest *de*) ; => highest roll
(de-lowest *de*)  ; => lowest roll
(de-average *de*) ; => average roll
```

You can also draw ASCII art of the dice rolls to the standard output by wrapping the roll inside the draw-dice function. If you set the second argument of the draw-dice function to t, the cases 1-6 will not be treated as special cases.

```lisp
(draw-dice 4)   ; => returns 4 and draws a 4-pipped dice.
(draw-dice 20)  ; => returns 20 and draws a dice with the number 20 in it.
(draw-dice 4 t) ; => returns 4 and draws a dice with the number 4 in it.
```

Also you can use `(describe 'fun-name')` to get a detailed explainantion of what is going on.

## Last Paragraph

Thanks for the [Stuck Overflow Discord](https://discord.com/invite/W69eNJkpUK) for making memes about this happy little library.
