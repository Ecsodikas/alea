;;;; package.lisp

(defpackage #:alea
  (:use #:cl)
  (:export roll-dice
	   roll-dice-range
	   roll-n-dice
	   roll-n-dice-range
	   count-hits
	   sum-hits
	   average
	   n-highest
	   n-lowest
	   roll-dice-parse
	   make-dice-engine
	   clear-history
	   de-roll-dice
	   de-roll-dice-range
	   de-roll-n-dice
	   de-roll-n-dice-range
	   de-roll-dice-parse
	   de-history
	   de-highest
	   de-lowest
	   de-average
	   draw-dice))
