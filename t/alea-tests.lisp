(in-package #:alea)
(fiveam:def-suite alea-tests)
(fiveam:in-suite alea-tests)

(fiveam:test roll-single-dice
	     (fiveam:is (= 1 (loop for i from 0 to 100000 minimize (roll-dice 9))))
	     (fiveam:is (= 6 (loop for i from 0 to 100000 maximize (roll-dice 6))))
	     (fiveam:is (= 0 (loop for i from 0 to 100000 maximize (roll-dice 0))))
	     (fiveam:is (= 0 (loop for i from 0 to 100000 maximize (roll-dice -1))))
	     (fiveam:is (= 4 (loop for i from 0 to 100000 maximize (roll-dice 4.3))))
	     (fiveam:is (= 0 (loop for i from 0 to 100000 maximize (roll-dice -13423432)))))

(fiveam:test roll-single-dice-range
	     (fiveam:is (= 12 (loop for i from 0 to 100000 minimize (roll-dice-range 12 33))))
	     (fiveam:is (= 33 (loop for i from 0 to 100000 maximize (roll-dice-range 12 33))))
	     (fiveam:is (= 0 (loop for i from 0 to 100000 maximize (roll-dice-range 10 9))))
	     (fiveam:is (= 10 (loop for i from 0 to 100000 maximize (roll-dice-range -14 10))))
	     (fiveam:is (= 0 (loop for i from 0 to 100000 maximize (roll-dice-range -10 0)))))

(fiveam:test roll-multiple-dice
	     (let ((d-rolls (roll-n-dice 10000 12)))
	       (fiveam:is (= 1 (apply #'min d-rolls)))
	       (fiveam:is (= 12 (apply #'max d-rolls)))
	       (fiveam:is (= 10000 (length d-rolls)))))

(fiveam:test roll-multiple-dice-range
	     (let ((d-rolls (roll-n-dice-range 10000 12 100)))
	       (fiveam:is (= 12 (apply #'min d-rolls)))
	       (fiveam:is (= 100 (apply #'max d-rolls)))
	       (fiveam:is (= 10000 (length d-rolls))))
	     (fiveam:is (not (roll-n-dice-range 0 12 21)))
	     (fiveam:is (not (roll-n-dice-range -14 23 33))))

(fiveam:test dice-parse
	     (let ((d-rolls (roll-dice-parse "1000D6a1000D20")))
	       (fiveam:is (= 1 (apply #'min d-rolls)))
	       (fiveam:is (= 20 (apply #'max d-rolls)))
	       (fiveam:is (= 2000 (length d-rolls))))
	     (fiveam:is (not (roll-dice-parse "-1D6")))
	     (fiveam:is (equal '(0 0 0) (roll-dice-parse "3D-1"))))

(fiveam:test n-highest-lowest
	     (let ((d-rolls (roll-dice-parse "1000D6a5000D22")))
	       (fiveam:is (equal '(22 22 22 22 22) (n-highest d-rolls 5)))
	       (fiveam:is (equal '(1 1 1 1 1) (n-lowest d-rolls 5))))
	     (fiveam:is (not (n-highest '() 5)))
	     (fiveam:is (not (n-lowest '() 5)))
	     (fiveam:is (equal '(1 2 3 4) (n-lowest '(1 2 3 4) 100)))
	     (fiveam:is (equal '(3 2 1) (n-highest '(1 2 3) 100))))

(fiveam:test average-roll
	     (let ((d-rolls (roll-dice-parse "10000D6")))
	       (fiveam:is (< 3 (average d-rolls) 4)))
	     (let ((d-rolls (roll-dice-parse "10000D10")))
	       (fiveam:is (< 4 (average d-rolls) 6)))
	     (let ((d-rolls (roll-dice-parse "10000D100")))
	       (fiveam:is (< 48 (average d-rolls) 52)))
	     (fiveam:is (= 0 (average '()))))

(fiveam:test dice-engine-caching
	     (let ((de (make-dice-engine)))
	       (de-roll-dice de 6)
	       (fiveam:is (<= 1 (de-highest de) 6))
	       (fiveam:is (<= 1 (de-lowest de) 6))
	       (fiveam:is (= (de-highest de) (de-lowest de)))
	       (fiveam:is (= 1 (length (de-history de))))))
