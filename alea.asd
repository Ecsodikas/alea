;;;; alea.asd

(asdf:defsystem #:alea
  :description "A library to roll some dice and get statistics about those rolls."
  :author "Timo Netzer <exodiquas@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "src/package")
	       (:file "src/utility")
	       (:file "src/dice")
	       (:file "src/dice-engine"))
  :in-order-to ((asdf:test-op (asdf:test-op "alea/tests"))))

(asdf:defsystem #:alea/tests
  :depends-on ("alea" "fiveam")
  :components ((:file "t/alea-tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :fiveam '#:run! :alea-suite)))
