(defsystem "miniml-lisp-tests"
  :author "manabu.matsui@gmail.com"
  :license "MIT"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("miniml-lisp-tests/main"
	       "miniml-lisp/main"
               "rove")
  :description "Test system for miniml-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
