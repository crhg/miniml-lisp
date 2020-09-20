(defsystem "miniml-lisp"
  :version "0.1.0"
  :author "manabu.matsui@gmail.com"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("miniml-lisp/main"
	       "my-util"
	       "let-over-lambda"
	       "let-plus")
  :description ""
  :in-order-to ((test-op (test-op "miniml-lisp-tests"))))
