(defsystem "miniml-lisp"
  :version "0.1.0"
  :author "manabu.matsui@gmail.com"
  :license "MIT"
  :depends-on ("my-util" "let-over-lambda" "let-plus")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "miniml-lisp/tests"))))
 
(defsystem "miniml-lisp/tests"
  :author "manabu.matsui@gmail.com"
  :license "MIT"
  :depends-on ("miniml-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for miniml-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
