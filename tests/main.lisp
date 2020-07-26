(defpackage miniml-lisp/tests/main
  (:use :cl
        :miniml-lisp
        :rove))
(in-package :miniml-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :miniml-lisp)' in your Lisp.

(defmacro ok-eval (exp expected env)
  `(ok (equal (eval-exp ',env ',exp) ',expected)))

(defmacro ok-eval-list (&rest test-cases)
  `(progn
     ,@(mapcar
	#'(lambda (test-case)
	    (let ((exp (car test-case))
		  (expected (cadr test-case))
		  (env (caddr test-case)))
	      `(ok-eval ,exp ,expected ,env)))
	test-cases)))

(deftest test-eval-exp
    (ok-eval-list
     (1 1)
     (true true)
     ((+ 1 2) 3)
     ((* 2 3) 6)
     ((< 1 2) true)
     ((if true 1 2) 1)
     ((if false 1 2) 2)
     (x 10 ((x . 10)))
     ((let ((x 1)) x) 1)
     ((let ((x 1)
	    (y 2))
	(+ x y))
      3)
     ((let ((x 1))
	(let ((x 2)
	      (y x))
	  y))
      1)
     (((fun x x) 1) 1)
     ))

;; XXX: 環境をalistで表していることに依存しているのはあまり良くない気もする
(deftest test-eval-prog1
  (ok (equal (eval-prog1 '() '1) '(((nil 1)) ())))
  (ok (equal (eval-prog1 '() '(def ((x 1)))) '(((x 1)) ((x . 1)))))
  (ok (equal
       (eval-prog1 '() '(def ((x 1) (y 2))))
       '(
	 ((x 1) (y 2))
	 ((y . 2)(x . 1)))))
  (ok (equal
       (eval-prog1
	'((x . 1))
	'(def ((x 2) (y x))))
       '(
	 ((x 2) (y 1))
	 ((y . 1)(x . 2)(x . 1))))))

