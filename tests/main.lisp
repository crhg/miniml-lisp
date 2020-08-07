(defpackage miniml-lisp/tests/main
  (:use
   :cl
   :miniml-lisp
   :my-util
   :rove
   :let-plus))
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
     ((letrec ((f (fun x (if (< x 1) 1 (* x (f (+ x -1)))))))
	(f 0))
      1)
     ((letrec ((f (fun x (if (< x 1) 1 (* x (f (+ x -1)))))))
	(f 1))
      1)
     ((letrec ((f (fun x (if (< x 1) 1 (* x (f (+ x -1)))))))
	(f 5))
      120)
     ((quote a)
      a)
     ))

;; XXX: 環境をalistで表していることに依存しているのはあまり良くない気もする
(deftest test-eval-prog1
  (macrolet ((ok-eval-prog1 (env program expected-result expected-env)
	       (with-gensyms (actual-result actual-env)
		 `(multiple-value-bind (,actual-result ,actual-env) (eval-prog1 ,env ,program)
		    (ok (equal ,actual-result ,expected-result))
		    (ok (equal ,actual-env ,expected-env))))))
    (ok-eval-prog1 '() '1 '((nil 1)) '())
    (ok-eval-prog1 '() '(def ((x 1))) '((x 1)) '((x . 1)))
    (ok-eval-prog1
     '()
     '(def ((x 1) (y 2)))
     '((x 1) (y 2))
     '((y . 2)(x . 1)))
    (ok-eval-prog1
     '((x . 1))
     '(def ((x 2) (y x)))
     '((x 2) (y 1))
     '((y . 1)(x . 2)(x . 1)))))

(deftest test-subst-find
  (let ((tyvar1 (fresh-tyvar))
	(tyvar2 (fresh-tyvar)))
    (ok (equal (subst-find '() tyvar1) nil))
    (ok (equal (subst-find `((,tyvar1 ty-int)) tyvar1)
	       `(,tyvar1 ty-int)))
    (ok (equal (subst-find `((,tyvar1 ty-int)) tyvar2)
	       nil))
    (ok (equal (subst-find `((,tyvar1 ty-int) (,tyvar2 ty-bool)) tyvar2)
	       `(,tyvar2 ty-bool)))))

(deftest test-subst-type
  (let ((tyvar1 (fresh-tyvar))
	(tyvar2 (fresh-tyvar)))
    (ok (equal (subst-type '() 'ty-int)
	       'ty-int))
    (ok (equal (subst-type `((,tyvar1 ty-int)) tyvar1)
	       'ty-int))
    (ok (equal (subst-type `((,tyvar1 ty-int)) tyvar2)
	       tyvar2))
    (ok (equal (subst-type `((,tyvar1 ty-int) (,tyvar2 ,tyvar1)) tyvar2)
	       'ty-int))
    (ok (equal (subst-type `((,tyvar2 ,tyvar1) (,tyvar1 ty-int)) tyvar2)
	       'ty-int))))

(deftest test-unify
  (let ((tyvar1 (fresh-tyvar))
	(tyvar2 (fresh-tyvar)))
    (ok (equal (unify '())
	       '()))
    (ok (equal (unify `((,tyvar1 ,tyvar1)))
	       '()))
    (ok (equal (unify '((ty-int ty-int)))
	       '()))
    (ok (equal (unify '((ty-int ty-int) (ty-bool ty-bool)))
	       '()))
    (ok (equal (unify `((,tyvar1 ty-int)))
	       `((,tyvar1 ty-int))))
    (ok (equal (unify `((,tyvar1 ,tyvar2)))
	       `((,tyvar1 ,tyvar2))))
    (ok (equal (unify `((ty-int ,tyvar2)))
	       `((,tyvar2 ty-int))))
    (let ((subst (unify `((,tyvar1 ty-int) (,tyvar2 ty-bool)))))
      (ok (equal (subst-type subst tyvar1) 'ty-int))
      (ok (equal (subst-type subst tyvar2) 'ty-bool)))
    (ok (equal (unify `(((ty-fun ty-int ty-bool) (ty-fun ty-int ty-bool))))
	       '()))
    (ok (equal (unify `(((ty-fun ,tyvar1 ty-int) (ty-fun ty-int ty-int))))
	       `((,tyvar1 ty-int))))
    ))

(deftest test-ty-exp
  (macrolet ((ok-ty-exp (exp expected-type)
	       (with-gensyms (subst ty)
	       `(ok (let+ (((&values ,subst ,ty) (ty-exp '() ',exp)))
		      (equal (subst-type ,subst ,ty) ',expected-type))))))
    (ok-ty-exp 1 ty-int)
    (ok-ty-exp true ty-bool)
    (ok-ty-exp (+ 1 2) ty-int)
    (ok-ty-exp (if true 1 1) ty-int)
    (ok-ty-exp (fun x (+ x 1)) (ty-fun ty-int ty-int))
    (ok-ty-exp ((fun x x) 1) ty-int)
))


