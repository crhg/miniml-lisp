(defpackage miniml-lisp
  (:use :cl :my-util/case-match)
  (:export
   eval-exp eval-prog1
   true false if let def fun))
(in-package :miniml-lisp)

(defun boolp (x)
  (or (eq x 'true)
      (eq x 'false)))

(defun env-empty () '())

(defun env-lookup (x env)
  (cond
    ((null env) (error (format nil "not found: ~A" x)))
    ((eq x (caar env)) (cdar env))
    (t (env-lookup x (cdr env)))))

(defun env-add (id value env)
  `((,id . ,value) ,@env))

(defun env-add-binds (binds env)
  (reduce
   #'(lambda (env bind)
       (let ((id (car bind))
	     (value (cadr bind)))
	 (env-add id value env)))
   binds
   :initial-value env))

(defun make-closure (var exp env)
  `(closure ,var ,exp ,env))

(defun is-closure (x)
  (and (consp x) (eq (car x) 'closure)))

(defun closure-var (x) (cadr x))
(defun closure-exp (x) (caddr x))
(defun closure-env (x) (cadddr x))

(defun eval-exp (env exp)
  (case-match exp
	      (:_ :where (numberp exp) exp)
	      (true 'true)
	      (false 'false)
	      (:_ :where (symbolp exp) (env-lookup exp env))
	      ((?op ?lhs ?rhs) :where (member ?op '(+ * <))
	       (apply-prim env ?op ?lhs ?rhs))
	      ((if ?test ?then ?else)
	       (let ((test (eval-exp env ?test)))
		 (case test
		   (true (eval-exp env ?then))
		   (false (eval-exp env ?else))
		   (t (error
		       (format nil "Test expression must be boolean: if: ~A" test))))))
	      ((let ?binds ?exp)
	       (let ((new-env
		      (env-add-binds (eval-binds env ?binds) env)))
		 (eval-exp new-env ?exp)))
	      ((fun ?var ?exp)
	       (make-closure ?var ?exp env))
	      ((?exp1 ?exp2)
	       (let ((val1 (eval-exp env ?exp1))
		     (val2 (eval-exp env ?exp2)))
		 (if (is-closure val1)
		     (eval-exp (env-add (closure-var val1) val2 (closure-env val1))
			       (closure-exp val1))
		     (error (format nil "Cannot apply: ~A" val1)))))
	      (:_
	       (error (format nil "Cannot eval: ~A" exp)))))

(defun eval-binds (env binds)
  (mapcar #'(lambda (bind)
	      (let ((var (car bind))
		    (exp (cadr bind)))
		`(,var ,(eval-exp env exp))))
	  binds))

(defun apply-prim (env op left right)
  (let ((left-value (eval-exp env left))
	(right-value (eval-exp env right)))
    (case op
      (+ (or (numberp left-value)
	     (error (format nil "left must be number(+): ~A" left-value)))
	 (or (numberp right-value)
	     (error (format nil "right must be number(+): ~A" right-value)))
	 (+ left-value right-value))
      (* (or (numberp left-value)
	     (error (format nil "left must be number(*): ~A" left-value)))
	 (or (numberp right-value)
	     (error (format nil "right must be number(*): ~A" right-value)))
	 (* left-value right-value))
      (< (or (numberp left-value)
	     (error (format nil "left must be number(<): ~A" left-value)))
	 (or (numberp right-value)
	     (error (format nil "right must be number(<): ~A" right-value)))
	 (if (< left-value right-value) 'true 'false))
      (t (error (format nil "unknown operator: ~A" op))))))

;; progをenvのもとで評価して
;; (<結果> <新しい環境)の形のリストを返す。
;; <結果>はprogが
;; (def ((v1 e1) (v2 e2)...)) のとき ((v1 <e1の値>) (v2 <e2の値>)...)で
;; <式>のときは ((nil <式の値>)) である
(defun eval-prog1 (env prog)
  (case-match prog
	      ((def ?binds)
	       (let* ((bind-values (eval-binds env ?binds))
		      (new-env (env-add-binds bind-values env)))
		 `(,bind-values ,new-env)))
	      (:_
	       (let ((value (eval-exp env prog)))
		 `(((nil ,value)),env)))))
	       
(defun repl ()
  (let ((env (env-empty)))
    (loop
       for prog1 = (read)
       until (eq prog1 :quit)
       do
	 (let* ((result
		 (handler-bind
		     ((error #'(lambda (cond) `(,cond ,env))))
		   (eval-prog1 env prog1)))
		(var-values (car result))
		(new-env (cadr result)))
	   (format t "~A~%" var-values)
	   (setq env new-env)))))

