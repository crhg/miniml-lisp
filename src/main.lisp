(defpackage miniml-lisp
  (:use :cl :my-util/case-match :my-util/dbind :let-over-lambda :let-plus)
  (:export
   eval-exp eval-prog1
   true false if let letrec def fun
   ty-int ty-bool ty-fun
   ty-exp fresh-tyvar subst-find subst-type unify))
(in-package :miniml-lisp)

(defun boolp (x)
  (or (eq x 'true)
      (eq x 'false)))

(defun env-empty () '())

(defun env-lookup (x env)
  (cdr (assoc x env)))
       
(defun env-add (id value env)
  (acons id value env))

(defun env-add-binds (binds env)
  (reduce
   #'(lambda (env bind)
       (dbind (id value) bind
	 (env-add id value env)))
   binds
   :initial-value env))

(defun env-overwrite (id value env)
  (let ((found (assoc id env)))
    (if found
	(setf (cdr (assoc id env)) value)
	(error (format nil "env-set not found : ~A" id)))
    env))

(defun env-overwrite-binds (binds env)
  (mapc #'(lambda (bind)
	    (dbind (id value) bind
		   (env-overwrite id value env)))
	binds)
  env)

(defun make-closure (var exp env)
  `(closure ,var ,exp ,env))

(defun is-closure (x)
  (and (consp x) (eq (car x) 'closure)))

(defun closure-var (x) (cadr x))
(defun closure-exp (x) (caddr x))
(defun closure-env (x) (cadddr x))

(defun eval-exp (env exp)
  (case-match exp
	      (nil nil)
	      (:_ :where (numberp exp) exp)
	      (true 'true)
	      (false 'false)
	      (:_ :where (symbolp exp) (env-lookup exp env))
	      ((quote ?x) ?x)
	      ((?op ?lhs ?rhs) :where (member ?op '(+ * < cons))
	       (apply-prim env ?op ?lhs ?rhs))
	      ((list . ?exps)
	       (eval-exp env
			 (reduce #'(lambda (e c) `(cons ,e ,c)) ?exps :initial-value nil :from-end t)))
	      ((if ?test ?then ?else)
	       (let ((test (eval-exp env ?test)))
		 (case test
		   (true (eval-exp env ?then))
		   (false (eval-exp env ?else))
		   (t (error
		       (format nil "Test expression must be boolean: if: ~A" test))))))
	      ((match ?expr . ?pat-exprs)
	       (let ((value (eval-exp env ?expr)))
		 (eval-match env value ?pat-exprs)))
	      ((let ?binds ?exp)
	       (let ((new-env
		      (env-add-binds (eval-binds env ?binds) env)))
		 (eval-exp new-env ?exp)))
	      ((letrec ?binds ?exp)
	       (let* ((dummy-env
		       (env-add-binds (dummy-binds ?binds) env))
		      (new-env
		       (env-overwrite-binds (eval-binds dummy-env ?binds) dummy-env)))
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
	      (dbind (var exp) bind
		`(,var ,(eval-exp env exp))))
	  binds))

(defun dummy-binds (binds)
  (mapcar #'(lambda (bind)
	      (let ((var (car bind)))
		`(,var nil)))
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
      (cons (cons left-value right-value))
      (t (error (format nil "unknown operator: ~A" op))))))

(defun eval-match (env value pat-exps)
  (if (null pat-exps)
      (error (format nil "no match"))
      (dbind ((pat exp) . rest) pat-exps
	(multiple-value-bind (matched binds) (match pat value)
	  (if matched
	      (eval-exp (env-add-binds binds env) exp)
	      (eval-match env value rest))))))

(defun match (pat value)
  (case-match pat
    (nil (values (null value) '()))
    (true (values (eq 'true value) '()))
    (false (values (eq 'false value) '()))
    (?var :where (symbolp ?var) (values t `((,?var ,value))))
    (?num :where (numberp ?num) (values (= ?num value) '()))
    ((quote ?const) (values (equal ?const value) '()))
    ((?car-pat . ?cdr-pat)
     (if (consp value)
	 (multiple-value-bind (car-matched car-binds) (match ?car-pat (car value))
	   (if car-matched
	       (multiple-value-bind (cdr-matched cdr-binds) (match ?cdr-pat (cdr value))
		 (if cdr-matched
		     (values t (union car-binds cdr-binds))
		     (values nil '())))
	       (values nil '())))
	 (values nil '())))
    (:_ (error (format nil "invalid pattern: ~A" pat)))))

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
		 (values bind-values new-env)))
	      ((defrec ?binds)
	       (let* ((dummy-env
		       (env-add-binds (dummy-binds ?binds) env))
		      (bind-values (eval-binds dummy-env ?binds))
		      (new-env
		       (env-overwrite-binds bind-values dummy-env)))
		 (values bind-values new-env)))
	      (:_
	       (let ((value (eval-exp env prog)))
		 (values `((nil ,value)) env)))))
	       
(defun repl ()
  (let ((tyenv (env-empty))
	(env (env-empty)))
    (loop
       for prog1 = (read)
       until (eq prog1 :quit)
       do
	 (handler-case
	     (multiple-value-bind (var-tys new-tyenv)
		 (ty-prog1 tyenv prog1)
	       (multiple-value-bind (var-values new-env)
		   (handler-case
		       (eval-prog1 env prog1)
		     (error (e) (values e env)))
		 (let ((*print-circle* t))
		   (format t "~a~%" (mapcar #'(lambda (var-ty var-value)
						`(,(car var-ty) ,(cadr var-ty) ,(cadr var-value)))
					    var-tys
					    var-values)))
		 (setq tyenv new-tyenv)
		 (setq env new-env)))
	   (error (e)
	     ;; 型エラー
	     (format t "~a~%" e))))))

(defun ty-exp (tyenv exp)
  "型環境TYENVと式EXPから型代入とEXPの型を返す"
  (case-match exp
	      (:_ :where (numberp exp) (values '() 'ty-int))
	      (true (values '() 'ty-bool))
	      (false (values '() 'ty-bool))
	      (?var :where (symbolp exp)
		    (aif (env-lookup ?var tyenv)
			 (values '() it)
			 (error (format nil "variable not bound: ~A" ?var))))
	      ((?op ?exp1 ?exp2) :where (member ?op '(+ * <))
	       (let+ (((&values s1 ty1) (ty-exp tyenv ?exp1))
		      ((&values s2 ty2) (ty-exp tyenv ?exp2))
		      ((&values eqs3 ty) (ty-prim ?op ty1 ty2))
		      (eqs `(,@(eqs-of-subst s1) ,@(eqs-of-subst s2) ,@eqs3))
		      (s3 (unify eqs)))
		     (values s3 (subst-type s3 ty))))
	      ((if ?test ?then ?else)
	       (let+ (((&values s-test ty-test) (ty-exp tyenv ?test))
		      ((&values s-then ty-then) (ty-exp tyenv ?then))
		      ((&values s-else ty-else) (ty-exp tyenv ?else))
		      (eqs `(,@(eqs-of-subst s-test)
			       ,@(eqs-of-subst s-then)
			       ,@(eqs-of-subst s-else)
			       (,ty-test ty-bool)
			       (,ty-then ,ty-else)))
		      (s (unify eqs)))
		     (values s (subst-type s ty-then))))
	      ((fun ?var ?exp)
	       (let+ ((ty-var (fresh-tyvar))
		      ((&values s ty) (ty-exp (env-add ?var ty-var tyenv) ?exp)))
		 (values s (subst-type s `(ty-fun ,ty-var ,ty)))))
	      (:_ (error (format nil "not implemented: ~A" exp)))))

(defun ty-prim (op ty1 ty2)
  "演算子OPが生成すべきTY1,TY2に対する制約集合と返値の型を返す"
  (case op
    (+ (values `((,ty1 ty-int) (,ty2 ty-int)) 'ty-int))
    (* (values `((,ty1 ty-int) (,ty2 ty-int)) 'ty-int))
    (< (values `((,ty1 ty-int) (,ty2 ty-int)) 'ty-bool))
    (t (error (format nil "not implemented operator: ~A" op)))))
    
(defun ty-prog1 (tyenv prog)
  "PROGにTYENVのもとで型を付けて
   (<結果> <新しい型環境>)の形のリストを返す。
   <結果>はPROGが
   (DEF ((v1 e1) (v2 e2)...)) のとき ((v1 <e1の型>) (v2 <e2の型>)...)で
   <式>のときは ((NIL <式の型>)) である"
  (case-match prog
	      ((def ?binds)
	       (let* ((bind-tys (ty-binds tyenv ?binds))
		      (new-tyenv (env-add-binds bind-tys tyenv)))
		 (values bind-tys new-tyenv)))
	      ((defrec ?binds)
	       (error "not implemented(ty-prog1): ~A" prog))
	      (:_
	       (let+ (((&values &ign ty) (ty-exp tyenv prog)))
		 (values `((nil ,ty)) tyenv)))))

(defun ty-binds (tyenv binds)
  (mapcar #'(lambda (bind)
	      (let+ (((var exp) bind)
		     ((&values &ign ty) (ty-exp tyenv exp)))
		    `(,var ,ty)))
	  binds))

(defun fresh-tyvar ()
  "新しい型変数を返します"

  `(ty-var ,(gensym)))

(defun freevar-ty (ty)
  "型TYに含まれる自由な型変数のリストを返します"
  
  (case-match ty
	      (ty-int '())
	      (ty-bool '())
	      ((ty-var :_) `(,ty))
	      ((ty-fun ?ty1 ?ty2) (union (freevar-ty ?ty1) (freevar-ty ?ty2)))))

(defun subst-find (subst tyvar)
  "SUBST ((tyvar1 ty1) ...) から TYVARについてのものを探す。
   戻り値は(tyvar_i ty_i)の形のリスト。見つからなければNIL"
  
  (find-if #'(lambda (s) (eq (car s) tyvar))
	   subst ))

(defun subst-type (subst ty)
  "TYの中の型変数をSUBSTに基づいて置き換えます"

  (case-match ty
	      (ty-int 'ty-int)
	      (ty-bool 'ty-bool)
	      ((ty-var :_)
	       (aif (subst-find subst ty)
		    (subst-type subst (cadr it))
		    ty))
	      ((ty-fun ?ty1 ?ty2)
	       `(ty-fun ,(subst-type subst ?ty1) ,(subst-type subst ?ty2)))))

(defun eqs-of-subst (subst) subst)

(defun subst-eqs (subst ty-eqs)
  "型等式リストTY-EQSに現れる型を型代入SUBSTに基づいて置き換えます"
  
  (mapcar #'(lambda (equation)
	      (dbind (ty1 ty2) equation
		`(,(subst-type subst ty1) ,(subst-type subst ty2))))
	  ty-eqs))
	       
(defun unify (ty-eqs)
  "与えられた型等式リストが全て等しくなる型代入を求めます"
  
  (if (null ty-eqs)
      '()
      (dbind (ty-eq . rest) ty-eqs
	(dbind (ty1 ty2) ty-eq
	  (case-match ty-eq
		      ((?ty1 ?ty2) :where (equal ?ty1 ?ty2)
		       (unify rest))
		      (((ty-fun ?ty11 ?ty12) (ty-fun ?ty21 ?ty22))
		       (format nil "~A ~A ~A ~A" ?ty11 ?ty12 ?ty21 ?ty22)
		       (unify `((,?ty11 ,?ty21) (,?ty12 ,?ty22) ,@rest)))
		      (((ty-var :_) :_)
		       (when (member ty1 (freevar-ty ty2))
			 (error (format nil "occur check failed: ~A ~A" ty1 ty2)))
		       (let*
			   ((subst1 `(,ty1 ,ty2))
			    (subst `(,subst1))
			    (substed-rest (subst-eqs subst rest)))
			 `(,subst1 ,@(unify substed-rest))))
		      ((:_ (ty-var :_))
		       (unify `((,ty2 ,ty1) ,@rest)))
		      (:_
		       (error (format nil "cannot unify: ~A ~A" ty1 ty2))))))))
