(defpackage :miniml-lisp
  (:use :cl)
  (:import-from :let-plus #:let+ #:&values #:&ign)
  (:import-from :let-over-lambda #:aif #:it)
  (:import-from :my-util #:case-match #:dbind #:mb-mapcar)
  (:export
   #:eval-exp #:eval-prog1
   #:true #:false
   #:cons #:nil
   #:if #:let #:letrec #:def #:defrec #:fun #:match
   #:ty-int #:ty-bool #:ty-fun
   #:ty-exp #:fresh-tyvar #:subst-find #:subst-type #:unify))
(in-package :miniml-lisp)

(define-condition miniml-error (simple-error)
  ((fmt :initarg :fmt :reader miniml-error-fmt)
   (args :initarg :args :reader miniml-error-args))
  (:report (lambda (condition stream)
	     (apply #'format stream
		    (miniml-error-fmt condition)
		    (miniml-error-args condition)))))

(defmacro make-miniml-error (fmt &rest args)
  `(make-condition 'miniml-error :fmt ,fmt :args (list ,@args)))

(defun boolp (x)
  (or (eq x 'true)
      (eq x 'false)))

(defun env-empty () '())

(defun env-lookup (x env)
  (aif (assoc x env)
       (cdr it)
       (error (make-miniml-error "env-lookup: not found ~A in ~A" x env))))
       
(defun env-add (id value env)
  (acons id value env))

(defun env-adds (ids values env)
  "IDSとVALUESを先頭からそれぞれ組み合わせてENVに追加した環境を返す"
  (env-add-binds (mapcar #'list ids values) env))

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
	(error (make-miniml-error "env-set not found : ~A" id)))
    env))

(defun env-overwrite-binds (binds env)
  (mapc #'(lambda (bind)
	    (dbind (id value) bind
		   (env-overwrite id value env)))
	binds)
  env)

(defun env-map (f env)
  "idとvalueを引数にとる関数FをENVのidとvalueに適用した結果のリスト"
  (mapcar #'(lambda (e)
	      (dbind (id . value) e
		(funcall f id value)))
	  env))

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
	     (make-miniml-error "Test expression must be boolean: if: ~A" test))))))
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
	   (error (make-miniml-error "Cannot apply: ~A" val1)))))
    (:_
     (error (make-miniml-error "Cannot eval: ~A" exp)))))

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
      (t (error (make-miniml-error "unknown operator: ~A" op))))))

(defun eval-match (env value pat-exps)
  (if (null pat-exps)
      (error (make-miniml-error "no match"))
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
    ((cons ?car-pat ?cdr-pat)
     (if (consp value)
	 (multiple-value-bind (car-matched car-binds) (match ?car-pat (car value))
	   (if car-matched
	       (multiple-value-bind (cdr-matched cdr-binds) (match ?cdr-pat (cdr value))
		 (if cdr-matched
		     (values t (union car-binds cdr-binds))
		     (values nil '())))
	       (values nil '())))
	 (values nil '())))
    (:_ (error (make-miniml-error "invalid pattern: ~A" pat)))))

(defun eval-prog1 (env prog)
  "PROGをENVのもとで評価して<結果>と<新しい環境>を返します。
   <結果>はPROGが
  (<defまたはdefrec> ((v1 e1) (v2 e2)...)) のとき ((v1 <e1の値>) (v2 <e2の値>)...)で
  <式>のときは ((nil <式の値>)) です"
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
	 (case prog1
	   (:env
	    (let ((*print-circle* t))
	      (format t "~a~%" env)
	      (format t "~a~%" tyenv)))
	   (otherwise
	    (handler-case
		(multiple-value-bind (var-tys new-tyenv)
		    (ty-prog1 tyenv prog1)
		  (multiple-value-bind (var-values new-env)
		      (eval-prog1 env prog1)
		    (let ((*print-circle* t))
		      (format t "~a~%" (mapcar #'(lambda (var-ty var-value)
						   `(,(car var-ty) ,(cadr var-ty) ,(cadr var-value)))
					       var-tys
					       var-values)))
		    (setq tyenv new-tyenv)
		    (setq env new-env)))
	      (miniml-error (e)
		;; 型エラー
		(format t "~a~%" e))))))))

(defun ty-exp (tyenv exp)
  "型環境TYENVと式EXPから型代入とEXPの型を返す"
  (case-match exp
    (:_ :where (numberp exp) (values '() 'ty-int))
    (true (values '() 'ty-bool))
    (false (values '() 'ty-bool))
    (nil (values '() `(ty-cons ,(fresh-tyvar))))
    (?var :where (symbolp exp)
     (aif (env-lookup ?var tyenv)
	  (let+ ((tyvars (vars-of-tysc it))
		 (s (mapcar #'(lambda (v) `(,v ,(fresh-tyvar))) tyvars))
		 (ty (ty-of-tysc it)))
	    (values '() (subst-type s ty)))
	  (error (make-miniml-error "ty-exp: variable not bound: ~A" ?var))))
    ((?op ?exp1 ?exp2) :where (member ?op '(+ * < cons))
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
	    (tysc-var (tysc-of-ty ty-var))
	    ((&values s ty) (ty-exp (env-add ?var tysc-var tyenv) ?exp)))
       (values s (subst-type s `(ty-fun ,ty-var ,ty)))))
    ((let ?binds ?exp)
     (let+ ((vars (mapcar #'car ?binds))
	    (exps (mapcar #'cadr ?binds))
	    ((&values substs tys)
	      (mb-mapcar #'(lambda (exp) (ty-exp tyenv exp))
			 exps))
	    (tyscs (mapcar #'(lambda (s ty) (ty-closure ty tyenv s)) substs tys))
	    (new-tyenv (env-adds vars tyscs tyenv))
	    ((&values s-exp ty-exp) (ty-exp new-tyenv ?exp))
	    (eqs `(,@(mapcan #'eqs-of-subst substs) ,@(eqs-of-subst s-exp)))
	    (s (unify eqs)))
       (values s (subst-type s ty-exp))))
    ((letrec ?binds ?exp)
     (let+ ((vars (mapcar #'car ?binds))
	    (exps (mapcar #'cadr ?binds))
	    (var-tys (mapcar #'(lambda (v)
				 (declare (ignore v))
				 (fresh-tyvar))
			     vars))
	    (var-tyscs (mapcar #'tysc-of-ty var-tys))
	    (temp-tyenv (env-adds vars var-tyscs tyenv))
	    ((&values substs tys)
	      (mb-mapcar #'(lambda (exp) (ty-exp temp-tyenv exp))
			 exps))
	    (tyscs (mapcar #'(lambda (s ty) (ty-closure ty temp-tyenv s)) substs tys))
	    (new-tyenv (env-adds vars tyscs tyenv))
	    ((&values s-exp ty-exp) (ty-exp new-tyenv ?exp))
	    (eqs
	      `(,@(mapcar #'(lambda (t1 t2) `(,t1 ,t2)) var-tys tys)
		,@(mapcan #'eqs-of-subst substs)
		,@(eqs-of-subst s-exp)))
	    (s (unify eqs)))
       (values s (subst-type s ty-exp))))
    ((match ?exp . ?pat-exprs)
     (ty-match tyenv ?exp ?pat-exprs))
    ((?fun ?operand)
     (let+ (((&values s-fun ty-fun) (ty-exp tyenv ?fun))
	    ((&values s-operand ty-operand) (ty-exp tyenv ?operand))
	    (ty1 (fresh-tyvar))
	    (ty2 (fresh-tyvar))
	    (eqs `(,@(eqs-of-subst s-fun)
		   ,@(eqs-of-subst s-operand)
		   (,ty-fun (ty-fun ,ty1 ,ty2))
		   (,ty-operand ,ty1)))
	    (s (unify eqs)))
       (values s (subst-type s ty2))))
    (:_ (error (make-miniml-error "ty-exp: not implemented: ~A" exp)))))

(defun ty-binds (tyenv binds)
  "束縛binds=((var1 exp1)...)と型環境tyenvから型代入と各変数の型のリスト=((var1 ty1)...)を返す"
  (if (null binds)
      (values '() '())
      (let+ ((((var exp) . rest) binds)
	     ((&values s-exp ty-exp) (ty-exp tyenv exp))
	     ((&values s-rest tys-rest) (ty-binds tyenv rest))
	     (tysc (ty-closure ty-exp tyenv s-exp)))
	(values (append s-exp s-rest)
		`((,var ,tysc) ,@tys-rest)))))

(defun ty-prim (op ty1 ty2)
  "演算子OPが生成すべきTY1,TY2に対する制約集合と返値の型を返す"
  (case op
    (+ (values `((,ty1 ty-int) (,ty2 ty-int)) 'ty-int))
    (* (values `((,ty1 ty-int) (,ty2 ty-int)) 'ty-int))
    (< (values `((,ty1 ty-int) (,ty2 ty-int)) 'ty-bool))
    (cons (values `((,ty2 (ty-cons ,ty1))) ty2))
    (t (error (make-miniml-error "ty-prim: not implemented operator: ~A" op)))))
    
(defun ty-match (tyenv exp pat-exps)
  (when (null pat-exps)
    (error (make-miniml-error "ty-match: no pattern")))
  (let+ (((&values subst ty) (ty-exp tyenv exp))
	 ((&values ty-pats
		   (ty-exp . ty-exp-rest)
		   eqs-list)
	   (mb-mapcar #'(lambda (pat-exp)
			  (let+ (((pat exp) pat-exp))
			    (ty-pat-exp tyenv pat exp)))
		      pat-exps))
	 (eqs `(,@(eqs-of-subst subst)
		,@(mapcar #'(lambda (ty-pat) `(,ty ,ty-pat)) ty-pats)
		,@(mapcar #'(lambda (ty-rest) `(,ty-exp ,ty-rest)) ty-exp-rest)
		,@(apply #'nconc eqs-list)))
	 (s (unify eqs)))
    (values s (subst-type s ty-exp))))

(defun ty-pat-exp (tyenv pat exp)
  "match式のパターンと式の組について以下の物を返します
    * PATの型
    * EXPの型
    * 満たすべき型等式のリスト"
  (let+ (((&values ty-pat new-tyenv eqs-pat)
	   (ty-pat tyenv pat))
	 ((&values subst ty) (ty-exp new-tyenv exp))
	 (eqs `(,@eqs-pat ,@(eqs-of-subst subst))))
    (values ty-pat ty eqs)))

(defun ty-pat (tyenv pat)
  "match式のパターンPATについて以下の物を返します。
     * その型
     * キャプチャした変数でTYENVを更新した型環境
     * 満たすべき型等式のリスト
  キャプチャする変数には重複がないものとします(いまのところ積極的にチェックはしません)"
  (case-match pat
    (nil (let ((ty (fresh-tyvar)))
	   (values `(ty-cons ,ty) tyenv '())))
    (?bool :where (boolp ?bool)
     (values 'ty-bool tyenv '()))
    (?var :where (symbolp ?var)
     (let ((ty (fresh-tyvar)))
       (values ty (env-add ?var (tysc-of-ty ty) tyenv) '())))
    (?num :where (numberp ?num)
     (values 'ty-int tyenv '()))
    ((quote . :_)
     (error (make-miniml-error "ty-pat: not implemented: %a" pat)))
    ((cons ?car-pat ?cdr-pat)
     (let+ (((&values ty-car tyenv1 tyeq-car) (ty-pat tyenv ?car-pat))
	    ((&values ty-cdr tyenv2 tyeq-cdr) (ty-pat tyenv1 ?cdr-pat))
	    (tyeq `(((ty-cons ,ty-car) ,ty-cdr)
		    ,@tyeq-car
		    ,@tyeq-cdr)))
       (values ty-cdr tyenv2 tyeq)))))

(defun ty-prog1 (tyenv prog)
  "PROGにTYENVのもとで型を付けて
   (<結果> <新しい型環境>)の形のリストを返す。
   <結果>はPROGが
   (DEF ((v1 e1) (v2 e2)...)) のとき ((v1 <e1の型>) (v2 <e2の型>)...)で
   <式>のときは ((NIL <式の型>)) である"
  (case-match prog
    ((def ?binds)
     (let+ ((vars (mapcar #'car ?binds))
	    (exps (mapcar #'cadr ?binds))
	    ((&values substs tys)
	      (mb-mapcar #'(lambda (exp) (ty-exp tyenv exp))
			 exps))
	    (tyscs (mapcar #'(lambda (s ty) (ty-closure ty tyenv s)) substs tys))
	    (eqs (mapcan #'eqs-of-subst substs))
	    (s (unify eqs))
	    (new-tyenv (subst-tyenv s (env-adds vars tyscs tyenv))))
       (values
	(mapcar #'(lambda (var)
		    `(,var ,(ty-of-tysc (env-lookup var new-tyenv))))
		vars)
	new-tyenv)))
    ((defrec ?binds)
     (let+ ((vars (mapcar #'car ?binds))
	    (exps (mapcar #'cadr ?binds))
	    (var-tys (mapcar #'(lambda (v)
				 (declare (ignore v))
				 (fresh-tyvar))
			     vars))
	    (var-tyscs (mapcar #'tysc-of-ty var-tys))
	    (temp-tyenv (env-adds vars var-tyscs tyenv))
	    ((&values substs tys)
	      (mb-mapcar #'(lambda (exp) (ty-exp temp-tyenv exp))
			 exps))
	    (eqs
	      `(,@(mapcar #'(lambda (t1 t2) `(,t1 ,t2)) var-tys tys)
		,@(mapcan #'eqs-of-subst substs)))
	    (s (unify eqs))
	    (tyscs (mapcar #'(lambda (s ty) (ty-closure ty temp-tyenv s)) substs tys))
	    (new-tyenv (subst-tyenv s (env-adds vars tyscs tyenv))))
       (values
	(mapcar #'(lambda (var)
		    `(,var ,(ty-of-tysc (env-lookup var new-tyenv))))
		vars)
	new-tyenv)))
    (:_
     (let+ (((&values &ign ty) (ty-exp tyenv prog)))
       (values `((nil ,ty)) tyenv)))))

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
     `(ty-fun ,(subst-type subst ?ty1) ,(subst-type subst ?ty2)))
    ((ty-cons ?ty)
     `(ty-cons ,(subst-type subst ?ty)))
    (:_ (error (make-miniml-error "subst-type: not implemented: ~a" ty)))))

(defun subst-tysc (subst tysc)
  "型スキームTYSCの中の型変数を型代入SUBSTで置き換えます。
   TYSCが束縛する型変数はSUBSTに現れないものとします"
  (make-tysc (vars-of-tysc tysc)
	     (subst-type subst (ty-of-tysc tysc))))

(defun eqs-of-subst (subst) subst)

(defun subst-eqs (subst ty-eqs)
  "型等式リストTY-EQSに現れる型を型代入SUBSTに基づいて置き換えます"
  
  (mapcar #'(lambda (equation)
	      (dbind (ty1 ty2) equation
		`(,(subst-type subst ty1) ,(subst-type subst ty2))))
	  ty-eqs))

(defun subst-tyenv (subst tyenv)
  "型環境TYENVの型スキームの型を型代入リストSUBSTに基づいて置き換た型環境を返します"
  (env-add-binds
   (reverse (env-map #'(lambda (id tysc)
			 `(,id ,(subst-tysc subst tysc)))
		     tyenv))
   (env-empty)))

(defun unify (ty-eqs)
  "与えられた型等式リストが全て等しくなる型代入を求めます"
  
  (if (null ty-eqs)
      '()
      (dbind (ty-eq . rest) ty-eqs
	(dbind (ty1 ty2) ty-eq
	  (case-match ty-eq
	    ((?ty1 ?ty2) :where (equal ?ty1 ?ty2)
	     (unify rest))
	    (((ty-cons ?ty1) (ty-cons ?ty2))
	     (unify `((,?ty1 ,?ty2) ,@rest)))
	    (((ty-fun ?ty11 ?ty12) (ty-fun ?ty21 ?ty22))
	     (unify `((,?ty11 ,?ty21) (,?ty12 ,?ty22) ,@rest)))
	    (((ty-var :_) :_)
	     (when (member ty1 (freevar-ty ty2))
	       (error (make-miniml-error "occur check failed: ~A ~A" ty1 ty2)))
	     (let*
		 ((subst1 `(,ty1 ,ty2))
		  (subst `(,subst1))
		  (substed-rest (subst-eqs subst rest)))
	       `(,subst1 ,@(unify substed-rest))))
	    ((:_ (ty-var :_))
	     (unify `((,ty2 ,ty1) ,@rest)))
	    (:_
	     (error (make-miniml-error "cannot unify: ~A ~A" ty1 ty2))))))))

(defun make-tysc (vars ty)
  `(,vars ,ty))

(defun tysc-of-ty (ty)
  "型TYを何も束縛しない型スキームに変換する"
  `(() ,ty))

(defun ty-of-tysc (tysc)
  "型スキームの型"
  (cadr tysc))

(defun vars-of-tysc (tysc)
  "型スキームが束縛する型変数"
  (car tysc))

(defun freevar-of-tysc (tysc)
  "型スキームTYSCに含まれる自由な型変数のリスト"
  (set-difference (freevar-ty (ty-of-tysc tysc))
		  (vars-of-tysc tysc)
		  :test #'equal))

;;; (* New! 下の説明を参照 *)
;;; let closure ty tyenv subst =
;;;   let fv_tyenv' = freevar_tyenv tyenv in
;;;   let fv_tyenv =
;;;     MySet.bigunion
;;;       (MySet.map
;;;          (fun id -> freevar_ty (subst_type subst (TyVar id)))
;;;          fv_tyenv') in
;;;  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
;;;     TyScheme (MySet.to_list ids, ty)

(defun ty-closure (ty tyenv subst)
  "型TYの自由な型変数から型環境TYENVに出現する自由な型変数を型置換SUBSTで置換したものに既に出現する型変数を取り除いて束縛した型スキーム"
  (let+ ((fv-tyenv (freevar-tyenv tyenv))
	 (substed-fv-tyenv (apply #'big-union
				  (mapcar #'(lambda (var) (freevar-ty (subst-type subst var)))
					  fv-tyenv)))
	 (vars (set-difference (freevar-ty ty) substed-fv-tyenv :test #'equal)))
    (make-tysc vars ty)))

(defun freevar-tyenv (tyenv)
  "TYENVに現れる全ての型スキームに含まれる自由な型変数のリスト"
  (apply #'big-union
	 (env-map #'(lambda (var tysc)
		      (declare (ignore var))
		      (freevar-of-tysc tysc))
		  tyenv)))

(defun big-union (&rest sets)
  (reduce #'union sets :initial-value '()))
