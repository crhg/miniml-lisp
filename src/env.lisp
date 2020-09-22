(defpackage :miniml-lisp/env
  (:use :cl)
  (:import-from :let-over-lambda #:aif #:it)
  (:import-from :my-util #:dbind)
  (:import-from :miniml-lisp/error #:make-miniml-error)
  (:export
   #:env-empty
   #:env-add
   #:env-adds
   #:env-add-binds
   #:env-overwrite
   #:env-overwrite-binds
   #:env-map))
(in-package :miniml-lisp/env)

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
