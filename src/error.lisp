(defpackage :miniml-lisp/error
  (:use :cl)
  (:export
   #:miniml-error
   #:make-miniml-error))
(in-package :miniml-lisp/error)

(define-condition miniml-error (simple-error)
  ((fmt :initarg :fmt :reader miniml-error-fmt)
   (args :initarg :args :reader miniml-error-args))
  (:report (lambda (condition stream)
	     (apply #'format stream
		    (miniml-error-fmt condition)
		    (miniml-error-args condition)))))

(defmacro make-miniml-error (fmt &rest args)
  `(make-condition 'miniml-error :fmt ,fmt :args (list ,@args)))

