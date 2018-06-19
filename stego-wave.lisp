;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

;;; General macros

(defmacro while (condition &body body)
    `(loop while ,condition
        do (progn ,@body)))

;;; Utilities

(defun get-param (keyname params)
    (nth (+ 1 (position keyname params)) params))

(defun validate-write-params (params)
    (validate-param :host    param)
    (validate-param :message param)
    (validate-param :result  param))
    
    
(defun validate-param (param params)
    (if (or (not (position param params)) (not (get-param param params)))
        (format t "Parameter ~a is required ~%" param)
        (error "Missing required parameter."))

;;; Entry point

(defun main ()
    (defparameter parsedparams (apply-argv:parse-argv (apply-argv:get-argv)))
    (format t "Parsed parameters: ~a ~%" parsedparams)
    (validate-write-params (parsedparams))    )
    