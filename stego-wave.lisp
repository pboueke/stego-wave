;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

;;; General macros

(defmacro while (condition &body body)
    `(loop while ,condition
        do (progn ,@body)))

;;; Utilities

(defun getparam (keyname params)
    (nth (+ 1 (position keyname params)) params))

;;; Entry point

(defun main ()
    (print sb-ext:*posix-argv*)
    (defparameter parsedparams (apply-argv:parse-argv sb-ext:*posix-argv*))
    (print parsedparams)
    (format t "~t ~%" (getparam :host parsedparams)))