;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

;;; Parameters utilities

(defun get-param (keyname params)
    (nth (+ 1 (position keyname params)) params))

(defun get-flag (flagname params)
    (if (find flagname params)
        (get-param flagname params)
        NIL))

(defun validate-write-params (params)
    (validate-param :host    params)
    (validate-param :message params)
    (validate-param :result  params))

(defun validate-read-params (params)
    (validate-param :host    params)
    (validate-param :result  params))

(defun validate-operation (params)
    (if (eq (get-flag :write params) (get-flag :read params))
        (error "[ERROR] Must choose either --write or --read operation")))
    
(defun validate-param (param params)
    (if (or (not (position param params)) (not (get-param param params)))
        (error "[ERROR] Missing required parameter ~a" param)))

(defun validate-params (params)
    (format t "Parsed parameters: ~a ~%" parsedparams)
    (validate-operation params)
    (if (get-flag :write params)
        (validate-write-params params)
        (validate-read-params params)))

;;; Entry point

(defun main ()
    (defparameter parsedparams (apply-argv:parse-argv (apply-argv:get-argv)))
    (validate-params parsedparams))
    