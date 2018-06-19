;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

;;; Globals

(defparameter header-size 64)

;;; Macros

(defmacro concat-string (&rest strings) `(concatenate 'string ,@strings) )

;;; Parameters Utils

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
    (format t "|* Parsed parameters: ~a ~%" parsedparams)
    (validate-operation params)
    (if (get-flag :write params)
        (validate-write-params params)
        (validate-read-params params)))

;;; Data Utils

(defun decimal-to-binary-list (n)
  (cond ((= n 0) (list 0))
        ((= n 1) (list 1))
        (t (nconc (decimal-to-binary-list (truncate n 2)) (list (mod n 2))))))

(defun ovewrite-lsb (inbyte inbit)
    (if (eq (mod inbyte 2) 0)
        (if (eq inbit 0)
            inbyte 
            (+ inbyte 1))
        (if (eq inbit 1)
            inbyte
            (- inbyte 1))))

(defun get-lsb (inbyte)
    (mod inbyte 2))

;;;; File Utils

(defun get-bits-from-file(in list read-counter)
    (let ((inbyte (read-byte in nil)))
        (if (eq 0 (mod read-counter (* 1024 1)))
            (format t "|* Partial parsed message size: ~a bytes ~%" read-counter))
        (if (not inbyte)
            list
            (get-bits-from-file in (append list 
                (decimal-to-binary-list inbyte)) (+ 1 read-counter)))))

(defun parse-wav-header (in out)
    (let ((counter  0)
          (channels ""))
        (format t "=> Parsing file header ~%")
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (write-byte inbyte out)
                (cond 
                    ((or (eq counter 22) (eq counter 23)) 
                        (setq channels (concat-string (make-string inbyte) channels)))
                    ((> counter header-size)
                        (return-from parse-wav-header 
                            (parse-integer channels :radix 16))))
                (+ counter 1)))))

;;; Stego utils

(defun write-op (params)
    (format t "=> Starting WRITE operation~%")
    (let ((message (open (get-param :message params) 
                    :element-type '(unsigned-byte 8)))
          (original (open (get-param :host params) 
                    :element-type '(unsigned-byte 8)))
          (new (open (get-param :result params) 
                    :element-type '(unsigned-byte 8)
                    :direction :output 
                    :if-exists :supersede)))
        (let ((content (get-bits-from-file message '() 0))
              (channels-number 0))
            (close message)
            (format t "|* Total parsed message size: ~a bytes ~%" (list-length content))
            (setq channels-number (parse-wav-header original new))
            (format t "|*Number of channels: ~a ~%" channels-number))
        (close original)
        (close new))

(defun read-op (params)
    (format t "=> Starting READ operation ~%"))

;;; Entry point

(defun main ()
    (defparameter parsedparams (apply-argv:parse-argv (apply-argv:get-argv)))
    (validate-params parsedparams)
    (if (get-flag :write parsedparams)
        (write-op parsedparams)
        (read-op parsedparams)))
    