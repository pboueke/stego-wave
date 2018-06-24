;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

;;; Globals

(defparameter *header-size* 64)

;;; Parameters Utils

(defun get-param (keyname params)
    "Gets the keyname parameter value from the parsed arguments"
    (nth (+ 1 (position keyname params)) params))

(defun get-flag (flagname params)
    "Gets the value of flags from the parsed arguments"
    (if (find flagname params)
        (get-param flagname params)
        NIL))

(defun validate-write-params (params)
    "Validates parameters for the write operation"
    (validate-param :host    params)
    (validate-param :message params)
    (validate-param :result  params))

(defun validate-read-params (params)
    "Validated parameters for the read operation"
    (validate-param :host    params)
    (validate-param :result  params))

(defun validate-operation (params)
    "Validates the operation selected by the user"
    (if (eq (get-flag :write params) (get-flag :read params))
        (error "[ERROR] Must choose either --write or --read operation")))
    
(defun validate-param (param params)
    "Throws an error if the parameter param is not present at params"
    (if (or (not (position param params)) (not (get-param param params)))
        (error "[ERROR] Missing required parameter ~a" param)))

(defun validate-params (params)
    "Validates all parameters"
    (format t " * Parsed parameters: ~a ~%" params)
    (validate-operation params)
    (if (get-flag :write params)
        (validate-write-params params)
        (validate-read-params params)))

;;; Data Utils

(defun decimal-to-binary-list (n)
    "Transforms a decimal into a binary list"
    (cond ((= n 0) (list 0))
        ((= n 1) (list 1))
        (t (nconc (decimal-to-binary-list (truncate n 2)) (list (mod n 2))))))

(defun ovewrite-lsb (inbyte inbit)
    "Overwrites the last bit from a byte"
    (if (eq (mod inbyte 2) 0)
        (if (eq inbit 0)
            inbyte 
            (+ inbyte 1))
        (if (eq inbit 1)
            inbyte
            (- inbyte 1))))

(defun get-lsb (inbyte)
    "Returns the last bit from a byte"
    (mod inbyte 2))

;;;; File Utils

(defun get-bits-from-file(in list read-counter)
    "Returns the contents of a file as a binary list"
    (let ((inbyte (read-byte in nil)))
        (if (eq 0 (mod read-counter (* 1024 1)))
            (format t " * Partial parsed message size: ~a bytes ~%" read-counter))
        (if (not inbyte)
            list
            (get-bits-from-file in 
                (append list (decimal-to-binary-list inbyte)) 
                (+ 1 read-counter)))))

(defun parse-wav-header (in out)
    "Parses the header of a .WAV file and wtites it to another empty file"
    (let ((counter  0))
        (format t "=> Parsing file header ~%")
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (write-byte inbyte out)
                (if (> counter *header-size*) (return-from parse-wav-header))
                (setq counter (+ counter 1))))))

(defun write-message-bit (byte message out)
    "Writes a byte to a file overwriting its LSB using the first bit of a bit array message"
    (let ((bit (car message)))
        (if bit
            (write-byte (ovewrite-lsb byte bit) out)
            (write-byte (ovewrite-lsb byte (random 2)) out))
        (cdr message)))

(defun write-message (message in out)
    "Writes a message using the LSB method on a .WAV file"
    (let ((counter  0) (message-size (list-length message)))
        (format t "=> Writing data ~%")
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (if (eq 0 (mod counter 2))
                    (setq message (write-message-bit inbyte message out))
                (setq counter (+ 1 counter))))
        (if (> counter message-size) 
            (error "Message size larger than host capacity")))))

;;; Stego utils

(defun write-op (params)
    "The write operation function"
    (format t "=> Starting WRITE operation~%")
    (let ((message (open (get-param :message params) 
                    :element-type '(unsigned-byte 8)))
          (original (open (get-param :host params) 
                    :element-type '(unsigned-byte 8)))
          (new (open (get-param :result params) 
                    :element-type '(unsigned-byte 8)
                    :direction :output 
                    :if-exists :supersede)))
        (let ((content (get-bits-from-file message '() 0)))
            (close message)
            (format t " * Total parsed message size: ~a bytes ~%" (list-length content))
            (parse-wav-header original new)
            (write-message content original new))
        (close original)
        (close new)))

(defun read-op (params)
    "The read operation function"
    (format t "=> Starting READ operation ~%"))

;;; Entry point

(defun main ()
    (defparameter *parsedparams* (apply-argv:parse-argv (apply-argv:get-argv)))
    (validate-params *parsedparams*)
    (if (get-flag :write *parsedparams*)
        (write-op *parsedparams*)
        (read-op *parsedparams*)))
    