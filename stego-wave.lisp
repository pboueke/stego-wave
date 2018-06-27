;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

;;; Globals

(defparameter *header-size* 64)
(defparameter *message-header-size* 32)

(defmacro while (condition &body body)
  `(loop while ,condition
      do (progn ,@body)))

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

(defun decimal-to-8bit-array (n)
    "Generates an 8bit representation of a decimal"
    (let ((arr (decimal-to-binary-list n)))
        (while (< (list-length arr) 8)
            (setq arr (append '(0) arr)))
        arr))

(defun decimal-to-binary-list (n)
    "Generates an binary array representation of a decimal"
    (cond ((= n 0) (list 0))
          ((= n 1) (list 1))
          (t (nconc (decimal-to-binary-list (truncate n 2)) (list (mod n 2))))))

(defun calculate-decimal (dec mult bin)
    "Recursively calculates a 'dec'imal number given a 'bin'ary array"
    (if bin 
        (calculate-decimal 
            (+ dec (* (car (reverse bin)) mult))
            (* mult 2)
            (reverse (cdr (reverse bin))))
        dec))

(defun get-decimal-from-binary-list (bin)
    (calculate-decimal 0 1 bin))

(defun overwrite-lsb (inbyte inbit)
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
                (append list (decimal-to-8bit-array inbyte)) 
                (+ 1 read-counter)))))

(defun parse-wav-header (in out)
    "Parses the header of a .WAV file and wtites it to another empty file"
    (let ((counter  0))
        (format t "=> Parsing file header ~%")
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (if out (write-byte inbyte out))
                (setq counter (+ counter 1))
                (if (>= counter *header-size*) (return-from parse-wav-header))))))

(defun parse-message-header (in)
    "Reads the first *message-header-size* bits and returns the message-header (size) as a decimal"
    (let ((counter 0) (parsed 0) (header '()))
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (setq counter (+ 1 counter))
                (when (eq 0 (mod counter 2))
                    (setq header (append header (list (get-lsb inbyte))))
                    (setq parsed (+ 1 parsed)))
                (if (>= parsed *message-header-size*)
                        (return-from parse-message-header (get-decimal-from-binary-list header)))))))

                                

(defun write-message-header (message in out)
    "Writes the size of the message at the first *message-header-size* available bits"
    (let ((counter 0) (written 0)
          (message-header (decimal-to-binary-list (list-length message))))
        (while (< (list-length message-header) *message-header-size*) 
            (setq message-header (append '(0) message-header)))
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (setq counter (+ 1 counter))
                (if (eq 0 (mod counter 2))
                    (progn 
                        (write-byte (overwrite-lsb inbyte (car message-header)) out)
                        (setq written (+ 1 written))
                        (setq message-header (cdr message-header))
                        (if (>= written *message-header-size*) 
                            (return-from write-message-header)))
                    (write-byte inbyte out))))))


(defun write-message-bit (byte message out)
    "Writes a byte to a file overwriting its LSB using the first bit of a bit array message"
    (let ((bit (car message)))
        (if bit
            (write-byte (overwrite-lsb byte bit) out)
            (write-byte (overwrite-lsb byte (random 2)) out))
        (cdr message)))

(defun write-message (message in out)
    "Writes a message using the LSB method on a .WAV file"
    (let ((counter 0) (message-size (list-length message)))
        (format t "=> Writing data ~%")
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn
                (setq counter (+ 1 counter))
                (if (eq 0 (mod counter 2))
                    (setq message (write-message-bit inbyte message out))
                    (write-byte inbyte out))))
        (if (> message-size (/ counter 2))
            (error "Message size ~d larger than host capacity ~d" message-size counter))))

(defun read-message (size in out)
    "Reads an inputed .wav file and writes the hidden message to an out file using the LSB method"
    (let ((counter 0) (parsed 0) (outbyte (list)))
        (format t " * Message size: ~a bits~%" size)
        (loop for inbyte = (read-byte in nil) while inbyte do
            (progn 
                (setq counter (+ 1 counter))
                (when (eq 0(mod counter 2))
                    (setq outbyte (append outbyte (list (get-lsb inbyte))))
                    (setq parsed (+ 1 parsed)))
                (when (and (eq 0 (mod (list-length outbyte) 8)) (not (eq (list-length outbyte) 0)))
                    (write-byte (get-decimal-from-binary-list outbyte) out)
                    (setq outbyte (list))
                    (when (>= parsed size)
                        (return-from read-message)))))))

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
            (format t " * Total parsed message size: ~a bits ~%" (list-length content))
            (parse-wav-header original new)
            (write-message-header content original new)
            (write-message content original new))
        (close original)
        (close new)))

(defun read-op (params)
    "The read operation function"
    (format t "=> Starting READ operation ~%")
    (let ((original (open (get-param :host params)
                    :element-type '(unsigned-byte 8)))
         (message (open (get-param :result params) 
                    :element-type '(unsigned-byte 8)
                    :direction :output 
                    :if-exists :supersede)))
        (parse-wav-header original nil)
        (read-message (parse-message-header original) original message)
        (close original)
        (close message)))

;;; Entry point

(defun main ()
    (defparameter *parsedparams* (apply-argv:parse-argv (apply-argv:get-argv)))
    (validate-params *parsedparams*)
    (if (get-flag :write *parsedparams*)
        (write-op *parsedparams*)
        (read-op *parsedparams*))
    (format t "=> All done!~%"))