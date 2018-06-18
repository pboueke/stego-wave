;;;; stego-wave.lisp

(in-package #:stego-wave)
(use-package :apply-argv)

(apply-argv:parse-argv '("--foo" "bar"
              "--bar=qwe"
              "--qwe"
              "--no-xyz"
              "more" "stuff" "here"))

(defun main ()
    (format t "hello" ))