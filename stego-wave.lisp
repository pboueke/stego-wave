;;;; stego-wave.lisp

(in-package #:stego-wave
            #:apply-argv)

(parse-argv '("--foo" "bar"
              "--bar=qwe"
              "--qwe"
              "--no-xyz"
              "more" "stuff" "here"))

