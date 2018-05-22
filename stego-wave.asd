;;;; stego-wave.asd

(asdf:defsystem #:stego-wave
  :description "Simple tool for .wav steganography."
  :author "pboueke"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:apply-argv)
  :components ((:file "package")
               (:file "stego-wave")))
