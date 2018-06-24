;;;; stego-wave.asd

(asdf:defsystem #:stego-wave
  :build-operation "program-op"
  :build-pathname "stegowave"
  :entry-point "stego-wave:main"
  :description "Simple tool for .wav steganography."
  :author "pboueke"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:apply-argv)
  :components ((:file "package")
               (:file "stego-wave")))
