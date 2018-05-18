#|
  This file is a part of stego-wave project.
|#

(defsystem "stego-wave"
  :version "0.1.0"
  :author "Pedro Boueke"
  :license "MIT"
  :depends-on ("apply-argv")
  :components ((:module "src"
                :components
                ((:file "stego-wave"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "stego-wave-test"))))
