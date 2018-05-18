#|
  This file is a part of stego-wave project.
|#

(defsystem "stego-wave-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Pedro Boueke"
  :license ""
  :depends-on ("stego-wave"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "stego-wave"))))
  :description "Test system for stego-wave"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
