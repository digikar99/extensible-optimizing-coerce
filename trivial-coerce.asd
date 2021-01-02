 
(defsystem "trivial-coerce"
  :licence "MIT"
  :depends-on ("introspect-environment"
               ;; fetch trivial-types from https://github.com/digikar99/trivial-types
               "trivial-types")
  :serial t
  :components ((:file "trivial-coerce")
               (:file "optim")
               (:file "coercions"))
  :in-order-to ((test-op (test-op "trivial-coerce/tests"))))

(defsystem "trivial-coerce/tests"
  :depends-on ("trivial-coerce"
               "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run! :trivial-coerce)))
