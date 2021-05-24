 
(defsystem "trivial-coerce"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "`trivial-coerce` primarily provides a `trivial-coerce:coerce` function that is intended as an extensible alternative to `cl:coerce`."
  :depends-on ("introspect-environment"
               ;; fetch trivial-types from https://github.com/digikar99/trivial-types
               "trivial-types"
               "cl-form-types"
               "compiler-macro-notes")
  :version "0.0.1"
  :serial t
  :components ((:file "trivial-coerce")
               (:file "optim")
               (:file "coercions"))
  :in-order-to ((test-op (test-op "trivial-coerce/tests"))))

(defsystem "trivial-coerce/tests"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Test system for trivial-coerce."
  :depends-on ("trivial-coerce"
               "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run! :trivial-coerce)))
