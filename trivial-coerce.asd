 
(defsystem "trivial-coerce"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "`trivial-coerce` primarily provides a `trivial-coerce:coerce` function intended as an extensible alternative to `cl:coerce`."
  :depends-on (;; fetch trivial-types from https://github.com/digikar99/trivial-types
               "trivial-types"
               "polymorphic-functions"
               "ctype")
  :version "0.0.2"
  :serial t
  :components ((:file "package")
               (:file "trivial-coerce")
               (:file "coercions"))
  :in-order-to ((test-op (test-op "trivial-coerce/tests"))))

(defsystem "trivial-coerce/tests"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Test system for trivial-coerce."
  :depends-on ("trivial-coerce"
               "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (eval (read-from-string "(5AM:RUN! :TRIVIAL-COERCE)"))))
