
(defsystem "extensible-optimizing-coerce"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "`extensible-optimizing-coerce` primarily provides a `extensible-optimizing-coerce:coerce` function intended as an extensible alternative to `cl:coerce`."
  :depends-on ("trivial-types"
               "closer-mop"
               "optima"
               "extensible-compound-types"
               "extensible-compound-types-interfaces")
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "tables")
               (:file "coerce")
               (:file "coercions"))
  :in-order-to ((test-op (test-op "extensible-optimizing-coerce/tests"))))

(defsystem "extensible-optimizing-coerce/tests"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Test system for trivial-coerce."
  :depends-on ("extensible-optimizing-coerce"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c)
             (eval (read-from-string
                    "(LET ((5AM:*ON-ERROR* :DEBUG)
                           (5AM:*ON-FAILURE* :DEBUG))
                       (5AM:RUN! :EXTENSIBLE-OPTIMIZING-COERCE))"))))
