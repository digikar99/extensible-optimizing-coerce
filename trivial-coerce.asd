 
(defsystem "trivial-coerce"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "`trivial-coerce` primarily provides a `trivial-coerce:coerce` function intended as an extensible alternative to `cl:coerce`."
  :depends-on ("trivial-types"
               "closer-mop"
               "optima"
               "extensible-compound-types"
               (:feature :extensible-compound-types "extensible-compound-types-cl"))
  :version "0.0.3"
  :serial t
  :components ((:file "package")
               (:file "tables")
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
  :perform (test-op (o c)
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                        (5AM:RUN! :TRIVIAL-COERCE))"))))
