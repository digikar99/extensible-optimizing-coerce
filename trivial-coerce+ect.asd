 
(defsystem "trivial-coerce+ect"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "`trivial-coerce` primarily provides a `trivial-coerce:coerce` function intended as an extensible alternative to `cl:coerce`."
  :depends-on ("trivial-types"
               "ctype" ;; needed for ctype::+floats+
               "polymorphic-functions+extensible-compound-types")
  :version "0.0.2"
  :serial t
  :pathname #P"extensible-compound-types/"
  :components ((:file "package")
               (:file "trivial-coerce")
               (:file "coercions"))
  :in-order-to ((test-op (test-op "trivial-coerce+ect/tests"))))

(defsystem "trivial-coerce+ect/tests"
  :licence "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "Test system for trivial-coerce."
  :depends-on ("trivial-coerce+ect"
               "fiveam")
  :components ((:file "tests"))
  :pathname #P"extensible-compound-types/"
  :perform (test-op (o c)
                    (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                                   (5AM:*ON-FAILURE* :DEBUG))
                                              (5AM:RUN! :TRIVIAL-COERCE))"))))
