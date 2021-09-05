(defpackage :trivial-coerce
  (:use :cl :ctype :polymorphic-functions)
  (:shadowing-import-from
   :polymorphic-functions.extended-types
   #:*extended-type-specifiers*
   #:extended-type-specifier-p
   #:type-specifier-p
   #:supertypep
   #:subtypep
   #:typep
   #:type=)
  (:shadow #:coerce)
  (:export
   #:coerce
   #:define-coercion
   #:undefine-coercion
   #:list-all-coercions))
