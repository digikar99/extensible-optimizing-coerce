(defpackage :trivial-coerce
  (:use :cl :ctype :polymorphic-functions)
  (:shadowing-import-from
   :polymorphic-functions.extended-types
   #:*extended-type-specifiers*
   #:*subtypep-alist*
   #:extended-type-specifier-p
   #:type-specifier-p
   #:supertypep
   #:subtypep
   #:typep
   #:type=
   #:type-pair-=)
  (:shadow #:coerce)
  (:export
   #:coerce
   #:define-coercion
   #:undefine-coercion
   #:list-all-coercions))

(in-package :trivial-coerce)
(setf (alexandria:assoc-value *subtypep-alist*
                              (cons '(and symbol trivial-types:character-designator)
                                    nil)
                              :test #'type-pair-=)
      nil)
