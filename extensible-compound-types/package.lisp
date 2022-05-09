(defpackage :trivial-coerce
  (:use :extensible-compound-types-cl :polymorphic-functions)
  (:shadow #:coerce)
  (:export
   #:coerce
   #:define-coercion
   #:undefine-coercion
   #:list-all-coercions))

(in-package :trivial-coerce)

