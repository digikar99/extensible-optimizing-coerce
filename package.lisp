#+extensible-compound-types
(defpackage :trivial-coerce
  (:use :extensible-compound-types-cl)
  (:shadow #:coerce)
  (:export
   #:coerce
   #:define-coercion
   #:undefine-coercion
   #:list-all-coercions))

#-extensible-compound-types
(defpackage :trivial-coerce
  (:use :cl)
  (:import-from :extensible-compound-types
                #:clhs-class-from-type-spec
                #:clhs-class-from-object)
  (:import-from :extensible-compound-types #:typexpand)
  (:shadow #:coerce)
  (:export
   #:coerce
   #:define-coercion
   #:undefine-coercion
   #:list-all-coercions))

(in-package :trivial-coerce)
