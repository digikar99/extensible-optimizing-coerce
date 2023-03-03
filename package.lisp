(defpackage :trivial-coerce
  (:use :cl)
  (:import-from :extensible-compound-types
                #:clhs-class-from-type-spec
                #:clhs-class-from-object
                #:specializing-type-specifier-p
                #:specializing
                #:upgraded-cl-type)
  (:import-from :extensible-compound-types #:typexpand #:type=)
  (:shadow #:coerce)
  (:export
   #:coerce
   #:define-coercion
   #:undefine-coercion
   #:list-all-coercions))

(in-package :trivial-coerce)
