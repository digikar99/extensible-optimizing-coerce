(in-package :trivial-coerce)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun coerce-error (name arg-list &optional env)
    ;; This is a commentary from the time when COERCE was for non-extended types:
    ;; Should we use CTYPE here? Naah, just let users use this as a DROP-IN replacement
    ;; without having to think twice.
    (declare (ignorable name arg-list))
    (destructuring-bind (object output-type-spec) arg-list
      (if *compiler-macro-expanding-p*
          (no-applicable-polymorph name arg-list env)
          (if (eq t output-type-spec)
              object
              (error 'simple-type-error
                     :format-control "No coercion defined from ~S of type ~S to ~S. Available coercions include:~%  ~{~S~^~%  ~}"
                     :format-arguments (list object
                                             (type-of object)
                                             output-type-spec
                                             (list-all-coercions))))))))

(define-polymorphic-function coerce (object output-type-spec)
  :default #'coerce-error
  :documentation
   "Converts OBJECT to type specified by OUTPUT-TYPE-SPEC. To do so, the system
internally makes use of coercions (lambda functions) defined using DEFINE-COERCION.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of (sub)type specified by OUTPUT-TYPE-SPEC. If multiple
coercions are applicable, the most specialized coercion is selected.

For instance, consider two coercions defined as:

    (define-coercion (list :from list :to string) (write-to-string list))
    (define-coercion (list :from list :to vector) (cl:coerce list 'vector))

FIXME: Then, the value of `(coerce '(1 2 3) 'vector)` is permitted to be `\"(1 2 3)\"`.
One may use `(coerce '(1) '(and vector (not string)))` to obtain the expected.")

(defmacro define-coercion ((var &key (to nil to-p) (from t))
                           &body body)
  "Defines a coercion for VAR of type FROM to type TO.
Assumes the coercion to be valid in null lexical environment.
If a TYPE= coercion is available, and
  - if IF-EXISTS is :OVERWRITE, then the same coercion will be overwritten.
  - if IF-EXISTS is :SUPERSEDE, then the existing coercion will be first deleted.
    In practice, this means that :SUPERSEDE also replaces the stored type-specs, while
    :OVERWRITE leaves the stored type-specs unchanged.
  - if IF-EXISTS is :ERROR, an ERROR is signalled"
  (declare (ignore to-p))
  (alexandria:with-gensyms (output-type-spec)
    `(defpolymorph coerce ((,var ,from) (,output-type-spec (supertypep ,to))) ,to
       (declare (ignorable ,var ,output-type-spec))
       ,@body)))

(defun undefine-coercion (from to)
  "Removes a coercion TYPE= to FROM and TO."
  ;; FIXME: TYPE= checks in polymorphic-functions
  (undefpolymorph 'coerce `(,from (supertypep ,to))))

(defun list-all-coercions (&optional (to nil to-p))
  (remove-duplicates (mapcar (lambda (type-list)
                               (destructuring-bind (%from (supertypep %to)) type-list
                                 (declare (ignore supertypep))
                                 (if to-p
                                     %from
                                     (list :from %from :to %to))))
                             (remove-if-not (lambda (type-list)
                                              (destructuring-bind (%from (supertypep %to))
                                                  type-list
                                                (declare (ignore %from supertypep))
                                                (if to-p
                                                    (subtypep to %to)
                                                    t)))
                                            (polymorphic-function-type-lists
                                             (fdefinition 'coerce))))
                     :test #'equalp))
