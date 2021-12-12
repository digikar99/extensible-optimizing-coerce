(in-package :trivial-coerce)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun coerce-error (name env args &optional arg-types)
    ;; This is a commentary from the time when COERCE was for non-extended types:
    ;; Should we use CTYPE here? Naah, just let users use this as a DROP-IN replacement
    ;; without having to think twice.
    (destructuring-bind (object output-type-spec) args
      (if *compiler-macro-expanding-p*
          (no-applicable-polymorph name env args arg-types)
          (if (eq t output-type-spec)
              object
              (error 'simple-type-error
                     :format-control "No coercion defined from ~S of type~%  ~S~%to~%  ~S~%Available coercions include:~%  ~{~S~^~%  ~}"
                     :format-arguments (list object
                                             (type-of object)
                                             output-type-spec
                                             (let ((coercions-list (list-all-coercions)))
                                               (if (and *print-length*
                                                        (> (length coercions-list) *print-length*))
                                                   (append (subseq coercions-list 0 *print-length*)
                                                           '("..."))
                                                   coercions-list)))))))))

(define-polymorphic-function coerce (object output-type-spec)
  :default #'coerce-error
  :documentation
   "Converts OBJECT to type specified by OUTPUT-TYPE-SPEC. To do so, the system
internally makes use of coercions (lambda functions) defined using DEFINE-COERCION.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of (sub)type specified by OUTPUT-TYPE-SPEC. It is guaranteed
that only a single (predefined) coercion is applicable for a given OUTPUT-TYPE-SPEC.
")

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
    `(defpolymorph (coerce :inline t) ((,var ,from) (,output-type-spec (type= ,to))) ,to
       (declare (ignorable ,var ,output-type-spec))
       ,@body)))

(defun undefine-coercion (from to)
  "Removes a coercion TYPE= to FROM and TO."
  ;; FIXME: TYPE= checks in polymorphic-functions
  (undefpolymorph 'coerce `(,from (type= ,to))))

(defun list-all-coercions (&optional (to nil to-p))
  (remove-duplicates (mapcar (lambda (type-list)
                               (destructuring-bind (%from (type= %to)) type-list
                                 (declare (ignore type=))
                                 (if to-p
                                     %from
                                     (list :from %from :to %to))))
                             (remove-if-not (lambda (type-list)
                                              (destructuring-bind (%from (type= %to))
                                                  type-list
                                                (declare (ignore %from type=))
                                                (if to-p
                                                    (subtypep to %to)
                                                    t)))
                                            (remove '(t t)
                                                    (polymorphic-function-type-lists
                                                     (fdefinition 'coerce))
                                                    :test #'equal)))
                     :test #'equalp))

(defpolymorph coerce (object output-type-spec) t
  (if (typep object output-type-spec)
      object
      (coerce-error 'coerce nil (list object output-type-spec))))
