(defpackage :trivial-coerce.optim
  (:use :cl :trivial-coerce)
  (:shadowing-import-from :trivial-coerce :coerce)
  (:import-from
   :trivial-coerce
   :coercion
   :coercion-fbody
   :coercion-function)
  (:local-nicknames (:tt :trivial-types)
                    (:ie :introspect-environment)))

(in-package :trivial-coerce.optim)

;; TODO: Abstract this out either using COMPILER-MACRO system
;; or some other better system

(define-condition no-coercion (compiler-macro-notes:optimization-failure-note)
  ((from :initarg :from)
   (to   :initarg :to))
  (:report (lambda (condition stream)
             (with-slots (from to) condition
               (format stream "No coercion found from ~S to ~S"
                       from
                       to)))))

(define-condition potentially-invalid-output-type-spec
    (compiler-macro-notes:optimization-failure-note)
  ((output-type-spec-type :initarg :output-type-spec-type))
  (:report (lambda (condition stream)
             (with-slots (output-type-spec-type) condition
               (format stream "Unable to infer OUTPUT-TYPE-SPEC from form derived to be of type ~S"
                       output-type-spec-type)))))

(define-condition no-object-type (compiler-macro-notes:optimization-failure-note)
  ((object :initarg :object)
   (to     :initarg :to))
  (:report (lambda (condition stream)
             (with-slots (object to) condition
               (format stream "Unable to infer the type of ~S at compile time
and no coercion known from T to ~S"
                       object
                       to)))))

(define-compiler-macro coerce (&whole form object output-type-spec &environment env)
  (compiler-macro-notes:with-notes
      (form env :optimization-note-condition (= 3 (ie:policy-quality 'speed env)))
    (when (< (ie:policy-quality 'speed env) 3)
      (return-from coerce form))
    (let* ((object-type (cl-form-types:nth-form-type object env 0 t t))
           (output-type-spec-type
             (cl-form-types:nth-form-type output-type-spec env 0 t t))
           (output-type-spec (if (and (listp output-type-spec-type)
                                      (or (member (first output-type-spec-type)
                                                  '(eql member))))
                                 (second output-type-spec-type)
                                 (signal 'potentially-invalid-output-type-spec
                                         :output-type-spec-type output-type-spec-type))))
      ;; FIXME: Does it matter if the COERCION is EXACT?
      (multiple-value-bind (t-coercion exact) (coercion object-type output-type-spec)
        (declare (ignore exact))
        (cond (t-coercion
               (return-from coerce `(the ,output-type-spec
                                         (,(coercion-fbody t-coercion) ,object))))
              ((eq t object-type)
               (signal 'no-object-type :object object :to output-type-spec))
              (t
               (signal 'no-coercion :to output-type-spec :from object-type)))))))
