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

(defvar *form*)
(defvar *env*)

(setf (documentation '*form* 'variable) "Bound in COERCE's compiler-macro.")
(setf (documentation '*env*  'variable) "Bound in COERCE's compiler-macro.")

(define-condition coerce-optim-failure (condition)
  ((header :initform (format nil "; Unable to optimize
;   ~S
; because~%"
                             *form*)
           :allocation :instance)))

(define-condition no-coercion (coerce-optim-failure)
  ((from :initarg :from)
   (to   :initarg :to))
  (:report (lambda (condition stream)
             (with-slots (header from to) condition
               (format stream "~A;   no coercion found from ~S to ~S"
                       header
                       from
                       to)))))

(define-condition no-output-type-spec (coerce-optim-failure)
  ((output-type-spec :initarg :output-type-spec))
  (:report (lambda (condition stream)
             (with-slots (header output-type-spec) condition
               (format stream "~A;   unable to infer the value of ~S at compile time"
                       header
                       output-type-spec)))))

(define-condition no-object-type (coerce-optim-failure)
  ((object :initarg :object)
   (to     :initarg :to))
  (:report (lambda (condition stream)
             (with-slots (header object to) condition
               (format stream "~A;   unable to infer the type of ~S at compile time
;   and no coercion known from T to ~S"
                       header
                       object
                       to)))))

(defun form-type (form)
  "Returns two values: the first value is the declared type
if the second value is non-nil"
  (cond ((constantp form)
         (values (type-of (ie:constant-form-value form *env*))
                 t))
        ((symbolp form)
         (values (ie:variable-type form *env*)
                 (cdr (assoc 'type (nth-value 2 (ie:variable-information form *env*))))))
        ((eq 'the (first form))
         (values (second form)
                 t))
        (t
         (values nil nil))))

(define-compiler-macro coerce (&whole form object output-type-spec &environment env)
  (when (< (ie:policy-quality 'speed env) 3)
    (return-from coerce form))
  (let ((*form* form)
        (*env*  env))
    (handler-case
        (progn
          (unless (constantp output-type-spec env)
            (error 'no-output-type-spec :output-type-spec output-type-spec))
          (setq output-type-spec (ie:constant-form-value output-type-spec))
          (multiple-value-bind (t-coercion exact) (coercion t output-type-spec)
            (when (and exact t-coercion)
              (return-from coerce `(the ,output-type-spec (funcall ,t-coercion ,object))))
            (multiple-value-bind (object-type type-known-p) (form-type object)
              (cond ((and (not type-known-p) (not t-coercion))
                     (error 'no-object-type :object object :to output-type-spec))
                    ((and type-known-p (not t-coercion))
                     (let ((coercion (coercion object-type output-type-spec)))
                       (if coercion
                           (return-from coerce
                             `(the ,output-type-spec (,(coercion-fbody coercion)
                                                      ,object)))
                           (error 'no-coercion :to output-type-spec :from object-type))))))))
      (coerce-optim-failure (c)
        (format *error-output* "~&~A" c)
        form))))
