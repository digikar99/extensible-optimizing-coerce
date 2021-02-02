(defpackage :trivial-coerce
  (:use :cl)
  (:shadow :coerce)
  (:local-nicknames (:tt :trivial-types)
                    (:ie :introspect-environment))
  (:export
   :coerce
   :define-coercion
   :undefine-coercion
   :list-all-coercions))

(in-package :trivial-coerce)

;; TODO: Define a hashing function for type-specifiers using CL-CUSTOM-HASH-TABLE
;; Need to handle the notion of subtypes
(defparameter *coercion-alist* nil
  "An ALIST of (ALIST of APPLICABLE-P and COERCIONs). The APPLICABLE-P function
is a single argument function that takes an object and checks if the coercion is applicable.")

(defstruct coercion
  fbody
  function
  (applicable-p nil :type function)
  from)

(defun supertypep (type1 type2 &optional env)
  (subtypep type2 type1 env))

(defun type-intersects-p (type1 type2 &optional env)
  (or (subtypep type1 type2 env)
      (subtypep type2 type1 env)))

(defun type= (type1 type2 &optional env)
  (and (subtypep type1 type2 env)
       (subtypep type2 type1 env)))

(defun list-all-coercions (&optional (to nil to-p))
  (reduce #'nconc
          (mapcar (lambda (to-coercions-pair)
                    (destructuring-bind (to &rest coercions) to-coercions-pair
                      (mapcar (lambda (coercion)
                                (if to-p
                                    (coercion-from coercion)
                                    (list :from (coercion-from coercion)
                                          :to to)))
                              coercions)))
                  (if to
                      (list (assoc to *coercion-alist* :test 'subtypep))
                      *coercion-alist*))
          :initial-value ()))

(define-condition no-coercion (error)
  ((from :initarg :from)
   (to   :initarg :to))
  (:report (lambda (condition stream)
             (with-slots (from to) condition
               (format stream "No coercion found from ~S to ~S"
                       from
                       to)))))

(defun applicable-coercion (object to)
  "Returns two values
  - first value is the coercion function to be funcall-ed
  - second value is the lambda expression of the function for inline and optimization"
  (declare (type tt:type-specifier to)
           (optimize speed))
  (let ((coercion (block %coercion
                    (loop :for (to-type . to-coercions) :in *coercion-alist*
                          :when (subtypep to-type to)
                            :do (loop :for coercion :in to-coercions
                                      :if (funcall (coercion-applicable-p coercion)
                                                   object)
                                        :do (return-from %coercion coercion))))))
    (if coercion
        (values (coercion-function coercion)
                (coercion-fbody    coercion))
        (values nil nil))))

(defun coercion (from to)
  (declare (type tt:type-specifier to))
  (let* ((to-coercions (cdr (assoc to *coercion-alist* :test 'type=))))
    (or (loop :for coercion :in to-coercions
              :if (type= from (coercion-from coercion))
                :do (return-from coercion (values coercion t)))
        (loop :for (to-type . to-coercions) :in *coercion-alist*
              :when (subtypep to-type to)
                :do (loop :for coercion :in to-coercions
                          :if (subtypep from (coercion-from coercion))
                            :do (return-from coercion
                                  (values coercion nil)))))))

(defun undefine-coercion (from to)
  "Removes a coercion TYPE= to FROM and TO."
  (declare (type tt:type-specifier from to))
  (let ((to-coercions (cdr (assoc to *coercion-alist* :test 'type=))))
    (if (and to-coercions (find-if (lambda (coercion) (type= from (coercion-from coercion)))
                                   to-coercions))
        (setf (cdr (assoc to *coercion-alist* :test 'type=))
              (remove-if (lambda (coercion)
                           (type= from (coercion-from coercion)))
                         to-coercions))
        (cerror "Ignore" 'no-coercion :from from :to to))
    nil))

(defun (setf coercion) (coercion from to)
  (declare (type tt:type-specifier from to))
  (let ((to-coercions (cdr (assoc to *coercion-alist* :test 'type=))))
    (declare (type list to-coercions))
    (if to-coercions
        (let ((from-coercion (find-if (lambda (coercion)
                                        (type= from (coercion-from coercion)))
                                      to-coercions)))
          (if from-coercion
              (setf (coercion-fbody    from-coercion) (coercion-fbody    coercion)
                    (coercion-function from-coercion) (coercion-function coercion))
              (setf (cdr to-coercions)
                    (cons coercion
                          (cdr to-coercions)))))
        (push (cons to
                    (list coercion))
              *coercion-alist*))))

(defun coerce (object output-type-spec)
  "Converts OBJECT to type specified by OUTPUT-TYPE-SPEC. To do so, the system
internally makes use of coercions (lambda functions) defined using DEFINE-COERCION.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of (sub)type specified by OUTPUT-TYPE-SPEC. If multiple
coercions are applicable, the specific coercion that is called is undefined.

For instance, consider two coercions defined as:

    (define-coercion (list :from list :to string) (write-to-string list))
    (define-coercion (list :from list :to vector) (cl:coerce list 'vector))

Then, the value of `(coerce '(1 2 3) 'vector)` is permitted to be `\"(1 2 3)\"`.
One may use `(coerce '(1) '(and vector (not string)))` to obtain the expected."
  (declare (type tt::type-specifier output-type-spec))
  (if (typep object output-type-spec)
      object
      (let ((coerce-function (nth-value 0 (applicable-coercion object output-type-spec))))
        (if coerce-function
            (funcall coerce-function object)
            (error 'simple-type-error
                   :format-control "No coercion defined from ~S of type ~S to ~S. Available coercions include:~%  ~{~S~^~%  ~}"
                   :format-arguments (list object
                                           (type-of object)
                                           output-type-spec
                                           (list-all-coercions)))))))

(defmacro define-coercion ((var &key (to nil to-p) (from t) (if-exists :supersede)) &body body)
  "Defines a coercion for use by TRIVIAL-COERCE:COERCE for VAR of type FROM to type TO.
Assumes the coercion to be valid in null lexical environment.
If a TYPE= coercion is available, and
  - if IF-EXISTS is :OVERWRITE, then the same coercion will be overwritten.
  - if IF-EXISTS is :SUPERSEDE, then the existing coercion will be first deleted.
    In practice, this means that :SUPERSEDE also replaces the stored type-specs, while
    :OVERWRITE leaves the stored type-specs unchanged.
  - if IF-EXISTS is :ERROR, an ERROR is signalled"
  (assert to-p () "TO type-specifier has to be specified")
  (let ((coercion-fbody (gensym "COERCION-FBODY"))
        (coercion       (gensym "COERCION")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,coercion-fbody
                `(lambda (,',var)
                   (declare (type ,',from ,',var)
                            (ignorable ,',var))
                   ;; TODO: Should BODY have access to the exact typespec of the caller?
                   ;; Perhaps no because, then THE form cannot be used
                   (the ,',to
                        (block coerce
                          (nth-value 0 (locally ,@',body))))))
              (,coercion (make-coercion :fbody ,coercion-fbody
                                       :function (compile nil ,coercion-fbody)
                                       :from ',from
                                       :applicable-p (compile nil
                                                              `(lambda (object)
                                                                 (typep object ',',from))))))
         (when (coercion ',from ',to)
           ,(ecase if-exists
              (:overwrite nil)
              (:supersede `(undefine-coercion ',from ',to))
              (:error `(error "A coercion TYPE= from ~S to ~S already exists" ',from ',to))))
         (setf (coercion ',from ',to) ,coercion)
         t))))




