(in-package :extensible-optimizing-coerce)

(defun class-name-from-type-spec (type-spec)
  (optima:match (typexpand type-spec)
    ((list* 'specializing class-name args)
     (values class-name args))
    ((list* class-name args)
     (values class-name args))))

(defun class-name-from-object (object) (class-name (class-of object)))

(defmacro named-lambda (name lambda-list &body body)
  `(flet ((,name ,lambda-list ,@body))
     (cl:function ,name)))

;; COERCE operates from a type to a class

(defun coerce (object output-type-spec)
  "Converts OBJECT to type specified by OUTPUT-TYPE-SPEC. To do so, the system
internally makes use of coercions (lambda functions) defined using DEFINE-COERCION.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of (sub)type specified by OUTPUT-TYPE-SPEC. It is guaranteed
that only a single (predefined) coercion is applicable for a given OUTPUT-TYPE-SPEC.
"
  (cond ((eq output-type-spec t)
         object)
        ((typep object output-type-spec)
         object)
        (t
         (let* ((from-class (class-name (class-of object))))
           (multiple-value-bind (to-class args)
               (optima:match (typexpand output-type-spec)
                 ((list* 'specializing class-name args)
                  (values class-name args))
                 ((list class-name args)
                  (values class-name args))
                 (_
                  (if (and (atom output-type-spec)
                           (ignore-errors (find-class output-type-spec nil)))
                      (values output-type-spec nil)
                      (values (clhs-class-from-type-spec output-type-spec) nil))))
             (let ((coercion (coercion from-class to-class)))
               (if coercion
                   (apply coercion object args)
                   (error 'simple-type-error
                          :format-control "No coercion defined from ~S of type~%  ~S~%to~%  ~S~%Available coercions include:~%  ~{~S~^~%  ~}"
                          :format-arguments
                          (list object
                                from-class
                                output-type-spec
                                (let ((coercions-list (list-all-coercions)))
                                  (if (and *print-length*
                                           (> (length coercions-list) *print-length*))
                                      (append (subseq coercions-list 0 *print-length*)
                                              '("..."))
                                      coercions-list)))))))))))

(defun speed-more-than-safety-p (&optional env)
  (> (introspect-environment:policy-quality 'speed env)
     (introspect-environment:policy-quality 'safety env)))

(defun speed-more-than-debug-p (&optional env)
  (> (introspect-environment:policy-quality 'speed env)
     (introspect-environment:policy-quality 'debug env)))

(define-compiler-macro coerce (&whole form object output-type-spec &environment env)
  (if (not (speed-more-than-debug-p env))
      form
      (flet ((notify-optim-failure (datum &rest args)
               (signal 'compiler-macro-notes:optimization-failure-note
                       :datum datum :args args)))
        (compiler-macro-notes:with-notes
            (form env
                  :optimization-note-condition (speed-more-than-safety-p env))
          (let* ((object-type
                   (cl-form-types:nth-form-type object env 0 t t))
                 (from-class-name (clhs-class-from-type-spec object-type))
                 (output-type-spec-type-unexpanded
                   (if (constantp output-type-spec env)
                       `(eql ,(introspect-environment:constant-form-value
                               output-type-spec env))
                       (cl-form-types:nth-form-type
                        output-type-spec env 0 t t)))
                 (output-type-spec-type (typexpand output-type-spec-type-unexpanded))
                 (output-type-spec
                   (optima:match output-type-spec-type
                     ((list 'eql type-spec)
                      (alexandria:ensure-list type-spec))
                     ((list 'member type-spec)
                      (alexandria:ensure-list type-spec))
                     (_
                      (notify-optim-failure "Could not derive the OUTPUT-TYPE-SPEC from its type derived to be~%  ~S" output-type-spec-type-unexpanded))))
                 (to-class-name
                   (clhs-class-from-type-spec output-type-spec)))
            (let* ((to-class (ignore-errors (find-class to-class-name nil)))
                   (coercion-expression
                     (if (not to-class)
                         (notify-optim-failure
                          "No class found for output-type-specifier ~S" to-class)
                         (coercion-expression from-class-name to-class-name))))
              (if coercion-expression
                  `(,coercion-expression ,object ,@(etypecase output-type-spec
                                                     (atom ())
                                                     (cons (rest output-type-spec))))
                  (notify-optim-failure
                   "No coercion found for object derived to be of type~%  ~S~%to output-type-spec~%  ~S"
                   object-type output-type-spec))))))))

(defmacro define-coercion ((var &key (to nil to-p) (from t))
                           lambda-list
                           &body body)
  "Defines a coercion for VAR of class FROM to class TO.
Assumes the coercion to be valid in null lexical environment.

LAMBDA-LIST can be used to specify the additional arguments in cases
where the class name has a LIST form of the type specifier."
  (declare (ignore to-p))
  (check-type from symbol)
  (check-type to symbol)
  (assert (or (null lambda-list)
              (eq to
                  (class-name-from-type-spec to))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (coercion ',from ',to) (named-lambda coerce (,var ,@lambda-list) ,@body))
     (setf (coercion-expression ',from ',to) '(cl:lambda (,var ,@lambda-list) ,@body))
     t))

(defun undefine-coercion (from to)
  "Removes a coercion TYPE= to FROM and TO."
  ;; FIXME: TYPE= checks in polymorphic-functions
  (setf (coercion from to) nil)
  (setf (coercion-expression from to) nil))

;; TODO
(defun list-all-coercions (&optional (to nil to-p))
  (remove-duplicates (mapcar (lambda (type-list)
                               (destructuring-bind (%from &rest %to) type-list
                                 (if to-p
                                     %from
                                     (list :from %from :to %to))))
                             (remove-if-not (lambda (type-list)
                                              (destructuring-bind (%from &rest %to)
                                                  type-list
                                                (declare (ignore %from))
                                                (if to-p
                                                    (type= to %to)
                                                    t)))
                                            (remove '(t . t)
                                                    (mapcar #'car
                                                            (coercion-table-all
                                                             *coercion-table*))
                                                    :test #'equal)))
                     :test #'equalp))

