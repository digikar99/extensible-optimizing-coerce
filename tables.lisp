(in-package :trivial-coerce)

(defvar *coercion-table* (make-hash-table :test #'equal))
(defun coercion (from-class to-class)
  (loop :for class :in (closer-mop:class-precedence-list
                        (find-class from-class t))
        :do (multiple-value-bind (coercion existsp)
                (gethash (cons (class-name class) to-class) *coercion-table*)
              (when existsp
                (return coercion)))))
(defun (setf coercion) (coercion-lambda from-class to-class)
  (if coercion-lambda
      (setf (gethash (cons from-class to-class) *coercion-table*) coercion-lambda)
      (remhash (cons from-class to-class) *coercion-table*)))

(defvar *coercion-expression-table* (make-hash-table :test #'equal))
(defun coercion-expression (from-class to-class)
  (loop :for class :in (closer-mop:class-precedence-list
                        (find-class from-class t))
        :do (multiple-value-bind (coercion-expression existsp)
                (gethash (cons (class-name class) to-class) *coercion-expression-table*)
              (when existsp
                (return coercion-expression)))))
(defun (setf coercion-expression) (coercion-expression-lambda from-class to-class)
  (if coercion-expression-lambda
      (setf (gethash (cons from-class to-class) *coercion-expression-table*)
            coercion-expression-lambda)
      (remhash (cons from-class to-class) *coercion-expression-table*)))

