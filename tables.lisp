(in-package :extensible-optimizing-coerce)

(defstruct coercion-table
  (cache (make-hash-table :test #'equal) :type hash-table)
  (all nil :type list))

;; TODO: Make a two layer table
(defvar *coercion-table* (make-coercion-table))
(defun coercion (from-class to-class)
  (with-slots (cache all) *coercion-table*
    (or (gethash (cons from-class to-class) cache)
        (let ((coercion-lambda
                (block coercion
                  (loop :for class :in (closer-mop:class-precedence-list
                                        (find-class from-class t))
                        :do (loop :for ((from-type . %to-class) . coercion-lambda) :in all
                                  :do (when (and (eq to-class %to-class)
                                                 (subtypep from-class from-type))
                                        (return-from coercion coercion-lambda)))))))
          (when coercion-lambda
            (setf (gethash (cons from-class to-class) cache) coercion-lambda))
          coercion-lambda))))

(defun (setf coercion) (coercion-lambda from-class to-class)
  (with-slots (cache all) *coercion-table*
    (setf cache (make-hash-table :test #'equal))
    (if coercion-lambda
        (setf (alexandria:assoc-value all (cons from-class to-class)
                                      :test #'equal)
              coercion-lambda)
        (alexandria:removef all (cons from-class to-class) :test #'equal :key #'car))))

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

