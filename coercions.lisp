(in-package :trivial-coerce)

(define-coercion (object :from t :to t) () object)

(define-coercion (sequence :to list :from sequence) ()  (cl:coerce sequence 'list))
(define-coercion (sequence :to vector :from sequence) (&rest args)
  (cl:coerce sequence `(vector ,@args)))

(macrolet ((def-stub (type)
             `(define-coercion (o :to character
                                  :from ,type)
                               ()
                (cl:coerce o 'character))))
  (def-stub symbol)
  (def-stub string))

;; There is no proper notion of complex rationals in Common Lisp
;; (typep (cl:coerce 1 'complex) 'complex) ;=> NIL
;; http://clhs.lisp.se/Body/12_aec.htm
;; Complex rationals should automagically be converted to rationals
;; (define-coercion (number :from real :to complex) (complex number))

(define-coercion (number :from real :to complex) (type)
  (cl:coerce number `(complex ,type)))

(macrolet ((def-stub (type)
             (if (assoc type ctype::+floats+)
                 `(define-coercion (number :from real :to ,type) ()
                    (cl:coerce number ',type))
                 nil)))
  (def-stub double-float)
  (def-stub single-float)
  (def-stub short-float)
  (def-stub long-float))

(define-coercion (symbol :from symbol :to cl:function) ()
  (cl:coerce symbol 'cl:function))
(define-coercion (lambda-expression :from cons :to cl:function) ()
  (cl:coerce lambda-expression 'cl:function))

;; Beyond CLHS =================================================================

(define-coercion (o :from t :to string) ()
  (typecase o
    (trivial-types:string-designator o)
    (t (write-to-string o))))

(define-coercion (str :from string :to integer) ()
  (parse-integer str))

(define-coercion (number :from real :to integer) (&optional low high)
  (declare (ignore low high))
  (floor number))

(define-coercion (char :from character :to integer) ()
  (char-code char))
(define-coercion (code :from integer :to character) ()
  (code-char code))

(define-coercion (pathname :from pathname :to string) ()
  (namestring pathname))
(define-coercion (pathspec :from string   :to pathname) ()
  (pathname pathspec))

(define-coercion (int :from integer :to integer) (&optional low high)
  (cond ((or (null low)
             (eq low 'cl:*))
         int)
        ((or (null high)
             (eq high 'cl:*))
         (- int low))
        (t
         (if (<= low int high)
             int
             (let ((diff (- high low -1)))
               (+ (mod (- int low) diff) low))))))

(define-coercion (n :from single-float :to double-float) () (cl:coerce n 'double-float))
(define-coercion (n :from double-float :to single-float) () (cl:coerce n 'single-float))

#-(or ccl sbcl)
(warn "TRIVIAL-COERCE:COERCE fo FIXNUM is untested on non-SBCL/CCL platforms")
(define-coercion (int :from integer :to fixnum) ()
  (let* ((fixnum-range (1+ (- most-positive-fixnum most-negative-fixnum)))
         (uint (mod int fixnum-range)))
    (if (< uint (1+ most-positive-fixnum))
        uint
        (- uint fixnum-range))))

;; (macrolet ((def-signed-stub (type)
;;              `(define-coercion (num :from real :to ,type)
;;                 (mod)))))
