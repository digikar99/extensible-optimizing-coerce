(in-package :trivial-coerce)

(define-coercion (object :from t :to t) object)

(define-coercion (sequence :to list :from sequence) (cl:coerce sequence 'list))
(define-coercion (sequence :to vector :from sequence) (cl:coerce sequence 'vector))

(macrolet ((def-stub (type)
             (if (assoc type ctype::+floats+)
                 `(define-coercion (sequence :from sequence :to (vector ,type))
                    (cl:coerce sequence '(vector ,type)))
                 nil)))
  (def-stub single-float)
  (def-stub double-float)
  (def-stub short-float)
  (def-stub long-float)
  (def-stub (complex single-float))
  (def-stub (complex double-float))
  (def-stub (complex short-float))
  (def-stub (complex long-float))
  (def-stub (unsigned-byte 02))
  (def-stub (unsigned-byte 04))
  (def-stub (unsigned-byte 08))
  (def-stub (unsigned-byte 16))
  (def-stub (unsigned-byte 32))
  (def-stub (unsigned-byte 64))
  (def-stub (signed-byte 02))
  (def-stub (signed-byte 04))
  (def-stub (signed-byte 08))
  (def-stub (signed-byte 16))
  (def-stub (signed-byte 32))
  (def-stub (signed-byte 64)))

(macrolet ((def-stub (type)
             `(define-coercion (o :to character
                                  :from ,type)
                (cl:coerce o 'character))))
  (def-stub trivial-types:character-designator)
  (def-stub (and string (not trivial-types:character-designator))))

;; There is no proper notion of complex rationals in Common Lisp
;; (typep (cl:coerce 1 'complex) 'complex) ;=> NIL
;; http://clhs.lisp.se/Body/12_aec.htm
;; Complex rationals should automagically be converted to rationals
;; (define-coercion (number :from real :to complex) (complex number))

(macrolet ((def-stub (type)
             (if (assoc type ctype::+floats+)
                 `(define-coercion (number :from real :to (complex ,type))
                    (cl:coerce number '(complex ,type)))
                 nil)))
  (def-stub double-float)
  (def-stub single-float)
  (def-stub short-float)
  (def-stub long-float))

(macrolet ((def-stub (type)
             (if (assoc type ctype::+floats+)
                 `(define-coercion (number :from real :to ,type)
                    (cl:coerce number ',type))
                 nil)))
  (def-stub double-float)
  (def-stub single-float)
  (def-stub short-float)
  (def-stub long-float))

(define-coercion (symbol :from symbol :to cl:function)
  (cl:coerce symbol 'cl:function))
(define-coercion (lambda-expression :from cons :to cl:function)
  (cl:coerce lambda-expression 'cl:function))

;; Beyond CLHS =================================================================

(define-coercion (string-designator :to string
                                    :from trivial-types:string-designator)
  (string string-designator))
(define-coercion (number :from number :to string)
  (write-to-string number))

(define-coercion (str :from string :to integer) (parse-integer str))
(define-coercion (number :from real :to integer) (floor number))

(define-coercion (char :from character :to integer) (char-code char))
(define-coercion (code :from integer :to character) (code-char code))

(define-coercion (pathname :from pathname :to string) (namestring pathname))
(define-coercion (pathspec :from string   :to pathname) (pathname pathspec))

(macrolet ((def (bits)
             `(progn
                (define-coercion (int :from integer :to (unsigned-byte ,bits))
                  (mod int ,(expt 2 bits)))
                (define-coercion (int :from integer :to (signed-byte ,bits))
                  (let ((ub (mod int ,(expt 2 bits))))
                    (if (< ub ,(expt 2 (1- bits)))
                        ub
                        (- ub
                           ,(expt 2 bits))))))))
  (def 64)
  (def 32)
  (def 16)
  (def 08)
  (def 04)
  (def 02)
  (def 01))

#-(or ccl sbcl)
(warn "TRIVIAL-COERCE:COERCE fo FIXNUM is untested on non-SBCL/CCL platforms")
(define-coercion (int :from integer :to fixnum)
  (let* ((fixnum-range (1+ (- most-positive-fixnum most-negative-fixnum)))
         (uint (mod int fixnum-range)))
    (if (< uint (1+ most-positive-fixnum))
        uint
        (- uint fixnum-range))))

;; (macrolet ((def-signed-stub (type)
;;              `(define-coercion (num :from real :to ,type)
;;                 (mod)))))
