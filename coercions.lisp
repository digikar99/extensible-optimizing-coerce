(in-package :trivial-coerce)

(define-coercion (sequence :to list :from sequence) (cl:coerce sequence 'list))
(define-coercion (sequence :to vector :from sequence) (cl:coerce sequence 'vector))

(macrolet ((def-stub (type)
             `(define-coercion (sequence :from sequence :to (vector ,type))
                (cl:coerce sequence '(vector ,type)))))
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
             `(define-coercion (,type :to character
                                      :from ,type)
                (cl:coerce ,type 'character))))
  (def-stub symbol)
  (def-stub character)
  (def-stub string))

;; There is no proper notion of complex rationals in Common Lisp
;; (typep (cl:coerce 1 'complex) 'complex) ;=> NIL
;; http://clhs.lisp.se/Body/12_aec.htm
;; Complex rationals should automagically be converted to rationals
;; (define-coercion (number :from real :to complex) (complex number))

(macrolet ((def-stub (type)
             `(define-coercion (number :from real :to (complex ,type))
                (cl:coerce number '(complex ,type)))))
  (def-stub double-float)
  (def-stub single-float)
  (def-stub short-float)
  (def-stub long-float))

(macrolet ((def-stub (type)
             `(define-coercion (number :from real :to ,type)
                (cl:coerce number ',type))))
  (def-stub double-float)
  (def-stub single-float)
  (def-stub short-float)
  (def-stub long-float))

(define-coercion (symbol :from symbol :to function)
  (cl:coerce symbol 'function))
(define-coercion (lambda-expression :from list :to function)
  (cl:coerce lambda-expression 'function))

;; Beyond CLHS =================================================================

(define-coercion (string-designator :to string
                                    :from tt:string-designator)
  (string string-designator))
(define-coercion (number :from number :to simple-string)
  (write-to-string number))

(define-coercion (str :from string :to integer) (parse-integer str))
(define-coercion (number :from real :to integer) (floor number))

(define-coercion (char :from character :to integer) (char-code char))
(define-coercion (code :from integer :to character) (code-char code))

(define-coercion (pathname :from pathname :to simple-string) (namestring pathname))
(define-coercion (pathspec :from string   :to pathname) (pathname pathspec))

;; (macrolet ((def-signed-stub (type)
;;              `(define-coercion (num :from real :to ,type)
;;                 (mod)))))
