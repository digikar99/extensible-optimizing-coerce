# trivial-coerce

## Status

- Optimized for compile-time performance and notes: use `(optimize (speed 3))`
- API is immature; wait for a few months or years until this library gets more thoroughly tested.

I suspect that unless there are requests, I will not work on the following:

- Optimize for run-time performance
- Use hash-tables instead of alists: requires type preserving hash-functions

>In case someone has a better idea for `trivial-coerce` - feel free to raise an issue, and in the worst case, if more than a handful think (or I get convinced) the other library better deserves the name, I'd be glad to rename this library to something else. (Therefore, use with package-local-nicknames.)

## Why

Common Lisp has a variety of non-uniform type-conversions. Some use `cl:coerce`, others use `cl:float` or `cl:string`, and lots more. In some sense, the `cl:coerce` function is information-preserving in that it prevents non-integers from being coerced into integers, or characters from being converted to and from integers. If you find these semantics useful, you might not find `trivial-coerce:coerce` as useful.

OTOH, a case could be made that the prevalence of information non-preserving type conversions makes things easier for prototyping, enables better parametric polymorphism, as well as makes things easier for new people.

## Examples

See [coercions.lisp](./coercions.lisp) and [tests.lisp](./tests.lisp) and docstrings.

## generic-cl:coerce

I do not find generic-functions suitable for the purpose of coercion. For instance:

```lisp
(in-package :generic-cl)
(deftype int () 'integer)
(defmethod coerce ((num real) (type (eql 'integer)))
  (floor num))

(coerce 2.5 'integer) ;=> works
(coerce 2.5 'int) ;=> does not work
(trivial-coerce:coerce 2.5 'int) ;=> works

;; Yet another example would concern the order of applicants in a OR or AND or MEMBER
;; compound-type-specifiers - and it surely feels useful to say: (and vector (not string))
```

I do not know of alternatives.

## Documentation

The main function is `(trivial-coerce:coerce object output-type-spec)`:
This converts OBJECT to type specified by OUTPUT-TYPE-SPEC. To do so, the system
internally makes use of coercions (lambda functions) defined using DEFINE-COERCION.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of (sub)type specified by OUTPUT-TYPE-SPEC. If multiple
coercions are applicable, the specific coercion that is called is undefined.

For instance, consider two coercions defined as:

```lisp
(define-coercion (list :from list :to string) (write-to-string list))
(define-coercion (list :from list :to vector) (cl:coerce list 'vector))
```

Then, the value of `(coerce '(1 2 3) 'vector)` is permitted to be `\"(1 2 3)\"`.
One may use `(coerce '(1) '(and vector (not string)))` to obtain the expected result.

### Important functions and macros

- coerce
- list-all-coercions
- define-coercion
- undefine-coercion

Example usages of `define-coercion` can be found in [coercions.lisp](./coercions.lisp).

### Compile Time Optimizations

```lisp
CL-USER> (defun to-type (a type)
           (coerce a type))
TO-TYPE
CL-USER> (defun to-type (a type)
           (declare (optimize speed))
           (coerce a type))
; Unable to optimize
;   (COERCE A TYPE)
; because
;   unable to infer the value of TYPE at compile time
WARNING: redefining COMMON-LISP-USER::TO-TYPE in DEFUN
TO-TYPE
CL-USER> (defun to-string (a)
           (declare (optimize speed))
           (coerce a 'string))
; Unable to optimize
;   (COERCE A 'STRING)
; because
;   unable to infer the type of A at compile time
;   and no coercion known from T to STRING
WARNING: redefining COMMON-LISP-USER::TO-STRING in DEFUN
TO-STRING
CL-USER> (defun to-string (a)
           (declare (optimize speed)
                    (type number a))
           (coerce a 'string))
WARNING: redefining COMMON-LISP-USER::TO-STRING in DEFUN
TO-STRING
CL-USER> (disassemble 'to-string)
; disassembly for TO-STRING
; Size: 31 bytes. Origin: #x52D68A34                          ; TO-STRING
; 34:       4883EC10         SUB RSP, 16
; 38:       B902000000       MOV ECX, 2
; 3D:       48892C24         MOV [RSP], RBP
; 41:       488BEC           MOV RBP, RSP
; 44:       B862523750       MOV EAX, #x50375262              ; #<FDEFN SB-INT:STRINGIFY-OBJECT>
; 49:       FFD0             CALL RAX
; 4B:       488BE5           MOV RSP, RBP
; 4E:       F8               CLC
; 4F:       5D               POP RBP
; 50:       C3               RET
; 51:       CC10             INT3 16                          ; Invalid argument count trap
NIL
```
