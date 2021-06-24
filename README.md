# trivial-coerce

## Status

- Re-implemented using [polymorphic-functions.extended-types](https://github.com/digikar99/polymorphic-functions/) use `(optimize (speed 3))` with (< debug 3) to compile-time optimize for inline static dispatch.
- API is immature; wait for a few months or years until this library gets more thoroughly tested.

I suspect that unless there are requests, I will not work on the following:

- Optimize for run-time performance
- Use hash-tables instead of alists: requires type preserving hash-functions

>In case someone has a better idea for `trivial-coerce` - feel free to raise an issue, and in the worst case, if more than a handful think (or I get convinced) the other library better deserves the name, I'd be glad to rename this library to something else. (Therefore, use with package-local-nicknames.)

Crucial dependencies:

- [polymorphic-functions](https://github.com/digikar99/polymorphic-functions/)
- [ctype](https://github.com/s-expressionists/ctype/)

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
This converts OBJECT to type specified by OUTPUT-TYPE-SPEC.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of (sub)type specified by OUTPUT-TYPE-SPEC. If multiple
coercions are applicable, the most specific coercion is called. (See [Role of Extended Types](#role-of-extended-types).)

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
; Compiler-macro of #<POLYMORPHIC-FUNCTION COERCE (28)> is unable to optimize
;   (COERCE A TYPE)
; because:
;
;   Type of
;     TYPE
;   could not be determined
;   Type of
;     A
;   could not be determined
WARNING: redefining COMMON-LISP-USER::TO-TYPE in DEFUN
TO-TYPE
CL-USER> (defun to-string (a)
           (declare (optimize speed))
           (coerce a 'string))
; Compiler-macro of #<POLYMORPHIC-FUNCTION COERCE (28)> is unable to optimize
;   (COERCE A 'STRING)
; because:
;
;   Type of
;     A
;   could not be determined
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
; Size: 17 bytes. Origin: #x5374A224                          ; TO-STRING
; 24:       B902000000       MOV ECX, 2
; 29:       FF7508           PUSH QWORD PTR [RBP+8]
; 2C:       B8C2333650       MOV EAX, #x503633C2              ; #<FDEFN SB-INT:STRINGIFY-OBJECT>
; 31:       FFE0             JMP RAX
; 33:       CC10             INT3 16                          ; Invalid argument count trap
NIL
```

### Role of Extended Types

The form `(define-coercion (sequence :to list :from sequence) (cl:coerce sequence 'list))` macroexpands to:

```lisp
(defpolymorph coerce
    ((sequence sequence) (#:output-type-spec2636 (supertypep list)))
    list
  (declare (ignorable sequence #:output-type-spec2636))
  (common-lisp:coerce sequence 'list))
```

Thus, we use the extended type `(supertypep list)` to denote all the type specifiers that are a supertype of `list`. Thus `(typep 'sequence (supertypep list))` and `(typep t '(supertypep list))` holds (using `polymorphic-functions.extended-types:typep`).

Amongst these, consider `(supertypep string)` and `(supertypep vector)` - and note that the latter is a subset (and hence a subtype) of the former. In other words, the polymorph corresponding to the latter is a more specialized polymorph than the former. And thus, this most specialized polymorph is chosen accordingly.
