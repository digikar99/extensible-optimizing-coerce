# trivial-coerce

## Status

- Library is immature; wait for a few months or years until this library gets more thoroughly tested.

I suspect that unless there are requests, I will not work on optimizing for run-time performance.

>In case someone has a better idea for `trivial-coerce` - feel free to raise an issue, and in the worst case, if more than a handful think (or I get convinced) the other library better deserves the name, I'd be glad to rename this library to something else. (Therefore, use with package-local-nicknames.)

Crucial dependencies:

- [ctype](https://github.com/s-expressionists/ctype/)

## Why

Common Lisp has a variety of non-uniform type-conversions. Some use `cl:coerce`, others use `cl:float` or `cl:string`, and lots more. In some sense, the `cl:coerce` function is information-preserving in that it prevents non-integers from being coerced into integers, or characters from being converted to and from integers. If you find these semantics useful, you might not find `trivial-coerce:coerce` as useful.

OTOH, a case could be made that the prevalence of information non-preserving type conversions makes things easier for prototyping, enables better polymorphism, as well as makes things easier for new people.

## Examples

See [coercions.lisp](./coercions.lisp) and [tests.lisp](./tests.lisp) and docstrings.

## generic-cl:coerce

A naive use of generic-functions is not suitable for the purpose of coercion in the Common Lisp world. For instance:

```lisp
(in-package :generic-cl)
(deftype int () 'integer)
(defmethod coerce ((num real) (type (eql 'integer)))
  (floor num))

(coerce 2.5 'integer) ;=> works
(coerce 2.5 'int) ;=> does not work
(trivial-coerce:coerce 2.5 'int) ;=> works
```

## Philosophy

An earlier version of trivial-coerce allowed for coercions between arbitrary *types*. However, that made no sense, since coercions seem to be intended at changing the internal representation of an object. The more appropriate representation of the internal structure is the *class* of which the object is an instance of! See [Pittman's Best of Intentions](http://www.nhplace.com/kent/PS/EQUAL.html).

Thus, now, coercions can only be defined from one *class* to another *class*. One may certainly supply additional arguments. See [coercions.lisp](./coercions.lisp) for examples.

## Documentation

The main function is `(trivial-coerce:coerce object output-type-spec)`:
This converts OBJECT to type specified by OUTPUT-TYPE-SPEC.

The applicable coercion is guaranteed to take an object of (super)type of OBJECT
and return an object of type= specified by OUTPUT-TYPE-SPEC. (See [Role of Extended Types](#role-of-extended-types).)

### Important functions and macros

- coerce
- list-all-coercions
- define-coercion
- undefine-coercion

Example usages of `define-coercion` can be found in [coercions.lisp](./coercions.lisp).

### Compile Time Optimizations

```lisp
CL-USER> (defun to-type (a type)
           (trivial-coerce:coerce a type))
TO-TYPE
CL-USER> (defun to-type (a type)
           (declare (optimize speed))
           (trivial-coerce:coerce a type))

; (Compiler) Macro of TRIVIAL-COERCE:COERCE is unable to optimize
;   (TRIVIAL-COERCE:COERCE A TYPE)
; because:
;
;   Could not derive the OUTPUT-TYPE-SPEC from its type derived to be
;     T
WARNING: redefining COMMON-LISP-USER::TO-TYPE in DEFUN
TO-TYPE
CL-USER> (defun to-integer (a)
           (declare (optimize speed))
           (trivial-coerce:coerce a 'integer))

; (Compiler) Macro of TRIVIAL-COERCE:COERCE is unable to optimize
;   (TRIVIAL-COERCE:COERCE A 'INTEGER)
; because:
;
;   No coercion found for object derived to be of type
;     T
;   to output-type-spec
;     (INTEGER)
TO-INTEGER
CL-USER> (defun to-integer (a)
           (declare (optimize speed)
                    (type real a))
           (trivial-coerce:coerce a 'integer))
WARNING: redefining EXCL::TO-INTEGER in DEFUN
TO-INTEGER
CL-USER> (disassemble 'to-integer)
; disassembly for TO-INTEGER
; Size: 141 bytes. Origin: #x53B6AB8F                         ; TO-INTEGER
; B8F:       4883EC10         SUB RSP, 16
; B93:       488B55F8         MOV RDX, [RBP-8]
; B97:       48892C24         MOV [RSP], RBP
; B9B:       488BEC           MOV RBP, RSP
; B9E:       B842462550       MOV EAX, #x50254642             ; #<FDEFN SB-KERNEL:UNARY-TRUNCATE>
; BA3:       FFD0             CALL RAX
; BA5:       488955F0         MOV [RBP-16], RDX
; BA9:       48897DE8         MOV [RBP-24], RDI
; BAD:       488B55E8         MOV RDX, [RBP-24]
; BB1:       31FF             XOR EDI, EDI
; BB3:       FF142598050050   CALL [#x50000598]               ; #x52A00FF0: GENERIC-=
; BBA:       751F             JNE L2
; BBC: L0:   488B45F0         MOV RAX, [RBP-16]
; BC0:       488B7DE8         MOV RDI, [RBP-24]
; BC4: L1:   488BD0           MOV RDX, RAX
; BC7:       488D5D10         LEA RBX, [RBP+16]
; BCB:       B904000000       MOV ECX, 4
; BD0:       BE17010050       MOV ESI, #x50000117             ; NIL
; BD5:       F9               STC
; BD6:       488BE5           MOV RSP, RBP
; BD9:       5D               POP RBP
; BDA:       C3               RET
; BDB: L2:   488B55F8         MOV RDX, [RBP-8]
; BDF:       31FF             XOR EDI, EDI
; BE1:       FF142588050050   CALL [#x50000588]               ; #x52A00F90: GENERIC-<
; BE8:       7DD2             JNL L0
; BEA:       488B55F0         MOV RDX, [RBP-16]
; BEE:       BF02000000       MOV EDI, 2
; BF3:       FF142570050050   CALL [#x50000570]               ; #x52A00E30: GENERIC--
; BFA:       488BC2           MOV RAX, RDX
; BFD:       488945F8         MOV [RBP-8], RAX
; C01:       488B55E8         MOV RDX, [RBP-24]
; C05:       BF02000000       MOV EDI, 2
; C0A:       FF142568050050   CALL [#x50000568]               ; #x52A00DC0: GENERIC-+
; C11:       488BFA           MOV RDI, RDX
; C14:       488B45F8         MOV RAX, [RBP-8]
; C18:       EBAA             JMP L1
; C1A:       CC10             INT3 16                         ; Invalid argument count trap
NIL
```
