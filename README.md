# trivial-coerce

## Status

- Optimized for compile-time performance and notes: use `(optimize (speed 3))`
- API is immature; wait for a few months or years until this library gets more thoroughly tested.

I suspect that unless there are requests, I will not work on the following:

- Optimize for run-time performance
- Use hash-tables instead of alists: requires type preserving hash-functions

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
```

I do not know of alternatives.
