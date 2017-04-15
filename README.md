# Minion

## Description

A JSON parser I wrote to teach myself about parsers, recursion and
Common Lisp. It's not meant for production use - its brevity is cute
to my eyes, but there are no optimizations to speak of. As it stands,
`cl-minion` is about 50% slower than `jsown` (informally tested).

It also lacks some standard/required features that would make it
suitable for general use (See TODO list). Since it was primarily meant
as a learning tool for myself, there is currently no API to access the
parsed list.

## Usage

The usual. Place it in your central repository or anywhere where ASDF
can find it and then

```
(asdf:load-system :cl-minion)
```

```
(cl-minion:parse *string*)
```

to run.

## TODO

- Scientific notation for numeric values
- Array elements should be stored in a vector.
- Escaping & special character (i.e \f \n, etc.) support in strings.
- Ban-bag for strings (keywords, for example, should not be allowed to
  contain whitespaces)

## Thanks

Peter Csengodi for much appreciated advice.
