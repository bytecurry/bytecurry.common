bytecurry.common
================

bytecurry.common is a compilation of useful common lisp code built on top of alexandria
and other common lisp utilities.

Currently it contains two packages: bytecurry.common and bytecurry.common.syntax.
The bytecurry.common package defines a number of useful functions and classes, and
bytecurry.common.syntax provides reader macros that make common lisp development easier.

bytecurry.common package
------------------------

The bytecurry.common package provide the following functionality:

 - *list functions* -- Currently this includes `insert` and `ninsert-after` functions which
   insert an item into a list at a specified index non-destructively and destructively respectively.
 - *sget and sset* -- These provide a more uniform way to get and set properties of various things,
   like plists, alists, vectors, hash-tables, etc. It is also extensible, so you can use it to add
   getters and setters for your own objects.
 - *buffer* -- There is a `buffer` class which makes it easiser to work with a fixed-size buffer of
   memmory. It is basically just a wrapper around a simple vector.
 - *piped-stream* -- There is a `piped-stream` class that implements a piped stream backed by a
   `buffer`. This is similar to a combination of PipedInputStream nad PipedOutputStream in Java.
   It is intended to be read from one thread and written to from another thread. If it is only used
   from one thread it may become deadlocked if you try to read or write more data than can fit in the
   buffer.

See the docstrings for more information.

bytecurry.common.syntax package
-------------------------------

bytecurry.common.syntax provides the `enable-syntax` function, which enable the reader macros described below, as well as the `syntax` symbol, which identifies the named-readtable for all the macros, and
the `defreadtable-with-syntax` macro, which is similar to `named-readtables:defreadtable`, but
automatically includes the common syntax.

The following reader macros are included:

|Dispatch | Example            | Description                                                  |
|:-------:|:-------------------|:-------------------------------------------------------------|
| @       | `@trace (+ 1 2)`   | cl-annot reader macro                                        |
| #?      | `#?"foo: $foo"`    | cl-interpol reader macro                                     |
| #N      | `#n(4 + 5)`        | infix reader macro from mexpr                                |
| #H      | `#h(:foo "foo")`   | hash-table reader macro to facilitate creation of hashtables |
