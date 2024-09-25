ᕦ(ツ)ᕤ
# zero

A place for notes and scribbles about the language, rather than the zeta project.

Current definition of the language is:

## types

concrete numerical types:

    i1, i2, i3 ...      : n-bit signed integer
    u1, u2, u3 ...      : n-bit unsigned integer
    f1, f2, f3 ...      : n-bit float

abstract numerical types:

    int     : signed integer
    uint    : unsigned integer
    float   : floating-point
    number  : int or float

convenience:

    bool    : boolean
    string  : string
    time    : time

## arrays

    int a[] = [ 0 .. 10 ]               : [ 0, 1, ..., 9 ]
    int b[] = [ 1 .= 10 ]               : [ 1, 2, ..., 10 ]
    int c[] = a[] + b[]                 : map '+'
    int d[] = a[] + 1                   : map '+' with singular value
    int sum = a[] + _                   : reduce using '+'
    int e[] = a[] where _ & 1 == 0      : filter

    int f[] = a[] << b[]                : concatenate
    int g[] = a[] << 1                  : concatenate singular value
    int h[] = a[ 0 .. 5 ]               : slice (0-5 non-incl)
    int j[] = a[ 0 .= 5 ]               : since (0-5 inclusive)
    
## streams

    