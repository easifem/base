# Strings in easifem

- EASIFEM employs StringiFor library to implement `String` datatype.
- More information about the `String` datatype can be found in [StringiFor](https://github.com/szaghi/StringiFor/releases) library

In addition, following routines are used from [PENF](https://github.com/szaghi/PENF) library.

- `Str()` method has been defined which is defined in
- `Strz()` method has been defined, which convert integer to string with the righ number of zeros
- `cton()` method convert string to number
- `bstr()` method converts number to bit-string
- `bcton()` method converts bit-string to number

## `Str(fm,n)`

- `fm` is character which acts a format specifier;
- `n` can be real or integer

```fortran
program main
use easifem
implicit none

CALL Display( str( FReal64, 1.0_Real64) )
CALL Display( str( FReal32, 1.0_Real32) )
CALL Display( str( FDFP, 1.0_DFP) )
CALL Display( str( FI4B, 1_I4B) )
CALL Display( str( FInt32, 1_Int32) )
CALL Display( str( FInt16, 1_Int16) )

end program main
```

## `Str(n, no_sign, compact)`

- `n` can be real
- `no_sign` is logical, if present it is consider true, else false
- `compact` is logical, if present then it is consider true, else false

```fortran
CALL Display( str( pi, no_sign=.true., compact=.true.), "no_sign, compact :: " )
CALL Display( str( Pi, no_sign=.true. ), "no_sign :: " )
CALL Display( str( Pi ) )
```

## `Str(n, no_sign)`

- `n` is integer
- `no_sign` is logical

```fortran
CALL Display( str( 1, no_sign=.true. ), "int no_sign :: " )
CALL Display( str( 1 ), "int :: " )
```

## `Str(n, no_sign, separator, delimiters, compact)`

- `n` is vector of reals
- `no_sign`  is logical, if present then true, else false
- `separator` is character of length 1, optional, default is `,`
- `delimiters` is vector of characters of length 2, optional
- `compact` is logical, if present then true, else false

```fortran
CALL Display( str(n=[1._DFP, -2._DFP]), "Default :: " )
CALL Display( str(n=[1._DFP, -2._DFP], separator=";" ), "Separator ; :: " )
CALL Display( str(n=[1._DFP, -2._DFP], separator=",", delimiters= ["(", ")"] ), "Separator and delimiters :: " )
```

## `Str(n, no_sign, separator, delimiters)`

- `n` is vector of integers
- `no_sign`  is logical, if present then true, else false
- `separator` is character of length 1, optional, default is `,`
- `delimiters` is vector of characters of length 2, optional

```fortran
CALL Display( str(n=[1, -2]), "Default :: " )
CALL Display( str(n=[1, -2], separator=";" ), "Separator ; :: " )
CALL Display( str(n=[1, -2], separator=",", delimiters= ["(", ")"]), "Separator and delimiters :: " )
```

## `Strz(n, nz_pad)`

- `n` is integer number
- `nz_pad` is the padding size

```fortran
WRITE( *, * ) strz( 111 )
WRITE( *, * ) strz( 111, nz_pad=3 )
WRITE( *, * ) strz( 111, nz_pad=4 )
WRITE( *, * ) strz( 111, nz_pad=5 )
```

## `CTON(str, knd) `

- `str` is the string
- `knd` is the real or integer number

```fortran
WRITE( *, * ) CTON( "-1", 1.0_DFP )
WRITE( *, * ) CTON( "-1", 1_I4B )
```

## `BSTR(n)`

- `n` can be real or integer number

