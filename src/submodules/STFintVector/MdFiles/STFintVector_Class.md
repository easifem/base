# Space Time Internal Force Vector

## ToDo

- Make construction method like `STFintVector(S1, S2)` where the S1 and S2 are Integer vectors. The size of S2 is two. and it contains `NIPS` and `NIPT`. The size of S1 should be either 1 or 3. If One allocates `Obj % Vec1(S1(1))`, If 3 then allocate `Obj % Vec3()`.

## Description

The class `STFintVector_Class` is the subclass of `STElemShapeData_` class. It is designed to compute the internal force vector for `SolidMechanics` applications.

## Getting Started

### Making the Object

We have defined the function called `STFintVector_Pointer` that will return the pointer to the object. We have also defined the function `STFintVector()` that will return the object of `STFintVector_` type.

```fortran
Obj => STFintVector_Pointer( )
Obj => STFintVector_Pointer( Row, NIPS, NIPT )
Obj => STFintVector_Pointer( I1, I2, I3, NIPS, NIPT )
```

- The first call, above, is empty contructor.
- The second call, above, allocates `Obj % Vec1(row)` and `Obj%SD(NIPS, NIPT)`
- The third call, above, allocates `Obj%Vec3(I1, I2, I3)` and `Obj%SD(NIPS, NIPT)`

```fortran
Obj = STFintVector( )
Obj = STFintVector( Row, NIPS, NIPT )
Obj = STFintVector( I1, I2, I3, NIPS, NIPT )
```

- The first call, above, is empty contructor.
- The second call, above, allocates `Obj % Vec1(row)` and `Obj%SD(NIPS, NIPT)`
- The third call, above, allocates `Obj%Vec3(I1, I2, I3)` and `Obj%SD(NIPS, NIPT)`

### Getting the Fint vector

```fortran
CALL Obj % getFintVector( Sigma( :, :, : ) )
```

```fortran
CALL Obj % getFintVector( Sigma( :, : ) )
```

```fortran
CALL Obj % getFintVector( Sigma( : ) )
```

```fortran
CALL Obj % getFintVector( CData( :, : ) )
```

```fortran
CALL Obj % getFintVector( CData( : ) )
```

```fortran
CALL Obj % getFintVector( CData )
```