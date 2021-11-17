# Material Jacobian

## Structure

```fortran
TYPE, PUBLIC :: MaterialJacobian_
    REAL( DFP ), ALLOCATABLE :: C( :, : )
    TYPE( String_ ) :: StressType, StrainType, RateType
END TYPE MaterialJacobian_
```

## Description

`MaterialJacobian_` class is defined as constitutive data. Material jacobian relates the change in flux with the changes in gradient of some field. If the field is scalar then the flux is vector. If the field is vector then the flux is rank-2 tensor. When the material tangent is Rank-4 tensor then we assume that it has atleast minor-symmetery, therefore, we can use Voigt notation for Rank-4 tensor.

## Getting Started

### Initiating the `MaterialJacobian_` object

We can construct the object using the routine called `Initiate()`.

```fortran
CALL obj%Initiate( N )
CALL obj%Initiate( N, Fill )
CALL obj%Initiate( Mat )
CALL obj%Initiate( N, StressType, StrainType, RateType )
CALL obj%Initiate( N, [StressType, StrainType, RateType] )
```

We can also initiate the `MaterialJacobian_` using the function called `MaterialJacobian()` or `MaterialJacobian_Pointer()`

```fortran
obj = MaterialJacobian( N )
obj = MaterialJacobian( N, Fill )
obj = MaterialJacobian( Mat )
obj = MaterialJacobian( N, StressType, StrainType, RateType )
obj = MaterialJacobian( N, [StressType, StrainType, RateType] )
```

```fortran
obj => MaterialJacobian_Pointer( N )
obj => MaterialJacobian_Pointer( N, Fill )
obj => MaterialJacobian_Pointer( Mat )
obj => MaterialJacobian_Pointer( N, StressType, StrainType, RateType )
obj => MaterialJacobian_Pointer( N, [StressType, StrainType, RateType] )
```

```fortran
CALL obj%Initiate( N )
obj = MaterialJacobian( N )
obj => MaterialJacobian_Pointer( N )
```

The above call allocate `obj%C( N, N )` with all zero entries.

```fortran
CALL obj%Initiate( N, Fill )
obj = MaterialJacobian( N, Fill )
obj => MaterialJacobian_Pointer( N, Fill )
```

The above call allocates `obj%C(N,N)` with all entries equal to `Fill`.

```fortran
CALL obj%Initiate( Mat )
obj = MaterialJacobian( Mat )
obj => MaterialJacobian_Pointer( Mat )
```

The above call allocates `obj%C` with `Mat`.

```fortran
CALL obj%Initiate( N, StressType, StrainType, RateType )
obj = MaterialJacobian( N, StressType, StrainType, RateType )
obj => MaterialJacobian_Pointer( N, StressType, StrainType, RateType )
```

The above call allocates `obj%C` with size `N`. `StressType`, `StrainType`, and `RateType` can be `String_` object or character object.

```fortran
CALL obj%Initiate( N, [StressType, StrainType, RateType] )
obj = MaterialJacobian( N, [StressType, StrainType, RateType] )
obj => MaterialJacobian_Pointer( N, [StressType, StrainType, RateType] )
```

The above call allocates `obj%C` with size `N`. The size-3 rank-1 array can be can be `String_` object or character object.

### Setting the value of Names

We can set the values of `obj%StressType` `obj%StrainType`, and `obj%RateType` using the subroutine.

```Fortran
CALL obj%setStressType( StressType )
CALL obj%setStrainType( StrainType )
CALL obj%setRateType( RateType )
```

>The argument can be `Character` type or `String_` type.

### Getting the values of Names

```fortran
StressType = .StressType. obj
StrainType = .StrainType. obj
RateType = .RateType. obj
```

### Geting the values of `obj%C`

We can get the size of `obj%C` using the operator called `.SIZE.` and we can deallocate the data using the routine called `obj%Deallocate()`.

We can access the values using both subroutines and functions.

Subroutines to access the hardcopy and pointer to `obj%C` are given below.

```fortran
CALL obj%getCijkl( Mat )
CALL obj%getCijkl_Pointer( Mat )
```

Functions to access the hardcopy and pointer to `obj%C` are given below.

```fortran
Mat => obj%Cijkl_Pointer( )
Mat = obj
```

The Operator `.Cijkl.` and `.At.` can also be used to access the hardcopies of Cijkl.

```fortran
Mat = .Cijkl. obj
Mat = obj .Cijkl. [Indx1, Indx2]
Mat = obj .Cijkl. [i,j,k,l]
```

```fortran
CALL obj%getCijkl( Mat )
Mat = obj
```

The above call reallocates `Mat` with the `obj%C`.

```fortran
CALL obj%getCijkl_Pointer( Mat )
Mat => obj%Cijkl_Pointer( )
```

The above call returns the Pointer to the `obj%C`.

```fortran
Mat = .Cijkl. obj
```

The above call will return the `obj%C` hardcopy.

```fortran
Mat = obj .Cijkl. [Indx1, Indx2]
```

The above call will return the `obj%C( i,j)`

```fortran
Mat = obj .Cijkl. [i,j,k,l]
```

The above call will return the `C(i,j,k,l)`. In this case `[i,j,k,l]` are convered into voigt index then value of `obj%C` correspoding to those voigt-indices are returned.

There is another interesting way to use `.Cijkl.`. Suppose you want to obtain 6 by 6 jacobian matrix then we can call `obj .Cijkl. 6`. In this case, even if `obj%C` is not 6 by 6 we will get 6 by 6 form.

```fortran
C = obj .Cijkl. 6
C = obj .Cijkl. 4
C = obj .Cijkl. 3
C = obj .Cijkl. 2
C = obj .Cijkl. 1
```

Alternatively you can also use `obj .Shape. 6` or `obj .Shape. M` for getting the M by M matrix.

### Assignment Operator (=)

```fortran
obj = Mat
Mat = obj
obj = obj2
```

### Contraction Operator

Contraction of Material Jacobian with the Tensor and matrix is defined. It will return a 3 by 3 matrix. If you want to convert it into voigt vector then use `VoigtVec()` function from the `Voigt` module.

```fortran
Mat = obj .Contraction. Rank2Tensorobj
Mat = Rank2Tensorobj .Contraction. obj
Mat = obj .Contraction. Mat
Mat = Mat .Contraction. obj
```

### Matmul Operator

Matmul operator is defined so that we can operate `MaterialJacobian_` object directly with the `VoigtVec`. Using `matmul` operator we can do matrix multiplication of obj with voigt vector.

```fortran
Vec = obj .matmul. Vec
Vec = Vec .matmul. obj
```

### Addition Operator

We have defined the addition operator for material jacobian class. We can add `obj + obj` `obj + Mat` `obj +Scalar`. Note that in first two cases the shape should be compatible. Suppose if the shapes are not identical then we can use `obj .Cijkl. N  + obj .Cijkl. N`. A Rank-2 fortran array is returned.

```fortran
Mat = obj + obj
Mat = obj + Mat
Mat = Mat + obj
Mat = obj + Scalar
Mat = Scalar + obj
```

### Subtraction Operator

```fortran
Mat = obj - obj
Mat = obj - Mat
Mat = Mat - obj
Mat = obj - Scalar
Mat = Scalar - obj
```

### Asterics Operator