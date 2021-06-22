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
CALL Obj%Initiate( N )
CALL Obj%Initiate( N, Fill )
CALL Obj%Initiate( Mat )
CALL Obj%Initiate( N, StressType, StrainType, RateType )
CALL Obj%Initiate( N, [StressType, StrainType, RateType] )
```

We can also initiate the `MaterialJacobian_` using the function called `MaterialJacobian()` or `MaterialJacobian_Pointer()`

```fortran
Obj = MaterialJacobian( N )
Obj = MaterialJacobian( N, Fill )
Obj = MaterialJacobian( Mat )
Obj = MaterialJacobian( N, StressType, StrainType, RateType )
Obj = MaterialJacobian( N, [StressType, StrainType, RateType] )
```

```fortran
Obj => MaterialJacobian_Pointer( N )
Obj => MaterialJacobian_Pointer( N, Fill )
Obj => MaterialJacobian_Pointer( Mat )
Obj => MaterialJacobian_Pointer( N, StressType, StrainType, RateType )
Obj => MaterialJacobian_Pointer( N, [StressType, StrainType, RateType] )
```

```fortran
CALL Obj%Initiate( N )
Obj = MaterialJacobian( N )
Obj => MaterialJacobian_Pointer( N )
```

The above call allocate `Obj%C( N, N )` with all zero entries.

```fortran
CALL Obj%Initiate( N, Fill )
Obj = MaterialJacobian( N, Fill )
Obj => MaterialJacobian_Pointer( N, Fill )
```

The above call allocates `Obj%C(N,N)` with all entries equal to `Fill`.

```fortran
CALL Obj%Initiate( Mat )
Obj = MaterialJacobian( Mat )
Obj => MaterialJacobian_Pointer( Mat )
```

The above call allocates `Obj%C` with `Mat`.

```fortran
CALL Obj%Initiate( N, StressType, StrainType, RateType )
Obj = MaterialJacobian( N, StressType, StrainType, RateType )
Obj => MaterialJacobian_Pointer( N, StressType, StrainType, RateType )
```

The above call allocates `Obj%C` with size `N`. `StressType`, `StrainType`, and `RateType` can be `String_` object or character object.

```fortran
CALL Obj%Initiate( N, [StressType, StrainType, RateType] )
Obj = MaterialJacobian( N, [StressType, StrainType, RateType] )
Obj => MaterialJacobian_Pointer( N, [StressType, StrainType, RateType] )
```

The above call allocates `Obj%C` with size `N`. The size-3 rank-1 array can be can be `String_` object or character object.

### Setting the value of Names

We can set the values of `Obj%StressType` `Obj%StrainType`, and `Obj%RateType` using the subroutine.

```Fortran
CALL Obj%setStressType( StressType )
CALL Obj%setStrainType( StrainType )
CALL Obj%setRateType( RateType )
```

>The argument can be `Character` type or `String_` type.

### Getting the values of Names

```fortran
StressType = .StressType. Obj
StrainType = .StrainType. Obj
RateType = .RateType. Obj
```

### Geting the values of `Obj%C`

We can get the size of `Obj%C` using the operator called `.SIZE.` and we can deallocate the data using the routine called `Obj%DeallocateData()`.

We can access the values using both subroutines and functions.

Subroutines to access the hardcopy and pointer to `Obj%C` are given below.

```fortran
CALL Obj%getCijkl( Mat )
CALL Obj%getCijkl_Pointer( Mat )
```

Functions to access the hardcopy and pointer to `Obj%C` are given below.

```fortran
Mat => Obj%Cijkl_Pointer( )
Mat = Obj
```

The Operator `.Cijkl.` and `.At.` can also be used to access the hardcopies of Cijkl.

```fortran
Mat = .Cijkl. Obj
Mat = Obj .Cijkl. [Indx1, Indx2]
Mat = Obj .Cijkl. [i,j,k,l]
```

```fortran
CALL Obj%getCijkl( Mat )
Mat = Obj
```

The above call reallocates `Mat` with the `Obj%C`.

```fortran
CALL Obj%getCijkl_Pointer( Mat )
Mat => Obj%Cijkl_Pointer( )
```

The above call returns the Pointer to the `Obj%C`.

```fortran
Mat = .Cijkl. Obj
```

The above call will return the `Obj%C` hardcopy.

```fortran
Mat = Obj .Cijkl. [Indx1, Indx2]
```

The above call will return the `Obj%C( i,j)`

```fortran
Mat = Obj .Cijkl. [i,j,k,l]
```

The above call will return the `C(i,j,k,l)`. In this case `[i,j,k,l]` are convered into voigt index then value of `Obj%C` correspoding to those voigt-indices are returned.

There is another interesting way to use `.Cijkl.`. Suppose you want to obtain 6 by 6 jacobian matrix then we can call `Obj .Cijkl. 6`. In this case, even if `Obj%C` is not 6 by 6 we will get 6 by 6 form.

```fortran
C = Obj .Cijkl. 6
C = Obj .Cijkl. 4
C = Obj .Cijkl. 3
C = Obj .Cijkl. 2
C = Obj .Cijkl. 1
```

Alternatively you can also use `Obj .Shape. 6` or `Obj .Shape. M` for getting the M by M matrix.

### Assignment Operator (=)

```fortran
Obj = Mat
Mat = Obj
Obj = Obj2
```

### Contraction Operator

Contraction of Material Jacobian with the Tensor and matrix is defined. It will return a 3 by 3 matrix. If you want to convert it into voigt vector then use `VoigtVec()` function from the `Voigt` module.

```fortran
Mat = Obj .Contraction. Rank2TensorObj
Mat = Rank2TensorObj .Contraction. Obj
Mat = Obj .Contraction. Mat
Mat = Mat .Contraction. Obj
```

### Matmul Operator

Matmul operator is defined so that we can operate `MaterialJacobian_` object directly with the `VoigtVec`. Using `matmul` operator we can do matrix multiplication of Obj with voigt vector.

```fortran
Vec = Obj .matmul. Vec
Vec = Vec .matmul. Obj
```

### Addition Operator

We have defined the addition operator for material jacobian class. We can add `Obj + Obj` `Obj + Mat` `Obj +Scalar`. Note that in first two cases the shape should be compatible. Suppose if the shapes are not identical then we can use `Obj .Cijkl. N  + Obj .Cijkl. N`. A Rank-2 fortran array is returned.

```fortran
Mat = Obj + Obj
Mat = Obj + Mat
Mat = Mat + Obj
Mat = Obj + Scalar
Mat = Scalar + Obj
```

### Subtraction Operator

```fortran
Mat = Obj - Obj
Mat = Obj - Mat
Mat = Mat - Obj
Mat = Obj - Scalar
Mat = Scalar - Obj
```

### Asterics Operator