# Deformation Gradient Class

## Structure

```fortran
 TYPE, PUBLIC, EXTENDS( Rank2Tensor_ ) :: DeformationGradient_
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: R
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: U
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: V
    REAL( DFP ), ALLOCATABLE, DIMENSION( : ) :: EigenVal
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: EigenVec_U
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: EigenVec_V
```

## Description

## Getting Started

### Constructing the object

The subroutine `Initiate()` can be used to create the `DeformationGradient_` class.

```fortran
CALL obj%Initiate( )
CALL obj%Initiate( Mat2( :, : ) )
CALL obj%Initiate( Scalar )
CALL obj%Initiate( VoigtVec, VoigtType )
CALL obj%Initiate( obj2 )
```

In addition we can use the function `DeformationGradient()` which returns the `DeformationGradient_` type.

```fortran
obj = DeformationGradient( )
obj = DeformationGradient( Mat2, FULL )
obj = DeformationGradient( Mat2 )
```

We have also defined function `DeformationGradient_Pointer()` that returns the pointer to the `DeformationGradient_` pointer.

```fortran
obj = DeformationGradient_Pointer( )
obj = DeformationGradient_Pointer( Mat2, FULL )
obj = DeformationGradient_Pointer( Mat2 )
```

Here `Full` can be `True` or `False`. If `True` then `R, U, V, EigenVal, EigenVec_U, EigenVec_V` all will be computed.

We can also use `Assignment Operator( = )`

```fortran
obj = Mat2( :, : )
```

### Deallocating the object

We can call `obj%Deallocate()`

### Getting the Rotation part

```fortran
R = .R. obj
```

### Getting the Right Stretch Tensor

```fortran
U = .U. obj
```

### Getting the Left Stretch Tensor

```fortran
V = .V. obj
```

### Getting the EigenValues of F, U, V

Not that U and V are similar tensor, therefore, F, U, V all have same eigenvalues.

```fortran
Val = .EigenValues. obj
```

### Getting the EigenVectors of U and V

```fortran
P( :, : ) = .EigenVectorsU. obj
P( :, : ) = .EigenVectorsV. obj
```

### Getting the Jacobian

```fortran
J = .J. obj
```

### Getting Right and Left Cauchy Green Deformation Tensor

```fortran
C = RightCauchyGreen( obj )
C = RightCauchyGreen( Mat )
C = .C. obj
C = .C. Mat
```

```fortran
B = LeftCauchyGreen( obj )
B = LeftCauchyGreen( Mat )
B = .B. obj
B = .B. Mat
```

### Getting the Strain

```fortran
E = GreenStrain( obj )
E = GreenStrain( Mat )
E = .GreenStrain. obj
E = .GreenStrain. Mat
```

```fortran
e = AlmansiStrain( obj )
e = AlmansiStrain( Mat )
e = .AlmansiStrain. obj
e = .AlmansiStrain. Mat
```