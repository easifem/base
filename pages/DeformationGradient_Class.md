# Deformation Gradient Class

This documentation is old. Update it.

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

### Constructing the Object

The subroutine `Initiate()` can be used to create the `DeformationGradient_` class.

```fortran
CALL Obj%Initiate( )
CALL Obj%Initiate( Mat2( :, : ) )
CALL Obj%Initiate( Scalar )
CALL Obj%Initiate( VoigtVec, VoigtType )
CALL Obj%Initiate( Obj2 )
```

In addition we can use the function `DeformationGradient()` which returns the `DeformationGradient_` type.

```fortran
Obj = DeformationGradient( )
Obj = DeformationGradient( Mat2, FULL )
Obj = DeformationGradient( Mat2 )
```

We have also defined function `DeformationGradient_Pointer()` that returns the pointer to the `DeformationGradient_` pointer.

```fortran
Obj = DeformationGradient_Pointer( )
Obj = DeformationGradient_Pointer( Mat2, FULL )
Obj = DeformationGradient_Pointer( Mat2 )
```

Here `Full` can be `True` or `False`. If `True` then `R, U, V, EigenVal, EigenVec_U, EigenVec_V` all will be computed.

We can also use `Assignment Operator( = )`

```fortran
Obj = Mat2( :, : )
```

### Deallocating the Object

We can call `Obj%DeallocateData()`

### Getting the Rotation part

```fortran
R = .R. Obj
```

### Getting the Right Stretch Tensor

```fortran
U = .U. Obj
```

### Getting the Left Stretch Tensor

```fortran
V = .V. Obj
```

### Getting the EigenValues of F, U, V

Not that U and V are similar tensor, therefore, F, U, V all have same eigenvalues.

```fortran
Val = .EigenValues. Obj
```

### Getting the EigenVectors of U and V

```fortran
P( :, : ) = .EigenVectorsU. Obj
P( :, : ) = .EigenVectorsV. Obj
```

### Getting the Jacobian

```fortran
J = .J. Obj
```

### Getting Right and Left Cauchy Green Deformation Tensor

```fortran
C = RightCauchyGreen( Obj )
C = RightCauchyGreen( Mat )
C = .C. Obj
C = .C. Mat
```

```fortran
B = LeftCauchyGreen( Obj )
B = LeftCauchyGreen( Mat )
B = .B. Obj
B = .B. Mat
```

### Getting the Strain

```fortran
E = GreenStrain( Obj )
E = GreenStrain( Mat )
E = .GreenStrain. Obj
E = .GreenStrain. Mat
```

```fortran
e = AlmansiStrain( Obj )
e = AlmansiStrain( Mat )
e = .AlmansiStrain. Obj
e = .AlmansiStrain. Mat
```