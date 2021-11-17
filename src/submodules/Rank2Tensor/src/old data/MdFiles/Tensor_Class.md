# Rank2Tensor Class is defined

## Notice

On 13-July-2018, when I tried to compile this module using ifort then there was an error related to .matmul. I think it is due to the fact that this operator was defined as an method as well as the module generic operator. So I have removed it as a method, and i have kept it only as module generic operator.


## ToDO

-Extend to `VelocityGradient_`
-Extend to `RightCauchyGreen_`
-Extend to `LeftCauchyGreen_`
-Extend to `StrainRate_`
-Extend to `SpinTensor_`
-Extend to `ContinuumSpin_`
-Extend to `MaterialJacobian_` a Rank-4 tensor but in Voigt form
-Add methods for getting derivative of invariants and Tensor.
-Add methods for Convective Rates
-Add methods so that T = Mat2
-Add EigenProjection methods.
-Add robust tensor-exponentatial function.
-Add method for getting the isochoric and volumetric part

## Structure

```fortran
 TYPE, PUBLIC :: Tensor_
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: T
    INTEGER( I4B ) :: NSD
```

## Description

`NSD` stands for number of spatial dimension. Rank2Tensor `T` is always (3,3), but NSD helps us to identify which components are meaningful.

## Getting Started

### Initiating the `Tensor_` object

The subroutine `Initiate()` can be used to create the `Tensor_` class.

```fortran
CALL obj%Initiate( )
CALL obj%Initiate( Mat2( :, : ) )
CALL obj%Initiate( Scalar )
CALL obj%Initiate( VoigtVec, VoigtType )
CALL obj%Initiate( obj2 )
```

In addition we can use the function `Rank2Tensor()` which returns the `Rank2Tensor_` type.

```fortran
obj = Rank2Tensor( )
obj = Rank2Tensor( Mat2 )
obj = Rank2Tensor( Scalar )
obj = Rank2Tensor( VoigtVec, VoigtType )
obj = Rank2Tensor( obj2 )
```

We have also defined function `Rank2Tensor_Pointer()` that returns the pointer to the `Rank2Tensor_` pointer.

```fortran
obj => Rank2Tensor_Pointer( )
obj => Rank2Tensor_Pointer( Mat2 )
obj => Rank2Tensor_Pointer( Scalar )
obj => Rank2Tensor_Pointer( VoigtVec, VoigtType )
obj => Rank2Tensor_Pointer( obj2 )
```

We can also use `Assignment Operator( = )`

```fortran
obj = Mat2( :, : )
```

```fortran
CALL obj%Initiate( )
obj = Rank2Tensor( )
obj => Rank2Tensor_Pointer( )
```

The above call will create the `Tensor_` object with all zero entries and NSD = 3.

```fortran
CALL obj%Initiate( Mat2 )
obj = Rank2Tensor( Mat2 )
obj => Rank2Tensor_Pointer( Mat2 )
```

The above call will create the `Tensor_` object. Depending upon the size of `Mat2(:,:)` NSD is decided.

```fortran
CALL obj%Initiate( Scalar )
obj = Rank2Tensor( Scalar )
obj => Rank2Tensor_Pointer( Scalar )
```

The above call will fill all entries with the scalar, and `NSD` will be set to 3.

```fortran
CALL obj%Initiate( VoigtVec, VoigtType )
obj = Rank2Tensor( VoigtVec, VoigtType )
obj => Rank2Tensor_Pointer( VoigtVec, VoigtType )
```

The above call will make tensor object from the voigt vector. `VoigtType` can be `Stress` or `Strain`.

```fortran
CALL obj%Initiate( obj2 )
obj = Rank2Tensor( obj2 )
obj => Rank2Tensor_Pointer( obj2 )
```

The above call will make tensor object from other tensor object.

### Checking the status and deallocating the data

```fortran
CALL obj%isInitiated( )
CALL obj%Deallocate( )
```

### Setting and getting the NSD

```fortran
NSD = obj%getNSD( )
NSD = .NSD. obj
CALL obj%setNSD( NSD )
```

### Getting the tensor

We can get the Tensor in a matrix as well as voigt vector. To get tensor in voigt vector we need to call `getTensor()`

```fortran
CALL obj%getTensor( Mat )
CALL obj%getTensor( VoigtVec, VoigtType )
```

Both `Mat` and `VoigtVec` must be allocatable as they are reallocated by the method.

We can use assignment operator.

```fortran
Mat = obj
```

### Logical Functions for Tensor

We have defined the function `isSymmetric()` and `isDeviatoric()`

```fortran
L = isSymmetric( obj )
L = isSymmetric( Mat2 )
L = isDeviatoric( obj )
L = isDeviatoric( Mat2 )
```

### Invariants

**Trace Of Tensor or Matrix**

```fortran
t = Trace( obj )
t = Trace( Mat )
t = .Trace. obj
t = .Trace. Mat
```

**Contraction of Tensors**

```fortran
s = DoubleDot_Product( obj, obj2 )
s = DoubleDot_Product( obj, Mat )
s = DoubleDot_Product( obj, VoigtVec, VoigtType )
s = DoubleDot_Product( A, B )
s = DoubleDot_Product( A, VoigtType_A, B, VoigtType_B )
s = DoubleDot_Product( Mat, VoigtVec, VoigtType )
```

We also defined the `DoubleDot` operator. This operator works only on matrices and Rank2Tensor_ objects.

```fortran
s = obj .doubledot. obj
s = obj .doubledot. mat
s = mat .doubledot. obj
s = mat .doubledot. mat
```

**Invariant_I1**

$$I_1 = Trace( T ) $$

```fortran
I1 = Invarinant_I1( obj )
I1 = Invarinant_I1( Mat )
```

**Invariant_I2**

`I2 = 0.5( ( Tr( T )**2 - Tr( T*T ) ) )`

```fortran
I2 = Invarinant_I2( obj )
I2 = Invarinant_I2( Mat )
```

**Invariant_I3**

`I3 = det( Tensor )`

```fortran
I3 = Invarinant_I3( obj )
I3 = Invarinant_I3( Mat )
```

**Invariant_J2**

`I2 = 0.5 * Dev( T ): Dev( T )`

```fortran
J2 = Invarinant_J2( obj )
J2 = Invarinant_J2( Mat )
```

**Invariant_J3**

`J3 = det( Dev( T ) )`

```fortran
J3 = Invarinant_J3( obj )
J3 = Invarinant_J3( Mat )
```

**LodeAngle**


```fortran
theta = LodeAngle( J2, J3, LodeAngleType )
theta = LodeAngle( obj, LodeAngleType )
theta = LodeAngle( Mat, LodeAngleType )
```

`LodeAngleType` can be `Sine` or `Cosine`.


### Tensor Decomposition

**Getting Symmetric and Skew Symmetric Part**

```fortran
Dummy = SymmetricPart( obj )
Dummy = SymmetricPart( Mat )
Dummy = AntiSymmetricPart( obj )
Dummy = AntiSymmetricPart( Mat )
```

We can also use the operator `.Sym.` and `.Anti.`

```fortran
Dummy = .Sym. obj
Dummy = .Sym. Mat
Dummy = .Anti. obj
Dummy = .Anti. Mat
```

**getting the Hydrostatic Part**

`Trace( T ) / 3`

```fortran
Dummy = HydrostaticPart( obj )
Dummy = HydrostaticPart( Mat )
Dummy = SphericalPart( obj )
Dummy = Spherical( Mat )
```

We can also use `.Hydro.` operator.

```fortran
Dummy = .Hydro. obj
Dummy = .Hydro. Mat
```

**Getting the Deviatoric Part**

```fortran
Dummy = DeviatoricPart( obj )
Dummy = DeviatoricPart( Mat )
Dummy = .Dev.( obj )
Dummy = .Dev.( Mat )
```

### Pull-Back operation

```fortran
Dummy = Pullback( T, F, indx1, indx2 )
```

`T` can be a `Rank2Tensor_` object or Matrix of (3,3) shape. `F` can be a `Rank2Tensor_` or Matrix of (3,3) shape. Indx1, and Indx2 should be `Contra`, `CoVar`.

We can also use `Pullback` of vector using the same functions.

```fortran
Dummy = Pullback( Vec, F, indx1 )
```

`F` can be a `Rank2Tensor_` object or Matrix of (3,3) shape.

### Push-Forward operation

```fortran
Dummy = PushForward( T, F, indx1, indx2 )
```

`T` can be a `Rank2Tensor_` object or Matrix of (3,3) shape. `F` can be a `Rank2Tensor_` or Matrix of (3,3) shape. Indx1, and Indx2 should be `Contra`, `CoVar`.

We can also use `Pullback` of vector using the same functions.

```fortran
Dummy = PushForward( Vec, F, indx1 )
```

`F` can be a `Rank2Tensor_` object or Matrix of (3,3) shape.

## Spectral Decomposition of tensor

**Getting the EigenValues and EigenVectors**

We have defined the routine called `Tensor_Eigens` for getting the eigen values and eigen-vectors.

```fortran
CALL Tensor_Eigens( Mat, EigenValues, EigenVectors )
```

EigenValues can be Rank-2 or Rank-1 fortran array. If Rank-1 then only real parts of eigen-values will be returned.

**Getting the PrincipalValue**

Principal values is defined by the maximum eigen value.

```fortran
dummy = Tensor_PrincipalValue( obj )
dummy = Tensor_PrincipalValue( Mat )
```

**Getting the SpectralRadius**

```fortran
dummy = Tensor_SpectralRadius( obj )
dummy = Tensor_SpectralRadius( Mat )
```

### Polar Decomposition

```fortran
CALL PolarDecomposition( Mat, R, H, PDType )
CALL PolarDecomposition( obj, R, H, PDType )
```

```fortran
R = RotationPart( Mat )
R = RotationPart( obj )
```

### Vector Operations

**VectorProduct**

We have defined `VectorProduct()` function for computing the cross product( also known as vector product ), It returns a length 3 vector. `VectorProduct(u,v)` is $u \times v$. `VectorProduct(u,v,w)`  is equivalent to $u \times ( v \times w )$

```fortran
Vec = VectorProduct( u, v )
Vec = u .X. v
Vec = VectorProduct( u, v, w )
```

**BoxProduct**

The `BoxProduct(u,v,w)` is equivalent to $[u,v,w] = u \cdot ( v \times w )$

```fortran
Dummy = BoxProduct( u, v, w)
```

**getAngle**

Returns the angle (in radians) betrween two vectors

```fortran
theta = getAngle( u, v )
theta = u .Angle. v
```

**getProjection**

`getProjection(u,v)` project vector u on v and returns the projection vector in the direction of v.

```fortran
P = u .ProjectOn. v
```

**UnitVector**

Returns the unit vector

```fortran
uhat = UnitVector( u )
uhat = .UnitVector. u
```

**Dot Product**

```fortran
s =  DOT_PRODUCT( u, v )
s = u .dot. v
```

**Normal and Parallel components**

We have defined two operators to decompose a vector in the direction along and perpendicular to some vector.

```fortran
p = u .ComponentParallelTo. v
n = u .ComponentNormalTo. v
```

**Vector2D**

`Vector2D` converts any vector in two 2D vector format.

**Vector3D**

`vector3D` converts any vector in 3D vector format.

**Vector1D**

`vector1D` converts any vector in 1D vector format.


### Operator Overloading

**Contraction**

```fortran
obj .Contraction. MaterialJacobianobj
MaterialJaconbianobj .Contraction. obj
```
































## Construction Methods

```fortran
CALL obj%initiate( )
CALL obj%Initiate( Mat )
CALL obj%Initiate( Scalar )
CALL obj%Initiate( VoigtVec, VoigtType )
CALL obj%Initiate( obj2 )
```

```fortran
obj => Tensor_Pointer( )
obj => Tensor_Pointer( Mat )
obj => Tensor_Pointer( Scalar )
obj => Tensor_Pointer( VoigtVec, VoigtType )
obj => Tensor_Pointer( obj2 )
```

```fortran
obj = Rank2Tensor( )
obj = Rank2Tensor( Mat )
obj = Rank2Tensor( Scalar )
obj = Rank2Tensor( VoigtVec, VoigtType )
obj = Rank2Tensor( obj2 )
```

### Initiate()

<mark>Type-1</mark>

Interface

```fortran
 SUBROUTINE Initiate1( obj )
    CLASS( Tensor_ ), INTENT( INOUT ) :: obj
    IF( ALLOCATED( obj%T ) ) DEALLOCATE( obj%T )
    ALLOCATE( obj%T( 3, 3 ) )
    obj%NSD = 3
    obj%T = 0.0_DFP
 END SUBROUTINE Initiate1
```

Description

See the above code.

<mark>Type-2</mark>

Interface

```fortran
 SUBROUTINE Initiate2( obj, A )
    CLASS( Tensor_ ), INTENT( INOUT ) :: obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: A
```

Description

Here `A` should be a square matrix of the size in the list {1,2,3}.

<mark>Type-3</mark>

Interface

```fortran
 SUBROUTINE Initiate3( obj, A )
    CLASS( Tensor_ ), INTENT( INOUT ) :: obj
    REAL( DFP ), INTENT( IN ) :: A

    IF( ALLOCATED( obj%T ) ) DEALLOCATE( obj%T )
    ALLOCATE( obj%T( 3, 3 ) )
    obj%T = A
 END SUBROUTINE Initiate3
```

Description

See the code above

<mark>Type-4</mark>

Interface

```fortran
 SUBROUTINE Initiate4( obj, A, VoigtType )
    USE Voigt
    CLASS( Tensor_ ), INTENT( INOUT ) :: obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType
```

Description

Coverts a Voigt vector into a tensor.

<mark>Type-5</mark>

Interface

```fortran
 SUBROUTINE Initiate5( obj, obj2 )
    CLASS( Tensor_ ), INTENT( INOUT ) :: obj
    CLASS( Tensor_ ), INTENT( IN ) :: obj2
```

Description

Coverts a Voigt vector into a tensor.

### Tensor_Pointer( )

<mark>Type-1</mark>

Interface

```fortran
 FUNCTION Constructor1( )
    CLASS( Tensor_ ), POINTER :: Constructor1

    ALLOCATE( Tensor_ :: Constructor1 )
    CALL Constructor1%Initiate( )
 END FUNCTION Constructor1
```

Description

<mark>Type-2</mark>

Interface

```fortran
 FUNCTION Constructor2( A )
    CLASS( Tensor_ ), POINTER :: Constructor2
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: A
    Error_Flag = .FALSE.
    ALLOCATE( Tensor_ :: Constructor2 )
    CALL Constructor2%Initiate( A )
 END FUNCTION Constructor2
```

Description

<mark>Type-3</mark>

Interface

```fortran
 FUNCTION Constructor3( A )
    CLASS( Tensor_ ), POINTER :: Constructor3
    REAL( DFP ), INTENT( IN ) :: A

    Error_Flag = .FALSE.

    ALLOCATE( Tensor_ :: Constructor3 )
    CALL Constructor3%Initiate( A )
 END FUNCTION Constructor3
```

Description

<mark>Type-4</mark>

Interface

```fortran
 FUNCTION Constructor4( A, VoigtType )
    USE Voigt
    CLASS( Tensor_ ), POINTER :: Constructor4
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType

    Error_Flag = .FALSE.
    ALLOCATE( Tensor_ :: Constructor4 )
    CALL Constructor4%Initiate( A, VoigtType )
 END FUNCTION Constructor4
```

Description

<mark>Type-5</mark>

Interface

```fortran
 FUNCTION Constructor10( obj )
    CLASS( Tensor_ ), POINTER :: Constructor10
    CLASS( Tensor_ ), INTENT( IN ) :: obj

    ALLOCATE( Constructor10 )
    CALL Constructor10%Initiate( obj )
 END FUNCTION Constructor10
```

Description

### Rank2Tensor( )

<mark>Type-1</mark>

Interface

```fortran
 FUNCTION Constructor5( )
    TYPE( Tensor_ ) :: Constructor5

    CALL Constructor5%Initiate( )
 END FUNCTION Constructor5
```

Description

<mark>Type-2</mark>

Interface

```fortran
 FUNCTION Constructor6( A )
    TYPE( Tensor_ ) :: Constructor6
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: A

    Error_Flag = .FALSE.
    CALL Constructor6%Initiate( A )
 END FUNCTION Constructor6
```

Description

<mark>Type-3</mark>

Interface

```fortran
 FUNCTION Constructor7( A )
    TYPE( Tensor_ ) :: Constructor7
    REAL( DFP ), INTENT( IN ) :: A

    Error_Flag = .FALSE.

    CALL Constructor7%Initiate( A )
 END FUNCTION Constructor7
```

Description

<mark>Type-4</mark>

Interface

```fortran
 FUNCTION Constructor8( A, VoigtType )
    USE Voigt
    TYPE( Tensor_ ) :: Constructor8
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType


    Error_Flag = .FALSE.
    CALL Constructor8%Initiate( A, VoigtType )

    CALL Check_Error( &
        "Tensor_Class.F90>>Constructor.part>>Constructor8()", &
        "TraceBack --->  CALL Constructor8%Initiate( A, VoigtType )" &
        )
 END FUNCTION Constructor8
```

Description

<mark>Type-5</mark>

Interface

```fortran
 FUNCTION Constructor9( obj )
    TYPE( Tensor_ ) :: Constructor9
    CLASS( Tensor_ ), INTENT( IN ) :: obj

    CALL Constructor9%Initiate( obj )
 END FUNCTION Constructor9
```

Description

### getNSD( )

Interface

```fortran
 INTEGER( I4B ) FUNCTION getNSD( obj )
    CLASS( Tensor_ ), INTENT( IN ) :: obj
    getNSD = obj%NSD
 END FUNCTION getNSD
```

### getTensor

<mark>Type-1</mark>

```fortran
 SUBROUTINE getTensor_1( obj, T )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ), INTENT( INOUT ) :: T

    Error_Flag = .FALSE.

    IF( .NOT. obj%isInitiated( ) ) THEN
        CALL Err_Msg(                                       &
                      "Tensor_Class.F90>>getTensor_1.part",   &
                      "getTensor_1()",                        &
                      "Tensor obj is Not Initiated"         &
                    )
        Error_Flag = .TRUE.
        RETURN

    END IF

    IF( ALLOCATED( T )  ) DEALLOCATE( T )
    T = obj%T

END SUBROUTINE getTensor_1
```

<mark>Type-2</mark>

```fortran
 SUBROUTINE getTensor_2( obj, Vec, VoigtType )
    USE Voigt
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), ALLOCATABLE, DIMENSION( : ), INTENT( INOUT ) :: Vec
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType
```

Description

If `obj%NSD` is 2 then `Vec` has length 4. Note that `Vec` is reallocated by the routine.

<mark>Type-3</mark>

```fortran
 SUBROUTINE getTensor_3( T, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ), INTENT( OUT ) :: T
```

> This subroutine is used for overloading the assignment operator. Now we can obtain the value using `Mat = obj`.

## Operator Overloading ( * )

<mark>Type-1</mark>

```fortran
 FUNCTION TensorTimesScalar_1( obj, Scalar )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: Scalar
    REAL( DFP ), DIMENSION( 3, 3 ) :: TensorTimesScalar_1
```

`obj * 2.0_DFP` returns a (3,3) matrix.

<mark>Type-2</mark>

```fortran
 FUNCTION TensorTimesScalar_2( Scalar, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: Scalar
    REAL( DFP ), DIMENSION( 3, 3 ) :: TensorTimesScalar_2
```

`2.0_DFP * obj` returns a (3,3) matrix.

<mark>Type-3</mark>

```fortran
 FUNCTION TensorTimesScalar_3( obj, Scalar )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: Scalar
    REAL( DFP ), DIMENSION( 3, 3 ) :: TensorTimesScalar_3
```

`obj * 2` returns a (3,3) matrix.

<mark>Type-4</mark>

```fortran
 FUNCTION TensorTimesScalar_4( Scalar, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: Scalar
    REAL( DFP ), DIMENSION( 3, 3 ) :: TensorTimesScalar_4
```

`2 * obj` returns a (3,3) matrix.

<mark>Type-5</mark>

```fortran
 FUNCTION TensorTimesTensor( obj, obj2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj, obj2
    REAL( DFP ), DIMENSION( 3, 3 ) :: TensorTimesTensor
```

`obj1 * obj2` perfoms element wise multiplication

<mark>Type-6</mark>

```fortran
 FUNCTION TensorTimesVector( obj, Vec )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Vec
    REAL( DFP ), DIMENSION( 3 ) :: TensorTimesVector
```

`obj * Vec` returns array of length-3 after performing matrix vector multiplication. Symbolically, $w = T \cdot v$

Example

```fortran
TENSOR =

         1.000000         1.000000         1.000000
         1.000000         1.000000         1.000000
         1.000000         1.000000         1.000000

NSD =  3

Vec2 = T * Vec1
     6.0000         6.0000         6.0000

Vec2 = T * [1.d0, 2.d0, 3.d0]
     6.0000         6.0000         6.0000

Vec1 = T * Vec1
     6.0000         6.0000         6.0000

Vec2 = T * [1.d0, 2.d0]
     3.0000         3.0000         0.0000

Vec2 = T * [1.d0]
     1.0000         0.0000         0.0000
```

<mark>Type-7</mark>

```fortran
 FUNCTION VectorTimesTensor( Vec, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Vec
    REAL( DFP ), DIMENSION( 3 ) :: VectorTimesTensor
```

`Vec * obj` returns array of length-3 after performing matrix vector multiplication. Symbolically $w = v \cdot T$

Example

```fortran
TENSOR =

         1.000000         2.000000         3.000000
         1.000000         2.000000         3.000000
         1.000000         2.000000         3.000000

NSD =  3

Vec2 = Vec1 * T
     6.0000         12.000         18.000

Vec2 = [1.d0, 2.d0, 3.d0] * T
     6.0000         12.000         18.000

Vec1 = Vec1 * T
     6.0000         12.000         18.000

Vec2 = [1.d0, 2.d0] * T
     3.0000         6.0000         0.0000

Vec2 = [1.d0] * T
     1.0000         0.0000         0.0000
```

<mark>Type-8</mark>

```fortran
 FUNCTION TensorTimesMat( obj, Mat )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: TensorTimesMat
```

<mark>Type-9</mark>

```fortran
 FUNCTION MatTimesTensor( Mat, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: MatTimesTensor
```

### MatMul

<mark>Type-1</mark>

```fortran
 FUNCTION MatMul_1( obj, obj2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj, obj2
    REAL( DFP ), DIMENSION( 3, 3 ) :: MatMul_1
```

<mark>Type-2</mark>

```fortran
 FUNCTION MatMul_2( obj, Mat2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat2
    REAL( DFP ), DIMENSION( 3, 3 ) :: MatMul_2

```

<mark>Type-3</mark>

```fortran
 FUNCTION MatMul_3( Mat2, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat2
    REAL( DFP ), DIMENSION( 3, 3 ) :: MatMul_3
```

<mark>Type-4</mark>

```fortran
 FUNCTION VectorTimesTensor( Vec, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Vec
    REAL( DFP ), DIMENSION( 3 ) :: VectorTimesTensor
```

<mark>Type-5</mark>

```fortran
 FUNCTION TensorTimesVector( obj, Vec )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Vec
    REAL( DFP ), DIMENSION( 3 ) :: TensorTimesVector
```

### Dyadic Product/Otimes

<mark>Type-1</mark>

```fortran
 FUNCTION Tensor_Dyadic_Tensor( obj, obj2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj, obj2
    REAL( DFP ), DIMENSION( 6, 6 ) :: Tensor_Dyadic_Tensor
```

<mark>Type-2</mark>

```fortran
 FUNCTION Tensor_Dyadic_Mat( obj, Mat )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 6, 6 ) :: Tensor_Dyadic_Mat
```

<mark>Type-3</mark>

```fortran
 FUNCTION Mat_Dyadic_Tensor( Mat, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 6, 6 ) :: Mat_Dyadic_Tensor
```

> Note that .Otimes. always return a (6,6) matrix. If you want to use (4,4) or (3,3) matrix then use `Mat4_From_Mat6`, and `Mat3_From_Mat6` function.
> For Voigt represent of rank-2 tensor dyadic product both tensor must be symmetric.

Examples

```fortran
    CALL T%FreeThePointer( T )

    T => Rank2Tensor_Pointer( RESHAPE( [                  &
                                        1.d0, 1.d0, 1.d0, &
                                        1.d0, 2.d0, 3.d0, &
                                        1.d0, 3.d0, 3.d0  &
                                        ],                &
                                        [3,3]             &
                                    )                     &
                            )

    CALL T%Display( )

    DummyMat = T .Otimes. T

    CALL Display_Array( DummyMat, "T .Otimes. T" )

    DummyMat = T
    CALL Display_Array( ( T .Otimes. DummyMat ), "T .Otimes. DummyMat " )

    DummyMat = T .Otimes. DummyMat
    CALL Display_Array( DummyMat, "DummyMat = T .Otimes. DummyMat " )

    DummyMat = T
    CALL Display_Array( ( DummyMat .Otimes. T ), "DummyMat .Otimes. T " )
```

Resutls

```fortran
TENSOR =

         1.000000         1.000000         1.000000
         1.000000         2.000000         3.000000
         1.000000         3.000000         3.000000

NSD =  3


T .Otimes. T=

           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           2.000000         4.000000         6.000000         2.000000         6.000000         2.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000


T .Otimes. DummyMat=

           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           2.000000         4.000000         6.000000         2.000000         6.000000         2.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000


DummyMat = T .Otimes. DummyMat=

           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           2.000000         4.000000         6.000000         2.000000         6.000000         2.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000

DummyMat .Otimes. T=

           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           2.000000         4.000000         6.000000         2.000000         6.000000         2.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
           3.000000         6.000000         9.000000         3.000000         9.000000         3.000000
           1.000000         2.000000         3.000000         1.000000         3.000000         1.000000
```

Example of getting (4,4) array from (6,6) array

```fortran
    DummyMat = Mat4_From_Mat6( DummyMat .Otimes. T )
    CALL Display_Array( DummyMat, "DummyMat = Mat4_From_Mat6( DummyMat .Otimes. T ) ")
```

Result

```fortran
DummyMat = Mat4_From_Mat6( DummyMat .Otimes. T )=

           1.000000         2.000000         1.000000         3.000000
           2.000000         4.000000         2.000000         6.000000
           1.000000         2.000000         1.000000         3.000000
           3.000000         6.000000         3.000000         9.000000
```

### Transpose

```fortran
 FUNCTION Transpose_1( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: Transpose_1
```

> Returns a (3,3) matrix.

### Addition

Type-1

```fortran
 FUNCTION obj_Add_obj( obj, obj2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj, obj2
    REAL( DFP ), DIMENSION( 3, 3 ) :: obj_Add_obj
    obj_Add_obj = obj%T + obj2%T
 END FUNCTION obj_Add_obj
```

Example : `obj + obj2`

Type-2

```fortran
 FUNCTION obj_Add_Mat( obj, Mat )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: obj_Add_Mat
    obj_Add_Mat = obj%T + Mat
 END FUNCTION obj_Add_Mat
```

Example: `obj + Mat`

Type-3

```fortran
 FUNCTION Mat_Add_obj( Mat, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: Mat_Add_obj
    Mat_Add_obj = obj%T + Mat
 END FUNCTION Mat_Add_obj
```

Example: `Mat + obj`

### Subtraction

Type-1

```fortran
 FUNCTION obj_Minus_obj( obj, obj2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj, obj2
    REAL( DFP ), DIMENSION( 3, 3 ) :: obj_Minus_obj
    obj_Minus_obj = obj%T - obj2%T
 END FUNCTION obj_Minus_obj
```

Example : `obj - obj2`

Type-2

```fortran
 FUNCTION obj_Minus_Mat( obj, Mat )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: obj_Minus_Mat
    obj_Minus_Mat = obj%T - Mat
 END FUNCTION obj_Minus_Mat
```

Example: `obj - Mat`

Type-3

```fortran
 FUNCTION Mat_Minus_obj( Mat, obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: Mat_Minus_obj
    Mat_Minus_obj = obj%T - Mat
 END FUNCTION Mat_Minus_obj
```

Example: `Mat - obj`

## Vector Methods

### VectorProduct

Type-1

```fortran
 FUNCTION VectorProduct2( u, v )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: u, v
    REAL( DFP ), DIMENSION( 3 ) :: VectorProduct2
```

Description

Computes $u \times v$

Type-2

```fortran
 FUNCTION VectorProduct3( u, v, w )
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) :: u, v, w
    REAL( DFP ), DIMENSION( 3 ) :: VectorProduct3
```

Description

Computes $u \times ( v \times w )$

### BoxProduct

```fortran
 REAL( DFP ) FUNCTION BoxProduct( u, v, w )
    USE Utility, ONLY: Det
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) :: u, v, w
```

Description

Computes $[u,v,w] = u \cdot (v \times w)$

Example

```fortran
    Vec1 = [1.d0, 2.d0, 0.d0]
    Vec2 = [1.d0, 1.d0, 0.d0]
    Vec3 = Vec1 .X. Vec2
    CALL Display_Array( Vec3, "Vec3 = Vec1 .X. Vec2 ")
    CALL Display_Array( &
                        VectorProduct( Vec1, Vec2, Vec3 ),  &
                        "VectorProduct( Vec1, Vec2, Vec3 )" &
                      )

    CALL Display_Array(                             &
                        -Vec2 .x. Vec3 .x. Vec1,    &
                        "-Vec2 .x. Vec3 .x. Vec1"   &
                      )
    CALL Display_Array( [Box(Vec1, Vec2, Vec3)], "Box[V1, V2, V3] ")
```

### getAngle

```Fortran
 REAL( DFP ) FUNCTION getAngle( u, v )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: u, v
```

Returns angle between two vectors

Example

```fortran
    Vec1 = [1.d0, 0.d0, 0.d0]
    Vec2 = [1.d0, 1.d0, 0.d0]

    CALL Display_Array( [Vec1.Angle.Vec2], "Vec1.Angle.Vec2")
```

### getProjection

```fortran
 FUNCTION getProjection( u, v )
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) :: u, v
    REAL( DFP ), DIMENSION( 3 ) :: getProjection
```

Project u on v. New Operator is defined `u .ProjectOn. v`.

Example

```fortran
    Vec1 = [1.d0, 0.d0, 0.d0]
    Vec2 = [-1.d0, 1.d0, 0.d0]

    CALL T%FreeThePointer( T )
    T => Rank2Tensor_Pointer( RESHAPE( [                  &
                                        1.d0, 1.d0, 1.d0, &
                                        2.d0, 2.d0, 2.d0, &
                                        3.d0, 3.d0, 3.d0  &
                                        ],                &
                                        [3,3]             &
                                    )                     &
                            )

    CALL Display_Array( T*Vec2 .ProjectOn. Vec1, "T*Vec2 .ProjectOn. Vec1")
```

### UnitVector

```fortran
 FUNCTION UnitVector( u )
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) :: u
    REAL( DFP ), DIMENSION( 3 ) :: UnitVector
```

Returns the unit vector.

### DotProduct

```fortran
 FUNCTION DotProduct( u, v )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: u, v
    REAL( DFP ) :: DotProduct
    DotProduct = DOT_PRODUCT( u, v )
 END FUNCTION DotProduct
```

Returns the dot product, used for defining the operator `u .dot. v`

Example

```fortran
    Vec1 = [1.d0, -1.d0, 0.d0]
    Vec2 = [-1.d0, 1.d0, 0.d0]
    dp = Vec1 .dot. Vec2
```

### getNormalComponent

```fortran
 FUNCTION getNormalComponent( u, v )
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) :: u, v
    REAL( DFP ), DIMENSION( 3 ) :: getNormalComponent
    getNormalComponent = u - ( u .ProjectOn. v )
 END FUNCTION getNormalComponent
```

Returns component of `u` that is normal to `v`. New operator is defined `u .ComponentNormalTo. v`

### getParallelComponent

Alias of `getProjection` method. it is used to define a new operator `u .ComponentParallelTo. v`.


Example

```fortran
    Vec1 = [1.d0, 1.d0, 1.d0]
    CALL Display_Array( Vec1, "Vec1 ")

    Vec2 = [1.d0, 0.d0, 0.d0]
    CALL Display_Array( Vec2, "Vec2 ")

    Vec3 = Vec1 .ComponentParallelTo. Vec2
    CALL Display_Array( Vec3, "Vec3 = Vec1 .ComponentParallelTo. Vec2 ")

    Vec3 = Vec1 .ComponentNormalTo. Vec2
    CALL Display_Array( Vec3, "Vec3 = Vec1 .ComponentNormalTo. Vec2 ")

    Vec3 = (Vec1 .ComponentNormalTo. Vec2) + (Vec1 .ComponentParallelTo. Vec2)
    CALL Display_Array( Vec3, &
    " Vec3 = Vec1 .ComponentNormalTo. Vec2 + Vec1 .ComponentParallelTo. Vec2 ")
```

### Vector3D

```fortran
 FUNCTION Vector3D( u )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: u
    REAL( DFP ), DIMENSION( 3 ) :: Vector3D

    Vector3D = 0.0_DFP
    SELECT CASE( SIZE( u ) )
        CASE( 1 )
            Vector3D( 1 ) = u( 1 )
        CASE( 2 )
            Vector3D( 1 : 2 ) = u( 1 : 2 )
        CASE DEFAULT
            Vector3D( 1: 3 ) = u( 1 : 3 )
    END SELECT
 END FUNCTION Vector3D
```

### Vector2D

```fortran
 FUNCTION Vector2D( u )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: u
    REAL( DFP ), DIMENSION( 2 ) :: Vector2D
    Vector2D = 0.0_DFP
    SELECT CASE( SIZE( U ) )
    CASE( 1 )
        Vector2D( 1 ) = U( 1 )
    CASE DEFAULT
        Vector2D( 1: 2 ) = U( 1: 2 )
    END SELECT
 END FUNCTION Vector2D
```

### Vector1D

```fortran
 FUNCTION Vector1D( u )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: u
    REAL( DFP ), DIMENSION( 1 ) :: Vector1D
    Vector1D( 1 ) = u( 1 )
 END FUNCTION Vector1D
```

> We have made BoxProduct, VectorProduct, Box, getAngle, getProjection, UnitVector, getParallelComponent, getNormalComponent, Vector2D, Vector3D, Vector1D public. These functions can be used as vector functions.
> In addition we have defined the OPERATOR( .X. ), OPERATOR( .Angle. ), OPERATOR( .ProjectOn. ), OPERATOR( .dot. ), OPERATOR( .ComponentParallelTo. ), OPERATOR( .ComponentNormalTo. ).

## Tensor Decomposition

### Symmetric Part

Method

```fortran

```

Function

```fortran

```

### Symmetric Part

Method

```fortran
 FUNCTION m_SymmetricPart( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: m_SymmetricPart
    m_SymmetricPart = 0.5_DFP * ( obj%T + TRANSPOSE( obj%T ) )
 END FUNCTION m_SymmetricPart
```

Function

```fortran
 FUNCTION f_SymmetricPart( Mat )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( SIZE( Mat, 1 ), SIZE( Mat, 2 ) ) :: f_SymmetricPart
    f_SymmetricPart = 0.5_DFP * ( Mat + TRANSPOSE( Mat ) )
 END FUNCTION f_SymmetricPart
```

### AntiSymmetric Part

Method

```fortran
 FUNCTION m_AntiSymmetricPart( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: m_AntiSymmetricPart
    m_AntiSymmetricPart = 0.5_DFP * ( obj%T - TRANSPOSE( obj%T ) )
 END FUNCTION m_AntiSymmetricPart
```

Function

```fortran
 FUNCTION f_AntiSymmetricPart( Mat )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( SIZE( Mat, 1 ), SIZE( Mat, 2 ) ) :: &
                                                        f_AntiSymmetricPart
    f_AntiSymmetricPart = 0.5_DFP * ( Mat - TRANSPOSE( Mat ) )
 END FUNCTION f_AntiSymmetricPart
```

### Hydrostatic Part

Method

```fortran
 FUNCTION m_HydrostaticPart( obj )
    USE Utility, ONLY : Eye
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: m_HydrostaticPart
    m_HydrostaticPart = Trace( obj ) * Eye( 3 ) / 3
 END FUNCTION m_HydrostaticPart
```

Function

```fortran
 FUNCTION f_HydrostaticPart( Mat )
    USE Utility, ONLY : Eye
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: f_HydrostaticPart
    f_HydrostaticPart = Trace( Mat ) * Eye( 3 ) / 3
 END FUNCTION f_HydrostaticPart
```

### Deviatoric Part

Method

```fortran
 FUNCTION m_DeviatoricPart( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: m_DeviatoricPart

    m_DeviatoricPart = obj%T - HydrostaticPart( obj )
```

Function

```fortran
 FUNCTION f_DeviatoricPart( Mat )
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( 3, 3 ) :: f_DeviatoricPart
    f_DeviatoricPart = Mat - HydrostaticPart( Mat )
 END FUNCTION f_DeviatoricPart
```

## Invariants

### Trace

Method

```fortran
I = obj%Trace( )
```

Returns the trace of Tensor object.

Module Function

```fortran
I = Trace( T )
```

Returns the trace of fortran array `T`.

### Double_DotProduct

There is a generic method _Double\_DotProduct_ and module-function called _Double\_DotProduct_. Therefore you can use this function e.g. `real_val = obj%Double_DotProduct( ... )` as a method as well as a module-function `real_val = Double_DotProduct()`.

Type-1

```fortran
 REAL( DFP ) FUNCTION DoubleDot_Product1( obj, obj2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj, obj2
    DoubleDot_Product1 = SUM( obj * obj2 )
 END FUNCTION DoubleDot_Product1
```

Type-2

```fortran
 REAL( DFP ) FUNCTION DoubleDot_Product2( obj, A )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: A
    DoubleDot_Product2 = SUM( obj * A )
 END FUNCTION DoubleDot_Product2
```

Type-3

```fortran
 REAL( DFP ) FUNCTION DoubleDot_Product3( A, B )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: A, B
    DoubleDot_Product3 = SUM( A * B )
 END FUNCTION DoubleDot_Product3
```

Type-4

```fortran
 REAL( DFP ) FUNCTION DoubleDot_Product4( obj, A, VoigtType )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: A
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType

    TYPE( Rank2Tensor_ ) :: T
    T = Rank2Tensor( A, VoigtType )
    DoubleDot_Product4 = SUM( T*obj )

    CALL T%Deallocate( )

 END FUNCTION DoubleDot_Product4
```

Type-5

```fortran
 REAL( DFP ) FUNCTION DoubleDot_Product5( A, B, VoigtType )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: A
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: B
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType

    TYPE( Rank2Tensor_ ) :: T
    T = Rank2Tensor( B, VoigtType )
    DoubleDot_Product5 = SUM( T * A )
    CALL T%Deallocate( )
 END FUNCTION DoubleDot_Product5
```

Type-6

```fortran
 REAL( DFP ) FUNCTION DoubleDot_Product6( A, VoigtType_A, B, VoigtType_B )
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: A, B
    CHARACTER( LEN = * ), INTENT( IN ) :: VoigtType_A, VoigtType_B
    TYPE( Rank2Tensor_ ) :: T1, T2
    T1 = Rank2Tensor( A, VoigtType_A )
    T2 = Rank2Tensor( B, VoigtType_B )
    DoubleDot_Product6 = SUM( T1 * T2 )
    CALL T1%Deallocate( )
    CALL T2%Deallocate( )
 END FUNCTION DoubleDot_Product6
```

We have also defined two operators called `.DoubleDot.` and `.Contraction.`

Use of `.DoubleDot.` operator.

- `obj .DoubleDot. obj` returns scalar
- `obj .DoubleDot. Mat` returns scalar
- `Mat .DoubleDot. obj` returns scalar
- `Mat .DoubleDot. Mat` returns scalar

Use of `.Contraction.` operator.

- `obj .Contraction. obj` returns scalar
- `obj .Contraction. Mat` returns scalar
- `Mat .Contraction. obj` returns scalar
- `Mat .Contraction. Mat` returns scalar
- `Mat .Contraction. Mat` returns scalar
- `Mat .Contraction. Vec` returns vector
- `Mat .Contraction. Vec` returns vector $T \cdot v$
- `Vec .Contraction. Mat` returns vector $T^T \cdot v$

### Invariant I1

$$I_1 = Trace( T )$$

There are methods as well as module-function for this.

Method

```fortran
obj%Invariant_I1( )
```

Module-function

```fortran
Invariant_I1( obj )
Invariant_I1( Mat )
```

### Invariant I2

$$I_2 = \frac{1}{2} \Big [ Trace^2( T ) - Trace( T^2 ) \Big ]$$

There are methods as well as module-function for this.

Method

```fortran
obj%Invariant_I2( )
```

Module-function

```fortran
Invariant_I2( obj )
Invariant_I2( Mat )
```

### Invariant I3

$$I_3 = \det{T}$$

There are methods as well as module-function for this.

Method

```fortran
obj%Invaria3t_I2( )
```

Module-function

```fortran
Invariant_I3( obj )
Invariant_I3( Mat )
```

### Invariant J2

$$J_2 = \frac{1}{2} dev(T):dev(T)$$

Method

```fortran
 REAL( DFP ) FUNCTION  m_Invariant_J2( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), ALLOCATABLE :: S( :, : )
    IF( isDeviatoric( obj ) ) THEN
         m_Invariant_J2 = 0.5_DFP * ( obj .Contraction. obj )
    ELSE
        S = DeviatoricPart( obj )
        m_Invariant_J2 = 0.5_DFP * ( S .Contraction. S )
    END IF
    IF( ALLOCATED( S ) ) DEALLOCATE( S )
 END FUNCTION  m_Invariant_J2
```

Module Function

```fortran
 REAL( DFP ) FUNCTION  f_Invariant_J2( Mat )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), ALLOCATABLE :: S( :, : )
    IF( isDeviatoric( Mat ) ) THEN
         f_Invariant_J2 = 0.5_DFP * ( Mat .Contraction. Mat )
    ELSE
        S = DeviatoricPart( Mat )
        f_Invariant_J2 = 0.5_DFP * ( S .Contraction. S )
    END IF
    IF( ALLOCATED( S ) ) DEALLOCATE( S )
 END FUNCTION  f_Invariant_J2
```

### Invariant J3

$$J_3 = \det( dev( T ) )$$

Method

```fortran
 REAL( DFP ) FUNCTION  m_Invariant_J3( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), ALLOCATABLE :: S( :, : )
    IF( isDeviatoric( obj ) ) THEN
         m_Invariant_J3 = Invariant_I3( obj )
    ELSE
        S = DeviatoricPart( obj )
        m_Invariant_J3 =  Invariant_I3( S )
    END IF
    IF( ALLOCATED( S ) ) DEALLOCATE( S )
 END FUNCTION  m_Invariant_J3
```

Module Function

```fortran
 REAL( DFP ) FUNCTION  f_Invariant_J3( Mat )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), ALLOCATABLE :: S( :, : )
    IF( isDeviatoric( Mat ) ) THEN
         f_Invariant_J3 = Invariant_I3( Mat )
    ELSE
        S = DeviatoricPart( Mat )
        f_Invariant_J3 =  Invariant_I3( S )
    END IF
    IF( ALLOCATED( S ) ) DEALLOCATE( S )
 END FUNCTION  f_Invariant_J3
```

### LodeAngle

Type-1

```fortran
 REAL( DFP ) f_LodeAngle_1( J2, J3, LodeAngleType )
    REAL( DFP ), INTENT( IN ) :: J2, J3
    CHARACTER( LEN = * ), INTENT( IN ) :: LodeAngleType
    REAL( DFP ) :: Dummy

    IF( J2 .EQ. 0.0_DFP ) THEN
        f_LodeAngle_1 = 0.0_DFP
        RETURN
    END IF

    Dummy = 1.5_DFP * SQRT( 3.0_DFP ) * J3 / J2 / SQRT( J2 )

    IF( Dummy .GE. 1.0_DFP ) Dummy = 1.0_DFP
    IF( Dummy .LE. -1.0_DFP ) Dummy = -1.0_DFP

    SELECT CASE( TRIM( LodeAngleType ) )
        CASE( "SIN", "SINE", "Sin", "Sine", "sine", "sin" )
            f_LodeAngle_1 = ASIN( -Dummy ) / 3.0_DFP
        CASE DEFAULT
            f_LodeAngle_1 = ACOS( Dummy ) / 3.0_DFP
    END SELECT
 END FUNCTION f_LodeAngle_1

```

Type-2

```fortran
 REAL( DFP ) m_LodeAngle( obj, LodeAngleType )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    CHARACTER( LEN = * ), INTENT( IN ) :: LodeAngleType

    REAL( DFP ) :: J2, J3
    J2 = Invariant_J2( obj )
    J3 = Invariant_J3( obj )
    m_LodeAngle = f_LodeAngle_1( J2, J3, LodeAngleType )
 END FUNCTION m_LodeAngle
```

Type-3

```fortran
 REAL( DFP ) f_LodeAngle_2( Mat, LodeAngleType )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    CHARACTER( LEN = * ), INTENT( IN ) :: LodeAngleType

    REAL( DFP ) :: J2, J3
    J2 = Invariant_J2( Mat )
    J3 = Invariant_J3( Mat )
    f_LodeAngle_2 = f_LodeAngle_1( J2, J3, LodeAngleType )
 END FUNCTION f_LodeAngle_2
```

## PullBack

Type-1

```fortran
 FUNCTION f_Rank2PullBack_1( T, F, indx1, indx2 )
    USE Utility, ONLY: det, INV
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) ::  T, F
    REAL( DFP ), DIMENSION( SIZE( T, 1 ), SIZE( T, 2 ) ) ::  f_Rank2PullBack_1
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2
```

Description

To be added later. See page-123 of Hashiguchi and Yamakawa, 2014.

Type-2

```fortran
 FUNCTION f_Rank2PullBack_2( T, obj, indx1, indx2 )
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) ::  T
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3, 3 ) ::  f_Rank2PullBack_2
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2

    REAL( DFP ), ALLOCATABLE :: F( :, :)

    F = obj

    f_Rank2PullBack_2 = f_Rank2PullBack_1( T, F, indx1, indx2 )

    DEALLOCATE( F )

 END FUNCTION f_Rank2PullBack_2
```

Type-3

```fortran
 FUNCTION m_Rank2PullBack_1( obj, F, indx1, indx2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) ::  F
    REAL( DFP ), DIMENSION( 3, 3 ) ::  m_Rank2PullBack_1
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2

    REAL( DFP ), ALLOCATABLE :: T( :, : )
    T = obj
    m_Rank2PullBack_1 = f_Rank2PullBack_1( T, F, indx1, indx2 )
 END FUNCTION m_Rank2PullBack_1
```

Description

To be added later. See page-123 of Hashiguchi and Yamakawa, 2014.

Type-4

```fortran
 FUNCTION m_Rank2PullBack_2( obj, obj2, indx1, indx2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj, obj2
    REAL( DFP ), DIMENSION( 3, 3 ) ::  m_Rank2PullBack_2
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2

    REAL( DFP ), ALLOCATABLE :: T( :, : ), F
    T = obj
    F = obj2
    m_Rank2PullBack_2 = f_Rank2PullBack_1( T, F, indx1, indx2 )
 END FUNCTION m_Rank2PullBack_2
```

Description

To be added later. See page-123 of Hashiguchi and Yamakawa, 2014.

Type-5

```fortran
 FUNCTION f_VecPullBack_1( Vec, F, indx1 )
    USE Utility, ONLY: det, INV
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) ::  Vec
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) ::  F
    REAL( DFP ), DIMENSION( SIZE( Vec ) ) ::  f_VecPullBack_1
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1
```

Type-6

```fortran
 FUNCTION f_VecPullBack_2( Vec, obj, indx1 )
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) ::  Vec
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3 ) ::  f_VecPullBack_2
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1

    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: F

    F = obj
    f_VecPullBack_2 = f_VecPullBack_1( Vec, F, indx1 )
    DEALLOCATE( F )

 END FUNCTION f_VecPullBack_2
```

## PushForward

Type-1

```fortran
 FUNCTION f_Rank2PushForward_1( T, F, indx1, indx2 )
    USE Utility, ONLY: det, INV
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) ::  T, F
    REAL( DFP ), DIMENSION( SIZE( T, 1 ), SIZE( T, 2 ) ) ::  f_Rank2PushForward_1
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2
```

Description

To be added later. See page-123 of Hashiguchi and Yamakawa, 2014.

Type-2

```fortran
 FUNCTION f_Rank2PushForward_2( T, obj, indx1, indx2 )
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) ::  T
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3, 3 ) ::  f_Rank2PushForward_2
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2

    REAL( DFP ), ALLOCATABLE :: F( :, :)

    F = obj

    f_Rank2PushForward_2 = f_Rank2PushForward_1( T, F, indx1, indx2 )

    DEALLOCATE( F )

 END FUNCTION f_Rank2PushForward_2
```

Type-3

```fortran
 FUNCTION m_Rank2PushForward_1( obj, F, indx1, indx2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3, 3 ), INTENT( IN ) ::  F
    REAL( DFP ), DIMENSION( 3, 3 ) ::  m_Rank2PushForward_1
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2

    REAL( DFP ), ALLOCATABLE :: T( :, : )
    T = obj
    m_Rank2PushForward_1 = f_Rank2PushForward_1( T, F, indx1, indx2 )
 END FUNCTION m_Rank2PushForward_1
```

Description

To be added later. See page-123 of Hashiguchi and Yamakawa, 2014.

Type-4

```fortran
 FUNCTION m_Rank2PushForward_2( obj, obj2, indx1, indx2 )
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj, obj2
    REAL( DFP ), DIMENSION( 3, 3 ) ::  m_Rank2PushForward_2
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1, indx2

    REAL( DFP ), ALLOCATABLE :: T( :, : ), F
    T = obj
    F = obj2
    m_Rank2PushForward_2 = f_Rank2PushForward_1( T, F, indx1, indx2 )
 END FUNCTION m_Rank2PushForward_2
```

Description

To be added later. See page-123 of Hashiguchi and Yamakawa, 2014.

Type-5

```fortran
 FUNCTION f_VecPushForward_1( Vec, F, indx1 )
    USE Utility, ONLY: det, INV
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) ::  Vec
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) ::  F
    REAL( DFP ), DIMENSION( SIZE( Vec ) ) ::  f_VecPushForward_1
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1
```

Type-6

```fortran
 FUNCTION f_VecPushForward_2( Vec, obj, indx1 )
    REAL( DFP ), DIMENSION( 3 ), INTENT( IN ) ::  Vec
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3 ) ::  f_VecPushForward_2
    CHARACTER( LEN = * ), INTENT( IN ) :: indx1

    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: F

    F = obj
    f_VecPushForward_2 = f_VecPushForward_1( Vec, F, indx1 )
    DEALLOCATE( F )

 END FUNCTION f_VecPushForward_2
```

## Spectral Decomposition

### Eigens

Type-1

```fortran
 SUBROUTINE f_Eigens( Mat, EigenVectors, EigenValues )
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  Eigen values are computed using DGEEV( ) subroutine of
!           lapack libarary.
!       2.  EigenValues( :, 2 ) has two columns, the first column denotes
!           the real value of eigen value and second column denotes the
!           imaginary/complex value of eigenvalue. The conjugate values
!           are put next to each other. With positive imaginary value
!           put first.
!       3.  If j-th eigen value is imaginary then j-th and j+1 th Eigenvectors
!           are given by
!           v(j) = EigenVectors( :, j ) + i * EigenVectors( :, j +1 )
!           v(j+1) = EigenVectors( :, j ) - i * EigenVectors( :, j +1 )
!
!       4.  DGEEV function from lapack library has been used.
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), ALLOCATABLE, INTENT( OUT ) ::  EigenValues( :, : ), &
                                                EigenVectors( :, : )
```

Type-2

```fortran
 SUBROUTINE m_Eigens( obj, EigenVectors, EigenValues )
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  Eigen values are computed using DGEEV( ) subroutine of
!           lapack libarary.
!       2.  EigenValues( :, 2 ) has two columns, the first column denotes
!           the real value of eigen value and second column denotes the
!           imaginary/complex value of eigenvalue. The conjugate values
!           are put next to each other. With positive imaginary value
!           put first.
!       3.  If j-th eigen value is imaginary then j-th and j+1 th Eigenvectors
!           are given by
!           v(j) = EigenVectors( :, j ) + i * EigenVectors( :, j +1 )
!           v(j+1) = EigenVectors( :, j ) - i * EigenVectors( :, j +1 )
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), ALLOCATABLE, INTENT( OUT ) ::  EigenValues( :, : ), &
                                                EigenVectors( :, : )
```

Example

```fortran
CALL Tensor_Eigens( Mat, EigenVectors, EigenValues )
CALL Tensor_Eigens( obj, EigenVectors, EigenValues )
CALL obj%Eigens( EigenVectors, EigenValues )
```

### Principal Value

Type-1

```fortran
REAL( DFP ) FUNCTION f_PrincipalValue_1( Mat )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), ALLOCATABLE :: EigenVectors( :, : ), EigenValues( :, : )
    CALL f_Eigens( Mat, EigenVectors, EigenValues )
    f_PrincipalValue_1 = MAXVAL( EigenValues( :, 1 ) )
    DEALLOCATE( EigenValues, EigenVectors )
END FUNCTION f_PrincipalValue_1
```

Description

Returns the max( Real( eigenvalue ) )

Type-2

```fortran
REAL( DFP ) FUNCTION m_PrincipalValue_1( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), ALLOCATABLE :: EigenVectors( :, : ), EigenValues( :, : )
    CALL m_Eigens( obj, EigenVectors, EigenValues )
    m_PrincipalValue_1 = MAXVAL( Eigenvalues( :, 1 ) )
    DEALLOCATE( EigenValues, EigenVectors )
END FUNCTION m_PrincipalValue_1
```

Exam

### Spectral Radius

Type-1

```fortran
REAL( DFP ) FUNCTION f_SpectralRadius_1( Mat )
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat

    REAL( DFP ), ALLOCATABLE :: EigenVectors( :, : ), EigenValues( :, : )
    COMPLEX( DFP ), ALLOCATABLE :: Lambda( :, : )
    INTEGER( I4B ) :: N

    CALL f_Eigens( Mat, EigenVectors, EigenValues )
    f_SpectralRadius_1 = MAXVAL( EigenValues( :, 1 ) )
    N = SIZE( Eigenvalues, 1 )
    ALLOCATE( Lambda( N ) )
    Lambda( 1 : N ) = CMPLX( EigenValues( 1 : N, 1 ), EigenValues( 1 : N, 2 ) )
    EigenValues = MAXVAL( ABS( Lambda ) )
    DEALLOCATE( EigenValues, EigenVectors, Lambda )

END FUNCTION f_SpectralRadius_1
```

Description

Returns the max( Real( eigenvalue ) )

Type-2

```fortran
REAL( DFP ) FUNCTION m_SpectralRadius_1( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj

    REAL( DFP ), ALLOCATABLE :: Mat( :, : )
    Mat = obj
    m_SpectralRadius_1 = f_SpectralRadius_1( Mat )
    DEALLOCATE( Mat )
END FUNCTION m_SpectralRadius_1
```

Description

Returns the max( Real( eigenvalue ) )

## Polar Decomposition

Type-1

```fortran
 SUBROUTINE f_getPolarDecomp_1( Mat, R, H, PDType )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1. 	Ref: Higham and Noferini, 2015 Algorithm 3.1 for NSD = 3
!       2.	PDType = "Right", "U", "Left", "V", "RU", "VR"
!		3.	Mat = RU = VR, Therefore H denotes either U or V
!		4.	RU is called "Right" polar decomposition and VR is called left
!			polar decomposition
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE LinearAlgebra, ONLY: JacobiMethod
    USE Utility, ONLY: IMAXLOC, INV

	! Define intent of dummy variables
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ), INTENT( OUT ) :: R
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ), INTENT( OUT ) :: H
    CHARACTER( LEN = * ), INTENT( IN ) :: PDType
```

Type-2

```fortran
 SUBROUTINE m_getPolarDecomp_1( obj, R, H, PDType )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1. 	Ref: Higham and Noferini, 2015 Algorithm 3.1 for NSD = 3
!       2.	PDType = "Right", "U", "Left", "V", "RU", "VR"
!		3.	Mat = RU = VR, Therefore H denotes either U or V
!		4.	RU is called "Right" polar decomposition and VR is called left
!			polar decomposition
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

	! Define intent of dummy variables
    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ), INTENT( OUT ) :: R
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ), INTENT( OUT ) :: H
    CHARACTER( LEN = * ), INTENT( IN ) :: PDType
```

### Rotation Part

Type-1

```fortran
 FUNCTION f_getRotationPart( Mat )
    USE LinearAlgebra, ONLY: JacobiMethod
    USE Utility, ONLY: IMAXLOC, INV
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( SIZE( Mat, 1 ), SIZE( Mat, 2 ) ) :: f_getRotationPart
```

Type-2

```fortran
 FUNCTION m_getRotationPart( obj )
    CLASS( Rank2Tensor_ ), INTENT( IN ) :: obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: m_getRotationPart
	REAL( DFP ), ALLOCATABLE :: Mat( :, : )
	Mat = obj
	m_getRotationPart = f_getRotationPart( Mat )
	DEALLOCATE( Mat )
 END FUNCTION m_getRotationPart
```

