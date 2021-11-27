# Space Time Diffusion Matrix Class

## ToDo

## Structure

## Description

`STDiffusionMatrix_` object is child of `STElemShapeData_` class. It can be initiated using following commands.

## Getting Started

### Making The Object

We can make the object using the `Initiate` method.

```fortran 
CALL Obj % Initiate( NIPS = NIPS, NIPT = NIPT)
CALL Obj % InitiateMatrix( row= row, col = col)
CALL Obj % InitiateMatrix( I1 = I1, I2 = I2, I3 = I3, I4= I4) 
```

We can also use the `STDiffusionMatrix()` function

```fortran
STElemSD = STDiffusionMatrix( )
STElemSD = STDiffusionMatrix( row = row, col = col, NIPS = NIPS, NIPT = NIPT )
STElemSD = STDiffusionMatrix( I1, I2, I3, I4, I5, NIPS, NIPT)
```

We can also use the `STDiffusionMatrix_Pointer()` function

```fortran
STElemSD => STDiffusionMatrix_Pointer( )
STElemSD => STDiffusionMatrix_Pointer( row = row, col = col, NIPS = NIPS, NIPT = NIPT )
STElemSD => STDiffusionMatrix_Pointer( I1, I2, I3, I4, I5, NIPS, NIPT)
```

### Getting The Diffusion Matrix

To compute the following matrix

$$\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial \delta u\,}}{{\partial {\bf{x}}}} \cdot \frac{{\partial u}}{{\partial {\bf{x}}}}d\Omega dt} }  = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial {x_i}}}\,\frac{{\partial {N^J}{T_b}}}{{\partial {x_i}}}d\Omega dt} } } \right]\,{}^b{u_J}$$

we can use the following command

```fortran
CALL Obj % getDiffusionMatrix( )
```

To compute the following matrix

$$
\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial \delta {u_i}\,}}{{\partial {\bf{x}}}} \cdot \frac{{\partial {u_i}}}{{\partial {\bf{x}}}}d\Omega dt} }  = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial {x_p}}}\,\frac{{\partial {N^J}{T_b}}}{{\partial {x_p}}}d\Omega dt} } } \right]\,{}^b{u_{iJ}}
$$

we can call following fortran command.


```fortran
CALL Obj % getDiffusionMatrix( nCopy )
```

To compute the following matrix

$$
\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial \delta u\,}}{{\partial {x_i}}}{K_{ij}}\frac{{\partial u}}{{\partial {x_j}}}d\Omega dt} }  = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial {x_i}}}\,{K_{ij}}\frac{{\partial {N^J}{T_b}}}{{\partial {x_j}}}d\Omega dt} } } \right]\,{}^b{u_J}
$$

We can call following command.

```fortran
CALL Obj % getDiffusionMatrix( K )
```

To compute the following matrix

$$
\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial \delta {u_i}\,}}{{\partial {x_p}}}{K_{pq}}\frac{{\partial {u_i}}}{{\partial {x_q}}}d\Omega dt} }  = {}^a\delta {u_{iI}}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{\partial {x_p}}}\,{K_{pq}}\frac{{\partial {N^J}{T_b}}}{{\partial {x_q}}}d\Omega dt} } } \right]\,{}^b{u_{iJ}}
$$

we can call following fortran command.

```fortran
CALL Obj % getDiffusionMatrix( K, nCopy )
```

In the above two calls `K` can be rank-4 `K(:,:,:,:)`, rank-3 `K(:,:,:)`, rank-2 `K(:,:)`. If `K` is varying in both space and time then it is given by space-time matrix. If `K` is changing only with the space then it is given by the Rank-3, and if `K` is constant in both space and time then it is given by the Rank-2 matrix. `K` is defined at the integration points.

To compute the following matrix

$$
\[\int_{{I_n}}^{} {\int_\Omega ^{} {{{\bf{c}}_1} \cdot \frac{{\partial \delta u}}{{\partial {\bf{x}}}}{{\bf{c}}_2} \cdot \frac{{\partial u}}{{\partial {\bf{x}}}}d\Omega dt} }  = {}^a\delta {u_I}\left[ {\int_{{I_n}}^{} {\int_\Omega ^{} {{{\bf{c}}_1} \cdot \frac{{\partial {N^I}{T_a}}}{{\partial {\bf{x}}}}{{\bf{c}}_2} \cdot \frac{{\partial {N^J}{T_b}}}{{\partial {\bf{x}}}}d\Omega dt} } } \right]{}^b{u_J}\]
$$

```fortran
CALL Obj % getDiffusionMatrix( c1, c2, c1Type, c2Type )
CALL Obj % getDiffusionMatrix( c1, c2, c1Type, c2Type, nCopy )
```

In the above call `c1, c2` can be rank-3 `(:,:,:)`, rank-2 `(:,:)`, rank-1 `(:)`. `c1Type, c2Type` can be `NodalValues` or `QuadPoints`. `c1, c2` denotes the convective velocity.

- If convective velocity is changing in space and time then it must be given by Rank-3 matrix.
- If convective velocity is changing in only space then it must be given by Rank-2 matrix.
- If convective velocity is constant in both space and time then it must be given by Rank-1 matrix.



```fortran
CALL Obj % getDiffusionMatrix( K, Term1, Term2 )
```

In the above call `K` can be rank-4 `K(:,:,:,:)`, rank-3 `K(:,:,:)`, rank-2 `K(:,:)`. `Term1` and `Term2` can be `dx, dy, dz`.




























## Theory

Consider the following _scalar_ term present in the pde 

$${\nabla}^2 u + \cdots $$

then we may need to compute the following matrices.

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

Note here, $u \in R$.

> These tasks are performed by following methods; `getDiffusionMatrix_1()`, `getDiffusionMatrix_2()`

Now consider the following terms in a pde. 

$${\nabla} \cdot \Big( -{}^{2}\mathbf{K}{\nabla}u \Big) + \cdots $$
or
$$\frac{\partial}{\partial x_i} \cdot \Big(  -{}^{2}K_{ij} \frac{\partial u}{\partial x_j} \Big) + \cdots $$

> These tasks are performed by following methods; `getDiffusionMatrix_3()`, `getDiffusionMatrix_4()`, `getDiffusionMatrix_5()`, `getDiffusionMatrix_6()`, `getDiffusionMatrix_7()`, and `getDiffusionMatrix_8()`



## Methods

### getDiffusionMatrix_1()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_1( Obj )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable. This is the simplest form possible. No arguments are required.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
CALL STElemSD % getDiffusionMatrix( )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_1( )"
CALL STElemSD % DisplayMatrix4
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_1( )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444
```
> Note that in this case integrad is quadratic in time, therefore we need atleast 2 integration points in the time. This condition may change when the mesh is moving. Note that the row sum and column sum is zero as expected.

### getDiffusionMatrix_2()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_2( Obj, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable. This is the simplest form possible. No arguments are required.
- `nCopy` is an integer, which decides how many copies need to be placed on the diagonal.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( nCopy = 2 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
CALL STElemSD % getDiffusionMatrix( nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_2( nCopy = 2 )"
CALL STElemSD % DisplayMatrix4
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_2( nCopy = 2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111     -0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000       0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000       0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000       0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111     -0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444     -0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111      0.4444444
```

### getDiffusionMatrix_3()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_3( Obj, K )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ), INTENT( IN ) :: K
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `K(:,:,:,:)` is a four dimensional array. The shape of `K` should be `(NSD, NSD, NIPS, NIPT)`. The third index denotes the spatial-integration points. The fourth index denotes the temporal integration points. In this case, `K` matrix varies in both space and time.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( K = DummyMat4 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
IF( ALLOCATED( DummyMat4 ) ) DEALLOCATE( DummyMat4 )
ALLOCATE( DummyMat4( NSD, NSD, NIPS, NIPT ) )
DummyMat4 = 0.0_DFP; DummyMat4( 1, 1, :, : ) = 1.0_DFP
DummyMat4( 2, 2, :, : ) = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( K = DummyMat4 )
cALL Check_Error( " " , " " )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_3( K = DummyMat4 )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_3( K = DummyMat4 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444
```

### getDiffusionMatrix_4()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_4( Obj, K )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: K
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `K(:,:,:)` is a three dimensional array. The shape of `K` should be `(NSD, NSD, NIPS)`. The third index denotes the spatial-integration points. In this case, `K` matrix varies in only in space and remains constant in time.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( K = DummyMat3 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NSD, NIPS ) )
DummyMat3 = 0.0_DFP; DummyMat3( 1, 1, : ) = 1.0_DFP
DummyMat3( 2, 2, : ) = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( K = DummyMat3 )
cALL Check_Error( " " , " " )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_4( K = DummyMat3 )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_4( K = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444
```

### getDiffusionMatrix_5()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_5( Obj, K )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: K
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `K(:,:)` is a two dimensional array. The shape of `K` should be `(NSD, NSD)`. In this case, `K` matrix remains constant in both space and time.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( K = DummyMat2 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NSD ) )
DummyMat2 = 0.0_DFP; DummyMat2( 1, 1 ) = 1.0_DFP
DummyMat2( 2, 2 ) = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( K = DummyMat2 )
cALL Check_Error( " " , " " )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_5( K = DummyMat2 )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_5( K = DummyMat2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.2222222     -0.1111111      0.4444444     -0.1111111
 -0.1111111     -0.2222222     -0.1111111      0.4444444
```

### getDiffusionMatrix_6()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_6( Obj, K, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ), INTENT( IN ) :: K
    INTEGER( I4B ), INTENT( IN ) :: nCopy 
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `K(:,:,:,:)` is a four dimensional array. The shape of `K` should be `(NSD, NSD, NIPS, NIPT)`. The third index denotes the spatial-integration points. The fourth index denotes the temporal integration points. In this case, `K` matrix varies in both space and time.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( K = DummyMat4, nCopy = 2 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
IF( ALLOCATED( DummyMat4 ) ) DEALLOCATE( DummyMat4 )
ALLOCATE( DummyMat4( NSD, NSD, NIPS, NIPT ) )
DummyMat4 = 0.0_DFP; DummyMat4( 1, 1, :, :  ) = 1.0_DFP
DummyMat4( 2, 2, :, : ) = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( K = DummyMat4, nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_6( K = DummyMat4 )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_6( K = DummyMat4 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000  0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111-0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111 0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01 0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01 0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000  0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111-0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111 0.4444444
```

### getDiffusionMatrix_7()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_7( Obj, K, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: K
    INTEGER( I4B ), INTENT( IN ) :: nCopy 
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `K(:,:,:)` is a three dimensional array. The shape of `K` should be `(NSD, NSD, NIPS)`. The third index denotes the spatial-integration points. In this case, `K` matrix varies in only in space and remains constant in time.
CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( K = DummyMat3, nCopy = 2 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NSD, NIPS ) )
DummyMat3 = 0.0_DFP; DummyMat3( 1, 1, :  ) = 1.0_DFP
DummyMat3( 2, 2, : ) = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( K = DummyMat3, nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_7( K = DummyMat3 )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_7( K = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000  0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111-0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111 0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01 0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01 0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000  0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111-0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111 0.4444444
```

### getDiffusionMatrix_8()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_8( Obj, K, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: K
    INTEGER( I4B ), INTENT( IN ) :: nCopy 
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `K(:,:)` is a two dimensional array. The shape of `K` should be `(NSD, NSD)`. In this case, `K` matrix remains constant in both space and time.

```fortran
CALL STElemSD % getDiffusionMatrix( K = DummyMat2, nCopy = 2 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_I \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_i} \frac{\partial N^J T_b}{ \partial x_i} {dQ} \quad {}^{b}u_J$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NSD ) )
DummyMat2 = 0.0_DFP; DummyMat2( 1, 1 ) = 1.0_DFP
DummyMat2( 2, 2 ) = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( K = DummyMat2, nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_5( K = DummyMat2 )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_5( K = DummyMat2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000  0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111-0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111 0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01 0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01   0.000000       0.000000       0.000000  0.000000
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.2222222     -0.5555556E-01 -0.1111111-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01  0.2222222     -0.5555556E-01-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.5555556E-01  0.2222222-0.5555556E-01
   0.000000       0.000000       0.000000       0.000000     -0.5555556E-01 -0.1111111     -0.5555556E-01 0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111      0.4444444     -0.1111111     -0.2222222       0.000000       0.000000       0.000000  0.000000
 -0.2222222     -0.1111111      0.4444444     -0.1111111       0.000000       0.000000       0.000000  0.000000
 -0.1111111     -0.2222222     -0.1111111      0.4444444       0.000000       0.000000       0.000000  0.000000
   0.000000       0.000000       0.000000       0.000000      0.4444444     -0.1111111     -0.2222222-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111      0.4444444     -0.1111111-0.2222222
   0.000000       0.000000       0.000000       0.000000     -0.2222222     -0.1111111      0.4444444-0.1111111
   0.000000       0.000000       0.000000       0.000000     -0.1111111     -0.2222222     -0.1111111 0.4444444
```

### getDiffusionMatrix_9()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_9( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c1, c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c1(:,:,:)` and `c2(:,:,:)` are three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` or `C2Type` are in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` and/or `c2` denotes the space-time nodal values. In this case their shape should be `(NSD, NNS, NNT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `C1Type` or `C2Type` are in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c1` and/or `c2` denotes the values at space-time integration(quadrature) points. In this case, their shape should be `(NSD, NIPS, NIPT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat3, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat3, C1Type = 'Nodal', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Nodal' )
```

SYMBOLIC CALCULATION 
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat3,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_9( c1 = &
DummyMat3, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_9( c1 = DummyMat3, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_10()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_9( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c1, c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c1(:,:)` and `c2(:,:)` are two dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` or `C2Type` are in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` and/or `c2` denotes the spatial nodal values. In this case, their shape should be `(NSD, NNS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.
- `C1Type` or `C2Type` are in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c1` and/or `c2` denotes the values at spatial integration(quadrature) points. In this case, their shape should be `(NSD, NIPS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat2, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat2, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat2,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_10( c1 = &
DummyMat2, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_10( c1 = DummyMat2, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_11()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_9( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c1, c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c1(:)` and `c2(:)` are vectors. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` or `C2Type` are string type and has no effect on the functionality of the method. they are inlcuded here to maintain the subroutine. 

CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyVec, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyVec, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD) )
DummyVec = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyVec,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_11( c1 = &
DummyVec, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_11( c1 = DummyVec, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_12()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_12( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c1
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c1(:,:,:)` is three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` denotes the space-time nodal values. In this case their shape should be `(NSD, NNS, NNT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `C1Type` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c1` denotes the values at space-time integration(quadrature) points. In this case, their shape should be `(NSD, NIPS, NIPT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `c2(:,:)` are two dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C2Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c2` denotes the spatial nodal values. In this case, their shape should be `(NSD, NNS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.
- `C2Type` are in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c2` denotes the values at spatial integration(quadrature) points. In this case, their shape should be `(NSD, NIPS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.




CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat2, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat2, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP
    
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyMat2,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_12( c1 = &
DummyMat3, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_12( c1 = DummyMat3, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_13()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_12( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c2
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c1
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c2(:,:,:)` is three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C2Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` denotes the space-time nodal values. In this case their shape should be `(NSD, NNS, NNT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `C2Type` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c2` denotes the values at space-time integration(quadrature) points. In this case, their shape should be `(NSD, NIPS, NIPT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `c1(:,:)` are two dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` denotes the spatial nodal values. In this case, their shape should be `(NSD, NNS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.
- `C1Type` are in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c1` denotes the values at spatial integration(quadrature) points. In this case, their shape should be `(NSD, NIPS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.




CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat3, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat3, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP
    
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyMat3,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_12( c1 = &
DummyMat2, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_13( c1 = DummyMat2, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_14()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_12( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c1
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c1(:,:,:)` is three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` denotes the space-time nodal values. In this case their shape should be `(NSD, NNS, NNT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `C1Type` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c1` denotes the values at space-time integration(quadrature) points. In this case, their shape should be `(NSD, NIPS, NIPT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `c2(:)` is a vector. It denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C2Type` is string type and has no effect on the functionality of the method. they are inlcuded here to maintain the subroutine. 



CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyVec, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyVec, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP
    
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyMat3, c2 = DummyVec,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_14( c1 = &
DummyMat3, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_14( c1 = DummyMat3, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_15()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_12( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c2
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c1
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c2(:,:,:)` is three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C2Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c2` denotes the space-time nodal values. In this case their shape should be `(NSD, NNS, NNT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `C2Type` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c2` denotes the values at space-time integration(quadrature) points. In this case, their shape should be `(NSD, NIPS, NIPT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `c1(:)` is a vector. It denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` is string type and has no effect on the functionality of the method. they are inlcuded here to maintain the subroutine. 



CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat3, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat3, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP
    
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat3,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_15( c1 = &
DummyVec, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_15( c1 = DummyVec, c2 = DummyMat3, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_16()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_12( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c1
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c1(:,:)` is three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c1` denotes the space nodal values. In this case their shape should be `(NSD, NNS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.
- `C1Type` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c1` denotes the values at space integration(quadrature) points. In this case, their shape should be `(NSD, NIPS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.
- `c2(:)` is a vector. It denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C2Type` is string type and has no effect on the functionality of the method. they are inlcuded here to maintain the subroutine. 


CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyVec, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyVec, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP
    
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyMat2, c2 = DummyVec,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_16( c1 = &
DummyMat2, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_16( c1 = DummyMat2, c2 = DummyVec, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_17()

INTERFACE 

```fortran
 SUBROUTINE getDiffusionMatrix_12( Obj, c1, c2, c1Type, c2Type )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c2
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c1
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
```

DESCRIPTION

- This methods computes the diffusion matrix for a scalar variable.
- `c2(:,:)` is three dimensional arrays. They denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C2Type` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `c2` denotes the space nodal values. In this case their shape should be `(NSD, NNS, NNT)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node.
- `C2Type` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Qaud Points]` then `c2` denotes the values at space integration(quadrature) points. In this case, their shape should be `(NSD, NIPS)`. The first index denotes the spatial-coordinate. The second index denotes the spatial-node. Third index denotes the temporal nodes.
- `c1(:)` is a vector. It denotes the vector over which projection of $\frac{\partial N^I T_a}{\partial x_k}$ will be taken. 
- `C1Type` is string type and has no effect on the functionality of the method. they are inlcuded here to maintain the subroutine. 



CODE SNIPPET

```fortran
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat2, C1Type = 'Nodal', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Nodal' )
CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat2, C1Type = 'Nodal', C2Type = 'Quad' )
```

SYMBOLIC CALCULATION 

?

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP
    
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getDiffusionMatrix( c1 = DummyVec, c2 = DummyMat2,&
C1Type = 'Quad', C2Type = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") "CALL STElemSD % getDiffusionMatrix_17( c1 = &
DummyVec, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )"
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getDiffusionMatrix_17( c1 = DummyVec, c2 = DummyMat2, C1Type = 'Quad', C2Type = 'Quad' )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111

Mat4( :, :,  1, 2 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 1 )

  0.3888889     -0.5555556E-01 -0.2777778     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01
 -0.2777778     -0.5555556E-01  0.3888889     -0.5555556E-01
 -0.5555556E-01  0.5555556E-01 -0.5555556E-01  0.5555556E-01

Mat4( :, :,  2, 2 )

  0.7777778     -0.1111111     -0.5555556     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
 -0.5555556     -0.1111111      0.7777778     -0.1111111
 -0.1111111      0.1111111     -0.1111111      0.1111111
```

### getDiffusionMatrix_18()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_18( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c1, c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_19()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_19( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c1, c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_20()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_20( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c1, c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_21()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_21( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c1
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_22()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_22( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c2
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c1
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_23()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_23( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c1
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_24()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_24( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: c2
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c1
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_25()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_25( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c1
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c2
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_26()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_26( Obj, c1, c2, c1Type, c2Type, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: c2
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: c1
    CHARACTER( LEN = * ), INTENT( IN ) :: c1Type, c2Type
    INTEGER( I4B ), INTENT( IN ) :: nCopy
```

### getDiffusionMatrix_27()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_27( Obj, K, Term1, Term2 )

   USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ), INTENT( IN ) :: K
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

### getDiffusionMatrix_28()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_28( Obj, K, Term1, Term2 )

   USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: K
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```

### getDiffusionMatrix_29()

INTERFACE

```fortran
 SUBROUTINE getDiffusionMatrix_29( Obj, K, Term1, Term2 )

   USE Utility, ONLY : OUTERPROD

    CLASS( STDiffusionMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: K
    CHARACTER( LEN = * ), INTENT( IN ) :: Term1, Term2
```


