# Convective Matrix Class

## Structure

## Constructor methods

There are two methods to initiate `ConvectiveMatrix_` object.

```fortran
ElemSD => ConvectiveMatrix()
```

This will just allocate the pointer to the convective matrix object. This will not allocate any arrays in the field of convective matrix object. There is another way to do this where we can descibe the sizes of various array.

```fortran
ElemSD => ConvectiveMatrix( row = row, col = col, NIP = NIP)
```

We can also use the `initiate` method which is inherited from the `ElemShapeData_` object. This will allocate the shapedata object at given number of integration points. The sentence is given below.

```fortran
CALL ElemSD % initiate( NIP = NIP )
```

## Theory

Consider the following terms in PDE.

_scalar unknown_

$$\frac{\partial u}{\partial t} + c_k \frac{\partial u}{\partial x_k} + \cdots$$

_vector unknown_

$$\frac{\partial u_i}{\partial t} + c_k \frac{\partial u_i}{\partial x_k} + \cdots$$

In this case we want to compute the following finite element matrices

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

> These tasks are performed using the following methods; `getConvectiveMatrix_1()`, `getConvectiveMatrix_2()`, `getConvectiveMatrix_3()`, `getConvectiveMatrix_4()`, `getConvectiveMatrix_5()`, `getConvectiveMatrix_6()`.

Now consider the following terms in PDE.

_scalar unknown_

$$\frac{\partial u}{\partial t}+\frac{\partial f(u)}{\partial x} + \frac{\partial g(u)}{\partial y} + \frac{\partial h(u)}{\partial z} + \cdots$$

_vector unknown_

$$\frac{\partial \mathbf{U}}{\partial t} + \frac{\partial \mathbf{f(U)} }{\partial x} + \frac{\partial \mathbf{g(u)}}{\partial y} + \frac{\partial \mathbf{h(u)}}{\partial z} + \cdots$$

Now we want to compute the following matrices.

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial x} d{\Omega} \quad f_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial x} N^J d{\Omega} \quad f_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial y} d{\Omega} \quad g_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad g_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial z} d{\Omega} \quad h_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial z} N^J d{\Omega} \quad h_{iJ}$$

> These tasks are performed by the following methods; `getConvectiveMatrix_7()`, `getConvectiveMatrix_8()`

Now consider the following terms in a PDE.

$$\frac{\partial \mathbf{U}}{\partial t} + [\mathbf{A_1}] \frac{\partial \mathbf{U} }{\partial x} + [\mathbf{A_2}] \frac{\partial \mathbf{U}}{\partial y} + [\mathbf{A_3}] \frac{\partial \mathbf{U}}{\partial z} + \cdots$$

where, $\mathbf{U} \in R^m$, and $[\mathbf{A_i}] \in R^{(m\times m)}$.

For this we may need to compute the following matrices.

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  [\mathbf{A_1}]_{ij} \frac{\partial N^J}{\partial x} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_1}]_{ji} \frac{\partial N^I}{\partial x} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  [\mathbf{A_2}]_{ij} \frac{\partial N^J}{\partial y} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_2}]_{ji} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I [\mathbf{A_3}]_{ij} \frac{\partial N^J}{\partial z} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_3}]_{ji} \frac{\partial N^I}{\partial z} N^J d{\Omega} \quad U_{jJ}$$

> This task is performed using the following the methods; `getConvectiveMatrix_9()`, `getConvectiveMatrix_10()`

Now consider the following terms in the pde.

$$[\mathbf{B}] \frac{\partial \mathbf{U}}{\partial t} + [\mathbf{A_1}] \frac{\partial \mathbf{U} }{\partial x} + [\mathbf{A_2}] \frac{\partial \mathbf{U}}{\partial y} + [\mathbf{A_3}] \frac{\partial \mathbf{U}}{\partial z} + \cdots$$


We may want to compute following matrices


$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I  [\mathbf{A_1}]_{kj} \frac{\partial N^J}{\partial x} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_1}]_{ki} \frac{\partial N^I}{\partial x} [\mathbf{B}]_{kj} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I  [\mathbf{A_2}]_{kj} [\mathbf{B}]_{kj} \frac{\partial N^J}{\partial y} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_2}]_{ki} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I [\mathbf{A_3}]_{kj} \frac{\partial N^J}{\partial z} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_3}]_{ki} \frac{\partial N^I}{\partial z} [\mathbf{B}]_{kj} N^J d{\Omega} \quad U_{jJ}$$

> This task is performed using the following methods; `getConvectiveMatrix_11()`, `getConvectiveMatrix_12()`


## Methods

### getConvectiveMatrix_1()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_1( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- `C(:,:)` is a two dimensional array. It represents the spatial nodal values of _convective velocity_. The shape of `C` is `(NSD, NNS)`. The first index of `C` denotes the spatial coordinates. The second index denotes the spatial nodal number. In this case, _convective velocity_ varies in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.

CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1 )
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 1, Term2 = 0 )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1 )
CALL Check_Error( "Main-Program", " CALL ElemSD % getConvectiveMatrix_1()")

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_1( C = DummyMat2, Term1 = 0, Term2 = 1 )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

 -0.6666667      0.1666667      0.3333333      0.1666667
 -0.5000000       0.000000      0.5000000       0.000000
 -0.3333333     -0.1666667      0.6666667     -0.1666667
 -0.5000000       0.000000      0.5000000       0.000000
```

```fortran
CALL ElemSD % getConvectiveMatrix_1( C = DummyMat2, Term1 = 1, &
Term2 = 0 )
CALL Check_Error( "Main-Program", " CALL ElemSD % getConvectiveMatrix_1()")

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 1, &
Term2 = 0 )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 1, Term2 = 0 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.6666667     -0.5000000     -0.3333333     -0.5000000
  0.1666667       0.000000     -0.1666667       0.000000
  0.3333333      0.5000000      0.6666667      0.5000000
  0.1666667       0.000000     -0.1666667       0.000000
```

> These matrices are transpose of each other, therefore we will consider the first one only.

### getConvectiveMatrix_2()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_2( Obj, C, Term1, Term2 )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
```

DESCRIPTION

- `C(:)` is a vector. It represents the spatial coordinates of _convective velocity_. The shape of `C` is `(NSD)`. The first index of `C` denotes the spatial coordinates. In this case, _convective velocity_ remains constant in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.

CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 0, Term2 = 1 )
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 1, Term2 = 0 )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 0, &
Term2 = 1 )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_2( C = DummyVec, Term1 = 0, Term2 = 1 )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 0, Term2 = 1 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.6666667      0.1666667      0.3333333      0.1666667
 -0.5000000       0.000000      0.5000000       0.000000
 -0.3333333     -0.1666667      0.6666667     -0.1666667
 -0.5000000       0.000000      0.5000000       0.000000
```

### getConvectiveMatrix_3()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_3( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
```

DESCRIPTION

- `C(:)` is a vector. It represents the spatial coordinates of _convective velocity_. The shape of `C` is `(NSD)`. The first index of `C` denotes the spatial coordinates. In this case, _convective velocity_ remains constant in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `nCopy` is an integer; which copies the matrix to ncopy diagonal.

CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 1, Term2 = 0, nCopy = 2 )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_3( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_3( C = DummyMat2, Term1 = 0, Term2 = 1, nCopy = 2 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.6666667      0.1666667      0.3333333      0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.5000000       0.000000      0.5000000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.3333333     -0.1666667      0.6666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.5000000       0.000000      0.5000000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.6666667      0.1666667      0.3333333      0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.5000000       0.000000      0.5000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333     -0.1666667      0.6666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.5000000       0.000000      0.5000000       0.000000
```

### getConvectiveMatrix_4()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_4( Obj, C, Term1, Term2, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
```

DESCRIPTION

- `C(:,:)` is a two dimensional array. It represents the spatial nodal values of _convective velocity_. The shape of `C` is `(NSD, NNS)`. The first index of `C` denotes the spatial coordinates. The second index denotes the spatial nodal number. In this case, _convective velocity_ varies in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `nCopy` is an integer; which copies the matrix to ncopy diagonal.

CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 0, Term2 = 1, nCopy = 2 )
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 1, Term2 = 0, nCopy = 2 )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( C = DummyVec, Term1 = 0, Term2 = 1, nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_4( C = DummyVec, Term1 = 0, Term2 = 1, nCopy = 2 )"
CALL ElemSD %  DisplayMatrix()
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_4( C = DummyVec, Term1 = 0, Term2 = 1, nCopy = 2 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.6666667      0.1666667      0.3333333      0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.5000000       0.000000      0.5000000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.3333333     -0.1666667      0.6666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.5000000       0.000000      0.5000000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.6666667      0.1666667      0.3333333      0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.5000000       0.000000      0.5000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333     -0.1666667      0.6666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.5000000       0.000000      0.5000000       0.000000
```

### getConvectiveMatrix_5()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_5( Obj, C, Term1, Term2, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- `C(:,:)` is a two dimensional array.
    - If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, SpaceNodalValues, Space Nodal Values]` then `C` denotes the spatial nodal values. In this case, its shape should be `(NSD, NNS)`. The first index of `C` denotes the spatial coordinates. The second index denotes the spatial-node number.
    - If `CType` is in the set `[Integration IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of convective velocity at the spatial integration points. In this case its shape should be `(NSD, NIPS)`. The first index of `C` denotes the spatial coordinates. The second index denotes the spatial-integration points.
- In this case, _convective velocity_ varies in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.

CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, Ctype = 'Quad' )
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 1, Term2 = 0, Ctype = 'Quad' )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIP ) )
DummyMat2 = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, CType = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_5( C = DummyMat2, Term1 = 0, Term2 = 1, CType = 'Quad' )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_5( C = DummyMat2, Term1 = 0, Term2 = 1, CType = 'Quad' )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.6666667      0.1666667      0.3333333      0.1666667
 -0.5000000       0.000000      0.5000000       0.000000
 -0.3333333     -0.1666667      0.6666667     -0.1666667
 -0.5000000       0.000000      0.5000000       0.000000
```

### getConvectiveMatrix_6()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_5( Obj, C, Term1, Term2, CType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION

- `C(:,:)` is a two dimensional array.
    - If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, SpaceNodalValues, Space Nodal Values]` then `C` denotes the spatial nodal values. In this case, its shape should be `(NSD, NNS)`. The first index of `C` denotes the spatial coordinates. The second index denotes the spatial-node number.
    - If `CType` is in the set `[Integration IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C` denotes the value of convective velocity at the spatial integration points. In this case its shape should be `(NSD, NIPS)`. The first index of `C` denotes the spatial coordinates. The second index denotes the spatial-integration points.
- In this case, _convective velocity_ varies in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `nCopy` is an integer; which copies the matrix to ncopy diagonal.

CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, Ctype = 'Quad', nCopy = 2 )
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 1, Term2 = 0, Ctype = 'Quad', nCopy = 2 )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} N^I c_k \frac{\partial N^J}{\partial x_k} d{\Omega} \quad u_{iJ}$$

$${}^{2}M(I,J) = \partial u_{iI} \int_{\Omega} c_k \frac{\partial N^I}{\partial x_k} N^J d{\Omega} \quad u_{iJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIP ) )
DummyMat2 = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( C = DummyMat2, Term1 = 0, Term2 = 1, CType = 'Quad', nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_5( C = DummyMat2, &
Term1 = 0, Term2 = 1, CType = 'Quad', nCopy = 2 )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_5( C = DummyMat2, Term1 = 0, Term2 = 1, CType = 'Quad', nCopy = 2 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.6666667      0.1666667      0.3333333      0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.5000000       0.000000      0.5000000       0.000000       0.000000       0.000000       0.000000       0.000000
 -0.3333333     -0.1666667      0.6666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.5000000       0.000000      0.5000000       0.000000       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.6666667      0.1666667      0.3333333      0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.5000000       0.000000      0.5000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333     -0.1666667      0.6666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.5000000       0.000000      0.5000000       0.000000
```

### getConvectiveMatrix_7()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_7( Obj, Term1, Term2, XType )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
```

DESCRIPTION

- In this case, _convective velocity_ varies in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `XType` is string type, it denotes the type of spatial gradient.
- If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means that the spatial gradient is with respect to the `x` coordinate.
- If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means that the spatial gradient is with respect to the `y` coordinate.
- If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means that the spatial gradient is with respect to the `z` coordinate.


CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, Xtype = 'dx')
CALL ElemSD % getConvectiveMatrix( Term1 = 1, Term2 = 0, Xtype = 'dx')
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial x} d{\Omega} \quad f_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial x} N^J d{\Omega} \quad f_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial y} d{\Omega} \quad g_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad g_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial z} d{\Omega} \quad h_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial z} N^J d{\Omega} \quad h_{iJ}$$

TESTING

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx' )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_7( Term1 = 0, Term2 = 1, XType = 'dx' )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_7( Term1 = 0, Term2 = 1, XType = 'dx' )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.3333333      0.3333333      0.1666667     -0.1666667
 -0.3333333      0.3333333      0.1666667     -0.1666667
 -0.1666667      0.1666667      0.3333333     -0.3333333
 -0.1666667      0.1666667      0.3333333     -0.3333333
```

### getConvectiveMatrix_8()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_8( Obj, Term1, Term2, XType, nCopy )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2, nCopy
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
```

DESCRIPTION

- In this case, _convective velocity_ varies in the space.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `XType` is string type, it denotes the type of spatial gradient.
- If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means that the spatial gradient is with respect to the `x` coordinate.
- If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means that the spatial gradient is with respect to the `y` coordinate.
- If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means that the spatial gradient is with respect to the `z` coordinate.
- `nCopy` is the number of copies to be placed on the diagonal.


CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, Xtype = 'dx', nCopy = 2)
CALL ElemSD % getConvectiveMatrix( Term1 = 1, Term2 = 0, Xtype = 'dx', nCopy = 2)
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial x} d{\Omega} \quad f_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial x} N^J d{\Omega} \quad f_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial y} d{\Omega} \quad g_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad g_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  \frac{\partial N^J}{\partial z} d{\Omega} \quad h_{iJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} \frac{\partial N^I}{\partial z} N^J d{\Omega} \quad h_{iJ}$$

TESTING

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', nCopy = 2 )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_8( Term1 = 0, Term2 = 1, XType = 'dx', nCopy = 2 )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_8( Term1 = 0, Term2 = 1, XType = 'dx', nCopy = 2 )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
```

### getConvectiveMatrix_9()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_9( Obj, A, Term1, Term2, XType, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ) :: A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:,:)` is a three dimensional array. The shape of `A` is `(M,M,NIPS)`. This array is defined at spatial-integration points. This array is responsible for coupling between different unknowns. In this method, `A` varies in the spatial domain.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `XType` is string type, it denotes the type of spatial gradient.
- If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means that the spatial gradient is with respect to the `x` coordinate.
- If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means that the spatial gradient is with respect to the `y` coordinate.
- If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means that the spatial gradient is with respect to the `z` coordinate.
- `MultiVar` is a logical value which has no effect on the functionality of the method; this just for letting compiler know that this method has different interface.


CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE., A = DummyMat3)
CALL ElemSD % getConvectiveMatrix( Term1 = 1, Term2 = 0, Xtype = 'dx', MultiVar = .TRUE., A = DummyMat3)
```

SYMBOLIC CALCULATION

?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( 2,2, NIP ) )
DummyMat3 = 0.0_DFP
DummyMat3( 1, 1, : ) = 1.0_DFP; DummyMat3( 2, 2, : ) = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat3, MultiVar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_9( Term1 = 0, Term2 = 1, &
XType = 'dx', A = DummyMat3, MultiVar = .TRUE. )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_9( Term1 = 0, Term2 = 1, &
XType = 'dx', A = DummyMat3, MultiVar = .TRUE. )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
```

### getConvectiveMatrix_10()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_10( Obj, A, Term1, Term2, XType, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ) :: A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:)` is a two dimensional array. The shape of `A` is `(M,M)`. This array is responsible for coupling between different unknowns. The array is constant in the spatial domain.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `XType` is string type, it denotes the type of spatial gradient.
- If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means that the spatial gradient is with respect to the `x` coordinate.
- If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means that the spatial gradient is with respect to the `y` coordinate.
- If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means that the spatial gradient is with respect to the `z` coordinate.
- `MultiVar` is a logical value which has no effect on the functionality of the method; this just for letting compiler know that this method has different interface.


CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, Xtype = 'dx', MultiVar = .TRUE., A = DummyMat2)
CALL ElemSD % getConvectiveMatrix( Term1 = 1, Term2 = 0, Xtype = 'dx', MultiVar = .TRUE., A = DummyMat2)
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  [\mathbf{A_1}]_{ij} \frac{\partial N^J}{\partial x} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_1}]_{ji} \frac{\partial N^I}{\partial x} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I  [\mathbf{A_2}]_{ij} \frac{\partial N^J}{\partial y} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_2}]_{ji} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} N^I [\mathbf{A_3}]_{ij} \frac{\partial N^J}{\partial z} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_3}]_{ji} \frac{\partial N^I}{\partial z} N^J d{\Omega} \quad U_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 2,2 ) )
DummyMat2 = 0.0_DFP
DummyMat2( 1, 1 ) = 1.0_DFP; DummyMat3( 2, 2 ) = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat3, MultiVar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_10( Term1 = 0, Term2 = 1, &
XType = 'dx', A = DummyMat2, MultiVar = .TRUE. )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_9( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat2, MultiVar = .TRUE. )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
```

### getConvectiveMatrix_11()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_11( Obj, A, A0, Term1, Term2, XType, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ) :: A0, A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:,:)` and `A0(:,:,:)` are three dimensional array. The shape of `A` and `A0` is `(M,M,NIPS)`. This array is defined at spatial-integration points. This array is responsible for coupling between different unknowns. In this method, `A` and `A0` vary in the spatial domain.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `XType` is string type, it denotes the type of spatial gradient.
- If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means that the spatial gradient is with respect to the `x` coordinate.
- If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means that the spatial gradient is with respect to the `y` coordinate.
- If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means that the spatial gradient is with respect to the `z` coordinate.
- `MultiVar` is a logical value which has no effect on the functionality of the method; this just for letting compiler know that this method has different interface.


CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat3, A0 = DummyMat3, MultiVar = .TRUE. )
CALL ElemSD % getConvectiveMatrix( Term1 = 1, Term2 = 0, XType = 'dx', A = DummyMat3, A0 = DummyMat3, MultiVar = .TRUE. )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I  [\mathbf{A_1}]_{kj} \frac{\partial N^J}{\partial x} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_1}]_{ki} \frac{\partial N^I}{\partial x} [\mathbf{B}]_{kj} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I  [\mathbf{A_2}]_{kj} [\mathbf{B}]_{kj} \frac{\partial N^J}{\partial y} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_2}]_{ki} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I [\mathbf{A_3}]_{kj} \frac{\partial N^J}{\partial z} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_3}]_{ki} \frac{\partial N^I}{\partial z} [\mathbf{B}]_{kj} N^J d{\Omega} \quad U_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( 2,2, NIP ) )
DummyMat3 = 0.0_DFP
DummyMat3( 1, 1, : ) = 1.0_DFP; DummyMat3( 2, 2, : ) = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', &
A = DummyMat3, A0 = DummyMat3, MultiVar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_11( Term1 = 0, Term2 = 1, &
XType = 'dx', A = DummyMat3, A0 = DummyMat3, MultiVar = .TRUE. )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_11( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat3, A0 = DummyMat3, MultiVar = .TRUE. )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
```

### getConvectiveMatrix_12()

INTERFACE

```fortran
 SUBROUTINE getConvectiveMatrix_11( Obj, A, A0, Term1, Term2, XType, MultiVar )

    USE Utility, ONLY : OUTERPROD

    CLASS( ConvectiveMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ) :: A0, A
    INTEGER( I4B ), INTENT( IN ) :: Term1, Term2
    CHARACTER( LEN = * ), INTENT( IN ) :: XType
    LOGICAL( LGT ), INTENT( IN ) :: MultiVar
```

DESCRIPTION

- `A(:,:)` and `A0(:,:)` are two dimensional array. The shape of `A` and `A0` is `(M,M)`. This array is responsible for coupling between different unknowns. In this method, `A` and `A0` do not vary in the spatial domain.
- `Term1` and `Term2` can have integer values, either 0 or 1. They denote the spatial derivative. If it is 1 then it means the first derivative and if it is 0 then it means no derivative.
- `XType` is string type, it denotes the type of spatial gradient.
- If `Xtype` is in the set `[dx, dX, dx1, dX1, x, X, x1, X1]` then it means that the spatial gradient is with respect to the `x` coordinate.
- If `Xtype` is in the set `[dy, dY, dx2, dX2, y, Y, x2, X2]` then it means that the spatial gradient is with respect to the `y` coordinate.
- If `Xtype` is in the set `[dz, dZ, dx3, dX3, z, Z, x3, X3]` then it means that the spatial gradient is with respect to the `z` coordinate.
- `MultiVar` is a logical value which has no effect on the functionality of the method; this just for letting compiler know that this method has different interface.


CODE SNIPPET

```fortran
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat2, A0 = DummyMat2, MultiVar = .TRUE. )
CALL ElemSD % getConvectiveMatrix( Term1 = 1, Term2 = 0, XType = 'dx', A = DummyMat2, A0 = DummyMat2, MultiVar = .TRUE. )
```

SYMBOLIC CALCULATION

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I  [\mathbf{A_1}]_{kj} \frac{\partial N^J}{\partial x} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_1}]_{ki} \frac{\partial N^I}{\partial x} [\mathbf{B}]_{kj} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I  [\mathbf{A_2}]_{kj} [\mathbf{B}]_{kj} \frac{\partial N^J}{\partial y} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_2}]_{ki} \frac{\partial N^I}{\partial y} N^J d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{B}]_{ki} N^I [\mathbf{A_3}]_{kj} \frac{\partial N^J}{\partial z} d{\Omega} \quad U_{jJ}$$

$${}^{2}M(I,J) = \delta U_{iI} \int_{\Omega} [\mathbf{A_3}]_{ki} \frac{\partial N^I}{\partial z} [\mathbf{B}]_{kj} N^J d{\Omega} \quad U_{jJ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 2,2 ) )
DummyMat2 = 0.0_DFP
DummyMat2( 1, 1 ) = 1.0_DFP; DummyMat2( 2, 2 ) = 1.0_DFP
CALL ElemSD % getConvectiveMatrix( Term1 = 0, Term2 = 1, XType = 'dx', &
A = DummyMat2, A0 = DummyMat2, MultiVar = .TRUE. )

CALL BlankLines( )
WRITE( *, "(A)" ) "CALL ElemSD % getConvectiveMatrix_11( Term1 = 0, Term2 = 1, &
XType = 'dx', A = DummyMat2, A0 = DummyMat2, MultiVar = .TRUE. )"
CALL ElemSD %  DisplayMatrix( )
```

**NIP = 4**

```fortran
CALL ElemSD % getConvectiveMatrix_11( Term1 = 0, Term2 = 1, XType = 'dx', A = DummyMat2, A0 = DummyMat2, MultiVar = .TRUE. )

MATRIX STORED IN ELEMENT SHAPE DATA ::

NIPS ::   4

-------------------------------------------------
2D MATRIX, Mat2(:, :) ::

 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.3333333      0.3333333      0.1666667     -0.1666667       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
 -0.1666667      0.1666667      0.3333333     -0.3333333       0.000000       0.000000       0.000000       0.000000
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.3333333      0.3333333      0.1666667     -0.1666667
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
   0.000000       0.000000       0.000000       0.000000     -0.1666667      0.1666667      0.3333333     -0.3333333
```