# Space Time Fext Vector Class

## ToDo

- External force due to function
- External force due to line load or point load
- External force due to impact load, dirac delta function

[toc]

## Getting Started

### Making the object

`STFextVector_` class is subclass of `STElemShapeData_` class. The object of this class can be initiated using following commands. 

- Calling the inherited method `initiate`

```fortran
CALL STElemSD % Initiate( NIPS = NIPS, NIPT = NIPT)
```

- We can use the `STFextVector()` function

```fortran
CLASS( STELemShapeData_ ), POINTER :: STElemSD
STElemSD => STFextVector( )
STElemSD => STFextVector( row = row, NIPS = NIPS, NIPT = NIPT )
STElemSD => STFextVector( I1, I2, I3, NIPS, NIPT)
```

### Getting the Space time Fext Vector

$$
J\left( {i,I,a} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {{N^I}{T_a}\mathop {{f_i}}\limits_{ext} d\Omega dt} }
$$

We can compute the above integral using the following fortran command

```fortran
CALL Obj % getFextVector( Fext )
```

In the above call the argument `Fext` can be a Rank-1, Rank-2, or Rank-3 matrix.

- If Fext is changing with both space and time then it must be described by the Rank-3 matrix.
- If Fext is changing only in space then it must be described by the Rank-2 matrix.
- If Fext is not changing with space and time then it must be described by the Rnak-1 array.

In case `Fext` is defined at the quadrature points then we can use the following fortran command.

```fortran
CALL Obj % getFextVector(Fext, FextType)
```

Here `FextType` can be `NodalValues` or `QuadPoints`. Once again `Fext` can be a rank-1, rank-2, or rank-3 array.

To compute the following integral

$$
J\left( {i,I,a} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {{\bf{c}} \cdot \frac{{\partial {N^I}{T_a}}}{{d{\bf{x}}}}\mathop {{f_i}}\limits_{ext} d\Omega dt} }
$$

we can use the following fortran command

```fortran
CALL Obj % getFextVector(Fext, C, FextType, CType)
```

> In the above call `C` denotes the convective matrix, `Fext` denotes external force vector. `FextType` and `CType` can be `NodalValues` and/or `QuadPoints`. Moreover, `C` and `Fext` can be rank-1, rank-2, rank-3 fortran arrays.

IF both `Fext` and `C` can be given by nodal-values instead of quadrature point values then we can use the following fortran command.

```fortran
CALL Obj % getFextVector( Fext, C )
```

> Note that in above call Fext and C can be rank-1, rank-2, and/or rank-3 fortran array.

To compute the following integral

$$
J\left( {i,I,a} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{dt}}\mathop {{f_i}}\limits_{ext} d\Omega dt} }
$$

we can call the following fortran command

```fortran
CALL Obj % getFextVector( Fext, FextType, "dt")
```

To compute the following integral

$$
J\left( {i,I,a} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{dx}}\mathop {{f_i}}\limits_{ext} d\Omega dt} }
$$

we can call the following fortran command

```fortran
CALL Obj % getFextVector( Fext, FextType, "dx")
```

To compute the following integral

$$
J\left( {i,I,a} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{dy}}\mathop {{f_i}}\limits_{ext} d\Omega dt} }
$$

we can call the following fortran command

```fortran
CALL Obj % getFextVector( Fext, FextType, "dy")
```

To compute the following integral

$$
J\left( {i,I,a} \right) = \int_{{I_n}}^{} {\int_\Omega ^{} {\frac{{\partial {N^I}{T_a}}}{{dz}}\mathop {{f_i}}\limits_{ext} d\Omega dt} }
$$

we can call the following fortran command

```fortran
CALL Obj % getFextVector( Fext, FextType, "dz")
```



## Theory

Very often we need to compute the following matrices.

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_{iI} \int_{Q_n} N^I T_a f_i {dQ}$$

> These tasks are performed by following methods; `getFextVector_1()`, `getFextVector_2()`, `getFextVector_3()`, `getFextVector_4()`, and `getFextVector_5()`

Now consider the following space-time finite element matrices.

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

> These tasks are performed by following methods; `getFextVector_6()`, `getFextVector_7()`, `getFextVector_8()`, `getFextVector_9()`, `getFextVector_10()`, `getFextVector_10()`, `getFextVector_11()`, `getFextVector_12()`, `getFextVector_13()`, `getFextVector_14()`, `getFextVector_15()`, `getFextVector_16()`, `getFextVector_17()`, `getFextVector_18()`, `getFextVector_19()`, `getFextVector_20()`, `getFextVector_21()`, `getFextVector_22()`, and `getFextVector_23()`.

Now consider the following space-time finite element matrices.

$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial t} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial x} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial y} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial z} f_i dQ$$

> These tasks are performed by following methods; `getFextVector_24()`, `getFextVector_25()`, `getFextVector_26()`.

## Methods

### getFextVector_1()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_1( Obj, Fext )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `Fext(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `Fext` varies in space-time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat3 )
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_iI \int_{Q_n} N^I T_a f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3 )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_1( Fext = DummyMat3 )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_1( Fext = DummyMat3 )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```

### getFextVector_2()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_2( Obj, Fext )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `Fext(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the space nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case, `Fext` varies only in space and remains constant in time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat2 )
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_iI \int_{Q_n} N^I T_a f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat2 )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_2( Fext = DummyMat2 )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_2( Fext = DummyMat2 )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```

### getFextVector_3()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_3( Obj, Fext )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `Fext(:)` is a vector of shape `(M)`. It denotes the spatial components of external force. The first index denotes the componenets of a vector. In this case, `Fext` does not change in both space and time.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyVec )
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_iI \int_{Q_n} N^I T_a f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyVec )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_3( Fext = DummyVec )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_3( Fext = DummyVec )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```

### getFextVector_4()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_4( Obj, Fext, FextType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `FextType` is a string. 
    -   If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `Fext` varies in space-time domain.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `Fext` varies in space-time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType =  'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType =  'Quad')
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_iI \int_{Q_n} N^I T_a f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_4( Fext = DummyMat3, &
& FextType = "Quad" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_4( Fext = DummyMat3,  FextType = "Quad" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```

### getFextVector_5()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_5( Obj, Fext, FextType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :), INTENT( IN ) :: Fext
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `FextType` is a string. 
    -   If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the spatial nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case `Fext` varies only in space, and remains constant in time domain.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:)` is a two dimensional array of shape `(M, NIPS)`. It denotes the values of external force at spatial integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point. In this case `Fext` varies only in spatial domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType =  'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType =  'Quad')
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = {}^{a}\delta u_iI \int_{Q_n} N^I T_a f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = 'Quad' )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_5( Fext = DummyMat2, &
& FextType = "Quad" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_5( Fext = DummyMat2,  FextType = "Quad" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```

### getFextVector_6()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_6( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext, C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `FextType` is a string. 
    -   If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `Fext` varies in space-time domain.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `Fext` varies in space-time domain.
- `Ctype` is a string.
    -   If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `C(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of _convective velocity_. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `C` varies in space-time domain.
    - If `CType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `C` varies in space-time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat3, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat3, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat3, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat3, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Quad", &
C = DummyMat3, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_6( Fext = DummyMat3, FextType = "Quad", &
C = DummyMat3, CType = "Quad" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_6( Fext = DummyMat3, FextType = "Quad", C = DummyMat3, CType = "Quad" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_7()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_7( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext, C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- `FextType` is a string. 
    -   If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the spatial nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case `Fext` varies in space-domain, but remains constant in time.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:)` is a two dimensional array of shape `(M, NIPS)`. It denotes the values of external force at space-integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point. In this case `Fext` varies only in space, but remains constant in time domain.
- `Ctype` is a string.
    -   If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `C(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the space-nodal values of _convective velocity_. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case `C` varies in space-time domain.
    - If `CType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C(:,:)` is a three dimensional array of shape `(M, NIPS)`. It denotes the values of external force at space-integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point. In this case `C` varies in space-domain, but remains constant in time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat2, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat2, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat2, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat2, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NIPS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Quad", &
C = DummyMat2, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_7( Fext = DummyMat2, FextType = "Quad", &
C = DummyMat2, CType = "Quad" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_7( Fext = DummyMat2, FextType = "Quad", C = DummyMat2, CType = "Quad" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_8()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_8( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext, C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- This methods computes the space-time element vector for external force. 
- There is no effect of `Ctype` or `FextType`. 
- Both `Fext` and `C` are constant in space and time.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyVec, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyVec, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyVec, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyVec, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION 

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Quad", &
C = DummyVec, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_7( Fext = DummyVec, FextType = "Quad", &
C = DummyVec, CType = "Quad" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_8( Fext = DummyVec, FextType = "Quad", C =DummyVec, CType = "Quad" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_9()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_9( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- `FextType` is a string.

    - If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `Fext` varies in space-time domain.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `Fext` varies in space-time domain.
- `Ctype` is a string.
    -   If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `C(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the space-nodal values of _convective velocity_. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case `C` varies in space-time domain.
    - If `CType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C(:,:)` is a three dimensional array of shape `(M, NIPS)`. It denotes the values of external force at space-integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point. In this case `C` varies in space-domain, but remains constant in time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat2, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat2, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat2, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat2, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat2, FextType = "Nodal", CType = "Nodal" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_25( Fext = DummyMat3, &
C = DummyMat2, FextType = "Nodal", CType = "Nodal" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyMat3, C = DummyMat2, FextType = "Nodal", CType = "Nodal" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_10()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_10( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- `FextType` is a string.

    - If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `Fext` varies in space-time domain.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `Fext` varies in space-time domain.
- There is no effect of `Ctype`, `C` is constant in space and time.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyVec, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyVec, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyVec, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyVec, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyVec, FextType = "Nodal", CType = "Nodal" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_10( Fext = DummyMat3, &
C = DummyVec, FextType = "Nodal", CType = "Nodal" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_10( Fext = DummyMat3, C = DummyVec, FextType = "Nodal", CType = "Nodal" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_11()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_11( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- `FextType` is a string. 
    -   If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the spatial nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case `Fext` varies in space-domain, but remains constant in time.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:)` is a two dimensional array of shape `(M, NIPS)`. It denotes the values of external force at space-integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point. In this case `Fext` varies only in space, but remains constant in time domain.

- `Ctype` is a string.
    -   If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `C(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of _convective velocity_. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `C` varies in space-time domain.
    - If `CType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `C` varies in space-time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat3, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat3, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat3, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyMat3, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEAL[LOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3, C = DummyMat2, FextType = "Nodal", CType = "Nodal" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_11( Fext = DummyMat3, &
C = DummyMat2, FextType = "Nodal", CType = "Nodal" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_11( Fext = DummyMat3, C = DummyMat2, FextType = "Nodal", CType = "Nodal" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_12()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_12( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- There is no effect of `FextType`, and `Fext` is constant in space and time.

- `Ctype` is a string.
    -   If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `C(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of _convective velocity_. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `C` varies in space-time domain.
    - If `CType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `C` varies in space-time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat3, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat3, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat3, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat3, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat3, FextType = "Nodal", CType = "Nodal" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_12( Fext = DummyVec, &
C = DummyMat3, FextType = "Nodal", CType = "Nodal" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_12( Fext = DummyVec, C = DummyMat3, FextType = "Nodal", CType = "Nodal" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_13()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_13( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- There is no effect of `CType`, and `C` is constant in space and time.

- `FextType` is a string. 
    -   If `FextType` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `Fext(:,:)` is a two dimensional array of shape `(M, NNS)`. It denotes the spatial nodal values of external force. The first index denotes the componenets of a vector. The second index denotes the spatial-node. In this case `Fext` varies in space-domain, but remains constant in time.
    - If `FextType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `Fext(:,:)` is a two dimensional array of shape `(M, NIPS)`. It denotes the values of external force at space-integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point. In this case `Fext` varies only in space, but remains constant in time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyVec, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyVec, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyVec, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyVec, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat2, C = DummyVec, FextType = "Nodal", CType = "Nodal" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_12( Fext = DummyMat2, &
C = DummyVec, FextType = "Nodal", CType = "Nodal" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_12( Fext = DummyMat2, C = DummyVec, FextType = "Nodal", CType = "Nodal" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_14()

INTERFACE 

```fortran
 SUBROUTINE getFextVector_14( Obj, Fext, C, FextType, CType )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, CType
```

DESCRIPTION

- There is no effect of `FextType`, and `Fext` is constant in space and time.

- `Ctype` is a string.
    -   If `Ctype` is in the set `[Nodal, NodalValues, Nodal Values, STNodalValues, ST Nodal Values]` then `C(:,:,:)` is a three dimensional array of shape `(M, NNS, NNT)`. It denotes the space-time nodal values of _convective velocity_. The first index denotes the componenets of a vector. The second index denotes the spatial-node, the third index denotes the temporal-node. In this case `C` varies in space-time domain.
    - If `CType` is in the set `[Integration, IntegrationPoints, Integration Points, Quad, QuadPoints, Quad Points]` then `C(:,:,:)` is a three dimensional array of shape `(M, NIPS, NIPT)`. It denotes the values of external force at space-time integation points. The first index denotes the componenets of a vector. The second index denotes spatial integration point, the third index denotes the temporal-integration point. In this case `C` varies in space-time domain.

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat2, FextType =  'Nodal', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat2, FextType =  'Quad', Ctype = 'Quad')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat2, FextType =  'Quad', Ctype = 'Nodal')
CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat2, FextType =  'Nodal', Ctype = 'Quad')
```

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyVec, C = DummyMat2, FextType = "Nodal", CType = "Nodal" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_14( Fext = DummyVec, &
C = DummyMat2, FextType = "Nodal", CType = "Nodal" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 1, NIPT = 1__

```fortran
CALL STElemSD % getFextVector_14( Fext = DummyVec, C = DummyMat2, FextType = "Nodal", CType = "Nodal" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   1  NIPT ::   1

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000

Vec3( :, :,  2 )

  -2.000000      -2.000000
   0.000000       0.000000
   2.000000       2.000000
   0.000000       0.000000
```

### getFextVector_15()

INTERFACE

```fortran
 SUBROUTINE getFextVector_15( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext, C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_16()

INTERFACE

```fortran
 SUBROUTINE getFextVector_16( Obj, Fext, C )
    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext, C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_17()

INTERFACE

```fortran
 SUBROUTINE getFextVector_17( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext, C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_18()

INTERFACE

```fortran
 SUBROUTINE getFextVector_18( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
```

### getFextVector_19()

INTERFACE

```fortran
 SUBROUTINE getFextVector_19( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_20()

INTERFACE

```fortran
 SUBROUTINE getFextVector_20( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_21()

INTERFACE

```fortran
 SUBROUTINE getFextVector_21( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_22()

INTERFACE

```fortran
 SUBROUTINE getFextVector_22( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_23()

INTERFACE

```fortran
 SUBROUTINE getFextVector_23( Obj, Fext, C )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
```

DESCRIPTION

SYNTAX

SYMBOLIC CALCULATION

$${}^{3}Vec(i, I, a) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{\partial x_k} c_k f_i {dQ}$$

### getFextVector_24()

INTERFACE

```fortran
 SUBROUTINE getFextVector_24( Obj, Fext, FextType, Term1 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Fext
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, Term1
```

DESCRIPTION

SYMBOLIC CALCULATION

$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial t} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial x} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial y} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial z} f_i dQ$$

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Nodal", Term1 = "dx" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Nodal", Term1 = "dy" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Nodal", Term1 = "dz" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Nodal", Term1 = "dt" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Quad", Term1 = "dx" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Quad", Term1 = "dy" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Quad", Term1 = "dz" )
CALL STElemSD % getFextVector( Fext = DummyMat3, FextType = "Quad", Term1 = "dt" )
```

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat3, &
FextType = "Nodal", Term1 = "dx" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_24( Fext = DummyMat3, &
FextType = "Nodal", Term1 = "dx" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 4, NIPS = 2__

```fortran
CALL STElemSD % getFextVector_24( Fext = DummyMat3, FextType = "Nodal", Term1 = "dx" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000
```

```fortran
CALL STElemSD % getFextVector_24( Fext = DummyMat3, FextType = "Nodal", Term1 = "dy" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

  -1.000000      -1.000000
  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
```

```fortran
CALL STElemSD % getFextVector_24( Fext = DummyMat3, FextType = "Nodal", Term1 = "dt" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
  -1.000000      -1.000000
  -1.000000      -1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```


### getFextVector_25()

INTERFACE

```fortran
 SUBROUTINE getFextVector_25( Obj, Fext, FextType, Term1 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Fext
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, Term1
```

DESCRIPTION

SYMBOLIC CALCULATION

$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial t} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial x} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial y} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial z} f_i dQ$$

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Nodal", Term1 = "dx" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Nodal", Term1 = "dy" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Nodal", Term1 = "dz" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Nodal", Term1 = "dt" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Quad", Term1 = "dx" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Quad", Term1 = "dy" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Quad", Term1 = "dz" )
CALL STElemSD % getFextVector( Fext = DummyMat2, FextType = "Quad", Term1 = "dt" )
```

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyMat2, &
    FextType = "Nodal", Term1 = "dx" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_25( Fext = DummyMat2, &
FextType = "Nodal", Term1 = "dx" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 4, NIPT =2__

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyMat2, FextType = "Nodal", Term1 = "dx" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000
```

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyMat2, FextType = "Nodal", Term1 = "dx" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000
```

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyMat2, FextType = "Nodal", Term1 = "dt" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
  -1.000000      -1.000000
  -1.000000      -1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```

### getFextVector_26()

INTERFACE

```fortran
 SUBROUTINE getFextVector_26( Obj, Fext, FextType, Term1 )

    USE Utility, ONLY : OUTERPROD

    CLASS( STFextVector_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Fext
    CHARACTER( LEN = * ), INTENT( IN ) :: FextType, Term1
```

DESCRIPTION

SYMBOLIC CALCULATION

$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial t} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial x} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial y} f_i dQ$$
$${}^{3}V( i, I, a ) = {}^{a}\delta u_{iI} \int_{Q_n} \frac{\partial N^I T_a}{\partial z} f_i dQ$$

SYNTAX

```fortran
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Nodal", Term1 = "dx" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Nodal", Term1 = "dy" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Nodal", Term1 = "dz" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Nodal", Term1 = "dt" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Quad", Term1 = "dx" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Quad", Term1 = "dy" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Quad", Term1 = "dz" )
CALL STElemSD % getFextVector( Fext = DummyVec, FextType = "Quad", Term1 = "dt" )
```

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

CALL STElemSD % getFextVector( Fext = DummyVec, &
    FextType = "Nodal", Term1 = "dx" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getFextVector_25( Fext = DummyVec, &
FextType = "Nodal", Term1 = "dx" )'

CALL STElemSD % DisplayVector3( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyVec, FextType = "Nodal", Term1 = "dx" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA
NIPS ::   4  NIPT ::   2

-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
  -1.000000      -1.000000
```

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyVec, FextType = "Nodal", Term1 = "dy" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000

Vec3( :, :,  2 )

  -1.000000      -1.000000
  -1.000000      -1.000000
   1.000000       1.000000
   1.000000       1.000000
```

```fortran
CALL STElemSD % getFextVector_25( Fext = DummyVec, FextType = "Nodal", Term1 = "dt" )

VECTOR STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2
-------------------------------------------------

Vec3(:,:,:) ::

Vec3( :, :,  1 )

  -1.000000      -1.000000
  -1.000000      -1.000000
  -1.000000      -1.000000
  -1.000000      -1.000000

Vec3( :, :,  2 )

   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
   1.000000       1.000000
```