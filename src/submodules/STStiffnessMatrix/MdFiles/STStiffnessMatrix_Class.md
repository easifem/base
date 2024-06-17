# Space Time Stiffness Matrix Class

## Description

## Getting Started

### Making the Object

`STStiffnessMatrix_` object is child of `STElemShapeData_` class. This class is designed for _solid mechanics_ applications. The object of this class can be initiated using following commands. 

Calling the inherited method `initiate`

```fortran 
CALL STElemSD % Initiate( NIPS = NIPS, NIPT = NIPT)
CALL STElemSD % InitiateMatrix( row= row, col = col)
CALL STElemSD % InitiateMatrix( I1 = I1, I2 = I2, I3 = I3, I4= I4)
```

We can use the `STStiffnessMatrix_Pointer()` function, which returns the pointer to the object

```fortran
CLASS( STELemShapeData_ ), POINTER :: STElemSD

STElemSD => STStiffnessMatrix_Pointer( )
STElemSD => STStiffnessMatrix_Pointer( row = row, col = col, NIPS = NIPS, NIPT = NIPT )
STElemSD => STStiffnessMatrix_Pointer( I1, I2, I3, I4, I5, NIPS, NIPT)
```

We can also use the function `STStiffnessMatrix()` function, which returns the object

```fortran
STElemSD = STStiffnessMatrix( )
STElemSD = STStiffnessMatrix( row = row, col = col, NIPS = NIPS, NIPT = NIPT )
STElemSD = STStiffnessMatrix( I1, I2, I3, I4, I5, NIPS, NIPT)
```

### Getting the Stiffness Matrix

We have devided the Stiffness matrix into three basic categories. See the next section Theory for the explanation.

```fortran
CALL Obj % getStiffnessMatrix( Cijkl )
```

In above case Cijkl is a fortran array. It can be Rank-4, Rank-3, Rank-2.

```fortran
CALL Obj % getStiffnessMatrix( CData )
```

In above case `CData` is `ConstitutiveData_` object. It can be Rank-2, Rank-1 or Rank-0.

```fortran
CALL Obj % getStiffnessMatrix( Cijkl, TimeVector, IntegrationSide )
```

In above case Cijkl is a fortran array. It can be Rank-4, Rank-3, Rank-2. Inaddition, TimeVec is a length-2 rank-0 fortran array. It contains the starting and ending time i.e. t1, and t2. `IntegrationSide` is the character. It can be `Left`, `Right`, `Both`, or `NA`. See the Next section for more details

```fortran
CALL Obj % getStiffnessMatrix( CData, TimeVector, IntegrationSide )
```

In above case `CData` is `ConstitutiveData_` object. It can be Rank-2, Rank-1 or Rank-0. Inaddition, TimeVec is a length-2 rank-0 fortran array. It contains the starting and ending time i.e. t1, and t2. `IntegrationSide` is the character. It can be `Left`, `Right`, `Both`, or `NA`. See the Next section for more details.




































## Theory

Very often we need to compute the following matrices in solid-mechanics applications.

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_j} C_{ijkl} \frac{\partial N^J T_b}{ \partial x_l} {dQ} \quad {}^{b}u_kJ$$

Note here, $u \in R^{nsd}$. Generally, `Cijkl` has minor symmetry.

$$C_{ijkl} = C_{jikl}$$ 
$$C_{ijkl} = C_{ijlk}$$

The shape of ${}^{4}Mat(:, :, a, b)$ will be `(NSD*NNS, NSD*NNS)`. It will be a block matrix, and shape of each block will be `(NNS, NNS)`. For more details see the notes (_page 55_)


> These tasks are performed by following methods; `getStiffnessMatrix_1()`, `getStiffnessMatrix_2()`, and `getStiffnessMatrix_3()`


## Methods

### getStiffnessMatrix_1()

INTERFACE 

```fortran
 SUBROUTINE getStiffnessMatrix_1( Obj, Cijkl )

    USE Utility, ONLY : OUTERPROD

    CLASS( STStiffnessMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, :, : ), INTENT( IN ) :: Cijkl
```

DESCRIPTION

- This methods computes the Stiffness matrix. 
- `Cijkl(:,:,:,:)` is a four dimensional array of shape `(M,M,NIPS, NIPT)`. In two dimensional case it `M` should be either 3 or 4. In case of three dimensional it should be 6. See _page 55_  in the notes for maore details. In this case Cijkl may change in space and time domain. The third index denotes the spatial integration point and fourth index denotes the temporal integration points.

CODE SNIPPET

```fortran
CALL STElemSD % getStiffnessMatrix( Cijkl = DummyMat4 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_j} C_{ijkl} \frac{\partial N^J T_b}{ \partial x_l} {dQ} \quad {}^{b}u_kJ$$

TESTING

```fortran
IF( ALLOCATED( DummyMat4 ) ) DEALLOCATE( DummyMat4 )
ALLOCATE( DummyMat4( 3, 3, NIPS, NIPT ) )
DummyMat4 = 0.0_DFP; DummyMat4( 1,1, :, : ) = 1.0_DFP;
DummyMat4( 2,2, :, : ) = 1.0_DFP; DummyMat4( 3,3, :, : ) = 1.0_DFP
    
CALL STElemSD % getStiffnessMatrix( Cijkl = DummyMat4 )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getStiffnessMatrix_1( Cijkl = DummyMat4 )'
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getStiffnessMatrix_1( Cijkl = DummyMat4 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.1111111      0.4444444     -0.1111111     -0.2222222      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.2222222     -0.1111111      0.4444444     -0.1111111     -0.1666667      0.1666667      0.1666667     -0.1666667
 -0.1111111     -0.2222222     -0.1111111      0.4444444     -0.1666667      0.1666667      0.1666667     -0.1666667
  0.1666667      0.1666667     -0.1666667     -0.1666667      0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.2222222     -0.1111111      0.4444444     -0.1111111
  0.1666667      0.1666667     -0.1666667     -0.1666667     -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111      0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111      0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.1111111      0.4444444     -0.1111111     -0.2222222      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.2222222     -0.1111111      0.4444444     -0.1111111     -0.1666667      0.1666667      0.1666667     -0.1666667
 -0.1111111     -0.2222222     -0.1111111      0.4444444     -0.1666667      0.1666667      0.1666667     -0.1666667
  0.1666667      0.1666667     -0.1666667     -0.1666667      0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.2222222     -0.1111111      0.4444444     -0.1111111
  0.1666667      0.1666667     -0.1666667     -0.1666667     -0.1111111     -0.2222222     -0.1111111      0.4444444
```
> Note that in this case integrad is quadratic in time, therefore we need atleast 2 integration points in the time. This condition may change when the mesh is moving. Note that the row sum and column sum is zero as expected.

### getStiffnessMatrix_2()

INTERFACE 

```fortran
 SUBROUTINE getStiffnessMatrix_2( Obj, Cijkl )

    USE Utility, ONLY : OUTERPROD

    CLASS( STStiffnessMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: Cijkl
```

DESCRIPTION

- This methods computes the Stiffness matrix. 
- `Cijkl(:,:,:)` is a three dimensional array of shape `(M,M,NIPS)`. In two dimensional case it `M` should be either 3 or 4. In case of three dimensional it should be 6. See _page 55_  in the notes for more details. In this case, Cijkl changes only in space, and remains constant in time domain. The third index, denotes the spatial integration point.

CODE SNIPPET

```fortran
CALL STElemSD % getStiffnessMatrix( Cijkl = DummyMat3 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_j} C_{ijkl} \frac{\partial N^J T_b}{ \partial x_l} {dQ} \quad {}^{b}u_kJ$$

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( 3, 3, NIPS ) )
DummyMat3 = 0.0_DFP; DummyMat3( 1,1, : ) = 1.0_DFP;
DummyMat3( 2,2, : ) = 1.0_DFP; DummyMat3( 3,3, : ) = 1.0_DFP
    
CALL STElemSD % getStiffnessMatrix( Cijkl = DummyMat3 )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getStiffnessMatrix_2( Cijkl = DummyMat3 )'
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getStiffnessMatrix_2( Cijkl = DummyMat3 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.1111111      0.4444444     -0.1111111     -0.2222222      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.2222222     -0.1111111      0.4444444     -0.1111111     -0.1666667      0.1666667      0.1666667     -0.1666667
 -0.1111111     -0.2222222     -0.1111111      0.4444444     -0.1666667      0.1666667      0.1666667     -0.1666667
  0.1666667      0.1666667     -0.1666667     -0.1666667      0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.2222222     -0.1111111      0.4444444     -0.1111111
  0.1666667      0.1666667     -0.1666667     -0.1666667     -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111      0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111      0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.1111111      0.4444444     -0.1111111     -0.2222222      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.2222222     -0.1111111      0.4444444     -0.1111111     -0.1666667      0.1666667      0.1666667     -0.1666667
 -0.1111111     -0.2222222     -0.1111111      0.4444444     -0.1666667      0.1666667      0.1666667     -0.1666667
  0.1666667      0.1666667     -0.1666667     -0.1666667      0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.2222222     -0.1111111      0.4444444     -0.1111111
  0.1666667      0.1666667     -0.1666667     -0.1666667     -0.1111111     -0.2222222     -0.1111111      0.4444444
```

### getStiffnessMatrix_3()

INTERFACE 

```fortran
 SUBROUTINE getStiffnessMatrix_3( Obj, Cijkl )

    USE Utility, ONLY : OUTERPROD

    CLASS( STStiffnessMatrix_ ), INTENT( INOUT ) ::  Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Cijkl
```

DESCRIPTION

- This methods computes the Stiffness matrix. 
- `Cijkl(:,:)` is a three dimensional array of shape `(M,M)`. In two dimensional case it `M` should be either 3 or 4. In case of three dimensional it should be 6. See _page 55_  in the notes for more details. In this case, Cijkl does not change in both space and time.

CODE SNIPPET

```fortran
CALL STElemSD % getStiffnessMatrix( Cijkl = DummyMat2 )
```

SYMBOLIC CALCULATION 

$${}^{4}Mat(I, J, a, b) = \delta {}^{a} u_iI \int_{Q_n} \frac{\partial N^I T_a}{ \partial x_j} C_{ijkl} \frac{\partial N^J T_b}{ \partial x_l} {dQ} \quad {}^{b}u_kJ$$

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( 3, 3 ) )
DummyMat2 = 0.0_DFP; DummyMat2( 1,1 ) = 1.0_DFP;
DummyMat2( 2,2 ) = 1.0_DFP; DummyMat2( 3,3 ) = 1.0_DFP
    
CALL STElemSD % getStiffnessMatrix( Cijkl = DummyMat2 )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getStiffnessMatrix_3( Cijkl = DummyMat2 )'
CALL STElemSD % DisplayMatrix4( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getStiffnessMatrix_3( Cijkl = DummyMat2 )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

4D MATRIX, MAT4(:,:,:,:) ::

Mat4( :, :,  1, 1 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.1111111      0.4444444     -0.1111111     -0.2222222      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.2222222     -0.1111111      0.4444444     -0.1111111     -0.1666667      0.1666667      0.1666667     -0.1666667
 -0.1111111     -0.2222222     -0.1111111      0.4444444     -0.1666667      0.1666667      0.1666667     -0.1666667
  0.1666667      0.1666667     -0.1666667     -0.1666667      0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.2222222     -0.1111111      0.4444444     -0.1111111
  0.1666667      0.1666667     -0.1666667     -0.1666667     -0.1111111     -0.2222222     -0.1111111      0.4444444

Mat4( :, :,  1, 2 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111      0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 1 )

  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111      0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.8333333E-01
 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.8333333E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01  0.2222222     -0.5555556E-01 -0.1111111     -0.5555556E-01
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.5555556E-01  0.2222222     -0.5555556E-01 -0.1111111
 -0.8333333E-01 -0.8333333E-01  0.8333333E-01  0.8333333E-01 -0.1111111     -0.5555556E-01  0.2222222     -0.5555556E-01
  0.8333333E-01  0.8333333E-01 -0.8333333E-01 -0.8333333E-01 -0.5555556E-01 -0.1111111     -0.5555556E-01  0.2222222

Mat4( :, :,  2, 2 )

  0.4444444     -0.1111111     -0.2222222     -0.1111111      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.1111111      0.4444444     -0.1111111     -0.2222222      0.1666667     -0.1666667     -0.1666667      0.1666667
 -0.2222222     -0.1111111      0.4444444     -0.1111111     -0.1666667      0.1666667      0.1666667     -0.1666667
 -0.1111111     -0.2222222     -0.1111111      0.4444444     -0.1666667      0.1666667      0.1666667     -0.1666667
  0.1666667      0.1666667     -0.1666667     -0.1666667      0.4444444     -0.1111111     -0.2222222     -0.1111111
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.1111111      0.4444444     -0.1111111     -0.2222222
 -0.1666667     -0.1666667      0.1666667      0.1666667     -0.2222222     -0.1111111      0.4444444     -0.1111111
  0.1666667      0.1666667     -0.1666667     -0.1666667     -0.1111111     -0.2222222     -0.1111111      0.4444444
```