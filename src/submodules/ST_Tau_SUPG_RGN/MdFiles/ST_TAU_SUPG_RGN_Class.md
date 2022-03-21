# Space Time SUPG Stabilization Parameter

## Summary

The `STELemShapeData_` class is extended to `ST_Tau_SUPG_RGN_` class. This class is defined to compute the SUPG parameter as defined by Tezduyar and workers (see Tezduyar and Sathe, 2007). The stabilization parameter $\tau_{SUPG}$ is defined at space-time integration points. Therefore we store these values in `Obj % Mat2`. The shape of this array will be `(NIPS, NIPT)`.

### Initiating the element

The object can be initiated in following manner.

```fortran
CALL Obj % Initiate( NIPS, NIPT )
```

```fortran
CLASS( STELemShapeData_), POINTER :: STElemSD 

STElemSD => ST_Tau_SUPG_RGN( )
STElemSD => ST_Tau_SUPG_RGN( NIPS, NIPT )
```



## Theory

We are intended to compute the following.

## Methods

### getSUPG\_For\_Scalar\_1( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_1( Obj, Phi, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyMat2, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NNS, NNT ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyMat2, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_1( C = DummyMat3, Phi = DummyMat2, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_1( C = DummyMat3, Phi = DummyMat2, Mu= 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_2( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_2( Obj, Phi, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat2, Phi = DummyMat2, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NNS, NNT ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3( :, :, 1), Phi = DummyMat2, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_2( C = DummyMat2, Phi = DummyMat2, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_2( C = DummyMat2, Phi = DummyMat2, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_3( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_3( Obj, Phi, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyVec, Phi = DummyMat2, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NSD ) )
DummyVec = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NNS, NNT ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyVec, Phi = DummyMat2, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_3( C = DummyVec, Phi = DummyMat2, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_3(C = DummyVec, Phi = DummyMat2, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_4( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_4( Obj, Phi, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NNS ) )
DummyVec = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_4( C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_4(C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_5( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_5( Obj, Phi, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat2, Phi = DummyVec, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NNS ) )
DummyVec = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat2, Phi = DummyVec, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_5( C = DummyMat2, Phi = DummyVec, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_4(C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_6( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_6( Obj, Phi, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyVec, Phi = DummyVec, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NSD, NNS ) )
DummyMat2 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NNS ) )
DummyVec = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat2(:,1), Phi = DummyVec, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_6( C = DummyVec, Phi = DummyVec, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_6( C = DummyVec, Phi = DummyVec, Mu = 1.0_DFP )
MATRIX STORED IN ST-ELEMENT-SHAPEDATA
NIPS ::   4  NIPT ::   2
-------------------------------------------------
2D MATRIX, Mat2(:, :) ::
  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_7( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_7( Obj, Phi, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NNS, NNT ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_7( C = DummyMat3,&
  Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_7( C = DummyMat3,Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )
MATRIX STORED IN ST-ELEMENT-SHAPEDATA
NIPS ::   4  NIPT ::   2
-------------------------------------------------
2D MATRIX, Mat2(:, :) ::
  0.5358984      0.6000000  
  0.6000000      0.6000000
  0.6000000      0.5358984  
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_8( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_8( Obj, Phi, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat2, Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NNS, NNT ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3( :, :, 1), Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_8( C = DummyMat2,&
  Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_8( C = DummyMat2,Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )
MATRIX STORED IN ST-ELEMENT-SHAPEDATA
NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_9( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_9( Obj, Phi, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyVec, Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyMat2 ) ) DEALLOCATE( DummyMat2 )
ALLOCATE( DummyMat2( NNS, NNT ) )
DummyMat2 = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3( :, 1, 1), Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_9( C = DummyVec,&
Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_9( C = DummyVec,Phi = DummyMat2, Mu = 1.0_DFP, CType = "Quad" )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_10( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_10( Obj, Phi, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NNS ) )
DummyVec = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3, Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_10( C = DummyMat3,&
Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_10( C = DummyMat3,Phi = DummyVec, Mu =1.0_DFP, CType = "Quad" )
MATRIX STORED IN ST-ELEMENT-SHAPEDATA
NIPS ::   4  NIPT ::   2
-------------------------------------------------
2D MATRIX, Mat2(:, :) ::
  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_11( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_11( Obj, Phi, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyMat2, Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NNS ) )
DummyVec = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3(:,:,1), Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_11( C = DummyMat2,&
 Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_11( C = DummyMat2,Phi = DummyVec, Mu =1.0_DFP, CType = "Quad" )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Scalar\_12( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Scalar_12( Obj, Phi, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: Phi
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForScalar( C = DummyVec, Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NIPS, NIPT ) )
DummyMat3 = 1.0_DFP

IF( ALLOCATED( DummyVec ) ) DEALLOCATE( DummyVec )
ALLOCATE( DummyVec( NNS ) )
DummyVec = 1.0_DFP

CALL STElemSD % getSUPGForScalar( C = DummyMat3(:,1,1), Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Scalar_12( C = DummyVec,&
Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Scalar_11( C = DummyVec,Phi = DummyVec, Mu = 1.0_DFP, CType = "Quad" )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_1( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_1( Obj, U, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForVector( C = DummyMat3, U = DummyMat3, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getSUPGForVector( C = DummyMat3, U = DummyMat3, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Vector_1( C = DummyMat3, U = DummyMat3, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Vector_1( C = DummyMat3, U = DummyMat3, Mu = 1.0_DFP )
MATRIX STORED IN ST-ELEMENT-SHAPEDATA
NIPS ::   4  NIPT ::   2
-------------------------------------------------
2D MATRIX, Mat2(:, :) ::
  0.5358984      0.6000000  
  0.6000000      0.6000000
  0.6000000      0.5358984  
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_2( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_2( Obj, U, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForVector( C = DummyMat2, U = DummyMat3, Mu = 1.0_DFP )

```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getSUPGForVector( C = DummyMat3(:,:,1), U = DummyMat3, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Vector_2( C = DummyMat2, U = DummyMat3, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Vector_2( C = DummyMat2, U = DummyMat3, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_3( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_3( Obj, U, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForVector( C = DummyVec, U = DummyMat3, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getSUPGForVector( C = DummyMat3(:,1,1), U = DummyMat3, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Vector_3( C = DummyVec, U = DummyMat3, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran

CALL STElemSD % getSUPG_For_Vector_3( C = DummyVec, U = DummyMat3, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_4( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_4( Obj, U, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForVector( C = DummyMat3, U = DummyMat2, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getSUPGForVector( C = DummyMat3, U = DummyMat3(:,:,1), Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Vector_4( C = DummyMat3, U = DummyMat2, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Vector_4( C = DummyMat3, U = DummyMat2, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_5( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_5( Obj, U, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
CALL STElemSD % getSUPGForVector( C = DummyMat2, U = DummyMat2, Mu = 1.0_DFP )
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getSUPGForVector( C = DummyMat3( :, :, 1 ), U = DummyMat3(:,:,1), Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Vector_5( C = DummyMat2, U = DummyMat2, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Vector_5( C = DummyMat2, U = DummyMat2, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_6( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_6( Obj, U, C, Mu  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
IF( ALLOCATED( DummyMat3 ) ) DEALLOCATE( DummyMat3 )
ALLOCATE( DummyMat3( NSD, NNS, NNT ) )
DummyMat3 = 1.0_DFP

CALL STElemSD % getSUPGForVector( C = DummyVec, U = DummyMat2, Mu = 1.0_DFP )

CALL BlankLines( )
WRITE( *, "(A)") 'CALL STElemSD % getSUPG_For_Vector_6( C = DummyVec, U = DummyMat2, Mu = 1.0_DFP )'

CALL STElemSD % DisplayMatrix2( )
```

__NIPS = 4, NIPT = 2__

```fortran
CALL STElemSD % getSUPG_For_Vector_6( C = DummyVec, U = DummyMat2, Mu = 1.0_DFP )

MATRIX STORED IN ST-ELEMENT-SHAPEDATA

NIPS ::   4  NIPT ::   2

-------------------------------------------------

2D MATRIX, Mat2(:, :) ::

  0.5358984      0.6000000
  0.6000000      0.6000000
  0.6000000      0.5358984
  0.6000000      0.6000000
```

### getSUPG\_For\_Vector\_7( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_7( Obj, U, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
?
```

__NIPS = 4, NIPT = 2__

```fortran
?
```

### getSUPG\_For\_Vector\_8( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_7( Obj, U, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
?
```

__NIPS = 4, NIPT = 2__

```fortran
?
```

### getSUPG\_For\_Vector\_9( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_9( Obj, U, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
?
```

__NIPS = 4, NIPT = 2__

```fortran
?
```

### getSUPG\_For\_Vector\_10( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_10( Obj, U, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
?
```

__NIPS = 4, NIPT = 2__

```fortran
?
```

### getSUPG\_For\_Vector\_11( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_11( Obj, U, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
?
```

__NIPS = 4, NIPT = 2__

```fortran
?
```

### getSUPG\_For\_Vector\_12( )

INTERFACE

```fortran
 SUBROUTINE getSUPG_For_Vector_12( Obj, U, C, Mu, CType  )

    CLASS( ST_Tau_SUPG_RGN_ ), INTENT( INOUT ) :: Obj  
    REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: C
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: U
    REAL( DFP ), INTENT( IN ) :: Mu
    CHARACTER( LEN = * ), INTENT( IN ) :: CType
```

DESCRIPTION
?

SYNTAX

```fortran
?
```

SYMBOLIC CALCULATION
?

TESTING

```fortran
?
```

__NIPS = 4, NIPT = 2__

```fortran
?
```