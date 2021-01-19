SUBMODULE ( RealVector_Method ) BLAS1
  USE BLASInterface
  USE BaseMethod

  IMPLICIT NONE

CONTAINS

!<-----------------------------------------------------------------------------
!                                                                        NORM2
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE NRM2scalar

    Ans = NORM2( Obj % Val )

  END PROCEDURE NRM2scalar

!<-----------------------------------------------------------------------------
!                                                                        NORM2
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE NRM2vector

    Ans = SQRT( DOT( Obj, Obj ) )

  END PROCEDURE NRM2vector

!<-----------------------------------------------------------------------------
!                                                                  SquareLength
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE NRM2SQRscalar

    Ans = DOT( Obj, Obj )

  END PROCEDURE NRM2SQRscalar

!<-----------------------------------------------------------------------------
!                                                                      NORM2SQR
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE NRM2SQRvector

    Ans = DOT( Obj, Obj )

  END PROCEDURE NRM2SQRvector

!<-----------------------------------------------------------------------------
!                                                                      NORM2SQR
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE NRM2SQRintrinsic

    Ans = DOT_PRODUCT( Val, Val )

  END PROCEDURE NRM2SQRintrinsic

!<-----------------------------------------------------------------------------
!                                                                          DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicDOTintrinsic

    Ans = DOT( SIZE( Val1 ), Val1, 1, Val2, 1 )

  END PROCEDURE intrinsicDOTintrinsic

!<-----------------------------------------------------------------------------
!                                                                         DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarDOTscalar

    Ans = DOT( Obj1 % Val, Obj2 % Val )

  END PROCEDURE scalarDOTscalar

!<-----------------------------------------------------------------------------
!                                                                         DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE vectorDOTvector

    INTEGER( I4B ) :: i
    Ans = 0.0
    DO i = 1, SIZE( Obj1 )
      Ans = Ans + DOT( Obj1( i ) % Val, Obj2( i ) % Val )
    END DO

  END PROCEDURE vectorDOTvector

!<-----------------------------------------------------------------------------
!                                                                         DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE vectorDOTscalar

    INTEGER( I4B ) :: i

    Ans = 0.0
    DO i = 1, SIZE( Obj1 )
      Ans = Ans + DOT( Obj1( i ) % Val, Obj2 % Val )
    END DO

  END PROCEDURE vectorDOTscalar

!<-----------------------------------------------------------------------------
!                                                                         DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarDOTvector

    INTEGER( I4B ) :: i

    Ans = 0.0
    DO i = 1, SIZE( Obj2 )
      Ans = Ans + DOT( Obj1 % Val, Obj2( i ) % Val )
    END DO

  END PROCEDURE scalarDOTvector

!<-----------------------------------------------------------------------------
!                                                                         DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarDOTintrinsic

    Ans = DOT( Obj % Val, Val )

  END PROCEDURE scalarDOTintrinsic

!<-----------------------------------------------------------------------------
!                                                                         DOT
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicDOTscalar

    Ans = DOT( Obj % Val, Val )

  END PROCEDURE intrinsicDOTscalar

!<-----------------------------------------------------------------------------
!                                                                         ASUM
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE ASUMintrinsic

    Ans = ASUM( SIZE( Val ), Val, 1 )

  END PROCEDURE ASUMintrinsic

!<-----------------------------------------------------------------------------
!                                                                         ASUM
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE ASUMscalar

    Ans = ASUM( Obj % Val )

  END PROCEDURE ASUMscalar

!<-----------------------------------------------------------------------------
!                                                                         ASUM
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE ASUMvector

    INTEGER( I4B ) :: i

    Ans = 0.0
    DO i = 1, SIZE( Obj )
      Ans = Ans + ASUM( Obj( i ) % Val )
    END DO

  END PROCEDURE ASUMvector

!<-----------------------------------------------------------------------------
!                                                                         COPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicCOPYintrinsic

    CALL SHALLOWCOPY( Val1, Val2 )
    CALL COPY( SIZE( Val2 ), Val2, 1, Val1, 1 )

  END PROCEDURE intrinsicCOPYintrinsic

!<-----------------------------------------------------------------------------
!                                                                         COPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarCOPYscalar

    CALL COPY( Obj1 % Val, Obj2 % Val )
    CALL setTotalDimension( Obj1, 1_I4B )

  END PROCEDURE scalarCOPYscalar

!<-----------------------------------------------------------------------------
!                                                                         COPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE vectorCOPYvector

    INTEGER( I4B ) :: i
    CALL SHALLOWCOPY( Obj1, Obj2 )
    DO i = 1, SIZE( Obj1 )
      CALL COPY( Obj1( i ) % Val, Obj2( i ) % Val )
      CALL setTotalDimension( Obj1( i ), 1_I4B )
    END DO

  END PROCEDURE vectorCOPYvector

!<-----------------------------------------------------------------------------
!                                                                         COPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarCOPYvector

    INTEGER( I4B ) :: i, r1, r2, tNodes
    CALL SHALLOWCOPY( Obj1, Obj2 )
    r1 = 0; r2 = 0
    DO i = 1, SIZE( Obj2 )
      tNodes = SIZE( Obj2( i ) % Val )
      r1 = r2 + 1
      r2 = r2 + tNodes
      Obj1 % Val( r1 : r2 ) = Obj2( i ) % Val
    END DO

  END PROCEDURE scalarCOPYvector

!<-----------------------------------------------------------------------------
!                                                                         COPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarCOPYintrinsic

    CALL SHALLOWCOPY( Obj%Val, Val )
    CALL COPY( Obj % Val, Val )

  END PROCEDURE scalarCOPYintrinsic

!<-----------------------------------------------------------------------------
!                                                                         COPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicCOPYscalar

    CALL SHALLOWCOPY( Val, Obj % Val )
    CALL COPY( Val, Obj % Val )

  END PROCEDURE intrinsicCOPYscalar

!<-----------------------------------------------------------------------------
!                                                                  SHALLOWCOPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicSHALLOWCOPYintrinsic

    IF( ALLOCATED( Val1 ) ) THEN
      IF( SIZE( Val1 ) .NE. SIZE( Val2 ) ) THEN
        DEALLOCATE( Val1 )
        ALLOCATE( Val1( SIZE( Val2 ) ) )
      END IF
    ELSE
      ALLOCATE( Val1( SIZE( Val2 ) ) )
    END IF

  END PROCEDURE intrinsicSHALLOWCOPYintrinsic

!<-----------------------------------------------------------------------------
!                                                                  SHALLOWCOPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarSHALLOWCOPYscalar

    CALL SHALLOWCOPY( Obj1 % Val , Obj2 % Val )

  END PROCEDURE scalarSHALLOWCOPYscalar

!<-----------------------------------------------------------------------------
!                                                                  SHALLOWCOPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE vectorSHALLOWCOPYvector

    INTEGER( I4B ) :: i

    IF( ALLOCATED( Obj1 ) ) THEN
      IF( SIZE( Obj1 ) .NE. SIZE( Obj2 ) ) THEN
        DEALLOCATE( Obj1 )
        ALLOCATE( Obj1( SIZE( Obj2 ) ) )
      END IF
    ELSE
      ALLOCATE( Obj1( SIZE( Obj2 ) ) )
    END IF

    DO i = 1, SIZE( Obj1 )
      CALL SHALLOWCOPY( Obj1( i ) % Val, Obj2( i ) % Val )
    END DO

  END PROCEDURE vectorSHALLOWCOPYvector

!<-----------------------------------------------------------------------------
!                                                                   SHALLOWCOPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarSHALLOWCOPYvector
    INTEGER( I4B ) :: i, tNodes

    tNodes = 0

    DO i = 1, SIZE( Obj2 )
      tNodes = tNodes + SIZE( Obj2( i ) % Val )
    END DO

    IF( ALLOCATED( Obj1 % Val ) ) THEN
      IF( SIZE( Obj1 % Val ) .NE. tNodes ) THEN
        DEALLOCATE( Obj1 % Val )
        ALLOCATE( Obj1 % Val( tNodes ) )
      END IF
    ELSE
      ALLOCATE( Obj1 % Val( tNodes ) )
    END IF

  END PROCEDURE scalarSHALLOWCOPYvector

!<-----------------------------------------------------------------------------
!                                                                   SHALLOWCOPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarSHALLOWCOPYintrinsic
    CALL SHALLOWCOPY( Obj%Val, Val )
  END PROCEDURE scalarSHALLOWCOPYintrinsic

!<-----------------------------------------------------------------------------
!                                                                   SHALLOWCOPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicSHALLOWCOPYscalar
    CALL SHALLOWCOPY( Val, Obj % Val )
    CALL COPY( Val, Obj % Val )
  END PROCEDURE intrinsicSHALLOWCOPYscalar

!<-----------------------------------------------------------------------------
!                                                                         SWAP
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicSWAPintrinsic

    CALL SWAP( SIZE( Val1 ), Val1, 1, Val2, 1 )

  END PROCEDURE intrinsicSWAPintrinsic

!<-----------------------------------------------------------------------------
!                                                                         SWAP
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarSWAPscalar

    CALL SWAP( Obj1 % Val, Obj2 % Val )

  END PROCEDURE scalarSWAPscalar

!<-----------------------------------------------------------------------------
!                                                                         SWAP
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE vectorSWAPvector

    INTEGER( I4B ) :: i

    DO CONCURRENT ( i = 1:SIZE( Obj1 ) )
      CALL SWAP( Obj1( i ) % Val, Obj2( i ) % Val )
    END DO

  END PROCEDURE vectorSWAPvector

!<-----------------------------------------------------------------------------
!                                                                         SWAP
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarSWAPintrinsic

    CALL SWAP( Val, Obj % Val )

  END PROCEDURE scalarSWAPintrinsic

!<-----------------------------------------------------------------------------
!                                                                         SWAP
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicSWAPscalar

    CALL SWAP( Val, Obj % Val )

  END PROCEDURE intrinsicSWAPscalar

!<-----------------------------------------------------------------------------
!                                                                         SCALE
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE SCALintrinsic

#ifndef _REAL_
    Val = Val * Alpha
#else
    CALL SCALE( SIZE( Val ), Alpha, Val, 1 )
#endif

  END PROCEDURE SCALintrinsic

!<-----------------------------------------------------------------------------
!                                                                         SCALE
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE SCALscalar

    CALL SCALE( Alpha, Obj % Val )

  END PROCEDURE SCALscalar

!<-----------------------------------------------------------------------------
!                                                                         SCALE
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE SCALvector

    INTEGER( I4B ) :: i

    DO CONCURRENT( i = 1:SIZE( Obj ) )
      CALL SCALE( Alpha, Obj( i ) % Val )
    END DO

  END PROCEDURE SCALvector

!<-----------------------------------------------------------------------------
!                                                                         AXPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE intrinsicAXPYintrinsic

#ifndef _REAL_
    Val1 = Val1 + Alpha * Val2
#else
    CALL AXPY( SIZE( Val2 ), Alpha, Val2, 1, Val1, 1 )
#endif

  END PROCEDURE intrinsicAXPYintrinsic

!<-----------------------------------------------------------------------------
!                                                                         AXPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarAXPYscalar

    CALL AXPY( Obj1 % Val, Alpha, Obj2 % Val )

  END PROCEDURE scalarAXPYscalar

!<-----------------------------------------------------------------------------
!                                                                         AXPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE scalarAXPYintrinsic

    CALL AXPY( Obj % Val, Alpha, Val )

  END PROCEDURE scalarAXPYintrinsic

!<-----------------------------------------------------------------------------
!                                                                         AXPY
!<-----------------------------------------------------------------------------

  MODULE PROCEDURE vectorAXPYvector

    INTEGER( I4B ) :: i

    DO CONCURRENT( i = 1:SIZE( Obj1 ) )
      CALL AXPY( Obj1( i ) % Val, Alpha, Obj2( i ) % Val )
    END DO

  END PROCEDURE vectorAXPYvector

END SUBMODULE BLAS1