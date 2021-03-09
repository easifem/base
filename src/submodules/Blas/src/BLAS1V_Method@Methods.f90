! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This submodule  implements BLAS1 methods of [[RealVector_]]

SUBMODULE ( BLAS1V_Method ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMscalar
  INTEGER ( I4B ) :: Indx( 4 ), i
  REAL( DFP ) :: local_sum

  !! small data
  IF( SIZE( Obj%Val ) .LE. SMALL_VECTOR_LEN ) THEN
    Ans = SUM( ABS( Obj%Val ) )

  !! big data
  ELSE

    IF( OMP%STATE .EQ. OMP_THREADS_FORKED ) THEN
      !> someone already started the working sharing outside the lexical scope
      Ans = ASUM( Obj%Val )
    ELSE

      !> create threads and share the work
      Ans = 0.0_DFP
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i, Indx, local_sum) REDUCTION( +: Ans )
      CALL OMP_INITIATE
      Indx = OMP_Partition( SIZE( Obj%Val ), OMP%NUM_THREADS )
      local_sum = ASUM( Obj%Val( Indx( 1 ) : Indx( 2 ) : Indx( 3 ) ) )
      Ans = Ans + local_sum
      CALL OMP_FINALIZE
      !$OMP END PARALLEL

    END IF
  END IF

END PROCEDURE ASUMscalar

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMvector
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj )
    Ans = Ans + ASUM( Obj( i )%Val )
  END DO
END PROCEDURE ASUMvector

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYscalar
  IF( SIZE( X%Val ) .LE. SMALL_VECTOR_LEN ) THEN
    Y%Val = Y%Val + A*X%Val
  ELSE
    CALL AXPY( X=X%Val, Y=Y%Val, A=A )
  END IF
END PROCEDURE scalarAXPYscalar

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYintrinsic
  CALL AXPY( Y=Y%Val, A=A, X=X )
END PROCEDURE scalarAXPYintrinsic

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorAXPYvector
  INTEGER( I4B ) :: i
  DO i = 1, SIZE( Y )
    CALL AXPY( Y=Y( i )%Val, A=A, X=X( i )%Val )
  END DO
END PROCEDURE vectorAXPYvector

!----------------------------------------------------------------------------
!                                                                       COPY
!----------------------------------------------------------------------------

PURE SUBROUTINE internal_copy(Y,X)
  REAL( DFP ), INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: X( : )
  IF( SIZE(X) .LE. SMALL_VECTOR_LEN ) THEN
    Y = X
  ELSE
    CALL COPY( Y=Y, X=X )
  END IF
END SUBROUTINE internal_copy

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYscalar
  CALL SHALLOWCOPY( Y=Y%Val, X=X%Val )
  CALL internal_copy( Y = Y%Val, X = X%Val )
  CALL setTotalDimension( Y, 1_I4B )
END PROCEDURE scalarCOPYscalar

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYintrinsic
  CALL SHALLOWCOPY( Y=Y%Val, X=X )
  CALL internal_copy( Y = Y%Val, X = X )
  CALL setTotalDimension( Y, 1_I4B )
END PROCEDURE scalarCOPYintrinsic

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicCOPYscalar
  CALL SHALLOWCOPY( Y=Y, X=X%Val )
  CALL internal_copy( Y = Y, X = X%Val )
END PROCEDURE intrinsicCOPYscalar

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorCOPYvector
  INTEGER( I4B ) :: i
  CALL SHALLOWCOPY( Y=Y, X=X )
  DO i = 1, SIZE( X )
    CALL COPY( X=X( i ), Y=Y( i ) )
  END DO
END PROCEDURE vectorCOPYvector

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYvector
  INTEGER( I4B ) :: i, r1, r2
  CALL SHALLOWCOPY( Y=Y, X=X )
  r1 = 0; r2 = 0
  DO i = 1, SIZE( X )
    r1 = r2 + 1
    r2 = r2 + SIZE( X( i )%Val )
    Y%Val( r1 : r2 ) = X( i )%Val
  END DO
END PROCEDURE scalarCOPYvector

!----------------------------------------------------------------------------
!                                                                       DOT
!----------------------------------------------------------------------------

PURE FUNCTION inner_dot( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1( : )
  REAL( DFP ), INTENT( IN ) :: Obj2( : )
  REAL( DFP ) :: Ans

  IF( SIZE( Obj1 ) .LE. SMALL_VECTOR_LEN ) THEN
    Ans = DOT_PRODUCT( Obj1, Obj2 )
  ELSE
    Ans = DOT( Obj1, Obj2 )
  END IF

END FUNCTION inner_dot

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTscalar
  Ans = inner_dot(Obj1%Val, Obj2%Val)
END PROCEDURE scalarDOTscalar

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTintrinsic
  Ans = inner_dot(Obj%val, val)
END PROCEDURE scalarDOTintrinsic

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorDOTvector
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj1 )
    Ans = Ans + DOT( Obj1( i ), Obj2( i ) )
  END DO
END PROCEDURE vectorDOTvector

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorDOTscalar
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj1 )
    Ans = Ans + DOT( Obj1( i )%Val, Obj2%Val )
  END DO
END PROCEDURE vectorDOTscalar

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTvector
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj2 )
    Ans = Ans + DOT( Obj1%Val, Obj2( i )%Val )
  END DO
END PROCEDURE scalarDOTvector

!----------------------------------------------------------------------------
!                                                                      NRM2
!----------------------------------------------------------------------------

PURE FUNCTION inner_nrm2(X) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: X( : )
  REAL( DFP ) :: Ans
  IF( SIZE( X ) .LE. SMALL_VECTOR_LEN ) THEN
    Ans = NORM2( x )
  ELSE
    Ans = NRM2( X )
  END IF
END FUNCTION inner_nrm2

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2scalar
  Ans = inner_nrm2(Obj%Val)
END PROCEDURE NRM2scalar

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2vector
  Ans = SQRT( DOT( Obj, Obj ) )
END PROCEDURE NRM2vector


!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSWAPscalar
  CALL SWAP( X=X%Val, Y=Y%Val )
END PROCEDURE scalarSWAPscalar

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorSWAPvector
  INTEGER( I4B ) :: i
  DO i = 1, SIZE( X )
    CALL SWAP( X=X( i )%Val, Y=Y( i )%Val )
  END DO
END PROCEDURE vectorSWAPvector

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSWAPintrinsic
  CALL SWAP( X=X%Val, Y=Y )
END PROCEDURE scalarSWAPintrinsic

!----------------------------------------------------------------------------
!                                                                       SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE SCALscalar
  CALL SCAL( A=A, X=X%Val )
END PROCEDURE SCALscalar

!----------------------------------------------------------------------------
!                                                                       SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE SCALvector
  INTEGER( I4B ) :: i
  DO i = 1, SIZE( X )
    CALL SCAL( A=A, X=X( i )%Val )
  END DO
END PROCEDURE SCALvector


!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicSHALLOWCOPYintrinsic
  CALL Reallocate( Y, SIZE( X ) )
END PROCEDURE intrinsicSHALLOWCOPYintrinsic

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSHALLOWCOPYscalar
  CALL SHALLOWCOPY( Y=Y%Val, X=X%Val )
END PROCEDURE scalarSHALLOWCOPYscalar

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorSHALLOWCOPYvector
  INTEGER( I4B ) :: i
  IF( ALLOCATED( Y ) ) THEN
    IF( SIZE( Y ) .NE. SIZE( X ) ) THEN
      DEALLOCATE( Y )
      ALLOCATE( Y( SIZE( X ) ) )
    END IF
  ELSE
    ALLOCATE( Y( SIZE( X ) ) )
  END IF
  DO i = 1, SIZE( Y )
    CALL SHALLOWCOPY( Y=Y( i )%Val, X=X( i )%Val )
  END DO
END PROCEDURE vectorSHALLOWCOPYvector

!----------------------------------------------------------------------------
!                                                               SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSHALLOWCOPYvector
  INTEGER( I4B ) :: i, tNodes
  tNodes = 0
  DO i = 1, SIZE( X )
    tNodes = tNodes + SIZE( X( i )%Val )
  END DO
  CALL Reallocate( Y%Val, tNodes )
END PROCEDURE scalarSHALLOWCOPYvector

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSHALLOWCOPYintrinsic
  CALL SHALLOWCOPY( Y=Y%Val, X=X )
END PROCEDURE scalarSHALLOWCOPYintrinsic

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicSHALLOWCOPYscalar
  CALL SHALLOWCOPY( Y=Y, X=X%Val )
  CALL COPY( Y=Y, X=X%Val )
END PROCEDURE intrinsicSHALLOWCOPYscalar

END SUBMODULE Methods