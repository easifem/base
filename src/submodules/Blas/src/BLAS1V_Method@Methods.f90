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

#define _tr_ Indx(1):Indx(2):Indx(3)

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMscalar
  INTEGER ( I4B ) :: Indx( 4 )

  !! small data
  IF( (SIZE( Obj%Val ) .LE. SMALL_VECTOR_LEN) &
    & .OR. (OMP%STATE .EQ. OMP_THREADS_FORKED) ) THEN
    Ans = ASUM( Obj%Val )
  !! big data
  ELSE
    !> create threads and share the work
    Ans = 0.0_DFP
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(Indx) REDUCTION( +: Ans )
    CALL OMP_INITIATE
    Indx = OMP_Partition( SIZE( Obj%Val ), OMP%NUM_THREADS )
    Ans = Ans + ASUM( Obj%Val( _tr_ ) )
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE ASUMscalar

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMvector
  INTEGER( I4B ) :: i, Indx( 4 )

  IF( OMP%STATE .EQ. OMP_THREADS_FORKED ) THEN
    !> already forked
    Ans = 0.0
    DO i = 1, SIZE( Obj )
      !> Calling BLAS routine
      Ans = Ans + ASUM( Obj( i )%Val )
    END DO
  ELSE
    !> forking
    Ans = 0.0_DFP
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i, Indx) REDUCTION( +: Ans )
    CALL OMP_INITIATE
    DO i = 1, SIZE(Obj)
      Indx = OMP_Partition( SIZE( Obj(i)%Val ), OMP%NUM_THREADS )
      Ans = Ans + ASUM( Obj(i)%Val( _tr_ ) )
    END DO
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE ASUMvector

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYscalar
  INTEGER( I4B ) :: Indx( 4 )

  IF( ( SIZE( X%Val ) .LE. SMALL_VECTOR_LEN ) .OR. &
    & ( OMP%STATE .EQ. OMP_THREADS_FORKED ) ) THEN
    CALL AXPY( X=X%Val, Y=Y%Val, A=A )
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(Indx)
    CALL OMP_INITIATE
    Indx = OMP_Partition( SIZE( X%Val ), OMP%NUM_THREADS )
    CALL AXPY( X = X%Val( _tr_ ), Y = Y%Val( _tr_ ), A = A )
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE scalarAXPYscalar

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYintrinsic
  INTEGER( I4B ) :: Indx( 4 )
  IF( ( SIZE( X ) .LE. SMALL_VECTOR_LEN ) .OR. &
    & ( OMP%STATE .EQ. OMP_THREADS_FORKED ) ) THEN
    CALL AXPY( X=X, Y=Y%Val, A=A )
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(Indx)
    CALL OMP_INITIATE
    Indx = OMP_Partition( SIZE( X ), OMP%NUM_THREADS )
    CALL AXPY( X = X( _tr_ ), Y = Y%Val( _tr_ ), A = A )
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE scalarAXPYintrinsic

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorAXPYvector
  INTEGER( I4B ) :: i, Indx( 4 )

#ifdef DEBUG_VER
  IF( SIZE( A ) .NE. SIZE( X ) ) THEN
    CALL ErrMSG(&
      & Msg = "SIZE(A) should be equal to SIZE(X)", &
      & File = __FILE__, &
      & Routine = "vectorAXPYvector()", &
      & Line = __LINE__, &
      & UnitNo = stderr )
    STOP
  END IF
#endif

  IF( OMP%STATE .EQ. OMP_THREADS_FORKED ) THEN
    DO i = 1, SIZE( X )
      CALL AXPY( Y=Y( i )%Val, A=A( i ), X=X( i )%Val )
    END DO
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i, Indx)
    CALL OMP_INITIATE
    DO i = 1, SIZE( X )
      Indx = OMP_Partition( SIZE( X( i )%Val ), OMP%NUM_THREADS )
      CALL AXPY( Y=Y( i )%Val( _tr_ ), X=X( i )%Val( _tr_ ), A = A( i ) )
    END DO
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE vectorAXPYvector

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYscalar
  INTEGER( I4B ) :: Indx( 4 )

  CALL SHALLOWCOPY( Y = Y%Val, X = X%Val )
  CALL setTotalDimension( Y, 1_I4B )
  IF( ( SIZE( X%Val ) .LE. SMALL_VECTOR_LEN ) .OR. &
    & ( OMP%STATE .EQ. OMP_THREADS_FORKED ) ) THEN
    CALL COPY( Y = Y%Val, X = X%Val )
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(Indx)
    CALL OMP_INITIATE
    Indx = OMP_Partition( SIZE( X%Val ), OMP%NUM_THREADS )
    CALL COPY( X = X%Val( _tr_ ), Y = Y%Val( _tr_ ) )
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE scalarCOPYscalar

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYintrinsic
  INTEGER( I4B ) :: Indx( 4 )

  CALL SHALLOWCOPY( Y = Y%Val, X = X )
  CALL setTotalDimension( Y, 1_I4B )
  IF( ( SIZE( X ) .LE. SMALL_VECTOR_LEN ) .OR. &
    & ( OMP%STATE .EQ. OMP_THREADS_FORKED ) ) THEN
    CALL COPY( Y = Y%Val, X = X )
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(Indx)
    CALL OMP_INITIATE
    Indx = OMP_Partition( SIZE( X ), OMP%NUM_THREADS )
    CALL COPY( X = X( _tr_ ), Y = Y%Val( _tr_ ) )
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE scalarCOPYintrinsic

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicCOPYscalar
  INTEGER( I4B ) :: Indx( 4 )

  CALL SHALLOWCOPY( Y = Y, X = X%Val )
  IF( ( SIZE( X%Val ) .LE. SMALL_VECTOR_LEN ) .OR. &
    & ( OMP%STATE .EQ. OMP_THREADS_FORKED ) ) THEN
    CALL COPY( Y = Y, X = X%Val )
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(Indx)
    CALL OMP_INITIATE
    Indx = OMP_Partition( SIZE( X%Val ), OMP%NUM_THREADS )
    CALL COPY( X = X%Val( _tr_ ), Y = Y( _tr_ ) )
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE intrinsicCOPYscalar

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorCOPYvector
  INTEGER( I4B ) :: i, Indx( 4 )

  CALL SHALLOWCOPY( Y=Y, X=X )
  IF( OMP%STATE .EQ. OMP_THREADS_FORKED ) THEN
    DO i = 1, SIZE( X )
      CALL COPY( Y=Y( i )%Val, X=X( i )%Val )
      CALL setTotalDimension( Y(i), 1_I4B )
    END DO
  ELSE
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i, Indx)
    CALL OMP_INITIATE
    DO i = 1, SIZE( X )
      Indx = OMP_Partition( SIZE( X( i )%Val ), OMP%NUM_THREADS )
      CALL COPY( X=X( i )%Val( _tr_ ), Y=Y( i )%Val( _tr_ ) )
    END DO
    !$OMP DO
    DO i = 1, SIZE( Y )
      CALL setTotalDimension( Y(i), 1_I4B )
    END DO
    !$OMP ENDDO
    CALL OMP_FINALIZE
    !$OMP END PARALLEL
  END IF
END PROCEDURE vectorCOPYvector

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

!!Y=X(:)%Val
MODULE PROCEDURE scalarCOPYvector
  INTEGER( I4B ) :: i, r1, r2, Indx( 4 )

  CALL SHALLOWCOPY( Y=Y, X=X )
  CALL setTotalDimension( Y, 1_I4B )
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

#undef _tr_
END SUBMODULE Methods