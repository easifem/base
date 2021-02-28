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

SUBMODULE ( RealVector_Method ) BLAS1
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTscalar
  Ans = DOT( Obj1%Val, Obj2%Val )
END PROCEDURE scalarDOTscalar

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorDOTvector
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj1 )
    Ans = Ans + DOT( Obj1( i ) % Val, Obj2( i ) % Val )
  END DO
END PROCEDURE vectorDOTvector

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorDOTscalar
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj1 )
    Ans = Ans + DOT( Obj1( i ) % Val, Obj2%Val )
  END DO
END PROCEDURE vectorDOTscalar

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTvector
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj2 )
    Ans = Ans + DOT( Obj1%Val, Obj2( i ) % Val )
  END DO
END PROCEDURE scalarDOTvector

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTintrinsic
  Ans = DOT( Obj%Val, Val )
END PROCEDURE scalarDOTintrinsic

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicDOTscalar
  Ans = DOT( Obj%Val, Val )
END PROCEDURE intrinsicDOTscalar

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2scalar
  Ans = SQRT( DOT( Obj, Obj ) )
END PROCEDURE NRM2scalar

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2vector
  Ans = SQRT( DOT( Obj, Obj ) )
END PROCEDURE NRM2vector

!----------------------------------------------------------------------------
!                                                                   NORM2SQR
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2SQRscalar
  Ans = DOT( Obj, Obj )
END PROCEDURE NRM2SQRscalar

!----------------------------------------------------------------------------
!                                                                   NORM2SQR
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2SQRvector
  Ans = DOT( Obj, Obj )
END PROCEDURE NRM2SQRvector

!----------------------------------------------------------------------------
!                                                                   NORM2SQR
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2SQRintrinsic
  Ans = DOT( Val, Val )
END PROCEDURE NRM2SQRintrinsic

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMscalar
  Ans = ASUM( Obj%Val )
END PROCEDURE ASUMscalar

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMvector
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, SIZE( Obj )
    Ans = Ans + ASUM( Obj( i ) % Val )
  END DO
END PROCEDURE ASUMvector

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
    tNodes = tNodes + SIZE( X( i ) % Val )
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

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYscalar
  CALL COPY( Y=Y%Val, X=X%Val )
  CALL setTotalDimension( Y, 1_I4B )
END PROCEDURE scalarCOPYscalar

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorCOPYvector
  INTEGER( I4B ) :: i
  CALL SHALLOWCOPY( Y=Y, X=X )
  DO i = 1, SIZE( X )
    CALL COPY( X=X( i ) % Val, Y=Y( i ) % Val )
    CALL setTotalDimension( Y( i ), 1_I4B )
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
    r2 = r2 + SIZE( X( i ) % Val )
    Y%Val( r1 : r2 ) = X( i ) % Val
  END DO
END PROCEDURE scalarCOPYvector

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYintrinsic
  CALL SHALLOWCOPY( Y=Y%Val, X=X )
  CALL COPY( Y=Y%Val, X=X )
END PROCEDURE scalarCOPYintrinsic

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicCOPYscalar
  CALL SHALLOWCOPY( Y=Y, X=X%Val )
  CALL COPY( Y=Y, X=X%Val )
END PROCEDURE intrinsicCOPYscalar

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
    CALL SWAP( X=X( i ) % Val, Y=Y( i ) % Val )
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
!
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYscalar
  CALL AXPY( X=X%Val, Y=Y%Val, A=A )
END PROCEDURE scalarAXPYscalar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorAXPYvector
  INTEGER( I4B ) :: i
  DO i = 1, SIZE( Y )
    CALL AXPY( Y=Y( i )%Val, A=A, X=X( i )%Val )
  END DO
END PROCEDURE vectorAXPYvector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYintrinsic
  CALL AXPY( Y=Y%Val, A=A, X=X )
END PROCEDURE scalarAXPYintrinsic

END SUBMODULE BLAS1