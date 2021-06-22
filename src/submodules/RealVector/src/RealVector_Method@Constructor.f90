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
! date: 25 Feb 2021
! summary: This module contains constructor methods of [[RealVector_]]

SUBMODULE ( RealVector_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  IF( ALLOCATED( Obj%Val ) ) THEN
    Ans(1) = SIZE( Obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  IF( ALLOCATED( Obj%Val ) ) THEN
    Ans = SIZE( Obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  CALL Reallocate( Obj%Val, Dims )
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj%Val ) ) DEALLOCATE( Obj%Val )
END PROCEDURE deallocate_data

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  CALL AllocateData( Obj, tSize )
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_vector
  INTEGER( I4B ) :: n, i

  n = SIZE( tSize )

  IF( ALLOCATED( Obj ) ) THEN
    IF( SIZE( Obj ) .NE. n ) THEN
      DEALLOCATE( Obj )
      ALLOCATE( Obj( n ) )
    END IF
  ELSE
    ALLOCATE( Obj( n ) )
  END IF

  DO i = 1, n
    CALL AllocateData( Obj( i ), tSize( i ) )
  END DO

END PROCEDURE initiate_obj_vector

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_ab
  IF( ALLOCATED( Obj%Val ) ) DEALLOCATE( Obj%Val )
  ALLOCATE( Obj%Val( a:b ) )
  Obj%Val = 0.0_DFP
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE initiate_obj_ab

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE random_number_obj
  CALL Initiate( Obj=Obj, tSize=tSize )
  CALL RANDOM_NUMBER( Obj%Val )
END PROCEDURE random_number_obj

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE random_number_obj_vec
  INTEGER( I4B ) :: ii, n
  n = SIZE( tSize )
  IF( ALLOCATED( Obj ) ) THEN
    IF( SIZE( Obj ) .NE. n ) THEN
      DEALLOCATE( Obj )
      ALLOCATE( Obj( n ) )
    END IF
  ELSE
    ALLOCATE( Obj( n ) )
  END IF
  DO ii = 1, n
    CALL Initiate( Obj=Obj(ii), tSize=tSize(ii) )
    CALL RANDOM_NUMBER( Obj(ii)%Val )
  END DO
END PROCEDURE random_number_obj_vec

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL AllocateData( Obj, tSize )
END PROCEDURE Constructor1

MODULE PROCEDURE Constructor2
  Obj%Val = Val
END PROCEDURE Constructor2

MODULE PROCEDURE Constructor3
  Obj%Val = Val
END PROCEDURE Constructor3

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  CALL AllocateData( Obj, tSize )
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( Obj )
  Obj%Val = Val
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_3
  ALLOCATE( Obj )
  Obj%Val = Val
END PROCEDURE Constructor_3

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
END PROCEDURE intrinsicSHALLOWCOPYscalar

END SUBMODULE Constructor