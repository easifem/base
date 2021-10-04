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

SUBMODULE ( RealVector_Method) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Shape
  IF( ALLOCATED( obj%Val ) ) THEN
    Ans(1) = SIZE( obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE realVec_Shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Size
  IF( ALLOCATED( obj%Val ) ) THEN
    Ans = SIZE( obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE realVec_Size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVec_getTotalDimension
  ans = obj%tDimension
END PROCEDURE RealVec_getTotalDimension

!----------------------------------------------------------------------------
!                                                        setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVec_setTotalDimension
  obj%tDimension = tDimension
END PROCEDURE RealVec_setTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_AllocateData
  CALL Reallocate( obj%Val, Dims )
  CALL setTotalDimension( obj, 1_I4B )
END PROCEDURE realVec_AllocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_DeallocateData
  IF( ALLOCATED( obj%Val ) ) DEALLOCATE( obj%Val )
END PROCEDURE realVec_DeallocateData

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate1
  CALL AllocateData( obj, tSize )
END PROCEDURE realVec_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate2
  INTEGER( I4B ) :: n, i
  n = SIZE( tSize )
  IF( ALLOCATED( obj ) ) THEN
    IF( SIZE( obj ) .NE. n ) THEN
      DEALLOCATE( obj )
      ALLOCATE( obj( n ) )
    END IF
  ELSE
    ALLOCATE( obj( n ) )
  END IF
  DO i = 1, n
    CALL AllocateData( obj( i ), tSize( i ) )
  END DO
END PROCEDURE realVec_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Initiate3
  IF( ALLOCATED( obj%Val ) ) DEALLOCATE( obj%Val )
  ALLOCATE( obj%Val( a:b ) )
  obj%Val = 0.0_DFP
  CALL setTotalDimension( obj, 1_I4B )
END PROCEDURE realVec_Initiate3

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Random_Number1
  CALL Initiate( obj=obj, tSize=tSize )
  CALL RANDOM_NUMBER( obj%Val )
END PROCEDURE realVec_Random_Number1

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Random_Number2
  INTEGER( I4B ) :: ii, n
  n = SIZE( tSize )
  IF( ALLOCATED( obj ) ) THEN
    IF( SIZE( obj ) .NE. n ) THEN
      DEALLOCATE( obj )
      ALLOCATE( obj( n ) )
    END IF
  ELSE
    ALLOCATE( obj( n ) )
  END IF
  DO ii = 1, n
    CALL Initiate( obj=obj(ii), tSize=tSize(ii) )
    CALL RANDOM_NUMBER( obj(ii)%Val )
  END DO
END PROCEDURE realVec_Random_Number2

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor1
  CALL AllocateData( obj, tSize )
END PROCEDURE realVec_Constructor1

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor2
  CALL AllocateData( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=REAL(val, DFP) )
END PROCEDURE realVec_Constructor2

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor3
  CALL AllocateData( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=val )
END PROCEDURE realVec_Constructor3

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor_1
  ALLOCATE( obj )
  CALL AllocateData( obj, tSize )
END PROCEDURE realVec_Constructor_1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor_2
  ALLOCATE( obj )
  CALL AllocateData( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=REAL(val, DFP) )
END PROCEDURE realVec_Constructor_2

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_Constructor_3
  ALLOCATE( obj )
    CALL AllocateData( obj, SIZE( val ))
  CALL COPY( Y=obj%val, X=REAL(val, DFP) )
END PROCEDURE realVec_Constructor_3

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY1
  CALL Reallocate( Y, SIZE( X ) )
END PROCEDURE realVec_SHALLOWCOPY1

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY2
  CALL SHALLOWCOPY( Y=Y%Val, X=X%Val )
END PROCEDURE realVec_SHALLOWCOPY2

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY3
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
END PROCEDURE realVec_SHALLOWCOPY3

!----------------------------------------------------------------------------
!                                                               SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY4
  INTEGER( I4B ) :: i, tNodes
  tNodes = 0
  DO i = 1, SIZE( X )
    tNodes = tNodes + SIZE( X( i )%Val )
  END DO
  CALL Reallocate( Y%Val, tNodes )
END PROCEDURE realVec_SHALLOWCOPY4

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY5
  CALL SHALLOWCOPY( Y=Y%Val, X=X )
END PROCEDURE realVec_SHALLOWCOPY5

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY6
  CALL SHALLOWCOPY( Y=Y, X=X%Val )
END PROCEDURE realVec_SHALLOWCOPY6

!----------------------------------------------------------------------------
!                                                                 SHALLOWCOPY
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_SHALLOWCOPY7
  INTEGER( I4B ) :: ii, m
  m = 0
  DO ii = 1, SIZE( X )
    m = m + SIZE( X( ii ) )
  END DO
  CALL Reallocate( Y, m )
END PROCEDURE realVec_SHALLOWCOPY7

!----------------------------------------------------------------------------
!                                                                     Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign1
  CALL SHALLOWCOPY( Y=lhs, X=rhs )
  CALL setTotalDimension( lhs, 1_I4B )
  CALL COPY( Y=lhs%val, X=rhs%val )
END PROCEDURE realVec_assign1

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign2
  INTEGER( I4B ) :: m, ii, aa
  CALL SHALLOWCOPY( Y=lhs, X=rhs )
  CALL setTotalDimension( lhs, 1_I4B )
  m = 0
  DO ii = 1, SIZE( rhs )
    aa = m + 1
    m = m + SIZE( rhs( ii ) )
    CALL COPY( Y=lhs%val( aa:m ), X=rhs(ii)%val )
  END DO
END PROCEDURE realVec_assign2

!----------------------------------------------------------------------------
!                                                                     Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign3
  CALL SHALLOWCOPY( Y=lhs, X=rhs )
  CALL setTotalDimension( lhs, 1_I4B )
  CALL COPY( Y=lhs%val, X=rhs )
END PROCEDURE realVec_assign3

!----------------------------------------------------------------------------
!                                                                     Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign4
  CALL SHALLOWCOPY( Y=lhs, X=rhs )
  CALL COPY( Y=lhs, X=rhs%val )
END PROCEDURE realVec_assign4

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign5
  INTEGER( I4B ) :: m, ii, aa
  CALL SHALLOWCOPY( Y=lhs, X=rhs )
  m = 0
  DO ii = 1, SIZE( rhs )
    aa = m + 1
    m = m + SIZE( rhs( ii ) )
    CALL COPY( Y=lhs( aa:m ), X=rhs(ii)%val )
  END DO
END PROCEDURE realVec_assign5

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign6
  lhs = REAL( rhs, DFP )
END PROCEDURE realVec_assign6

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign7
  REAL( DFP ), ALLOCATABLE :: dummy( : )
  dummy = rhs
  lhs = INT( dummy, I4B )
  IF( ALLOCATED( dummy ) ) DEALLOCATE( dummy )
END PROCEDURE realVec_assign7

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE realVec_assign8
  REAL( DFP ), ALLOCATABLE :: dummy( : )
  dummy = rhs
  lhs = INT( dummy, I4B )
  IF( ALLOCATED( dummy ) ) DEALLOCATE( dummy )
END PROCEDURE realVec_assign8

END SUBMODULE Constructor