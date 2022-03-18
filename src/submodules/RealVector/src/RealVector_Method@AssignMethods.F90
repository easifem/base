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

SUBMODULE(RealVector_Method) AssignMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

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

END SUBMODULE AssignMethods