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

SUBMODULE(RealVector_AssignMethods) Methods
USE RealVector_ShallowCopyMethods, ONLY: ShallowCopy
USE F95_BLAS, ONLY: COPY
USE RealVector_ConstructorMethods, ONLY: SetTotalDimension, &
                                         Size

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign1
CALL ShallowCopy(Y=lhs, X=rhs)
CALL SetTotalDimension(lhs, 1_I4B)
CALL COPY(Y=lhs%val, X=rhs%val)
END PROCEDURE obj_assign1

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign2
INTEGER(I4B) :: m, ii, aa
CALL ShallowCopy(Y=lhs, X=rhs)
CALL SetTotalDimension(lhs, 1_I4B)
m = 0
DO ii = 1, SIZE(rhs)
  aa = m + 1
  m = m + SIZE(rhs(ii))
  CALL COPY(Y=lhs%val(aa:m), X=rhs(ii)%val)
END DO
END PROCEDURE obj_assign2

!----------------------------------------------------------------------------
!                                                                     Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign3a
CALL ShallowCopy(Y=lhs, X=rhs)
CALL SetTotalDimension(lhs, 1_I4B)
lhs%val = rhs
END PROCEDURE obj_assign3a

MODULE PROCEDURE obj_assign3b
CALL ShallowCopy(Y=lhs, X=rhs)
CALL SetTotalDimension(lhs, 1_I4B)
lhs%val = rhs
END PROCEDURE obj_assign3b

!----------------------------------------------------------------------------
!                                                                     Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign4a
#ifdef USE_Real64
lhs = rhs%val
#else
CALL ShallowCopy(Y=lhs, X=rhs)
CALL COPY(Y=lhs, X=rhs%val)
#endif
END PROCEDURE obj_assign4a
MODULE PROCEDURE obj_assign4b
CALL ShallowCopy(Y=lhs, X=rhs)
#ifdef USE_Real64
CALL COPY(Y=lhs, X=rhs%val)
#else
lhs = rhs%val
#endif
END PROCEDURE obj_assign4b

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign5a
INTEGER(I4B) :: m, ii, aa
CALL ShallowCopy(Y=lhs, X=rhs)
m = 0
DO ii = 1, SIZE(rhs)
  aa = m + 1
  m = m + SIZE(rhs(ii))
#ifndef USE_Real64
  CALL COPY(Y=lhs(aa:m), X=rhs(ii)%val)
#else
  lhs(aa:m) = rhs(ii)%val(:)
#endif
END DO
END PROCEDURE obj_assign5a

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign5b
INTEGER(I4B) :: m, ii, aa
  !!
CALL ShallowCopy(Y=lhs, X=rhs)
m = 0
DO ii = 1, SIZE(rhs)
  aa = m + 1
  m = m + SIZE(rhs(ii))
#ifdef USE_Real64
  CALL COPY(Y=lhs(aa:m), X=rhs(ii)%val)
#else
  lhs(aa:m) = rhs(ii)%val(:)
#endif
END DO
END PROCEDURE obj_assign5b

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign6
lhs = REAL(rhs, DFP)
END PROCEDURE obj_assign6

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign7
REAL(DFP), ALLOCATABLE :: dummy(:)
dummy = rhs
lhs = INT(dummy, I4B)
IF (ALLOCATED(dummy)) DEALLOCATE (dummy)
END PROCEDURE obj_assign7

!----------------------------------------------------------------------------
!                                                                    Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign8
REAL(DFP), ALLOCATABLE :: dummy(:)
dummy = rhs
lhs = INT(dummy, I4B)
IF (ALLOCATED(dummy)) DEALLOCATE (dummy)
END PROCEDURE obj_assign8

END SUBMODULE Methods
