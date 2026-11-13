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

SUBMODULE(IntVector_EnquireMethod) Methods
USE IntegerUtility, ONLY: OPERATOR(.IN.)
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       intVec_isAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsAllocated
IF (ALLOCATED(obj%val)) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_IsAllocated

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_In1
ans = obj1%val(1:obj1%tsize) .IN. obj2%val(1:obj2%tsize)
END PROCEDURE obj_In1

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_In2a
ans = ANY(a .EQ. obj%val(1:obj%tsize))
END PROCEDURE obj_In2a

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_In2b
ans = ANY(a .EQ. obj%val(1:obj%tsize))
END PROCEDURE obj_In2b

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_In2c
ans = ANY(a .EQ. obj%val(1:obj%tsize))
END PROCEDURE obj_In2c

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_In2d
ans = ANY(a .EQ. obj%val(1:obj%tsize))
END PROCEDURE obj_In2d

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsPresent1
INTEGER(I4B) :: i
ans = .FALSE.
DO i = 1, obj%tsize
  !SIZE(obj%val)
  IF (obj%val(i) .EQ. VALUE) THEN
    ans = .TRUE.
    EXIT
  END IF
END DO
END PROCEDURE obj_IsPresent1

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsPresent2
INTEGER(I4B) :: i, m, j
LOGICAL(LGT), ALLOCATABLE :: search(:)
m = SIZE(VALUE)
ALLOCATE (ans(m), search(m))
search = .TRUE.
ans = .FALSE.
DO i = 1, obj%tsize
  !SIZE(obj%val)
  DO j = 1, m
    IF (search(j)) THEN
      IF (VALUE(j) .EQ. obj%val(i)) THEN
        search(j) = .FALSE.
        ans(j) = .TRUE.
      END IF
    END IF
  END DO
END DO
END PROCEDURE obj_IsPresent2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
