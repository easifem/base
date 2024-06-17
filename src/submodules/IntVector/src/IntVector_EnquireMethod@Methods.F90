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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       intVec_isAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_isAllocated
IF (ALLOCATED(obj%Val)) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE intVec_isAllocated

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_in1
ans = obj1%val.in.obj2%val
END PROCEDURE intVec_in1

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_in2a
ans = ANY(a .EQ. obj%val)
END PROCEDURE intVec_in2a

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_in2b
ans = ANY(a .EQ. obj%val)
END PROCEDURE intVec_in2b

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_in2c
ans = ANY(a .EQ. obj%val)
END PROCEDURE intVec_in2c

!----------------------------------------------------------------------------
!                                                                       IN
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_in2d
ans = ANY(a .EQ. obj%val)
END PROCEDURE intVec_in2d

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_isPresent1
INTEGER(I4B) :: i
Ans = .FALSE.
DO i = 1, SIZE(obj%Val)
  IF (obj%Val(i) .EQ. VALUE) THEN
    Ans = .TRUE.
    EXIT
  END IF
END DO
END PROCEDURE intVec_isPresent1

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_isPresent2
INTEGER(I4B) :: i, m, j
LOGICAL(LGT), ALLOCATABLE :: Search(:)
m = SIZE(VALUE)
ALLOCATE (Ans(m), Search(m))
Search = .TRUE.
Ans = .FALSE.
  !!
DO i = 1, SIZE(obj%Val)
  DO j = 1, m
    IF (Search(j)) THEN
      IF (VALUE(j) .EQ. obj%Val(i)) THEN
        Search(j) = .FALSE.
        Ans(j) = .TRUE.
      END IF
    END IF
  END DO
END DO
  !!
END PROCEDURE intVec_isPresent2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
