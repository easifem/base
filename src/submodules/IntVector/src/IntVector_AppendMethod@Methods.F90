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

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: Append and horizontal concat methods of [[IntVector_]]
!
! Changes:
! - 2026-11-05: Use `tsize` and `capacity` of [[IntVector_]]

SUBMODULE(IntVector_AppendMethod) Methods
USE AppendUtility, ONLY: Expand
USE BaseType, ONLY: math => TypeMathOpt
USE IntVector_ConstructorMethod, ONLY: IntVectorInitiate => Initiate
IMPLICIT NONE

! Number of entries added when [[obj_Append1]] finds `obj%val` full
#define _CHUNK_SIZE_ 10_I4B

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append1
CALL Expand(vec=obj%val, n=obj%tsize, chunk_size=_CHUNK_SIZE_, &
            val=VALUE, finished=math%no)
obj%capacity = SIZE(obj%val)
END PROCEDURE obj_Append1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append2
INTEGER(I4B) :: ii, value_size

value_size = SIZE(VALUE)

! A chunk of value_size makes room for all entries in one reallocation
DO ii = 1, value_size
  CALL Expand(vec=obj%val, n=obj%tsize, chunk_size=value_size, &
              val=VALUE(ii), finished=math%no)
END DO

IF (obj%tsize .GT. math%zero_i) THEN
  obj%capacity = SIZE(obj%val)
END IF
END PROCEDURE obj_Append2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append3
IF (anotherObj%tsize .GT. 0) THEN
  CALL Append(obj, anotherObj%val(1:anotherObj%tsize))
END IF
END PROCEDURE obj_Append3

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_H_CONCAT1
INTEGER(I4B) :: ii, s1

s1 = SIZE(vec1)
DO ii = 1, s1
  ans(ii) = vec1(ii)
END DO

DO ii = 1, SIZE(vec2)
  ans(s1 + ii) = vec2(ii)
END DO
END PROCEDURE obj_H_CONCAT1

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_H_CONCAT2
INTEGER(I4B) :: ii

CALL IntVectorInitiate(ans, obj1%tsize + obj2%tsize)

DO ii = 1, obj1%tsize
  ans%val(ii) = obj1%val(ii)
END DO

DO ii = 1, obj2%tsize
  ans%val(obj1%tsize + ii) = obj2%val(ii)
END DO
END PROCEDURE obj_H_CONCAT2

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_H_CONCAT3
INTEGER(I4B) :: ii, s1

s1 = SIZE(vec1)
CALL IntVectorInitiate(ans, s1 + obj2%tsize)

DO ii = 1, s1
  ans%val(ii) = vec1(ii)
END DO

DO ii = 1, obj2%tsize
  ans%val(s1 + ii) = obj2%val(ii)
END DO
END PROCEDURE obj_H_CONCAT3

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_H_CONCAT4
INTEGER(I4B) :: ii, s2

s2 = SIZE(vec2)
CALL IntVectorInitiate(ans, s2 + obj1%tsize)

DO ii = 1, obj1%tsize
  ans%val(ii) = obj1%val(ii)
END DO

DO ii = 1, s2
  ans%val(obj1%tsize + ii) = vec2(ii)
END DO

END PROCEDURE obj_H_CONCAT4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
