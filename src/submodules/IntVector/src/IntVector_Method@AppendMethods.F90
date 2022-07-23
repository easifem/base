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
! date:         25 Feb 2021
! summary:         This submodule implements set methods of [[IntVector_]]

SUBMODULE(IntVector_Method) AppendMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Append_1
  CALL Append(obj%Val, Value)
END PROCEDURE IntVec_Append_1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Append_2
  CALL Append(obj%Val, Value)
END PROCEDURE IntVec_Append_2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_Append_3
  CALL Append(obj%Val, Anotherobj%Val)
END PROCEDURE IntVec_Append_3

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_1
  INTEGER(I4B) :: s1, s2
  s1 = SIZe(vec1)
  s2 = SIZe(vec2)
  ans(1:s1) = vec1(:)
  ans(s1 + 1:) = vec2(:)
END PROCEDURE IntVec_H_CONCAT_1

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_2
  INTEGER(I4B) :: s1, s2
  s1 = SIZE(obj1)
  s2 = SIZE(obj2)
  CALL Initiate(ans, s1 + s2)
  ans%val(1:s1) = obj1%val(:)
  ans%val(s1 + 1:) = obj2%val(:)
END PROCEDURE IntVec_H_CONCAT_2

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_3
  INTEGER(I4B) :: s1, s2
  s1 = SIZE(vec1)
  s2 = SIZE(obj2)
  CALL Initiate(ans, s1 + s2)
  ans%val(1:s1) = vec1(:)
  ans%val(s1 + 1:) = obj2%val(:)
END PROCEDURE IntVec_H_CONCAT_3

!----------------------------------------------------------------------------
!                                                                 H_CONCAT
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_H_CONCAT_4
  INTEGER(I4B) :: s1, s2
  s1 = SIZE(obj1)
  s2 = SIZE(vec2)
  CALL Initiate(ans, s1 + s2)
  ans%val(1:s1) = obj1%val(:)
  ans%val(s1 + 1:) = vec2(:)
END PROCEDURE IntVec_H_CONCAT_4

END SUBMODULE AppendMethods
