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

MODULE IntVector_AppendMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP, I4B, LGT
PRIVATE

PUBLIC :: Append
PUBLIC :: H_CONCAT

!----------------------------------------------------------------------------
!                                                         Append@setMethods
!----------------------------------------------------------------------------

INTERFACE Append
  MODULE PURE SUBROUTINE IntVec_Append_1(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE IntVec_Append_1
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                         Append@setMethods
!----------------------------------------------------------------------------

INTERFACE Append
  MODULE PURE SUBROUTINE IntVec_Append_2(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
  END SUBROUTINE IntVec_Append_2
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                         Append@setMethods
!----------------------------------------------------------------------------

INTERFACE Append
  MODULE PURE SUBROUTINE IntVec_Append_3(obj, Anotherobj)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    CLASS(IntVector_), INTENT(IN) :: Anotherobj
  END SUBROUTINE IntVec_Append_3
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Horizontally concat two integer vectors

INTERFACE H_CONCAT
  MODULE PURE FUNCTION IntVec_H_CONCAT_1(vec1, vec2) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: vec1(:)
    INTEGER(I4B), INTENT(IN) :: vec2(:)
    INTEGER(I4B) :: ans(SIZE(vec1) + SIZE(vec2))
  END FUNCTION IntVec_H_CONCAT_1
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         18 June 2021
! summary: Horizontally concat two [[IntVector_]]

INTERFACE H_CONCAT
  MODULE PURE FUNCTION IntVec_H_CONCAT_2(obj1, obj2) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj1
    TYPE(IntVector_), INTENT(IN) :: obj2
    TYPE(IntVector_) :: ans
  END FUNCTION IntVec_H_CONCAT_2
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         18 June 2021
! summary: Horizontally concat a integer vector to a IntVec datatype.

INTERFACE H_CONCAT
  MODULE PURE FUNCTION IntVec_H_CONCAT_3(vec1, obj2) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: vec1(:)
    TYPE(IntVector_), INTENT(IN) :: obj2
    TYPE(IntVector_) :: ans
  END FUNCTION IntVec_H_CONCAT_3
END INTERFACE H_CONCAT

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         18 June 2021
! summary: Horizontally concat a integer vector to a IntVec datatype.

INTERFACE H_CONCAT
  MODULE PURE FUNCTION IntVec_H_CONCAT_4(obj1, vec2) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: vec2(:)
    TYPE(IntVector_), INTENT(IN) :: obj1
    TYPE(IntVector_) :: ans
  END FUNCTION IntVec_H_CONCAT_4
END INTERFACE H_CONCAT

END MODULE IntVector_AppendMethod


