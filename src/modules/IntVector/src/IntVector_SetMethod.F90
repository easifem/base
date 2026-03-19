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

MODULE IntVector_SetMethod
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: IntVector_
IMPLICIT NONE

PRIVATE
PUBLIC :: SetTotalDimension
PUBLIC :: Set
PUBLIC :: RemoveDuplicates
PUBLIC :: Repeat

!----------------------------------------------------------------------------
!                                               SetTotalDimension@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This subroutine set the total dimension (rank) of an array
!
!# Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE SetTotalDimension
  MODULE PURE SUBROUTINE obj_setTotalDimension(obj, tDimension)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE obj_setTotalDimension
END INTERFACE SetTotalDimension

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: set the value in IntVector

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(obj, indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: set the value in IntVector

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2(obj, indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set all values IntVector to a constant value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-19
! summary: Set all values IntVector to a constant value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(obj, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                   RemoveDuplicates@Methods
!----------------------------------------------------------------------------

INTERFACE RemoveDuplicates
  MODULE PURE SUBROUTINE obj_RemoveDuplicates1(obj)
    CLASS(IntVector_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_RemoveDuplicates1
END INTERFACE RemoveDuplicates

!----------------------------------------------------------------------------
!                                                             Repeat@Methods
!----------------------------------------------------------------------------

INTERFACE Repeat
  MODULE PURE FUNCTION obj_Repeat1(obj, rtimes) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(I4B) :: ans(SIZE(obj%Val) * rtimes)
  END FUNCTION obj_Repeat1
END INTERFACE Repeat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE IntVector_SetMethod
