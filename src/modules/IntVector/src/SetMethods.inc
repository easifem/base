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

PUBLIC :: setTotalDimension
PUBLIC :: set
PUBLIC :: RemoveDuplicates
PUBLIC :: Repeat

!----------------------------------------------------------------------------
!                                             setTotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This subroutine set the total dimension (rank) of an array
!
!# Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE
  MODULE PURE SUBROUTINE IntVec_setTotalDimension(obj, tDimension)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE IntVec_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE IntVec_setTotalDimension
END INTERFACE setTotalDimension

!----------------------------------------------------------------------------
!                                                        setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: set the value in IntVector

INTERFACE
  MODULE PURE SUBROUTINE intVec_set1(obj, Indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx(:)
    INTEGER(I4B), INTENT(IN) :: VALUE(:)
  END SUBROUTINE intVec_set1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE intVec_set1
END INTERFACE set

!----------------------------------------------------------------------------
!                                                        setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: set the value in IntVector

INTERFACE
  MODULE PURE SUBROUTINE intVec_set2(obj, Indx, VALUE)
    CLASS(IntVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Indx
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE intVec_set2
END INTERFACE

INTERFACE set
  MODULE PROCEDURE intVec_set2
END INTERFACE set

!----------------------------------------------------------------------------
!                                                 RemoveDuplicates@setMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE IntVec_RemoveDuplicates_1(obj)
    CLASS(IntVector_), INTENT(INOUT) :: obj
  END SUBROUTINE IntVec_RemoveDuplicates_1
END INTERFACE

INTERFACE RemoveDuplicates
  MODULE PROCEDURE IntVec_RemoveDuplicates_1
END INTERFACE RemoveDuplicates

!----------------------------------------------------------------------------
!                                                           Repeat@setMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION IntVec_Repeat_1(obj, rtimes) RESULT(Ans)
    TYPE(IntVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: rtimes
    INTEGER(I4B) :: Ans(SIZE(obj%Val) * rtimes)
  END FUNCTION IntVec_Repeat_1
END INTERFACE

INTERFACE Repeat
  MODULE PROCEDURE IntVec_Repeat_1
END INTERFACE Repeat
