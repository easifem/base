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

MODULE MedianUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: Median
PUBLIC :: ArgMedian

!----------------------------------------------------------------------------
!                                                                  Median
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-27
! summary:  Given three numbers, find their median and sort at the same time

INTERFACE Median
  MODULE PURE SUBROUTINE Median_Int8(this, left, mid, right)
    INTEGER(INT8), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int8

  MODULE PURE SUBROUTINE Median_Int16(this, left, mid, right)
    INTEGER(INT16), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int16

  MODULE PURE SUBROUTINE Median_Int32(this, left, mid, right)
    INTEGER(INT32), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int32

  MODULE PURE SUBROUTINE Median_Int64(this, left, mid, right)
    INTEGER(INT64), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Int64

  MODULE PURE SUBROUTINE Median_Real32(this, left, mid, right)
    REAL(REAL32), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Real32

  MODULE PURE SUBROUTINE Median_Real64(this, left, mid, right)
    REAL(REAL64), INTENT(INOUT) :: this(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE Median_Real64
END INTERFACE Median

!----------------------------------------------------------------------------
!                                                             ArgMedian
!----------------------------------------------------------------------------

INTERFACE ArgMedian
  MODULE PURE SUBROUTINE ArgMedian_Int8(this, indx, left, mid, right)
    INTEGER(INT8), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int8

  MODULE PURE SUBROUTINE ArgMedian_Int16(this, indx, left, mid, right)
    INTEGER(INT16), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int16

  MODULE PURE SUBROUTINE ArgMedian_Int32(this, indx, left, mid, right)
    INTEGER(INT32), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int32

  MODULE PURE SUBROUTINE ArgMedian_Int64(this, indx, left, mid, right)
    INTEGER(INT64), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Int64

  MODULE PURE SUBROUTINE ArgMedian_Real32(this, indx, left, mid, right)
    REAL(REAL32), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Real32

  MODULE PURE SUBROUTINE ArgMedian_Real64(this, indx, left, mid, right)
    REAL(REAL64), INTENT(IN) :: this(:)
    INTEGER(I4B), INTENT(INOUT) :: indx(:)
    INTEGER(I4B), INTENT(IN) :: left
    INTEGER(I4B), INTENT(IN) :: mid
    INTEGER(I4B), INTENT(IN) :: right
  END SUBROUTINE ArgMedian_Real64
END INTERFACE ArgMedian

END MODULE MedianUtility
