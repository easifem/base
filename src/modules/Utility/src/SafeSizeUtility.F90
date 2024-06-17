! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE SafeSizeUtility
USE GlobalData, ONLY: INT8, INT16, INT32, REAL32, REAL64, I4B
IMPLICIT NONE

PRIVATE

PUBLIC :: SafeSize

!----------------------------------------------------------------------------
!                                                                 SafeSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-08
! summary:  Like size but safe for unallocatable

INTERFACE SafeSize
  MODULE PURE FUNCTION SafeSize1(VALUE) RESULT(ans)
    INTEGER(INT8), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize1

  MODULE PURE FUNCTION SafeSize2(VALUE) RESULT(ans)
    INTEGER(INT16), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize2

  MODULE PURE FUNCTION SafeSize3(VALUE) RESULT(ans)
    INTEGER(INT32), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize3

  MODULE PURE FUNCTION SafeSize4(VALUE) RESULT(ans)
    REAL(REAL32), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize4

  MODULE PURE FUNCTION SafeSize5(VALUE) RESULT(ans)
    REAL(REAL64), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize5

END INTERFACE SafeSize

END MODULE SafeSizeUtility
