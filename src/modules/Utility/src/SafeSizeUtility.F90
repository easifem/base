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

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Size of an allocatable vector that may not be allocated.
!
! Description:
!   This module contains the generic function `SafeSize`. It works like
!   the intrinsic `SIZE` for an allocatable vector, but returns 0 when the
!   vector is not allocated, instead of being an error.
!
!   `SafeSize` is available for allocatable vectors of
!
!   - integers (`INT8`, `INT16`, `INT32`, `INT64`),
!   - reals (`REAL32`, `REAL64`),
!   - logicals (`LGT`).

MODULE SafeSizeUtility
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE GlobalData, ONLY: I4B
IMPLICIT NONE

PRIVATE

PUBLIC :: SafeSize

!----------------------------------------------------------------------------
!                                                                 SafeSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Size of an allocatable integer vector, or 0 if not allocated.
!
! Description:
!   Returns `SIZE(value)` if `value` is allocated, and 0 otherwise. It is
!   available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   An allocated vector of size 0 also gives 0.
!
! Arguments:
!   value : INTEGER, ALLOCATABLE, rank(1) - input vector, may be
!     unallocated
!
! Returns:
!   ans : INTEGER(I4B) - `SIZE(value)` if allocated, otherwise 0
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{SIZE}(\mathrm{value}), & \text{if value is allocated} \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE SafeSizeUtility, ONLY: SafeSize
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B), ALLOCATABLE :: a(:)
!
! ! not allocated
! CALL OK(SafeSize(a) .EQ. 0_I4B, "not allocated")
!
! ! allocated with size 0
! ALLOCATE (a(0))
! CALL OK(SafeSize(a) .EQ. 0_I4B, "allocated with size 0")
! DEALLOCATE (a)
!
! ! allocated with size 5
! ALLOCATE (a(5))
! a = 1_I4B
! CALL OK(SafeSize(a) .EQ. 5_I4B, "allocated with size 5")
! CALL OK(SafeSize(a) .EQ. SIZE(a), "same as SIZE when allocated")
!
! ! after deallocation
! DEALLOCATE (a)
! CALL OK(SafeSize(a) .EQ. 0_I4B, "after deallocation")
!
! END PROGRAM main
!```

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

  MODULE PURE FUNCTION SafeSize6(VALUE) RESULT(ans)
    INTEGER(INT64), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize6
END INTERFACE SafeSize

!----------------------------------------------------------------------------
!                                                                 SafeSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Size of an allocatable real vector, or 0 if not allocated.
!
! Description:
!   Returns `SIZE(value)` if `value` is allocated, and 0 otherwise. It is
!   available for `REAL32` and `REAL64`.
!
!   An allocated vector of size 0 also gives 0. The lower bound of
!   `value` does not matter.
!
! Arguments:
!   value : REAL, ALLOCATABLE, rank(1) - input vector, may be
!     unallocated
!
! Returns:
!   ans : INTEGER(I4B) - `SIZE(value)` if allocated, otherwise 0
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{SIZE}(\mathrm{value}), & \text{if value is allocated} \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE SafeSizeUtility, ONLY: SafeSize
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: a(:)
!
! ! not allocated
! CALL OK(SafeSize(a) .EQ. 0_I4B, "not allocated")
!
! ! allocated with size 0
! ALLOCATE (a(0))
! CALL OK(SafeSize(a) .EQ. 0_I4B, "allocated with size 0")
! DEALLOCATE (a)
!
! ! allocated with size 3, lower bound is not 1
! ALLOCATE (a(-1:1))
! a = 0.5_DFP
! CALL OK(SafeSize(a) .EQ. 3_I4B, "allocated with size 3")
!
! ! automatic reallocation on assignment
! a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
! CALL OK(SafeSize(a) .EQ. 4_I4B, "size after reallocation")
!
! DEALLOCATE (a)
! CALL OK(SafeSize(a) .EQ. 0_I4B, "after deallocation")
!
! END PROGRAM main
!```

INTERFACE SafeSize
  MODULE PURE FUNCTION SafeSize4(VALUE) RESULT(ans)
    REAL(REAL32), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize4

  MODULE PURE FUNCTION SafeSize5(VALUE) RESULT(ans)
    REAL(REAL64), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize5
END INTERFACE SafeSize

!----------------------------------------------------------------------------
!                                                                 SafeSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Size of an allocatable logical vector, or 0 if not allocated.
!
! Description:
!   Returns `SIZE(value)` if `value` is allocated, and 0 otherwise.
!
! Arguments:
!   value : LOGICAL(LGT), ALLOCATABLE, rank(1) - input vector, may be
!     unallocated
!
! Returns:
!   ans : INTEGER(I4B) - `SIZE(value)` if allocated, otherwise 0
!
! Formula:
!   $$\mathrm{ans} = \begin{cases}
!     \mathrm{SIZE}(\mathrm{value}), & \text{if value is allocated} \\
!     0, & \text{otherwise}
!   \end{cases}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE SafeSizeUtility, ONLY: SafeSize
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! LOGICAL(LGT), ALLOCATABLE :: a(:)
!
! ! not allocated
! CALL OK(SafeSize(a) .EQ. 0_I4B, "not allocated")
!
! ! allocated with size 3
! ALLOCATE (a(3))
! a = .TRUE.
! CALL OK(SafeSize(a) .EQ. 3_I4B, "allocated with size 3")
!
! END PROGRAM main
!```

INTERFACE SafeSize
  MODULE PURE FUNCTION SafeSize7(VALUE) RESULT(ans)
    LOGICAL(LGT), ALLOCATABLE, INTENT(IN) :: VALUE(:)
    INTEGER(I4B) :: ans
  END FUNCTION SafeSize7
END INTERFACE SafeSize

END MODULE SafeSizeUtility
