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

MODULE ArangeUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64
IMPLICIT NONE
PUBLIC :: Arange

!----------------------------------------------------------------------------
!                                                            Arange@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: Returns a range of reals.
!
! Returns a range of REAL64. Starting value is istart, iend,
! and increment.

INTERFACE Arange
  MODULE PURE FUNCTION Arange_Real64(istart, iend, increment) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: istart
    !! Start value of the array
    REAL(REAL64), INTENT(IN) :: iend
    !! End value of the array
    REAL(REAL64), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION Arange_Real64
END INTERFACE Arange

!----------------------------------------------------------------------------
!                                                            Arange@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: Returns a vector of reals
!
! Returns a range of REAL32. Starting value is istart, iend,
! and increment.
!
! Example
!
!```fortran
! PROGRAM main
! USE ArangeUtility, ONLY: Arange
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
! CALL Display(arange(1.0, 10.0, 3.0), "ans = ", &
!              full=.TRUE., orient="row")
! CALL Display(arange(1, 10, 2), "ans = ", &
!              full=.TRUE., orient="row")
! END PROGRAM main
! !               ans =
! ! ----------------------------------
! !  1.0000   4.0000   7.0000  10.0000
! !     ans =
! ! -------------
! ! 1  3  5  7  9
!```

INTERFACE Arange
  MODULE PURE FUNCTION Arange_Real32(istart, iend, increment) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: istart
    !! Start value of the array
    REAL(REAL32), INTENT(IN) :: iend
    !! End value of the array
    REAL(REAL32), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(REAL32), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION Arange_Real32
END INTERFACE Arange

!----------------------------------------------------------------------------
!                                                           Arange@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: Returns a vector of integer
!
! Returns an array of integers given `istart`,  `iend`,  and
! `increment` values.
!
! Default value of increment is 1.
!
! Calling example:
!
!```fortran
! Arange(1,10,1)
! Arange(1,10,2)
!```
!
! Example
!
!```fortran
! PROGRAM main
! USE ArangeUtility, ONLY: Arange
! USE Display_Method, ONLY: Display
! IMPLICIT NONE
! CALL Display(arange(1.0, 10.0, 3.0), "ans = ", &
!              full=.TRUE., orient="row")
! CALL Display(arange(1, 10, 2), "ans = ", &
!              full=.TRUE., orient="row")
! END PROGRAM main
! !               ans =
! ! ----------------------------------
! !  1.0000   4.0000   7.0000  10.0000
! !     ans =
! ! -------------
! ! 1  3  5  7  9
!```

INTERFACE Arange
  MODULE PURE FUNCTION Arange_Int8(istart, iend, increment) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: istart
    INTEGER(INT8), INTENT(IN) :: iend
    INTEGER(INT8), INTENT(IN), OPTIONAL :: increment
    INTEGER(INT8), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION Arange_Int8

  MODULE PURE FUNCTION Arange_Int16(istart, iend, increment) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: istart
    INTEGER(INT16), INTENT(IN) :: iend
    INTEGER(INT16), INTENT(IN), OPTIONAL :: increment
    INTEGER(INT16), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION Arange_Int16

  MODULE PURE FUNCTION Arange_Int32(istart, iend, increment) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: istart
    INTEGER(INT32), INTENT(IN) :: iend
    INTEGER(INT32), INTENT(IN), OPTIONAL :: increment
    INTEGER(INT32), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION Arange_Int32

  MODULE PURE FUNCTION Arange_Int64(istart, iend, increment) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: istart
    INTEGER(INT64), INTENT(IN) :: iend
    INTEGER(INT64), INTENT(IN), OPTIONAL :: increment
    INTEGER(INT64), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION Arange_Int64
END INTERFACE Arange

END MODULE ArangeUtility
