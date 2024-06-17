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

MODULE SplitUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: SPLIT

!----------------------------------------------------------------------------
!                                                                     SPLIT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE SPLIT
  MODULE PURE FUNCTION split_Int8(x, section) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT8), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int8

  MODULE PURE FUNCTION split_Int16(x, section) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT16), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int16

  MODULE PURE FUNCTION split_Int32(x, section) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT32), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int32

  MODULE PURE FUNCTION split_Int64(x, section) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    !! Input array
    INTEGER(I4B), INTENT(IN) :: section
    !! Array section to return
    INTEGER(INT64), ALLOCATABLE :: Ans(:)
  END FUNCTION split_Int64
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`,
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE SPLIT
  MODULE PURE FUNCTION split_Real32(x, section) RESULT(Ans)
    REAL(REAL32), DIMENSION(:), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    REAL(REAL32), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_Real32

  MODULE PURE FUNCTION split_Real64(x, section) RESULT(Ans)
    REAL(REAL64), DIMENSION(:), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_Real64
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!                                                                    SPLIT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`,
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE SPLIT
  MODULE PURE FUNCTION split_char(x, section) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    CHARACTER(:), ALLOCATABLE :: Ans
  END FUNCTION split_char
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SplitUtility
