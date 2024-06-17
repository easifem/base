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
USE GlobalData
IMPLICIT NONE
PUBLIC :: arange

!----------------------------------------------------------------------------
!                                                            arange@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE arange
  MODULE PURE FUNCTION arange_Real64(istart, iend, increment) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: istart
    !! Start value of the array
    REAL(REAL64), INTENT(IN) :: iend
    !! End value of the array
    REAL(REAL64), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Real64
END INTERFACE arange

!----------------------------------------------------------------------------
!                                                            arange@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE arange
  MODULE PURE FUNCTION arange_Real32(istart, iend, increment) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: istart
    !! Start value of the array
    REAL(REAL32), INTENT(IN) :: iend
    !! End value of the array
    REAL(REAL32), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(REAL32), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Real32
END INTERFACE arange

!----------------------------------------------------------------------------
!                                                           arange@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of integer
!
!# Introduction
! Returns an array of integers given `istart`,  `iend`,  and
! `increment` values. Default value of increment is 1
! This function belongs to the generic function [[Utility:arange]]
!
!### Usage
!
!```fortran
!        arange(1,10,1)
! arange(1,10,2)
!```

INTERFACE arange
  MODULE PURE FUNCTION arange_Int8(istart, iend, increment) RESULT(Ans)
    INTEGER(Int8), INTENT(IN) :: istart
    INTEGER(Int8), INTENT(IN) :: iend
    INTEGER(Int8), INTENT(IN), OPTIONAL :: increment
    INTEGER(Int8), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Int8

  MODULE PURE FUNCTION arange_Int16(istart, iend, increment) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: istart
    INTEGER(Int16), INTENT(IN) :: iend
    INTEGER(Int16), INTENT(IN), OPTIONAL :: increment
    INTEGER(Int16), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Int16

  MODULE PURE FUNCTION arange_Int32(istart, iend, increment) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: istart
    INTEGER(Int32), INTENT(IN) :: iend
    INTEGER(Int32), INTENT(IN), OPTIONAL :: increment
    INTEGER(Int32), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Int32

  MODULE PURE FUNCTION arange_Int64(istart, iend, increment) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: istart
    INTEGER(Int64), INTENT(IN) :: iend
    INTEGER(Int64), INTENT(IN), OPTIONAL :: increment
    INTEGER(Int64), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Int64
END INTERFACE arange

END MODULE ArangeUtility
