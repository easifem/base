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

MODULE HeadUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: HEAD

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Returns the first element of array `x`.

INTERFACE HEAD
  MODULE PURE FUNCTION head_Int8(x) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: Ans
  END FUNCTION head_Int8

  MODULE PURE FUNCTION head_Int16(x) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: Ans
  END FUNCTION head_Int16

  MODULE PURE FUNCTION head_Int32(x) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: Ans
  END FUNCTION head_Int32

  MODULE PURE FUNCTION head_Int64(x) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: Ans
  END FUNCTION head_Int64
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE HEAD
  MODULE PURE FUNCTION head_Real32(x) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: Ans
  END FUNCTION head_Real32

  MODULE PURE FUNCTION head_Real64(x) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: Ans
  END FUNCTION head_Real64
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                                      Head
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE HEAD
  MODULE PURE FUNCTION head_char(x) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(1) :: Ans
  END FUNCTION
END INTERFACE HEAD

END MODULE HeadUtility
