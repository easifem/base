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

MODULE TailUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: TAIL
PUBLIC :: LAST

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

INTERFACE TAIL
  MODULE PURE FUNCTION tail_Int8(x) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int8

  MODULE PURE FUNCTION tail_Int16(x) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int16

  MODULE PURE FUNCTION tail_Int32(x) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int32

  MODULE PURE FUNCTION tail_Int64(x) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Int64
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

INTERFACE TAIL
  MODULE PURE FUNCTION tail_Real32(x) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Real32

  MODULE PURE FUNCTION tail_Real64(x) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans(SIZE(x) - 1)
  END FUNCTION tail_Real64
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

INTERFACE TAIL
  MODULE PURE FUNCTION tail_char(x) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(LEN(x) - 1) :: ans
  END FUNCTION tail_char
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Last
!----------------------------------------------------------------------------

INTERFACE LAST
  MODULE PURE FUNCTION last_Int8(x) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: ans
  END FUNCTION last_Int8

  MODULE PURE FUNCTION last_Int16(x) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: ans
  END FUNCTION last_Int16

  MODULE PURE FUNCTION last_Int32(x) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: ans
  END FUNCTION last_Int32

  MODULE PURE FUNCTION last_Int64(x) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: ans
  END FUNCTION last_Int64
END INTERFACE LAST

!----------------------------------------------------------------------------
!                                                                      Last
!----------------------------------------------------------------------------

INTERFACE LAST
  MODULE PURE FUNCTION last_Real32(x) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans
  END FUNCTION last_Real32

  MODULE PURE FUNCTION last_Real64(x) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans
  END FUNCTION last_Real64
END INTERFACE LAST

!----------------------------------------------------------------------------
!                                                                      Last
!----------------------------------------------------------------------------

INTERFACE LAST
  MODULE PURE FUNCTION last_char(x) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(1) :: ans
  END FUNCTION last_char
END INTERFACE LAST

END MODULE TailUtility
