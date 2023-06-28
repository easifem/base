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

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

INTERFACE TAIL
  MODULE PURE FUNCTION tail_Int8(x) RESULT(Ans)
    INTEGER(INT8), INTENT(IN) :: x(:)
    INTEGER(INT8) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_Int8

  MODULE PURE FUNCTION tail_Int16(x) RESULT(Ans)
    INTEGER(INT16), INTENT(IN) :: x(:)
    INTEGER(INT16) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_Int16

  MODULE PURE FUNCTION tail_Int32(x) RESULT(Ans)
    INTEGER(INT32), INTENT(IN) :: x(:)
    INTEGER(INT32) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_Int32

  MODULE PURE FUNCTION tail_Int64(x) RESULT(Ans)
    INTEGER(INT64), INTENT(IN) :: x(:)
    INTEGER(INT64) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_Int64
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

INTERFACE TAIL
  MODULE PURE FUNCTION tail_Real32(x) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_Real32

  MODULE PURE FUNCTION tail_Real64(x) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_Real64
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                                      Tail
!----------------------------------------------------------------------------

INTERFACE TAIL
  MODULE PURE FUNCTION tail_char(x) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: x
    CHARACTER(LEN(x) - 1) :: Ans
  END FUNCTION tail_char
END INTERFACE TAIL

END MODULE TailUtility
