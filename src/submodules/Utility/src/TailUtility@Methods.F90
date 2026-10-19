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

SUBMODULE(TailUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_Int8
ans = x(2:)
END PROCEDURE tail_Int8

MODULE PROCEDURE tail_Int16
ans = x(2:)
END PROCEDURE tail_Int16

MODULE PROCEDURE tail_Int32
ans = x(2:)
END PROCEDURE tail_Int32

MODULE PROCEDURE tail_Int64
ans = x(2:)
END PROCEDURE tail_Int64

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_Real32
ans = x(2:)
END PROCEDURE tail_Real32

MODULE PROCEDURE tail_Real64
ans = x(2:)
END PROCEDURE tail_Real64

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_char
ans = x(2:)
END PROCEDURE tail_char

!----------------------------------------------------------------------------
!                                                                      last
!----------------------------------------------------------------------------

MODULE PROCEDURE last_Int8
IF (SIZE(x) .EQ. 0) THEN
  ans = 0_INT8
  RETURN
END IF
ans = x(SIZE(x))
END PROCEDURE last_Int8

MODULE PROCEDURE last_Int16
IF (SIZE(x) .EQ. 0) THEN
  ans = 0_INT16
  RETURN
END IF
ans = x(SIZE(x))
END PROCEDURE last_Int16

MODULE PROCEDURE last_Int32
IF (SIZE(x) .EQ. 0) THEN
  ans = 0_INT32
  RETURN
END IF
ans = x(SIZE(x))
END PROCEDURE last_Int32

MODULE PROCEDURE last_Int64
IF (SIZE(x) .EQ. 0) THEN
  ans = 0_INT64
  RETURN
END IF
ans = x(SIZE(x))
END PROCEDURE last_Int64

!----------------------------------------------------------------------------
!                                                                 last
!----------------------------------------------------------------------------

MODULE PROCEDURE last_Real32
IF (SIZE(x) .EQ. 0) THEN
  ans = 0.0_REAL32
  RETURN
END IF
ans = x(SIZE(x))
END PROCEDURE last_Real32

MODULE PROCEDURE last_Real64
IF (SIZE(x) .EQ. 0) THEN
  ans = 0.0_REAL64
  RETURN
END IF
ans = x(SIZE(x))
END PROCEDURE last_Real64

!----------------------------------------------------------------------------
!                                                                 last
!----------------------------------------------------------------------------

MODULE PROCEDURE last_char
IF (LEN(x) .EQ. 0) THEN
  ans = " "
  RETURN
END IF
ans = x(LEN(x):LEN(x))
END PROCEDURE last_char

END SUBMODULE Methods
