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

MODULE InputUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Int8(default, option) RESULT(Ans)
    INTEGER(Int8), INTENT(IN) :: default
    INTEGER(Int8), OPTIONAL, INTENT(IN) :: option
    INTEGER(Int8) :: Ans
  END FUNCTION input_Int8
  MODULE PURE FUNCTION input_Int16(default, option) RESULT(Ans)
    INTEGER(Int16), INTENT(IN) :: default
    INTEGER(Int16), OPTIONAL, INTENT(IN) :: option
    INTEGER(Int16) :: Ans
  END FUNCTION input_Int16
  MODULE PURE FUNCTION input_Int32(default, option) RESULT(Ans)
    INTEGER(Int32), INTENT(IN) :: default
    INTEGER(Int32), OPTIONAL, INTENT(IN) :: option
    INTEGER(Int32) :: Ans
  END FUNCTION input_Int32
  MODULE PURE FUNCTION input_Int64(default, option) RESULT(Ans)
    INTEGER(Int64), INTENT(IN) :: default
    INTEGER(Int64), OPTIONAL, INTENT(IN) :: option
    INTEGER(Int64) :: Ans
  END FUNCTION input_Int64
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_Int8, input_Int16, input_Int32, input_Int64
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real32(default, option) RESULT(ans)
    REAL(Real32), INTENT(in) :: default
    REAL(Real32), OPTIONAL, INTENT(in) :: option
    REAL(Real32) :: ans
  END FUNCTION input_Real32
  MODULE PURE FUNCTION input_Real64(default, option) RESULT(ans)
    REAL(Real64), INTENT(in) :: default
    REAL(Real64), OPTIONAL, INTENT(in) :: option
    REAL(Real64) :: ans
  END FUNCTION input_Real64
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_Real32, input_Real64
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Int8Vec(default, option) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: default(:)
    INTEGER(Int8), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(Int8) :: ans(SIZE(default))
  END FUNCTION input_Int8Vec
  MODULE PURE FUNCTION input_Int16Vec(default, option) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: default(:)
    INTEGER(Int16), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(Int16) :: ans(SIZE(default))
  END FUNCTION input_Int16Vec
  MODULE PURE FUNCTION input_Int32Vec(default, option) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: default(:)
    INTEGER(Int32), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(Int32) :: ans(SIZE(default))
  END FUNCTION input_Int32Vec
  MODULE PURE FUNCTION input_Int64Vec(default, option) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: default(:)
    INTEGER(Int64), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(Int64) :: ans(SIZE(default))
  END FUNCTION input_Int64Vec
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_Int8Vec, input_Int16Vec, input_Int32Vec, &
    & input_Int64Vec
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real32vec(default, option) RESULT(ans)
    REAL(Real32), INTENT(IN) :: default(:)
    REAL(Real32), OPTIONAL, INTENT(IN) :: option(:)
    REAL(Real32) :: ans(SIZE(default))
  END FUNCTION
  MODULE PURE FUNCTION input_Real64vec(default, option) RESULT(ans)
    REAL(Real64), INTENT(IN) :: default(:)
    REAL(Real64), OPTIONAL, INTENT(IN) :: option(:)
    REAL(Real64) :: ans(SIZE(default))
  END FUNCTION
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_Real32vec, input_Real64vec
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Int8Array(default, option) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: default(:, :)
    INTEGER(Int8), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(Int8) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int8Array
  MODULE PURE FUNCTION input_Int16Array(default, option) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: default(:, :)
    INTEGER(Int16), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(Int16) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int16Array
  MODULE PURE FUNCTION input_Int32Array(default, option) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: default(:, :)
    INTEGER(Int32), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(Int32) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int32Array
  MODULE PURE FUNCTION input_Int64Array(default, option) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: default(:, :)
    INTEGER(Int64), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(Int64) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Int64Array
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_Int8Array, input_Int16Array, input_Int32Array, &
    & input_Int64Array
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real32Array(default, option) RESULT(ans)
    REAL(Real32), INTENT(IN) :: default(:, :)
    REAL(Real32), OPTIONAL, INTENT(IN) :: option(:, :)
    REAL(Real32) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Real32Array
  MODULE PURE FUNCTION input_Real64Array(default, option) RESULT(ans)
    REAL(Real64), INTENT(IN) :: default(:, :)
    REAL(Real64), OPTIONAL, INTENT(IN) :: option(:, :)
    REAL(Real64) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION input_Real64Array
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_Real32Array, input_Real64Array
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_String(default, option) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: default
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: option
    CHARACTER(LEN=:), ALLOCATABLE :: ans
  END FUNCTION
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_String
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

! INTERFACE
!   MODULE PURE FUNCTION input_StringVec(default, option) RESULT(ans)
!     CHARACTER(LEN=*), INTENT(IN) :: default(:)
!     CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: option(:)
!     CHARACTER(LEN=:), ALLOCATABLE :: ans(:)
!   END FUNCTION input_StringVec
! END INTERFACE

! INTERFACE Input
!   MODULE PROCEDURE input_StringVec
! END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_logical(default, option) RESULT(ans)
    LOGICAL(LGT), INTENT(IN) :: default
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option
    LOGICAL(LGT) :: ans
  END FUNCTION
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_logical
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_logicalvec(default, option) RESULT(ans)
    LOGICAL(LGT), INTENT(IN) :: default(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option(:)
    LOGICAL(LGT) :: ans(SIZE(default))
  END FUNCTION
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_logicalvec
END INTERFACE Input

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_logicalArray(default, option) RESULT(ans)
    LOGICAL(LGT), INTENT(IN) :: default(:, :)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option(:, :)
    LOGICAL(LGT) :: ans(SIZE(default, 1), SIZE(default, 2))
  END FUNCTION
END INTERFACE

INTERFACE Input
  MODULE PROCEDURE input_logicalArray
END INTERFACE Input

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE InputUtility
