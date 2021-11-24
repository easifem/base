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


!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_int(default, option) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: option
    INTEGER(I4B) :: Ans
  END FUNCTION input_int
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real64(default, option) RESULT(val)
    REAL(Real64), INTENT(in) :: default
    REAL(Real64), OPTIONAL, INTENT(in) :: option
    REAL(Real64) :: val
  END FUNCTION input_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real32(default, option) RESULT(val)
    REAL(Real32), INTENT(in) :: default
    REAL(Real32), OPTIONAL, INTENT(in) :: option
    REAL(Real32) :: val
  END FUNCTION input_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_IntVec(default, option) RESULT(val)
    INTEGER(I4B), INTENT(IN) :: default(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: option(:)
    INTEGER(I4B), ALLOCATABLE :: val(:)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real64vec(default, option) RESULT(val)
    REAL(Real64), INTENT(IN) :: default(:)
    REAL(Real64), OPTIONAL, INTENT(IN) :: option(:)
    REAL(Real64), ALLOCATABLE :: val(:)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real32vec(default, option) RESULT(val)
    REAL(Real32), INTENT(IN) :: default(:)
    REAL(Real32), OPTIONAL, INTENT(IN) :: option(:)
    REAL(Real32), ALLOCATABLE :: val(:)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_IntArray(default, option) RESULT(val)
    INTEGER(I4B), INTENT(IN) :: default(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: option(:, :)
    INTEGER(I4B), ALLOCATABLE :: val(:, :)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real64Array(default, option) RESULT(val)
    REAL(Real64), INTENT(IN) :: default(:, :)
    REAL(Real64), OPTIONAL, INTENT(IN) :: option(:, :)
    REAL(Real64), ALLOCATABLE :: val(:, :)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_Real32Array(default, option) RESULT(val)
    REAL(Real32), INTENT(IN) :: default(:, :)
    REAL(Real32), OPTIONAL, INTENT(IN) :: option(:, :)
    REAL(Real32), ALLOCATABLE :: val(:, :)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_String(default, option) RESULT(val)
    CHARACTER(LEN=*), INTENT(IN) :: default
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: option
    CHARACTER(200) :: val
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION input_logical(default, option) RESULT(val)
    LOGICAL(LGT), INTENT(IN) :: default
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: option
    LOGICAL(LGT) :: val
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Input@Input
!----------------------------------------------------------------------------

INTERFACE Input
  MODULE PROCEDURE input_Int, input_Real64, input_Real32, input_IntVec, &
    & input_Real64Vec, input_Real32Vec, input_IntArray, &
    & input_Real64Array, input_Real32Array, input_String, &
    & input_logical
END INTERFACE Input

PUBLIC :: Input
