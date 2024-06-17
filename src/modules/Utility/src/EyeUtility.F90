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

MODULE EyeUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-26
! summary: Return an identity matrix of an integers

INTERFACE
  MODULE PURE FUNCTION int_eye_1(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT8), INTENT(IN) :: DataType
    INTEGER(INT8) :: ans(m, m)
  END FUNCTION int_eye_1

  MODULE PURE FUNCTION int_eye_2(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT16), INTENT(IN) :: DataType
    INTEGER(INT16) :: ans(m, m)
  END FUNCTION int_eye_2

  MODULE PURE FUNCTION int_eye_3(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT32), INTENT(IN) :: DataType
    INTEGER(INT32) :: ans(m, m)
  END FUNCTION int_eye_3

  MODULE PURE FUNCTION int_eye_4(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(INT64), INTENT(IN) :: DataType
    INTEGER(INT64) :: ans(m, m)
  END FUNCTION int_eye_4
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE int_eye_1, int_eye_2, int_eye_3, int_eye_4
END INTERFACE Eye

#ifdef USE_Int128
INTERFACE
  MODULE PURE FUNCTION int_eye_5(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    INTEGER(Int128), INTENT(IN) :: DataType
    INTEGER(Int128) :: ans(m, m)
  END FUNCTION int_eye_5
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE int_eye_5
END INTERFACE Eye
#endif

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-26
! summary:  Return identity matrix of real numbers
INTERFACE

  MODULE PURE FUNCTION real_eye_1(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    REAL(REAL64) :: ans(m, m)
    REAL(REAL64), INTENT(IN) :: DataType
  END FUNCTION real_eye_1
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE real_eye_1
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-26
! summary:  Return identity matrix of real number
INTERFACE

  MODULE PURE FUNCTION real_eye_2(m) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    REAL(DFP) :: ans(m, m)
  END FUNCTION real_eye_2
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE real_eye_2
END INTERFACE Eye

!----------------------------------------------------------------------------
!                                                            Eye@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-26
! summary:  Return identity matrix of real numbers
INTERFACE

  MODULE PURE FUNCTION real_eye_3(m, DataType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: m
    REAL(REAL32) :: ans(m, m)
    REAL(REAL32), INTENT(IN) :: DataType
  END FUNCTION real_eye_3
END INTERFACE

INTERFACE Eye
  MODULE PROCEDURE real_eye_3
END INTERFACE Eye

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EyeUtility
