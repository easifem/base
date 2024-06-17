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

SUBMODULE(ApproxUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 APPROX
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeq_1
REAL(REAL64), PARAMETER :: my_zero = 1.0E-10
ans = (ABS(a - b) .LE. my_zero)
END PROCEDURE approxeq_1

!----------------------------------------------------------------------------
!                                                                 APPROX
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeq_2
REAL(REAL64), PARAMETER :: my_zero = 1.0E-10
ans = (ABS(a - b) .LE. my_zero)
END PROCEDURE approxeq_2

!----------------------------------------------------------------------------
!                                                                 APPROXR
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeqr_1
REAL(REAL32) :: eps
REAL(REAL32), PARAMETER :: my_zero = REAL(Zero, REAL32)
eps = my_zero + MAX(ABS(a), ABS(b)) * my_zero
ans = (ABS(a - b) .LE. eps)
END PROCEDURE approxeqr_1

!----------------------------------------------------------------------------
!                                                                 APPROXR
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeqr_2
REAL(REAL64) :: eps
REAL(REAL64), PARAMETER :: my_zero = REAL(Zero, REAL64)
eps = my_zero + MAX(ABS(a), ABS(b)) * my_zero
ans = (ABS(a - b) .LE. eps)
END PROCEDURE approxeqr_2

!----------------------------------------------------------------------------
!                                                                 APPROXEQF
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeq_ulp_real
LOGICAL(LGT) :: isok

isok = (a > 0._DFP .AND. b < 0._DFP) .OR. (a < 0._DFP .AND. b > 0._DFP)

IF (isok) THEN
  ans = approxeq_1(a, b)
ELSE
  ans = (ABS(TRANSFER(a, 1_I4B) - TRANSFER(b, 1_I4B)) <= 10_I4B)
END IF
END PROCEDURE approxeq_ulp_real

!----------------------------------------------------------------------------
!                                                                 APPROXLE
!----------------------------------------------------------------------------

MODULE PROCEDURE approxle_1
REAL(DFP), PARAMETER :: my_zero = 1.0E-10
ans = (r1 - r2 .LE. my_zero)
END PROCEDURE approxle_1

!----------------------------------------------------------------------------
!                                                                 APPROXLE
!----------------------------------------------------------------------------

MODULE PROCEDURE approxle_2
REAL(DFP), PARAMETER :: my_zero = 1.0E-10
ans = (r1 - r2 .LE. my_zero)
END PROCEDURE approxle_2

!----------------------------------------------------------------------------
!                                                                   APPROXGE
!----------------------------------------------------------------------------

MODULE PROCEDURE approxge_1
REAL(DFP), PARAMETER :: my_zero = 1.0E-10
ans = (my_zero .GE. r2 - r1)
END PROCEDURE approxge_1

!----------------------------------------------------------------------------
!                                                                   APPROXGE
!----------------------------------------------------------------------------

MODULE PROCEDURE approxge_2
REAL(DFP), PARAMETER :: my_zero = 1.0E-10
ans = (my_zero .GE. r2 - r1)
END PROCEDURE approxge_2

!----------------------------------------------------------------------------
!                                                                     SOFTEQ
!----------------------------------------------------------------------------

MODULE PROCEDURE softeq_1
ans = (ABS(r1 - r2) .LE. tol)
END PROCEDURE softeq_1

!----------------------------------------------------------------------------
!                                                                     SOFTEQ
!----------------------------------------------------------------------------

MODULE PROCEDURE softeq_2
ans = (ABS(r1 - r2) .LE. tol)
END PROCEDURE softeq_2

!----------------------------------------------------------------------------
!                                                                   SOFTEQR
!----------------------------------------------------------------------------

MODULE PROCEDURE softeqr_1
REAL(REAL32) :: eps
eps = REAL(Zero, REAL32) + MAX(ABS(r1), ABS(r2)) * tol
ans = (ABS(r1 - r2) .LE. eps)
END PROCEDURE softeqr_1

!----------------------------------------------------------------------------
!                                                                   SOFTEQR
!----------------------------------------------------------------------------

MODULE PROCEDURE softeqr_2
REAL(REAL64) :: eps
eps = REAL(Zero, REAL64) + MAX(ABS(r1), ABS(r2)) * tol
ans = (ABS(r1 - r2) .LE. eps)
END PROCEDURE softeqr_2

!----------------------------------------------------------------------------
!                                                                    SOFTLE
!----------------------------------------------------------------------------

MODULE PROCEDURE softle_1
ans = (r1 .LE. r2 + tol)
END PROCEDURE softle_1

!----------------------------------------------------------------------------
!                                                                    SOFTLE
!----------------------------------------------------------------------------

MODULE PROCEDURE softle_2
ans = (r1 .LE. r2 + tol)
END PROCEDURE softle_2

!----------------------------------------------------------------------------
!                                                                     SOFTLT
!----------------------------------------------------------------------------

MODULE PROCEDURE softlt_1
ans = (r1 < r2 - tol)
END PROCEDURE softlt_1

!----------------------------------------------------------------------------
!                                                                     SOFTLT
!----------------------------------------------------------------------------

MODULE PROCEDURE softlt_2
ans = (r1 < r2 - tol)
END PROCEDURE softlt_2

!----------------------------------------------------------------------------
!                                                                     SOFTGE
!----------------------------------------------------------------------------

MODULE PROCEDURE softge_1
ans = (r1 + tol .GE. r2)
END PROCEDURE softge_1

!----------------------------------------------------------------------------
!                                                                     SOFTGE
!----------------------------------------------------------------------------

MODULE PROCEDURE softge_2
ans = (r1 + tol .GE. r2)
END PROCEDURE softge_2

!----------------------------------------------------------------------------
!                                                                     SOFTGT
!----------------------------------------------------------------------------

MODULE PROCEDURE softgt_1
ans = (r1 > r2 + tol)
END PROCEDURE softgt_1

!----------------------------------------------------------------------------
!                                                                     SOFTGT
!----------------------------------------------------------------------------

MODULE PROCEDURE softgt_2
ans = (r1 > r2 + tol)
END PROCEDURE softgt_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE equalto_logical
ans = (l1 .EQV. l2)
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE notequalto_logical
ans = (l1 .NEQV. l2)
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_Int8
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(I'//TRIM(fmt)//')') i
END PROCEDURE assign_char_to_Int8

MODULE PROCEDURE assign_char_to_Int16
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(I'//TRIM(fmt)//')') i
END PROCEDURE assign_char_to_Int16

MODULE PROCEDURE assign_char_to_Int32
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(I'//TRIM(fmt)//')') i
END PROCEDURE assign_char_to_Int32

MODULE PROCEDURE assign_char_to_Int64
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(I'//TRIM(fmt)//')') i
END PROCEDURE assign_char_to_Int64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_bool
IF (c == 'true') THEN
  b = .TRUE.
ELSE
  b = .FALSE.
END IF
END PROCEDURE assign_char_to_bool

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_real32
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(f'//TRIM(fmt)//'.0)') s
END PROCEDURE assign_char_to_real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_real64
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(f'//TRIM(fmt)//'.0)') s
END PROCEDURE assign_char_to_real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE isNumeric
INTEGER(I4B) :: i, val
bool = .FALSE.
IF (LEN(char_str) < 1) THEN
  RETURN
ELSE
  DO i = 1, LEN(char_str)
    ! 0-9 are represented by ASCII codes 48-57
    val = IACHAR(char_str(i:i))
    IF (.NOT. (val > 47 .AND. val < 58)) THEN
      ! If any character isn't between those codes, it isn't an integer
      RETURN
    END IF
  END DO
END IF
bool = .TRUE.
END PROCEDURE

END SUBMODULE Methods
