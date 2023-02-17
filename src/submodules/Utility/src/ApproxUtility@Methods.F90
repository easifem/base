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
Ans = (ABS(a - b) .LE. Zero)
END PROCEDURE approxeq_1

!----------------------------------------------------------------------------
!                                                                 APPROX
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeq_2
Ans = (ABS(a - b) .LE. Zero)
END PROCEDURE approxeq_2

!----------------------------------------------------------------------------
!                                                                 APPROXR
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeqr_1
REAL(DFP) :: eps
eps = MAX(ABS(a), ABS(b)) * Zero
IF (a .EQ. 0.0_DFP .OR. b .EQ. 0.0_DFP) eps = Zero
Ans = (ABS(a - b) .LE. eps)
END PROCEDURE approxeqr_1

!----------------------------------------------------------------------------
!                                                                 APPROXEQF
!----------------------------------------------------------------------------

MODULE PROCEDURE approxeq_ulp_real
IF (a .EQ. 0.0_DFP .OR. b .EQ. 0.0_DFP &
  & .OR. (a > 0._DFP .AND. b < 0._DFP) &
  & .OR. (a < 0._DFP .AND. b > 0._DFP)) THEN
  Ans = approxeq_1(a, b)
ELSE
  Ans = (ABS(TRANSFER(a, 1_I4B) - TRANSFER(b, 1_I4B)) <= 10_I4B)
END IF
END PROCEDURE approxeq_ulp_real

!----------------------------------------------------------------------------
!                                                                 APPROXLE
!----------------------------------------------------------------------------

MODULE PROCEDURE approxle_real
Ans = (r1 .LE. r2 + Zero)
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                   APPROXGE
!----------------------------------------------------------------------------

MODULE PROCEDURE approxge_real
Ans = (r1 + Zero .GE. r2)
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                     SOFTEQ
!----------------------------------------------------------------------------

MODULE PROCEDURE softeq_real
Ans = (ABS(r1 - r2) .LE. tol)
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                   SOFTEQR
!----------------------------------------------------------------------------

MODULE PROCEDURE softeqr_real
REAL(DFP) :: eps
eps = MAX(ABS(r1), ABS(r2)) * tol
IF (r1 .EQ. 0.0_DFP .OR. r1 .EQ. 0.0_DFP) eps = REAL(Zero, DFP)
Ans = (ABS(r1 - r2) .LE. eps)
END PROCEDURE softeqr_real

!----------------------------------------------------------------------------
!                                                                    SOFTLE
!----------------------------------------------------------------------------

MODULE PROCEDURE softle_real
Ans = (r1 .LE. r2 + tol)
END PROCEDURE softle_real

!----------------------------------------------------------------------------
!                                                                     SOFTLT
!----------------------------------------------------------------------------

MODULE PROCEDURE softlt_real
Ans = (r1 < r2 - tol)
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                     SOFTGE
!----------------------------------------------------------------------------

MODULE PROCEDURE softge_real
Ans = (r1 + tol .GE. r2)
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                     SOFTGT
!----------------------------------------------------------------------------

MODULE PROCEDURE softgt_real
Ans = (r1 > r2 + tol)
END PROCEDURE softgt_real

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE equalto_logical
Ans = (l1 .EQV. l2)
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE notequalto_logical
Ans = (l1 .NEQV. l2)
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_int
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(I'//TRIM(fmt)//')') i
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_bool
IF (c == 'true') THEN
  b = .TRUE.
ELSE
  b = .FALSE.
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE assign_char_to_real
INTEGER(I4B) :: tmpInt
CHARACTER(4) :: fmt
tmpInt = LEN(c)
WRITE (fmt, '(i4)') tmpInt; fmt = ADJUSTL(fmt)
READ (c, '(f'//TRIM(fmt)//'.0)') s
END PROCEDURE

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
