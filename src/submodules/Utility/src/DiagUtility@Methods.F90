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

SUBMODULE(DiagUtility) Methods
USE BaseMethod, ONLY: Reallocate, Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_1
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_1

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_2
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_2

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_3
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_3

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_4
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_4

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_5
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_5

!----------------------------------------------------------------------------
!                                                                      Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_6
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_6

!----------------------------------------------------------------------------
!                                                                      Diag
!----------------------------------------------------------------------------

#ifdef USE_Int128
MODULE PROCEDURE Diag_7
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_7
#endif

!----------------------------------------------------------------------------
!                                                                      Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_8
INTEGER(I4B) :: n, m, ii
n = SIZE(mat, 1)
IF (diagNo .EQ. 0) THEN
  !!
  CALL Reallocate(ans, n)
  DO CONCURRENT(ii=1:n)
    ans(ii) = mat(ii, ii)
  END DO
  !!
ELSEIF (diagNo .GT. 0) THEN
  !!
  m = n - diagNo
  CALL Reallocate(ans, m)
  DO CONCURRENT(ii=1:m)
    ans(ii) = mat(ii, ii + diagNo)
  END DO
  !!
ELSE
  !!
  m = n + diagNo
  CALL Reallocate(ans, m)
  DO CONCURRENT(ii=1:m)
    ans(ii) = mat(ii - diagNo, ii)
  END DO
  !!
END IF
  !!
END PROCEDURE Diag_8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetDiag1
#include "./Diag/SetDiag.inc"
END PROCEDURE SetDiag1

MODULE PROCEDURE SetDiag2
#include "./Diag/SetDiag.inc"
END PROCEDURE SetDiag2

MODULE PROCEDURE SetDiag3
#include "./Diag/SetDiag.inc"
END PROCEDURE SetDiag3

MODULE PROCEDURE SetDiag4
#include "./Diag/SetDiag.inc"
END PROCEDURE SetDiag4

MODULE PROCEDURE SetDiag5
#include "./Diag/SetDiag.inc"
END PROCEDURE SetDiag5

MODULE PROCEDURE SetDiag6
#include "./Diag/SetDiag.inc"
END PROCEDURE SetDiag6

!----------------------------------------------------------------------------
!                                                                 SetTriDiag
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTriDiag1
#include "./Diag/SetTriDiag.inc"
END PROCEDURE SetTriDiag1

MODULE PROCEDURE SetTriDiag2
#include "./Diag/SetTriDiag.inc"
END PROCEDURE SetTriDiag2

MODULE PROCEDURE SetTriDiag3
#include "./Diag/SetTriDiag.inc"
END PROCEDURE SetTriDiag3

MODULE PROCEDURE SetTriDiag4
#include "./Diag/SetTriDiag.inc"
END PROCEDURE SetTriDiag4

MODULE PROCEDURE SetTriDiag5
#include "./Diag/SetTriDiag.inc"
END PROCEDURE SetTriDiag5

MODULE PROCEDURE SetTriDiag6
#include "./Diag/SetTriDiag.inc"
END PROCEDURE SetTriDiag6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiagSize1
ans = DiagSize(m=n, n=n, diagNo=diagNo)
END PROCEDURE DiagSize1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE DiagSize2
IF (diagNo .GE. 0 .AND. diagNo .LE. n) THEN
  ans = MIN(m, n - diagNo)
ELSE IF (diagNo .LT. 0 .AND. -diagNo .LE. m) THEN
  ans = MIN(n, m + diagNo)
ELSE
  ans = 0
END IF
END PROCEDURE DiagSize2

!----------------------------------------------------------------------------
!                                                                 DiagIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE DiagIndx
INTEGER(I4B) :: tsize, ii
!
tsize = DiagSize(m, n, diagNo)
!
ALLOCATE (ans(tsize, 2))
!
IF (diagNo .GE. 0 .AND. diagNo .LE. n) THEN
  DO CONCURRENT(ii=1:tsize)
    ans(ii, 1) = ii
    ans(ii, 2) = ii + diagNo
  END DO
ELSE IF (diagNo .LT. 0 .AND. -diagNo .LE. m) THEN
  DO CONCURRENT(ii=1:tsize)
    ans(ii, 2) = ii
    ans(ii, 1) = ii - diagNo
  END DO
END IF
END PROCEDURE DiagIndx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Tridiag_1
#include "./Diag/Tridiag.inc"
END PROCEDURE Tridiag_1

MODULE PROCEDURE Tridiag_2
#include "./Diag/Tridiag.inc"
END PROCEDURE Tridiag_2

MODULE PROCEDURE Tridiag_3
#include "./Diag/Tridiag.inc"
END PROCEDURE Tridiag_3

MODULE PROCEDURE Tridiag_4
#include "./Diag/Tridiag.inc"
END PROCEDURE Tridiag_4

MODULE PROCEDURE Tridiag_5
#include "./Diag/Tridiag.inc"
END PROCEDURE Tridiag_5

MODULE PROCEDURE Tridiag_6
#include "./Diag/Tridiag.inc"
END PROCEDURE Tridiag_6

END SUBMODULE Methods
