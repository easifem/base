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

#ifdef USE_Int128
MODULE PROCEDURE Diag_5
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_5
#endif

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_6
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_6

!----------------------------------------------------------------------------
!                                                                  Diag
!----------------------------------------------------------------------------

MODULE PROCEDURE Diag_7
INTEGER(I4B) :: ii
ans = 0.0_DFP
DO ii = 1, SIZE(a)
  ans(ii, ii) = REAL(a(ii), kind=DFP)
END DO
END PROCEDURE Diag_7

END SUBMODULE Methods
