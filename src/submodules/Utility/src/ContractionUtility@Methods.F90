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

SUBMODULE(ContractionUtility) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE MatmulUtility, ONLY: Matmul
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r4_r1
ans = MATMUL(a1, a2)
END PROCEDURE contraction_r4_r1

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r4_r2
INTEGER(I4B) :: ii
ans = math%zero
DO ii = 1, SIZE(a2, 2)
  ans = ans + MATMUL(a1(:, :, :, ii), a2(:, ii))
END DO
END PROCEDURE contraction_r4_r2

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r4_r3
INTEGER(I4B) :: ii, jj
ans = math%zero
DO jj = 1, SIZE(a2, 3)
  DO ii = 1, SIZE(a2, 2)
    ans = ans + MATMUL(a1(:, :, ii, jj), a2(:, ii, jj))
  END DO
END DO
END PROCEDURE contraction_r4_r3

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r4_r4
ans = SUM(a1 * a2)
END PROCEDURE contraction_r4_r4

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r3_r1
ans = MATMUL(a1, a2)
END PROCEDURE contraction_r3_r1

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r3_r2
INTEGER(I4B) :: ii
ans = math%zero
DO ii = 1, SIZE(a2, 2)
  ans = ans + MATMUL(a1(:, :, ii), a2(:, ii))
END DO
END PROCEDURE contraction_r3_r2

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r3_r3
ans = SUM(a1 * a2)
END PROCEDURE contraction_r3_r3

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r3_r4
INTEGER(I4B) :: ii
DO ii = 1, SIZE(a2, 4)
  ans(ii) = Contraction(a1, a2(:, :, :, ii))
END DO
END PROCEDURE contraction_r3_r4

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r2_r1
ans = MATMUL(a1, a2)
END PROCEDURE contraction_r2_r1

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r2_r2
ans = SUM(a1 * a2)
END PROCEDURE contraction_r2_r2

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r2_r3
INTEGER(I4B) :: ii
DO ii = 1, SIZE(a2, 3)
  ans(ii) = Contraction(a1, a2(:, :, ii))
END DO
END PROCEDURE contraction_r2_r3

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r2_r4
INTEGER(I4B) :: ii, jj
DO jj = 1, SIZE(a2, 4)
  DO ii = 1, SIZE(a2, 3)
    ans(ii, jj) = Contraction(a1, a2(:, :, ii, jj))
  END DO
END DO
END PROCEDURE contraction_r2_r4

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r1_r1
ans = DOT_PRODUCT(a1, a2)
END PROCEDURE contraction_r1_r1

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r1_r2
ans = MATMUL(a1, a2)
END PROCEDURE contraction_r1_r2

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r1_r3
ans = MATMUL(a1, a2)
END PROCEDURE contraction_r1_r3

!----------------------------------------------------------------------------
!                                                               contraction
!----------------------------------------------------------------------------

MODULE PROCEDURE contraction_r1_r4
ans = MATMUL(a1, a2)
END PROCEDURE contraction_r1_r4

END SUBMODULE Methods
