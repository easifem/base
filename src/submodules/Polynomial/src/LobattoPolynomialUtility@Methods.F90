! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modIFy
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
! along with this program.  IF not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(LobattoPolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       LobattoLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoLeadingCoeff
REAL(DFP) :: avar, m
  !!
SELECT CASE (n)
CASE (0)
  ans = 0.5_DFP
CASE (1)
  ans = -0.5_DFP
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
  m = LegendreLeadingCoeff(n=n)
  ans = m * avar
END SELECT
END PROCEDURE LobattoLeadingCoeff

!----------------------------------------------------------------------------
!                                                             LobattoNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoNormSqr
REAL(DFP) :: m, a1, a2
SELECT CASE (n)
CASE (0, 1)
  ans = 2.0_DFP / 3.0_DFP
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  a1 = (2.0_DFP * m + 1)
  a2 = (2.0_DFP * m + 5)
  ans = 2.0_DFP / a1 / a2
END SELECT
END PROCEDURE LobattoNormSqr

!----------------------------------------------------------------------------
!                                                               LobattoZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoZeros
SELECT CASE (n)
CASE (1)
  ans(1) = 1.0_DFP
CASE (2)
  ans(1) = -1.0_DFP
  ans(2) = 1.0_DFP
CASE DEFAULT
  ans(1) = -1.0_DFP
  ans(n) = 1.0_DFP
  ans(2:n - 1) = JacobiZeros(alpha=1.0_DFP, beta=1.0_DFP, n=n - 2_I4B)
END SELECT
END PROCEDURE LobattoZeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEval1
REAL(DFP) :: avar, m
SELECT CASE (n)
CASE (0)
  ans = 0.5_DFP * (1.0_DFP - x)
CASE (1)
  ans = 0.5_DFP * (1.0_DFP + x)
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
  ans = avar * (LegendreEval(n=n, x=x) - LegendreEval(n=n - 2_I4B, x=x))
END SELECT
END PROCEDURE LobattoEval1

!----------------------------------------------------------------------------
!                                                                 LobattoEval
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEval2
REAL(DFP) :: avar, m
  !!
SELECT CASE (n)
CASE (0)
  ans = 0.5_DFP * (1.0_DFP - x)
CASE (1)
  ans = 0.5_DFP * (1.0_DFP + x)
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
  ans = avar * (LegendreEval(n=n, x=x) - LegendreEval(n=n - 2_I4B, x=x))
END SELECT
END PROCEDURE LobattoEval2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEvalAll1
REAL(DFP) :: avar, m
REAL(DFP) :: p(n + 1)
INTEGER(I4B) :: ii
  !!
SELECT CASE (n)
CASE (0)
  ans(1) = 0.5_DFP * (1.0_DFP - x)
CASE (1)
  ans(1) = 0.5_DFP * (1.0_DFP - x)
  ans(2) = 0.5_DFP * (1.0_DFP + x)
CASE DEFAULT
  ans(1) = 0.5_DFP * (1.0_DFP - x)
  ans(2) = 0.5_DFP * (1.0_DFP + x)
  p = LegendreEvalAll(n=n, x=x)
  DO ii = 1, n - 1
    m = REAL(ii - 1, KIND=DFP)
    avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
    ans(2 + ii) = avar * (p(ii + 2) - p(ii))
  END DO
END SELECT
END PROCEDURE LobattoEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEvalAll2
REAL(DFP) :: avar, m
REAL(DFP) :: p(SIZE(x), n + 1)
INTEGER(I4B) :: ii
  !!
SELECT CASE (n)
CASE (0)
  ans(:, 1) = 0.5_DFP * (1.0_DFP - x)
CASE (1)
  ans(:, 1) = 0.5_DFP * (1.0_DFP - x)
  ans(:, 2) = 0.5_DFP * (1.0_DFP + x)
CASE DEFAULT
  ans(:, 1) = 0.5_DFP * (1.0_DFP - x)
  ans(:, 2) = 0.5_DFP * (1.0_DFP + x)
  p = LegendreEvalAll(n=n, x=x)
  DO ii = 1, n - 1
    m = REAL(ii - 1, KIND=DFP)
    avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
    ans(:, 2 + ii) = avar * (p(:, ii + 2) - p(:, ii))
  END DO
END SELECT
END PROCEDURE LobattoEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoMonomialExpansionAll
REAL(DFP) :: avar, m
REAL(DFP) :: p(n + 1, n + 1)
INTEGER(I4B) :: ii
!!
ans = 0.0_DFP
!!
SELECT CASE (n)
CASE (0)
  ans(1, 1) = 0.5_DFP
CASE (1)
  ans(1, 1) = 0.5_DFP
  ans(2, 1) = -0.5_DFP
  ans(1, 2) = 0.5_DFP
  ans(2, 2) = 0.5_DFP
CASE DEFAULT
  ans(1, 1) = 0.5_DFP
  ans(2, 1) = -0.5_DFP
  ans(1, 2) = 0.5_DFP
  ans(2, 2) = 0.5_DFP
  !!
  p = LegendreMonomialExpansionAll(n=n)
  !!
  DO ii = 1, n - 1
    m = REAL(ii - 1, KIND=DFP)
    avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
    ans(:, ii + 2) = avar * (p(:, ii + 2) - p(:, ii))
  END DO
  !!
END SELECT
END PROCEDURE LobattoMonomialExpansionAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoMonomialExpansion
REAL(DFP) :: coeff(n + 1, n + 1)
coeff = LobattoMonomialExpansionAll(n)
ans = coeff(:, n + 1)
END PROCEDURE LobattoMonomialExpansion

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEvalAll1
REAL(DFP) :: p(n), avar, m
INTEGER(I4B) :: ii
  !!
SELECT CASE (n)
CASE (0)
  ans(1) = -0.5_DFP
CASE (1)
  ans(1) = -0.5_DFP
  ans(2) = 0.5_DFP
CASE DEFAULT
  ans(1) = -0.5_DFP
  ans(2) = 0.5_DFP
  !!
  p = LegendreEvalAll(n=n - 1_I4B, x=x)
  !!
  DO ii = 1, n - 1
    m = REAL(ii - 1, DFP)
    avar = SQRT((2.0_DFP * m + 3.0) / 2.0)
    ans(ii + 2) = avar * p(ii + 1)
    ! ans(3:) = p(2:)
  END DO
  !!
END SELECT
END PROCEDURE LobattoGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEvalAll2
REAL(DFP) :: p(SIZE(x), n), avar, m
INTEGER(I4B) :: ii
  !!
SELECT CASE (n)
CASE (0)
  ans(:, 1) = -0.5_DFP
CASE (1)
  ans(:, 1) = -0.5_DFP
  ans(:, 2) = 0.5_DFP
CASE DEFAULT
  ans(:, 1) = -0.5_DFP
  ans(:, 2) = 0.5_DFP
  !!
  p = LegendreEvalAll(n=n - 1_I4B, x=x)
  !!
  DO ii = 1, n - 1
    m = REAL(ii - 1, DFP)
    avar = SQRT((2.0_DFP * m + 3.0) / 2.0)
    ans(:, ii + 2) = avar * p(:, ii + 1)
    ! ans(3:) = p(2:)
  END DO
  !!
END SELECT
END PROCEDURE LobattoGradientEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEval1
REAL(DFP) :: p, avar, m
  !!
SELECT CASE (n)
CASE (0)
  ans = -0.5_DFP
CASE (1)
  ans = 0.5_DFP
CASE DEFAULT
  !!
  p = LegendreEval(n=n - 1_I4B, x=x)
  m = REAL(n - 2, DFP)
  avar = SQRT((2.0_DFP * m + 3.0) / 2.0)
  ans = avar * p
END SELECT
END PROCEDURE LobattoGradientEval1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEval2
REAL(DFP) :: p(SIZE(x)), avar, m
  !!
SELECT CASE (n)
CASE (0)
  ans = -0.5_DFP
CASE (1)
  ans = 0.5_DFP
CASE DEFAULT
  !!
  p = LegendreEval(n=n - 1_I4B, x=x)
  m = REAL(n - 2, DFP)
  avar = SQRT((2.0_DFP * m + 3.0) / 2.0)
  ans = avar * p
END SELECT
END PROCEDURE LobattoGradientEval2

!----------------------------------------------------------------------------
!                                                         LobattoMassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoMassMatrix
INTEGER(I4B) :: ii
REAL(DFP) :: m
!!
ans = 0.0_DFP
!!
DO ii = 1, n + 1
  ans(ii, ii) = LobattoNormSQR(n=ii - 1_I4B)
END DO
!!
IF (n .EQ. 0_I4B) RETURN
!!
ans(1, 2) = 1.0_DFP / 3.0_DFP
ans(2, 1) = ans(1, 2)
!!
IF (n .EQ. 1_I4B) RETURN
!!
ans(1, 3) = -1.0_DFP / SQRT(6.0_DFP)
ans(3, 1) = ans(1, 3)
ans(2, 3) = ans(1, 3)
ans(3, 2) = ans(2, 3)
!!
IF (n .EQ. 2_I4B) RETURN
!!
ans(1, 4) = 1.0_DFP / SQRT(90.0_DFP)
ans(4, 1) = ans(1, 4)
ans(2, 4) = -ans(1, 4)
ans(4, 2) = ans(2, 4)
!!
IF (n .EQ. 3_I4B) RETURN
!!
DO ii = 3, n + 1
  !!
  m = REAL(ii - 3, DFP)
  !!
  IF (ii + 2 .LE. n + 1) THEN
    ans(ii, ii + 2) = -1.0_DFP / (2.0_DFP * m + 5.0_DFP) / &
      & SQRT((2.0_DFP * m + 7.0_DFP) * (2.0_DFP * m + 3.0_DFP))
    !!
    ans(ii + 2, ii) = ans(ii, ii + 2)
  END IF
  !!
END DO
!!
END PROCEDURE LobattoMassMatrix

!----------------------------------------------------------------------------
!                                                     LobattoStiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoStiffnessMatrix
INTEGER(I4B) :: ii
REAL(DFP) :: m
!!
ans = 0.0_DFP
!!
DO ii = 1, n + 1
  ans(ii, ii) = 1.0_DFP
END DO
!!
ans(1, 1) = 0.5_DFP
!!
IF (n .EQ. 0_I4B) RETURN
!!
ans(2, 2) = 0.5_DFP
ans(1, 2) = -0.5_DFP
ans(2, 1) = ans(1, 2)
!!
END PROCEDURE LobattoStiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
