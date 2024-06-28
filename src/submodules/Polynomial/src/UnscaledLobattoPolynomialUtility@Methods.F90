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

SUBMODULE(UnscaledLobattoPolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                UnscaledLobattoLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoLeadingCoeff
REAL(DFP) :: avar, m
  !!
SELECT CASE (n)
CASE (0)
  ans = 0.5_DFP
CASE (1)
  ans = -0.5_DFP
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  avar = 1.0_DFP / (2.0_DFP * m + 3.0_DFP)
  m = LegendreLeadingCoeff(n=n)
  ans = m * avar
END SELECT
END PROCEDURE UnscaledLobattoLeadingCoeff

!----------------------------------------------------------------------------
!                                                     UnscaledLobattoNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoNormSqr
REAL(DFP) :: m, a1, a2, a3
SELECT CASE (n)
CASE (0, 1)
  ans = 2.0_DFP / 3.0_DFP
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  a1 = (2.0_DFP * m + 1)
  a2 = (2.0_DFP * m + 3)
  a3 = (2.0_DFP * m + 5)
  ans = 4.0_DFP / a1 / a2 / a3
END SELECT
END PROCEDURE UnscaledLobattoNormSqr

!----------------------------------------------------------------------------
!                                                       UnscaledLobattoZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoZeros
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
END PROCEDURE UnscaledLobattoZeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoEval1
REAL(DFP) :: avar, m
SELECT CASE (n)
CASE (0)
  ans = 0.5_DFP * (1.0_DFP - x)
CASE (1)
  ans = 0.5_DFP * (1.0_DFP + x)
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  avar = 1.0_DFP / (2.0_DFP * m + 3.0_DFP)
  ans = avar * (LegendreEval(n=n, x=x) - LegendreEval(n=n - 2_I4B, x=x))
END SELECT
END PROCEDURE UnscaledLobattoEval1

!----------------------------------------------------------------------------
!                                                        UnscaledLobattoEval
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoEval2
REAL(DFP) :: avar, m
  !!
SELECT CASE (n)
CASE (0)
  ans = 0.5_DFP * (1.0_DFP - x)
CASE (1)
  ans = 0.5_DFP * (1.0_DFP + x)
CASE DEFAULT
  m = REAL(n, KIND=DFP) - 2.0_DFP
  avar = 1.0_DFP / (2.0_DFP * m + 3.0_DFP)
  ans = avar * (LegendreEval(n=n, x=x) - LegendreEval(n=n - 2_I4B, x=x))
END SELECT
END PROCEDURE UnscaledLobattoEval2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoEvalAll1
INTEGER(I4B) :: tsize
CALL UnscaledLobattoEvalAll1_(n=n, x=x, ans=ans, tsize=tsize)
END PROCEDURE UnscaledLobattoEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoEvalAll1_
REAL(DFP) :: avar, m
REAL(DFP) :: p(n + 1)
INTEGER(I4B) :: ii

tsize = n + 1

SELECT CASE (n)
CASE (0)
  ans(1) = 0.5_DFP * (1.0_DFP - x)

CASE (1)
  ans(1) = 0.5_DFP * (1.0_DFP - x)
  ans(2) = 0.5_DFP * (1.0_DFP + x)

CASE DEFAULT
  ans(1) = 0.5_DFP * (1.0_DFP - x)
  ans(2) = 0.5_DFP * (1.0_DFP + x)

  CALL LegendreEvalAll_(n=n, x=x, ans=p, tsize=ii)

  DO ii = 1, n - 1
    m = REAL(ii - 1, KIND=DFP)
    avar = 1.0_DFP / (2.0_DFP * m + 3.0_DFP)
    ans(2 + ii) = avar * (p(ii + 2) - p(ii))
  END DO

END SELECT
END PROCEDURE UnscaledLobattoEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL UnscaledLobattoEvalAll2_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE UnscaledLobattoEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoEvalAll2_
REAL(DFP) :: avar, m
REAL(DFP) :: p(SIZE(x), n + 1)
INTEGER(I4B) :: ii, aint, bint

nrow = SIZE(x)
ncol = n + 1

SELECT CASE (n)
CASE (0)
  ans(1:nrow, 1) = 0.5_DFP * (1.0_DFP - x)

CASE (1)
  ans(1:nrow, 1) = 0.5_DFP * (1.0_DFP - x)
  ans(1:nrow, 2) = 0.5_DFP * (1.0_DFP + x)

CASE DEFAULT
  ans(1:nrow, 1) = 0.5_DFP * (1.0_DFP - x)
  ans(1:nrow, 2) = 0.5_DFP * (1.0_DFP + x)

  CALL LegendreEvalAll_(n=n, x=x, ans=p, nrow=aint, ncol=bint)

  DO ii = 1, n - 1
    m = REAL(ii - 1, KIND=DFP)
    avar = 1.0_DFP / (2.0_DFP * m + 3.0_DFP)
    ans(1:nrow, 2 + ii) = avar * (p(1:nrow, ii + 2) - p(1:nrow, ii))
  END DO

END SELECT
END PROCEDURE UnscaledLobattoEvalAll2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoMonomialExpansionAll
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
    avar = 1.0_DFP / (2.0_DFP * m + 3.0_DFP)
    ans(:, ii + 2) = avar * (p(:, ii + 2) - p(:, ii))
  END DO
  !!
END SELECT
END PROCEDURE UnscaledLobattoMonomialExpansionAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoMonomialExpansion
REAL(DFP) :: coeff(n + 1, n + 1)
coeff = UnscaledLobattoMonomialExpansionAll(n)
ans = coeff(:, n + 1)
END PROCEDURE UnscaledLobattoMonomialExpansion

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoGradientEvalAll1
REAL(DFP) :: p(n)
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
    ans(ii + 2) = p(ii + 1)
    ! ans(3:) = p(2:)
  END DO
  !!
END SELECT
END PROCEDURE UnscaledLobattoGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoGradientEvalAll2
REAL(DFP) :: p(SIZE(x), n)
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
    ans(:, ii + 2) = p(:, ii + 1)
    ! ans(3:) = p(2:)
  END DO
  !!
END SELECT
END PROCEDURE UnscaledLobattoGradientEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoGradientEval1
  !!
SELECT CASE (n)
CASE (0)
  ans = -0.5_DFP
CASE (1)
  ans = 0.5_DFP
CASE DEFAULT
  ans = LegendreEval(n=n - 1_I4B, x=x)
END SELECT
END PROCEDURE UnscaledLobattoGradientEval1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoGradientEval2
SELECT CASE (n)
CASE (0)
  ans = -0.5_DFP
CASE (1)
  ans = 0.5_DFP
CASE DEFAULT
  ans = LegendreEval(n=n - 1_I4B, x=x)
END SELECT
END PROCEDURE UnscaledLobattoGradientEval2

!----------------------------------------------------------------------------
!                                                  UnscaledLobattoMassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoMassMatrix
INTEGER(I4B) :: ii
REAL(DFP) :: m
!!
ans = 0.0_DFP
!!
DO ii = 1, n + 1
  ans(ii, ii) = UnscaledLobattoNormSQR(n=ii - 1_I4B)
END DO
!!
IF (n .EQ. 0_I4B) RETURN
!!
ans(1, 2) = 1.0_DFP / 3.0_DFP
ans(2, 1) = ans(1, 2)
!!
IF (n .EQ. 1_I4B) RETURN
!!
ans(1, 3) = -1.0_DFP / 3.0_DFP
ans(3, 1) = ans(1, 3)
ans(2, 3) = ans(1, 3)
ans(3, 2) = ans(2, 3)
!!
IF (n .EQ. 2_I4B) RETURN
!!
ans(1, 4) = 1.0_DFP / 15.0_DFP
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
    ans(ii, ii + 2) = -2.0_DFP / (2.0_DFP * m + 3.0_DFP) / &
      & (2.0_DFP * m + 5.0_DFP) / (2.0_DFP * m + 7.0_DFP)
    !!
    ans(ii + 2, ii) = ans(ii, ii + 2)
  END IF
  !!
END DO
!!
END PROCEDURE UnscaledLobattoMassMatrix

!----------------------------------------------------------------------------
!                                             UnscaledLobattoStiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE UnscaledLobattoStiffnessMatrix
INTEGER(I4B) :: ii
REAL(DFP) :: m
!!
ans = 0.0_DFP
!!
ans(1, 1) = 0.5_DFP
!!
IF (n .EQ. 0_I4B) RETURN
!!
ans(2, 2) = 0.5_DFP
ans(1, 2) = -0.5_DFP
ans(2, 1) = ans(1, 2)
!!
DO ii = 3, n + 1
  m = REAL(ii - 3, DFP)
  ans(ii, ii) = 2.0_DFP / (2.0_DFP * m + 3.0_DFP)
END DO
END PROCEDURE UnscaledLobattoStiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
