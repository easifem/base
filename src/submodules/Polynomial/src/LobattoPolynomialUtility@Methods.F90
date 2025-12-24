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
USE Sym_LinearSolveMethods, ONLY: SymLinSolve

USE LegendrePolynomialUtility, ONLY: LegendreLeadingCoeff, &
                                     LegendreNormSqr, &
                                     LegendreEval, &
                                     LegendreEvalAll_, &
                                     LegendreMonomialExpansionAll, &
                                     LegendreQuadrature

USE JacobiPolynomialUtility, ONLY: JacobiZeros

USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll_, &
                                           UltrasphericalGradientEvalAll_, &
                                           UltrasphericalGradientEvalAll

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       LobattoTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoTransform1_
INTEGER(I4B) :: ii, jj, nips
REAL(DFP) :: areal(0:n), massmat(0:n, 0:n)

tsize = n + 1
areal = 0.0_DFP
nips = SIZE(coeff)

DO jj = 0, n
  DO ii = 0, nips - 1
    areal(jj) = areal(jj) + PP(ii, jj) * w(ii) * coeff(ii)
  END DO
END DO

massmat = LobattoMassMatrix(n=n)

CALL SymLinSolve(X=ans(0:n), A=massmat(0:n, 0:n), B=areal(0:n))

END PROCEDURE LobattoTransform1_

!----------------------------------------------------------------------------
!                                                          LobattoTransform_
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoTransform2_
REAL(DFP), ALLOCATABLE :: PP(:, :)
INTEGER(I4B) :: ii, jj, nips

nips = SIZE(coeff)
ALLOCATE (PP(nips, n + 1))
CALL LobattoEvalAll_(n=n, x=x, ans=PP, nrow=ii, ncol=jj)
CALL LobattoTransform_(n=n, coeff=coeff, PP=PP, w=w, quadType=quadType, &
                       ans=ans, tsize=tsize)
DEALLOCATE (PP)
END PROCEDURE LobattoTransform2_

!----------------------------------------------------------------------------
!                                                         LobattoTransform_
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoTransform3_
REAL(DFP) :: pt(0:n + 1), wt(0:n + 1), coeff(0:n + 1), x
REAL(DFP), PARAMETER :: one = 1.0_DFP, half = 0.5_DFP
INTEGER(I4B) :: ii, nips

nips = n + 2
CALL LegendreQuadrature(n=nips, pt=pt, wt=wt, quadType=quadType)
!! We are using n+2 quadrature points as it works well in case of
!! GaussLobatto quadrature points also

DO ii = 0, nips - 1
  x = (one - pt(ii)) * x1 + (one + pt(ii)) * x2
  x = x * half
  coeff(ii) = f(x)
END DO

CALL LobattoTransform_(n=n, coeff=coeff, x=pt, w=wt, quadType=quadType, &
                       ans=ans, tsize=tsize)

END PROCEDURE LobattoTransform3_

!----------------------------------------------------------------------------
!                                                       LobattoLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoLeadingCoeff
REAL(DFP) :: avar, m

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
INTEGER(I4B) :: tsize
CALL LobattoEvalAll1_(n=n, x=x, ans=ans, tsize=tsize)
END PROCEDURE LobattoEvalAll1

!----------------------------------------------------------------------------
!                                                            LobattoEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEvalAll1_
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
    avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
    ans(ii + 2) = avar * (p(ii + 2) - p(ii))
  END DO

END SELECT
END PROCEDURE LobattoEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL LobattoEvalAll2_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LobattoEvalAll2

!----------------------------------------------------------------------------
!                                                             LobattoEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoEvalAll2_
REAL(DFP) :: avar, m
REAL(DFP) :: p(SIZE(x), n + 1)
INTEGER(I4B) :: ii, aint, bint

nrow = SIZE(x)
ncol = 1 + n

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
    avar = 1.0_DFP / SQRT(2.0_DFP * (2.0_DFP * m + 3.0_DFP))
    ans(1:nrow, 2 + ii) = avar * (p(1:nrow, ii + 2) - p(1:nrow, ii))
  END DO

END SELECT
END PROCEDURE LobattoEvalAll2_

!----------------------------------------------------------------------------
!                                                       LobattoKernelEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoKernelEvalAll1
INTEGER(I4B) :: nrow, ncol
CALL LobattoKernelEvalAll1_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LobattoKernelEvalAll1

!----------------------------------------------------------------------------
!                                                       LobattoKernelEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoKernelEvalAll1_
REAL(DFP) :: m, avar
INTEGER(I4B) :: ii

CALL UltrasphericalEvalAll_(n=n, x=x, lambda=1.5_DFP, ans=ans, nrow=nrow, &
                            ncol=ncol)

DO ii = 0, n
  m = REAL(ii, KIND=DFP)
  avar = -SQRT(8.0_DFP*(2.0_DFP*m+3.0_DFP))/(m+1.0_DFP)/(m+2.0_DFP)
  ans(1:nrow, ii) = avar * ans(1:nrow, ii)
END DO
END PROCEDURE LobattoKernelEvalAll1_

!----------------------------------------------------------------------------
!                                             LobattoKernelGradientEvalAll1
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoKernelGradientEvalAll1
REAL(DFP) :: m, avar
INTEGER(I4B) :: ii

ans = UltrasphericalGradientEvalAll(n=n, x=x, lambda=1.5_DFP)
DO ii = 0, n
  m = REAL(ii, KIND=DFP)
  avar = -SQRT(8.0_DFP*(2.0_DFP*m+3.0_DFP))/(m+1.0_DFP)/(m+2.0_DFP)
  ans(:, ii) = avar * ans(:, ii)
END DO
END PROCEDURE LobattoKernelGradientEvalAll1

!----------------------------------------------------------------------------
!                                             LobattoKernelGradientEvalAll1
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoKernelGradientEvalAll1_
REAL(DFP) :: m, avar
INTEGER(I4B) :: ii

CALL UltrasphericalGradientEvalAll_(n=n, x=x, lambda=1.5_DFP, nrow=nrow, &
                                    ncol=ncol, ans=ans)
DO CONCURRENT(ii=0:n)
  m = REAL(ii, KIND=DFP)
  avar = -SQRT(8.0_DFP*(2.0_DFP*m+3.0_DFP))/(m+1.0_DFP)/(m+2.0_DFP)
  ans(1:nrow, ii) = avar * ans(1:nrow, ii)
END DO
END PROCEDURE LobattoKernelGradientEvalAll1_

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
INTEGER(I4B) :: tsize
CALL LobattoGradientEvalAll1_(n=n, x=x, ans=ans, tsize=tsize)
END PROCEDURE LobattoGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEvalAll1_
REAL(DFP) :: p(n), avar, m
INTEGER(I4B) :: ii

tsize = n + 1

SELECT CASE (n)

CASE (0)
  ans(1) = -0.5_DFP

CASE (1)
  ans(1) = -0.5_DFP
  ans(2) = 0.5_DFP

CASE DEFAULT
  ans(1) = -0.5_DFP
  ans(2) = 0.5_DFP

  CALL LegendreEvalAll_(n=n - 1_I4B, x=x, ans=p, tsize=ii)

  DO ii = 1, n - 1
    m = REAL(ii - 1, DFP)
    avar = SQRT((2.0_DFP * m + 3.0) / 2.0)
    ans(ii + 2) = avar * p(ii + 1)

  END DO

END SELECT
END PROCEDURE LobattoGradientEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL LobattoGradientEvalAll2_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LobattoGradientEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LobattoGradientEvalAll2_
REAL(DFP) :: p(SIZE(x), n), avar, m
INTEGER(I4B) :: ii

nrow = SIZE(x)
ncol = n + 1

SELECT CASE (n)
CASE (0)
  ans(1:nrow, 1) = -0.5_DFP

CASE (1)
  ans(1:nrow, 1) = -0.5_DFP
  ans(1:nrow, 2) = 0.5_DFP

CASE DEFAULT
  ans(1:nrow, 1) = -0.5_DFP
  ans(1:nrow, 2) = 0.5_DFP

  CALL LegendreEvalAll_(n=n - 1_I4B, x=x, ans=p, nrow=nrow, ncol=ii)

  DO ii = 1, n - 1
    m = REAL(ii - 1, DFP)
    avar = SQRT((2.0_DFP * m + 3.0) / 2.0)
    ans(1:nrow, ii + 2) = avar * p(1:nrow, ii + 1)
    ! ans(3:) = p(2:)
  END DO

END SELECT

END PROCEDURE LobattoGradientEvalAll2_

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

ans = 0.0_DFP

DO ii = 1, n + 1
  ans(ii, ii) = 1.0_DFP
END DO

ans(1, 1) = 0.5_DFP

IF (n .EQ. 0_I4B) RETURN

ans(2, 2) = 0.5_DFP
ans(1, 2) = -0.5_DFP
ans(2, 1) = ans(1, 2)

END PROCEDURE LobattoStiffnessMatrix

!----------------------------------------------------------------------------
!                                                                 Lobatto0
!----------------------------------------------------------------------------

MODULE PROCEDURE Lobatto0
ans = 0.5_DFP * (1.0_DFP - x)
END PROCEDURE Lobatto0

MODULE PROCEDURE Lobatto1
ans = 0.5_DFP * (1.0_DFP + x)
END PROCEDURE Lobatto1

MODULE PROCEDURE Lobatto2
REAL(DFP), PARAMETER :: coeff = 0.5_DFP * SQRT(3.0_DFP) / SQRT(2.0_DFP)
ans = coeff * (x**2 - 1.0_DFP)
END PROCEDURE Lobatto2

MODULE PROCEDURE Lobatto3
REAL(DFP), PARAMETER :: coeff = 0.5_DFP * SQRT(5.0_DFP) / SQRT(2.0_DFP)
ans = coeff * (x**2 - 1.0_DFP) * x
END PROCEDURE Lobatto3

MODULE PROCEDURE Lobatto4
REAL(DFP), PARAMETER :: coeff = SQRT(7.0_DFP) / SQRT(2.0_DFP) / 8.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (5.0_DFP * x**2 - 1.0_DFP)
END PROCEDURE Lobatto4

MODULE PROCEDURE Lobatto5
REAL(DFP), PARAMETER :: coeff = SQRT(9.0_DFP) / SQRT(2.0_DFP) / 8.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (7.0_DFP * x**2 - 3.0_DFP) * x
END PROCEDURE Lobatto5

MODULE PROCEDURE Lobatto6
REAL(DFP), PARAMETER :: coeff = SQRT(11.0_DFP) / SQRT(2.0_DFP) / 16.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (21.0_DFP * x**4 - 14.0_DFP * x**2 + 1.0_DFP)
END PROCEDURE Lobatto6

MODULE PROCEDURE Lobatto7
REAL(DFP), PARAMETER :: coeff = SQRT(13.0_DFP) / SQRT(2.0_DFP) / 16.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (33.0_DFP * x**4 - 30.0_DFP * x**2 + 5.0_DFP) * x
END PROCEDURE Lobatto7

MODULE PROCEDURE Lobatto8
REAL(DFP), PARAMETER :: coeff = SQRT(15.0_DFP) / SQRT(2.0_DFP) / 128.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (429.0_DFP * x**6 - 495.0_DFP * x**4 &
                                  + 135.0_DFP * x**2 - 5.0_DFP)
END PROCEDURE Lobatto8

MODULE PROCEDURE Lobatto9
REAL(DFP), PARAMETER :: coeff = SQRT(17.0_DFP) / SQRT(2.0_DFP) / 128.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (715.0_DFP * x**6 - 1001.0_DFP * x**4 &
                                  + 385.0_DFP * x**2 - 35.0_DFP) * x
END PROCEDURE Lobatto9

MODULE PROCEDURE Lobatto10
REAL(DFP), PARAMETER :: coeff = SQRT(19.0_DFP) / SQRT(2.0_DFP) / 256.0_DFP
ans = coeff * (x**2 - 1.0_DFP) * (2431.0_DFP * x**8 - 4004.0_DFP * x**6 &
                             + 2002.0_DFP * x**4 - 308.0_DFP * x**2 + 7.0_DFP)
END PROCEDURE Lobatto10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
