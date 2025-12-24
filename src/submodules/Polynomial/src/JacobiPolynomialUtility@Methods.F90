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

SUBMODULE(JacobiPolynomialUtility) Methods
USE OrthogonalPolynomialUtility, ONLY: JacobiMatrix

#ifdef USE_LAPACK95
USE F95_Lapack, ONLY: STEV
#endif

USE ErrorHandling, ONLY: ErrorMsg

USE MiscUtility, ONLY: Factorial

USE BaseType, ONLY: qp => TypeQuadratureOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                JacobiAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiAlpha
IF (n .EQ. 0) THEN
  ans = (beta - alpha) / (alpha + beta + 2.0_DFP)
ELSE
  ans = (beta**2 - alpha**2) / (alpha + beta + 2.0_DFP * n) &
    & / (alpha + beta + 2.0_DFP + 2.0_DFP * n)
END IF
END PROCEDURE JacobiAlpha

!----------------------------------------------------------------------------
!                                                                 JacobiBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiBeta
IF (n .EQ. 0) THEN
  ans = 2.0_DFP**(alpha + beta + 1.0_DFP) * GAMMA(alpha + 1.0_DFP) &
  & * GAMMA(beta + 1.0_DFP) &
  & / GAMMA(alpha + beta + 2.0_DFP)
ELSEIF (n .EQ. 1) THEN
  ans = 4.0_DFP * (1.0_DFP + alpha) * (1.0_DFP + beta) / &
    & (alpha + beta + 2.0_DFP)**2 / (alpha + beta + 3.0_DFP)
ELSE
  ans = 4.0_DFP * n * (n + alpha) * (n + beta) * (n + alpha + beta) &
    & / (alpha + beta + 2.0_DFP * n)**2 / (alpha + beta + 1.0_DFP + 2.0 * n) &
    & / (alpha + beta - 1.0_DFP + 2.0 * n)
END IF
END PROCEDURE JacobiBeta

!----------------------------------------------------------------------------
!                                                   GetJacobiRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE GetJacobiRecurrenceCoeff
REAL(DFP), PARAMETER :: two = 2.0_DFP, four = 4.0_DFP
REAL(DFP) :: ab1, ab, ab2, abm1, bma, ab3, b2ma2, ab4
INTEGER(I4B) :: ii
  !!
IF (n .LE. 0) RETURN
  !!
ab = alpha + beta
ab1 = ab + 1.0_DFP
abm1 = ab - 1.0_DFP
bma = beta - alpha
ab2 = ab1 + 1.0_DFP
ab3 = ab2 + 1.0_DFP
ab4 = ab3 + 1.0_DFP
b2ma2 = beta * beta - alpha * alpha
  !!
  !! beta 0
  !!
betaCoeff(0) = two**(ab1) * GAMMA(alpha + 1.0_DFP) &
  & * GAMMA(beta + 1.0_DFP) &
  & / GAMMA(ab1 + 1.0_DFP)
  !!
  !! alpha 0
  !!
alphaCoeff(0) = bma / ab2
  !!
  !! RETURN IF n = 1
  !!
IF (n .EQ. 1) RETURN
  !!
betaCoeff(1) = four * (1.0_DFP + alpha) * (1.0_DFP + beta) / (ab2 * ab2 * ab3)
alphaCoeff(1) = b2ma2 / (ab2 * ab4)
  !!
  !! Now it safe to compute other coefficients
  !!
DO ii = 2, n - 1
    !!
  betaCoeff(ii) = four * ii * (ii + alpha) * (ii + beta) * (ii + ab) &
    & / (ab + 2.0 * ii)**2 / (ab1 + 2.0 * ii) / (abm1 + 2.0 * ii)
    !!
  alphaCoeff(ii) = b2ma2 / (ab + 2.0 * ii) / (ab2 + 2.0 * ii)
    !!
END DO
  !!
END PROCEDURE GetJacobiRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                GetJacobiRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetJacobiRecurrenceCoeff2
REAL(DFP) :: j
INTEGER(I4B) :: ii
!!
IF (n .LT. 1) RETURN
A(0) = 0.5_DFP * (alpha + beta + 2.0_DFP)
B(0) = -A(0) * JacobiAlpha(n=0_I4B, alpha=alpha, beta=beta)
j = JacobiBeta(n=0_I4B, alpha=alpha, beta=beta)
C(0) = SQRT(j) * A(0)
!!
IF (n .EQ. 1) RETURN
!!
DO ii = 2, n
  j = REAL(ii, KIND=DFP)
  A(ii-1) = (2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta)); 
  B(ii - 1) = (alpha * alpha - beta * beta) * (2 * j + alpha + beta - 1) &
    & / (2 * j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  C(ii - 1) = (j - 1 + alpha) * (j - 1 + beta) * (2 * j + alpha + beta) &
    & / (j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
END DO
!!
END PROCEDURE GetJacobiRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                       JacobiLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiLeadingCoeff
ans = GAMMA(2.0_DFP * n + alpha + beta + 1.0_DFP) / GAMMA(n + 1.0_DFP) / &
  & GAMMA(n + alpha + beta + 1.0_DFP) / 2.0_DFP**n
END PROCEDURE JacobiLeadingCoeff

!----------------------------------------------------------------------------
!                                                  JacobiLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiLeadingCoeffRatio
REAL(DFP) :: a1, a2, rn
IF (n .EQ. 0) THEN
  ans = 0.5_DFP * (alpha + beta + 2.0_DFP)
ELSE
  rn = REAL(n, KIND=DFP)
  a1 = 2.0_DFP * rn + alpha + beta + 1.0_DFP
  ans = 0.5_DFP * a1 * (a1 + 1.0_DFP) / (rn + 1.0_DFP) / (a1 - rn)
END IF
END PROCEDURE JacobiLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                             JacobiNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiNormSqr
REAL(DFP) :: a1, a2, a3, b1, b2, b3
a1 = 2.0**(alpha + beta + 1.0_DFP)
a2 = GAMMA(n + alpha + 1.0_DFP)
a3 = GAMMA(n + beta + 1.0_DFP)
b1 = 2.0_DFP * n + alpha + beta + 1.0_DFP
b2 = Factorial(n)
b3 = GAMMA(n + alpha + beta + 1.0_DFP)
ans = a1 * a2 * a3 / b1 / b2 / b3
END PROCEDURE JacobiNormSqr

!----------------------------------------------------------------------------
!                                                             JacobiNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiNormSqr2
REAL(DFP) :: rn, s
INTEGER(I4B) :: ii
!!
ans(0) = JacobiNormSQR(n=0_I4B, alpha=alpha, beta=beta)
!!
IF (n .EQ. 0) RETURN
!!
s = JacobiNormSQRRatio(n=0_I4B, alpha=alpha, beta=beta)
ans(1) = ans(0) * s
!!
DO ii = 1, n - 1
  rn = REAL(ii, KIND=DFP)
  s = (rn + alpha + 1.0_DFP) * (rn + beta + 1.0_DFP) * &
    & (2.0_DFP * rn + alpha + beta + 1.0_DFP) / (rn + 1.0_DFP) &
    & / (2.0_DFP * rn + alpha + beta + 3.0_DFP) &
    & / (rn + alpha + beta + 1.0_DFP)
  ans(ii + 1) = s * ans(ii)
END DO
END PROCEDURE JacobiNormSqr2

!----------------------------------------------------------------------------
!                                                         JacobiNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiNormSqrRatio
REAL(DFP) :: rn
IF (n .EQ. 0) THEN
  ans = (1.0_DFP + alpha) * (1.0_DFP + beta) / (3.0_DFP + alpha + beta)
ELSE
  rn = REAL(n, KIND=DFP)
  ans = (rn + alpha + 1.0_DFP) * (rn + beta + 1.0_DFP) * &
    & (2.0_DFP * rn + alpha + beta + 1.0_DFP) / (rn + 1.0_DFP) &
    & / (2.0_DFP * rn + alpha + beta + 3.0_DFP) &
    & / (rn + alpha + beta + 1.0_DFP)
END IF
END PROCEDURE JacobiNormSqrRatio

!----------------------------------------------------------------------------
!                                                         JacobiJacobiMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiJacobiMatrix
REAL(DFP), DIMENSION(0:n - 1) :: alphaCoeff0, betaCoeff0
  !!
IF (n .LT. 1) RETURN
  !!
CALL GetJacobiRecurrenceCoeff(n=n, alpha=alpha, beta=beta, &
  & alphaCoeff=alphaCoeff0, betaCoeff=betaCoeff0)
IF (PRESENT(alphaCoeff)) alphaCoeff(0:n - 1) = alphaCoeff0
IF (PRESENT(betaCoeff)) betaCoeff(0:n - 1) = betaCoeff0
CALL JacobiMatrix(alphaCoeff=alphaCoeff0, &
  & betaCoeff=betaCoeff0, D=D, E=E)
  !!
END PROCEDURE JacobiJacobiMatrix

!----------------------------------------------------------------------------
!                                                     JacobiGaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGaussQuadrature
REAL(DFP) :: beta0, Z(n, n), betaCoeff(0:n - 1), pn(n)
INTEGER(I4B) :: ii
  !!
CALL JacobiJacobiMatrix(n=n, alpha=alpha, beta=beta, D=pt, &
  & E=pn, betaCoeff=betaCoeff)
  !!
#ifdef USE_LAPACK95
IF (PRESENT(wt)) THEN
  wt = pn
  CALL STEV(D=pt, E=wt, Z=Z)
  DO ii = 1, n
    wt(ii) = betaCoeff(0) * Z(1, ii)**2
  END DO
ELSE
  CALL STEV(D=pt, E=pn)
END IF
  !!
#else
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="JacobiGaussQuadrature", &
  & line=__LINE__, &
  & unitno=stdout)
#endif
  !!
END PROCEDURE JacobiGaussQuadrature

!----------------------------------------------------------------------------
!                                                    JacobiJacobiRadauMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiJacobiRadauMatrix
REAL(DFP) :: avar, r1, r2, r3, ab, ab2
  !!
IF (n .LT. 1) RETURN
  !!
CALL JacobiJacobiMatrix(n=n, alpha=alpha, beta=beta, &
  & D=D, E=E, alphaCoeff=alphaCoeff, betaCoeff=betaCoeff)
  !!
r1 = (1.0 - a) * n * (n + alpha) - (1.0 + a) * n * (n + beta)
r2 = 2.0 * n + alpha + beta
r3 = r2 + 1.0
avar = a + r1 / r2 / r3
D(n + 1) = avar
  !!
ab = alpha + beta
ab2 = ab + 2.0_DFP
IF (n .EQ. 1) THEN
  avar = 4.0_DFP * (1.0_DFP+alpha) * (1.0_DFP+beta) / (ab2*ab2*(ab2+1.0))
ELSE
  avar = 4.0_DFP * n * (n + alpha) * (n + beta) * (n + ab) &
    & / (ab + 2.0 * n)**2 / (ab + 1.0 + 2.0 * n) / (ab - 1.0 + 2.0 * n)
END IF
  !!
E(n) = SQRT(avar)
  !!
END PROCEDURE JacobiJacobiRadauMatrix

!----------------------------------------------------------------------------
!                                                JacobiGaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGaussRadauQuadrature
  !!
REAL(DFP) :: beta0, Z(n + 1, n + 1), betaCoeff(0:n), pn(n + 1)
INTEGER(I4B) :: ii
  !!
CALL JacobiJacobiRadauMatrix(a=a, n=n, alpha=alpha, beta=beta, D=pt, &
  & E=pn, betaCoeff=betaCoeff)
  !!
#ifdef USE_LAPACK95
  !!
IF (PRESENT(wt)) THEN
  wt = pn
  CALL STEV(D=pt, E=wt, Z=Z)
  DO ii = 1, n + 1
    wt(ii) = betaCoeff(0) * Z(1, ii)**2
  END DO
ELSE
  CALL STEV(D=pt, E=pn)
END IF
  !!
#else
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="JacobiGaussRadauQuadrature", &
  & line=__LINE__, &
  & unitno=stdout)
#endif
  !!
END PROCEDURE JacobiGaussRadauQuadrature

!----------------------------------------------------------------------------
!                                                  JacobiJacobiLobattoMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiJacobiLobattoMatrix
  !!
REAL(DFP) :: avar, r1, r2, r3, ab
  !!
IF (n .LT. 0) RETURN
  !!
CALL JacobiJacobiMatrix(  &
  & n=n + 1, &
  & alpha=alpha, &
  & beta=beta, &
  & D=D, &
  & E=E, &
  & alphaCoeff=alphaCoeff, &
  & betaCoeff=betaCoeff)
  !!
r1 = alpha - beta
r2 = 2.0 * n + alpha + beta + 2.0_DFP
r3 = 1.0
avar = r1 / r2 / r3
D(n + 2) = avar
  !!
ab = alpha + beta
r1 = 4.0_DFP * (n + alpha + 1.0) * (n + beta + 1.0) * (n + ab + 1.0)
r2 = 2.0 * n + ab + 1.0
r3 = (r2 + 1.0)**2
  !!
E(n + 1) = SQRT(r1 / r2 / r3)
  !!
END PROCEDURE JacobiJacobiLobattoMatrix

!----------------------------------------------------------------------------
!                                              JacobiGaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGaussLobattoQuadrature
  !!
REAL(DFP) :: beta0, Z(n + 2, n + 2), betaCoeff(0:n + 1), pn(n + 2)
INTEGER(I4B) :: ii
  !!
CALL JacobiJacobiLobattoMatrix(n=n, alpha=alpha, beta=beta, D=pt, &
  & E=pn, betaCoeff=betaCoeff)
!!
#ifdef USE_LAPACK95
IF (PRESENT(wt)) THEN
  wt = pn
  CALL STEV(D=pt, E=wt, Z=Z)
  DO ii = 1, n + 2
    wt(ii) = betaCoeff(0) * Z(1, ii)**2
  END DO
ELSE
  CALL STEV(D=pt, E=pn)
END IF
  !!
#else
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="JacobiGaussLobattoQuadrature", &
  & line=__LINE__, &
  & unitno=stdout)
#endif
  !!
END PROCEDURE JacobiGaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                               JacobiZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiZeros
  !!
REAL(DFP) :: E(n)
  !!
CALL JacobiJacobiMatrix( &
  & n=n, &
  & alpha=alpha, &
  & beta=beta, &
  & D=ans, &
  & E=E)
  !!
#ifdef USE_LAPACK95
  !!
CALL STEV(D=ans, E=E)
  !!
#else
  !!
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="JacobiZeros", &
  & line=__LINE__, &
  & unitno=stdout)
  !!
#endif
  !!
END PROCEDURE JacobiZeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiQuadrature
INTEGER(I4B) :: order
REAL(DFP), PARAMETER :: left = -1.0_DFP, right = 1.0_DFP
  !!
SELECT CASE (quadType)
CASE (qp%Gauss)
  order = n
  CALL JacobiGaussQuadrature(n=order, alpha=alpha, beta=beta, &
    & pt=pt, wt=wt)
CASE (qp%GaussRadau, qp%GaussRadauLeft)
  order = n - 1
  CALL JacobiGaussRadauQuadrature(a=left, n=order, alpha=alpha, beta=beta, &
    & pt=pt, wt=wt)
CASE (qp%GaussRadauRight)
  order = n - 1
  CALL JacobiGaussRadauQuadrature(a=right, n=order, alpha=alpha, beta=beta, &
    & pt=pt, wt=wt)
CASE (qp%GaussLobatto)
  order = n - 2
  CALL JacobiGaussLobattoQuadrature(n=order, alpha=alpha, beta=beta, &
    & pt=pt, wt=wt)
END SELECT
END PROCEDURE JacobiQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEvalAll1
INTEGER(I4B) :: tsize
CALL JacobiEvalAll1_(n=n, x=x, alpha=alpha, beta=beta, ans=ans, tsize=tsize)
END PROCEDURE JacobiEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEvalAll1_
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, c4, r_i, apb, amb, r2, apb_minus_2, apb_minus_1, &
             alpha_minus_1, beta_minus_1

tsize = 0

IF (alpha <= -1.0_DFP) RETURN
IF (beta <= -1.0_DFP) RETURN

IF (n < 0) RETURN

tsize = 1 + n
ans(1) = 1.0_DFP

IF (n .EQ. 0) RETURN

apb = alpha + beta
apb_minus_2 = apb - 2.0_DFP
apb_minus_1 = apb - 1.0_DFP
alpha_minus_1 = alpha - 1.0_DFP
beta_minus_1 = beta - 1.0_DFP
amb = alpha - beta

ans(2) = (1.0_DFP + 0.5_DFP * apb) * x + 0.5_DFP * amb

DO i = 2, n

  r_i = REAL(i, kind=DFP)
  r2 = 2.0_DFP * r_i

  c1 = r2 * (r_i + apb) * (r2 + apb_minus_2)

  c2 = (r2 + apb_minus_1) * (r2 + apb) * (r2 + apb_minus_2)
  c2 = c2 / c1

  c3 = (r2 + apb_minus_1) * apb * amb
  c3 = c3 / c1

  c4 = -2.0_DFP * (r_i + alpha_minus_1) * (r_i + beta_minus_1) * (r2 + apb)

  c4 = c4 / c1

  ans(i + 1) = (c3 + c2 * x) * ans(i) + c4 * ans(i - 1)

END DO

END PROCEDURE JacobiEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL JacobiEvalAll2_(n=n, alpha=alpha, beta=beta, x=x, ans=ans, nrow=nrow, &
                     ncol=ncol)
END PROCEDURE JacobiEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEvalAll2_
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, c4, r_i, apb, amb, r2, apb_minus_2, apb_minus_1, &
             alpha_minus_1, beta_minus_1

nrow = 0
ncol = 0
IF (alpha <= -1.0_DFP) RETURN
IF (beta <= -1.0_DFP) RETURN
IF (n < 0) RETURN

nrow = SIZE(x)
ncol = 1 + n

ans(1:nrow, 1) = 1.0_DFP

IF (n .EQ. 0) RETURN

apb = alpha + beta
apb_minus_2 = apb - 2.0_DFP
apb_minus_1 = apb - 1.0_DFP
alpha_minus_1 = alpha - 1.0_DFP
beta_minus_1 = beta - 1.0_DFP

ans(1:nrow, 2) = (1.0_DFP + 0.5_DFP * apb) * x + 0.5_DFP * amb

DO i = 2, n

  r_i = REAL(i, kind=DFP)
  r2 = 2.0_DFP * r_i

  c1 = r2 * (r_i + apb) * (r2 + apb_minus_2)

  c2 = (r2 + apb_minus_1) * (r2 + apb) * (r2 + apb_minus_2)
  c2 = c2 / c1

  c3 = (r2 + apb_minus_1) * apb * amb
  c3 = c3 / c1

  c4 = -2.0_DFP * (r_i + alpha_minus_1) * (r_i + beta_minus_1) * (r2 + apb)
  c4 = c4 / c1

  ans(1:nrow, i + 1) = (c3 + c2 * x) * ans(1:nrow, i) &
                       + c4 * ans(1:nrow, i - 1)

END DO

END PROCEDURE JacobiEvalAll2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEval1
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, c4, r_i, ans_1, ans_2
!!
ans = 0.0_DFP
!!
IF (alpha <= -1.0_DFP) THEN
  RETURN
END IF
!!
IF (beta <= -1.0_DFP) THEN
  RETURN
END IF
!!
IF (n < 0) THEN
  RETURN
END IF
!!
ans = 1.0_DFP
ans_2 = ans
!!
IF (n .EQ. 0) THEN
  RETURN
END IF
!!
ans = (1.0_DFP + 0.5_DFP * (alpha + beta)) * x &
  & + 0.5_DFP * (alpha - beta)
!!
DO i = 2, n
  !!
  r_i = REAL(i, kind=DFP)
  !!
  c1 = 2.0_DFP * r_i * (r_i + alpha + beta) &
      & * (2.0_DFP * r_i - 2.0_DFP + alpha + beta)
  !!
  c2 = (2.0_DFP * r_i - 1.0_DFP + alpha + beta) &
      & * (2.0_DFP * r_i + alpha + beta) &
      & * (2.0_DFP * r_i - 2.0_DFP + alpha + beta)
  !!
  c3 = (2.0_DFP * r_i - 1.0_DFP + alpha + beta) &
      & * (alpha + beta) * (alpha - beta)
  !!
  c4 = -2.0_DFP * (r_i - 1.0_DFP + alpha) &
      & * (r_i - 1.0_DFP + beta) * (2.0_DFP * r_i + alpha + beta)
  !!
  ans_1 = ans
  ans = ((c3 + c2 * x) * ans + c4 * ans_2) / c1
  ans_2 = ans_1
  !!
END DO
END PROCEDURE JacobiEval1

!----------------------------------------------------------------------------
!                                                                 JacobiEval
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEval2
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, c4, r_i
REAL(DFP), DIMENSION(SIZE(x)) :: ans_1, ans_2
!!
ans = 0.0_DFP
!!
IF (alpha <= -1.0_DFP) THEN
  RETURN
END IF
!!
IF (beta <= -1.0_DFP) THEN
  RETURN
END IF
!!
IF (n < 0) THEN
  RETURN
END IF
!!
ans = 1.0_DFP
ans_2 = ans
!!
IF (n .EQ. 0) THEN
  RETURN
END IF
!!
ans = (1.0_DFP + 0.5_DFP * (alpha + beta)) * x &
  & + 0.5_DFP * (alpha - beta)
!!
DO i = 2, n
  !!
  r_i = REAL(i, kind=DFP)
  !!
  c1 = 2.0_DFP * r_i * (r_i + alpha + beta) &
      & * (2.0_DFP * r_i - 2.0_DFP + alpha + beta)
  !!
  c2 = (2.0_DFP * r_i - 1.0_DFP + alpha + beta) &
      & * (2.0_DFP * r_i + alpha + beta) &
      & * (2.0_DFP * r_i - 2.0_DFP + alpha + beta)
  !!
  c3 = (2.0_DFP * r_i - 1.0_DFP + alpha + beta) &
      & * (alpha + beta) * (alpha - beta)
  !!
  c4 = -2.0_DFP * (r_i - 1.0_DFP + alpha) &
      & * (r_i - 1.0_DFP + beta) * (2.0_DFP * r_i + alpha + beta)
  !!
  ans_1 = ans
  ans = ((c3 + c2 * x) * ans + c4 * ans_2) / c1
  ans_2 = ans_1
  !!
END DO
END PROCEDURE JacobiEval2

!----------------------------------------------------------------------------
!                                                             JacobiEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEvalSum1
REAL(DFP) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP), DIMENSION(0:n + 1) :: A, B, C
!!
IF (n .LT. 0) RETURN
IF (alpha .LE. -1.0_DFP) RETURN
IF (beta .LE. -1.0_DFP) RETURN
!!
CALL GetJacobiRecurrenceCoeff2(n=n + 2, alpha=alpha, beta=beta, A=A, B=B, C=C)
!!
b1 = 0.0_DFP
b2 = 0.0_DFP
!!
DO j = n, 0, -1
  t = (A(j) * x + B(j)) * b1 - C(j + 1) * b2 + coeff(j); 
  b2 = b1
  b1 = t
END DO
!!
ans = b1
!!
END PROCEDURE JacobiEvalSum1

!----------------------------------------------------------------------------
!                                                             JacobiEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiEvalSum2
REAL(DFP), DIMENSION(SIZE(x)) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP), DIMENSION(0:n + 1) :: A, B, C
!!
IF (n .LT. 0) RETURN
IF (alpha .LE. -1.0_DFP) RETURN
IF (beta .LE. -1.0_DFP) RETURN
!!
CALL GetJacobiRecurrenceCoeff2(n=n + 2, alpha=alpha, beta=beta, A=A, B=B, C=C)
!!
b1 = 0.0_DFP
b2 = 0.0_DFP
!!
DO j = n, 0, -1
  t = (A(j) * x + B(j)) * b1 - C(j + 1) * b2 + coeff(j); 
  b2 = b1
  b1 = t
END DO
!!
ans = b1
!!
END PROCEDURE JacobiEvalSum2

!----------------------------------------------------------------------------
!                                                     JacobiGradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEval1
!!
INTEGER(I4B) :: ii
REAL(DFP) :: j
REAL(DFP) :: p, p_1, p_2
REAL(DFP) :: ans_1, ans_2
REAL(DFP) :: ab, amb, a1, a2, a3, b1, b2, b3
!!
IF (n < 0) THEN
  RETURN
END IF
!!
p = 1.0_DFP
ans = 0.0_DFP
p_2 = p
ans_2 = ans
!!
IF (n < 1) THEN
  RETURN
END IF
!!
!!
ab = alpha + beta
amb = alpha - beta
p = 0.5 * (ab + 2.0) * x + 0.5 * amb
ans = 0.5 * (ab + 2.0)
!!
DO ii = 2, n
  !!
  j = REAL(ii, KIND=DFP)
  !!
  p_1 = p
  !!
  a1 = (2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta)); 
  a2 = (alpha * alpha - beta * beta) * (2 * j + alpha + beta - 1) &
    & / (2 * j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  a3 = (j - 1 + alpha) * (j - 1 + beta) * (2 * j + alpha + beta) &
    & / (j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  !!
  p = (a1 * x + a2) * p - a3 * p_2
  !!
  p_2 = p_1
  !!
  ans_1 = ans
  !!
  j = j - 1.0
  b1 = -2.0*(j+alpha)*(j+beta)/(j+ab)/(2.0*j+ab)/(2.0*j+ab+1.0)
  b2 = 2.0 * amb / (2.0 * j + ab) / (2.0 * j + ab + 2.0)
  b3 = 2.0 * (j + ab + 1.0) / (2.0 * j + ab + 1.0) / (2.0 * j + ab + 2.0)
  !!
  ans = (p_1 - b1 * ans_2 - b2 * ans_1) / b3
  ans_2 = ans_1
  !!
END DO
!!
END PROCEDURE JacobiGradientEval1

!----------------------------------------------------------------------------
!                                                     JacobiGradientEval
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEval2
!!
INTEGER(I4B) :: ii
REAL(DFP) :: j
REAL(DFP), DIMENSION(SIZE(x)) :: p, p_1, p_2, ans_1, ans_2
REAL(DFP) :: ab, amb, a1, a2, a3, b1, b2, b3
!!
IF (n < 0) THEN
  RETURN
END IF
!!
p = 1.0_DFP
ans = 0.0_DFP
p_2 = p
ans_2 = ans
!!
IF (n < 1) THEN
  RETURN
END IF
!!
!!
ab = alpha + beta
amb = alpha - beta
p = 0.5 * (ab + 2.0) * x + 0.5 * amb
ans = 0.5 * (ab + 2.0)
!!
DO ii = 2, n
  !!
  j = REAL(ii, KIND=DFP)
  !!
  p_1 = p
  !!
  a1 = (2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta)); 
  a2 = (alpha * alpha - beta * beta) * (2 * j + alpha + beta - 1) &
    & / (2 * j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  a3 = (j - 1 + alpha) * (j - 1 + beta) * (2 * j + alpha + beta) &
    & / (j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  !!
  p = (a1 * x + a2) * p - a3 * p_2
  !!
  p_2 = p_1
  !!
  ans_1 = ans
  !!
  j = j - 1.0
  b1 = -2.0*(j+alpha)*(j+beta)/(j+ab)/(2.0*j+ab)/(2.0*j+ab+1.0)
  b2 = 2.0 * amb / (2.0 * j + ab) / (2.0 * j + ab + 2.0)
  b3 = 2.0 * (j + ab + 1.0) / (2.0 * j + ab + 1.0) / (2.0 * j + ab + 2.0)
  !!
  ans = (p_1 - b1 * ans_2 - b2 * ans_1) / b3
  ans_2 = ans_1
  !!
END DO
!!
END PROCEDURE JacobiGradientEval2

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalAll1
INTEGER(I4B) :: tsize
CALL JacobiGradientEvalAll1_(n=n, alpha=alpha, beta=beta, x=x, ans=ans, &
                             tsize=tsize)
END PROCEDURE JacobiGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalAll1_
INTEGER(I4B) :: ii
REAL(DFP) :: j
REAL(DFP), DIMENSION(n + 1) :: p
REAL(DFP) :: ab, amb, a1, a2, a3, b1, b2, b3

tsize = 0

IF (n < 0) RETURN

tsize = n + 1

p(1) = 1.0_DFP
ans(1) = 0.0_DFP

IF (n < 1) RETURN

ab = alpha + beta
amb = alpha - beta
p(2) = 0.5 * (ab + 2.0) * x + 0.5 * amb
ans(2) = 0.5 * (ab + 2.0)

DO ii = 2, n

  j = REAL(ii, KIND=DFP)

  a1 = (2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta)); 
  a2 = (alpha * alpha - beta * beta) * (2 * j + alpha + beta - 1) &
       / (2 * j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  a3 = (j - 1 + alpha) * (j - 1 + beta) * (2 * j + alpha + beta) &
       / (j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  p(ii + 1) = (a1 * x + a2) * p(ii) - a3 * p(ii - 1)

  j = j - 1.0
  b1 = -2.0*(j+alpha)*(j+beta)/(j+ab)/(2.0*j+ab)/(2.0*j+ab+1.0)
  b2 = 2.0 * amb / (2.0 * j + ab) / (2.0 * j + ab + 2.0)
  b3 = 2.0 * (j + ab + 1.0) / (2.0 * j + ab + 1.0) / (2.0 * j + ab + 2.0)

  ans(ii + 1) = (p(ii) - b1 * ans(ii - 1) - b2 * ans(ii)) / b3

END DO

END PROCEDURE JacobiGradientEvalAll1_

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalAll2
!!
INTEGER(I4B) :: ii
REAL(DFP) :: j
REAL(DFP), DIMENSION(SIZE(x), n + 1) :: p
REAL(DFP) :: ab, amb, a1, a2, a3, b1, b2, b3
!!
IF (n < 0) THEN
  RETURN
END IF
!!
p(:, 1) = 1.0_DFP
ans(:, 1) = 0.0_DFP
!!
IF (n < 1) THEN
  RETURN
END IF
!!
ab = alpha + beta
amb = alpha - beta
p(:, 2) = 0.5 * (ab + 2.0) * x + 0.5 * amb
ans(:, 2) = 0.5 * (ab + 2.0)
!!
DO ii = 2, n
  !!
  j = REAL(ii, KIND=DFP)
  !!
  a1 = (2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta)); 
  a2 = (alpha * alpha - beta * beta) * (2 * j + alpha + beta - 1) &
    & / (2 * j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  a3 = (j - 1 + alpha) * (j - 1 + beta) * (2 * j + alpha + beta) &
    & / (j * (j + alpha + beta) * (2 * j + alpha + beta - 2)); 
  !!
  p(:, ii + 1) = (a1 * x + a2) * p(:, ii) - a3 * p(:, ii - 1)
  !!
  j = j - 1.0
  b1 = -2.0*(j+alpha)*(j+beta)/(j+ab)/(2.0*j+ab)/(2.0*j+ab+1.0)
  b2 = 2.0 * amb / (2.0 * j + ab) / (2.0 * j + ab + 2.0)
  b3 = 2.0 * (j + ab + 1.0) / (2.0 * j + ab + 1.0) / (2.0 * j + ab + 2.0)
  !!
  ans(:, ii + 1) = (p(:, ii) - b1 * ans(:, ii - 1) - b2 * ans(:, ii)) / b3
  !!
END DO
!!
END PROCEDURE JacobiGradientEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalAll2_
INTEGER(I4B) :: ii
REAL(DFP) :: j
REAL(DFP), DIMENSION(SIZE(x), n + 1) :: p
REAL(DFP) :: ab, amb, a1, a2, a3, b1, b2, b3

nrow = 0
ncol = 0

IF (n < 0) RETURN

nrow = SIZE(x)
ncol = 1 + n

p(1:nrow, 1) = 1.0_DFP
ans(1:nrow, 1) = 0.0_DFP

IF (n < 1) RETURN

ab = alpha + beta
amb = alpha - beta
p(:, 2) = 0.5 * (ab + 2.0) * x + 0.5 * amb
ans(:, 2) = 0.5 * (ab + 2.0)

DO ii = 2, n
  j = REAL(ii, KIND=DFP)

  a1 = (2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta))

  a2 = (alpha * alpha - beta * beta) * (2 * j + alpha + beta - 1) &
       / (2 * j * (j + alpha + beta) * (2 * j + alpha + beta - 2))

  a3 = (j - 1 + alpha) * (j - 1 + beta) * (2 * j + alpha + beta) &
       / (j * (j + alpha + beta) * (2 * j + alpha + beta - 2))

  p(1:nrow, ii + 1) = (a1 * x + a2) * p(1:nrow, ii) - a3 * p(1:nrow, ii - 1)

  j = j - 1.0
  b1 = -2.0*(j+alpha)*(j+beta)/(j+ab)/(2.0*j+ab)/(2.0*j+ab+1.0)
  b2 = 2.0 * amb / (2.0 * j + ab) / (2.0 * j + ab + 2.0)
  b3 = 2.0 * (j + ab + 1.0) / (2.0 * j + ab + 1.0) / (2.0 * j + ab + 2.0)

  ans(1:nrow, ii + 1) = p(1:nrow, ii) - b1 * ans(1:nrow, ii - 1) &
                        - b2 * ans(1:nrow, ii)

  ans(1:nrow, ii + 1) = ans(1:nrow, ii + 1) / b3

END DO

END PROCEDURE JacobiGradientEvalAll2_

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalSum1
REAL(DFP) :: t, b1, b2, Ac, A1, A2, a10, a11, a12, a20, a21, j
REAL(DFP), PARAMETER :: c = 0.5_DFP
INTEGER(I4B) :: i
!!
IF (n .LT. 0) RETURN
IF (alpha .LE. -1.0_DFP) RETURN
IF (beta .LE. -1.0_DFP) RETURN
!!
b1 = 0
b2 = 0
!!
DO i = n - 1, 0, -1
  j = REAL(i, KIND=DFP)
  !!
  !! Recurrence coeff
  !!
  Ac = j + 2 + alpha + beta; 
  a10 = (2 * j + 3 + alpha + beta) / ((2 * j + 2) * (j + 3 + alpha + beta)); 
  a11 = (2 * j + 4 + alpha + beta) * x; 
  a12 = ((alpha - beta) * (alpha + beta + 2)) / (alpha + beta + 2 * j + 2); 
  A1 = a10 * (a11 + a12); 
  a20 = -(j + 2 + alpha) * (j + 2 + beta) &
      & / ((j + 2) * (alpha + beta + j + 4)); 
  a21 = (alpha + beta + 2 * j + 6) / (alpha + beta + 2 * j + 4); 
  A2 = a20 * a21; 
  t = A1 * b1 + A2 * b2 + Ac * coeff(i + 1); 
  b2 = b1; 
  b1 = t; 
END DO

ans = c * b1

END PROCEDURE JacobiGradientEvalSum1

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalSum2
REAL(DFP) :: Ac, A2, a10, a12, a20, a21, j
REAL(DFP), DIMENSION(SIZE(x)) :: a11, A1, t, b1, b2
REAL(DFP), PARAMETER :: c = 0.5_DFP
INTEGER(I4B) :: i
!!
IF (n .LT. 0) RETURN
IF (alpha .LE. -1.0_DFP) RETURN
IF (beta .LE. -1.0_DFP) RETURN
!!
b1 = 0
b2 = 0
!!
DO i = n - 1, 0, -1
  j = REAL(i, KIND=DFP)
  !!
  !! Recurrence coeff
  !!
  Ac = j + 2 + alpha + beta; 
  a10 = (2 * j + 3 + alpha + beta) / ((2 * j + 2) * (j + 3 + alpha + beta)); 
  a11 = (2 * j + 4 + alpha + beta) * x; 
  a12 = ((alpha - beta) * (alpha + beta + 2)) / (alpha + beta + 2 * j + 2); 
  A1 = a10 * (a11 + a12); 
  a20 = -(j + 2 + alpha) * (j + 2 + beta) &
      & / ((j + 2) * (alpha + beta + j + 4)); 
  a21 = (alpha + beta + 2 * j + 6) / (alpha + beta + 2 * j + 4); 
  A2 = a20 * a21; 
  t = A1 * b1 + A2 * b2 + Ac * coeff(i + 1); 
  b2 = b1; 
  b1 = t; 
END DO

ans = c * b1
END PROCEDURE JacobiGradientEvalSum2

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalSum3
REAL(DFP) :: t, b1, b2, Ac, A1, A2, a10, a11, a12, a20, a21, c, s
INTEGER(I4B) :: i, j
!!
IF (n .LT. 0) RETURN
IF (alpha .LE. -1.0_DFP) RETURN
IF (beta .LE. -1.0_DFP) RETURN
!!
b1 = 0
b2 = 0
c = 1.0_DFP
!!
DO i = k, 1, -1
  c = c / 2.0_DFP
END DO
!!
DO i = n - k, 0, -1
  !!
  s = 1.0_DFP
  !!
  DO j = 1, k
    s = s * (alpha + beta + i + k + j)
  END DO
  !!
  a10=(2*i+1+2*k+alpha+beta)/((2*i+2)*(i+1+2*k+alpha+beta)); 
  a11 = (2 * i + 2 + 2 * k + alpha + beta) * x; 
  a12=((alpha-beta)*(alpha+beta+2*k))/(alpha+beta+2*i+2*k); 
  A1 = a10 * (a11 + a12); 
  a20=-(i+1+k+alpha)*(i+1+k+beta)/((i+2)*(alpha+beta+i+2+2*k)); 
  a21 = (alpha + beta + 2 * i + 4 + 2 * k) &
      & / (alpha + beta + 2 * i + 2 + 2 * k); 
  A2 = a20 * a21; 
  t = A1 * b1 + A2 * b2 + s * coeff(i + k); 
  b2 = b1; 
  b1 = t; 
END DO

ans = c * b1

END PROCEDURE JacobiGradientEvalSum3

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientEvalSum4
REAL(DFP) :: Ac, A2, a10, a12, a20, a21, c, s
REAL(DFP), DIMENSION(SIZE(x)) :: a11, A1, t, b1, b2
INTEGER(I4B) :: i, j
!!
IF (n .LT. 0) RETURN
IF (alpha .LE. -1.0_DFP) RETURN
IF (beta .LE. -1.0_DFP) RETURN
!!
b1 = 0
b2 = 0
c = 1.0_DFP
!!
DO i = k, 1, -1
  c = c / 2.0_DFP
END DO
!!
DO i = n - k, 0, -1
  !!
  s = 1.0_DFP
  !!
  DO j = 1, k
    s = s * (alpha + beta + i + k + j)
  END DO
  !!
  a10=(2*i+1+2*k+alpha+beta)/((2*i+2)*(i+1+2*k+alpha+beta)); 
  a11 = (2 * i + 2 + 2 * k + alpha + beta) * x; 
  a12=((alpha-beta)*(alpha+beta+2*k))/(alpha+beta+2*i+2*k); 
  A1 = a10 * (a11 + a12); 
  a20=-(i+1+k+alpha)*(i+1+k+beta)/((i+2)*(alpha+beta+i+2+2*k)); 
  a21 = (alpha + beta + 2 * i + 4 + 2 * k) &
      & / (alpha + beta + 2 * i + 2 + 2 * k); 
  A2 = a20 * a21; 
  t = A1 * b1 + A2 * b2 + s * coeff(i + k); 
  b2 = b1; 
  b1 = t; 
END DO

ans = c * b1

END PROCEDURE JacobiGradientEvalSum4

!----------------------------------------------------------------------------
!                                                         JacobiTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiTransform1
INTEGER(I4B) :: tsize
CALL JacobiTransform1_(n, alpha, beta, coeff, x, w, quadType, ans, tsize)
END PROCEDURE JacobiTransform1

!----------------------------------------------------------------------------
!                                                           JacobiTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiTransform1_
REAL(DFP), ALLOCATABLE :: PP(:, :)
INTEGER(I4B) :: ii, jj, nips
nips = SIZE(coeff)
ALLOCATE (PP(nips, n + 1))
CALL JacobiEvalAll_(n=n, alpha=alpha, beta=beta, x=x, nrow=ii, ncol=jj, &
                    ans=PP)
CALL JacobiTransform4_(n, alpha, beta, coeff, PP, w, quadType, ans, tsize)
DEALLOCATE (PP)
END PROCEDURE JacobiTransform1_

!----------------------------------------------------------------------------
!                                                         JacobiTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiTransform4_
REAL(DFP) :: nrmsqr, areal
INTEGER(I4B) :: jj, ii, nips
LOGICAL(LGT) :: abool

tsize = n + 1

nips = SIZE(coeff)

DO jj = 0, n
  areal = 0.0_DFP

  DO ii = 0, nips - 1
    areal = areal + PP(ii, jj) * w(ii) * coeff(ii)
  END DO

  nrmsqr = JacobiNormSQR(n=jj, alpha=alpha, beta=beta)
  ans(jj) = areal / nrmsqr

END DO

abool = (quadType .EQ. qp%GaussLobatto) .AND. (nips .EQ. n + 1)

IF (abool) THEN

  areal = 0.0_DFP
  jj = n
  DO ii = 0, nips - 1
    areal = areal + PP(ii, jj) * w(ii) * coeff(ii)
  END DO

  nrmsqr = (2.0_DFP + (alpha + beta + 1.0_DFP) / REAL(n, KIND=DFP)) * nrmsqr

  ans(jj) = areal / nrmsqr

END IF

END PROCEDURE JacobiTransform4_

!----------------------------------------------------------------------------
!                                                         JacobiTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiTransform3
INTEGER(I4B) :: tsize
CALL JacobiTransform3_(n, alpha, beta, f, quadType, x1, x2, ans, tsize)
END PROCEDURE JacobiTransform3

!----------------------------------------------------------------------------
!                                                           JacobiTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiTransform3_
REAL(DFP) :: pt(0:n), wt(0:n), coeff(0:n), x
REAL(DFP), PARAMETER :: one = 1.0_DFP, half = 0.5_DFP
INTEGER(I4B) :: ii

CALL JacobiQuadrature(n=n + 1, alpha=alpha, beta=beta, pt=pt, wt=wt, &
                      quadType=quadType)

DO ii = 0, n
  x = (one - pt(ii)) * x1 + (one + pt(ii)) * x2
  x = x * half
  coeff(ii) = f(x)
END DO

CALL JacobiTransform_(n=n, alpha=alpha, beta=beta, coeff=coeff, x=pt, &
                      w=wt, quadType=quadType, ans=ans, tsize=tsize)
END PROCEDURE JacobiTransform3_

!----------------------------------------------------------------------------
!                                                        JacobiInvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiInvTransform1
ans = JacobiEvalSum(n=n, alpha=alpha, beta=beta, coeff=coeff, &
                    x=x)
END PROCEDURE JacobiInvTransform1

!----------------------------------------------------------------------------
!                                                        JacobiInvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiInvTransform2
ans = JacobiEvalSum(n=n, alpha=alpha, beta=beta, coeff=coeff, &
                    x=x)
END PROCEDURE JacobiInvTransform2

!----------------------------------------------------------------------------
!                                                     JacobiGradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGradientCoeff1
REAL(DFP) :: a, b, c, ab, amb, tnab, nab
INTEGER(I4B) :: ii
REAL(DFP) :: jj

ans(n) = 0.0_DFP
IF (n .EQ. 0) RETURN
!!
!! c(n-1)
!!
ab = alpha + beta
amb = alpha - beta
tnab = 2.0 * n + ab
nab = n + ab
!!
IF (n .EQ. 1) THEN
  c = 2.0_DFP / (ab + 2.0_DFP)
ELSE
  c = 2.0 * (n + ab) / (tnab - 1.0) / tnab
END IF
!!
ans(n - 1) = coeff(n) / c
!!
DO ii = n - 1, 1, -1
  jj = REAL(ii, KIND=DFP)
  tnab = 2.0 * jj + ab
  nab = jj + ab
  c = 2.0 * (jj + ab) / (tnab - 1.0) / tnab
  b = 2.0 * amb / tnab / (tnab + 2.0)
  a = -2.0 * (jj+alpha+1.0)*(jj+beta+1.0) / (nab+1.0) / (tnab+2.0)/(tnab+3.0)
  ans(ii - 1) = (coeff(ii) - b * ans(ii) - a * ans(ii + 1)) / c
END DO
!!
END PROCEDURE JacobiGradientCoeff1

!----------------------------------------------------------------------------
!                                                             JacobiDMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiDMatrix1
SELECT CASE (quadType)
CASE (qp%GaussLobatto)
  CALL JacobiDMatrixGL(n=n, alpha=alpha, beta=beta, x=x, quadType=quadType,&
    & D=ans)
CASE (qp%Gauss)
  CALL JacobiDMatrixG(n=n, alpha=alpha, beta=beta, x=x, quadType=quadType, &
    &  D=ans)
END SELECT
END PROCEDURE JacobiDMatrix1

!----------------------------------------------------------------------------
!                                                           JacobiDMatrixGL
!----------------------------------------------------------------------------

PURE SUBROUTINE JacobiDMatrixGL(n, alpha, beta, x, quadType, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: alpha
      !! alpha > -1.0
  REAL(DFP), INTENT(IN) :: beta
      !! beta > -1.0
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! main
  !!
  REAL(DFP) :: J(0:n)
  REAL(DFP) :: ab, rn
  INTEGER(I4B) :: ii, jj
  REAL(DFP) :: gb2, gna1, gnb1, ga2, sgn, gn, ga1, temp
  !!
  !! Compute dJ_{N-1}(a+1,b+1)
  !!
  J = JacobiGradientEval(n=n - 1, alpha=alpha + 1.0_DFP, &
    & beta=beta + 1.0_DFP, x=x)
  !!
  !! zeroth column
  !!
  ab = alpha + beta
  rn = REAL(n, KIND=DFP)
  !!
  D(0, 0) = 0.5 * (alpha - rn * (rn + ab + 1.0)) / (beta + 2.0)
  !!
  !!
  gb2 = GAMMA(beta + 2.0_DFP)
  gna1 = GAMMA(rn + alpha + 1.0_DFP)
  gnb1 = GAMMA(rn + beta + 1.0_DFP)
  ga1 = GAMMA(alpha + 1.0_DFP)
  ga2 = ga1 * (alpha + 1.0_DFP)
  gn = GAMMA(rn)
  sgn = (-1.0)**n
  !!
  D(n, 0) = sgn * 0.5 * gb2 * gna1 / gnb1 / ga2
  !!
  sgn = (-1.0)**(n - 1)
  !!
  DO ii = 1, n - 1
    D(ii, 0) = sgn * 0.5 * gn * gb2 * (1.0 - x(ii)) * J(ii) / gnb1
  END DO
  !!
  !! last column
  !!
  sgn = (-1.0)**(n + 1)
  !!
  D(0, n) = sgn * 0.5 * ga2 * gnb1 / gna1 / gb2
  !!
  D(n, n) = 0.5 * (-beta + rn * (rn + ab + 1.0)) / (alpha + 2.0)
  !!
  D(1:n - 1, n) = (gn * ga2 * 0.5 / gna1) * (1.0_DFP + x(1:n - 1)) &
                 & * J(1:n - 1)
  !!
  !! internal columns
  !!
  sgn = (-1.0)**(n)
  DO ii = 1, n - 1
    temp = J(ii) * (1.0 - x(ii)) * (1.0 + x(ii))**2
    D(0, ii) = 2.0 * sgn * gnb1 / gn / gb2 / temp
    !!
    temp = J(ii) * (1.0 + x(ii)) * (1.0 - x(ii))**2
    D(n, ii) = -2.0 * gna1 / gn / ga2 / temp
  END DO
  !!
  DO jj = 1, n - 1
    DO ii = 1, n - 1
      IF (ii .EQ. jj) THEN
        D(ii, ii) = (alpha - beta + ab * x(ii)) / 2.0 / (1.0 - x(ii)**2)
      ELSE
        D(ii, jj) = (1.0 - x(ii)**2) * J(ii) / (1.0 - x(jj)**2) / J(jj) &
          & / (x(ii) - x(jj))
      END IF
    END DO
  END DO
END SUBROUTINE JacobiDMatrixGL

!----------------------------------------------------------------------------
!                                                           JacobiDMatrixG
!----------------------------------------------------------------------------

PURE SUBROUTINE JacobiDMatrixG(n, alpha, beta, x, quadType, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: alpha
      !! alpha > -1.0
  REAL(DFP), INTENT(IN) :: beta
      !! beta > -1.0
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! main
  !!
  REAL(DFP) :: J(0:n)
  REAL(DFP) :: ab, amb
  INTEGER(I4B) :: ii, jj
  !!
  !! Compute dJ_{N-1}(a+1,b+1)
  !!
  J = JacobiGradientEval(n=n + 1, alpha=alpha, beta=beta, x=x)
  !!
  !! zeroth column
  !!
  ab = alpha + beta
  ab = alpha - beta
  !!
  DO jj = 0, n
    DO ii = 0, n
      IF (ii .EQ. jj) THEN
        D(ii, ii) = (amb + (ab + 2.0) * x(ii)) / 2.0 / (1.0 - x(ii)**2)
      ELSE
        D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
      END IF
    END DO
  END DO
!!
END SUBROUTINE JacobiDMatrixG

END SUBMODULE Methods
