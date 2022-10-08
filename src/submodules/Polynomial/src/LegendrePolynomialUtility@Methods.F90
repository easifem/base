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

SUBMODULE(LegendrePolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                GetLegendreRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE GetLegendreRecurrenceCoeff
REAL(DFP), PARAMETER :: one = 1.0_DFP, two = 2.0_DFP, four = 4.0_DFP
REAL(DFP) :: avar
INTEGER(I4B) :: ii
!!
IF (n .LE. 0) RETURN
!!
alphaCoeff = 0.0_DFP
betaCoeff(0) = two
IF (n .EQ. 1) RETURN
!!
DO ii = 1, n - 1
  avar = REAL(ii**2, KIND=DFP)
  betaCoeff(ii) = avar / (four * avar - one)
END DO
!!
END PROCEDURE GetLegendreRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                       LegendreLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreLeadingCoeff
REAL(DFP) :: a1, a2, a3
a1 = REAL(Factorial(2 * n), KIND=DFP)
a2 = REAL(Factorial(n)**2, KIND=DFP)
a3 = REAL(2**n, KIND=DFP)
ans = a1 / a2 / a3
END PROCEDURE LegendreLeadingCoeff

!----------------------------------------------------------------------------
!                                                             LegendreNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreNormSqr
ans = 2.0_DFP / (2.0_DFP * n + 1.0_DFP)
END PROCEDURE LegendreNormSqr

!----------------------------------------------------------------------------
!                                                       LegendreJacobiMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreJacobiMatrix
REAL(DFP), DIMENSION(0:n - 1) :: alphaCoeff0, betaCoeff0
!!
IF (n .LT. 1) RETURN
!!
CALL GetLegendreRecurrenceCoeff(n=n, alphaCoeff=alphaCoeff0, &
  & betaCoeff=betaCoeff0)
IF (PRESENT(alphaCoeff)) alphaCoeff(0:n - 1) = alphaCoeff0
IF (PRESENT(betaCoeff)) betaCoeff(0:n - 1) = betaCoeff0
!!
CALL JacobiMatrix(alphaCoeff=alphaCoeff0, &
  & betaCoeff=betaCoeff0, D=D, E=E)
!!
END PROCEDURE LegendreJacobiMatrix

!----------------------------------------------------------------------------
!                                                    LegendreGaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGaussQuadrature
REAL(DFP) :: pn(n), fixvar
INTEGER(I4B) :: ii
!!
CALL LegendreJacobiMatrix(n=n, D=pt, E=wt)
!!
#ifdef USE_LAPACK95
CALL STEV(D=pt, E=wt)
pn = LegendreEval(n=n - 1, x=pt)
fixvar = 2.0_DFP / REAL(n**2, KIND=DFP)
DO ii = 1, n
  wt(ii) = fixvar * (1.0_DFP - pt(ii)**2) / (pn(ii)**2)
END DO
  !!
#else
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="LegendreGaussQuadrature", &
  & line=__LINE__, &
  & unitno=stdout)
#endif
  !!
END PROCEDURE LegendreGaussQuadrature

!----------------------------------------------------------------------------
!                                                    LegendreJacobiRadauMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreJacobiRadauMatrix
REAL(DFP) :: avar, r1, r2
!!
IF (n .LT. 1) RETURN
!!
CALL LegendreJacobiMatrix(n=n, D=D, E=E, &
  & alphaCoeff=alphaCoeff, betaCoeff=betaCoeff)
!!
r1 = a * REAL(n + 1, KIND=DFP)
r2 = REAL(2 * n + 1, KIND=DFP)
D(n + 1) = r1 / r2
!!
r1 = REAL(n**2, KIND=DFP)
r2 = 4.0_DFP * r1 - 1.0_DFP
!!
E(n) = SQRT(r1 / r2)
!!
END PROCEDURE LegendreJacobiRadauMatrix

!----------------------------------------------------------------------------
!                                               LegendreGaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGaussRadauQuadrature
REAL(DFP) :: pn(n + 1), fixvar
INTEGER(I4B) :: ii
  !!
CALL LegendreJacobiRadauMatrix(a=a, n=n, D=pt, E=wt)
!!
#ifdef USE_LAPACK95
!!
CALL STEV(D=pt, E=wt)
pn = LegendreEval(n=n, x=pt)
fixvar = 1.0_DFP / REAL((n + 1)**2, KIND=DFP)
!!
DO ii = 1, n + 1
  wt(ii) = fixvar * (1.0_DFP + a * pt(ii)) / (pn(ii)**2)
END DO
  !!
#else
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="LegendreGaussRadauQuadrature", &
  & line=__LINE__, &
  & unitno=stdout)
#endif
  !!
END PROCEDURE LegendreGaussRadauQuadrature

!----------------------------------------------------------------------------
!                                                 LegendreJacobiLobattoMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreJacobiLobattoMatrix
  !!
REAL(DFP) :: r1, r2
  !!
IF (n .LT. 1) RETURN
  !!
CALL LegendreJacobiMatrix(  &
  & n=n + 1, &
  & D=D, &
  & E=E, &
  & alphaCoeff=alphaCoeff, &
  & betaCoeff=betaCoeff)
  !!
D(n + 2) = 0.0_DFP
r1 = REAL(n + 1, KIND=DFP)
r2 = REAL(2 * n + 1, KIND=DFP)
  !!
E(n + 1) = SQRT(r1 / r2)
  !!
END PROCEDURE LegendreJacobiLobattoMatrix

!----------------------------------------------------------------------------
!                                            LegendreGaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGaussLobattoQuadrature
REAL(DFP) :: pn(n + 2), fixvar
INTEGER(I4B) :: ii
  !!
CALL LegendreJacobiLobattoMatrix(n=n, D=pt, E=wt)
!!
#ifdef USE_LAPACK95
!!
CALL STEV(D=pt, E=wt)
pn = LegendreEval(n=n + 1, x=pt)
fixvar = 2.0_DFP / REAL((n + 1) * (n + 2), KIND=DFP)
!!
DO ii = 1, n + 2
  wt(ii) = fixvar / (pn(ii)**2)
END DO
  !!
#else
CALL ErrorMsg( &
  & msg="The subroutine requires Lapack95 package", &
  & file=__FILE__, &
  & routine="LegendreGaussLobattoQuadrature", &
  & line=__LINE__, &
  & unitno=stdout)
#endif
  !!
END PROCEDURE LegendreGaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                               LegendreZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreZeros
ans = JacobiZeros(alpha=0.0_DFP, beta=0.0_DFP, n=n)
END PROCEDURE LegendreZeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreQuadrature
INTEGER(I4B) :: order
REAL(DFP), PARAMETER :: left = -1.0_DFP, right = 1.0_DFP
REAL(DFP), ALLOCATABLE :: p(:), w(:)
LOGICAL(LGT) :: inside
!!
IF (PRESENT(onlyInside)) THEN
  inside = onlyInside
ELSE
  inside = .FALSE.
END IF
!!
SELECT CASE (QuadType)
CASE (Gauss)
  !!
  order = n
  CALL LegendreGaussQuadrature(n=order, pt=pt, wt=wt)
  !!
CASE (GaussRadau, GaussRadauLeft)
  !!
  IF (inside) THEN
    order = n
    ALLOCATE (p(n + 1), w(n + 1))
    CALL LegendreGaussRadauQuadrature(a=left, n=order, pt=p, wt=w)
    pt = p(2:); wt = w(2:)
    DEALLOCATE (p, w)
  ELSE
    order = n - 1
    CALL LegendreGaussRadauQuadrature(a=left, n=order, pt=pt, wt=wt)
  END IF
  !!
CASE (GaussRadauRight)
  !!
  IF (inside) THEN
    order = n
    ALLOCATE (p(n + 1), w(n + 1))
    CALL LegendreGaussRadauQuadrature(a=right, n=order, pt=p, wt=w)
    pt = p(1:n); wt = w(1:n)
  ELSE
    order = n - 1
    CALL LegendreGaussRadauQuadrature(a=right, n=order, pt=pt, wt=wt)
  END IF
  !!
CASE (GaussLobatto)
  !!
  IF (inside) THEN
    order = n
    ALLOCATE (p(n + 2), w(n + 2))
    CALL LegendreGaussLobattoQuadrature(n=order, pt=p, wt=w)
    pt = p(2:n + 1); wt = w(2:n + 1)
  ELSE
    order = n - 2
    CALL LegendreGaussLobattoQuadrature(n=order, pt=pt, wt=wt)
  END IF
END SELECT
END PROCEDURE LegendreQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEval1
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, r_i, ans_1, ans_2
!!
ans = 0.0_DFP
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
ans = x
!!
DO i = 1, n - 1
  !!
  r_i = real(i, kind=DFP)
  c1 = r_i + 1.0_DFP
  c2 = 2.0_DFP * r_i + 1.0_DFP
  c3 = -r_i
  !!
  ans_1 = ans
  ans = ((c2 * x) * ans + c3 * ans_2) / c1
  ans_2 = ans_1
  !!
END DO
END PROCEDURE LegendreEval1

!----------------------------------------------------------------------------
!                                                                 LegendreEval
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEval2
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, r_i
REAL(DFP), DIMENSION(SIZE(x)) :: ans_1, ans_2
!!
ans = 0.0_DFP
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
ans = x
!!
DO i = 1, n - 1
  !!
  r_i = real(i, kind=DFP)
  c1 = r_i + 1.0_DFP
  c2 = 2.0_DFP * r_i + 1.0_DFP
  c3 = -r_i
  !!
  ans_1 = ans
  ans = ((c2 * x) * ans + c3 * ans_2) / c1
  ans_2 = ans_1
  !!
END DO
END PROCEDURE LegendreEval2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalAll1
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, r_i
!!
ans = 0.0_DFP
!!
IF (n < 0) THEN
  RETURN
END IF
!!
ans(1) = 1.0_DFP
!!
IF (n .EQ. 0) THEN
  RETURN
END IF
!!
ans(2) = x
!!
DO i = 2, n
  !!
  r_i = real(i, kind=DFP)
  c1 = r_i
  c2 = 2.0_DFP * r_i - 1.0_DFP
  c3 = -r_i + 1.0_DFP
  !!
  ans(i + 1) = ((c2 * x) * ans(i) + c3 * ans(i - 1)) / c1
  !!
END DO

END PROCEDURE LegendreEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalAll2
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, r_i
!!
ans = 0.0_DFP
!!
IF (n < 0) THEN
  RETURN
END IF
!!
ans(:, 1) = 1.0_DFP
!!
IF (n .EQ. 0) THEN
  RETURN
END IF
!!
ans(:, 2) = x
!!
DO i = 2, n
  !!
  r_i = real(i, kind=DFP)
  c1 = r_i
  c2 = 2.0_DFP * r_i - 1.0_DFP
  c3 = -r_i + 1.0_DFP
  !!
  ans(:, i + 1) = ((c2 * x) * ans(:, i) + c3 * ans(:, i - 1)) / c1
  !!
END DO
END PROCEDURE LegendreEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreMonomialExpansionAll
REAL(DFP) :: r_i
INTEGER(I4B) :: ii
  !!
IF (n < 0) THEN
  RETURN
END IF
  !!
ans = 0.0_DFP
ans(1, 1) = 1.0_DFP
  !!
IF (n .EQ. 0) THEN
  RETURN
END IF
  !!
ans(2, 2) = 1.0_DFP
  !!
DO ii = 2, n
  !!
  r_i = REAL(ii, KIND=DFP)
  !!
  ans(1:ii - 1, ii + 1) = &
    & (-r_i + 1.0) * ans(1:ii - 1, ii - 1) / r_i
  !!
  ans(2:ii + 1, ii + 1) = ans(2:ii + 1, ii + 1) &
    & + (2.0 * r_i - 1.0) * ans(1:ii, ii) / r_i
  !!
END DO
END PROCEDURE LegendreMonomialExpansionAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreMonomialExpansion
REAL(DFP) :: coeff(n + 1, n + 1)
coeff = LegendreMonomialExpansionAll(n)
ans = coeff(:, n + 1)
END PROCEDURE LegendreMonomialExpansion

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalAll1
  !!
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p(1:n + 1)
!!
IF (n < 0) THEN
  RETURN
END IF
!!
p(1) = 1.0_DFP
ans(1) = 0.0_DFP
!!
IF (n < 1) THEN
  RETURN
END IF
!!
p(2) = x
ans(2) = 1.0_DFP
!!
DO ii = 2, n
  !!
  r_ii = REAL(ii, KIND=DFP)
  !!
  p(ii + 1) = ((2.0_DFP * r_ii - 1) * x * p(ii) &
              & - (r_ii - 1.0_DFP) * p(ii - 1)) &
              & / r_ii
  !!
  ans(ii + 1) = (2.0_DFP * r_ii - 1.0_DFP) * p(ii) + ans(ii - 1)
  !!
END DO
!!
END PROCEDURE LegendreGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalAll2
!!
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p(1:SIZE(x), 1:n + 1)
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
p(:, 2) = x
ans(:, 2) = 1.0_DFP
!!
DO ii = 2, n
  !!
  r_ii = REAL(ii, KIND=DFP)
  !!
  p(:, ii + 1) = ((2.0_DFP * r_ii - 1) * x * p(:, ii) &
              & - (r_ii - 1.0_DFP) * p(:, ii - 1)) &
              & / r_ii
  !!
  ans(:, ii + 1) = (2.0_DFP * r_ii - 1.0_DFP) * p(:, ii) + ans(:, ii - 1)
  !!
END DO
!!
END PROCEDURE LegendreGradientEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEval1
  !!
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p, p_1, p_2
REAL(DFP) :: ans_1, ans_2
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
p = x
ans = 1.0_DFP
!!
DO ii = 2, n
  !!
  r_ii = REAL(ii, KIND=DFP)
  !!
  p_1 = p
  !!
  p = ((2.0_DFP * r_ii - 1) * x * p &
              & - (r_ii - 1.0_DFP) * p_2) &
              & / r_ii
  !!
  p_2 = p_1
  !!
  ans_1 = ans
  ans = (2.0_DFP * r_ii - 1.0_DFP) * p_1 + ans_2
  ans_2 = ans_1
  !!
END DO
!!
END PROCEDURE LegendreGradientEval1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEval2
!!
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP), DIMENSION(SIZE(x)) :: p, p_1, p_2
REAL(DFP), DIMENSION(SIZE(x)) :: ans_1, ans_2
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
p = x
ans = 1.0_DFP
!!
DO ii = 2, n
  !!
  r_ii = REAL(ii, KIND=DFP)
  !!
  p_1 = p
  !!
  p = ((2.0_DFP * r_ii - 1) * x * p &
              & - (r_ii - 1.0_DFP) * p_2) &
              & / r_ii
  !!
  p_2 = p_1
  !!
  ans_1 = ans
  ans = (2.0_DFP * r_ii - 1.0_DFP) * p_1 + ans_2
  ans_2 = ans_1
  !!
END DO
!!
END PROCEDURE LegendreGradientEval2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
