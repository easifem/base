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
!                                                             LegendreAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreAlpha
ans = 0.0_DFP
END PROCEDURE LegendreAlpha

!----------------------------------------------------------------------------
!                                                              LegendreBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreBeta
REAL(DFP) :: avar
!!
IF (n .EQ. 0_I4B) THEN
  ans = 2.0_DFP
ELSE
  avar = REAL(n**2, KIND=DFP)
  ans = avar / (4.0_DFP * avar - 1.0_DFP)
END IF
END PROCEDURE LegendreBeta

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
!                                              GetLegendreRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetLegendreRecurrenceCoeff2
REAL(DFP) :: j
INTEGER(I4B) :: ii
!!
IF (n .LT. 1) RETURN
B = 0.0_DFP
!!
DO ii = 1, n
  j = REAL(ii, KIND=DFP)
  A(ii - 1) = (2.0_DFP * j - 1.0_DFP) / j; 
  C(ii - 1) = (j - 1.0_DFP) / j; 
END DO
!!
END PROCEDURE GetLegendreRecurrenceCoeff2

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
!                                                 LegendreLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreLeadingCoeffRatio
ans = (2.0 * n + 1) / (n + 1.0_DFP)
END PROCEDURE LegendreLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                             LegendreNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreNormSqr
ans = 2.0_DFP / (2.0_DFP * n + 1.0_DFP)
END PROCEDURE LegendreNormSqr

!----------------------------------------------------------------------------
!                                                       LegendreNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreNormSqrRatio
ans = (2.0_DFP * n + 1.0_DFP) / (2.0_DFP * n + 3.0_DFP)
END PROCEDURE LegendreNormSqrRatio

!----------------------------------------------------------------------------
!                                                       LegendreNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreNormSqr2
INTEGER(I4B) :: ii
DO ii = 0, n
  ans(ii) = 2.0_DFP / (2.0_DFP * ii + 1.0_DFP)
END DO
END PROCEDURE LegendreNormSqr2

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
CALL LegendreJacobiMatrix(n=n, D=pt, E=pn)
!!
#ifdef USE_LAPACK95
CALL STEV(D=pt, E=pn)
!!
IF (PRESENT(wt)) THEN
  wt = pn
  pn = LegendreEval(n=n - 1, x=pt)
  fixvar = 2.0_DFP / REAL(n**2, KIND=DFP)
  DO ii = 1, n
    wt(ii) = fixvar * (1.0_DFP - pt(ii)**2) / (pn(ii)**2)
  END DO
END IF
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
CALL LegendreJacobiRadauMatrix(a=a, n=n, D=pt, E=pn)
!!
#ifdef USE_LAPACK95
!!
CALL STEV(D=pt, E=pn)
!!
IF (PRESENT(wt)) THEN
  wt = pn
  pn = LegendreEval(n=n, x=pt)
  fixvar = 1.0_DFP / REAL((n + 1)**2, KIND=DFP)
  !!
  DO ii = 1, n + 1
    wt(ii) = fixvar * (1.0_DFP + a * pt(ii)) / (pn(ii)**2)
  END DO
END IF
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
IF (n .LT. 0) RETURN
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
CALL LegendreJacobiLobattoMatrix(n=n, D=pt, E=pn)
!!
#ifdef USE_LAPACK95
!!
CALL STEV(D=pt, E=pn)
!!
IF (PRESENT(wt)) THEN
  wt = pn
  pn = LegendreEval(n=n + 1, x=pt)
  fixvar = 2.0_DFP / REAL((n + 1) * (n + 2), KIND=DFP)
  !!
  DO ii = 1, n + 2
    wt(ii) = fixvar / (pn(ii)**2)
  END DO
END IF
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
  r_i = REAL(i, kind=DFP)
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
  r_i = REAL(i, kind=DFP)
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
INTEGER(I4B) :: tsize
CALL LegendreEvalAll1_(n=n, x=x, ans=ans, tsize=tsize)
END PROCEDURE LegendreEvalAll1

!----------------------------------------------------------------------------
!                                                           LegendreEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalAll1_
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, r_i

tsize = 0
IF (n < 0) RETURN

tsize = n + 1
ans(1) = 1.0_DFP

IF (n .EQ. 0) RETURN

ans(2) = x

DO i = 2, n

  r_i = REAL(i, kind=DFP)

  c1 = r_i

  c2 = 2.0_DFP * r_i - 1.0_DFP
  c2 = c2 / c1

  c3 = -r_i + 1.0_DFP
  c3 = c3 / c1

  ans(i + 1) = (c2 * x) * ans(i) + c3 * ans(i - 1)
END DO

END PROCEDURE LegendreEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL LegendreEvalAll2_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LegendreEvalAll2

!----------------------------------------------------------------------------
!                                                          LegendreEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalAll2_
INTEGER(I4B) :: i
REAL(DFP) :: c1, c2, c3, r_i

nrow = 0; ncol = 0
IF (n < 0) RETURN

nrow = SIZE(x)
ncol = n + 1

ans(1:nrow, 1) = 1.0_DFP

IF (n .EQ. 0) RETURN

ans(1:nrow, 2) = x

DO i = 2, n
  r_i = REAL(i, kind=DFP)
  c1 = r_i
  c2 = 2.0_DFP * r_i - 1.0_DFP
  c2 = c2 / c1

  c3 = -r_i + 1.0_DFP
  c3 = c3 / c1

  ans(1:nrow, i + 1) = (c2 * x) * ans(1:nrow, i) + c3 * ans(1:nrow, i - 1)
END DO

END PROCEDURE LegendreEvalAll2_

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
INTEGER(I4B) :: tsize
CALL LegendreGradientEvalAll1_(n=n, x=x, ans=ans, tsize=tsize)
END PROCEDURE LegendreGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalAll1_
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p(1:n + 1)

tsize = 0

IF (n < 0) RETURN

tsize = n + 1
p(1) = 1.0_DFP
ans(1) = 0.0_DFP

IF (n < 1) RETURN

p(2) = x
ans(2) = 1.0_DFP

DO ii = 2, n
  r_ii = REAL(ii, KIND=DFP)

  p(ii + 1) = ((2.0_DFP * r_ii - 1) * x * p(ii) &
              & - (r_ii - 1.0_DFP) * p(ii - 1)) &
              & / r_ii

  ans(ii + 1) = (2.0_DFP * r_ii - 1.0_DFP) * p(ii) + ans(ii - 1)

END DO

END PROCEDURE LegendreGradientEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL LegendreGradientEvalAll2_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE LegendreGradientEvalAll2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalAll2_
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p(1:SIZE(x), 1:n + 1)

nrow = 0; ncol = 0

IF (n < 0) RETURN

nrow = SIZE(x)
ncol = n + 1

p(1:nrow, 1) = 1.0_DFP
ans(1:nrow, 1) = 0.0_DFP

IF (n < 1) RETURN

p(1:nrow, 2) = x
ans(1:nrow, 2) = 1.0_DFP

DO ii = 2, n

  r_ii = REAL(ii, KIND=DFP)

  p(1:nrow, ii + 1) = ((2.0_DFP * r_ii - 1) * x * p(1:nrow, ii) &
                       - (r_ii - 1.0_DFP) * p(1:nrow, ii - 1)) &
                      / r_ii

  ans(1:nrow, ii + 1) = (2.0_DFP * r_ii - 1.0_DFP) * p(1:nrow, ii) &
                        + ans(1:nrow, ii - 1)

END DO

END PROCEDURE LegendreGradientEvalAll2_

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
!                                                      LegendreEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalSum1
REAL(DFP) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP) :: i
!!
IF (n .LT. 0) RETURN
!!
b1 = 0.0_DFP
b2 = 0.0_DFP
!!
DO j = n, 1, -1
  i = REAL(j, KIND=DFP)
  t = (2 * i + 1) / (i + 1) * x * b1 - (i + 1) / (i + 2) * b2 + coeff(j)
  b2 = b1
  b1 = t
END DO
!!
ans = x * b1 - b2 / 2.0_DFP + coeff(0)
!!
END PROCEDURE LegendreEvalSum1

!----------------------------------------------------------------------------
!                                                      LegendreEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreEvalSum2
REAL(DFP), DIMENSION(SIZE(x)) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP) :: i
!!
IF (n .LT. 0) RETURN
!!
b1 = 0.0_DFP
b2 = 0.0_DFP
!!
DO j = n, 1, -1
  i = REAL(j, KIND=DFP)
  t = (2 * i + 1) / (i + 1) * x * b1 - (i + 1) / (i + 2) * b2 + coeff(j)
  b2 = b1
  b1 = t
END DO
!!
ans = x * b1 - b2 / 2.0_DFP + coeff(0)
!!
END PROCEDURE LegendreEvalSum2

!----------------------------------------------------------------------------
!                                                    LegendreGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalSum1
REAL(DFP) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP) :: i
!!
IF (n .LT. 0) RETURN
!!
b1 = 0
b2 = 0
!!
DO j = n - 1, 0, -1
  i = REAL(j, KIND=DFP)
  t = (2 * i + 3) / (i + 1) * x * b1 - (i + 3) / (i + 2) * b2 + coeff(j + 1); 
  b2 = b1; 
  b1 = t; 
END DO
ans = b1
END PROCEDURE LegendreGradientEvalSum1

!----------------------------------------------------------------------------
!                                                    LegendreGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalSum2
REAL(DFP), DIMENSION(SIZE(x)) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP) :: i
!!
IF (n .LT. 0) RETURN
!!
b1 = 0
b2 = 0
!!
DO j = n - 1, 0, -1
  i = REAL(j, KIND=DFP)
  t = (2 * i + 3) / (i + 1) * x * b1 - (i + 3) / (i + 2) * b2 + coeff(j + 1); 
  b2 = b1; 
  b1 = t; 
END DO
ans = b1
END PROCEDURE LegendreGradientEvalSum2

!----------------------------------------------------------------------------
!                                                   LegendreGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalSum3
REAL(DFP) :: t, b1, b2
REAL(DFP) :: s, A1, A2
INTEGER(I4B) :: j
REAL(DFP) :: i
!!
IF (n .LT. 0) RETURN
!!
b1 = 0
b2 = 0
s = 1.0_DFP
!!
DO j = 2 * k - 1, 1, -2
  s = j * s
END DO
!!
DO j = n - k, 0, -1
  i = REAL(j, KIND=DFP)
  A1 = (2 * i + 2 * k + 1) / (i + 1) * x; 
  A2 = -(i + 2 * k + 1) / (i + 2); 
  t = A1 * b1 + A2 * b2 + coeff(j + k); 
  b2 = b1; 
  b1 = t; 
END DO
ans = s * b1
END PROCEDURE LegendreGradientEvalSum3

!----------------------------------------------------------------------------
!                                                   LegendreGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientEvalSum4
REAL(DFP), DIMENSION(SIZE(x)) :: t, b1, b2, A1
REAL(DFP) :: s, A2
INTEGER(I4B) :: j
REAL(DFP) :: i
!!
IF (n .LT. 0) RETURN
!!
b1 = 0
b2 = 0
s = 1.0_DFP
!!
DO j = 2 * k - 1, 1, -2
  s = j * s
END DO
!!
DO j = n - k, 0, -1
  i = REAL(j, KIND=DFP)
  A1 = (2 * i + 2 * k + 1) / (i + 1) * x; 
  A2 = -(i + 2 * k + 1) / (i + 2); 
  t = A1 * b1 + A2 * b2 + coeff(j + k); 
  b2 = b1; 
  b1 = t; 
END DO
!!
ans = s * b1
END PROCEDURE LegendreGradientEvalSum4

!----------------------------------------------------------------------------
!                                                   LegendreTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreTransform1
REAL(DFP), DIMENSION(0:n) :: nrmsqr, temp
REAL(DFP), DIMENSION(0:n, 0:n) :: PP
INTEGER(I4B) :: jj
REAL(DFP) :: rn
!!
nrmsqr = LegendreNormSQR2(n=n)
!!
!! Correct nrmsqr(n)
!!
rn = REAL(n, KIND=DFP)
!!
IF (quadType .EQ. GaussLobatto) THEN
  nrmsqr(n) = 2.0_DFP / rn
END IF
!!
PP = LegendreEvalAll(n=n, x=x)
!!
DO jj = 0, n
  temp = PP(:, jj) * w * coeff
  ans(jj) = SUM(temp) / nrmsqr(jj)
END DO
!!
END PROCEDURE LegendreTransform1

!----------------------------------------------------------------------------
!                                                    LegendreTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreTransform2
REAL(DFP), DIMENSION(0:n) :: nrmsqr, temp
REAL(DFP), DIMENSION(0:n, 0:n) :: PP
INTEGER(I4B) :: jj, kk
REAL(DFP) :: rn
!!
nrmsqr = LegendreNormSQR2(n=n)
!!
!! Correct nrmsqr(n)
!!
rn = REAL(n, KIND=DFP)
!!
IF (quadType .EQ. GaussLobatto) THEN
  nrmsqr(n) = 2.0_DFP / rn
END IF
!!
PP = LegendreEvalAll(n=n, x=x)
!!
DO kk = 1, SIZE(coeff, 2)
  DO jj = 0, n
    temp = PP(:, jj) * w * coeff(:, kk)
    ans(jj, kk) = SUM(temp) / nrmsqr(jj)
  END DO
END DO
!!
END PROCEDURE LegendreTransform2

!----------------------------------------------------------------------------
!                                                    LegendreTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreTransform3
REAL(DFP) :: pt(0:n), wt(0:n), coeff(0:n)
INTEGER(I4B) :: ii
!!
CALL LegendreQuadrature(n=n + 1, pt=pt, wt=wt,&
  & quadType=quadType)
!!
DO ii = 0, n
  coeff(ii) = f(pt(ii))
END DO
!!
ans = LegendreTransform(n=n, coeff=coeff, x=pt, &
  & w=wt, quadType=quadType)
!!
END PROCEDURE LegendreTransform3

!----------------------------------------------------------------------------
!                                                       LegendreInvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreInvTransform1
ans = LegendreEvalSum(n=n, coeff=coeff, x=x)
END PROCEDURE LegendreInvTransform1

!----------------------------------------------------------------------------
!                                                       LegendreInvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreInvTransform2
ans = LegendreEvalSum(n=n, coeff=coeff, x=x)
END PROCEDURE LegendreInvTransform2

!----------------------------------------------------------------------------
!                                                      LegendreGradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreGradientCoeff1
ans = UltrasphericalGradientCoeff(n=n, lambda=0.5_DFP, coeff=coeff)
END PROCEDURE LegendreGradientCoeff1

!----------------------------------------------------------------------------
!                                                           LegendreDMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreDMatrix1
SELECT CASE (quadType)
CASE (GaussLobatto)
  CALL LegendreDMatrixGL2(n=n, x=x, D=ans)
CASE (Gauss)
  CALL LegendreDMatrixG2(n=n, x=x, D=ans)
END SELECT
END PROCEDURE LegendreDMatrix1

!----------------------------------------------------------------------------
!                                                          LegendreDMatrixGL
!----------------------------------------------------------------------------

PURE SUBROUTINE LegendreDMatrixGL(n, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! main
  !!
  REAL(DFP) :: J(0:n)
  REAL(DFP) :: rn
  INTEGER(I4B) :: ii, jj
  !!
  rn = REAL(n, KIND=DFP)
  !!
  J = LegendreEval(n=n, x=x)
  !!
  D = 0.0_DFP
  D(0, 0) = 0.125_DFP * rn * (rn + 1.0_DFP)
  D(n, n) = -D(0, 0)
  !!
  DO jj = 0, n
    DO ii = 0, n
      IF (ii .NE. jj) &
        & D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
    END DO
  END DO
  !!
END SUBROUTINE LegendreDMatrixGL

!----------------------------------------------------------------------------
!                                                          LegendreDMatrixGL
!----------------------------------------------------------------------------

PURE SUBROUTINE LegendreDMatrixGL2(n, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! main
  !!
  REAL(DFP) :: J(0:n)
  REAL(DFP) :: rn
  INTEGER(I4B) :: ii, jj, nb2
  !!
  nb2 = INT(n / 2)
  rn = REAL(n, KIND=DFP)
  !!
  J = LegendreEval(n=n, x=x)
  D = 0.0_DFP
  !!
  DO jj = 0, n
    DO ii = 0, nb2
      IF (ii .NE. jj) &
        & D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
    END DO
  END DO
  !!
  !! correct diagonal entries
  !!
  DO ii = 0, nb2
    D(ii, ii) = -SUM(D(ii, :))
  END DO
  !!
  !! copy
  !!
  DO jj = 0, n
    DO ii = 0, nb2
      D(n - ii, n - jj) = -D(ii, jj)
    END DO
  END DO
  !!
END SUBROUTINE LegendreDMatrixGL2

!----------------------------------------------------------------------------
!                                                           LegendreDMatrixG
!----------------------------------------------------------------------------

PURE SUBROUTINE LegendreDMatrixG(n, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! main
  !!
  REAL(DFP) :: J(0:n)
  INTEGER(I4B) :: ii, jj
  !!
  !! Compute dJ_{N-1}(a+1,b+1)
  !!
  J = LegendreGradientEval(n=n + 1, x=x)
  !!
  DO jj = 0, n
    DO ii = 0, n
      IF (ii .EQ. jj) THEN
        D(ii, ii) = x(ii) / (1.0 - x(ii)**2)
      ELSE
        D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
      END IF
    END DO
  END DO
!!
END SUBROUTINE LegendreDMatrixG

!----------------------------------------------------------------------------
!                                                           LegendreDMatrixG
!----------------------------------------------------------------------------

PURE SUBROUTINE LegendreDMatrixG2(n, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! internal variables
  !!
  REAL(DFP) :: J(0:n)
  INTEGER(I4B) :: ii, jj, nb2
  !!
  !! main
  !!
  nb2 = INT(n / 2)
  D = 0.0_DFP
  !!
  J = LegendreGradientEval(n=n + 1, x=x)
  !!
  DO jj = 0, n
    DO ii = 0, nb2
      IF (ii .NE. jj) &
      & D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
    END DO
  END DO
  !!
  !! correct diagonal entries
  !!
  DO ii = 0, nb2
    D(ii, ii) = -SUM(D(ii, :))
  END DO
  !!
  !! copy
  !!
  DO jj = 0, n
    DO ii = 0, nb2
      D(n - ii, n - jj) = -D(ii, jj)
    END DO
  END DO
  !!
END SUBROUTINE LegendreDMatrixG2

!----------------------------------------------------------------------------
!                                                       LegendreDMatEvenOdd
!----------------------------------------------------------------------------

MODULE PROCEDURE LegendreDMatEvenOdd1
CALL UltrasphericalDMatEvenOdd(n=n, D=D, o=o, e=e)
END PROCEDURE LegendreDMatEvenOdd1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
