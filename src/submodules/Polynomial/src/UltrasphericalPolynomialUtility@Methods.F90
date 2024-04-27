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

SUBMODULE(UltrasphericalPolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       UltrasphericalAlpha
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalAlpha
ans = 0.0_DFP
END PROCEDURE UltrasphericalAlpha

!----------------------------------------------------------------------------
!                                                       UltrasphericalBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalBeta
REAL(DFP) :: avar, bvar
!!
SELECT CASE (n)
CASE (0_I4B)
  avar = pi * GAMMA(2.0_DFP * lambda)
  bvar = (GAMMA(lambda))**2 * lambda * 2.0_DFP**(2.0_DFP * lambda - 1.0_DFP)
  ans = avar / bvar
CASE (1_I4B)
  ans = 0.5_DFP / (1.0_DFP + lambda)
CASE DEFAULT
  avar = n * (2.0_DFP * lambda + n - 1.0_DFP)
  bvar = 4.0_DFP * (n + lambda) * (n + lambda - 1.0_DFP)
  ans = avar / bvar
END SELECT
END PROCEDURE UltrasphericalBeta

!----------------------------------------------------------------------------
!                                           GetUltrasphericalRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE GetUltrasphericalRecurrenceCoeff
REAL(DFP) :: avar, bvar
INTEGER(I4B) :: ii
!!
IF (n .LE. 0) RETURN
!!
alphaCoeff = 0.0_DFP
!!
avar = pi * GAMMA(2.0_DFP * lambda)
bvar = (GAMMA(lambda))**2 * lambda * 2.0_DFP**(2.0_DFP * lambda - 1.0_DFP)
betaCoeff(0) = avar / bvar
!!
IF (n .EQ. 1) RETURN
!!
betaCoeff(1) = 0.5_DFP / (1.0_DFP + lambda)
!!
IF (n .EQ. 2) RETURN
!!
DO ii = 2, n - 1
  avar = ii * (2.0_DFP * lambda + ii - 1.0_DFP)
  bvar = 4.0_DFP * (ii + lambda) * (ii + lambda - 1.0_DFP)
  betaCoeff(ii) = avar / bvar
END DO
!!
END PROCEDURE GetUltrasphericalRecurrenceCoeff

!----------------------------------------------------------------------------
!                                          GetUltrasphericalRecurrenceCoeff2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetUltrasphericalRecurrenceCoeff2
REAL(DFP) :: j
INTEGER(I4B) :: ii
!!
IF (n .LT. 1) RETURN
B = 0.0_DFP
!!
DO ii = 1, n
  j = REAL(ii, KIND=DFP)
  A(ii - 1) = 2 * (j + lambda - 1) / j; 
  C(ii - 1) = (j + 2 * lambda - 2) / j; 
END DO
!!
END PROCEDURE GetUltrasphericalRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                 UltrasphericalLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalLeadingCoeff
REAL(DFP) :: a1, a2
a1 = (2.0_DFP**n) * GAMMA(n + lambda)
a2 = Factorial(n) * GAMMA(lambda)
ans = a1 / a2
END PROCEDURE UltrasphericalLeadingCoeff

!----------------------------------------------------------------------------
!                                           UltrasphericalLeadingCoeffRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalLeadingCoeffRatio
ans = 2.0_DFP * (n + lambda) / (n + 1.0_DFP)
END PROCEDURE UltrasphericalLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                      UltrasphericalNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalNormSqr
REAL(DFP) :: a1, a2
a1 = 2.0_DFP**(1.0_DFP - 2.0_DFP * lambda) * pi * GAMMA(n + 2.0_DFP * lambda)
a2 = GAMMA(lambda)**2 * (n + lambda) * Factorial(n)
ans = a1 / a2
END PROCEDURE UltrasphericalNormSqr

!----------------------------------------------------------------------------
!                                                UltrasphericalNormSqrRatio
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalNormSqrRatio
REAL(DFP) :: a1, a2
a1 = (n + lambda) * (n + 2.0_DFP * lambda)
a2 = (n + 1.0_DFP) * (n + lambda + 1.0_DFP)
ans = a1 / a2
END PROCEDURE UltrasphericalNormSqrRatio

!----------------------------------------------------------------------------
!                                                    UltrasphericalNormSqr2
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalNormSqr2
REAL(DFP) :: rn, s
INTEGER(I4B) :: ii
!!
ans(0) = UltrasphericalNormSQR(n=0_I4B, lambda=lambda)
!!
IF (n .EQ. 0) RETURN
!!
s = UltrasphericalNormSQRRatio(n=0_I4B, lambda=lambda)
ans(1) = ans(0) * s
!!
DO ii = 1, n - 1
  rn = REAL(ii, KIND=DFP)
  s = (rn + lambda) * (rn + 2.0_DFP * lambda) / (rn + 1.0_DFP) &
    & / (rn + lambda + 1.0_DFP)
  ans(ii + 1) = s * ans(ii)
END DO
END PROCEDURE UltrasphericalNormSqr2

!----------------------------------------------------------------------------
!                                       UltrasphericalJacobiMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalJacobiMatrix
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
CALL JacobiJacobiMatrix(n=n, alpha=alpha, beta=alpha, D=D, E=E, &
  & alphaCoeff=alphaCoeff, betaCoeff=betaCoeff)
END PROCEDURE UltrasphericalJacobiMatrix

!----------------------------------------------------------------------------
!                                              UltrasphericalGaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGaussQuadrature
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
CALL JacobiGaussQuadrature(n=n, alpha=alpha, beta=alpha, pt=pt, wt=wt)
END PROCEDURE UltrasphericalGaussQuadrature

!----------------------------------------------------------------------------
!                                   UltrasphericalJacobiRadauMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalJacobiRadauMatrix
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
CALL JacobiJacobiRadauMatrix(a=a, n=n, alpha=alpha, beta=alpha, D=D, E=E, &
  & alphaCoeff=alphaCoeff, betaCoeff=betaCoeff)
END PROCEDURE UltrasphericalJacobiRadauMatrix

!----------------------------------------------------------------------------
!                                         UltrasphericalGaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGaussRadauQuadrature
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
CALL JacobiGaussRadauQuadrature(a=a, n=n, alpha=alpha, beta=alpha, &
  & pt=pt, wt=wt)
END PROCEDURE UltrasphericalGaussRadauQuadrature

!----------------------------------------------------------------------------
!                                          UltrasphericalJacobiLobattoMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalJacobiLobattoMatrix
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
CALL JacobiJacobiLobattoMatrix(n=n, alpha=alpha, beta=alpha, D=D, E=E, &
  & alphaCoeff=alphaCoeff, betaCoeff=betaCoeff)
END PROCEDURE UltrasphericalJacobiLobattoMatrix

!----------------------------------------------------------------------------
!                                      UltrasphericalGaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGaussLobattoQuadrature
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
CALL JacobiGaussLobattoQuadrature(n=n, alpha=alpha, beta=alpha, &
  & pt=pt, wt=wt)
END PROCEDURE UltrasphericalGaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                        UltrasphericalZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalZeros
REAL(DFP) :: alpha
alpha = lambda - 0.5_DFP
ans = JacobiZeros(alpha=alpha, beta=alpha, n=n)
END PROCEDURE UltrasphericalZeros

!----------------------------------------------------------------------------
!                                                  UltrasphericalQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalQuadrature
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
  CALL UltrasphericalGaussQuadrature(n=order, lambda=lambda, pt=pt, wt=wt)
  !!
CASE (GaussRadau, GaussRadauLeft)
  !!
  IF (inside) THEN
    order = n
    ALLOCATE (p(n + 1), w(n + 1))
    CALL UltrasphericalGaussRadauQuadrature(a=left, lambda=lambda, &
      & n=order, pt=p, wt=w)
    pt = p(2:); wt = w(2:)
    DEALLOCATE (p, w)
  ELSE
    order = n - 1
    CALL UltrasphericalGaussRadauQuadrature(a=left, lambda=lambda, &
      & n=order, pt=pt, wt=wt)
  END IF
  !!
CASE (GaussRadauRight)
  !!
  IF (inside) THEN
    order = n
    ALLOCATE (p(n + 1), w(n + 1))
    CALL UltrasphericalGaussRadauQuadrature(a=right, lambda=lambda, &
      & n=order, pt=p, wt=w)
    pt = p(1:n); wt = w(1:n)
  ELSE
    order = n - 1
    CALL UltrasphericalGaussRadauQuadrature(a=right, lambda=lambda, &
      & n=order, pt=pt, wt=wt)
  END IF
  !!
CASE (GaussLobatto)
  !!
  IF (inside) THEN
    order = n
    ALLOCATE (p(n + 2), w(n + 2))
    CALL UltrasphericalGaussLobattoQuadrature(n=order, lambda=lambda, &
      & pt=p, wt=w)
    pt = p(2:n + 1); wt = w(2:n + 1)
  ELSE
    order = n - 2
    CALL UltrasphericalGaussLobattoQuadrature(n=order, lambda=lambda, &
      & pt=pt, wt=wt)
  END IF
END SELECT
END PROCEDURE UltrasphericalQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEval1
INTEGER(I4B) :: ii
REAL(DFP) :: c1, c2, c3, r_ii, ans_1, ans_2
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
ans = 2.0_DFP * lambda * x
IF (n .EQ. 1) RETURN
!!
DO ii = 1, n - 1
  !!
  r_ii = REAL(ii, kind=DFP)
  c1 = r_ii + 1.0_DFP
  c2 = 2.0_DFP * (r_ii + lambda)
  c3 = -(2.0_DFP * lambda + r_ii - 1.0_DFP)
  !!
  ans_1 = ans
  ans = ((c2 * x) * ans + c3 * ans_2) / c1
  ans_2 = ans_1
  !!
END DO
END PROCEDURE UltrasphericalEval1

!----------------------------------------------------------------------------
!                                                         UltrasphericalEval
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEval2
INTEGER(I4B) :: ii
REAL(DFP) :: c1, c2, c3, r_ii
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
IF (n .EQ. 0) RETURN
!!
ans = 2.0_DFP * lambda * x
!!
IF (n .EQ. 1) RETURN
!!
DO ii = 1, n - 1
  !!
  r_ii = REAL(ii, kind=DFP)
  c1 = r_ii + 1.0_DFP
  c2 = 2.0_DFP * (r_ii + lambda)
  c3 = -(2.0_DFP * lambda + r_ii - 1.0_DFP)
  !!
  ans_1 = ans
  ans = ((c2 * x) * ans + c3 * ans_2) / c1
  ans_2 = ans_1
  !!
END DO
END PROCEDURE UltrasphericalEval2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEvalAll1
INTEGER(I4B) :: a
CALL UltrasphericalEvalAll1_(n=n, lambda=lambda, x=x, ans=ans, tsize=a)
END PROCEDURE UltrasphericalEvalAll1

!----------------------------------------------------------------------------
!                                                     UltrasphericalEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEvalAll1_
INTEGER(I4B) :: ii
REAL(DFP) :: c1, c2, c3, r_ii

tsize = 0
IF (n < 0) RETURN

tsize = 1
ans(1) = 1.0_DFP
IF (n .EQ. 0) RETURN

tsize = n + 1
ans(2) = 2.0_DFP * lambda * x

DO ii = 2, n
  r_ii = REAL(ii, kind=DFP)
  c1 = 1.0_DFP / r_ii
  c2 = 2.0_DFP * (r_ii + lambda - 1.0_DFP) * c1 * x
  c3 = -(2.0_DFP * lambda + r_ii - 2.0_DFP) * c1
  ans(ii + 1) = c2 * ans(ii) + c3 * ans(ii - 1)
END DO

END PROCEDURE UltrasphericalEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEvalAll2
INTEGER(I4B) :: a, b
CALL UltrasphericalEvalAll2_(n=n, lambda=lambda, x=x, ans=ans, nrow=a, ncol=b)
END PROCEDURE UltrasphericalEvalAll2

!----------------------------------------------------------------------------
!                                                   UltrasphericalEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEvalAll2_
INTEGER(I4B) :: ii, jj
REAL(DFP) :: c1, c2, c3, r_ii

nrow = 0; ncol = 0
IF (n < 0) RETURN

! FIXME: What is this?
ans = 0.0_DFP

nrow = SIZE(x)
ncol = n + 1
ans(1:nrow, 1) = 1.0_DFP

IF (n .EQ. 0) RETURN

DO CONCURRENT(jj=1:nrow)

  ans(jj, 2) = 2.0_DFP * lambda * x(jj)

  DO ii = 2, n

    r_ii = REAL(ii, kind=DFP)
    c1 = 1.0_DFP / r_ii
    c2 = 2.0_DFP * (r_ii + lambda - 1.0_DFP) * c1 * x(jj)
    c3 = -(2.0_DFP * lambda + r_ii - 2.0_DFP) * c1

    ans(1:jj, ii + 1) = c2 * ans(1:jj, ii) + c3 * ans(1:jj, ii - 1)

  END DO
END DO

END PROCEDURE UltrasphericalEvalAll2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalAll1
INTEGER(I4B) :: tsize
CALL UltrasphericalGradientEvalAll1_(n=n, lambda=lambda, x=x, ans=ans, &
                                     tsize=tsize)
END PROCEDURE UltrasphericalGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalAll2
INTEGER(I4B) :: nrow, ncol
CALL UltrasphericalGradientEvalAll2_(n=n, lambda=lambda, x=x, ans=ans, &
                                     nrow=nrow, ncol=ncol)
END PROCEDURE UltrasphericalGradientEvalAll2

!----------------------------------------------------------------------------
!                                            UltrasphericalGradientEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalAll1_
INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p(1:n + 1)

tsize = 0
IF (n < 0) RETURN

tsize = n + 1
p(1) = 1.0_DFP
ans(1) = 0.0_DFP

IF (n < 1) RETURN

p(2) = 2.0_DFP * lambda * x
ans(2) = 2.0_DFP * lambda

DO ii = 2, n

  r_ii = REAL(ii, KIND=DFP)

  p(ii + 1) = ((r_ii + lambda - 1.0_DFP) * 2.0_DFP * x * p(ii) &
              & - (2.0_DFP * lambda + r_ii - 2.0_DFP) * p(ii - 1)) &
              & / r_ii

  ans(ii + 1) = 2.0_DFP * (r_ii + lambda - 1.0_DFP) * p(ii) + ans(ii - 1)

END DO

END PROCEDURE UltrasphericalGradientEvalAll1_

!----------------------------------------------------------------------------
!                                            UltrasphericalGradientEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalAll2_
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

p(1:nrow, 2) = 2.0_DFP * lambda * x
ans(1:nrow, 2) = 2.0_DFP * lambda

DO ii = 2, n

  r_ii = REAL(ii, KIND=DFP)

p(1:nrow, ii + 1) = ((r_ii + lambda - 1.0_DFP) * 2.0_DFP * x * p(1:nrow, ii) &
                & - (2.0_DFP * lambda + r_ii - 2.0_DFP) * p(1:nrow, ii - 1)) &
                              & / r_ii

  ans(1:nrow, ii + 1) = 2.0_DFP * (r_ii + lambda - 1.0_DFP) * p(1:nrow, ii) &
                  & + ans(1:nrow, ii - 1)

END DO

END PROCEDURE UltrasphericalGradientEvalAll2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEval1

INTEGER(I4B) :: ii
REAL(DFP) :: r_ii
REAL(DFP) :: p, p_1, p_2
REAL(DFP) :: ans_1, ans_2

ans = 0.0_DFP

IF (n < 0) THEN
  RETURN
END IF

p = 1.0_DFP
p_2 = p
ans_2 = ans

IF (n < 1) THEN
  RETURN
END IF

p = 2.0_DFP * lambda * x
ans = 2.0_DFP * lambda

DO ii = 2, n

  r_ii = REAL(ii, KIND=DFP)

  p_1 = p

  p = ((r_ii + lambda - 1.0_DFP) * 2.0_DFP * x * p &
              & - (2.0_DFP * lambda + r_ii - 2.0_DFP) * p_2) &
              & / r_ii

  p_2 = p_1

  ans_1 = ans
  ans = 2.0_DFP * (r_ii + lambda - 1.0_DFP) * p_1 + ans_2
  ans_2 = ans_1

END DO

END PROCEDURE UltrasphericalGradientEval1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEval2
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
p = 2.0_DFP * lambda * x
ans = 2.0_DFP * lambda
!!
DO ii = 2, n
  !!
  r_ii = REAL(ii, KIND=DFP)
  !!
  p_1 = p
  !!
  p = ((r_ii + lambda - 1.0_DFP) * 2.0_DFP * x * p &
              & - (2.0_DFP * lambda + r_ii - 2.0_DFP) * p_2) &
              & / r_ii
  !!
  p_2 = p_1
  !!
  ans_1 = ans
  ans = 2.0_DFP * (r_ii + lambda - 1.0_DFP) * p_1 + ans_2
  ans_2 = ans_1
  !!
END DO
!!
END PROCEDURE UltrasphericalGradientEval2

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEvalSum1
REAL(DFP) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP), DIMENSION(0:n + 1) :: A, B, C

! IF (n .LT. 0) RETURN
! IF (lambda .LE. -0.5_DFP) RETURN
! IF (lambda .EQ. 0.0_DFP) RETURN

CALL GetUltrasphericalRecurrenceCoeff2(n=n + 2, lambda=lambda, A=A, B=B, C=C)

b1 = 0.0_DFP
b2 = 0.0_DFP

DO j = n, 0, -1
  t = (A(j) * x) * b1 - C(j + 1) * b2 + coeff(j); 
  b2 = b1
  b1 = t
END DO

ans = b1

END PROCEDURE UltrasphericalEvalSum1

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalEvalSum2
REAL(DFP), DIMENSION(SIZE(x)) :: t, b1, b2
INTEGER(I4B) :: j
REAL(DFP), DIMENSION(0:n + 1) :: A, B, C

! IF (n .LT. 0) RETURN
! IF (lambda .LE. -0.5_DFP) RETURN
! IF (lambda .EQ. 0.0_DFP) RETURN

CALL GetUltrasphericalRecurrenceCoeff2(n=n + 2, lambda=lambda, A=A, B=B, C=C)

b1 = 0.0_DFP
b2 = 0.0_DFP

DO j = n, 0, -1
  t = (A(j) * x) * b1 - C(j + 1) * b2 + coeff(j); 
  b2 = b1
  b1 = t
END DO

ans = b1

END PROCEDURE UltrasphericalEvalSum2

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalSum1
REAL(DFP) :: t, b1, b2
REAL(DFP) :: A1, A2
INTEGER(I4B) :: i
REAL(DFP) :: j
REAL(DFP) :: c

! IF (n .LT. 0) RETURN
! IF (lambda .LE. -0.5_DFP) RETURN
! IF (lambda .EQ. 0.0_DFP) RETURN

c = 2 * lambda; 
b1 = 0
b2 = 0

DO i = n - 1, 0, -1
  j = REAL(i, KIND=DFP)
  A1 = 2 * (j + 1 + lambda) * x / (j + 1)
  A2 = -(j + 2 * lambda + 2) / (j + 2)
  t = A1 * b1 + A2 * b2 + coeff(i + 1)
  b2 = b1
  b1 = t
END DO
ans = C * b1
END PROCEDURE UltrasphericalGradientEvalSum1

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalSum2
REAL(DFP) :: A2
REAL(DFP), DIMENSION(SIZE(x)) :: A1, t, b1, b2
INTEGER(I4B) :: i
REAL(DFP) :: j
REAL(DFP) :: c

! IF (n .LT. 0) RETURN
! IF (lambda .LE. -0.5_DFP) RETURN
! IF (lambda .EQ. 0.0_DFP) RETURN

c = 2 * lambda; 
b1 = 0
b2 = 0

DO i = n - 1, 0, -1
  j = REAL(i, KIND=DFP)
  A1 = 2 * (j + 1 + lambda) * x / (j + 1)
  A2 = -(j + 2 * lambda + 2) / (j + 2)
  t = A1 * b1 + A2 * b2 + coeff(i + 1)
  b2 = b1
  b1 = t
END DO
ans = C * b1
END PROCEDURE UltrasphericalGradientEvalSum2

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalSum3
REAL(DFP) :: t, b1, b2, s
REAL(DFP) :: A1, A2
INTEGER(I4B) :: i
REAL(DFP) :: j

! IF (n .LT. 0) RETURN
! IF (lambda .LE. -0.5_DFP) RETURN
! IF (lambda .EQ. 0.0_DFP) RETURN

s = 1.0_DFP
DO i = 1, k
  s = 2 * s * (lambda + i - 1); 
END DO

b1 = 0
b2 = 0

DO i = n - k, 0, -1
  j = REAL(i, KIND=DFP)
  A1 = 2 * (j + k + lambda) * x / (j + 1); 
  A2 = -(j + 2 * lambda + 2 * k) / (j + 2); 
  t = A1 * b1 + A2 * b2 + coeff(i + k); 
  b2 = b1; 
  b1 = t; 
END DO
ans = s * b1
END PROCEDURE UltrasphericalGradientEvalSum3

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientEvalSum4
REAL(DFP) :: A2, s
REAL(DFP), DIMENSION(SIZE(x)) :: A1, b1, b2, t
INTEGER(I4B) :: i
REAL(DFP) :: j

! IF (n .LT. 0) RETURN
! IF (lambda .LE. -0.5_DFP) RETURN
! IF (lambda .EQ. 0.0_DFP) RETURN

s = 1.0_DFP
DO i = 1, k
  s = 2 * s * (lambda + i - 1); 
END DO

b1 = 0
b2 = 0

DO i = n - k, 0, -1
  j = REAL(i, KIND=DFP)
  A1 = 2 * (j + k + lambda) * x / (j + 1); 
  A2 = -(j + 2 * lambda + 2 * k) / (j + 2); 
  t = A1 * b1 + A2 * b2 + coeff(i + k); 
  b2 = b1; 
  b1 = t; 
END DO
ans = s * b1
END PROCEDURE UltrasphericalGradientEvalSum4

!----------------------------------------------------------------------------
!                                                   UltrasphericalTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalTransform1
REAL(DFP), DIMENSION(0:n) :: nrmsqr, temp
REAL(DFP), DIMENSION(0:n, 0:n) :: PP
INTEGER(I4B) :: jj
REAL(DFP) :: rn
!!
nrmsqr = UltrasphericalNormSQR2(n=n, lambda=lambda)
!!
!! Correct nrmsqr(n)
!!
rn = REAL(n, KIND=DFP)
!!
IF (quadType .EQ. GaussLobatto) THEN
  nrmsqr(n) = 2.0_DFP * (rn + lambda) / rn * nrmsqr(n)
END IF
!!
PP = UltrasphericalEvalAll(n=n, lambda=lambda, x=x)
!!
DO jj = 0, n
  temp = PP(:, jj) * w * coeff
  ans(jj) = SUM(temp) / nrmsqr(jj)
END DO
!!
END PROCEDURE UltrasphericalTransform1

!----------------------------------------------------------------------------
!                                                    UltrasphericalTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalTransform2
REAL(DFP), DIMENSION(0:n) :: nrmsqr, temp
REAL(DFP), DIMENSION(0:n, 0:n) :: PP
INTEGER(I4B) :: jj, kk
REAL(DFP) :: rn
!!
nrmsqr = UltrasphericalNormSQR2(n=n, lambda=lambda)
!!
!! Correct nrmsqr(n)
!!
rn = REAL(n, KIND=DFP)
!!
IF (quadType .EQ. GaussLobatto) THEN
  nrmsqr(n) = 2.0_DFP * (rn + lambda) / rn * nrmsqr(n)
END IF
!!
PP = UltrasphericalEvalAll(n=n, lambda=lambda, x=x)
!!
DO kk = 1, SIZE(coeff, 2)
  DO jj = 0, n
    temp = PP(:, jj) * w * coeff(:, kk)
    ans(jj, kk) = SUM(temp) / nrmsqr(jj)
  END DO
END DO
!!
END PROCEDURE UltrasphericalTransform2

!----------------------------------------------------------------------------
!                                                    UltrasphericalTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalTransform3
REAL(DFP) :: pt(0:n), wt(0:n), coeff(0:n)
INTEGER(I4B) :: ii

CALL UltrasphericalQuadrature(n=n + 1, lambda=lambda, pt=pt, wt=wt,&
  & quadType=quadType)

DO ii = 0, n
  coeff(ii) = f(pt(ii))
END DO

ans = UltrasphericalTransform(n=n, lambda=lambda, coeff=coeff, x=pt, &
  & w=wt, quadType=quadType)

END PROCEDURE UltrasphericalTransform3

!----------------------------------------------------------------------------
!                                                UltrasphericalInvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalInvTransform1
ans = UltrasphericalEvalSum(n=n, lambda=lambda, coeff=coeff, &
  & x=x)
END PROCEDURE UltrasphericalInvTransform1

!----------------------------------------------------------------------------
!                                                UltrasphericalInvTransform
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalInvTransform2
ans = UltrasphericalEvalSum(n=n, lambda=lambda, coeff=coeff, &
  & x=x)
END PROCEDURE UltrasphericalInvTransform2

!----------------------------------------------------------------------------
!                                                UltrasphericalGradientCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalGradientCoeff1
REAL(DFP) :: a, b, c
INTEGER(I4B) :: ii
REAL(DFP) :: jj
!!
ans(n) = 0.0_DFP
IF (n .EQ. 0) RETURN
!!
ans(n - 1) = 2.0 * (n + lambda - 1.0_DFP) * coeff(n)
!!
DO ii = n - 1, 1, -1
  jj = REAL(ii, KIND=DFP)
  a = jj + lambda - 1.0_DFP
  b = jj + lambda + 1.0_DFP
  c = a / b
  ans(ii - 1) = 2.0_DFP * a * coeff(ii) + c * ans(ii + 1)
END DO
!!
END PROCEDURE UltrasphericalGradientCoeff1

!----------------------------------------------------------------------------
!                                                     UltrasphericalDMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalDMatrix1
SELECT CASE (quadType)
CASE (GaussLobatto)
  CALL UltrasphericalDMatrixGL2(n=n, lambda=lambda, x=x,&
    & D=ans)
CASE (Gauss)
  CALL UltrasphericalDMatrixG2(n=n, lambda=lambda, x=x, &
    &  D=ans)
END SELECT
END PROCEDURE UltrasphericalDMatrix1

!----------------------------------------------------------------------------
!                                                    UltrasphericalDMatrixGL
!----------------------------------------------------------------------------

PURE SUBROUTINE UltrasphericalDMatrixGL(n, lambda, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
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
  J = UltrasphericalEval(n=n, lambda=lambda, x=x)
  !!
  !! first col
  !!
  D(0, 0) = (lambda - 0.5_DFP - rn * (rn + 2.0 * lambda)) / &
    & (2.0 * lambda + 3.0)
  DO ii = 1, n
    D(ii, 0) = (lambda + 0.5) * J(ii) / (x(ii) + 1.0) / J(0)
  END DO
  !!
  !! last col
  !!
  DO ii = 0, n - 1
    D(ii, n) = (lambda + 0.5) * J(ii) / (x(ii) - 1.0) / J(n)
  END DO
  D(n, n) = -D(0, 0)
  !!
  !! internal column
  !!
  DO jj = 1, n - 1
    DO ii = 0, n
      IF (ii .EQ. jj) THEN
        D(ii, ii) = (lambda - 0.5) * x(ii) / (1.0 - x(ii)**2)
      ELSE
        D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
      END IF
    END DO
  END DO
  !!
END SUBROUTINE UltrasphericalDMatrixGL

!----------------------------------------------------------------------------
!                                                    UltrasphericalDMatrixGL
!----------------------------------------------------------------------------

PURE SUBROUTINE UltrasphericalDMatrixGL2(n, lambda, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
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
  J = UltrasphericalEval(n=n, lambda=lambda, x=x)
  D = 0.0_DFP
  !!
  !! first col
  !!
  !D(0, 0) = (lambda - 0.5_DFP - rn * (rn + 2.0 * lambda)) / &
  !  & (2.0 * lambda + 3.0)
  DO ii = 1, nb2
    D(ii, 0) = (lambda + 0.5) * J(ii) / (x(ii) + 1.0) / J(0)
  END DO
  !!
  !! last col
  !!
  DO ii = 0, nb2
    D(ii, n) = (lambda + 0.5) * J(ii) / (x(ii) - 1.0) / J(n)
  END DO
  !!
  !! internal column
  !!
  DO jj = 1, n - 1
    DO ii = 0, nb2
      IF (ii .NE. jj) & !THEN
        & D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
      ! ELSE
      ! D(ii, ii) = (lambda - 0.5) * x(ii) / (1.0 - x(ii)**2)
      !END IF
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
END SUBROUTINE UltrasphericalDMatrixGL2

!----------------------------------------------------------------------------
!                                                    UltrasphericalDMatrixG
!----------------------------------------------------------------------------

PURE SUBROUTINE UltrasphericalDMatrixG(n, lambda, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
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
  J = UltrasphericalGradientEval(n=n + 1, lambda=lambda, x=x)
  !!
  DO jj = 0, n
    DO ii = 0, n
      IF (ii .EQ. jj) THEN
        D(ii, ii) = (lambda + 0.5_DFP) * x(ii) / (1.0 - x(ii)**2)
      ELSE
        D(ii, jj) = J(ii) / J(jj) / (x(ii) - x(jj))
      END IF
    END DO
  END DO
!!
END SUBROUTINE UltrasphericalDMatrixG

!----------------------------------------------------------------------------
!                                                    UltrasphericalDMatrixG
!----------------------------------------------------------------------------

PURE SUBROUTINE UltrasphericalDMatrixG2(n, lambda, x, D)
  INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
  REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
  REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
  REAL(DFP), INTENT(OUT) :: D(0:n, 0:n)
      !! D matrix
  !!
  !! main
  !!
  REAL(DFP) :: J(0:n)
  INTEGER(I4B) :: ii, jj, nb2
  !!
  !! Compute dJ_{N-1}(a+1,b+1)
  !!
  nb2 = INT(n / 2)
  !!
  J = UltrasphericalGradientEval(n=n + 1, lambda=lambda, x=x)
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
END SUBROUTINE UltrasphericalDMatrixG2

!----------------------------------------------------------------------------
!                                             UltrasphericalDMatEvenOdd
!----------------------------------------------------------------------------

MODULE PROCEDURE UltrasphericalDMatEvenOdd1
INTEGER(I4B) :: ii, jj, n1, n2
  !!
IF (MOD(N, 2) .EQ. 0) THEN
  !! even
  !!
  n1 = INT(n / 2) - 1
  !!
  DO jj = 0, n1
    DO ii = 0, n1
      e(ii, jj) = D(ii, jj) + D(ii, n - jj)
      o(ii, jj) = D(ii, jj) - D(ii, n - jj)
    END DO
  END DO
  !!
  n2 = n1 + 1
  e(1:n1, n2) = D(1:n1, n2)
  o(n2, 1:n1) = D(n2, 1:n1) - D(n2, 1:n1)
  !!
ELSE
  !! odd
  n2 = (n - 1) / 2
  n1 = n2
  !!
  DO jj = 0, n2
    DO ii = 0, n1
      e(ii, jj) = D(ii, jj) + D(ii, n - jj)
      o(ii, jj) = D(ii, jj) - D(ii, n - jj)
    END DO
  END DO
    !!
END IF
END PROCEDURE UltrasphericalDMatEvenOdd1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
