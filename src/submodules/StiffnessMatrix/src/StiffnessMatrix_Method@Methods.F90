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

SUBMODULE(StiffnessMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           StiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix1
REAL(DFP), ALLOCATABLE :: realval(:), CBar(:, :, :), &
  & Ce(:, :), BMat1(:, :), BMat2(:, :)
INTEGER(I4B) :: nips, nns1, nns2, i, j, ips, nsd
INTEGER(I4B), ALLOCATABLE :: indx(:, :)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)

CALL Reallocate(ans, nns1 * nsd, nns2 * nsd)
CALL GetInterpolation(obj=test, ans=CBar, val=Cijkl)

SELECT CASE (nsd)
CASE (1)
  ALLOCATE (indx(1, 1))
  indx = 1
CASE (2)
  ALLOCATE (indx(2, 2))
  indx = RESHAPE([1, 3, 3, 2], [2, 2])
CASE (3)
  ALLOCATE (indx(3, 3))
  indx = RESHAPE([1, 4, 6, 4, 2, 5, 6, 5, 3], [3, 3])
END SELECT

ALLOCATE (Ce(nsd * nsd, nsd * nsd), BMat1(nsd * nns1, nsd * nsd), &
  & BMat2(nsd * nns2, nsd * nsd))

BMat1 = 0.0_DFP
BMat2 = 0.0_DFP

CALL Reallocate(realval, nips)
realval = trial%ws * trial%js * trial%thickness

DO ips = 1, nips

  DO j = 1, nsd
    DO i = 1, nsd
      Ce((i - 1) * nsd + 1:i * nsd, (j - 1) * nsd + 1:j * nsd) &
        & = CBar(indx(:, i), indx(:, j), ips)
    END DO
  END DO

  DO i = 1, nsd
    BMat1((i - 1) * nns1 + 1:i * nns1, (i - 1) * nsd + 1:i * nsd) = &
      & test%dNdXt(:, :, ips)
    BMat2((i - 1) * nns2 + 1:i * nns2, (i - 1) * nsd + 1:i * nsd) = &
      & trial%dNdXt(:, :, ips)
  END DO

  ans = ans + realval(ips) * MATMUL(MATMUL(BMat1, Ce), TRANSPOSE(BMat2))

END DO

DEALLOCATE (BMat1, BMat2, indx, Ce, CBar, realval)

END PROCEDURE obj_StiffnessMatrix1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix1_
REAL(DFP) :: Cbar(test%nsd * (test%nsd + 1) / 2, &
                  trial%nsd * (trial%nsd + 1) / 2, &
                  trial%nips), &
             Ce(test%nsd * test%nsd, trial%nsd * trial%nsd), &
             BMat1(test%nsd * test%nns, trial%nsd * trial%nsd), &
             BMat2(trial%nsd * trial%nns, trial%nsd * trial%nsd)
INTEGER(I4B) :: nips, nns1, nns2, ips, nsd, ii, jj, kk
INTEGER(I4B) :: indx(3, 3)
REAL(DFP) :: realval

nns1 = test%nns
nns2 = trial%nns
nips = trial%nips
nsd = trial%nsd
nrow = nns1 * nsd
ncol = nns2 * nsd
ans(1:nrow, 1:ncol) = 0.0

CALL GetInterpolation_(obj=test, ans=CBar, val=Cijkl, &
                       dim1=ii, dim2=jj, dim3=kk)

SELECT CASE (nsd)
CASE (1)
  indx(1, 1) = 1
CASE (2)
  indx(1:2, 1:2) = RESHAPE([1, 3, 3, 2], [2, 2])
CASE (3)
  indx(1:3, 1:3) = RESHAPE([1, 4, 6, 4, 2, 5, 6, 5, 3], [3, 3])
END SELECT

BMat1 = 0.0_DFP
BMat2 = 0.0_DFP

DO ips = 1, nips
  realval = trial%ws(ips) * trial%js(ips) * trial%thickness(ips)

  DO jj = 1, nsd
    DO ii = 1, nsd
      Ce((ii - 1) * nsd + 1:ii * nsd, (jj - 1) * nsd + 1:jj * nsd) &
        & = CBar(indx(1:nsd, ii), indx(1:nsd, jj), ips)
    END DO
  END DO

  DO ii = 1, nsd
    BMat1((ii - 1) * nns1 + 1:ii * nns1, (ii - 1) * nsd + 1:ii * nsd) = &
      & test%dNdXt(1:nns1, 1:nsd, ips)
    BMat2((ii - 1) * nns2 + 1:ii * nns2, (ii - 1) * nsd + 1:ii * nsd) = &
      & trial%dNdXt(1:nns2, 1:nsd, ips)
  END DO

  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                        realval * MATMUL(MATMUL(BMat1, Ce), TRANSPOSE(BMat2))

END DO

END PROCEDURE obj_StiffnessMatrix1_

!----------------------------------------------------------------------------
!                                                           StiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix2
! Define internal variable
REAL(DFP), ALLOCATABLE :: lambdaBar(:), muBar(:), &
  & realval(:), Ke11(:, :)
REAL(DFP) :: real1, real2, real3
INTEGER(I4B) :: nns1, nns2, nips, nsd, c1, c2, i, j, r1, r2, ips
LOGICAL(LGT) :: case1
TYPE(FEVariable_) :: lambda0

IF (PRESENT(isLambdaYoungsModulus)) THEN
  case1 = isLambdaYoungsModulus
ELSE
  case1 = .FALSE.
END IF

IF (case1) THEN
  CALL GetLambdaFromYoungsModulus(lambda=lambda0,  &
    & youngsModulus=lambda, shearModulus=mu)
ELSE
  lambda0 = lambda
END IF

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)

ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP

CALL GetInterpolation(obj=test, ans=lambdaBar, val=lambda0)
CALL GetInterpolation(obj=test, ans=muBar, val=mu)

CALL Reallocate(realval, nips)
realval = trial%ws * trial%js * trial%thickness

DO ips = 1, nips
  real1 = muBar(ips) * realval(ips)
  real2 = (lambdaBar(ips) + muBar(ips)) * realval(ips)
  real3 = lambdaBar(ips) * realval(ips)
  c1 = 0
  c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1
    c2 = j * nns2
    r1 = 0
    r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1
      r2 = i * nns1
      IF (i .EQ. j) THEN
        Ke11 = real1 * MATMUL( &
          & test%dNdXt(:, :, ips), &
          & TRANSPOSE(trial%dNdXt(:, :, ips))) &
          & + real2 * OUTERPROD( &
          & test%dNdXt(:, i, ips), &
          & trial%dNdXt(:, i, ips))
      ELSE
        Ke11 = real3 * OUTERPROD( &
          & test%dNdXt(:, i, ips), &
          & trial%dNdXt(:, j, ips)) &
          + real1 * &
          & OUTERPROD( &
          & test%dNdXt(:, j, ips), &
          & trial%dNdXt(:, i, ips))
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + Ke11
    END DO
  END DO
END DO

DEALLOCATE (realval, Ke11, lambdaBar, muBar)
CALL DEALLOCATE (lambda0)

END PROCEDURE obj_StiffnessMatrix2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix2_
REAL(DFP) :: lambdaBar(trial%nips), muBar(trial%nips), &
             Ke11(test%nns, trial%nns)
REAL(DFP) :: realval
REAL(DFP) :: real1, real2, real3
INTEGER(I4B) :: nns1, nns2, nips, nsd, c1, c2, ii, jj, &
                r1, r2, ips, kk, ll
LOGICAL(LGT) :: abool
TYPE(FEVariable_) :: lambda0
REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP

abool = Input(default=.FALSE., option=isLambdaYoungsModulus)
IF (abool) THEN
  CALL GetLambdaFromYoungsModulus(lambda=lambda0,  &
    & youngsModulus=lambda, shearModulus=mu)
ELSE
  lambda0 = lambda
END IF

nns1 = test%nns
nns2 = trial%nns
nips = trial%nips
nsd = trial%nsd
nrow = nns1 * nsd
ncol = nns2 * nsd
ans(1:nrow, 1:ncol) = zero

CALL GetInterpolation_(obj=test, ans=lambdaBar, val=lambda0, tsize=ii)
CALL GetInterpolation_(obj=test, ans=muBar, val=mu, tsize=ii)

DO ips = 1, nips

  realval = trial%ws(ips) * trial%js(ips) * trial%thickness(ips)
  real1 = muBar(ips) * realval
  real2 = (lambdaBar(ips) + muBar(ips)) * realval
  real3 = lambdaBar(ips) * realval
  c1 = 0
  c2 = 0

  DO jj = 1, nsd
    c1 = c2 + 1
    c2 = jj * nns2
    r1 = 0
    r2 = 0
    DO ii = 1, nsd
      r1 = r2 + 1
      r2 = ii * nns1
      IF (ii .EQ. jj) THEN
        Ke11(1:nns1, 1:nns2) = real1 * MATMUL(test%dNdXt(:, :, ips), &
          & TRANSPOSE(trial%dNdXt(:, :, ips)))
        CALL OuterProd_(a=test%dNdXt(1:nns1, ii, ips), &
                        b=trial%dNdXt(1:nns2, ii, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real2, anscoeff=one)
      ELSE
        CALL OuterProd_(a=test%dNdXt(1:nns1, ii, ips), &
                        b=trial%dNdXt(1:nns2, jj, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real3, anscoeff=zero)
        CALL OuterProd_(a=test%dNdXt(1:nns1, jj, ips), &
                        b=trial%dNdXt(1:nns2, ii, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real1, anscoeff=one)
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + Ke11
    END DO
  END DO
END DO

CALL DEALLOCATE (lambda0)

END PROCEDURE obj_StiffnessMatrix2_

!----------------------------------------------------------------------------
!                                                            Stiffnessmatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix3
INTEGER(I4B) :: nns1, nns2, nips, ips, nsd, c1, c2, r1, r2, i, j
REAL(DFP), ALLOCATABLE :: realval(:), Ke11(:, :)
REAL(DFP) :: real1, real2, real3
nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)

CALL Reallocate(ans, nns1 * nsd, nns2 * nsd)
CALL Reallocate(realval, nips)
realval = trial%ws * trial%thickness * trial%js

DO ips = 1, nips
  real1 = mu * realval(ips)
  real2 = (lambda + mu) * realval(ips)
  real3 = lambda * realval(ips)
  c1 = 0; c2 = 0; 
  DO j = 1, nsd
    c1 = c2 + 1; c2 = j * nns2; r1 = 0; r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      IF (i .EQ. j) THEN
        Ke11 = real1 * MATMUL(test%dNdXt(:, :, ips), &
          & TRANSPOSE(trial%dNdXt(:, :, ips))) &
          & + real2 * OUTERPROD(test%dNdXt(:, i, ips), &
          & trial%dNdXt(:, i, ips))
      ELSE
        Ke11 = real3 * OUTERPROD(test%dNdXt(:, i, ips), &
          & trial%dNdXt(:, j, ips)) &
          + real1 * &
          & OUTERPROD(test%dNdXt(:, j, ips), &
            & trial%dNdXt(:, i, ips))
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + Ke11
    END DO
  END DO
END DO

DEALLOCATE (realval, Ke11)
END PROCEDURE obj_StiffnessMatrix3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix3_
INTEGER(I4B) :: nns1, nns2, nips, ips, nsd, c1, c2, &
                r1, r2, ii, jj, kk, ll
REAL(DFP) :: realval, Ke11(test%nns, trial%nns)
REAL(DFP) :: real1, real2, real3
REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP

nns1 = test%nns
nns2 = trial%nns
nips = trial%nips
nsd = trial%nsd
nrow = nns1 * nsd
ncol = nns2 * nsd
ans(1:nrow, 1:ncol) = zero

DO ips = 1, nips
  realval = trial%ws(ips) * trial%thickness(ips) * trial%js(ips)
  real1 = mu * realval
  real2 = (lambda + mu) * realval
  real3 = lambda * realval
  c1 = 0; c2 = 0; 
  DO jj = 1, nsd
    c1 = c2 + 1; c2 = jj * nns2; r1 = 0; r2 = 0
    DO ii = 1, nsd
      r1 = r2 + 1; r2 = ii * nns1
      IF (ii .EQ. jj) THEN
        Ke11 = real1 * MATMUL(test%dNdXt(:, :, ips), &
                              TRANSPOSE(trial%dNdXt(:, :, ips)))
        CALL OuterProd_(a=test%dNdXt(1:nns1, ii, ips), &
                        b=trial%dNdXt(1:nns2, ii, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real2, anscoeff=one)
      ELSE
        CALL OuterProd_(a=test%dNdXt(1:nns1, ii, ips), &
                        b=trial%dNdXt(1:nns2, jj, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real3, anscoeff=zero)
        CALL OuterProd_(a=test%dNdXt(1:nns1, jj, ips), &
                        b=trial%dNdXt(1:nns2, ii, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real1, anscoeff=one)
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + Ke11(1:nns1, 1:nns2)
    END DO
  END DO
END DO

END PROCEDURE obj_StiffnessMatrix3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix4
REAL(DFP), ALLOCATABLE :: realval(:), Ce(:, :), BMat1(:, :), BMat2(:, :)
INTEGER(I4B) :: nips, nns1, nns2, i, j, ips, nsd
INTEGER(I4B), ALLOCATABLE :: indx(:, :)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)

CALL Reallocate(ans, nns1 * nsd, nns2 * nsd)

SELECT CASE (nsd)
CASE (1)
  ALLOCATE (indx(1, 1))
  indx = 1
CASE (2)
  ALLOCATE (indx(2, 2))
  indx = RESHAPE([1, 3, 3, 2], [2, 2])
CASE (3)
  ALLOCATE (indx(3, 3))
  indx = RESHAPE([1, 4, 6, 4, 2, 5, 6, 5, 3], [3, 3])
END SELECT

ALLOCATE (Ce(nsd * nsd, nsd * nsd), &
  & BMat1(nsd * nns1, nsd * nsd), &
  & BMat2(nsd * nns2, nsd * nsd))

BMat1 = 0.0_DFP
BMat2 = 0.0_DFP

CALL Reallocate(realval, nips)
realval = trial%ws * trial%js * trial%thickness

DO ips = 1, nips

  DO j = 1, nsd
    DO i = 1, nsd
      Ce((i - 1) * nsd + 1:i * nsd, (j - 1) * nsd + 1:j * nsd) &
        & = Cijkl(indx(:, i), indx(:, j))
    END DO
  END DO

  DO i = 1, nsd
    BMat1((i - 1) * nns1 + 1:i * nns1, (i - 1) * nsd + 1:i * nsd) = &
      & test%dNdXt(:, :, ips)
    BMat2((i - 1) * nns2 + 1:i * nns2, (i - 1) * nsd + 1:i * nsd) = &
      & trial%dNdXt(:, :, ips)
  END DO

  ans = ans + realval(ips) * MATMUL(MATMUL(BMat1, Ce), TRANSPOSE(BMat2))

END DO

DEALLOCATE (BMat1, BMat2, indx, Ce, realval)

END PROCEDURE obj_StiffnessMatrix4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix4_
REAL(DFP) :: realval
REAL(DFP) :: Ce(test%nsd * test%nsd, trial%nsd * trial%nsd), &
             BMat1(test%nsd * test%nns, test%nsd * test%nsd), &
             BMat2(trial%nsd * trial%nns, trial%nsd * trial%nsd)
INTEGER(I4B) :: nips, nns1, nns2, ii, jj, ips, nsd
INTEGER(I4B) :: indx(3, 3)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)

nrow = nns1 * nsd
ncol = nns2 * nsd

SELECT CASE (nsd)
CASE (1)
  indx(1, 1) = 1
CASE (2)
  indx(1:2, 1:2) = RESHAPE([1, 3, 3, 2], [2, 2])
CASE (3)
  indx(1:3, 1:3) = RESHAPE([1, 4, 6, 4, 2, 5, 6, 5, 3], [3, 3])
END SELECT

BMat1 = 0.0_DFP
BMat2 = 0.0_DFP

DO ips = 1, nips

  realval = trial%ws(ips) * trial%js(ips) * trial%thickness(ips)
  DO jj = 1, nsd
    DO ii = 1, nsd
      Ce((ii - 1) * nsd + 1:ii * nsd, (jj - 1) * nsd + 1:jj * nsd) &
        & = Cijkl(indx(1:nsd, ii), indx(1:nsd, jj))
    END DO
  END DO

  DO ii = 1, nsd
    BMat1((ii - 1) * nns1 + 1:ii * nns1, (ii - 1) * nsd + 1:ii * nsd) = &
      & test%dNdXt(:, :, ips)
    BMat2((ii - 1) * nns2 + 1:ii * nns2, (ii - 1) * nsd + 1:ii * nsd) = &
      & trial%dNdXt(:, :, ips)
  END DO

  ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                        realval * MATMUL(MATMUL(BMat1, Ce), TRANSPOSE(BMat2))

END DO

END PROCEDURE obj_StiffnessMatrix4_

!----------------------------------------------------------------------------
!                                                           StiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix5
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:), Ke11(:, :)
REAL(DFP) :: real1, real2, real3
INTEGER(I4B) :: nns1, nns2, nips, nsd, c1, c2, i, j, r1, r2, ips

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)

CALL Reallocate(ans, nns1 * nsd, nns2 * nsd)
ans = 0.0_DFP

CALL Reallocate(realval, nips)
realval = trial%ws * trial%js * trial%thickness

DO ips = 1, nips
  real1 = mu(ips) * realval(ips)
  real2 = (lambda(ips) + mu(ips)) * realval(ips)
  real3 = lambda(ips) * realval(ips)
  c1 = 0
  c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1
    c2 = j * nns2
    r1 = 0
    r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1
      r2 = i * nns1
      IF (i .EQ. j) THEN
        Ke11 = real1 * MATMUL( &
          & test%dNdXt(:, :, ips), &
          & TRANSPOSE(trial%dNdXt(:, :, ips))) &
          & + real2 * OUTERPROD( &
          & test%dNdXt(:, i, ips), &
          & trial%dNdXt(:, i, ips))
      ELSE
        Ke11 = real3 * OUTERPROD( &
          & test%dNdXt(:, i, ips), &
          & trial%dNdXt(:, j, ips)) &
          + real1 * &
          & OUTERPROD( &
          & test%dNdXt(:, j, ips), &
          & trial%dNdXt(:, i, ips))
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + Ke11
    END DO
  END DO
END DO

DEALLOCATE (realval, Ke11)

END PROCEDURE obj_StiffnessMatrix5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StiffnessMatrix5_
REAL(DFP) :: realval, Ke11(test%nns, trial%nns)
REAL(DFP) :: real1, real2, real3
INTEGER(I4B) :: nns1, nns2, nips, nsd, c1, c2, ii, jj, &
                r1, r2, ips, kk, ll
REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = SIZE(trial%dNdXt, 2)
nrow = nns1 * nsd
ncol = nns2 * nsd
ans(1:nrow, 1:ncol) = zero

DO ips = 1, nips
  realval = trial%ws(ips) * trial%js(ips) * trial%thickness(ips)
  real1 = mu(ips) * realval
  real2 = (lambda(ips) + mu(ips)) * realval
  real3 = lambda(ips) * realval
  c1 = 0
  c2 = 0
  DO jj = 1, nsd
    c1 = c2 + 1
    c2 = jj * nns2
    r1 = 0
    r2 = 0
    DO ii = 1, nsd
      r1 = r2 + 1
      r2 = ii * nns1
      IF (ii .EQ. jj) THEN
        Ke11 = real1 * MATMUL( &
               test%dNdXt(:, :, ips), &
               TRANSPOSE(trial%dNdXt(:, :, ips)))
        CALL OuterProd_(a=test%dNdXt(1:nns1, ii, ips), &
                        b=trial%dNdXt(1:nns2, ii, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real2, anscoeff=one)
      ELSE
        CALL OuterProd_(a=test%dNdXt(1:nns1, ii, ips), &
                        b=trial%dNdXt(1:nns2, jj, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real3, anscoeff=zero)
        CALL OuterProd_(a=test%dNdXt(1:nns1, jj, ips), &
                        b=trial%dNdXt(1:nns2, ii, ips), &
                        nrow=kk, ncol=ll, ans=Ke11, &
                        scale=real1, anscoeff=one)
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + Ke11(1:nns1, 1:nns2)
    END DO
  END DO
END DO

END PROCEDURE obj_StiffnessMatrix5_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
