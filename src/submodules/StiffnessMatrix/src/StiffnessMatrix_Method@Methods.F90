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
CALL GetInterpolation(obj=test, interpol=CBar, val=Cijkl)

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

IF (isLambdaYoungsModulus) THEN
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

CALL GetInterpolation(obj=test, interpol=lambdaBar, val=lambda0)
CALL GetInterpolation(obj=test, interpol=muBar, val=mu)

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

END SUBMODULE Methods
