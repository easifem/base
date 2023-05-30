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

MODULE PROCEDURE femat_StiffnessMatrix1
REAL(DFP), ALLOCATABLE :: realval(:), CBar(:, :, :), &
  & Dummy(:, :, :), Ce(:, :), BMat1(:, :), BMat2(:, :)
INTEGER(I4B) :: nips, nns1, nns2, i, j, ips, nsd
INTEGER(I4B), ALLOCATABLE :: S(:), Indx(:, :)
LOGICAL(LGT) :: isNodal

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%refElem%NSD

ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP

IF (Cijkl%defineOn .EQ. Nodal) THEN
  isNodal = .TRUE.
ELSE
  isNodal = .FALSE.
END IF

S = SHAPE(Cijkl)

SELECT CASE (Cijkl%varType)
CASE (Constant)
  ALLOCATE (CBar(S(1), S(2), nips))
  CBar(:, :, 1) = Get(Cijkl, TypeFEVariableMatrix, &
    & TypeFEVariableConstant)
  DO i = 2, nips
    CBar(:, :, i) = CBar(:, :, 1)
  END DO
CASE (Space)
  Dummy = Get(Cijkl, TypeFEVariableMatrix, TypeFEVariableSpace)
  IF (isNodal) THEN
    CBar = Interpolation(trial, Dummy)
  ELSE
    CBar = Dummy
  END IF
  DEALLOCATE (Dummy)
END SELECT

SELECT CASE (nsd)
CASE (1)
  ALLOCATE (Indx(1, 1))
  Indx = 1
CASE (2)
  Indx = RESHAPE([1, 3, 3, 2], [2, 2])
CASE (3)
  Indx = RESHAPE([1, 4, 6, 4, 2, 5, 6, 5, 3], [3, 3])
END SELECT

ALLOCATE ( &
  & Ce(nsd * nsd, nsd * nsd), &
  & BMat1(nsd * nns1, nsd * nsd), &
  & BMat2(nsd * nns2, nsd * nsd))

BMat1 = 0.0_DFP
BMat2 = 0.0_DFP
realval = trial%Ws * trial%js * trial%Thickness

DO ips = 1, nips

  DO j = 1, nsd
    DO i = 1, nsd
      Ce((i - 1) * nsd + 1:i * nsd, (j - 1) * nsd + 1:j * nsd) &
        & = CBar(Indx(:, i), Indx(:, j), ips)
    END DO
  END DO

  DO i = 1, nsd
    BMat1((i - 1) * nns1 + 1:i * nns1, (i - 1) * nsd + 1:i * nsd) = &
      & test%dNdXt(:, :, ips)
    BMat2((i - 1) * nns2 + 1:i * nns2, (i - 1) * nsd + 1:i * nsd) = &
      & trial%dNdXt(:, :, ips)
  END DO

  ans = ans + realval(ips) * MATMUL( &
    & MATMUL(BMat1, Ce), TRANSPOSE(BMat2))

END DO

DEALLOCATE (BMat1, BMat2, Indx, Ce, CBar, realval, S)

END PROCEDURE femat_StiffnessMatrix1

!----------------------------------------------------------------------------
!                                                           StiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE femat_StiffnessMatrix2
! Define internal variable
REAL(DFP), ALLOCATABLE :: LambdaBar(:), MuBar(:), Dummy(:), &
  & realval(:), Ke11(:, :)
REAL(DFP) :: real1, real2, real3
INTEGER(I4B) :: nns1, nns2, nips, nsd, c1, c2, i, j, r1, r2, ips
LOGICAL(LGT) :: isLambdaNodal, isMunodal

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%RefElem%NSD

ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP

IF (Lambda%DefineOn .EQ. Nodal) THEN
  isLambdaNodal = .TRUE.
ELSE
  isLambdaNodal = .FALSE.
END IF

IF (Mu%DefineOn .EQ. Nodal) THEN
  isMuNodal = .TRUE.
ELSE
  isMuNodal = .FALSE.
END IF

SELECT CASE (Lambda%VarType)
CASE (Constant)
  ALLOCATE (LambdaBar(nips))
  LambdaBar = Get( &
    & Lambda, TypeFEVariableScalar, &
    & TypeFEVariableConstant)
CASE (Space)
  realval = Get(Lambda, TypeFEVariableScalar, TypeFEVariableSpace)
  IF (isLambdaNodal) THEN
    LambdaBar = Interpolation(trial, realval)
  ELSE
    LambdaBar = realval
  END IF
END SELECT

SELECT CASE (Mu%VarType)
CASE (Constant)
  ALLOCATE (MuBar(nips))
  MuBar = Get(Mu, TypeFEVariableScalar, TypeFEVariableConstant)
CASE (Space)
  realval = Get(Mu, TypeFEVariableScalar, TypeFEVariableSpace)
  IF (isMuNodal) THEN
    MuBar = Interpolation(trial, realval)
  ELSE
    MuBar = realval
  END IF
  DEALLOCATE (realval)
END SELECT

realval = trial%Ws * trial%Js * trial%Thickness

DO ips = 1, nips
  real1 = MuBar(ips) * realval(ips)
  real2 = (LambdaBar(ips) + MuBar(ips)) * realval(ips)
  real3 = LambdaBar(ips) * realval(ips)
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

DEALLOCATE (realval, Ke11, LambdaBar, MuBar)

END PROCEDURE femat_StiffnessMatrix2

!----------------------------------------------------------------------------
!                                                            Stiffnessmatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE femat_StiffnessMatrix3
INTEGER(I4B) :: nns1, nns2, nips, ips, nsd, c1, c2, r1, r2, i, j
REAL(DFP), ALLOCATABLE :: realval(:), Ke11(:, :)
REAL(DFP) :: real1, real2, real3
nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%RefElem%NSD
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP
realval = trial%Ws * trial%Thickness * trial%Js
DO ips = 1, nips
  real1 = Mu * realval(ips)
  real2 = (Lambda + Mu) * realval(ips)
  real3 = Lambda * realval(ips)
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
END PROCEDURE femat_StiffnessMatrix3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE femat_StiffnessMatrix4
END PROCEDURE femat_StiffnessMatrix4

!----------------------------------------------------------------------------
!                                                           StiffnessMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE femat_StiffnessMatrix5
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:), Ke11(:, :)
REAL(DFP) :: real1, real2, real3
INTEGER(I4B) :: nns1, nns2, nips, nsd, c1, c2, i, j, r1, r2, ips

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%refelem%NSD

ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP

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

END PROCEDURE femat_StiffnessMatrix5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
