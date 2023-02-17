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

SUBMODULE(NitscheMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_1
LOGICAL(LGT) :: isLamNod, ismuNod, isevecNod
REAL(DFP), ALLOCATABLE :: LamBar(:), muBar(:), realval(:), &
  & evecBar(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j
REAL(DFP), ALLOCATABLE :: SBar(:, :), DummyMat(:, :)

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%refElem%nsd
isLamNod = .FALSE.; ismuNod = .FALSE.; isevecNod = .FALSE.
IF (lambda%DefineOn .EQ. Nodal) isLamNod = .TRUE.
IF (mu%DefineOn .EQ. Nodal) ismuNod = .TRUE.
IF (evec%DefineOn .EQ. Nodal) isevecNod = .TRUE.

!<--- LamBar and muBar contains space varying values of Lam and mu
SELECT CASE (lambda%VarType)
CASE (Constant)

  ALLOCATE (LamBar(nips))
  LamBar = Get(lambda, TypeFEVariableScalar, &
    & TypeFEVariableConstant)

CASE (Space)

  realval = Get(lambda, TypeFEVariableScalar, &
    & TypeFEVariableSpace)

  IF (isLamNod) THEN
    LamBar = Interpolation(trial, realval)
  ELSE
    LamBar = realval
  END IF
END SELECT

SELECT CASE (mu%VarType)
CASE (Constant)

  ALLOCATE (muBar(nips))
  muBar = Get(mu, TypeFEVariableScalar, &
    & TypeFEVariableConstant)

CASE (Space)

  realval = Get(mu, TypeFEVariableScalar, &
    & TypeFEVariableSpace)

  IF (ismuNod) THEN
    muBar = Interpolation(trial, realval)
  ELSE
    muBar = realval
  END IF
END SELECT

SELECT CASE (evec%VarType)
CASE (Constant)

  ALLOCATE (evecBar(nsd, nips))
  evecBar(:, 1) = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableConstant)
  DO i = 2, nsd
    evecBar(:, i) = evecBar(:, 1)
  END DO

CASE (Space)

  ans = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableSpace)

  IF (isevecNod) THEN
    evecBar = Interpolation(trial, ans)
  ELSE
    evecBar = ans
  END IF

  DEALLOCATE (ans)
END SELECT

!<--- make integration parameters
realval = trial%Ws * trial%Js * trial%Thickness

!<--- allocate ans
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP

!<---
DO ips = 1, nips
  DummyMat = &
  &   LamBar(ips) * realval(ips) &
  & * DOT_PRODUCT(trial%normal(1:nsd, ips), evecBar(1:nsd, ips)) &
  & * Eye3(1:nsd, 1:nsd) &
  & + 2.0 * muBar(ips) * realval(ips) &
  & * OUTERPROD(a=trial%normal(1:nsd, ips), &
              & b=evecBar(1:nsd, ips), &
              & Sym=.TRUE.)

  SBar = MATMUL(trial%dNdXt(:, :, ips), DummyMat)

  DummyMat = RESHAPE(SBar, [nsd * nns2, 1])

  SBar = OUTERPROD(test%N(:, ips), DummyMat(:, 1))

  r1 = 0; r2 = 0
  DO i = 1, nsd
    r1 = r2 + 1; r2 = i * nns1
    ans(r1:r2, :) = ans(r1:r2, :) + evecBar(i, ips) * SBar(:, :)
  END DO
END DO

DEALLOCATE (LamBar, muBar, realval, evecBar, SBar, DummyMat)
END PROCEDURE space_nitsche_mat_1

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_3
LOGICAL(LGT) :: isevecNod
REAL(DFP), ALLOCATABLE :: realval(:), evecBar(:, :), &
  & SBar(:, :), DummyMat(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%refElem%nsd
isevecNod = .FALSE.

IF (evec%DefineOn .EQ. Nodal) isevecNod = .TRUE.

SELECT CASE (evec%VarType)
CASE (Constant)

  ALLOCATE (evecBar(nsd, nips))
  evecBar(1, :) = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableConstant)
  DO i = 2, nsd
    evecBar(i, :) = evecBar(1, :)
  END DO

CASE (Space)

  ans = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableSpace)

  IF (isevecNod) THEN
    evecBar = Interpolation(trial, ans)
  ELSE
    evecBar = ans
  END IF

  DEALLOCATE (ans)
END SELECT

!<--- make integration parameters
realval = trial%Ws * trial%Thickness * trial%Js

!<--- allocate ans
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP

!<---
DO ips = 1, nips
  DummyMat = &
  &   lambda * realval(ips) &
  & * DOT_PRODUCT(trial%normal(1:nsd, ips), evecBar(1:nsd, ips)) &
  & * Eye3(1:nsd, 1:nsd) &
  & + 2.0 * mu * realval(ips) &
  & * OUTERPROD(a=trial%normal(1:nsd, ips), &
              & b=evecBar(1:nsd, ips), &
              & Sym=.TRUE.)

  SBar = MATMUL(trial%dNdXt(:, :, ips), DummyMat)
  DummyMat = RESHAPE(SBar, [nsd * nns2, 1])
  SBar = OUTERPROD(a=test%N(:, ips), b=DummyMat(:, 1))

  r1 = 0; r2 = 0
  DO i = 1, nsd
    r1 = r2 + 1; r2 = i * nns1
    ans(r1:r2, :) = ans(r1:r2, :) + evecBar(i, ips) * SBar(:, :)
  END DO
END DO

DEALLOCATE (realval, evecBar, SBar, DummyMat)
END PROCEDURE space_nitsche_mat_3

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_5
REAL(DFP), ALLOCATABLE :: realval(:), SBar(:), cdNdXt(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j, c1, c2

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%refElem%nsd
!<--- make integration parameters
realval = trial%Ws * trial%Thickness * trial%Js
!<--- allocate ans
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP
ALLOCATE (cdNdXt(SIZE(trial%N, 1), SIZE(trial%N, 2)))
DO i = 1, SIZE(trial%N, 2)
  cdNdXt(:, i) = MATMUL(trial%dNdXt(:, :, i), trial%Normal(1:nsd, i))
END DO

DO ips = 1, nips
  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1; c2 = j * nns2
    r1 = 0; r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      IF (i .EQ. j) THEN
        SBar = lambda * trial%normal(i, ips) * trial%dNdXt(:, j, ips) &
          & + mu * cdNdXt(:, ips) &
          & + mu * trial%normal(j, ips) * trial%dNdXt(:, i, ips)
      ELSE
        SBar = lambda * trial%normal(i, ips) * trial%dNdXt(:, j, ips) &
          & + mu * trial%normal(j, ips) * trial%dNdXt(:, i, ips)
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) &
        & + realval(ips) * OUTERPROD(test%N(:, ips), SBar)
    END DO
  END DO
END DO

DEALLOCATE (realval, SBar, cdNdXt)
END PROCEDURE space_nitsche_mat_5

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_7
REAL(DFP), ALLOCATABLE :: realval(:), SBar(:), cdNdXt(:, :), &
  & LamBar(:), muBar(:)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j, c1, c2

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%refElem%nsd

SELECT CASE (lambda%VarType)
CASE (Constant)

  ALLOCATE (LamBar(nips))
  LamBar = Get(lambda, TypeFEVariableScalar, &
    & TypeFEVariableConstant)

CASE (Space)

  realval = Get(lambda, TypeFEVariableScalar, &
    & TypeFEVariableSpace)

  IF (lambda%DefineOn .EQ. Nodal) THEN
    LamBar = Interpolation(trial, realval)
  ELSE
    LamBar = realval
  END IF
END SELECT

SELECT CASE (mu%VarType)
CASE (Constant)

  ALLOCATE (muBar(nips))
  muBar = Get(mu, TypeFEVariableScalar, &
    & TypeFEVariableConstant)

CASE (Space)

  realval = Get(mu, TypeFEVariableScalar, &
    & TypeFEVariableSpace)

  IF (mu%DefineOn .EQ. Nodal) THEN
    muBar = Interpolation(trial, realval)
  ELSE
    muBar = realval
  END IF
END SELECT

!<--- make integration parameters
realval = trial%Ws * trial%Thickness * trial%Js
!<--- allocate ans
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP
ALLOCATE (cdNdXt(SIZE(trial%N, 1), SIZE(trial%N, 2)))
DO i = 1, SIZE(trial%N, 2)
  cdNdXt(:, i) = MATMUL(trial%dNdXt(:, :, i), trial%Normal(1:nsd, i))
END DO

DO ips = 1, nips
  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1; c2 = j * nns2
    r1 = 0; r2 = 0
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      IF (i .EQ. j) THEN
        SBar = LamBar(ips) * trial%normal(i, ips) * trial%dNdXt(:, j, ips) &
          & + muBar(ips) * cdNdXt(:, ips) &
          & + muBar(ips) * trial%normal(j, ips) * trial%dNdXt(:, i, ips)
      ELSE
        SBar = LamBar(ips) * trial%normal(i, ips) * trial%dNdXt(:, j, ips) &
          & + muBar(ips) * trial%normal(j, ips) * trial%dNdXt(:, i, ips)
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) &
        & + realval(ips) * OUTERPROD(test%N(:, ips), SBar)
    END DO
  END DO
END DO

DEALLOCATE (realval, SBar, cdNdXt, LamBar, muBar)
END PROCEDURE space_nitsche_mat_7

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_2

LOGICAL(LGT) :: isAlphaNod, isevecNod
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: AlphaBar(:), evecBar(:, :), realval(:)
REAL(DFP), ALLOCATABLE :: DummyMat(:, :)

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%refElem%nsd

isAlphaNod = .FALSE.; isevecNod = .FALSE.
IF (Alpha%DefineOn .EQ. Nodal) isAlphaNod = .TRUE.
IF (evec%DefineOn .EQ. Nodal) isevecNod = .TRUE.

SELECT CASE (Alpha%VarType)
CASE (Constant)

  ALLOCATE (AlphaBar(nips))
  AlphaBar = Get(Alpha, TypeFEVariableScalar, &
    & TypeFEVariableConstant)

CASE (Space)

  realval = Get(Alpha, TypeFEVariableScalar, &
    & TypeFEVariableSpace)

  IF (isAlphaNod) THEN
    AlphaBar = Interpolation(trial, realval)
  ELSE
    AlphaBar = realval
  END IF
END SELECT

SELECT CASE (evec%VarType)
CASE (Constant)

  ALLOCATE (evecBar(nsd, nips))
  evecBar(1, :) = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableConstant)
  DO i = 2, nsd
    evecBar(i, :) = evecBar(1, :)
  END DO

CASE (Space)

  ans = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableSpace)

  IF (isevecNod) THEN
    evecBar = Interpolation(trial, ans)
  ELSE
    evecBar = ans
  END IF

  DEALLOCATE (ans)
END SELECT

realval = trial%Ws * trial%Js * trial%Thickness * AlphaBar
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP

DO ips = 1, nips
  DummyMat = realval(ips) * &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1; c2 = j * nns2; r1 = 0; r2 = r1
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
        & evecBar(i, ips) * evecBar(j, ips) * DummyMat
    END DO
  END DO
END DO

DEALLOCATE (AlphaBar, evecBar, realval, DummyMat)
END PROCEDURE space_nitsche_mat_2

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE space_nitsche_mat_4
LOGICAL(LGT) :: isevecNod
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: evecBar(:, :), realval(:)
REAL(DFP), ALLOCATABLE :: DummyMat(:, :)
!!
nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%refElem%nsd
IF (evec%DefineOn .EQ. Nodal) THEN
  isevecNod = .TRUE.
ELSE
  isevecNod = .FALSE.
END IF
SELECT CASE (evec%VarType)
CASE (Constant)
  ALLOCATE (evecBar(nsd, nips))
  evecBar(1, :) = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableConstant)
  DO i = 2, nsd
    evecBar(i, :) = evecBar(1, :)
  END DO
CASE (Space)
  ans = Get(evec, TypeFEVariableVector, &
    & TypeFEVariableSpace)
  IF (isevecNod) THEN
    evecBar = Interpolation(trial, ans)
  ELSE
    evecBar = ans
  END IF
  DEALLOCATE (ans)
END SELECT
realval = trial%Ws * trial%Js * trial%Thickness * Alpha
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP
DO ips = 1, nips
  DummyMat = realval(ips) * &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1; c2 = j * nns2; r1 = 0; r2 = r1
    DO i = 1, nsd
      r1 = r2 + 1; r2 = i * nns1
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
        & evecBar(i, ips) * evecBar(j, ips) * DummyMat
    END DO
  END DO
END DO
DEALLOCATE (evecBar, realval, DummyMat)
END PROCEDURE space_nitsche_mat_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
