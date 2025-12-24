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

SUBMODULE(ElasticNitscheMatrix_Method) Matrix2
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix2a
REAL(DFP), ALLOCATABLE :: realval(:), SBar(:), cdNdXt(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j, c1, c2

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%nsd
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
END PROCEDURE ElasticNitscheMatrix2a

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix2b
REAL(DFP), ALLOCATABLE :: realval(:), SBar(:), cdNdXt(:, :), &
  & lamBar(:), muBar(:)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j, c1, c2

nns1 = SIZE(test%N, 1); nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2); nsd = trial%nsd

SELECT CASE (lambda%VarType)
CASE (Constant)

  ALLOCATE (lamBar(nips))
  lamBar = Get(lambda, TypeFEVariableScalar, &
    & TypeFEVariableConstant)

CASE (Space)

  realval = Get(lambda, TypeFEVariableScalar, &
    & TypeFEVariableSpace)

  IF (lambda%DefineOn .EQ. Nodal) THEN
    lamBar = Interpolation(trial, realval)
  ELSE
    lamBar = realval
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
        SBar = lamBar(ips) * trial%normal(i, ips) * trial%dNdXt(:, j, ips) &
          & + muBar(ips) * cdNdXt(:, ips) &
          & + muBar(ips) * trial%normal(j, ips) * trial%dNdXt(:, i, ips)
      ELSE
        SBar = lamBar(ips) * trial%normal(i, ips) * trial%dNdXt(:, j, ips) &
          & + muBar(ips) * trial%normal(j, ips) * trial%dNdXt(:, i, ips)
      END IF
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) &
        & + realval(ips) * OUTERPROD(test%N(:, ips), SBar)
    END DO
  END DO
END DO

DEALLOCATE (realval, SBar, cdNdXt, lamBar, muBar)
END PROCEDURE ElasticNitscheMatrix2b

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Matrix2
