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

SUBMODULE(ElasticNitscheMatrix_Method) Matrix3
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3a
REAL(DFP), ALLOCATABLE :: alphaBar(:), evecBar(:, :)
CALL GetInterpolation(obj=trial, ans=alphaBar, val=alpha)
CALL GetInterpolation(obj=trial, ans=evecBar, val=evec)
ans = ElasticNitscheMatrix( &
& test=test, trial=trial, alpha=alphaBar, evec=evecBar)
DEALLOCATE (alphaBar, evecBar)
END PROCEDURE ElasticNitscheMatrix3a

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3b
REAL(DFP), ALLOCATABLE :: evecBar(:, :)
CALL getInterpolation(obj=trial, ans=evecBar, val=evec)
ans = ElasticNitscheMatrix( &
& test=test, trial=trial, alpha=alpha, evec=evecBar)
DEALLOCATE (evecBar)
END PROCEDURE ElasticNitscheMatrix3b

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3c
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: dd(:, :)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
realval = trial%Ws * trial%Js * trial%Thickness * alpha
ALLOCATE (ans(nns1 * nsd, nns2 * nsd), dd(nns1, nns2))
ans = 0.0_DFP

DO ips = 1, nips

  dd = realval(ips) * &
    & OUTERPROD(a=test%N(1:nns1, ips), b=trial%N(1:nns2, ips))

  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1
    c2 = j * nns2
    r1 = 0
    r2 = r1
    DO i = 1, nsd
      r1 = r2 + 1
      r2 = i * nns1
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
        & evec(i, ips) * evec(j, ips) * dd
    END DO
  END DO
END DO

DEALLOCATE (realval, dd)

END PROCEDURE ElasticNitscheMatrix3c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3d
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: dd(:, :)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
realval = trial%Ws * trial%Js * trial%Thickness * alpha
ALLOCATE (ans(nns1 * nsd, nns2 * nsd), dd(nns1, nns2))
ans = 0.0_DFP

DO ips = 1, nips

  dd = realval(ips) * &
    & OUTERPROD(a=test%N(1:nns1, ips), b=trial%N(1:nns2, ips))

  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1
    c2 = j * nns2
    r1 = 0
    r2 = r1
    DO i = 1, nsd
      r1 = r2 + 1
      r2 = i * nns1
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
        & evec(i, ips) * evec(j, ips) * dd
    END DO
  END DO
END DO

DEALLOCATE (realval, dd)

END PROCEDURE ElasticNitscheMatrix3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3e
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, i, j, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: dd(:, :)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
realval = trial%Ws * trial%Js * trial%Thickness * alpha
ALLOCATE (ans(nns1 * nsd, nns2 * nsd), dd(nns1, nns2))
ans = 0.0_DFP

DO ips = 1, nips

  dd = realval(ips) * &
    & OUTERPROD(a=test%N(1:nns1, ips), b=trial%N(1:nns2, ips))

  c1 = 0; c2 = 0
  DO j = 1, nsd
    c1 = c2 + 1
    c2 = j * nns2
    r1 = 0
    r2 = r1
    DO i = 1, nsd
      r1 = r2 + 1
      r2 = i * nns1
      ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
        & evec(i) * evec(j) * dd
    END DO
  END DO
END DO

DEALLOCATE (realval, dd)

END PROCEDURE ElasticNitscheMatrix3e

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3f
REAL(DFP), ALLOCATABLE :: alphaBar(:)
CALL GetInterpolation(obj=trial, ans=alphaBar, val=alpha)
ans = ElasticNitscheMatrix(test=test, trial=trial, alpha=alphaBar, dim=dim)
DEALLOCATE (alphaBar)
END PROCEDURE ElasticNitscheMatrix3f

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3g
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: realval(:)
nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
realval = trial%Ws * trial%Js * trial%Thickness * alpha
ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP
r1 = (dim - 1) * nns1 + 1
r2 = dim * nns1
c1 = (dim - 1) * nns2 + 1
c2 = dim * nns2
DO ips = 1, nips
  ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
    & realval(ips) * &
    & OUTERPROD(a=test%N(1:nns1, ips), b=trial%N(1:nns2, ips))
END DO
DEALLOCATE (realval)
END PROCEDURE ElasticNitscheMatrix3g

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix3h
INTEGER(I4B) :: nns1, nns2, nsd, nips, ips, r1, r2, c1, c2
REAL(DFP), ALLOCATABLE :: realval(:)
nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
realval = trial%Ws * trial%Js * trial%Thickness * alpha
ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP
r1 = (dim - 1) * nns1 + 1
r2 = dim * nns1
c1 = (dim - 1) * nns2 + 1
c2 = dim * nns2
DO ips = 1, nips
  ans(r1:r2, c1:c2) = ans(r1:r2, c1:c2) + &
    & realval(ips) * &
    & OUTERPROD(a=test%N(1:nns1, ips), b=trial%N(1:nns2, ips))
END DO
DEALLOCATE (realval)
END PROCEDURE ElasticNitscheMatrix3h

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Matrix3
