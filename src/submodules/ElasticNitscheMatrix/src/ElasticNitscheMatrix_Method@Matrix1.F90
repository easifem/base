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

SUBMODULE(ElasticNitscheMatrix_Method) Matrix1
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1a
REAL(DFP), ALLOCATABLE :: lamBar(:), muBar(:), evecBar(:, :)
CALL getInterpolation(obj=trial, interpol=lamBar, val=lambda)
CALL getInterpolation(obj=trial, interpol=muBar, val=mu)
CALL getInterpolation(obj=trial, interpol=evecBar, val=evec)
ans = ElasticNitscheMatrix( &
& test=test, trial=trial, lambda=lamBar, mu=muBar, evec=evecBar)
DEALLOCATE (lamBar, muBar, evecBar)
END PROCEDURE ElasticNitscheMatrix1a

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1b
REAL(DFP), ALLOCATABLE :: evecBar(:, :)
CALL getInterpolation(obj=trial, interpol=evecBar, val=evec)
ans = ElasticNitscheMatrix( &
& test=test, &
& trial=trial, &
& lambda=lambda, &
& mu=mu, &
& evec=evecBar)
DEALLOCATE (evecBar)
END PROCEDURE ElasticNitscheMatrix1b

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1c
REAL(DFP), ALLOCATABLE :: evecBar(:, :)
CALL getInterpolation(obj=trial, interpol=evecBar, val=evec)
ans = ElasticNitscheMatrix(test=test, trial=trial, &
  & lambda=lambda, mu=mu, evec=evecBar)
DEALLOCATE (evecBar)
END PROCEDURE ElasticNitscheMatrix1c

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1d
REAL(DFP), ALLOCATABLE :: realval(:), ff(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i
REAL(DFP) :: dd(3, 3)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
ALLOCATE (ff(nns1, nsd * nns2), realval(nips))
realval = trial%Ws * trial%Js * trial%Thickness
ALLOCATE (ans(nns1 * nsd, nns2 * nsd)); ans = 0.0_DFP

DO ips = 1, nips
  dd(1:nsd, 1:nsd) = &
  &   lambda(ips) * realval(ips) &
  & * DOT_PRODUCT(trial%normal(1:nsd, ips), evec(1:nsd, ips)) &
  & * Eye3(1:nsd, 1:nsd) &
  & + 2.0 * mu(ips) * realval(ips) &
  & * OUTERPROD(a=trial%normal(1:nsd, ips), &
              & b=evec(1:nsd, ips), &
              & Sym=.TRUE.)

  ff = OUTERPROD( &
                  & test%N(1:nns1, ips), &
                  & RESHAPE( &
                              & MATMUL( &
                                        & trial%dNdXt(1:nns2, 1:nsd, ips), &
                                        & dd(1:nsd, 1:nsd) &
                                    & ), &
                              & [nsd * nns2] &
                          & ) &
              & )

  r1 = 0; r2 = 0
  DO i = 1, nsd
    r1 = r2 + 1; r2 = i * nns1
    ans(r1:r2, :) = ans(r1:r2, :) + evec(i, ips) * ff
  END DO
END DO

DEALLOCATE (realval, ff)

END PROCEDURE ElasticNitscheMatrix1d

!----------------------------------------------------------------------------
!                                                       ElasticNitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1e
REAL(DFP), ALLOCATABLE :: realval(:), ff(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i, j
REAL(DFP) :: dd(3, 3)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
ALLOCATE (ff(nns1, nsd * nns2), realval(nips))
realval = trial%Ws * trial%Js * trial%Thickness
ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP

DO ips = 1, nips
  dd(1:nsd, 1:nsd) = &
  &   lambda * realval(ips) &
  & * DOT_PRODUCT(trial%normal(1:nsd, ips), evec(1:nsd, ips)) &
  & * Eye3(1:nsd, 1:nsd) &
  & + 2.0 * mu * realval(ips) &
  & * OUTERPROD(a=trial%normal(1:nsd, ips), &
              & b=evec(1:nsd, ips), &
              & Sym=.TRUE.)

  ff = OUTERPROD( &
                  & test%N(1:nns1, ips), &
                  & RESHAPE( &
                              & MATMUL( &
                                        & trial%dNdXt(1:nns2, 1:nsd, ips), &
                                        & dd(1:nsd, 1:nsd) &
                                    & ), &
                              & [nsd * nns2] &
                          & ) &
              & )

  r1 = 0; r2 = 0
  DO i = 1, nsd
    r1 = r2 + 1; r2 = i * nns1
    ans(r1:r2, :) = ans(r1:r2, :) + evec(i, ips) * ff
  END DO
END DO

DEALLOCATE (realval, ff)

END PROCEDURE ElasticNitscheMatrix1e

!----------------------------------------------------------------------------
!                                                       ElasticNitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1f
REAL(DFP), ALLOCATABLE :: evecBar(:, :)
INTEGER(I4B) :: ii, nips
nips = SIZE(trial%N, 2)
ALLOCATE (evecBar(SIZE(evec), nips))
DO ii = 1, nips
  evecBar(:, ii) = evec
END DO
ans = ElasticNitscheMatrix(test=test, trial=trial, &
  & lambda=lambda, mu=mu, evec=evecBar)
DEALLOCATE (evecBar)
END PROCEDURE ElasticNitscheMatrix1f

!----------------------------------------------------------------------------
!                                                       ElasticNitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1g
REAL(DFP), ALLOCATABLE :: evecBar(:, :)
INTEGER(I4B) :: ii, nips
nips = SIZE(trial%N, 2)
ALLOCATE (evecBar(SIZE(evec), nips))
DO ii = 1, nips
  evecBar(:, ii) = evec
END DO
ans = ElasticNitscheMatrix(test=test, trial=trial, &
  & lambda=lambda, mu=mu, evec=evecBar)
DEALLOCATE (evecBar)
END PROCEDURE ElasticNitscheMatrix1g

!----------------------------------------------------------------------------
!                                                       ElasticNitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1h
REAL(DFP), ALLOCATABLE :: realval(:), ff(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i
REAL(DFP) :: dd(3, 3)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
ALLOCATE (ff(nns1, nsd * nns2), realval(nips))
realval = trial%Ws * trial%Js * trial%Thickness
ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP

r1 = 1; r2 = dim * nns1

DO ips = 1, nips

  dd(1:nsd, 1:nsd) = &
  &   lambda(ips) * realval(ips) * trial%normal(dim, ips) &
  & * Eye3(1:nsd, 1:nsd) &
  & + 2.0 * mu(ips) * realval(ips) &
  & * OUTERPROD(a=trial%normal(1:nsd, ips), &
              & b=Eye3(1:nsd, dim), &
              & Sym=.TRUE.)

  ff = OUTERPROD( &
                  & test%N(1:nns1, ips), &
                  & RESHAPE( &
                              & MATMUL( &
                                        & trial%dNdXt(1:nns2, 1:nsd, ips), &
                                        & dd(1:nsd, 1:nsd) &
                                    & ), &
                              & [nsd * nns2] &
                          & ) &
              & )

  ans(r1:r2, :) = ans(r1:r2, :) + ff
END DO

DEALLOCATE (realval, ff)

END PROCEDURE ElasticNitscheMatrix1h

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1i
REAL(DFP), ALLOCATABLE :: realval(:), ff(:, :)
INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i
REAL(DFP) :: dd(3, 3)

nns1 = SIZE(test%N, 1)
nns2 = SIZE(trial%N, 1)
nips = SIZE(trial%N, 2)
nsd = trial%nsd
ALLOCATE (ff(nns1, nsd * nns2), realval(nips))
realval = trial%Ws * trial%Js * trial%Thickness
ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
ans = 0.0_DFP

r1 = 1; r2 = dim * nns1

DO ips = 1, nips

  dd(1:nsd, 1:nsd) = &
  &   lambda * realval(ips) * trial%normal(dim, ips) &
  & * Eye3(1:nsd, 1:nsd) &
  & + 2.0 * mu * realval(ips) &
  & * OUTERPROD(a=trial%normal(1:nsd, ips), &
              & b=Eye3(1:nsd, dim), &
              & Sym=.TRUE.)

  ff = OUTERPROD( &
                  & test%N(1:nns1, ips), &
                  & RESHAPE( &
                              & MATMUL( &
                                        & trial%dNdXt(1:nns2, 1:nsd, ips), &
                                        & dd(1:nsd, 1:nsd) &
                                    & ), &
                              & [nsd * nns2] &
                          & ) &
              & )

  ans(r1:r2, :) = ans(r1:r2, :) + ff
END DO

DEALLOCATE (realval, ff)

END PROCEDURE ElasticNitscheMatrix1i

!----------------------------------------------------------------------------
!                                                              NitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ElasticNitscheMatrix1j
REAL(DFP), ALLOCATABLE :: lamBar(:), muBar(:)
CALL getInterpolation(obj=trial, interpol=lamBar, val=lambda)
CALL getInterpolation(obj=trial, interpol=muBar, val=mu)
ans = ElasticNitscheMatrix( &
& test=test, trial=trial, lambda=lamBar, mu=muBar, dim=dim)
DEALLOCATE (lamBar, muBar)
END PROCEDURE ElasticNitscheMatrix1j

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Matrix1
