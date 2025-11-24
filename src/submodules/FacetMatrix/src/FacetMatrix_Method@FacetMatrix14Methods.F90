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

SUBMODULE(FacetMatrix_Method) FacetMatrix14Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_1
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :),  &
  & m4(:, :, :, :)
INTEGER(I4B) :: ips, ii, nips, nns2, nns1, nsd, jj, nsd1, nsd2
  !!
nns2 = SIZE(elemsd%N, 1)
nns1 = SIZE(elemsd%dNdXt, 1)
nsd = SIZE(elemsd%dNdXt, 2)
nips = SIZE(elemsd%dNdXt, 3)
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & ans=masterC1, &
  & c=elemsd%normal)
  !!
CALL Reallocate(m4, nns2, nns1, nsd1, nsd2)
  !!
realval = elemsd%js * elemsd%ws * elemsd%thickness
  !!
DO ips = 1, nips
  DO jj = 1, nsd2
    DO ii = 1, nsd1
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * OUTERPROD( &
        & elemsd%N(:, ips), &
        & masterC1(:, ips) * elemsd%normal(ii + jj - 1, ips))
    END DO
  END DO
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (m4, realval, masterC1)
  !!
END PROCEDURE FacetMatrix14_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_2
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & m4(:, :, :, :)
INTEGER(I4B) :: ips, ii, nips, nns2, nns1, nsd, jj, nsd1, nsd2
  !!
nns2 = SIZE(elemsd%N, 1)
nns1 = SIZE(elemsd%dNdXt, 1)
nsd = SIZE(elemsd%dNdXt, 2)
nips = SIZE(elemsd%dNdXt, 3)
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
CALL Reallocate(m4, nns2, nns1, nsd1, nsd2)
  !!
CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & ans=masterC1, &
  & c=elemsd%normal)
  !!
realval = elemsd%js * elemsd%ws * elemsd%thickness * mu
  !!
DO ips = 1, nips
  DO jj = 1, nsd2
    DO ii = 1, nsd1
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * OUTERPROD( &
        & elemsd%N(:, ips), &
        & masterC1(:, ips) * elemsd%normal(ii + jj - 1, ips))
    END DO
  END DO
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (m4, realval, masterC1)
  !!
END PROCEDURE FacetMatrix14_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_3
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & m4(:, :, :, :), taubar(:)
INTEGER(I4B) :: ips, ii, nips, nns2, nns1, nsd, nsd1, nsd2, jj
  !!
nns2 = SIZE(elemsd%N, 1)
nns1 = SIZE(elemsd%dNdXt, 1)
nsd = SIZE(elemsd%dNdXt, 2)
nips = SIZE(elemsd%dNdXt, 3)
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
CALL Reallocate(m4, nns2, nns1, nsd1, nsd2)
  !!
CALL getProjectionOfdNdXt( &
& obj=elemsd, &
& ans=masterC1, &
& c=elemsd%normal)
  !!
  !!
CALL GetInterpolation(obj=elemsd, ans=taubar, val=tauvar)
  !!
realval = elemsd%js * elemsd%ws * elemsd%thickness * mu * taubar
  !!
DO ips = 1, nips
  DO jj = 1, nsd2
    DO ii = 1, nsd1
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * OUTERPROD( &
        & elemsd%N(:, ips), &
        & masterC1(:, ips) * elemsd%normal(ii + jj - 1, ips))
    END DO
  END DO
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (m4, realval, masterC1, taubar)
  !!
END PROCEDURE FacetMatrix14_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_4
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & m4(:, :, :, :), mubar(:)
INTEGER(I4B) :: ips, ii, nips, nns2, nns1, nsd, nsd1, nsd2, jj
  !!
nns2 = SIZE(elemsd%N, 1)
nns1 = SIZE(elemsd%dNdXt, 1)
nsd = SIZE(elemsd%dNdXt, 2)
nips = SIZE(elemsd%dNdXt, 3)
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
CALL Reallocate(m4, nns2, nns1, nsd1, nsd2)
  !!
CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & ans=masterC1, &
  & c=elemsd%normal)
  !!
CALL GetInterpolation(obj=elemsd, ans=mubar, val=mu)
  !!
realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar
  !!
DO ips = 1, nips
  DO jj = 1, nsd2
    DO ii = 1, nsd1
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * OUTERPROD( &
        & elemsd%N(:, ips), &
        & masterC1(:, ips) * elemsd%normal(ii + jj - 1, ips))
    END DO
  END DO
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (m4, realval, masterC1, mubar)
  !!
END PROCEDURE FacetMatrix14_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_5
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & m4(:, :, :, :), mubar(:), taubar(:)
INTEGER(I4B) :: ips, ii, nips, nns2, nns1, nsd, nsd1, nsd2, jj
  !!
nns2 = SIZE(elemsd%N, 1)
nns1 = SIZE(elemsd%dNdXt, 1)
nsd = SIZE(elemsd%dNdXt, 2)
nips = SIZE(elemsd%dNdXt, 3)
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
CALL Reallocate(m4, nns2, nns1, nsd1, nsd2)
  !!
CALL getProjectionOfdNdXt( &
& obj=elemsd, &
& ans=masterC1, &
& c=elemsd%normal)
  !!
CALL GetInterpolation(obj=elemsd, ans=mubar, val=mu)
CALL GetInterpolation(obj=elemsd, ans=taubar, val=tauvar)
  !!
realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar * taubar
  !!
DO ips = 1, nips
    !!
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * OUTERPROD( &
        & elemsd%N(:, ips), &
        & masterC1(:, ips) * elemsd%normal(ii + jj - 1, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (m4, realval, masterC1, mubar, taubar)
  !!
END PROCEDURE FacetMatrix14_5

END SUBMODULE FacetMatrix14Methods
