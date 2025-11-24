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

SUBMODULE(FacetMatrix_Method) FacetMatrix11Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix11_1
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C1(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, slaveips, nns
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nips = SIZE(masterElemSD%dNdXt, 3)
nns = nns1 + nns2
nsd = masterElemSD%nsd
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & ans=masterC1, &
  & c=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & ans=slaveC1, &
  & c=slaveElemsd%normal)
  !!
ALLOCATE (C1(nns, nips), ans(nns, nns)); ans = 0.0_DFP
  !!
DO ips = 1, nips
  slaveips = quadMap(ips)
  C1(1:nns1, ips) = masterC1(:, ips)
  C1(1 + nns1:, ips) = slaveC1(:, slaveips)
END DO
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
DO ips = 1, nips
    !!
  ans = ans + &
    & realval(ips) * OUTERPROD(C1(:, ips), C1(:, ips))
    !!
END DO
  !!
IF (PRESENT(nCopy)) CALL MakeDiagonalCopies(ans, nCopy)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C1)
  !!
END PROCEDURE FacetMatrix11_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix11_2
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), slaveC1(:, :), &
  & C1(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, slaveips, nns
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
ALLOCATE (C1(nns, nips), ans(nns, nns)); ans = 0.0_DFP
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & ans=masterC1, &
  & c=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & ans=slaveC1, &
  & c=slaveElemsd%normal)
  !!
masterC1 = masterC1 * muMaster
slaveC1 = slaveC1 * muSlave
  !!
DO ips = 1, nips
  slaveips = quadMap(ips)
  C1(1:nns1, ips) = masterC1(:, ips)
  C1(1 + nns1:, ips) = slaveC1(:, slaveips)
END DO
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
DO ips = 1, nips
    !!
  ans = ans &
    & + realval(ips) * OUTERPROD(C1(:, ips), C1(:, ips))
    !!
END DO
  !!
IF (PRESENT(nCopy)) CALL MakeDiagonalCopies(ans, nCopy)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C1)
  !!
END PROCEDURE FacetMatrix11_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix11_3
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), taubar(:), C1(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, slaveips, nns
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
ALLOCATE (C1(nns, nips), ans(nns, nns)); ans = 0.0_DFP
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & ans=masterC1, &
  & c=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & ans=slaveC1, &
  & c=slaveElemsd%normal)
  !!
CALL GetInterpolation(obj=masterElemSD, ans=taubar, val=tauvar)
  !!
masterC1 = masterC1 * muMaster
slaveC1 = slaveC1 * muSlave
  !!
DO ips = 1, nips
  slaveips = quadMap(ips)
  C1(1:nns1, ips) = masterC1(:, ips)
  C1(1 + nns1:, ips) = slaveC1(:, slaveips)
END DO
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness * &
  & taubar
  !!
DO ips = 1, nips
  ans = ans &
    & + realval(ips) * OUTERPROD(C1(:, ips), C1(:, ips))
END DO
  !!
IF (PRESENT(nCopy)) CALL MakeDiagonalCopies(ans, nCopy)
  !!
DEALLOCATE (realval, masterC1, slaveC1, taubar, C1)
  !!
END PROCEDURE FacetMatrix11_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix11_4
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & muMasterBar(:), muSlaveBar(:), slaveC1(:, :), C1(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, slaveips, nns
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
ALLOCATE (C1(nns, nips), ans(nns, nns)); ans = 0.0_DFP
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & ans=masterC1, &
  & c=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & ans=slaveC1, &
  & c=slaveElemsd%normal)
  !!
CALL GetInterpolation(obj=masterElemSD, ans=muMasterBar, val=muMaster)
  !!
CALL GetInterpolation(obj=slaveElemSD, ans=muSlaveBar, val=muSlave)
  !!
DO ips = 1, nips
  slaveips = quadMap(ips)
  masterC1(:, ips) = muMasterBar(ips) * masterC1(:, ips)
  slaveC1(:, ips) = muSlaveBar(ips) * slaveC1(:, ips)
  C1(1:nns1, ips) = masterC1(:, ips)
  C1(1 + nns1:, ips) = slaveC1(:, slaveips)
END DO
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
DO ips = 1, nips
  ans = ans &
    & + realval(ips) * OUTERPROD(C1(:, ips), C1(:, ips))
END DO
  !!
IF (PRESENT(nCopy)) CALL MakeDiagonalCopies(ans, nCopy)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C1, muMasterBar, muSlaveBar)
  !!
END PROCEDURE FacetMatrix11_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix11_5
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & muMasterBar(:), muSlaveBar(:), tauBar(:), slaveC1(:, :), &
  & C1(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, slaveips, nns
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
ALLOCATE (C1(nns, nips), ans(nns, nns))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & ans=masterC1, &
  & c=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & ans=slaveC1, &
  & c=slaveElemsd%normal)
  !!
CALL GetInterpolation(obj=masterElemSD, ans=muMasterBar, val=muMaster)
  !!
CALL GetInterpolation(obj=slaveElemSD, ans=muSlaveBar, val=muSlave)
  !!
CALL GetInterpolation(obj=masterElemSD, ans=tauBar, val=tauvar)
  !!
DO ips = 1, nips
  slaveips = quadMap(ips)
  masterC1(:, ips) = muMasterBar(ips) * masterC1(:, ips)
  slaveC1(:, ips) = muSlaveBar(ips) * slaveC1(:, ips)
  C1(1:nns1, ips) = masterC1(:, ips)
  C1(1 + nns1:, ips) = slaveC1(:, slaveips)
END DO
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness * tauBar
  !!
DO ips = 1, nips
  ans = ans &
    & + realval(ips) * OUTERPROD(C1(:, ips), C1(:, ips))
END DO
  !!
IF (PRESENT(nCopy)) CALL MakeDiagonalCopies(ans, nCopy)
  !!
DEALLOCATE (realval, masterC1, slaveC1, muMasterBar, &
  & muSlaveBar, C1)
  !!
END PROCEDURE FacetMatrix11_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetMatrix11Methods
