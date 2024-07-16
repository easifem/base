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

SUBMODULE(FacetMatrix_Method) FacetMatrix1Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_1
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), m4(:, :, :, :), G12(:, :, :), i3(:, :)
INTEGER(I4B) :: ips, ii, jj, nips, nns1, nns2, nsd, slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemSD, &
  & cdNdXt=masterC1, &
  & val=masterElemSD%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemSD, &
  & cdNdXt=slaveC1, &
  & val=slaveElemSD%normal)
  !!
i3 = eye(nsd)
  !!
CALL Reallocate(G12, nns1 + nns2, nsd, nsd)
CALL Reallocate(m4, nns1 + nns2, nns1 + nns2, nsd, nsd)
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  G12(1:nns1, :, :) = OUTERPROD( &
    & masterC1(:, ips), i3) &
    & + OUTERPROD(masterElemSD%dNdXt(:, :, ips),  &
    & masterElemSD%normal(1:nsd, ips))
    !!
  G12(nns1 + 1:, :, :) = OUTERPROD( &
    & slaveC1(:, slaveips), i3) &
    & + OUTERPROD(slaveElemSD%dNdXt(:, :, slaveips), &
    & slaveElemSD%normal(1:nsd, slaveips))
    !!
  DO jj = 1, nsd
      !!
    DO ii = 1, nsd
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii), &
        & TRANSPOSE(G12(:, :, jj)))
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, m4, G12, i3)
  !!
END PROCEDURE FacetMatrix1_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_2
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), m4(:, :, :, :), G12(:, :, :), i3(:, :)
INTEGER(I4B) :: ips, ii, jj, nips, nns1, nns2, nsd, slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemSD, &
  & cdNdXt=masterC1, &
  & val=masterElemSD%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemSD, &
  & cdNdXt=slaveC1, &
  & val=slaveElemSD%normal)
  !!
i3 = eye(nsd)
  !!
CALL Reallocate(G12, nns1 + nns2, nsd, nsd)
CALL Reallocate(m4, nns1 + nns2, nns1 + nns2, nsd, nsd)
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  G12(1:nns1, :, :) = muMaster * OUTERPROD( &
    & masterC1(:, ips), i3) &
    & + muMaster * OUTERPROD(masterElemSD%dNdXt(:, :, ips),  &
    & masterElemSD%normal(1:nsd, ips))
    !!
  G12(nns1 + 1:, :, :) = muSlave * OUTERPROD( &
    & slaveC1(:, slaveips), i3) &
    & + muSlave * OUTERPROD(slaveElemSD%dNdXt(:, :, slaveips), &
    & slaveElemSD%normal(1:nsd, slaveips))
    !!
  DO jj = 1, nsd
      !!
    DO ii = 1, nsd
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii), &
        & TRANSPOSE(G12(:, :, jj)))
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, m4, G12, i3)
  !!
END PROCEDURE FacetMatrix1_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_3
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), m4(:, :, :, :), G12(:, :, :), i3(:, :), &
  & taubar(:)
INTEGER(I4B) :: ips, ii, jj, nips, nns1, nns2, nsd, slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemSD, &
  & cdNdXt=masterC1, &
  & val=masterElemSD%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemSD, &
  & cdNdXt=slaveC1, &
  & val=slaveElemSD%normal)
  !!
CALL getInterpolation(obj=masterElemSD, Interpol=taubar, val=tauvar)
  !!
i3 = eye(nsd)
  !!
CALL Reallocate(G12, nns1 + nns2, nsd, nsd)
CALL Reallocate(m4, nns1 + nns2, nns1 + nns2, nsd, nsd)
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness &
 & * taubar
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  G12(1:nns1, :, :) = muMaster * OUTERPROD( &
    & masterC1(:, ips), i3) &
    & + muMaster * OUTERPROD(masterElemSD%dNdXt(:, :, ips),  &
    & masterElemSD%normal(1:nsd, ips))
    !!
  G12(nns1 + 1:, :, :) = muSlave * OUTERPROD( &
    & slaveC1(:, slaveips), i3) &
    & + muSlave * OUTERPROD(slaveElemSD%dNdXt(:, :, slaveips), &
    & slaveElemSD%normal(1:nsd, slaveips))
    !!
  DO jj = 1, nsd
      !!
    DO ii = 1, nsd
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii), &
        & TRANSPOSE(G12(:, :, jj)))
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, m4, G12, i3, taubar)
  !!
END PROCEDURE FacetMatrix1_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_4
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), m4(:, :, :, :), G12(:, :, :), i3(:, :), &
  & muMasterBar(:), muSlaveBar(:)
INTEGER(I4B) :: ips, ii, jj, nips, nns1, nns2, nsd, slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemSD, &
  & cdNdXt=masterC1, &
  & val=masterElemSD%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemSD, &
  & cdNdXt=slaveC1, &
  & val=slaveElemSD%normal)
  !!
CALL getInterpolation(obj=masterElemSD, interpol=muMasterBar, &
  & val=muMaster)
CALL getInterpolation(obj=slaveElemSD, interpol=muSlaveBar, &
  & val=muSlave)
  !!
i3 = eye(nsd)
  !!
CALL Reallocate(G12, nns1 + nns2, nsd, nsd)
CALL Reallocate(m4, nns1 + nns2, nns1 + nns2, nsd, nsd)
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  G12(1:nns1, :, :) = muMasterBar(ips) * OUTERPROD( &
    & masterC1(:, ips), i3) &
    & + muMasterBar(ips) * OUTERPROD(masterElemSD%dNdXt(:, :, ips),  &
    & masterElemSD%normal(1:nsd, ips))
    !!
  G12(nns1 + 1:, :, :) = muSlaveBar(slaveips) * OUTERPROD( &
    & slaveC1(:, slaveips), i3) &
   & + muSlaveBar(slaveips) * OUTERPROD(slaveElemSD%dNdXt(:, :, slaveips), &
    & slaveElemSD%normal(1:nsd, slaveips))
    !!
  DO jj = 1, nsd
      !!
    DO ii = 1, nsd
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii), &
        & TRANSPOSE(G12(:, :, jj)))
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, m4, G12, i3, muMasterBar, &
  & muSlaveBar)
  !!
END PROCEDURE FacetMatrix1_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_5
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), m4(:, :, :, :), G12(:, :, :), i3(:, :), &
  & muMasterBar(:), muSlaveBar(:), taubar(:)
INTEGER(I4B) :: ips, ii, jj, nips, nns1, nns2, nsd, slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemSD, &
  & cdNdXt=masterC1, &
  & val=masterElemSD%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemSD, &
  & cdNdXt=slaveC1, &
  & val=slaveElemSD%normal)
  !!
CALL getInterpolation(obj=masterElemSD, interpol=muMasterBar, &
  & val=muMaster)
CALL getInterpolation(obj=slaveElemSD, interpol=muSlaveBar, &
  & val=muSlave)
CALL getInterpolation(obj=masterElemSD, interpol=taubar, val=tauvar)
  !!
i3 = eye(nsd)
  !!
CALL Reallocate(G12, nns1 + nns2, nsd, nsd)
CALL Reallocate(m4, nns1 + nns2, nns1 + nns2, nsd, nsd)
  !!
realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness &
 & * taubar
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  G12(1:nns1, :, :) = muMasterBar(ips) * OUTERPROD( &
    & masterC1(:, ips), i3) &
    & + muMasterBar(ips) * OUTERPROD(masterElemSD%dNdXt(:, :, ips),  &
    & masterElemSD%normal(1:nsd, ips))
    !!
  G12(nns1 + 1:, :, :) = muSlaveBar(slaveips) * OUTERPROD( &
    & slaveC1(:, slaveips), i3) &
   & + muSlaveBar(slaveips) * OUTERPROD(slaveElemSD%dNdXt(:, :, slaveips), &
    & slaveElemSD%normal(1:nsd, slaveips))
    !!
  DO jj = 1, nsd
      !!
    DO ii = 1, nsd
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii), &
        & TRANSPOSE(G12(:, :, jj)))
        !!
    END DO
      !!
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, m4, G12, i3, muMasterBar, &
  & muSlaveBar, taubar)
  !!
END PROCEDURE FacetMatrix1_5

END SUBMODULE FacetMatrix1Methods
