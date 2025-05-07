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

SUBMODULE(FacetMatrix_Method) FacetMatrix5Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_1
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C2(:, :, :), m4(:, :, :, :), &
  & G12(:, :, :), i3(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
  & slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
i3 = eye(nsd)
  !!
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
ALLOCATE ( &
  & G12(nns, nsd, nsd), &
  & C2(nsd, nns, nips), &
  & m4(nns, nns, nsd1, nsd2))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & cdNdXt=masterC1, &
  & val=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & cdNdXt=slaveC1, &
  & val=slaveElemsd%normal)
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
  C2(:, 1:nns1, ips) = 0.5_DFP * TRANSPOSE(masterElemSD%dNdXt(:, :, ips))
C2(:, nns1 + 1:, ips) = 0.5_DFP * TRANSPOSE(slaveElemSD%dNdXt(:, :, slaveips))
    !!
END DO
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
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii + jj - 1), C2(:, :, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C2, m4, i3, G12)
  !!
END PROCEDURE FacetMatrix5_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_2
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C2(:, :, :), m4(:, :, :, :), &
  & G12(:, :, :), i3(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
  & slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
i3 = eye(nsd)
  !!
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
ALLOCATE ( &
  & G12(nns, nsd, nsd), &
  & C2(nsd, nns, nips), &
  & m4(nns, nns, nsd1, nsd2))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & cdNdXt=masterC1, &
  & val=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & cdNdXt=slaveC1, &
  & val=slaveElemsd%normal)
  !!
masterC1 = muMaster * masterC1
slaveC1 = muSlave * slaveC1
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
  C2(:, 1:nns1, ips) = 0.5_DFP * TRANSPOSE(masterElemSD%dNdXt(:, :, ips))
C2(:, nns1 + 1:, ips) = 0.5_DFP * TRANSPOSE(slaveElemSD%dNdXt(:, :, slaveips))
    !!
END DO
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
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii + jj - 1), C2(:, :, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C2, m4, i3, G12)
  !!
END PROCEDURE FacetMatrix5_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_3
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C2(:, :, :), m4(:, :, :, :), &
  & G12(:, :, :), i3(:, :)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
  & slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
i3 = eye(nsd)
  !!
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
ALLOCATE ( &
  & G12(nns, nsd, nsd), &
  & C2(nsd, nns, nips), &
  & m4(nns, nns, nsd1, nsd2))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & cdNdXt=masterC1, &
  & val=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & cdNdXt=slaveC1, &
  & val=slaveElemsd%normal)
  !!
masterC1 = muMaster * masterC1
slaveC1 = muSlave * slaveC1
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
  C2(:, 1:nns1, ips) = (0.5_DFP * tauMaster) * TRANSPOSE( &
    & masterElemSD%dNdXt(:, :, ips))
  C2(:, nns1 + 1:, ips) = (0.5_DFP * tauSlave) * TRANSPOSE( &
    & slaveElemSD%dNdXt(:, :, slaveips))
    !!
END DO
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
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii + jj - 1), C2(:, :, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C2, m4, i3, G12)
  !!
END PROCEDURE FacetMatrix5_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_4
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C2(:, :, :), m4(:, :, :, :), &
  & G12(:, :, :), i3(:, :), muMasterBar(:), muSlaveBar(:)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
  & slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
i3 = eye(nsd)
  !!
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
ALLOCATE ( &
  & G12(nns, nsd, nsd), &
  & C2(nsd, nns, nips), &
  & m4(nns, nns, nsd1, nsd2))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & cdNdXt=masterC1, &
  & val=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & cdNdXt=slaveC1, &
  & val=slaveElemsd%normal)
  !!
CALL getInterpolation( &
  & obj=masterElemSD, &
  & interpol=muMasterBar, &
  & val=muMaster)
  !!
CALL getInterpolation( &
  & obj=slaveElemSD, &
  & interpol=muSlaveBar, &
  & val=muSlave)
  !!
DO ips = 1, nips
  masterC1(:, ips) = muMasterBar(ips) * masterC1(:, ips)
  slaveC1(:, ips) = muSlaveBar(ips) * slaveC1(:, ips)
END DO
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
  C2(:, 1:nns1, ips) = (0.5_DFP) * TRANSPOSE( &
    & masterElemSD%dNdXt(:, :, ips))
  C2(:, nns1 + 1:, ips) = (0.5_DFP) * TRANSPOSE( &
    & slaveElemSD%dNdXt(:, :, slaveips))
    !!
END DO
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
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii + jj - 1), C2(:, :, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C2, m4, i3, &
  & muMasterBar, muSlaveBar, G12)
  !!
END PROCEDURE FacetMatrix5_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_5
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C2(:, :, :), m4(:, :, :, :), &
  & G12(:, :, :), i3(:, :), tauMasterBar(:), tauSlaveBar(:)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
  & slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
i3 = eye(nsd)
  !!
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
ALLOCATE ( &
  & G12(nns, nsd, nsd), &
  & C2(nsd, nns, nips), &
  & m4(nns, nns, nsd1, nsd2))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & cdNdXt=masterC1, &
  & val=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & cdNdXt=slaveC1, &
  & val=slaveElemsd%normal)
  !!
CALL getInterpolation( &
  & obj=masterElemSD, &
  & interpol=tauMasterBar, &
  & val=tauMaster)
  !!
CALL getInterpolation( &
  & obj=slaveElemSD, &
  & interpol=tauSlaveBar, &
  & val=tauSlave)
  !!
masterC1 = muMaster * masterC1
slaveC1 = muSlave * slaveC1
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  C2(:, 1:nns1, ips) = (0.5_DFP * tauMasterBar(ips)) * TRANSPOSE( &
    & masterElemSD%dNdXt(:, :, ips))
    !!
  C2(:, nns1 + 1:, ips) = (0.5_DFP * tauSlaveBar(slaveips)) * TRANSPOSE( &
    & slaveElemSD%dNdXt(:, :, slaveips))
    !!
END DO
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
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii + jj - 1), C2(:, :, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C2, m4, i3, &
  & tauMasterBar, tauSlaveBar, G12)
  !!
END PROCEDURE FacetMatrix5_5

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_6
  !!
REAL(DFP), ALLOCATABLE :: realval(:), masterC1(:, :), &
  & slaveC1(:, :), C2(:, :, :), m4(:, :, :, :), &
  & G12(:, :, :), i3(:, :), tauMasterBar(:), tauSlaveBar(:), &
  & muMasterBar(:), muSlaveBar(:)
INTEGER(I4B) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
  & slaveips
  !!
nns1 = SIZE(masterElemSD%dNdXt, 1)
nsd = SIZE(masterElemSD%dNdXt, 2)
nips = SIZE(masterElemSD%dNdXt, 3)
nns2 = SIZE(slaveElemSD%dNdXt, 1)
nns = nns1 + nns2
  !!
i3 = eye(nsd)
  !!
IF (opt .EQ. 1) THEN
  nsd1 = nsd
  nsd2 = 1
ELSE
  nsd1 = 1
  nsd2 = nsd
END IF
  !!
ALLOCATE ( &
  & G12(nns, nsd, nsd), &
  & C2(nsd, nns, nips), &
  & m4(nns, nns, nsd1, nsd2))
  !!
CALL getProjectionOfdNdXt( &
  & obj=masterElemsd, &
  & cdNdXt=masterC1, &
  & val=masterElemsd%normal)
  !!
CALL getProjectionOfdNdXt( &
  & obj=slaveElemsd, &
  & cdNdXt=slaveC1, &
  & val=slaveElemsd%normal)
  !!
CALL getInterpolation( &
  & obj=masterElemSD, &
  & interpol=muMasterBar, &
  & val=muMaster)
  !!
CALL getInterpolation( &
  & obj=slaveElemSD, &
  & interpol=muSlaveBar, &
  & val=muSlave)
  !!
CALL getInterpolation( &
  & obj=masterElemSD, &
  & interpol=tauMasterBar, &
  & val=tauMaster)
  !!
CALL getInterpolation( &
  & obj=slaveElemSD, &
  & interpol=tauSlaveBar, &
  & val=tauSlave)
  !!
DO ips = 1, nips
  masterC1(:, ips) = muMasterBar(ips) * masterC1(:, ips)
  slaveC1(:, ips) = muSlaveBar(ips) * slaveC1(:, ips)
END DO
  !!
DO ips = 1, nips
    !!
  slaveips = quadMap(ips)
    !!
  C2(:, 1:nns1, ips) = (0.5_DFP * tauMasterBar(ips)) * TRANSPOSE( &
    & masterElemSD%dNdXt(:, :, ips))
    !!
  C2(:, nns1 + 1:, ips) = (0.5_DFP * tauSlaveBar(slaveips)) * TRANSPOSE( &
    & slaveElemSD%dNdXt(:, :, slaveips))
    !!
END DO
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
  DO jj = 1, nsd2
    DO ii = 1, nsd1
        !!
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * MATMUL(G12(:, :, ii + jj - 1), C2(:, :, ips))
        !!
    END DO
  END DO
    !!
END DO
  !!
CALL Convert(from=m4, to=ans)
  !!
DEALLOCATE (realval, masterC1, slaveC1, C2, m4, i3, &
  & tauMasterBar, tauSlaveBar, muMasterBar, muSlaveBar, G12)
  !!
END PROCEDURE FacetMatrix5_6

END SUBMODULE FacetMatrix5Methods
