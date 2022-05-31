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
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), masterG12( :, :, : ), slaveG12( :, :, : ), &
    & m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd, slaveips
  !!
  nns = SIZE( masterElemSD%N, 1 )
  nips = SIZE( masterElemSD%N, 2 )
  nsd = masterElemSD%refelem%nsd
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt(obj=masterElemSD, cdNdXt=masterC1, &
    & val=masterElemSD%normal )
  !!
  CALL getProjectionOfdNdXt(obj=slaveElemSD, cdNdXt=slaveC1, &
    & val=slaveElemSD%normal )
  !!
  CALL Reallocate( masterG12, nns, nsd, nsd )
  CALL Reallocate( slaveG12, nns, nsd, nsd )
  CALL Reallocate(m4, nns, nns, nsd, nsd )
  !!
  DO ips = 1, nips
    slaveips=quadMap(ips)
    !!
    masterG12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( masterElemSD%dNdXt( :, :, ips ),  &
      & masterElemSD%normal( 1:nsd, ips ) )
    !!
    slaveG12 = OUTERPROD( slaveC1( :, slaveips ), eye( nsd ) ) &
      & + OUTERPROD( slaveElemSD%dNdXt( :, :, slaveips ), &
      & slaveElemSD%normal( 1:nsd, slaveips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( masterG12( :, :, ii ), &
          & TRANSPOSE( slaveG12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, slaveC1, masterG12, slaveG12 )
  !!
END PROCEDURE FacetMatrix1_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), masterG12( :, :, : ), slaveG12( :, :, : ), &
    & m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd, slaveips
  !!
  nns = SIZE( masterElemSD%N, 1 )
  nips = SIZE( masterElemSD%N, 2 )
  nsd = masterElemSD%refelem%nsd
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt(obj=masterElemSD, cdNdXt=masterC1, &
    & val=masterElemSD%normal )
  !!
  CALL getProjectionOfdNdXt(obj=slaveElemSD, cdNdXt=slaveC1, &
    & val=slaveElemSD%normal )
  !!
  CALL Reallocate(masterG12, nns, nsd, nsd)
  CALL Reallocate(slaveG12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    slaveips=quadMap(ips)
    !!
    masterG12 = muMaster*OUTERPROD(masterC1( :, ips ), eye( nsd, 1.0_DFP )) &
      & + muMaster*OUTERPROD( masterElemSD%dNdXt( :, :, ips ),  &
      & masterElemSD%normal( 1:nsd, ips ) )
    !!
    slaveG12 = muSlave*OUTERPROD( slaveC1( :, slaveips ), eye( nsd ) ) &
      & + muSlave*OUTERPROD( slaveElemSD%dNdXt( :, :, slaveips ), &
      & slaveElemSD%normal( 1:nsd, slaveips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( masterG12( :, :, ii ), &
          & TRANSPOSE( slaveG12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, slaveC1, masterG12, slaveG12 )
  !!
END PROCEDURE FacetMatrix1_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), masterG12( :, :, : ), slaveG12( :, :, : ), &
    & m4( :, :, :, : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd, slaveips
  !!
  nns = SIZE( masterElemSD%N, 1 )
  nips = SIZE( masterElemSD%N, 2 )
  nsd = masterElemSD%refelem%nsd
  !!
  CALL getProjectionOfdNdXt(obj=masterElemSD, cdNdXt=masterC1, &
    & val=masterElemSD%normal )
  !!
  CALL getProjectionOfdNdXt(obj=slaveElemSD, cdNdXt=slaveC1, &
    & val=slaveElemSD%normal )
  !!
  CALL getInterpolation(obj=masterElemSD, Interpol=taubar, val=tauvar)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness &
    & * taubar
  !!
  CALL Reallocate(masterG12, nns, nsd, nsd)
  CALL Reallocate(slaveG12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    slaveips=quadMap(ips)
    !!
    masterG12=muMaster*OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + muMaster*OUTERPROD( masterElemSD%dNdXt( :, :, ips ),  &
      & masterElemSD%normal( 1:nsd, ips ) )
    !!
    slaveG12 = muSlave*OUTERPROD( slaveC1( :, slaveips ), eye( nsd ) ) &
      & + muSlave*OUTERPROD( slaveElemSD%dNdXt( :, :, slaveips ), &
      & slaveElemSD%normal( 1:nsd, slaveips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( masterG12( :, :, ii ), &
          & TRANSPOSE( slaveG12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, slaveC1, masterG12, slaveG12, taubar )
  !!
END PROCEDURE FacetMatrix1_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_4
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), masterG12( :, :, : ), slaveG12( :, :, : ), &
    & m4( :, :, :, : ), muMasterBar( : ), muSlaveBar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd, slaveips
  !!
  nns = SIZE( masterElemSD%N, 1 )
  nips = SIZE( masterElemSD%N, 2 )
  nsd = masterElemSD%refelem%nsd
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt(obj=masterElemSD, cdNdXt=masterC1, &
    & val=masterElemSD%normal )
  CALL getProjectionOfdNdXt(obj=slaveElemSD, cdNdXt=slaveC1, &
    & val=slaveElemSD%normal )
  CALL getInterpolation( obj=masterElemSD, interpol=muMasterBar, &
    & val=muMaster )
  CALL getInterpolation( obj=slaveElemSD, interpol=muSlaveBar, &
    & val=muSlave )
  !!
  CALL Reallocate(masterG12, nns, nsd, nsd)
  CALL Reallocate(slaveG12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    slaveips=quadMap(ips)
    !!
    masterG12 = muMasterBar( ips ) &
      & * OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + muMasterBar( ips ) &
      & * OUTERPROD( masterElemSD%dNdXt( :, :, ips ),  &
      & masterElemSD%normal( 1:nsd, ips ) )
    !!
    slaveG12 = muSlaveBar( slaveips ) &
      & * OUTERPROD( slaveC1( :, slaveips ), eye( nsd ) ) &
      & + muSlaveBar( ips ) &
      & * OUTERPROD( slaveElemSD%dNdXt( :, :, slaveips ), &
      & slaveElemSD%normal( 1:nsd, slaveips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( masterG12( :, :, ii ), &
          & TRANSPOSE( slaveG12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, slaveC1, masterG12, muMasterBar, &
    & muSlaveBar, slaveG12 )
  !!
END PROCEDURE FacetMatrix1_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix1
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix1_5
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), masterG12( :, :, : ), slaveG12( :, :, : ), &
    & m4( :, :, :, : ), muMasterBar( : ), muSlaveBar( : ), &
    & tauBar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd, slaveips
  !!
  nns = SIZE( masterElemSD%N, 1 )
  nips = SIZE( masterElemSD%N, 2 )
  nsd = masterElemSD%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemSD, &
    & cdNdXt=masterC1, &
    & val=masterElemSD%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemSD, &
    & cdNdXt=slaveC1, &
    & val=slaveElemSD%normal )
  !!
  CALL getInterpolation( &
    & obj=masterElemSD, &
    & interpol=muMasterBar, &
    & val=muMaster )
  !!
  CALL getInterpolation( &
    & obj=slaveElemSD, &
    & interpol=muSlaveBar, &
    & val=muSlave )
  !!
  CALL getInterpolation( &
    & obj=masterElemSD, &
    & interpol=tauBar, &
    & val=tauvar )
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness &
    * tauBar
  !!
  CALL Reallocate(masterG12, nns, nsd, nsd)
  CALL Reallocate(slaveG12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    slaveips=quadMap(ips)
    !!
    masterG12 = muMasterBar( ips ) &
      & * OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + muMasterBar( ips ) &
      & * OUTERPROD( masterElemSD%dNdXt( :, :, ips ),  &
      & masterElemSD%normal( 1:nsd, ips ) )
    !!
    slaveG12 = muSlaveBar( slaveips ) &
      & * OUTERPROD( slaveC1( :, slaveips ), eye( nsd ) ) &
      & + muSlaveBar( slaveips ) &
      & * OUTERPROD( slaveElemSD%dNdXt( :, :, slaveips ), &
      & slaveElemSD%normal( 1:nsd, slaveips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( masterG12( :, :, ii ), &
          & TRANSPOSE( slaveG12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, slaveC1, masterG12, muMasterBar, &
    & muSlaveBar, slaveG12 )
  !!
END PROCEDURE FacetMatrix1_5


END SUBMODULE FacetMatrix1Methods