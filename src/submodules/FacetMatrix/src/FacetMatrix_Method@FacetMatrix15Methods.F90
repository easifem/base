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

SUBMODULE(FacetMatrix_Method) FacetMatrix15Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix15
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix15_1
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), C1( :, : ), C2( :, :, : ), m4( :, :, :, : ), &
    & C( : )
  INTEGER( I4B ) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
    & slaveips
  !!
  nns1 = SIZE( masterElemSD%dNdXt, 1 )
  nns2 = SIZE( slaveElemSD%dNdXt, 1 )
  nns = nns1 + nns2
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
  IF( opt .EQ. 1 ) THEN
    !!
    nsd1 = nsd
    nsd2 = 1
    !!
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate( C1, nns, nips )
  CALL Reallocate( C2, nns, nsd, nips )
  CALL Reallocate(m4, nns, nns, nsd1, nsd2)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemsd, &
    & cdNdXt=masterC1, &
    & val=masterElemsd%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemsd, &
    & cdNdXt=slaveC1, &
    & val=slaveElemsd%normal )
  !!
  DO ips = 1, nips
    slaveips = quadMap( ips )
    CALL APPEND( C, masterC1(:, ips), slaveC1(:, slaveips ) )
    C1( :, ips ) = C
    !!
    C2( :, :, ips ) = masterElemSD%dNdXt(:, :, ips ) &
      & .rowConcat. slaveElemSD%dNdXt(:, :, slaveips)
    C2( :, :, ips ) = 0.5_DFP * C2( :, :, ips )
  END DO
  !!
  !!
  DO ips = 1, nips
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( C1(:,ips), C2(:, ii, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( realval, masterC1, slaveC1, C1, C2, m4, C )
  !!
END PROCEDURE FacetMatrix15_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix15
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix15_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), C1( :, : ), C2( :, :, : ), m4( :, :, :, : ), C(:)
  INTEGER( I4B ) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
    & slaveips
  !!
  nns1 = SIZE( masterElemSD%dNdXt, 1 )
  nns2 = SIZE( slaveElemSD%dNdXt, 1 )
  nns = nns1 + nns2
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
  IF( opt .EQ. 1 ) THEN
    !!
    nsd1 = nsd
    nsd2 = 1
    !!
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate( C1, nns, nips )
  CALL Reallocate( C2, nns, nsd, nips )
  CALL Reallocate(m4, nns, nns, nsd1, nsd2)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemsd, &
    & cdNdXt=masterC1, &
    & val=masterElemsd%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemsd, &
    & cdNdXt=slaveC1, &
    & val=slaveElemsd%normal )
  !!
  masterC1 = muMaster * masterC1
  slaveC1 = muSlave * slaveC1
  !!
  DO ips = 1, nips
    slaveips = quadMap( ips )
    CALL APPEND( C, masterC1(:, ips), slaveC1(:, slaveips ) )
    C1( :, ips ) = C
    !!
    C2( :, :, ips ) = masterElemSD%dNdXt(:, :, ips ) &
      & .rowConcat. slaveElemSD%dNdXt(:, :, slaveips)
    C2( :, :, ips ) = 0.5_DFP * C2( :, :, ips )
  END DO
  !!
  DO ips = 1, nips
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( C1(:,ips), C2(:, ii, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( realval, masterC1, slaveC1, C1, C2, m4, C )
  !!
END PROCEDURE FacetMatrix15_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix15
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix15_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), C1( :, : ), C2( :, :, : ), m4( :, :, :, : ), C( : )
  INTEGER( I4B ) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
    & slaveips
  !!
  nns1 = SIZE( masterElemSD%dNdXt, 1 )
  nns2 = SIZE( slaveElemSD%dNdXt, 1 )
  nns = nns1 + nns2
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
  IF( opt .EQ. 1 ) THEN
    !!
    nsd1 = nsd
    nsd2 = 1
    !!
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate( C1, nns1+nns2, nips )
  CALL Reallocate( C2, nns, nsd, nips )
  CALL Reallocate(m4, nns, nns, nsd1, nsd2)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemsd, &
    & cdNdXt=masterC1, &
    & val=masterElemsd%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemsd, &
    & cdNdXt=slaveC1, &
    & val=slaveElemsd%normal )
  !!
  masterC1 = muMaster * masterC1
  slaveC1 = muSlave * slaveC1
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    CALL APPEND( C, masterC1(:, ips), slaveC1(:, slaveips ) )
    C1( :, ips ) = C
    !!
    C2( :, :, ips ) = (tauMaster * 0.5_DFP * masterElemSD%dNdXt(:, :, ips )) &
      & .rowConcat. (tauSlave * 0.5_DFP * slaveElemSD%dNdXt( :, :, slaveips ))
    !!
  END DO
  !!
  DO ips = 1, nips
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( C1(:,ips), C2(:, ii, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( realval, masterC1, slaveC1, C1, C2, m4, C )
  !!
END PROCEDURE FacetMatrix15_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix15
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix15_4
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), C1( :, : ), C2( :, :, : ), m4( :, :, :, : ), &
    & muMasterBar( : ), muSlaveBar( : ), C(:)
  INTEGER( I4B ) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
    & slaveips
  !!
  nns1 = SIZE( masterElemSD%dNdXt, 1 )
  nns2 = SIZE( slaveElemSD%dNdXt, 1 )
  nns = nns1 + nns2
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
  !!
  IF( opt .EQ. 1 ) THEN
    !!
    nsd1 = nsd
    nsd2 = 1
    !!
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate( C1, nns1+nns2, nips )
  CALL Reallocate( C2, nns, nsd, nips )
  CALL Reallocate(m4, nns, nns, nsd1, nsd2)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemsd, &
    & cdNdXt=masterC1, &
    & val=masterElemsd%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemsd, &
    & cdNdXt=slaveC1, &
    & val=slaveElemsd%normal )
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
  DO ips = 1, nips
    masterC1(:, ips) = muMasterBar( ips ) * masterC1( :, ips )
    slaveC1(:, ips) = muSlaveBar( ips ) * slaveC1( :, ips )
  END DO
  !!
  DO ips = 1, nips
    slaveips = quadMap( ips )
    CALL APPEND( C, masterC1(:, ips), slaveC1(:, slaveips ) )
    C1( :, ips ) = C
    !!
    C2( :, :, ips ) = masterElemSD%dNdXt(:, :, ips ) &
      & .rowConcat. slaveElemSD%dNdXt(:, :, slaveips)
    C2( :, :, ips ) = 0.5_DFP * C2( :, :, ips )
  END DO
  !!
  DO ips = 1, nips
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( C1(:,ips), C2(:, ii, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( realval, masterC1, slaveC1, C1, C2, m4, muMasterBar, &
    & muSlaveBar, C )
  !!
END PROCEDURE FacetMatrix15_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix15
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix15_5
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), C1( :, : ), C2( :, :, : ), m4( :, :, :, : ), &
    & tauMasterBar( : ), tauSlaveBar( : ), C( : )
  INTEGER( I4B ) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
    & slaveips
  !!
  nns1 = SIZE( masterElemSD%dNdXt, 1 )
  nns2 = SIZE( slaveElemSD%dNdXt, 1 )
  nns = nns1 + nns2
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
  IF( opt .EQ. 1 ) THEN
    !!
    nsd1 = nsd
    nsd2 = 1
    !!
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate( C1, nns1+nns2, nips )
  CALL Reallocate( C2, nns, nsd, nips )
  CALL Reallocate(m4, nns, nns, nsd1, nsd2)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemsd, &
    & cdNdXt=masterC1, &
    & val=masterElemsd%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemsd, &
    & cdNdXt=slaveC1, &
    & val=slaveElemsd%normal )
  !!
  CALL getInterpolation( &
    & obj=masterElemSD, &
    & interpol=tauMasterBar, &
    & val=tauMaster )
  !!
  CALL getInterpolation( &
    & obj=slaveElemSD, &
    & interpol=tauSlaveBar, &
    & val=tauSlave )
  !!
  masterC1 = muMaster * masterC1
  slaveC1 = muSlave * slaveC1
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    CALL APPEND( C, masterC1(:, ips), slaveC1(:, slaveips ) )
    C1( :, ips ) = C
    !!
    C2( :, :, ips ) = (0.5*tauMasterBar(ips)*masterElemSD%dNdXt(:, :, ips )) &
      & .rowConcat. (0.5*tauSlaveBar(ips)*slaveElemSD%dNdXt(:, :, slaveips))
    !!
  END DO
  !!
  DO ips = 1, nips
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( C1(:,ips), C2(:, ii, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( realval, masterC1, slaveC1, C1, C2, m4, tauMasterBar, &
    & tauSlaveBar, C )
  !!
END PROCEDURE FacetMatrix15_5

!----------------------------------------------------------------------------
!                                                              FacetMatrix15
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix15_6
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), C1( :, : ), C2( :, :, : ), m4( :, :, :, : ), &
    & tauMasterBar( : ), tauSlaveBar( : ), muMasterBar( : ), &
    & muSlaveBar( : ), C( : )
  INTEGER( I4B ) :: ips, nips, nns1, nns2, nsd, nns, nsd1, nsd2, ii, jj, &
    & slaveips
  !!
  nns1 = SIZE( masterElemSD%dNdXt, 1 )
  nns2 = SIZE( slaveElemSD%dNdXt, 1 )
  nns = nns1 + nns2
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
  IF( opt .EQ. 1 ) THEN
    !!
    nsd1 = nsd
    nsd2 = 1
    !!
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate( C1, nns1+nns2, nips )
  CALL Reallocate( C2, nns, nsd, nips )
  CALL Reallocate(m4, nns, nns, nsd1, nsd2)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=masterElemsd, &
    & cdNdXt=masterC1, &
    & val=masterElemsd%normal )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=slaveElemsd, &
    & cdNdXt=slaveC1, &
    & val=slaveElemsd%normal )
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
    & interpol=tauMasterBar, &
    & val=tauMaster )
  !!
  CALL getInterpolation( &
    & obj=slaveElemSD, &
    & interpol=tauSlaveBar, &
    & val=tauSlave )
  !!
  DO ips = 1, nips
    masterC1(:, ips) = muMasterBar( ips ) * masterC1( :, ips )
    slaveC1(:, ips) = muSlaveBar( ips ) * slaveC1( :, ips )
  END DO
  !!
  !!
  DO ips = 1, nips
    slaveips = quadMap( ips )
    CALL APPEND( C, masterC1(:, ips), slaveC1(:, slaveips ) )
    C1( :, ips ) = C
    !!
    C2( :, :, ips ) = (0.5*tauMasterBar(ips)*masterElemSD%dNdXt(:, :, ips )) &
      & .rowConcat. (0.5*tauSlaveBar(ips)*slaveElemSD%dNdXt(:, :, slaveips))
    !!
  END DO
  !!
  DO ips = 1, nips
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( C1(:,ips), C2(:, ii, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( realval, masterC1, slaveC1, C1, C2, m4, tauMasterBar, &
    & tauSlaveBar, muMasterBar, muSlaveBar, C )
  !!
END PROCEDURE FacetMatrix15_6

END SUBMODULE FacetMatrix15Methods