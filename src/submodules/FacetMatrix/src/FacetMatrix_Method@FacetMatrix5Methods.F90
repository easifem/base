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
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), slaveC1( :, : )
  INTEGER( I4B ) :: ips, nips, nns, nsd, slaveips
  !!
  nns = SIZE( masterElemSD%dNdXt, 1 )
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
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
  CALL Reallocate(ans, nns, nns)
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    !!
    ans = ans &
      & + realval( ips )*OUTERPROD( &
        & masterC1( :, ips ), &
        & slaveC1( :, slaveips ) )
    !!
  END DO
  !!
  CALL MakeDiagonalCopies(ans, nsd)
  !!
  DEALLOCATE( realval, masterC1, slaveC1 )
  !!
END PROCEDURE FacetMatrix5_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), slaveC1(:,:)
  INTEGER( I4B ) :: ips, slaveips, nips, nns, nsd
  !!
  nns = SIZE( masterElemSD%dNdXt, 1 )
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
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
  CALL Reallocate(ans, nns, nns)
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    !!
    ans = ans &
      & + realval( ips )*OUTERPROD( &
        & muMaster * masterC1( :, ips ), &
        & muSlave * slaveC1( :, slaveips ) )
    !!
  END DO
  !!
  CALL MakeDiagonalCopies(ans, nsd)
  !!
  DEALLOCATE( realval, masterC1, slaveC1 )
  !!
END PROCEDURE FacetMatrix5_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & slaveC1( :, : ), taubar( : )
  INTEGER( I4B ) :: ips, slaveips, nips, nns, nsd
  !!
  nns = SIZE( masterElemSD%dNdXt, 1 )
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
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
  CALL getInterpolation(obj=masterElemSD, Interpol=taubar, val=tauvar)
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness * &
    & taubar
  !!
  CALL Reallocate(ans, nns, nns)
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    !!
    ans = ans &
      & + realval( ips )*OUTERPROD( &
        & muMaster * masterC1( :, ips ), &
        & muSlave * slaveC1( :, slaveips ) )
    !!
  END DO
  !!
  CALL MakeDiagonalCopies(ans, nsd)
  !!
  DEALLOCATE( realval, masterC1, slaveC1, taubar )
  !!
END PROCEDURE FacetMatrix5_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_4
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & muMasterBar( : ), muSlaveBar( : ), slaveC1( :, : )
  INTEGER( I4B ) :: ips, slaveips, nips, nns, nsd
  !!
  nns = SIZE( masterElemSD%dNdXt, 1 )
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
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
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness
  !!
  CALL Reallocate(ans, nns, nns)
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    !!
    ans = ans &
      & + realval( ips )*OUTERPROD( &
        & muMasterBar( ips ) * masterC1( :, ips ), &
        & muSlaveBar( slaveips ) * slaveC1( :, slaveips ) )
    !!
  END DO
  !!
  CALL MakeDiagonalCopies(ans, nsd)
  !!
  DEALLOCATE( realval, masterC1, slaveC1, muMasterBar, muSlaveBar )
  !!
END PROCEDURE FacetMatrix5_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix5
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix5_5
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & muMasterBar( : ), muSlaveBar( : ), tauBar( : ), slaveC1( :, : )
  INTEGER( I4B ) :: ips, slaveips, nips, nns, nsd
  !!
  nns = SIZE( masterElemSD%dNdXt, 1 )
  nips = SIZE( masterElemSD%dNdXt, 3 )
  nsd = masterElemSD%refelem%nsd
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
    & interpol=tauBar, &
    & val=tauvar )
  !!
  realval = masterElemSD%js * masterElemSD%ws * masterElemSD%thickness * &
    & tauBar
  !!
  CALL Reallocate(ans, nns, nns)
  !!
  DO ips = 1, nips
    !!
    slaveips = quadMap( ips )
    !!
    ans = ans &
      & + realval( ips )*OUTERPROD( &
        & muMasterBar( ips ) * masterC1( :, ips ), &
        & muSlaveBar( slaveips ) * slaveC1( :, slaveips ) )
    !!
  END DO
  !!
  CALL MakeDiagonalCopies(ans, nsd)
  !!
  DEALLOCATE( realval, masterC1, slaveC1, muMasterBar, &
    & muSlaveBar )
  !!
END PROCEDURE FacetMatrix5_5

END SUBMODULE FacetMatrix5Methods