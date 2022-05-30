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

SUBMODULE(FacetMatrix_Method) FacetMatrix2Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix2_1
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  realval = elemsd%js * elemsd%ws * elemsd%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( G12( :, :, ii ), &
          & TRANSPOSE( G12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END PROCEDURE FacetMatrix2_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix2_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu * mu
  !!
  CALL getProjectionOfdNdXt(obj=elemsd, cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( G12( :, :, ii ), &
          & TRANSPOSE( G12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END PROCEDURE FacetMatrix2_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix2_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt(obj=elemsd, cdNdXt=masterC1, &
    & val=elemsd%normal )
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * taubar * mu * mu
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( G12( :, :, ii ), &
          & TRANSPOSE( G12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, taubar )
  !!
END PROCEDURE FacetMatrix2_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix2_4
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), muBar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt(obj=elemsd, cdNdXt=masterC1, val=elemsd%normal )
  CALL getInterpolation( obj=elemsd, interpol=muBar, val=mu )
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * muBar * muBar
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( G12( :, :, ii ), &
          & TRANSPOSE( G12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, muBar )
  !!
END PROCEDURE FacetMatrix2_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix2_5
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), muBar( : ), &
    & tauBar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt(obj=elemsd, cdNdXt=masterC1, val=elemsd%normal )
  CALL getInterpolation( obj=elemsd, interpol=muBar, val=mu )
  CALL getInterpolation( obj=elemsd, interpol=tauBar, val=tauvar )
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * tauBar * muBar * muBar
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd
      !!
      DO ii = 1, nsd
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * MATMUL( G12( :, :, ii ), &
          & TRANSPOSE( G12( :, :, jj ) ) )
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, muBar, tauBar )
  !!
END PROCEDURE FacetMatrix2_5

END SUBMODULE FacetMatrix2Methods