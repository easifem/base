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

SUBMODULE(FacetMatrix_Method) FacetMatrix3Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_1
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), i3( :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns1, nns2, nsd1, nsd2, nsd, jj
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  nns2 = SIZE( elemsd%N, 1 )
  i3 = Eye( nsd )
  !!
  IF( opt .EQ. 1 ) THEN
    nsd1 = nsd
    nsd2 = 1
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate(G12, nns1, nsd, nsd)
  CALL Reallocate(m4, nns1, nns2, nsd1, nsd2)
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), i3 ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii+jj-1 ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ips ) )
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, i3 )
  !!
END PROCEDURE FacetMatrix3_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), i3( :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns1, nns2, nsd, jj, nsd1, nsd2
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  nns2 = SIZE( elemsd%N, 1 )
  i3 = Eye( nsd )
  !!
  IF( opt .EQ. 1 ) THEN
    nsd1 = nsd
    nsd2 = 1
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate(G12, nns1, nsd, nsd)
  CALL Reallocate(m4, nns1, nns2, nsd1, nsd2)
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), i3 ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( &
            & MATMUL( &
            & G12( :, :, ii+jj-1 ), elemsd%normal( 1:nsd, ips ) ), &
            & elemsd%N( :, ips ) )
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END PROCEDURE FacetMatrix3_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), taubar( : ), i3( :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns1, nns2, nsd, jj, nsd1, nsd2
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  nns2 = SIZE( elemsd%N, 1 )
  i3 = Eye( nsd )
  !!
  IF( opt .EQ. 1 ) THEN
    nsd1 = nsd
    nsd2 = 1
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate(G12, nns1, nsd, nsd)
  CALL Reallocate(m4, nns1, nns2, nsd1, nsd2)
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu * taubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), i3 ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii+jj-1 ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ips ) )
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, taubar, i3 )
  !!
END PROCEDURE FacetMatrix3_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_4
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), mubar( : ), i3( :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns1, nns2, nsd, jj, nsd1, nsd2
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  nns2 = SIZE( elemsd%N, 1 )
  i3 = Eye( nsd )
  !!
  IF( opt .EQ. 1 ) THEN
    nsd1 = nsd
    nsd2 = 1
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate(G12, nns1, nsd, nsd)
  CALL Reallocate(m4, nns1, nns2, nsd1, nsd2)
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( &
            & MATMUL( &
            & G12( :, :, ii+jj-1 ), elemsd%normal( 1:nsd, ips ) ), &
            & elemsd%N( :, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, mubar, i3 )
  !!
END PROCEDURE FacetMatrix3_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_5
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), mubar( : ), taubar( : ), i3( :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns1, nns2, nsd, nsd1, nsd2, jj
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  nns2 = SIZE( elemsd%N, 1 )
  i3 = Eye( nsd )
  !!
  IF( opt .EQ. 1 ) THEN
    nsd1 = nsd
    nsd2 = 1
  ELSE
    nsd1 = 1
    nsd2 = nsd
  END IF
  !!
  CALL Reallocate(G12, nns1, nsd, nsd)
  CALL Reallocate(m4, nns1, nns2, nsd1, nsd2)
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar * taubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO jj = 1, nsd2
      DO ii = 1, nsd1
        !!
        m4( :, :, ii, jj ) = m4( :, :, ii, jj ) &
          & + realval( ips ) * OUTERPROD( &
            & MATMUL( &
            & G12( :, :, ii+jj-1 ), elemsd%normal( 1:nsd, ips ) ), &
            & elemsd%N( :, ips ) )
        !!
      END DO
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, mubar, taubar )
  !!
END PROCEDURE FacetMatrix3_5

END SUBMODULE FacetMatrix3Methods