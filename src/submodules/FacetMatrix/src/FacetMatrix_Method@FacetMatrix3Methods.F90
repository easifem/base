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
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix3_1_opt1( elemsd=elemsd, ans=ans )
  CASE( 2 )
    CALL FacetMatrix3_1_opt2( elemsd=elemsd, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix3_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_2
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix3_2_opt1( elemsd=elemsd, mu=mu, ans=ans )
  CASE( 2 )
    CALL FacetMatrix3_2_opt2( elemsd=elemsd, mu=mu, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix3_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_3
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix3_3_opt1( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  CASE( 2 )
    CALL FacetMatrix3_3_opt2( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix3_3


!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_4
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix3_4_opt1( elemsd=elemsd, mu=mu, ans=ans )
  CASE( 2 )
    CALL FacetMatrix3_4_opt2( elemsd=elemsd, mu=mu, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix3_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix3_5
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix3_5_opt1( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  CASE( 2 )
    CALL FacetMatrix3_5_opt2( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix3_5

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_1_opt1( elemsd, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
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
  CALL Reallocate(m4, nns, nns, nsd, 1)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END SUBROUTINE FacetMatrix3_1_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_1_opt2( elemsd, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
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
  CALL Reallocate(m4, nns, nns, 1, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END SUBROUTINE FacetMatrix3_1_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_2_opt1( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, 1)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END SUBROUTINE FacetMatrix3_2_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_2_opt2( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, 1, nsd)
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END SUBROUTINE FacetMatrix3_2_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_3_opt1( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, 1)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu* taubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, taubar )
  !!
END SUBROUTINE FacetMatrix3_3_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_3_opt2( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), tauBar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, 1, nsd)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu* taubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12 )
  !!
END SUBROUTINE FacetMatrix3_3_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_4_opt1( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), mubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, 1)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, mubar )
  !!
END SUBROUTINE FacetMatrix3_4_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_4_opt2( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), mubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, 1, nsd)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, mubar )
  !!
END SUBROUTINE FacetMatrix3_4_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_5_opt1( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), mubar( : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, nsd, 1)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar * taubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, mubar, taubar )
  !!
END SUBROUTINE FacetMatrix3_5_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix3_5_opt2( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & G12( :, :, : ), m4( :, :, :, : ), mubar( : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns, nsd
  !!
  nns = SIZE( elemsd%N, 1 )
  nips = SIZE( elemsd%N, 2 )
  nsd = elemsd%refelem%nsd
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(G12, nns, nsd, nsd)
  CALL Reallocate(m4, nns, nns, 1, nsd)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar * taubar
  !!
  DO ips = 1, nips
    !!
    G12 = OUTERPROD( masterC1( :, ips ), eye( nsd, 1.0_DFP ) ) &
      & + OUTERPROD( elemsd%dNdXt( :, :, ips ),  &
      & elemsd%normal( 1:nsd, ips ) )
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
          & MATMUL( &
          & G12( :, :, ii ), elemsd%normal( 1:nsd, ips ) ), &
          & elemsd%N( :, ii ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, G12, mubar, taubar )
  !!
END SUBROUTINE FacetMatrix3_5_opt2

END SUBMODULE FacetMatrix3Methods