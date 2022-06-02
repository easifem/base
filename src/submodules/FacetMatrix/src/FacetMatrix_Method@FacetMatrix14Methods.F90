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
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix14_1_opt1( elemsd=elemsd, ans=ans )
  CASE( 2 )
    CALL FacetMatrix14_1_opt2( elemsd=elemsd, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix14_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_2
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix14_2_opt1( elemsd=elemsd, mu=mu, ans=ans )
  CASE( 2 )
    CALL FacetMatrix14_2_opt2( elemsd=elemsd, mu=mu, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix14_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_3
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix14_3_opt1( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  CASE( 2 )
    CALL FacetMatrix14_3_opt2( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix14_3


!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_4
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix14_4_opt1( elemsd=elemsd, mu=mu, ans=ans )
  CASE( 2 )
    CALL FacetMatrix14_4_opt2( elemsd=elemsd, mu=mu, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix14_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix14_5
  !!
  SELECT CASE( opt )
  CASE( 1 )
    CALL FacetMatrix14_5_opt1( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  CASE( 2 )
    CALL FacetMatrix14_5_opt2( elemsd=elemsd, mu=mu, tauvar=tauvar, ans=ans )
  END SELECT
  !!
END PROCEDURE FacetMatrix14_5

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_1_opt1( elemsd, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ),  &
    & m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  realval = elemsd%js * elemsd%ws * elemsd%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, nsd, 1)
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1 )
  !!
END SUBROUTINE FacetMatrix14_1_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_1_opt2( elemsd, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  realval = elemsd%js * elemsd%ws * elemsd%thickness
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, 1, nsd)
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1 )
  !!
END SUBROUTINE FacetMatrix14_1_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_2_opt1( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, nsd, 1)
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1 )
  !!
END SUBROUTINE FacetMatrix14_2_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_2_opt2( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, 1, nsd)
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1 )
  !!
END SUBROUTINE FacetMatrix14_2_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_3_opt1( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, nsd, 1)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu* taubar
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, taubar )
  !!
END SUBROUTINE FacetMatrix14_3_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_3_opt2( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, 1, nsd)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu* taubar
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, taubar )
  !!
END SUBROUTINE FacetMatrix14_3_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_4_opt1( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : ), mubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, nsd, 1)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, mubar )
  !!
END SUBROUTINE FacetMatrix14_4_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_4_opt2( elemsd, mu, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : ), mubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, 1, nsd)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, mubar )
  !!
END SUBROUTINE FacetMatrix14_4_opt2

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_5_opt1( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : ), mubar( : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, nsd, 1)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar * taubar
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, ii, 1 ) = m4( :, :, ii, 1 ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, mubar, taubar )
  !!
END SUBROUTINE FacetMatrix14_5_opt1

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

PURE SUBROUTINE FacetMatrix14_5_opt2( elemsd, mu, tauvar, ans )
  !!
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ans( :, : )
  !!
  !!
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), &
    & m4( :, :, :, : ), mubar( : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd
  !!
  nns1 = SIZE( elemsd%dNdXt, 1 )
  nns2 = SIZE( elemsd%N, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
  & obj=elemsd, &
  & cdNdXt=masterC1, &
  & val=elemsd%normal )
  !!
  CALL Reallocate(m4, nns2, nns1, 1, nsd )
  !!
  CALL getInterpolation(obj=elemsd, Interpol=mubar, val=mu)
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mubar * taubar
  !!
  DO ips = 1, nips
    !!
    DO ii = 1, nsd
      !!
      m4( :, :, 1, ii ) = m4( :, :, 1, ii ) &
        & + realval( ips ) * OUTERPROD( &
        & elemsd%N( :, ips ), &
        & masterC1( :, ips ) * elemsd%normal( ii, ips ) )
      !!
    END DO
    !!
  END DO
  !!
  CALL Convert( from=m4, to=ans )
  !!
  DEALLOCATE( m4, realval, masterC1, mubar, taubar )
  !!
END SUBROUTINE FacetMatrix14_5_opt2

END SUBMODULE FacetMatrix14Methods