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

SUBMODULE(FacetMatrix_Method) FacetMatrix12Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix12_1
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), C1( :, : )
  INTEGER( I4B ) :: ips, nips, nns, nsd
  !!
  nns = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  CALL Reallocate(ans, nns, nns)
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=C1, &
    & val=elemsd%normal )
  realval = elemsd%js * elemsd%ws * elemsd%thickness
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips )*OUTERPROD( C1( :, ips ), C1( :, ips ) )
  END DO
  IF( PRESENT( nCopy ) ) CALL MakeDiagonalCopies(ans, nCopy)
  DEALLOCATE( realval, C1 )
  !!
END PROCEDURE FacetMatrix12_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix12_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), C1( :, : )
  INTEGER( I4B ) :: ips, nips, nns, nsd
  !!
  nns = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  CALL Reallocate(ans, nns, nns)
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=C1, &
    & val=elemsd%normal )
  realval = elemsd%js * elemsd%ws * elemsd%thickness * mu * mu
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips )*OUTERPROD( C1( :, ips ), C1( :, ips ) )
  END DO
  IF( PRESENT( nCopy ) ) CALL MakeDiagonalCopies(ans, nCopy)
  DEALLOCATE( realval, C1 )
  !!
END PROCEDURE FacetMatrix12_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix12_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), C1( :, : ), taubar( : )
  INTEGER( I4B ) :: ips, nips, nns, nsd
  !!
  nns = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  CALL Reallocate(ans, nns, nns)
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=C1, &
    & val=elemsd%normal )
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  realval = elemsd%js * elemsd%ws * elemsd%thickness * taubar * mu * mu
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips )*OUTERPROD( C1( :, ips ), C1( :, ips ) )
  END DO
  IF( PRESENT( nCopy ) ) CALL MakeDiagonalCopies(ans, nCopy)
  DEALLOCATE( realval, C1, taubar )
  !!
END PROCEDURE FacetMatrix12_3

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix12_4
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), C1( :, : ), muBar( : )
  INTEGER( I4B ) :: ips, nips, nns, nsd
  !!
  nns = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  CALL Reallocate(ans, nns, nns)
  CALL getProjectionOfdNdXt(obj=elemsd, cdNdXt=C1, val=elemsd%normal )
  CALL getInterpolation( obj=elemsd, interpol=muBar, val=mu )
  realval = elemsd%js * elemsd%ws * elemsd%thickness * muBar * muBar
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips )*OUTERPROD( C1( :, ips ), C1( :, ips ) )
  END DO
  IF( PRESENT( nCopy ) ) CALL MakeDiagonalCopies(ans, nCopy)
  DEALLOCATE( realval, C1, muBar )
  !!
END PROCEDURE FacetMatrix12_4

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix12_5
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), C1( :, : ), &
    & muBar( : ), tauBar( : )
  INTEGER( I4B ) :: ips, ii, jj, nips, nns, nsd
  !!
  nns = SIZE( elemsd%dNdXt, 1 )
  nsd = SIZE( elemsd%dNdXt, 2 )
  nips = SIZE( elemsd%dNdXt, 3 )
  CALL Reallocate(ans, nns, nns)
  CALL getProjectionOfdNdXt(obj=elemsd, cdNdXt=C1, val=elemsd%normal )
  CALL getInterpolation( obj=elemsd, interpol=muBar, val=mu )
  CALL getInterpolation( obj=elemsd, interpol=tauBar, val=tauvar )
  realval = elemsd%js * elemsd%ws * elemsd%thickness * tauBar * muBar * muBar
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips )*OUTERPROD( C1( :, ips ), C1( :, ips ) )
  END DO
  IF( PRESENT( nCopy ) ) CALL MakeDiagonalCopies(ans, nCopy)
  DEALLOCATE( realval, C1, muBar )
  !!
END PROCEDURE FacetMatrix12_5

END SUBMODULE FacetMatrix12Methods