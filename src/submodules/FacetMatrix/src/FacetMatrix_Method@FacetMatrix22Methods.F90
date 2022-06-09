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

SUBMODULE(FacetMatrix_Method) FacetMatrix22Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              FacetMatrix22
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix22_1
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : )
  INTEGER( I4B ) :: ips, nips, nns2, nns1
  !!
  nns1 = SIZE( elemsd%N, 1 )
  nns2 = SIZE( elemsd%dNdXt, 1 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  ALLOCATE( ans( nns2, nns1 ) )
  ans = 0.0_DFP
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness
  !!
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips ) * OUTERPROD( &
      & masterC1( :, ips ), &
      & elemsd%N( :, ips ))
  END DO
  !!
  DEALLOCATE( realval, masterC1 )
  !!
END PROCEDURE FacetMatrix22_1

!----------------------------------------------------------------------------
!                                                              FacetMatrix22
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix22_2
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : )
  INTEGER( I4B ) :: ips, nips, nns2, nns1
  !!
  nns1 = SIZE( elemsd%N, 1 )
  nns2 = SIZE( elemsd%dNdXt, 1 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  ALLOCATE( ans( nns2, nns1 ) )
  ans = 0.0_DFP
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * tauvar
  !!
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips ) * OUTERPROD( &
      & masterC1( :, ips ), &
      & elemsd%N( :, ips ))
  END DO
  !!
  DEALLOCATE( realval, masterC1 )
  !!
END PROCEDURE FacetMatrix22_2

!----------------------------------------------------------------------------
!                                                              FacetMatrix22
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetMatrix22_3
  !!
  REAL( DFP ), ALLOCATABLE :: realval( : ), masterC1( :, : ), taubar( : )
  INTEGER( I4B ) :: ips, ii, nips, nns2, nns1, nsd, nsd1, nsd2, jj
  !!
  nns1 = SIZE( elemsd%N, 1 )
  nns2 = SIZE( elemsd%dNdXt, 1 )
  nips = SIZE( elemsd%dNdXt, 3 )
  !!
  ALLOCATE( ans( nns2, nns ) )
  ans = 0.0_DFP
  !!
  CALL getProjectionOfdNdXt( &
    & obj=elemsd, &
    & cdNdXt=masterC1, &
    & val=elemsd%normal )
  !!
  CALL getInterpolation(obj=elemsd, Interpol=taubar, val=tauvar)
  !!
  realval = elemsd%js * elemsd%ws * elemsd%thickness * taubar
  !!
  DO ips = 1, nips
    ans( :, : ) = ans( :, : ) &
      & + realval( ips ) * OUTERPROD( &
      & masterC1( :, ips ), &
      & elemsd%N( :, ips ))
  END DO
  !!
  DEALLOCATE( realval, masterC1, taubar )
  !!
END PROCEDURE FacetMatrix22_3

END SUBMODULE FacetMatrix22Methods