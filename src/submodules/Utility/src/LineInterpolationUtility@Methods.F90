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

SUBMODULE(LineInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Line
  INTEGER( I4B ) :: ii, n
  n = LagrangeDOF_Line( order=order )
  ALLOCATE( ans( n, 1 ) )
  DO ii = 1, n
    ans( ii, 1 ) = ii-1
  END DO
END PROCEDURE LagrangeDegree_Line

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Point
  ans = 1_I4B
END PROCEDURE LagrangeDOF_Point

!----------------------------------------------------------------------------
!                                                         LagrangeDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Line
  ans = order+1
END PROCEDURE LagrangeDOF_Line

!----------------------------------------------------------------------------
!                                                         LagrangeInDOF_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Line
  ans = order - 1_I4B
END PROCEDURE LagrangeInDOF_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line1
  INTEGER( I4B ) :: n, ii
  REAL( DFP ) :: avar
  !!
  IF( order .LE. 1_I4B ) THEN
    ALLOCATE( ans( 0 ) )
    RETURN
  END IF
  !!
  n = LagrangeInDOF_Line(order=order)
  ALLOCATE( ans( n ) )
  !!
  avar = (xij(2) - xij(1)) / order
  !!
  DO ii = 1, n
    ans( ii ) = xij(1) + ii*avar
  END DO
  !!
END PROCEDURE EquidistanceInPoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2
  INTEGER( I4B ) :: n, ii, nsd
  REAL( DFP ) :: x0( 3, 2 )
  REAL( DFP ) :: avar(3)
  !!
  IF( order .LE. 1_I4B ) THEN
    ALLOCATE( ans( 0, 0 ) )
    RETURN
  END IF
  !!
  IF( PRESENT( xij ) ) THEN
    nsd = SIZE( xij, 1 )
    x0( 1:nsd, 1 ) = xij( 1:nsd, 1 )
    x0( 1:nsd, 2 ) = xij( 1:nsd, 2 )
  ELSE
    nsd = 3_I4B
    x0( 1:nsd, 1 ) = [-1.0, 0.0, 0.0]
    x0( 1:nsd, 2 ) = [1.0, 0.0, 0.0]
  END IF
  !!
  n = LagrangeInDOF_Line(order=order)
  ALLOCATE( ans( nsd, n ) )
  !!
  avar(1:nsd) = (x0(1:nsd,2) - x0(1:nsd,1)) / order
  !!
  DO ii = 1, n
    ans( 1:nsd, ii ) = x0(1:nsd, 1) + ii*avar(1:nsd)
  END DO
  !!
END PROCEDURE EquidistanceInPoint_Line2

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line1
  CALL Reallocate( ans, order+1 )
  ans(1) = xij(1)
  ans(2) = xij(2)
  IF( order .GE. 2 ) THEN
    ans( 3: ) = EquidistanceInPoint_Line(order=order, xij=xij)
  END IF
END PROCEDURE EquidistancePoint_Line1

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2
  INTEGER( I4B ) :: nsd
  !!
  IF( PRESENT( xij ) ) THEN
    nsd = SIZE( xij, 1 )
    CALL Reallocate( ans, nsd, order+1 )
    ans( 1:nsd, 1 ) = xij( 1:nsd, 1 )
    ans( 1:nsd, 2 ) = xij( 1:nsd, 2 )
  ELSE
    nsd = 3_I4B
    CALL Reallocate( ans, nsd, order+1 )
    ans( 1:nsd, 1 ) = [-1.0, 0.0, 0.0]
    ans( 1:nsd, 2 ) = [1.0, 0.0, 0.0]
  END IF
  !!
  IF( order .GE. 2 ) THEN
    ans(1:nsd, 3:) = EquidistanceInPoint_Line(order=order, xij=xij)
  END IF
  !!
END PROCEDURE EquidistancePoint_Line2

!----------------------------------------------------------------------------
!                                                    InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line
  !!
  SELECT CASE( ipType )
  !!
  CASE( Equidistance )
    ans = EquidistancePoint_Line( xij=xij, order=order )
  CASE( GaussLegendre )
  CASE( GaussLobatto )
  CASE( Chebyshev )
  !!
  END SELECT
  !!
END PROCEDURE InterpolationPoint_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods