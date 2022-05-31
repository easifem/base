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

SUBMODULE(Utility) QuadrangleInterpolationPoints
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle
  !!
  SELECT CASE( ipType )
  !!
  CASE( Equidistance )
    !!
    nodecoord = EquidistanceLIP_Quadrangle( xij=xij, order=order )
    !!
  CASE( GaussLegendre )
  CASE( GaussLobatto )
  CASE( Chebyshev )
  !!
  END SELECT
  !!
END PROCEDURE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!                                                EquidistanceLIP_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceLIP_Quadrangle
  REAL( DFP ) :: xij0( 3, 4 )
  !!
  IF( PRESENT( xij ) ) THEN
    xij0 = xij
  ELSE
    xij0 = RESHAPE( &
      & [-1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 1.0, 1.0, 0.0, -1.0, 1.0, 0.0],  &
      & [3,4] )
  END IF
  nodecoord( 1:3, 1:4 ) = xij0( 1:3, 1:4 )
  !!
  SELECT CASE( order )
    !!
  CASE( 2 )
    !!
    nodecoord( 1:3, 5 ) = 0.5_DFP * (xij0( 1:3, 1 ) + xij0( 1:3, 2 ))
    nodecoord( 1:3, 6 ) = 0.5_DFP * (xij0( 1:3, 2 ) + xij0( 1:3, 3 ))
    nodecoord( 1:3, 7 ) = 0.5_DFP * (xij0( 1:3, 3 ) + xij0( 1:3, 4 ))
    nodecoord( 1:3, 8 ) = 0.5_DFP * (xij0( 1:3, 4 ) + xij0( 1:3, 1 ))
    nodecoord( 1:3, 9 ) = 0.5_DFP * (nodecoord(1:3, 6) + nodecoord( 1:3, 8 ))
    !!
  CASE( 3 )
    !!
    nodecoord( 1:3, 5 ) = N1(-0.5_DFP) * xij0( 1:3, 1 ) + N2(-0.5_DFP) * &
      & xij0( 1:3, 2 )
    !!
    nodecoord( 1:3, 6 ) = N1(0.5_DFP) * xij0( 1:3, 1 ) + N2(0.5_DFP) * &
      & xij0( 1:3, 2 )
    !!
    nodecoord( 1:3, 7 ) = N1(-0.5_DFP) * xij0( 1:3, 2 ) + N2(-0.5_DFP) * &
      & xij0( 1:3, 3 )
    !!
    nodecoord( 1:3, 8 ) = N1(0.5_DFP) * xij0( 1:3, 2 ) + N2(0.5_DFP) * &
      & xij0( 1:3, 3 )
    !!
    nodecoord( 1:3, 10 ) = N1(-0.5_DFP) * xij0( 1:3, 4 ) + N2(-0.5_DFP) * &
      & xij0( 1:3, 4 )
    !!
    nodecoord( 1:3, 9 ) = N1(0.5_DFP) * xij0( 1:3, 4 ) + N2(0.5_DFP) * &
      & xij0( 1:3, 4 )
    !!
    nodecoord( 1:3, 12 ) = N1(-0.5_DFP) * xij0( 1:3, 1 ) + N2(-0.5_DFP) * &
      & xij0( 1:3, 4 )
    !!
    nodecoord( 1:3, 11 ) = N1(0.5_DFP) * xij0( 1:3, 1 ) + N2(0.5_DFP) * &
      & xij0( 1:3, 4 )
    !!
  END SELECT
  !!
  !!
  !!
  CONTAINS
  !!
  PURE REAL( DFP ) FUNCTION N1( x )
    REAL( DFP ), INTENT( IN ) ::  x
    N1 = 0.5_DFP * ( 1.0_DFP - x )
  END FUNCTION
  !!
  PURE REAL( DFP ) FUNCTION N2( x )
    REAL( DFP ), INTENT( IN ) ::  x
    N2 = 0.5_DFP * ( 1.0_DFP + x )
  END FUNCTION
  !!
END PROCEDURE EquidistanceLIP_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE QuadrangleInterpolationPoints