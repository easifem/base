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

SUBMODULE(Utility) LineInterpolationPoints
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line
  SELECT CASE( interpolType )
  CASE( "LagrangeInterpolation", "Equidistance" )
    nodecoord = EquidistanceLIP_Line( xij=xij, order=order )
  END SELECT
END PROCEDURE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                       EquidistanceLIP_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceLIP_Line
  !!
  !! Define internal variables
  !!
  INTEGER( I4B ) :: i,j
  REAL( DFP ) :: xi( order + 1 )
  !!
  !!
  !!
  SELECT CASE( order )
  !!
  CASE( 1 )
    !!
    xi = [-1.0_DFP, 1.0_DFP]
    !!
  CASE( 2 )
    !!
    xi = [-1.0_DFP, 1.0_DFP, 0.0_DFP]
    !!
  CASE( 3 )
    !!
    xi = [ -1.0_DFP, 1.0_DFP, -0.33333333333333333333_DFP, &
      & 0.33333333333333333333_DFP]
    !!
  CASE( 4 )
    !!
    xi = [ -1.0_DFP, 1.0_DFP, -0.5_DFP, 0.0_DFP, 0.5_DFP ]
    !!
  CASE( 5 )
    !!
    xi = [ -1.0, 1.0, -0.6, -0.2, 0.2, 0.6 ]
    !!
  CASE( 6 )
    !!
    xi = [ &
      & -1.0_DFP, 1.0_DFP, -0.66666666666666666667_DFP, &
      & -0.33333333333333333333_DFP, 0.0_DFP, 0.33333333333333333333_DFP, &
      & 0.66666666666666666667_DFP ]
    !!
  CASE( 7 )
    !!
    xi = [ -1.0_DFP, 1.0_DFP, -0.71428571428571428571_DFP, &
      & -0.42857142857142857143_DFP, &
      & -0.14285714285714285714_DFP, &
      & 0.14285714285714285714_DFP, &
      & 0.42857142857142857143_DFP, &
      & 0.71428571428571428571_DFP ]
    !!
  CASE( 8 )
    !!
    xi = [-1.0_DFP, 1.0_DFP, -0.75_DFP, -0.5_DFP, &
      & -0.25_DFP, 0.0_DFP, 0.25_DFP, 0.5_DFP, 0.75_DFP]
    !!
  CASE( 9 )
    !!
    xi = [-1.0_DFP, 1.0_DFP, &
      & -0.77777777777777777778_DFP, &
      & -0.55555555555555555556_DFP, &
      & -0.33333333333333333333_DFP, &
      & -0.11111111111111111111_DFP, &
      & 0.11111111111111111111_DFP, &
      & 0.33333333333333333333_DFP, &
      & 0.55555555555555555556_DFP, &
      & 0.77777777777777777778_DFP ]
    !!
  CASE( 10 )
    !!
    xi = [ -1.0_DFP, &
      & 1.0_DFP, &
      & -0.8_DFP, &
      & -0.6_DFP, &
      & -0.4_DFP, &
      & -0.2_DFP, &
      & 0.0_DFP, &
      & 0.2_DFP, &
      & 0.4_DFP, &
      & 0.6_DFP, &
      & 0.8_DFP ]
    !!
  CASE DEFAULT
    !!
    xi( 1 ) = -1.0_DFP
    xi( 2 ) = 1.0_DFP
    DO CONCURRENT (i = 3:order + 1)
      xi( i ) = ( 2.0_DFP * REAL( i, DFP ) &
        & - REAL( order, DFP )  - 4.0_DFP ) / ( REAL( order, DFP )  )
    END DO
    !!
  END SELECT
  !!
  DO CONCURRENT( i=1:3, j=1:order+1)
    !!
    nodecoord( i, j ) = 0.5_DFP * ( xij( i, 1 ) + xij( i, 2 ) ) &
      & +  0.5_DFP * ( xij( i, 2 ) - xij( i, 1 ) ) * xi( j )
    !!
  END DO
  !!
END PROCEDURE EquidistanceLIP_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LineInterpolationPoints