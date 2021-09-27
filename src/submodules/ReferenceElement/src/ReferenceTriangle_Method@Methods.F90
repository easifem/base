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

!> authors: Vikas Sharma, Ph. D.
! date: 	2 March 2021
! summary: This submodule contains methods for [[ReferenceTriangle_]]

SUBMODULE( ReferenceTriangle_Method ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_ref_Triangle
  INTEGER( I4B ) :: s(2)
  IF( PRESENT( XiJ ) ) THEN
    CALL REALLOCATE( obj%XiJ, 3, 3 )
    s = SHAPE( XiJ )
    obj%XiJ = XiJ
  ELSE
    obj%XiJ =  RESHAPE( [0, 0, 0, 1, 0, 0, 0, 1, 0], [3, 3] )
  END IF
  obj%EntityCounts = [3, 3, 1, 0]
  obj%XiDimension = 2
  obj%Name = Triangle3
  obj%Order = 1
  obj%NSD = NSD
  ALLOCATE( obj%Topology( 7 ) )
  obj%Topology( 1 ) = ReferenceTopology( [1], Point )
  obj%Topology( 2 ) = ReferenceTopology( [2], Point )
  obj%Topology( 3 ) = ReferenceTopology( [3], Point )
  obj%Topology( 4 ) = ReferenceTopology( [1, 2], Line2 )
  obj%Topology( 5 ) = ReferenceTopology( [2, 3], Line2 )
  obj%Topology( 6 ) = ReferenceTopology( [3, 1], Line2 )
  obj%Topology( 7 ) = ReferenceTopology( [1, 2, 3], Triangle3 )
  obj%LagrangeElement => LagrangeElement_Triangle
END PROCEDURE initiate_ref_Triangle

!----------------------------------------------------------------------------
!                                                          ReferenceTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Triangle
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( obj, NSD, XiJ )
  ELSE
    CALL Initiate( obj, NSD )
  END IF
END PROCEDURE reference_Triangle

!----------------------------------------------------------------------------
!                                                          ReferenceTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_Triangle_Pointer
  ALLOCATE( obj )
  IF( PRESENT( XiJ ) ) THEN
    CALL Initiate( obj, NSD, XiJ )
  ELSE
    CALL Initiate( obj, NSD )
  END IF
END PROCEDURE reference_Triangle_Pointer

!----------------------------------------------------------------------------
!                                                            LagrangePoints
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: 	This subroutine generate Lagrange points on triangle
!
!### Introduction
! * Returns equidistant points on triangle for lagrange interpolation
!	* Nodecoord is a 2D array with 3 rows
!	* first row is xi, second row is eta, third row is zeta

PURE FUNCTION EquidistanceLIP_Triangle( XiJ, Order ) RESULT( NodeCoord )
  REAL( DFP ), INTENT( IN ) :: XiJ( 3, 3 )
  INTEGER( I4B ), INTENT( IN ) :: Order
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )

  ! Define internal variables
  INTEGER( I4B ) :: i
  REAL( DFP ) :: x( 3 ), y( 3 )
  REAL( DFP ), ALLOCATABLE :: Xi( : ), Eta( : )

  SELECT CASE( Order )
    CASE( 1 )
    ! order 1; Triangle3
    NodeCoord = RESHAPE( [ &
      & 0.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 0.0_DFP], [3, 3] )

    CASE( 2 )
    ! order 2, Triangle6
    NodeCoord = RESHAPE( [ &
      & 0.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
      & 0.5_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.5_DFP, 0.5_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.5_DFP, 0.0_DFP ], [3, 6])

    CASE( 3 )
    ! order 3, Triangle10
    NodeCoord = RESHAPE( [ &
      & 0.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
      & 0.33333333333333333333_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.66666666666666666667_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.66666666666666666667_DFP, 0.33333333333333333333_DFP, 0.0_DFP, &
      & 0.33333333333333333333_DFP, 0.66666666666666666667_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.66666666666666666667_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.33333333333333333333_DFP, 0.0_DFP, &
      & 0.33333333333333333333_DFP, 0.33333333333333333333_DFP, 0.0_DFP], [3, 10])

    CASE( 4 )
    ! order 4 Includes bubble nodes also
    ! Trianagle15a
    NodeCoord = RESHAPE( [ &
      & 0.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
      & 0.25_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.5_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.75_DFP, 0.0_DFP, 0.0_DFP, &
      & 0.75_DFP, 0.25_DFP, 0.0_DFP, &
      & 0.5_DFP, 0.5_DFP, 0.0_DFP, &
      & 0.25_DFP, 0.75_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.75_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.5_DFP, 0.0_DFP, &
      & 0.0_DFP, 0.25_DFP, 0.0_DFP, &
      & 0.25_DFP, 0.25_DFP, 0.0_DFP, &
      & 0.5_DFP, 0.25_DFP, 0.0_DFP, &
      & 0.25_DFP, 0.5_DFP, 0.0_DFP], [3, 15])

    CASE( 5 )
    ! This is fifth order triangle
    ! 3 nodes on vertex, 12 nodes on edge, and 6 on the face
    ! Triangle21
      NodeCoord = RESHAPE( [ &
        & 0.0, 0.0, 0.0, &
        & 1.0, 0.0, 0.0, &
        & 0.0, 1.0, 0.0, &
        & 0.2, 0.0, 0.0, &
        & 0.4, 0.0, 0.0, &
        & 0.6, 0.0, 0.0, &
        & 0.8, 0.0, 0.0, &
        & 0.8, 0.2, 0.0, &
        & 0.6, 0.4, 0.0, &
        & 0.4, 0.6, 0.0, &
        & 0.2, 0.8, 0.0, &
        & 0.0, 0.8, 0.0, &
        & 0.0, 0.6, 0.0, &
        & 0.0, 0.4, 0.0, &
        & 0.0, 0.2, 0.0, &
        & 0.2, 0.2, 0.0, &
        & 0.6, 0.2, 0.0, &
        & 0.2, 0.6, 0.0, &
        & 0.4, 0.2, 0.0, &
        & 0.4, 0.4, 0.0, &
        & 0.2, 0.4, 0.0], [3, 21])

    CASE( 6 )
    ! Triangle28
    NodeCoord = RESHAPE( [ &
      & 0.0, 0.0, 0.0, &
      & 1.0, 0.0, 0.0, &
      & 0.0, 1.0, 0.0, &
      & 0.16666666666666666667, 0.0, 0.0, &
      & 0.33333333333333333333, 0.0, 0.0, &
      & 0.5, 0.0, 0.0, &
      & 0.66666666666666666667, 0.0, 0.0, &
      & 0.83333333333333333333, 0.0, 0.0, &
      & 0.83333333333333333333, 0.16666666666666666667, 0.0, &
      & 0.66666666666666666667, 0.33333333333333333333, 0.0, &
      & 0.5, 0.5, 0.0, &
      & 0.33333333333333333333, 0.66666666666666666667, 0.0, &
      & 0.16666666666666666667, 0.83333333333333333333, 0.0, &
      & 0.0, 0.83333333333333333333, 0.0, &
      & 0.0, 0.66666666666666666667, 0.0, &
      & 0.0, 0.5, 0.0, &
      & 0.0, 0.33333333333333333333, 0.0, &
      & 0.0, 0.16666666666666666667, 0.0, &
      & 0.16666666666666666667, 0.16666666666666666667, 0.0, &
      & 0.66666666666666666667, 0.16666666666666666667, 0.0, &
      & 0.16666666666666666667, 0.66666666666666666667, 0.0, &
      & 0.33333333333333333333, 0.16666666666666666667, 0.0, &
      & 0.5, 0.16666666666666666667, 0.0, &
      & 0.5, 0.33333333333333333333, 0.0, &
      & 0.33333333333333333333, 0.5, 0.0, &
      & 0.16666666666666666667, 0.5, 0.0, &
      & 0.16666666666666666667, 0.33333333333333333333, 0.0, &
      & 0.33333333333333333333, 0.33333333333333333333, 0.0], [3, 28] )

    CASE( 7 )
    NodeCoord = RESHAPE( [ &
      & 0.0, 0.0, 0.0, &
      & 1.0, 0.0, 0.0, &
      & 0.0, 1.0, 0.0, &
      & 0.14285714285714285714, 0.0, 0.0, &
      & 0.28571428571428571429, 0.0, 0.0, &
      & 0.42857142857142857143, 0.0, 0.0, &
      & 0.57142857142857142857, 0.0, 0.0, &
      & 0.71428571428571428571, 0.0, 0.0, &
      & 0.85714285714285714286, 0.0, 0.0, &
      & 0.85714285714285714286, 0.14285714285714285714, 0.0, &
      & 0.71428571428571428571, 0.28571428571428571429, 0.0, &
      & 0.57142857142857142857, 0.42857142857142857143, 0.0, &
      & 0.42857142857142857143, 0.57142857142857142857, 0.0, &
      & 0.28571428571428571429, 0.71428571428571428571, 0.0, &
      & 0.14285714285714285714, 0.85714285714285714286, 0.0, &
      & 0.0, 0.85714285714285714286, 0.0, &
      & 0.0, 0.71428571428571428571, 0.0, &
      & 0.0, 0.57142857142857142857, 0.0, &
      & 0.0, 0.42857142857142857143, 0.0, &
      & 0.0, 0.28571428571428571429, 0.0, &
      & 0.0, 0.14285714285714285714, 0.0, &
      & 0.14285714285714285714, 0.14285714285714285714, 0.0, &
      & 0.71428571428571428571, 0.14285714285714285714, 0.0, &
      & 0.14285714285714285714, 0.71428571428571428571, 0.0, &
      & 0.28571428571428571429, 0.14285714285714285714, 0.0, &
      & 0.42857142857142857143, 0.14285714285714285714, 0.0, &
      & 0.57142857142857142857, 0.14285714285714285714, 0.0, &
      & 0.57142857142857142857, 0.28571428571428571429, 0.0, &
      & 0.42857142857142857143, 0.42857142857142857143, 0.0, &
      & 0.28571428571428571429, 0.57142857142857142857, 0.0, &
      & 0.14285714285714285714, 0.57142857142857142857, 0.0, &
      & 0.14285714285714285714, 0.42857142857142857143, 0.0, &
      & 0.14285714285714285714, 0.28571428571428571429, 0.0, &
      & 0.28571428571428571429, 0.28571428571428571429, 0.0, &
      & 0.42857142857142857143, 0.28571428571428571429, 0.0, &
      & 0.28571428571428571429, 0.42857142857142857143, 0.0 ], [3,36])

    CASE( 8 )
    NodeCoord = RESHAPE( [ &
      & 0.0, 0.0, 0.0, &
      & 1.0, 0.0, 0.0, &
      & 0.0, 1.0, 0.0, &
      & 0.125, 0.0, 0.0, &
      & 0.25, 0.0, 0.0, &
      & 0.375, 0.0, 0.0, &
      & 0.5, 0.0, 0.0, &
      & 0.625, 0.0, 0.0, &
      & 0.75, 0.0, 0.0, &
      & 0.875, 0.0, 0.0, &
      & 0.875, 0.125, 0.0, &
      & 0.75, 0.25, 0.0, &
      & 0.625, 0.375, 0.0, &
      & 0.5, 0.5, 0.0, &
      & 0.375, 0.625, 0.0, &
      & 0.25, 0.75, 0.0, &
      & 0.125, 0.875, 0.0, &
      & 0.0, 0.875, 0.0, &
      & 0.0, 0.75, 0.0, &
      & 0.0, 0.625, 0.0, &
      & 0.0, 0.5, 0.0, &
      & 0.0, 0.375, 0.0, &
      & 0.0, 0.25, 0.0, &
      & 0.0, 0.125, 0.0, &
      & 0.125, 0.125, 0.0, &
      & 0.75, 0.125, 0.0, &
      & 0.125, 0.75, 0.0, &
      & 0.25, 0.125, 0.0, &
      & 0.375, 0.125, 0.0, &
      & 0.5, 0.125, 0.0, &
      & 0.625, 0.125, 0.0, &
      & 0.625, 0.25, 0.0, &
      & 0.5, 0.375, 0.0, &
      & 0.375, 0.5, 0.0, &
      & 0.25, 0.625, 0.0, &
      & 0.125, 0.625, 0.0, &
      & 0.125, 0.5, 0.0, &
      & 0.125, 0.375, 0.0, &
      & 0.125, 0.25, 0.0, &
      & 0.25, 0.25, 0.0, &
      & 0.5, 0.25, 0.0, &
      & 0.25, 0.5, 0.0, &
      & 0.375, 0.25, 0.0, &
      & 0.375, 0.375, 0.0, &
      & 0.25, 0.375, 0.0 ], [3, 45])

    CASE( 9 )
    NodeCoord = RESHAPE( [ &
      & 0.0, 0.0, 0.0, &
      & 1.0, 0.0, 0.0, &
      & 0.0, 1.0, 0.0, &
      & 0.11111111111111111111, 0.0, 0.0, &
      & 0.22222222222222222222, 0.0, 0.0, &
      & 0.33333333333333333333, 0.0, 0.0, &
      & 0.44444444444444444444, 0.0, 0.0, &
      & 0.55555555555555555556, 0.0, 0.0, &
      & 0.66666666666666666667, 0.0, 0.0, &
      & 0.77777777777777777778, 0.0, 0.0, &
      & 0.88888888888888888889, 0.0, 0.0, &
      & 0.88888888888888888889, 0.11111111111111111111, 0.0, &
      & 0.77777777777777777778, 0.22222222222222222222, 0.0, &
      & 0.66666666666666666667, 0.33333333333333333333, 0.0, &
      & 0.55555555555555555556, 0.44444444444444444444, 0.0, &
      & 0.44444444444444444444, 0.55555555555555555556, 0.0, &
      & 0.33333333333333333333, 0.66666666666666666667, 0.0, &
      & 0.22222222222222222222, 0.77777777777777777778, 0.0, &
      & 0.11111111111111111111, 0.88888888888888888889, 0.0, &
      & 0.0, 0.88888888888888888889, 0.0, &
      & 0.0, 0.77777777777777777778, 0.0, &
      & 0.0, 0.66666666666666666667, 0.0, &
      & 0.0, 0.55555555555555555556, 0.0, &
      & 0.0, 0.44444444444444444444, 0.0, &
      & 0.0, 0.33333333333333333333, 0.0, &
      & 0.0, 0.22222222222222222222, 0.0, &
      & 0.0, 0.11111111111111111111, 0.0, &
      & 0.11111111111111111111, 0.11111111111111111111, 0.0, &
      & 0.77777777777777777778, 0.11111111111111111111, 0.0, &
      & 0.11111111111111111111, 0.77777777777777777778, 0.0, &
      & 0.22222222222222222222, 0.11111111111111111111, 0.0, &
      & 0.33333333333333333333, 0.11111111111111111111, 0.0, &
      & 0.44444444444444444444, 0.11111111111111111111, 0.0, &
      & 0.55555555555555555556, 0.11111111111111111111, 0.0, &
      & 0.66666666666666666667, 0.11111111111111111111, 0.0, &
      & 0.66666666666666666667, 0.22222222222222222222, 0.0, &
      & 0.55555555555555555556, 0.33333333333333333333, 0.0, &
      & 0.44444444444444444444, 0.44444444444444444444, 0.0, &
      & 0.33333333333333333333, 0.55555555555555555556, 0.0, &
      & 0.22222222222222222222, 0.66666666666666666667, 0.0, &
      & 0.11111111111111111111, 0.66666666666666666667, 0.0, &
      & 0.11111111111111111111, 0.55555555555555555556, 0.0, &
      & 0.11111111111111111111, 0.44444444444444444444, 0.0, &
      & 0.11111111111111111111, 0.33333333333333333333, 0.0, &
      & 0.11111111111111111111, 0.22222222222222222222, 0.0, &
      & 0.22222222222222222222, 0.22222222222222222222, 0.0, &
      & 0.55555555555555555556, 0.22222222222222222222, 0.0, &
      & 0.22222222222222222222, 0.55555555555555555556, 0.0, &
      & 0.33333333333333333333, 0.22222222222222222222, 0.0, &
      & 0.44444444444444444444, 0.22222222222222222222, 0.0, &
      & 0.44444444444444444444, 0.33333333333333333333, 0.0, &
      & 0.33333333333333333333, 0.44444444444444444444, 0.0, &
      & 0.22222222222222222222, 0.44444444444444444444, 0.0, &
      & 0.22222222222222222222, 0.33333333333333333333, 0.0, &
      & 0.33333333333333333333, 0.33333333333333333333, 0.0 ], [3,55] )

    CASE( 10 )
    NodeCoord = RESHAPE( [ &
      & 0.0, 0.0, 0.0, &
      & 1.0, 0.0, 0.0, &
      & 0.0, 1.0, 0.0, &
      & 0.1, 0.0, 0.0, &
      & 0.2, 0.0, 0.0, &
      & 0.3, 0.0, 0.0, &
      & 0.4, 0.0, 0.0, &
      & 0.5, 0.0, 0.0, &
      & 0.6, 0.0, 0.0, &
      & 0.7, 0.0, 0.0, &
      & 0.8, 0.0, 0.0, &
      & 0.9, 0.0, 0.0, &
      & 0.9, 0.1, 0.0, &
      & 0.8, 0.2, 0.0, &
      & 0.7, 0.3, 0.0, &
      & 0.6, 0.4, 0.0, &
      & 0.5, 0.5, 0.0, &
      & 0.4, 0.6, 0.0, &
      & 0.3, 0.7, 0.0, &
      & 0.2, 0.8, 0.0, &
      & 0.1, 0.9, 0.0, &
      & 0.0, 0.9, 0.0, &
      & 0.0, 0.8, 0.0, &
      & 0.0, 0.7, 0.0, &
      & 0.0, 0.6, 0.0, &
      & 0.0, 0.5, 0.0, &
      & 0.0, 0.4, 0.0, &
      & 0.0, 0.3, 0.0, &
      & 0.0, 0.2, 0.0, &
      & 0.0, 0.1, 0.0, &
      & 0.1, 0.1, 0.0, &
      & 0.8, 0.1, 0.0, &
      & 0.1, 0.8, 0.0, &
      & 0.2, 0.1, 0.0, &
      & 0.3, 0.1, 0.0, &
      & 0.4, 0.1, 0.0, &
      & 0.5, 0.1, 0.0, &
      & 0.6, 0.1, 0.0, &
      & 0.7, 0.1, 0.0, &
      & 0.7, 0.2, 0.0, &
      & 0.6, 0.3, 0.0, &
      & 0.5, 0.4, 0.0, &
      & 0.4, 0.5, 0.0, &
      & 0.3, 0.6, 0.0, &
      & 0.2, 0.7, 0.0, &
      & 0.1, 0.7, 0.0, &
      & 0.1, 0.6, 0.0, &
      & 0.1, 0.5, 0.0, &
      & 0.1, 0.4, 0.0, &
      & 0.1, 0.3, 0.0, &
      & 0.1, 0.2, 0.0, &
      & 0.2, 0.2, 0.0, &
      & 0.6, 0.2, 0.0, &
      & 0.2, 0.6, 0.0, &
      & 0.3, 0.2, 0.0, &
      & 0.4, 0.2, 0.0, &
      & 0.5, 0.2, 0.0, &
      & 0.5, 0.3, 0.0, &
      & 0.4, 0.4, 0.0, &
      & 0.3, 0.5, 0.0, &
      & 0.2, 0.5, 0.0, &
      & 0.2, 0.4, 0.0, &
      & 0.2, 0.3, 0.0, &
      & 0.3, 0.3, 0.0, &
      & 0.4, 0.3, 0.0, &
      & 0.3, 0.4, 0.0 ], [3,66] )
  END SELECT

  ALLOCATE( Xi( SIZE( NodeCoord, 2 ) ), Eta( SIZE( NodeCoord, 2 ) ) )
  Xi( : ) = NodeCoord( 1, : )
  Eta( : ) = NodeCoord( 2, : )

  x = XiJ( 1, 1:3 )
  y = XiJ( 2, 1:3 )

  NodeCoord( 1, : ) = x( 1 ) + ( x( 2 ) - x( 1 ) ) * Xi &
    + ( x( 3 ) - x( 1 ) ) * Eta

  NodeCoord( 2, : ) = y( 1 ) + ( y( 2 ) - y( 1 ) ) * Xi &
    + ( y( 3 ) - y( 1 ) ) * Eta

  DEALLOCATE( Xi, Eta )
END FUNCTION EquidistanceLIP_Triangle


!----------------------------------------------------------------------------
!                                                           LagrangeElement
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeElement_Triangle
  INTEGER( I4B ) :: I, NNS, NSD

  CALL DeallocateData( obj )
  obj%XiJ = EquidistanceLIP_Triangle( RefElem%XiJ(1:3, 1:3), Order )
  NSD = RefElem%NSD
  obj%LagrangeElement => RefElem%LagrangeElement
  SELECT CASE( Order )
  CASE( 1 )
    NNS = 3
    obj%EntityCounts = [NNS, 3, 1, 0]
    obj%XiDimension = 2
    obj%Name = Triangle3
    obj%Order = Order
    obj%NSD = NSD
    ALLOCATE( obj%Topology( SUM( obj%EntityCounts) ) )
    DO I = 1, NNS
      obj%Topology( I ) = ReferenceTopology( [I], Point )
    END DO
    obj%Topology( NNS + 1 ) = ReferenceTopology( [1, 2], Line2 )
    obj%Topology( NNS + 2 ) = ReferenceTopology( [2, 3], Line2 )
    obj%Topology( NNS + 3 ) = ReferenceTopology( [3, 1], Line2 )
    obj%Topology( NNS + 4 ) = ReferenceTopology( [1, 2, 3], obj%Name )

  CASE( 2 )
    NNS = 6
    obj%EntityCounts = [NNS, 3, 1, 0]
    obj%XiDimension = 2
    obj%Name = Triangle6
    obj%Order = Order
    obj%NSD = NSD
    ALLOCATE( obj%Topology( SUM( obj%EntityCounts) ) )
    DO I = 1, NNS
      obj%Topology( I ) = ReferenceTopology( [I], Point )
    END DO
    obj%Topology( NNS + 1 ) = ReferenceTopology( [1, 2, 4], Line3 )
    obj%Topology( NNS + 2 ) = ReferenceTopology( [2, 3, 5], Line3 )
    obj%Topology( NNS + 3 ) = ReferenceTopology( [3, 1, 6], Line3 )
    obj%Topology( NNS + 4 ) = ReferenceTopology( [1, 2, 3, 4, 5, 6], obj%Name )

  CASE( 3 )
    NNS = 10
    obj%EntityCounts = [NNS, 3, 1, 0]
    obj%XiDimension = 2
    obj%Name = Triangle10
    obj%Order = Order
    obj%NSD = NSD
    ALLOCATE( obj%Topology( SUM( obj%EntityCounts) ) )
    DO I = 1, NNS
      obj%Topology( I ) = ReferenceTopology( [I], Point )
    END DO
    obj%Topology( NNS + 1 ) = ReferenceTopology( [1, 2, 4, 5], Line4 )
    obj%Topology( NNS + 2 ) = ReferenceTopology( [2, 3, 6, 7], Line4 )
    obj%Topology( NNS + 3 ) = ReferenceTopology( [3, 1, 8, 9], Line4 )
    obj%Topology( NNS + 4 ) = ReferenceTopology( &
      & [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], obj%Name )
  END SELECT
END PROCEDURE LagrangeElement_Triangle

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex_Triangle
  IF( refelem%nsd .EQ. 2 ) THEN
    CALL TRIANGLEAREA2D( XiJ( 1:2, 1:3 ), Ans )
  ELSE
    CALL TRIANGLEAREA3D( XiJ( 1:3, 1:3), Ans )
  END IF
END PROCEDURE Measure_Simplex_Triangle

!----------------------------------------------------------------------------
!                                                           Triangle_Angles
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_angles
  SELECT CASE( refelem%nsd )
  CASE( 2 )
    CALL triangle_angles_2d( xij(1:2, 1:3), ans )
  CASE( 3 )
    CALL triangle_angles_3d( xij(1:3, 1:3), ans )
  END SELECT
END PROCEDURE triangle_angles

!----------------------------------------------------------------------------
!                                                             Triangle_Area
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area
  SELECT CASE(refelem%nsd)
  CASE(2)
    CALL triangle_area_2d( xij(1:2, 1:3), ans )
  CASE(3)
    CALL TRIANGLE_AREA_3D_3( xij(1:3, 1:3), ans )
  END SELECT
END PROCEDURE triangle_area

!----------------------------------------------------------------------------
!                                                        Triangle_ArealVector
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_ArealVector
  SELECT CASE(refelem%nsd)
  CASE(2)
    ans(1:2) = 0.0_DFP
    CALL triangle_area_2d( xij(1:2, 1:3), ans(3))
  CASE(3)
    CALL triangle_area_vector_3d(xij(1:3, 1:3), ans)
  END SELECT
END PROCEDURE triangle_ArealVector

!----------------------------------------------------------------------------
!                                                       Triangle_Barycentric
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_barycentric
  CALL triangle_barycentric_2d ( xij(1:2, 1:3), x(1:2), ans )
END PROCEDURE triangle_barycentric

!----------------------------------------------------------------------------
!                                                         Triangle_Centroid
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid
  IF( refelem%nsd .EQ. 2 ) THEN
    Ans(3)=0.0_DFP
    CALL triangle_centroid_2d( xij(1:2, 1:3), ans(1:2) )
  ELSE
    CALL triangle_centroid_3d( xij(1:3, 1:3), ans(1:3) )
  END IF
END PROCEDURE triangle_centroid

!----------------------------------------------------------------------------
!                                                      triangle_circumcentre
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcentre
  IF( refelem%nsd .EQ. 2 ) THEN
    Ans(3)=0.0_DFP
    CALL triangle_circumcenter_2d( xij(1:2, 1:3), Ans(1:2))
  ELSE
    CALL triangle_circumcenter( 3, xij(1:3, 1:3), Ans(1:3))
  END IF
END PROCEDURE triangle_circumcentre

!----------------------------------------------------------------------------
!                                                      triangle_circumcircle
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcircle
  Ans(4)=0.0_DFP
  CALL triangle_circumcircle_2d( xij(1:2, 1:3), Ans(1), Ans(2:3))
END PROCEDURE triangle_circumcircle

!----------------------------------------------------------------------------
!                                                     triangle_circumradius
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumradius
  CALL triangle_circumradius_2d( xij(1:2, 1:3), Ans )
END PROCEDURE triangle_circumradius

!----------------------------------------------------------------------------
!                                                   triangle_contains_line
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line
  IF( parametricLine ) THEN
    CALL triangle_contains_line_par_3d( xij(1:3, 1:3), x1, x2, &
      & inside, xint)
  ELSE
    CALL triangle_contains_line_exp_3d( xij(1:3, 1:3), x1, x2, &
      & inside, xint)
  END IF
END PROCEDURE triangle_contains_line

!----------------------------------------------------------------------------
!                                                    triangle_contains_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point
  CALL triangle_contains_point_2d_1(xij(1:2,1:3), x(1:2), ans)
END PROCEDURE triangle_contains_point

!----------------------------------------------------------------------------
!                                                         triangle_diameter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_diameter
  CALL triangle_diameter_2d(xij(1:2, 1:3), Ans)
END PROCEDURE triangle_diameter

!----------------------------------------------------------------------------
!                                                       triangle_edge_length
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_edge_length
  CALL triangle_edge_length_2d(xij(1:2, 1:3), Ans)
END PROCEDURE triangle_edge_length

!----------------------------------------------------------------------------
!                                                        triangle_incenter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incenter
  Ans(3)=0.0_DFP
  CALL triangle_incenter_2d(xij(1:2,1:3), Ans(1:2))
END PROCEDURE triangle_incenter

!----------------------------------------------------------------------------
!                                                         triangle_incircle
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incircle
  Ans(4) = 0.0_DFP
  CALL triangle_incircle_2d(xij(1:2,1:3), Ans(1), Ans(2:3))
END PROCEDURE triangle_incircle

!----------------------------------------------------------------------------
!                                                          triangle_inradius
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_inradius
  CALL triangle_inradius_2d(xij(1:2,1:3), Ans)
END PROCEDURE triangle_inradius

!----------------------------------------------------------------------------
!                                                      triangle_orthocenter
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orthocenter
  Ans(3)=0.0_DFP
  CALL triangle_orthocenter_2d(xij(1:2,1:3), Ans(1:2))
END PROCEDURE triangle_orthocenter

!----------------------------------------------------------------------------
!                                                       triangle_point_dist
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist
  SELECT CASE( refelem%nsd)
  CASE(2)
    CALL triangle_point_dist_2d(xij(1:2,1:3), x(1:2), Ans)
  CASE(3)
      CALL triangle_point_dist_3d(xij(1:3,1:3), x(1:3), Ans)
  END SELECT
END PROCEDURE triangle_point_dist

!----------------------------------------------------------------------------
!                                                     triangle_nearest_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_get_nearest_point
  CALL triangle_point_near_2d(xij(1:2,1:3), x(1:2), xn(1:2), dist)
END PROCEDURE triangle_get_nearest_point

!----------------------------------------------------------------------------
!                                                      triangle_random_point
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_random_point
  Ans=0.0_DFP
  CALL triangle_sample(xij(1:2,1:3), n, seed, Ans(1:2, 1:n))
END PROCEDURE triangle_random_point

!----------------------------------------------------------------------------
!                                                          triangle_quality
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_quality
  REAL( DFP ) :: rvar(3)

  SELECT CASE(measure)

  CASE( QualityMeasure%area )
    Ans=Area(refelem=refelem, xij=xij)

  CASE( QualityMeasure%maxangle )
    Ans=MAXVAL(Angles(refelem=refelem, xij=xij))

  CASE( QualityMeasure%minangle )
    Ans=MINVAL(Angles(refelem=refelem, xij=xij))

  CASE( QualityMeasure%angleratio )
    Ans=3.0_DFP * MINVAL(Angles(refelem=refelem, xij=xij)) / Pi

  CASE( QualityMeasure%radiusRatio )
    Ans=2.0_DFP * InRadius(refelem=refelem, xij=xij) &
      & / CircumRadius(refelem=refelem, xij=xij)

  CASE( QualityMeasure%edgeRatio )
    rvar = EdgeLength(refelem=refelem, xij=xij)
    Ans = MINVAL(rvar)/MAXVAL(rvar)

  CASE( QualityMeasure%aspectRatio )
    rvar = EdgeLength(refelem=refelem, xij=xij)
    Ans = MAXVAL(rvar) * SUM(rvar) &
      & / (4.0_DFP*SQRT(3.0_DFP)*Area(refelem=refelem, xij=xij))

  END SELECT
END PROCEDURE triangle_quality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./modified_burkardt.inc"
END SUBMODULE Methods