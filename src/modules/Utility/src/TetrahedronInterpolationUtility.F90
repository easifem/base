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

MODULE TetrahedronInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary: 	Get interpolation points on Tetrahedron

INTERFACE
MODULE PURE FUNCTION InterpolationPoint_Tetrahedron( order, ipType, xij ) &
  & RESULT( nodecoord )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order of element
  INTEGER( I4B ), INTENT( IN ) :: ipType
  !! interpolation point type
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( 3, 4 )
  !! nodal coordinates of simplex
  REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
  !! interpolation points
END FUNCTION InterpolationPoint_Tetrahedron
END INTERFACE

PUBLIC :: InterpolationPoint_Tetrahedron

INTERFACE TetrahedronInterpolationPoint
  MODULE PROCEDURE InterpolationPoint_Tetrahedron
END INTERFACE TetrahedronInterpolationPoint

PUBLIC :: TetrahedronInterpolationPoint

!----------------------------------------------------------------------------
!                                               EquidistanceLIP_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary: 	Uniformly distributed points on tetrahedron
!
!# Introduction
!
! This routine returns the interpolation points on tetrahedron. These
! are uniformly distributed points. The scheme is same what Gmsh uses.

INTERFACE
  MODULE PURE FUNCTION EquidistanceLIP_Tetrahedron( order, xij ) &
    & RESULT( nodecoord )
    INTEGER( I4B ), INTENT( IN ) :: order
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( 3, 4 )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
  END FUNCTION EquidistanceLIP_Tetrahedron
END INTERFACE

INTERFACE TetrahedronLagrangeEquidistance
  MODULE PROCEDURE EquidistanceLIP_Tetrahedron
END INTERFACE TetrahedronLagrangeEquidistance

PUBLIC :: TetrahedronLagrangeEquidistance

END MODULE TetrahedronInterpolationUtility