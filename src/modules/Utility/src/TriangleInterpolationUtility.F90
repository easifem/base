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

MODULE TriangleInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION LagrangeDegree_Triangle( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ), ALLOCATABLE :: ans(:,:)
END FUNCTION LagrangeDegree_Triangle
END INTERFACE

PUBLIC :: LagrangeDegree_Triangle

!----------------------------------------------------------------------------
!                                                       LagrangeDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial on triangle

INTERFACE
MODULE PURE FUNCTION LagrangeDOF_Triangle( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeDOF_Triangle
END INTERFACE

PUBLIC :: LagrangeDOF_Triangle

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of triangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of triangle
!- These dof are strictly inside the triangle

INTERFACE
MODULE PURE FUNCTION LagrangeInDOF_Triangle( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeInDOF_Triangle
END INTERFACE

PUBLIC :: LagrangeInDOF_Triangle

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns equidistance points in triangle
!
!# Introduction
!
!- This function returns the equidistance points in triangle
!- All points are inside the triangle

INTERFACE
MODULE PURE FUNCTION EquidistanceInPoint_Triangle( order, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
  REAL( DFP ), ALLOCATABLE :: ans(:,:)
  !! returned coordinates in $x_{iJ}$ format
END FUNCTION EquidistanceInPoint_Triangle
END INTERFACE

PUBLIC :: EquidistanceInPoint_Triangle

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Returns the nodal coordinates of higher order triangle element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! triangle element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE
MODULE PURE FUNCTION EquidistancePoint_Triangle( order, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
  REAL( DFP ), ALLOCATABLE :: ans(:,:)
  !! returned coordinates in $x_{iJ}$ format
END FUNCTION EquidistancePoint_Triangle
END INTERFACE

PUBLIC :: EquidistancePoint_Triangle

!----------------------------------------------------------------------------
!                                                InterpolationPoint_Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Triangle( order, ipType, xij ) &
    & RESULT( nodecoord )
    INTEGER( I4B ), INTENT( IN ) :: order
    INTEGER( I4B ), INTENT( IN ) :: ipType
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( 3, 3 )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
  END FUNCTION InterpolationPoint_Triangle
END INTERFACE
=======
>>>>>>> dev

INTERFACE TriangleLagrangeEquidistance
  MODULE PROCEDURE EquidistanceLIP_Triangle
END INTERFACE TriangleLagrangeEquidistance

PUBLIC :: TriangleLagrangeEquidistance

PUBLIC :: InterpolationPoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TriangleInterpolationUtility