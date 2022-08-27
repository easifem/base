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

MODULE PyramidInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                     LagrangeDegree_Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION LagrangeDegree_Pyramid( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ), ALLOCATABLE :: ans(:,:)
END FUNCTION LagrangeDegree_Pyramid
END INTERFACE

PUBLIC :: LagrangeDegree_Pyramid

!----------------------------------------------------------------------------
!                                                       LagrangeDOF_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial on Pyramid

INTERFACE
MODULE PURE FUNCTION LagrangeDOF_Pyramid( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeDOF_Pyramid
END INTERFACE

PUBLIC :: LagrangeDOF_Pyramid

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Pyramid
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Pyramid
!- These dof are strictly inside the Pyramid

INTERFACE
MODULE PURE FUNCTION LagrangeInDOF_Pyramid( order ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  INTEGER( I4B ) :: ans
END FUNCTION LagrangeInDOF_Pyramid
END INTERFACE

PUBLIC :: LagrangeInDOF_Pyramid

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Pyramid( order, ipType, xij ) &
    & RESULT( nodecoord )
    !!
    INTEGER( I4B ), INTENT( IN ) :: order
    INTEGER( I4B ), INTENT( IN ) :: ipType
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
    REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
    !!
  END FUNCTION InterpolationPoint_Pyramid
END INTERFACE

PUBLIC :: InterpolationPoint_Pyramid

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: 	Returns equidistance points in Pyramid
!
!# Introduction
!
!- This function returns the equidistance points in Pyramid
!- All points are inside the Pyramid

INTERFACE
MODULE PURE FUNCTION EquidistanceInPoint_Pyramid( order, xij ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 4
  REAL( DFP ), ALLOCATABLE :: ans(:,:)
  !! returned coordinates in $x_{iJ}$ format
END FUNCTION EquidistanceInPoint_Pyramid
END INTERFACE

PUBLIC :: EquidistanceInPoint_Pyramid

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: 	Returns the nodal coordinates of higher order Pyramid element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Pyramid element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE
MODULE PURE FUNCTION EquidistancePoint_Pyramid( order, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
  REAL( DFP ), ALLOCATABLE :: ans(:,:)
  !! returned coordinates in $x_{iJ}$ format
END FUNCTION EquidistancePoint_Pyramid
END INTERFACE

PUBLIC :: EquidistancePoint_Pyramid

!----------------------------------------------------------------------------
!                                               EquidistanceLIP_Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION EquidistanceLIP_Pyramid( order, xij ) &
  & RESULT( nodecoord )
  INTEGER( I4B ), INTENT( IN ) :: order
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  REAL( DFP ), ALLOCATABLE :: nodecoord( :, : )
END FUNCTION EquidistanceLIP_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE PyramidInterpolationUtility