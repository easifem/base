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
!                                                 LagrangeDegree_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Tetrahedron
END INTERFACE

PUBLIC :: LagrangeDegree_Tetrahedron

!----------------------------------------------------------------------------
!                                                   LagrangeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Tetrahedron

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Tetrahedron
END INTERFACE

PUBLIC :: LagrangeDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                 LagrangeInDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Tetrahedron
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Tetrahedron
!- These dof are strictly inside the Tetrahedron

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Tetrahedron
END INTERFACE

PUBLIC :: LagrangeInDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Tetrahedron
!
!# Introduction
!
!- This function returns the equidistance points in Tetrahedron
!- All points are inside the Tetrahedron

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Tetrahedron(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Tetrahedron
END INTERFACE

PUBLIC :: EquidistanceInPoint_Tetrahedron

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Tetrahedron element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Tetrahedron element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint_Tetrahedron(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Tetrahedron
END INTERFACE

PUBLIC :: EquidistancePoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Interpolation point

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Tetrahedron(order, ipType, xij) &
    & RESULT(nodecoord)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation type
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(3, 4)
    !! coordinates of vertices in $x_{iJ}$ format
    REAL(DFP), ALLOCATABLE :: nodecoord(:, :)
    !! interpolation points in $x_{iJ}$ format
  END FUNCTION InterpolationPoint_Tetrahedron
END INTERFACE

PUBLIC :: InterpolationPoint_Tetrahedron

END MODULE TetrahedronInterpolationUtility
