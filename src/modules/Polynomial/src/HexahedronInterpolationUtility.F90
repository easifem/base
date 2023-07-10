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

MODULE HexahedronInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Hexahedron
PUBLIC :: LagrangeDOF_Hexahedron
PUBLIC :: LagrangeInDOF_Hexahedron
PUBLIC :: EquidistanceInPoint_Hexahedron
PUBLIC :: EquidistancePoint_Hexahedron
PUBLIC :: InterpolationPoint_Hexahedron
PUBLIC :: LagrangeCoeff_Hexahedron
PUBLIC :: RefHexahedronCoord
PUBLIC :: EdgeConnectivity_Hexahedron
PUBLIC :: FacetConnectivity_Hexahedron

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-07
! summary:  This function returns the edge connectivity of Hexahedron

INTERFACE
  MODULE PURE FUNCTION FacetConnectivity_Hexahedron() RESULT(ans)
    INTEGER(I4B) :: ans(4, 6)
  END FUNCTION FacetConnectivity_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-07
! summary:  This function returns the edge connectivity of Hexahedron

INTERFACE
  MODULE PURE FUNCTION EdgeConnectivity_Hexahedron() RESULT(ans)
    INTEGER(I4B) :: ans(2, 12)
  END FUNCTION EdgeConnectivity_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       RefHexahedronCoord
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION RefHexahedronCoord(refHexahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refHexahedron
    REAL(DFP) :: ans(3, 8)
  END FUNCTION RefHexahedronCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Hexahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Hexahedron

INTERFACE LagrangeDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeDOF_Hexahedron1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Hexahedron1
END INTERFACE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Hexahedron

INTERFACE LagrangeDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeDOF_Hexahedron2(p, q, r) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Hexahedron2
END INTERFACE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!- These dof are strictly inside the Hexahedron

INTERFACE LagrangeInDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeInDOF_Hexahedron1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Hexahedron1
END INTERFACE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Hexahedron
!- These dof are strictly inside the Hexahedron

INTERFACE LagrangeInDOF_Hexahedron
  MODULE PURE FUNCTION LagrangeInDOF_Hexahedron2(p, q, r) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Hexahedron2
END INTERFACE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Hexahedron
!
!# Introduction
!
!- This function returns the equidistance points in Hexahedron
!- All points are inside the Hexahedron

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Hexahedron(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Hexahedron element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Hexahedron element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention, VEFC.

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint_Hexahedron(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Interpolation point

INTERFACE InterpolationPoint_Hexahedron
  MODULE FUNCTION InterpolationPoint_Hexahedron1(order, ipType, &
    & layout, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: ipType
    CHARACTER(*), INTENT(IN) :: layout
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION InterpolationPoint_Hexahedron1
END INTERFACE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-10
! summary:  Interpolation points

INTERFACE InterpolationPoint_Hexahedron
  MODULE FUNCTION InterpolationPoint_Hexahedron2(p, q, r, ipType1, ipType2, &
    & ipType3, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: r
    INTEGER(I4B), INTENT(IN) :: ipType1
    INTEGER(I4B), INTENT(IN) :: ipType2
    INTEGER(I4B), INTENT(IN) :: ipType3
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION InterpolationPoint_Hexahedron2
END INTERFACE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE IJK2VEFC_Hexahedron(xi, eta, zeta, temp, &
    & p, q, r)
    REAL(DFP), INTENT(IN) :: xi(:, :, :)
    REAL(DFP), INTENT(IN) :: eta(:, :, :)
    REAL(DFP), INTENT(IN) :: zeta(:, :, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: r
  END SUBROUTINE IJK2VEFC_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Hexahedron1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron1
END INTERFACE

INTERFACE LagrangeCoeff_Hexahedron
  MODULE PROCEDURE LagrangeCoeff_Hexahedron1
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Hexahedron2(order, i, v, isVandermonde) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron2
END INTERFACE

INTERFACE LagrangeCoeff_Hexahedron
  MODULE PROCEDURE LagrangeCoeff_Hexahedron2
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Hexahedron3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron3
END INTERFACE

INTERFACE LagrangeCoeff_Hexahedron
  MODULE PROCEDURE LagrangeCoeff_Hexahedron3
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Hexahedron4(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron4
END INTERFACE

INTERFACE LagrangeCoeff_Hexahedron
  MODULE PROCEDURE LagrangeCoeff_Hexahedron4
END INTERFACE LagrangeCoeff_Hexahedron

END MODULE HexahedronInterpolationUtility
