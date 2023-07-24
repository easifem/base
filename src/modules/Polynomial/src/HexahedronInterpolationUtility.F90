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
PUBLIC :: EquidistancePoint_Hexahedron
PUBLIC :: EquidistanceInPoint_Hexahedron
PUBLIC :: InterpolationPoint_Hexahedron
PUBLIC :: LagrangeCoeff_Hexahedron
PUBLIC :: RefHexahedronCoord
PUBLIC :: EdgeConnectivity_Hexahedron
PUBLIC :: FacetConnectivity_Hexahedron
PUBLIC :: QuadratureNumber_Hexahedron
PUBLIC :: TensorProdBasis_Hexahedron
PUBLIC :: VertexBasis_Hexahedron
PUBLIC :: xEdgeBasis_Hexahedron
PUBLIC :: yEdgeBasis_Hexahedron
PUBLIC :: zEdgeBasis_Hexahedron
PUBLIC :: EdgeBasis_Hexahedron
PUBLIC :: xyFacetBasis_Hexahedron
PUBLIC :: yzFacetBasis_Hexahedron
PUBLIC :: xzFacetBasis_Hexahedron
PUBLIC :: FacetBasis_Hexahedron
PUBLIC :: CellBasis_Hexahedron
PUBLIC :: HeirarchicalBasis_Hexahedron
PUBLIC :: QuadraturePoint_Hexahedron
PUBLIC :: LagrangeEvalAll_Hexahedron
PUBLIC :: GetVertexDOF_Hexahedron
PUBLIC :: GetEdgeDOF_Hexahedron
PUBLIC :: GetFacetDOF_Hexahedron
PUBLIC :: GetCellDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                    GetVertexDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-24
! summary:  returns total number of vertex degrees of freedom

INTERFACE
  MODULE PURE FUNCTION GetVertexDOF_Hexahedron() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION GetVertexDOF_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-24
! summary:  returns total number of degrees of freedom on edges parallel to
! some axis

INTERFACE GetEdgeDOF_Hexahedron
  MODULE PURE FUNCTION GetEdgeDOF_Hexahedron1(pe1, pe2, pe3, pe4)  &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1, pe2, pe3, pe4
    !! Order of interpolation in x or y or z direction
    INTEGER(I4B) :: ans
  END FUNCTION GetEdgeDOF_Hexahedron1
END INTERFACE GetEdgeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary:  Returns total number of degrees of freedom on all edges

INTERFACE GetEdgeDOF_Hexahedron
  MODULE PURE FUNCTION GetEdgeDOF_Hexahedron2(p, q, r)  &
  & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! Order of approximation in x,y and z direction
    INTEGER(I4B) :: ans
  END FUNCTION GetEdgeDOF_Hexahedron2
END INTERFACE GetEdgeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary:  Returns total number of degrees of freedom on all edges

INTERFACE GetEdgeDOF_Hexahedron
  MODULE PURE FUNCTION GetEdgeDOF_Hexahedron3(p)  &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B) :: ans
  END FUNCTION GetEdgeDOF_Hexahedron3
END INTERFACE GetEdgeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary:  Returns total number of degrees of freedom on all edges

INTERFACE GetEdgeDOF_Hexahedron
  MODULE PURE FUNCTION GetEdgeDOF_Hexahedron4( &
    & px1, px2, px3, px4, &
    & py1, py2, py3, py4, &
    & pz1, pz2, pz3, pz4)  &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: px1, px2, px3, px4
    !! orders alongs edges parallel to x axis
    INTEGER(I4B), INTENT(IN) :: py1, py2, py3, py4
    !! orders along edges parallel to y axis
    INTEGER(I4B), INTENT(IN) :: pz1, pz2, pz3, pz4
    !! orders along edges parallel to z axis
    INTEGER(I4B) :: ans
  END FUNCTION GetEdgeDOF_Hexahedron4
END INTERFACE GetEdgeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns dof on all facets

INTERFACE GetFacetDOF_Hexahedron
  MODULE PURE FUNCTION GetFacetDOF_Hexahedron1( &
    & pxy1, pxy2, &
    & pxz1, pxz2, &
    & pyz1, pyz2) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pxy1, pxy2
    !! orders alongs facets parallel to xy plane
    INTEGER(I4B), INTENT(IN) :: pxz1, pxz2
    !! orders along facets parallel to xz plane
    INTEGER(I4B), INTENT(IN) :: pyz1, pyz2
    !! orders along facets parallel to yx plane
    INTEGER(I4B) :: ans
  END FUNCTION GetFacetDOF_Hexahedron1
END INTERFACE GetFacetDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns total degrees of freedom on all facets

INTERFACE GetFacetDOF_Hexahedron
  MODULE PURE FUNCTION GetFacetDOF_Hexahedron2(p, q, r) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! orders in x, y and z direction
    INTEGER(I4B) :: ans
  END FUNCTION GetFacetDOF_Hexahedron2
END INTERFACE GetFacetDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns total degree of freedom on a single facet

INTERFACE GetFacetDOF_Hexahedron
  MODULE PURE FUNCTION GetFacetDOF_Hexahedron3(p, q) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    !! orders alongs facets parallel to xy or xz or yz planes
    INTEGER(I4B) :: ans
  END FUNCTION GetFacetDOF_Hexahedron3
END INTERFACE GetFacetDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns total degrees of freedom on all facets

INTERFACE GetFacetDOF_Hexahedron
  MODULE PURE FUNCTION GetFacetDOF_Hexahedron4(p) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! orders alongs facets parallel to xy or xz or yz planes
    INTEGER(I4B) :: ans
  END FUNCTION GetFacetDOF_Hexahedron4
END INTERFACE GetFacetDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary:

INTERFACE GetCellDOF_Hexahedron
  MODULE PURE FUNCTION GetCellDOF_Hexahedron1(p, q, r) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! orders alongs to x, y, and z directions
    INTEGER(I4B) :: ans
  END FUNCTION GetCellDOF_Hexahedron1
END INTERFACE GetCellDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary:

INTERFACE GetCellDOF_Hexahedron
  MODULE PURE FUNCTION GetCellDOF_Hexahedron2(p) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! orders alongs to x, y, and z directions
    INTEGER(I4B) :: ans
  END FUNCTION GetCellDOF_Hexahedron2
END INTERFACE GetCellDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                 QuadratureNumber_Hexahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Hexahedron( &
    & p,  &
    & q,  &
    & r,  &
    & quadType1,  &
    & quadType2,  &
    & quadType3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2, quadType3
    INTEGER(I4B) :: ans(3)
  END FUNCTION QuadratureNumber_Hexahedron
END INTERFACE

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

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-07
! summary:  Returns coordinates of reference Hexahedron

INTERFACE
  MODULE PURE FUNCTION RefHexahedronCoord(refHexahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(3, 8)
  END FUNCTION RefHexahedronCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Hexahedron
  MODULE PURE FUNCTION LagrangeDegree_Hexahedron1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Hexahedron1
END INTERFACE LagrangeDegree_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Hexahedron
  MODULE PURE FUNCTION LagrangeDegree_Hexahedron2(p, q, r) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: r
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Hexahedron2
END INTERFACE LagrangeDegree_Hexahedron

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

INTERFACE EquidistanceInPoint_Hexahedron
  MODULE PURE FUNCTION EquidistanceInPoint_Hexahedron1(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Hexahedron1
END INTERFACE EquidistanceInPoint_Hexahedron

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

INTERFACE EquidistanceInPoint_Hexahedron
  MODULE PURE FUNCTION EquidistanceInPoint_Hexahedron2(p, q, r, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! order in x, y, and z direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Hexahedron2
END INTERFACE EquidistanceInPoint_Hexahedron

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

INTERFACE EquidistancePoint_Hexahedron
  MODULE PURE FUNCTION EquidistancePoint_Hexahedron1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Hexahedron1
END INTERFACE EquidistancePoint_Hexahedron

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

INTERFACE EquidistancePoint_Hexahedron
  MODULE PURE FUNCTION EquidistancePoint_Hexahedron2(p, q, r, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order in z direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! number of rows = 3
    !! number of cols = 8
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Hexahedron2
END INTERFACE EquidistancePoint_Hexahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Interpolation point

INTERFACE InterpolationPoint_Hexahedron
  MODULE FUNCTION InterpolationPoint_Hexahedron1(order, ipType, &
    & layout, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in x, y and z direction
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation type in x, y, and z direction
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! layout can be VEFC or INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordiantes of reference hexahedron
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! rows of ans denotes x, y, z components
    !! cols of ans denotes x, y, z components
  END FUNCTION InterpolationPoint_Hexahedron1
END INTERFACE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-10
! summary:  Interpolation points

INTERFACE InterpolationPoint_Hexahedron
  MODULE FUNCTION InterpolationPoint_Hexahedron2(  &
    & p, &
    & q, &
    & r, &
    & ipType1,  &
    & ipType2, &
    & ipType3,  &
    & layout,  &
    & xij, &
    & alpha1, beta1, lambda1, &
    & alpha2, beta2, lambda2, &
    & alpha3, beta3, lambda3 &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order in z direction
    INTEGER(I4B), INTENT(IN) :: ipType1
    !! interpolation type in x direction
    INTEGER(I4B), INTENT(IN) :: ipType2
    !! interpolation type in y direction
    INTEGER(I4B), INTENT(IN) :: ipType3
    !! interpolation type in z direction
    CHARACTER(*), INTENT(IN) :: layout
    !! layout can be VEFC or INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinate of reference Hexahedron
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta3
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda3
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! rows of ans denotes x, y, z components
    !! cols of ans denotes x, y, z components
  END FUNCTION InterpolationPoint_Hexahedron2
END INTERFACE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-10
! summary:  Convert IJK to VEFC format

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE IJK2VEFC_Hexahedron( &
    & xi, &
    & eta, &
    & zeta, &
    & temp, &
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

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns coefficients of monomials for ith lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points in xij format
    !! number of rows in xij is 3
    !! number of columns should be equal to the number degree of freedom
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron1
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns coefficients of monomials for ith lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
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
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns coefficients of monomials for ith lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
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
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns the coefficients of monomials for all lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron4(order, xij, basisType, &
    & refHexahedron, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron4
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-07-10
! summary:  Returns the coefficients of monomials for all lagrange polynomial

INTERFACE LagrangeCoeff_Hexahedron
  MODULE FUNCTION LagrangeCoeff_Hexahedron5(&
    & p, &
    & q, &
    & r, &
    & xij, &
    & basisType1, &
    & basisType2, &
    & basisType3, &
    & alpha1, &
    & beta1, &
    & lambda1, &
    & alpha2, &
    & beta2, &
    & lambda2, &
    & alpha3, &
    & beta3, &
    & lambda3, &
    & refHexahedron &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of polynomial in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of polynomial in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order of polynomial in z direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! These are interpolation points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis type in x direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basis type in y direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType3
    !! basis type in z direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! This parameter is needed when basisType1 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! This parameter is needed when basisType1 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! This parameter is needed when basisType1 is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! This parameter is needed when basisType2 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! This parameter is needed when basisType2 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! This parameter is needed when basisType2 is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3
    !! This parameter is needed when basisType3 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta3
    !! This parameter is needed when basisType3 is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda3
    !! This parameter is needed when basisType3 is Ultraspherical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refHexahedron
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Hexahedron5
END INTERFACE LagrangeCoeff_Hexahedron

!----------------------------------------------------------------------------
!                                            TensorProdBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all tensor product orthogoanl polynomial on hexahedron

INTERFACE TensorProdBasis_Hexahedron
  MODULE FUNCTION TensorProdBasis_Hexahedron1(  &
    & p,  &
    & q,  &
    & r,  &
    & xij, &
    & basisType1,  &
    & basisType2,  &
    & basisType3,  &
    & alpha1,  &
    & beta1,  &
    & lambda1,  &
    & alpha2,  &
    & beta2,  &
    & lambda2,  &
    & alpha3,  &
    & beta3,  &
    & lambda3) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    INTEGER(I4B), INTENT(IN) :: r
    !! highest order in x3 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: basisType1, basisType2, basisType3
    !! basis type in x1 direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! alpha1 needed when  basisType1 "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! beta1 is needed when basisType1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! lambda1 is needed when basisType1 is "Ultraspherical"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! alpha2 needed when basisType2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! beta2 needed when basisType2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! lambda2 is needed when basisType2 is "Ultraspherical"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3
    !! alpha3 needed when  basisType3 "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta3
    !! beta3 is needed when basisType3 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda3
    !! lambda3 is needed when basisType3 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1) * (r + 1))
    !!
  END FUNCTION TensorProdBasis_Hexahedron1
END INTERFACE TensorProdBasis_Hexahedron

!----------------------------------------------------------------------------
!                                            TensorProdBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all tensor product orthogoanl polynomial on quadrangle
!
!# Introduction
!
! This function returns the tensor product expansion of orthogonal
! polynomial on biunit quadrangle. Here xij is obtained by
! outer product of x and y

INTERFACE TensorProdBasis_Hexahedron
  MODULE FUNCTION TensorProdBasis_Hexahedron2( &
    & p, &
    & q, &
    & r, &
    & x, &
    & y, &
    & z, &
    & basisType1, &
    & basisType2, &
    & basisType3, &
    & alpha1, &
    & beta1, &
    & lambda1, &
    & alpha2, &
    & beta2, &
    & lambda2, &
    & alpha3,  &
    & beta3,  &
    & lambda3) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    INTEGER(I4B), INTENT(IN) :: r
    !! highest order in x3 direction
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: basisType1, basisType2, basisType3
    !! orthogonal polynomial family in x1 direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (p + 1) * (q + 1) * (r + 1))
    !! Tensor basis
    !! The number of rows corresponds to the
    !! total number of points
  END FUNCTION TensorProdBasis_Hexahedron2
END INTERFACE TensorProdBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    VertexBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit hexahedron

INTERFACE VertexBasis_Hexahedron
  MODULE PURE FUNCTION VertexBasis_Hexahedron1(x, y, z) &
     & RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), 8)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Hexahedron1
END INTERFACE VertexBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    VertexBasis_Hexahedron2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Hexahedron2(L1, L2, L3) RESULT(ans)
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! L1 Lobatto polynomial evaluated at x coordinates
    !! L2 is Lobatto polynomial evaluated at y coordinates
    !! L3 is Lobatto polynomial evaluated at z coordinates
    REAL(DFP) :: ans(SIZE(L1, 1), 8)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Hexahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    VertexBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

INTERFACE VertexBasis_Hexahedron
  MODULE PURE FUNCTION VertexBasis_Hexahedron3(xij) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(xij, 2), 8)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Hexahedron3
END INTERFACE VertexBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                     xEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edges parallel to x axis

INTERFACE xEdgeBasis_Hexahedron
  MODULE PURE FUNCTION xEdgeBasis_Hexahedron1(  &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & x, &
    & y,  &
    & z) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans(SIZE(x), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION xEdgeBasis_Hexahedron1
END INTERFACE xEdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE xEdgeBasis_Hexahedron
  MODULE PURE FUNCTION xEdgeBasis_Hexahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION xEdgeBasis_Hexahedron2
END INTERFACE xEdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                     yEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edges parallel to y axis

INTERFACE yEdgeBasis_Hexahedron
  MODULE PURE FUNCTION yEdgeBasis_Hexahedron1(  &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & x, &
    & y,  &
    & z) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans(SIZE(x), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION yEdgeBasis_Hexahedron1
END INTERFACE yEdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE yEdgeBasis_Hexahedron
  MODULE PURE FUNCTION yEdgeBasis_Hexahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION yEdgeBasis_Hexahedron2
END INTERFACE yEdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                     zEdgeBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edges parallel to y axis

INTERFACE zEdgeBasis_Hexahedron
  MODULE PURE FUNCTION zEdgeBasis_Hexahedron1(  &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & x, &
    & y,  &
    & z) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans(SIZE(x), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION zEdgeBasis_Hexahedron1
END INTERFACE zEdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE zEdgeBasis_Hexahedron
  MODULE PURE FUNCTION zEdgeBasis_Hexahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION zEdgeBasis_Hexahedron2
END INTERFACE zEdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                      EdgeBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edges parallel to y axis

INTERFACE EdgeBasis_Hexahedron
  MODULE PURE FUNCTION EdgeBasis_Hexahedron1(  &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & x, &
    & y,  &
    & z, &
    & dim) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim specifies the axis orientation, it can be
    !! dim = 1, means x axis
    !! dim = 2, means y axis
    !! dim = 3, means z axis
    REAL(DFP) :: ans(SIZE(x), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION EdgeBasis_Hexahedron1
END INTERFACE EdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE EdgeBasis_Hexahedron
  MODULE PURE FUNCTION EdgeBasis_Hexahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & L1, &
    & L2, &
    & L3, &
    & dim) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge e1, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge e2, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge e3, it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on edge e4, it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim specifies the axis orientation, it can be
    !! dim = 1, means x axis
    !! dim = 2, means y axis
    !! dim = 3, means z axis
    REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 + pe4 - 4)
  END FUNCTION EdgeBasis_Hexahedron2
END INTERFACE EdgeBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    xyFacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xyFacet

INTERFACE xyFacetBasis_Hexahedron
  MODULE PURE FUNCTION xyFacetBasis_Hexahedron1(  &
    & n1, &
    & n2, &
    & x,   &
    & y,   &
    & z)   &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of xy face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of xy face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans( &
      & SIZE(x), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION xyFacetBasis_Hexahedron1
END INTERFACE xyFacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    xyFacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xyFacet

INTERFACE xyFacetBasis_Hexahedron
  MODULE PURE FUNCTION xyFacetBasis_Hexahedron2( &
    & n1, &
    & n2, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of xy face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of xy face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans( &
      & SIZE(L1, 1), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION xyFacetBasis_Hexahedron2
END INTERFACE xyFacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    yzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on yzFacet

INTERFACE yzFacetBasis_Hexahedron
  MODULE PURE FUNCTION yzFacetBasis_Hexahedron1(  &
    & n1, &
    & n2, &
    & x,   &
    & y,   &
    & z)   &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of yz face
    !! it should be greater than  1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of yz face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans( &
      & SIZE(x), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION yzFacetBasis_Hexahedron1
END INTERFACE yzFacetBasis_Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on yzFacet

INTERFACE yzFacetBasis_Hexahedron
  MODULE PURE FUNCTION yzFacetBasis_Hexahedron2( &
    & n1, &
    & n2, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of yz face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of yz face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans( &
      & SIZE(L1, 1), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION yzFacetBasis_Hexahedron2
END INTERFACE yzFacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    xzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xzFacet

INTERFACE xzFacetBasis_Hexahedron
  MODULE PURE FUNCTION xzFacetBasis_Hexahedron1(  &
    & n1, &
    & n2, &
    & x,   &
    & y,   &
    & z)   &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of xz face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of xz face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans( &
      & SIZE(x), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION xzFacetBasis_Hexahedron1
END INTERFACE xzFacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    xzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xzFacet

INTERFACE xzFacetBasis_Hexahedron
  MODULE PURE FUNCTION xzFacetBasis_Hexahedron2( &
    & n1, &
    & n2, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of xz face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of xz face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans( &
      & SIZE(L1, 1), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION xzFacetBasis_Hexahedron2
END INTERFACE xzFacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                    xzFacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xzFacet

INTERFACE FacetBasis_Hexahedron
  MODULE PURE FUNCTION FacetBasis_Hexahedron1(  &
    & n1, &
    & n2, &
    & x,   &
    & y,   &
    & z,  &
    & dim1,  &
    & dim2)   &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of the face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of the face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    INTEGER(I4B), INTENT(IN) :: dim1
    !! direction in n1 direction
    INTEGER(I4B), INTENT(IN) :: dim2
    !! direction in n2 direction
    REAL(DFP) :: ans( &
      & SIZE(x), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION FacetBasis_Hexahedron1
END INTERFACE FacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                      FacetBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xzFacet

INTERFACE FacetBasis_Hexahedron
  MODULE PURE FUNCTION FacetBasis_Hexahedron2( &
    & n1, &
    & n2, &
    & L1, &
    & L2, &
    & L3, &
    & dim1,  &
    & dim2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1 of xy face
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2 of xy face
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    INTEGER(I4B), INTENT(IN) :: dim1
    !! direction in n1 direction
    INTEGER(I4B), INTENT(IN) :: dim2
    !! direction in n2 direction
    REAL(DFP) :: ans( &
      & SIZE(L1, 1), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * 2_I4B)
  END FUNCTION FacetBasis_Hexahedron2
END INTERFACE FacetBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                       CellBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on Cell

INTERFACE CellBasis_Hexahedron
  MODULE PURE FUNCTION CellBasis_Hexahedron1(  &
    & n1, &
    & n2, &
    & n3, &
    & x,   &
    & y,   &
    & z)   &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n3
    !! order along axis 3
    !! it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:), z(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans( &
      & SIZE(x), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * (n3 - 1_I4B))
  END FUNCTION CellBasis_Hexahedron1
END INTERFACE CellBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                      CellBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on xzCell

INTERFACE CellBasis_Hexahedron
  MODULE PURE FUNCTION CellBasis_Hexahedron2( &
    & n1, &
    & n2, &
    & n3, &
    & L1, &
    & L2, &
    & L3) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1
    !! order along axis 1
    !! it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: n2
    !! order along axis 2
    !! it should be greater than 3
    INTEGER(I4B), INTENT(IN) :: n3
    !! order along axis 3
    !! it should be greater than 3
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:), L3(1:, 0:)
    !! Lobatto polynomials in x, y, and z direction.
    REAL(DFP) :: ans( &
      & SIZE(L1, 1), &
      & (n1 - 1_I4B) * (n2 - 1_I4B) * (n3 - 1_I4B))
  END FUNCTION CellBasis_Hexahedron2
END INTERFACE CellBasis_Hexahedron

!----------------------------------------------------------------------------
!                                               HeirarchicalBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary: Returns the HeirarchicalBasis on Hexahedron

INTERFACE HeirarchicalBasis_Hexahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Hexahedron1(  &
    & pb1, pb2, pb3, &
    & pxy1, pxy2, &
    & pxz1, pxz2, &
    & pyz1, pyz2, &
    & px1, px2, px3, px4, &
    & py1, py2, py3, py4, &
    & pz1, pz2, pz3, pz4, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb1, pb2, pb3
    !! order of interpolation inside the element in x, y, and z dirs
    INTEGER(I4B), INTENT(IN) :: pxy1, pxy2
    !! order of interpolation on facets parallel to xy plane
    INTEGER(I4B), INTENT(IN) :: pxz1, pxz2
    !! order of interpolation on facets parallel to xz plane
    INTEGER(I4B), INTENT(IN) :: pyz1, pyz2
    !! order of interpolation on facets parallel to yz plane
    INTEGER(I4B), INTENT(IN) :: px1, px2, px3, px4
    !! order of interpolation on edges parallel to x-axis
    INTEGER(I4B), INTENT(IN) :: py1, py2, py3, py4
    !! order of interpolation on edges parallel to y-axis
    INTEGER(I4B), INTENT(IN) :: pz1, pz2, pz3, pz4
    !! order of interpolation on edges parallel to z-axis
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(  &
      & SIZE(xij, 2), &
      &   8_I4B &
      & + (pb1 - 1_I4B) * (pb2 - 1_I4B) * (pb3 - 1_I4B) &
      & + (pxy1 - 1_I4B) * (pxy2 - 1_I4B) * 2_I4B  &
      & + (pxz1 - 1_I4B) * (pxz2 - 1_I4B) * 2_I4B  &
      & + (pyz1 - 1_I4B) * (pyz2 - 1_I4B) * 2_I4B  &
      & + (px1 + px2 + px3 + px4 - 4_I4B) &
      & + (py1 + py2 + py3 + py4 - 4_I4B) &
      & + (pz1 + pz2 + pz3 + pz4 - 4_I4B) &
      & )
    !!
  END FUNCTION HeirarchicalBasis_Hexahedron1
END INTERFACE HeirarchicalBasis_Hexahedron

!----------------------------------------------------------------------------
!                                               HeirarchicalBasis_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary: Returns the HeirarchicalBasis on Hexahedron

INTERFACE HeirarchicalBasis_Hexahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Hexahedron2(  &
    & p, q, r, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! order of interpolation in x, y, and z dirs
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(  &
      & SIZE(xij, 2), &
      &   8_I4B &
      & + (p - 1_I4B) * (q - 1_I4B) * (r - 1_I4B) &
      & + (p - 1_I4B) * (q - 1_I4B) * 2_I4B  &
      & + (p - 1_I4B) * (r - 1_I4B) * 2_I4B  &
      & + (q - 1_I4B) * (r - 1_I4B) * 2_I4B  &
      & + (4_I4B * p - 4_I4B) &
      & + (4_I4B * q - 4_I4B) &
      & + (4_I4B * r - 4_I4B) &
      & )
    !!
  END FUNCTION HeirarchicalBasis_Hexahedron2
END INTERFACE HeirarchicalBasis_Hexahedron

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-19
! summary:  Returns quadrature points on reference hexahedron

INTERFACE QuadraturePoint_Hexahedron
  MODULE FUNCTION QuadraturePoint_Hexahedron1( &
    & order, &
    & quadType, &
    & xij, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand in x, y, and z direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussLegendreRadauLeft
    !! GaussLegendreRadauRight
    !! GaussChebyshev1
    !! GaussChebyshev1Lobatto
    !! GaussChebyshev1RadauLeft
    !! GaussChebyshev1RadauRight
    !! GaussUltraspherical
    !! GaussUltrasphericalLobatto
    !! GaussUltrasphericalRadauLeft
    !! GaussUltrasphericalRadauRight
    !! GaussJacobi
    !! GaussJacobiLobatto
    !! GaussJacobiRadauLeft
    !! GaussJacobiRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordiantes of hexahedron in xij format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points in xij format
  END FUNCTION QuadraturePoint_Hexahedron1
END INTERFACE QuadraturePoint_Hexahedron

!----------------------------------------------------------------------------
!                                                QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

INTERFACE QuadraturePoint_Hexahedron
  MODULE FUNCTION QuadraturePoint_Hexahedron2(  &
    & p, q, r, &
    & quadType1, quadType2, quadType3, &
    & xij, &
    & alpha1, beta1, lambda1, &
    & alpha2, beta2, lambda2, &
    & alpha3, beta3, lambda3 &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of  integrand in y direction
    INTEGER(I4B), INTENT(IN) :: r
    !! order of  integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2, quadType3
    !! quadrature point type in x direction
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussLegendreRadauLeft
    !! GaussLegendreRadauRight
    !! GaussChebyshev1
    !! GaussChebyshev1Lobatto
    !! GaussChebyshev1RadauLeft
    !! GaussChebyshev1RadauRight
    !! GaussUltraspherical
    !! GaussUltrasphericalLobatto
    !! GaussUltrasphericalRadauLeft
    !! GaussUltrasphericalRadauRight
    !! GaussJacobi
    !! GaussJacobiLobatto
    !! GaussJacobiRadauLeft
    !! GaussJacobiRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi parameter and Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION QuadraturePoint_Hexahedron2
END INTERFACE QuadraturePoint_Hexahedron

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-19
! summary:  Returns quadrature points on reference quadrangle

INTERFACE QuadraturePoint_Hexahedron
  MODULE FUNCTION QuadraturePoint_Hexahedron3(nips, quadType, &
    & xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of integration points in x, y, and z direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! interpolation point type
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussLegendreRadauLeft
    !! GaussLegendreRadauRight
    !! GaussChebyshev1
    !! GaussChebyshev1Lobatto
    !! GaussChebyshev1RadauLeft
    !! GaussChebyshev1RadauRight
    !! GaussUltraspherical
    !! GaussUltrasphericalLobatto
    !! GaussUltrasphericalRadauLeft
    !! GaussUltrasphericalRadauRight
    !! GaussJacobi
    !! GaussJacobiLobatto
    !! GaussJacobiRadauLeft
    !! GaussJacobiRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION QuadraturePoint_Hexahedron3
END INTERFACE QuadraturePoint_Hexahedron

!----------------------------------------------------------------------------
!                                                QuadraturePoint_Hexahedron
!----------------------------------------------------------------------------

INTERFACE QuadraturePoint_Hexahedron
  MODULE FUNCTION QuadraturePoint_Hexahedron4(  &
    & nipsx, nipsy, nipsz, &
    & quadType1, quadType2, quadType3, &
    & xij, &
    & alpha1, beta1, lambda1, &
    & alpha2, beta2, lambda2, &
    & alpha3, beta3, lambda3 &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! order of integrand in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! order of  integrand in y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! order of  integrand in z direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2, quadType3
    !! quadrature point type in x, y, and z direction
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussLegendreRadauLeft
    !! GaussLegendreRadauRight
    !! GaussChebyshev1
    !! GaussChebyshev1Lobatto
    !! GaussChebyshev1RadauLeft
    !! GaussChebyshev1RadauRight
    !! GaussUltraspherical
    !! GaussUltrasphericalLobatto
    !! GaussUltrasphericalRadauLeft
    !! GaussUltrasphericalRadauRight
    !! GaussJacobi
    !! GaussJacobiLobatto
    !! GaussJacobiRadauLeft
    !! GaussJacobiRadauRight
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1, beta1, lambda1
    !! Jacobi and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2, beta2, lambda2
    !! Jacobi and Ultraspherical parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha3, beta3, lambda3
    !! Jacobi and Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION QuadraturePoint_Hexahedron4
END INTERFACE QuadraturePoint_Hexahedron

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  Evaluate all Lagrange polynomials at several points

INTERFACE LagrangeEvalAll_Hexahedron
  MODULE FUNCTION LagrangeEvalAll_Hexahedron1( &
    & order, &
    & x, &
    & xij, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(3)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    !! x(3) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    !! The number of rows in xij is 3
    !! The number of columns in xij should be equal to total
    !! degree of freedom
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be computed and returned
    !! by this routine.
    !! If firstCall is False, then coeff should be given, which will be
    !! used.
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre
    !! Lobatto
    !! Chebyshev
    !! Jacobi
    !! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Hexahedron1
END INTERFACE LagrangeEvalAll_Hexahedron

!----------------------------------------------------------------------------
!                                                LagrangeEvalAll_Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  Evaluate all Lagrange polynomials at several points

INTERFACE LagrangeEvalAll_Hexahedron
  MODULE FUNCTION LagrangeEvalAll_Hexahedron2( &
    & order, &
    & x, &
    & xij, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Hexahedron2
END INTERFACE LagrangeEvalAll_Hexahedron

END MODULE HexahedronInterpolationUtility
