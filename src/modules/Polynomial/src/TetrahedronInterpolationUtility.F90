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
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

PUBLIC :: LagrangeDegree_Tetrahedron
PUBLIC :: LagrangeDOF_Tetrahedron
PUBLIC :: LagrangeInDOF_Tetrahedron
PUBLIC :: EquidistanceInPoint_Tetrahedron
PUBLIC :: EquidistancePoint_Tetrahedron
PUBLIC :: LagrangeCoeff_Tetrahedron
PUBLIC :: Isaac_Tetrahedron
PUBLIC :: BlythPozrikidis_Tetrahedron
PUBLIC :: InterpolationPoint_Tetrahedron
PUBLIC :: OrthogonalBasis_Tetrahedron
PUBLIC :: BarycentricVertexBasis_Tetrahedron
PUBLIC :: BarycentricEdgeBasis_Tetrahedron
PUBLIC :: BarycentricFacetBasis_Tetrahedron
PUBLIC :: BarycentricCellBasis_Tetrahedron
PUBLIC :: BarycentricHeirarchicalBasis_Tetrahedron
PUBLIC :: VertexBasis_Tetrahedron
PUBLIC :: EdgeBasis_Tetrahedron
PUBLIC :: FacetBasis_Tetrahedron
PUBLIC :: CellBasis_Tetrahedron
PUBLIC :: HeirarchicalBasis_Tetrahedron
PUBLIC :: FacetConnectivity_Tetrahedron
PUBLIC :: EdgeConnectivity_Tetrahedron
PUBLIC :: GetVertexDOF_Tetrahedron
PUBLIC :: GetEdgeDOF_Tetrahedron
PUBLIC :: GetFacetDOF_Tetrahedron
PUBLIC :: GetCellDOF_Tetrahedron
PUBLIC :: LagrangeEvalAll_Tetrahedron
PUBLIC :: QuadraturePoint_Tetrahedron
PUBLIC :: RefElemDomain_Tetrahedron
PUBLIC :: LagrangeGradientEvalAll_Tetrahedron
PUBLIC :: HeirarchicalBasisGradient_Tetrahedron
PUBLIC :: OrthogonalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                                 RefElemDomain_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE FUNCTION RefElemDomain_Tetrahedron(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetVertexDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-24
! summary:  returns total number of vertex degrees of freedom

INTERFACE
  MODULE PURE FUNCTION GetVertexDOF_Tetrahedron() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION GetVertexDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-24
! summary:  returns total number of degrees of freedom on edges parallel to
! some axis

INTERFACE GetEdgeDOF_Tetrahedron
  MODULE PURE FUNCTION GetEdgeDOF_Tetrahedron1(pe1, pe2, pe3, &
    & pe4, pe5, pe6) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1, pe2, pe3, pe4, pe5, pe6
    !! Order of interpolation in x or y or z direction
    INTEGER(I4B) :: ans
  END FUNCTION GetEdgeDOF_Tetrahedron1
END INTERFACE GetEdgeDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary:  Returns total number of degrees of freedom on all edges

INTERFACE GetEdgeDOF_Tetrahedron
  MODULE PURE FUNCTION GetEdgeDOF_Tetrahedron2(p)  &
  & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! Order of approximation  on all edges
    INTEGER(I4B) :: ans
  END FUNCTION GetEdgeDOF_Tetrahedron2
END INTERFACE GetEdgeDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                    GetFacetDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns dof on all facets

INTERFACE GetFacetDOF_Tetrahedron
  MODULE PURE FUNCTION GetFacetDOF_Tetrahedron1( &
    & ps1, ps2, &
    & ps3, ps4) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! orders alongs facets parallel to xy plane
    INTEGER(I4B), INTENT(IN) :: ps2
    !! orders along facets parallel to xz plane
    INTEGER(I4B), INTENT(IN) :: ps3
    !! orders along facets parallel to yz plane
    INTEGER(I4B), INTENT(IN) :: ps4
    !! orders along facets parallel to xyz plane
    INTEGER(I4B) :: ans
  END FUNCTION GetFacetDOF_Tetrahedron1
END INTERFACE GetFacetDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns total degrees of freedom on all facets

INTERFACE GetFacetDOF_Tetrahedron
  MODULE PURE FUNCTION GetFacetDOF_Tetrahedron2(p) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! orders alongs facets parallel to xy or xz or yz planes
    INTEGER(I4B) :: ans
  END FUNCTION GetFacetDOF_Tetrahedron2
END INTERFACE GetFacetDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                   GetFacetDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-24
! summary: Returns the number of cell degree of freedom

INTERFACE GetCellDOF_Tetrahedron
  MODULE PURE FUNCTION GetCellDOF_Tetrahedron1(p) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! orders alongs to x, y, and z directions
    INTEGER(I4B) :: ans
  END FUNCTION GetCellDOF_Tetrahedron1
END INTERFACE GetCellDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-10
! summary:  This function returns the facet-connectivity of Tetrahedron

INTERFACE
  MODULE PURE FUNCTION FacetConnectivity_Tetrahedron( &
    & baseInterpol, &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    CHARACTER(*), INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(3, 4)
  END FUNCTION FacetConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-07
! summary:  This function returns the edge connectivity of Tetrahedron

INTERFACE
  MODULE PURE FUNCTION EdgeConnectivity_Tetrahedron( &
    & baseInterpol,  &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    CHARACTER(*), INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2, 6)
  END FUNCTION EdgeConnectivity_Tetrahedron
END INTERFACE

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
  MODULE FUNCTION EquidistanceInPoint_Tetrahedron_old(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Tetrahedron_old
END INTERFACE

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points strictly in Tetrahedron
!
!# Introduction
!
!- This function returns the equidistance points in Tetrahedron
!- All points are inside the Tetrahedron

INTERFACE
  MODULE FUNCTION EquidistanceInPoint_Tetrahedron(order, xij) &
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
  MODULE FUNCTION EquidistancePoint_Tetrahedron(order, xij) RESULT(ans)
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
  MODULE RECURSIVE FUNCTION EquidistancePoint_Tetrahedron_old( &
    & order, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Tetrahedron_old
END INTERFACE

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Interpolation point

INTERFACE
  MODULE FUNCTION InterpolationPoint_Tetrahedron( &
    & order, &
    & ipType, &
    & layout, &
    & xij, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation type
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussChebyshev,
    !! GaussChebyshevLobatto
    !! GaussJacobi
    !! GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC", "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(3, 4)
    !! coordinates of vertices in $x_{iJ}$ format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in $x_{iJ}$ format
  END FUNCTION InterpolationPoint_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Tetrahedron1
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron2(order, i, v, isVandermonde) &
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
  END FUNCTION LagrangeCoeff_Tetrahedron2
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron3(order, i, v, ipiv) RESULT(ans)
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
  END FUNCTION LagrangeCoeff_Tetrahedron3
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron4( &
    & order, &
    & xij, &
    & basisType, &
    & refTetrahedron, &
    & alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials
    !! Jacobi (Dubiner)
    !! Heirarchical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT * default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Tetrahedron4
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                            Isaac_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Isaac points on triangle
!
!# Introduction
!
! https://tisaac.gitlab.io/recursivenodes/

INTERFACE
  MODULE FUNCTION Isaac_Tetrahedron(order, ipType, layout, xij,  &
  & alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussChebyshev,
    !! GaussChebyshevLobatto
    !! GaussJacobi
    !! GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of Tetrahedron
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! only layout = "VEFC" is allowed
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! xij coordinates
  END FUNCTION Isaac_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                BlythPozrikidis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Blyth Pozrikidis nodes on triangle
!
!# Introduction
!
! M. G. Blyth and C. Pozrikidis.
! A lobatto interpolation grid over the Tetrahedron.
! IMA Journal of Applied Mathematics, Feb 2006.

INTERFACE
  MODULE FUNCTION BlythPozrikidis_Tetrahedron(order, ipType, layout, xij,  &
    & alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussChebyshev,
    !! GaussChebyshevLobatto
    !! GaussJacobi
    !! GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! xij coordinates
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! only layout = "VEFC" is allowed
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! xij coordinates
  END FUNCTION BlythPozrikidis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                         IJ2VEFC_Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE SUBROUTINE IJK2VEFC_Tetrahedron( &
    & xi, &
    & eta, &
    & zeta, &
    & temp, &
    & order, &
    & N)
    REAL(DFP), INTENT(IN) :: xi(:, :, :)
    REAL(DFP), INTENT(IN) :: eta(:, :, :)
    REAL(DFP), INTENT(IN) :: zeta(:, :, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: N
  END SUBROUTINE IJK2VEFC_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Orthogongal basis on Tetrahedron

INTERFACE OrthogonalBasis_Tetrahedron
  MODULE FUNCTION OrthogonalBasis_Tetrahedron1( &
    & order, &
    & xij, &
    & refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in reference Tetrahedron.
    !! The shape functions will be evaluated
    !! at these points.
    !! the SIZE(xij,1) = 3, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION OrthogonalBasis_Tetrahedron1
END INTERFACE OrthogonalBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                                OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Orthogongal basis on Tetrahedron

INTERFACE OrthogonalBasis_Tetrahedron
  MODULE FUNCTION OrthogonalBasis_Tetrahedron2( &
    & order, &
    & x, y, z, refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates, total points = SIZE(x)*SIZE(y)*SIZE(z)
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinates, total points = SIZE(x)*SIZE(y)*SIZE(z)
    REAL(DFP), INTENT(IN) :: z(:)
    !! z coordinates, total points = SIZE(x)*SIZE(y)*SIZE(z)
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP) :: ans( &
      & SIZE(x) * SIZE(y) * SIZE(z), &
      & (order + 1) * (order + 2) * (order + 3) / 6)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION OrthogonalBasis_Tetrahedron2
END INTERFACE OrthogonalBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on reference Tetrahedron

INTERFACE
  MODULE PURE FUNCTION BarycentricVertexBasis_Tetrahedron(lambda) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentrix coords
    !! number of rows = 4
    !! number of columns = number of points
    REAL(DFP) :: ans(SIZE(lambda, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION BarycentricVertexBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Gradient of vertex basis in terms of barycentric coord

INTERFACE
  MODULE PURE FUNCTION BarycentricVertexBasisGradient_Tetrahedron(lambda) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentrix coords
    !! number of rows = 4
    !! number of columns = number of points
    REAL(DFP) :: ans(SIZE(lambda, 2), 4, 4)
    !! - ans(:,:,i) denotes gradient wrt $\lambda_{i}$
    !! - index1: point of evaluation
    !! - index2: vertex basis number
    !! - index3: gradient
  END FUNCTION BarycentricVertexBasisGradient_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricEdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edge of triangle
!
!# Introduction
!
! Evaluate basis functions on edges of triangle
! pe1, pe2, pe3 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Tetrahedron( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & lambda  &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on  edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on  edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on  edge parallel to yz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)
  END FUNCTION BarycentricEdgeBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                           BarycentricEdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Oct 2022
! summary: Evaluate the edge basis on Tetrahedron in terms of barycentric

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Tetrahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & lambda, &
    & phi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on  edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on  edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on  edge parallel to yz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! size(lambda,1) = 4
    !! size(lambda,2) = number of points of evaluation
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
    !! (lambda3-lambda1), (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)
  END FUNCTION BarycentricEdgeBasis_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                 BarycentricEdgeBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Oct 2022
! summary: Eval grad of the basis in terms of barycentric coord

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasisGradient_Tetrahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & lambda, &
    & phi,  &
    & dphi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on  edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on  edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on  edge parallel to yz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! size(lambda,1) = 4
    !! size(lambda,2) = number of points of evaluation
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 6*number of points
    !! - (lambda2-lambda1)
    !! - (lambda3-lambda1)
    !! - (lambda4-lambda1)
    !! - (lambda3-lambda2)
    !! - (lambda4-lambda2)
    !! - (lambda4-lambda3)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
    REAL(DFP), INTENT(IN) :: dphi(1:, 0:)
    !! gradient of lobatto kernel
    !! size(phi1, 1) = 3*number of points
    !! - (lambda2-lambda1),
    !! - (lambda3-lambda1),
    !! - (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6, 4)
    !! - ans(:,:,i) denotes gradient wrt $\lambda_{i}$
    !! - index1: point of evaluation
    !! - index2: vertex basis number
    !! - index3: gradient
  END FUNCTION BarycentricEdgeBasisGradient_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricFacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on facet of triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricFacetBasis_Tetrahedron( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda  &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on  facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on  facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on  facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on  facet parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2)
  END FUNCTION BarycentricFacetBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricFacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on facet of triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricFacetBasis_Tetrahedron2( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda,  &
    & phi &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on  edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on  edge parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
    !! (lambda3-lambda1), (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2)
  END FUNCTION BarycentricFacetBasis_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                 BarycentricFacetBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval gradient of facet-basis in terms of barycentric

INTERFACE
  MODULE PURE FUNCTION BarycentricFacetBasisGradient_Tetrahedron2( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda,  &
    & phi, &
    & dphi &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on  edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on  edge parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 6*number of points
    !! - (lambda2-lambda1)
    !! - (lambda3-lambda1)
    !! - (lambda4-lambda1)
    !! - (lambda3-lambda2)
    !! - (lambda4-lambda2)
    !! - (lambda4-lambda3)
    REAL(DFP), INTENT(IN) :: dphi(1:, 0:)
    !! gradient of lobatto kernel values
    !! size(phi1, 1) = 6*number of points
    !! - (lambda2-lambda1)
    !! - (lambda3-lambda1)
    !! - (lambda4-lambda1)
    !! - (lambda3-lambda2)
    !! - (lambda4-lambda2)
    !! - (lambda4-lambda3)
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2, 4)
    !! - ans(:,:,i) denotes gradient wrt $\lambda_{i}$
    !! - index1: point of evaluation
    !! - index2: vertex basis number
    !! - index3: gradient
  END FUNCTION BarycentricFacetBasisGradient_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricCellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on cell of triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasis_Tetrahedron( &
    & pb, lambda  &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on  facet parallel to xy
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B)
  END FUNCTION BarycentricCellBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricCellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Evaluate cellbasis function in terms of barycentric coord

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasis_Tetrahedron2( &
    & pb, lambda, phi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on  facet parallel to xy
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Value of lobatto kernel values
    !! size(phi1, 1) = 6*number of points
    !! - (lambda2-lambda1)
    !! - (lambda3-lambda1)
    !! - (lambda4-lambda1)
    !! - (lambda3-lambda2)
    !! - (lambda4-lambda2)
    !! - (lambda4-lambda3)
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B)
  END FUNCTION BarycentricCellBasis_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                   BarycentricCellBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-25
! summary: Gradient of cellbasis function in terms of barycentric coord

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasisGradient_Tetrahedron2( &
    & pb, lambda, phi, dphi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on  facet parallel to xy
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Value of lobatto kernel values
    !! size(phi1, 1) = 6*number of points
    !! - (lambda2-lambda1)
    !! - (lambda3-lambda1)
    !! - (lambda4-lambda1)
    !! - (lambda3-lambda2)
    !! - (lambda4-lambda2)
    !! - (lambda4-lambda3)
    REAL(DFP), INTENT(IN) :: dphi(1:, 0:)
    !! Gradient of lobatto kernel values
    !! size(phi1, 1) = 6*number of points
    !! - (lambda2-lambda1)
    !! - (lambda3-lambda1)
    !! - (lambda4-lambda1)
    !! - (lambda3-lambda2)
    !! - (lambda4-lambda2)
    !! - (lambda4-lambda3)
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B, 4)
    !! - ans(:,:,i) denotes gradient wrt $\lambda_{i}$
    !! - index1: point of evaluation
    !! - index2: vertex basis number
    !! - index3: gradient
  END FUNCTION BarycentricCellBasisGradient_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                  BarycentricHeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Tetrahedron

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasis_Tetrahedron1( &
    & order, &
    & pe1,  &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of interpolation on edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of interpolation on edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of interpolation on facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of interpolation on facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of interpolation on facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of interpolation on facet parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 4
    !! number of cols = number of points
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B)
  END FUNCTION BarycentricHeirarchicalBasis_Tetrahedron1
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!                         BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Gradient of heirarchical basis in terms of barycentric coord

INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron1( &
    & order, &
    & pe1,  &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of interpolation on edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of interpolation on edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of interpolation on facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of interpolation on facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of interpolation on facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of interpolation on facet parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 4
    !! number of cols = number of points
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B, 4_I4B)
  END FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron1
END INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-25
! summary:  Evaluate heirarchical basis in terms of barycentric coord

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasis_Tetrahedron2( &
    & order, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 4
    !! number of cols = number of points
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B)
  END FUNCTION BarycentricHeirarchicalBasis_Tetrahedron2
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!                        BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-25
! summary: Gradient of heirarchical basis in terms of barycentric coord

INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron2( &
    & order, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Point of evaluation in terms of barycentric coord
    !! Barycenteric coordinates
    !! number of rows = 4
    !! number of cols = number of points
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B, 4)
  END FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron2
END INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                                   VertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on  Tetrahedron

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Tetrahedron(xij, refTetrahedron) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! Unit or biunit
    REAL(DFP) :: ans(SIZE(xij, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     EdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the edge basis functions on Tetrahedron

INTERFACE
  MODULE PURE FUNCTION EdgeBasis_Tetrahedron( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on  edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on  edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on  edge parallel to yz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)
  END FUNCTION EdgeBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     FacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the edge basis functions on Tetrahedron

INTERFACE
  MODULE PURE FUNCTION FacetBasis_Tetrahedron( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on facet to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on facet to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on facet to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on facet to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2)
  END FUNCTION FacetBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     CellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the cell basis functions on Tetrahedron

INTERFACE
  MODULE PURE FUNCTION CellBasis_Tetrahedron( &
    & pb, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order in cell
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B)
  END FUNCTION CellBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the heirarchical basis functions on Tetrahedron

INTERFACE HeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Tetrahedron1( &
    & order, &
    & pe1,  &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of interpolation on edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of interpolation on edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of interpolation on facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of interpolation on facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of interpolation on facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of interpolation on facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B)
  END FUNCTION HeirarchicalBasis_Tetrahedron1
END INTERFACE HeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the heirarchical basis functions on Tetrahedron

INTERFACE HeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Tetrahedron2( &
    & order, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B)
  END FUNCTION HeirarchicalBasis_Tetrahedron2
END INTERFACE HeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  Evaluate all Lagrange polynomials at several points

INTERFACE LagrangeEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeEvalAll_Tetrahedron1( &
    & order, &
    & x, &
    & xij, &
    & refTetrahedron, &
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT *default
    !! BIUNIT
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
    !! Orthogonal
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Tetrahedron1
END INTERFACE LagrangeEvalAll_Tetrahedron

!----------------------------------------------------------------------------
!                                                LagrangeEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  Evaluate all Lagrange polynomials at several points

INTERFACE LagrangeEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeEvalAll_Tetrahedron2( &
    & order, &
    & x, &
    & xij, &
    & refTetrahedron, &
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT *default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre
    !! Lobatto
    !! Chebyshev
    !! Jacobi
    !! Ultraspherical
    !! Heirarchical
    !! Orthogonal
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Tetrahedron2
END INTERFACE LagrangeEvalAll_Tetrahedron

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  quadrature points on Tetrahedron

INTERFACE QuadraturePoint_Tetrahedron
  MODULE FUNCTION QuadraturePoint_Tetrahedron1(&
    & order, &
    & quadType, &
    & refTetrahedron, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    !! If xij is present then this argument is ignored
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be  3.
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION QuadraturePoint_Tetrahedron1
END INTERFACE QuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  quadrature points on Tetrahedron

INTERFACE QuadraturePoint_Tetrahedron
  MODULE FUNCTION QuadraturePoint_Tetrahedron2(&
    & nips, &
    & quadType, &
    & refTetrahedron, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! nips(1) .LE. 79, then we call
    !! economical quadrature rules.
    !! Otherwise, this routine will retport
    !! error
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type,
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    !! If xij is present then this argument is ignored
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be 3
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION QuadraturePoint_Tetrahedron2
END INTERFACE QuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor based quadrature points on Tetrahedron

INTERFACE TensorQuadraturePoint_Tetrahedron
  MODULE FUNCTION TensorQuadraturePoint_Tetrahedron1(order, quadType, &
    & refTetrahedron, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 4.
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Tetrahedron1
END INTERFACE TensorQuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor based quadrature points

INTERFACE TensorQuadraturePoint_Tetrahedron
  MODULE FUNCTION TensorQuadraturePoint_Tetrahedron2( &
    & nipsx, &
    & nipsy, &
    & nipsz, &
    & quadType, &
    & refTetrahedron, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in z direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be 3
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Tetrahedron2
END INTERFACE TensorQuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  GradientEvaluate all Lagrange polynomials at several points

INTERFACE LagrangeGradientEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeGradientEvalAll_Tetrahedron1( &
    & order, &
    & x, &
    & xij, &
    & refTetrahedron, &
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT *default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre
    !! Lobatto
    !! Chebyshev
    !! Jacobi
    !! Ultraspherical
    !! Heirarchical
    !! Orthogonal
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 3)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
  END FUNCTION LagrangeGradientEvalAll_Tetrahedron1
END INTERFACE LagrangeGradientEvalAll_Tetrahedron

!----------------------------------------------------------------------------
!                                       OrthogonalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Orthogongal basis on Tetrahedron

INTERFACE OrthogonalBasisGradient_Tetrahedron
  MODULE FUNCTION OrthogonalBasisGradient_Tetrahedron1( &
    & order, &
    & xij, &
    & refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in reference Tetrahedron.
    !! The shape functions will be evaluated
    !! at these points.
    !! the SIZE(xij,1) = 3, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6, 3)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION OrthogonalBasisGradient_Tetrahedron1
END INTERFACE OrthogonalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                    HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the heirarchical basis functions on Tetrahedron

INTERFACE HeirarchicalBasisGradient_Tetrahedron
  MODULE FUNCTION HeirarchicalBasisGradient_Tetrahedron1( &
    & order, &
    & pe1,  &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of interpolation on edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of interpolation on edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of interpolation on facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of interpolation on facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of interpolation on facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of interpolation on facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B, 3)
  END FUNCTION HeirarchicalBasisGradient_Tetrahedron1
END INTERFACE HeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                     HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the heirarchical basis functions on Tetrahedron

INTERFACE HeirarchicalBasisGradient_Tetrahedron
  MODULE FUNCTION HeirarchicalBasisGradient_Tetrahedron2( &
    & order, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B, 3)
  END FUNCTION HeirarchicalBasisGradient_Tetrahedron2
END INTERFACE HeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                                                 Tetrahedron
!----------------------------------------------------------------------------

END MODULE TetrahedronInterpolationUtility
