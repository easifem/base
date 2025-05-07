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
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Triangle
PUBLIC :: LagrangeDOF_Triangle
PUBLIC :: LagrangeInDOF_Triangle
PUBLIC :: EquidistanceInPoint_Triangle

PUBLIC :: EquidistancePoint_Triangle
PUBLIC :: EquidistancePoint_Triangle_

PUBLIC :: InterpolationPoint_Triangle
PUBLIC :: InterpolationPoint_Triangle_
PUBLIC :: LagrangeCoeff_Triangle
PUBLIC :: LagrangeCoeff_Triangle_

PUBLIC :: Dubiner_Triangle
PUBLIC :: Dubiner_Triangle_

PUBLIC :: OrthogonalBasis_Triangle
PUBLIC :: OrthogonalBasis_Triangle_

PUBLIC :: OrthogonalBasisGradient_Triangle
PUBLIC :: OrthogonalBasisGradient_Triangle_

PUBLIC :: VertexBasis_Triangle
PUBLIC :: EdgeBasis_Triangle
PUBLIC :: CellBasis_Triangle

PUBLIC :: HeirarchicalBasis_Triangle
PUBLIC :: HeirarchicalBasis_Triangle_

PUBLIC :: HeirarchicalBasisGradient_Triangle
PUBLIC :: HeirarchicalBasisGradient_Triangle_

PUBLIC :: LagrangeEvalAll_Triangle
PUBLIC :: LagrangeEvalAll_Triangle_

PUBLIC :: LagrangeGradientEvalAll_Triangle
PUBLIC :: LagrangeGradientEvalAll_Triangle_

PUBLIC :: QuadratureNumber_Triangle
PUBLIC :: QuadraturePoint_Triangle
PUBLIC :: QuadraturePoint_Triangle_

PUBLIC :: IJ2VEFC_Triangle
PUBLIC :: FacetConnectivity_Triangle
PUBLIC :: RefElemDomain_Triangle

PUBLIC :: GetTotalDOF_Triangle
PUBLIC :: GetTotalInDOF_Triangle

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on Triangle

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Triangle(order, baseContinuity, &
                                            baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Triangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Triangle
!- These dof are strictly inside the Triangle

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Triangle(order, baseContinuity, &
                                              baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalInDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the name of the reference element domain

INTERFACE
  MODULE FUNCTION RefElemDomain_Triangle(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
    !! UNIT or BIUNIT
  END FUNCTION RefElemDomain_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-10
! summary:  This function returns the edge connectivity of Triangle

INTERFACE
  MODULE FUNCTION FacetConnectivity_Triangle( &
    & baseInterpol, &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    CHARACTER(*), INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2, 3)
    !! rows represents the end points of an edges
    !! columns denote the edge (facet)
  END FUNCTION FacetConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         IJ2VEFC_Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE IJ2VEFC_Triangle(xi, eta, temp, order, N)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    REAL(DFP), INTENT(IN) :: eta(:, :)
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: N
  END SUBROUTINE IJ2VEFC_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! number of rows = LagrangeDOf_Triangle(order)
    !! number of cols = 2
  END FUNCTION LagrangeDegree_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE SUBROUTINE LagrangeDegree_Triangle_(order, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! number of rows = LagrangeDOf_Triangle(order)
    !! number of cols = 2
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeDegree_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on triangle

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of triangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of triangle
!- These dof are strictly inside the triangle

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points in triangle
!
!# Introduction
!
!- This function returns the equidistance points in triangle
!- All points are inside the triangle

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Triangle(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
    !! If xij is present then number of rows in ans is same as xij
    !! If xij is not present then number of rows in ans is 2.
  END FUNCTION EquidistanceInPoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points in triangle
!
!# Introduction
!
!- This function returns the equidistance points in triangle
!- All points are inside the triangle

INTERFACE
  MODULE PURE SUBROUTINE EquidistanceInPoint_Triangle_(order, ans, nrow, &
                                                       ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
    !! If xij is present then number of rows in ans is same as xij
    !! If xij is not present then number of rows in ans is 2.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 3
  END SUBROUTINE EquidistanceInPoint_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order triangle element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! triangle element, the layout is always "VEFC"
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention, VEFC.

INTERFACE
  MODULE RECURSIVE PURE FUNCTION EquidistancePoint_Triangle(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE EquidistancePoint_Triangle_(order, ans, &
                                                              nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 3
  END SUBROUTINE EquidistancePoint_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Blyth Pozrikidis nodes on triangle
!
!# Introduction
!
! M. G. Blyth and C. Pozrikidis.
! A lobatto interpolation grid over the triangle.
! IMA Journal of Applied Mathematics, 71(1):153–169, Feb 2006.
! URL: http://dx.doi.org/10.1093/imamat/hxh077,
! doi:10.1093/imamat/hxh077.

INTERFACE
  MODULE FUNCTION BlythPozrikidis_Triangle(order, ipType, layout, xij, &
                                           alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
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
  END FUNCTION BlythPozrikidis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 BlythPozrikidis_Triangle_
!----------------------------------------------------------------------------

INTERFACE
 MODULE SUBROUTINE BlythPozrikidis_Triangle_(order, ipType, ans, nrow, ncol, &
                                             layout, xij, alpha, beta, lambda)

    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
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
  END SUBROUTINE BlythPozrikidis_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Isaac points on triangle

INTERFACE
  MODULE FUNCTION Isaac_Triangle(order, ipType, layout, xij, &
   alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
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
  END FUNCTION Isaac_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Isaac_Triangle_(order, ipType, ans, nrow, ncol, &
                                    layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
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
  END SUBROUTINE Isaac_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                InterpolationPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary: Interpolation points on triangle
!
!# Introduction
!
!- This routine returns the interplation points on triangle.
!- `xij` contains nodal coordinates of triangle in xij format.
!- SIZE(xij,1) = nsd, and SIZE(xij,2)=3
!- If xij is absent then unit triangle is assumed
!- `ipType` is interpolation point type, it can take following values
!-  `Equidistance`, uniformly/evenly distributed points
!- `GaussLegendreLobatto ---> IsaacLegendre
!- `GaussChebyshevLobatto ---> IsaacChebyshev
!- `ChenBabuska`
!- `Hesthaven`
!- `Feket`
!- `BlythPozChebyshev`
!- `BlythPozLegendre`
!- `IsaacChebyshev`
!- `IsaacLegendre`
!
!- `layout` specifies the arrangement of points. The nodes are always
! returned in VEFC format (vertex, edge, face, cell). 1:3 are are
! vertex points, then edge, and then internal nodes. The internal nodes
! also follow the same convention. Please read Gmsh manual  on this topic.

INTERFACE
  MODULE FUNCTION InterpolationPoint_Triangle(order, ipType, &
                                 layout, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Coord of domain in xij format
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout, always VEFC
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! xij coordinates
  END FUNCTION InterpolationPoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle_
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE InterpolationPoint_Triangle_(order, ipType, ans, nrow, &
                                       ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! xij coordinates
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Coord of domain in xij format
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout, always VEFC
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE InterpolationPoint_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Triangle1
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Triangle_
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle1_(order, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Triangle1_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle2(order, i, v, isVandermonde) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue, the value of isVandermonde
    !! is not used in the function
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients of ith Lagrange polynomial
  END FUNCTION LagrangeCoeff_Triangle2
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle_
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle2_(order, i, v, isVandermonde, &
                                             ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue, the value of isVandermonde
    !! is not used in thesubroutine _
    REAL(DFP), INTENT(INOUT) :: ans(:)
    ! ans(SIZE(v, 1))
    !! coefficients of ith Lagrange polynomial
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Triangle2_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle3(order, i, v, ipiv) RESULT(ans)
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
  END FUNCTION LagrangeCoeff_Triangle3
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle3_(order, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Triangle3_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle4(order, xij, basisType, &
                                          refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials
    !! Jacobi (Dubiner)
    !! Heirarchical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTriangle
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Triangle4
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle4_(order, xij, basisType, &
                                             refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomials
    !! Jacobi (Dubiner)
    !! Heirarchical
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! UNIT
    !! BIUNIT
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Triangle4_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit.
!
! The shape of `ans` is (M,N), where M=SIZE(xij,2) (number of points)
! N = 0.5*(order+1)*(order+2).
!
! In this way, ans(j,:) denotes the values of all polynomial at jth point
!
! Polynomials are returned in following way:
!
!$$
! P_{0,0}, P_{0,1}, \cdots , P_{0,order} \\
! P_{1,0}, P_{1,1}, \cdots , P_{1,order-1} \\
! P_{2,0}, P_{2,1}, \cdots , P_{2,order-2} \\
! \cdots
! P_{order,0}
!$$
!
! For example for order=3, the polynomials are arranged as:
!
!$$
! P_{0,0}, P_{0,1}, P_{0,2}, P_{0,3} \\
! P_{1,0}, P_{1,1}, P_{1,2} \\
! P_{2,0}, P_{2,1} \\
! P_{3,0}
!$$

INTERFACE Dubiner_Triangle
  MODULE PURE FUNCTION Dubiner_Triangle1(order, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points in reference triangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference domain of triangle where xij are defined
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! Shape Functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Triangle1
END INTERFACE Dubiner_Triangle

INTERFACE OrthogonalBasis_Triangle
  MODULE PROCEDURE Dubiner_Triangle1
END INTERFACE OrthogonalBasis_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit.
!
! The shape of `ans` is (M,N), where M=SIZE(xij,2) (number of points)
! N = 0.5*(order+1)*(order+2).
!
! In this way, ans(j,:) denotes the values of all polynomial at jth point
!
! Polynomials are returned in following way:
!
!$$
! P_{0,0}, P_{0,1}, \cdots , P_{0,order} \\
! P_{1,0}, P_{1,1}, \cdots , P_{1,order-1} \\
! P_{2,0}, P_{2,1}, \cdots , P_{2,order-2} \\
! \cdots
! P_{order,0}
!$$
!
! For example for order=3, the polynomials are arranged as:
!
!$$
! P_{0,0}, P_{0,1}, P_{0,2}, P_{0,3} \\
! P_{1,0}, P_{1,1}, P_{1,2} \\
! P_{2,0}, P_{2,1} \\
! P_{3,0}
!$$

INTERFACE Dubiner_Triangle_
  MODULE PURE SUBROUTINE Dubiner_Triangle1_(order, xij, refTriangle, ans, &
                                            nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points in reference triangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference domain of triangle where xij are defined
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! Shape Functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Dubiner_Triangle1_
END INTERFACE Dubiner_Triangle_

INTERFACE OrthogonalBasis_Triangle_
  MODULE PROCEDURE Dubiner_Triangle1_
END INTERFACE OrthogonalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit. Here x and y are coordinate on line.
! xij is given by outerproduct of x and y.

INTERFACE Dubiner_Triangle
  MODULE PURE FUNCTION Dubiner_Triangle2(order, x, y, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! x and y coordinates, total points = SIZE(x)*SIZE(y)
    !! x denotes the coordinates along the x direction
    !! y denotes the coordinates along the y direction
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference domain of triangle where xij are defined
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Triangle2
END INTERFACE Dubiner_Triangle

INTERFACE OrthogonalBasis_Triangle
  MODULE PROCEDURE Dubiner_Triangle2
END INTERFACE OrthogonalBasis_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit. Here x and y are coordinate on line.
! xij is given by outerproduct of x and y.

INTERFACE Dubiner_Triangle_
  MODULE PURE SUBROUTINE Dubiner_Triangle2_(order, x, y, refTriangle, ans, &
                                            nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! x and y coordinates, total points = SIZE(x)*SIZE(y)
    !! x denotes the coordinates along the x direction
    !! y denotes the coordinates along the y direction
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference domain of triangle where xij are defined
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Dubiner_Triangle2_
END INTERFACE Dubiner_Triangle_

INTERFACE OrthogonalBasis_Triangle_
  MODULE PROCEDURE Dubiner_Triangle2_
END INTERFACE OrthogonalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                    VertexBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit Triangle

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Triangle(xij, refTriangle) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation on the triangle
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! UNIT or BIUNIT
    REAL(DFP) :: ans(SIZE(xij, 2), 3)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        EdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on left, right edge of biunit Triangle
!
!# Introduction
!
! Evaluate basis functions on left and right edge of biunit Triangle
!
! qe1 and qe2 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION EdgeBasis_Triangle(pe1, pe2, pe3, xij, refTriangle) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on left vertical edge (e1), should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on right vertical edge(e2), should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on right vertical edge(e3), should be greater than 1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    REAL(DFP) :: ans(SIZE(xij, 2), pe1 + pe2 + pe3 - 3)
  END FUNCTION EdgeBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      CellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of biunit Triangle
!
!# Introduction
!
! Evaluate basis functions in the cell of biunit Triangle

INTERFACE
  MODULE PURE FUNCTION CellBasis_Triangle(order, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation inside the cell, order>2
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    REAL(DFP) :: ans(SIZE(xij, 2), INT((order - 1) * (order - 2) / 2))
  END FUNCTION CellBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasis_Triangle
  MODULE PURE FUNCTION HeirarchicalBasis_Triangle1(order, pe1, pe2, pe3, &
                                                 xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! Order of interpolation on edge e1
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe2
    !! Order of interpolation on edge e2
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe3
    !! Order of interpolation on edge e3
    !! It should be greater than 1 for edge bubble to exists
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !!
  END FUNCTION HeirarchicalBasis_Triangle1
END INTERFACE HeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-22
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasis_Triangle
  MODULE PURE FUNCTION HeirarchicalBasis_Triangle2(order, xij, refTriangle) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP) :: ans(SIZE(xij, 2), &
                     3 * order + INT((order - 1) * (order - 2) / 2))
    !!
  END FUNCTION HeirarchicalBasis_Triangle2
END INTERFACE HeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle1_(order, pe1, pe2, pe3, &
                                            xij, refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! Order of interpolation on edge e1
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe2
    !! Order of interpolation on edge e2
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe3
    !! Order of interpolation on edge e3
    !! It should be greater than 1 for edge bubble to exists
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans( &
    !   & SIZE(xij, 2), &
    !   & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Triangle1_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle2_(order, xij, &
                                                 refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans( &
    !   & SIZE(xij, 2), &
    !   & order*3 + INT((order - 1) * (order - 2) / 2))
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Triangle2_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-04
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle3_(order, pe1, pe2, pe3, &
        xij, refTriangle, edgeOrient1, edgeOrient2, edgeOrient3, faceOrient, &
                                                      ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! Order of interpolation on edge e1
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe2
    !! Order of interpolation on edge e2
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe3
    !! Order of interpolation on edge e3
    !! It should be greater than 1 for edge bubble to exists
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    INTEGER(I4B), INTENT(IN) :: edgeOrient1, edgeOrient2, edgeOrient3
    !! edge orientation, 1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! face orient, size is 2, 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans( &
    !   & SIZE(xij, 2), &
    !   & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Triangle3_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-04
! summary: Evaluate all Lagrange polynomial of order n at single points

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle1(order, x, xij, refTriangle, &
                                      coeff, firstCall, basisType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !!
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Triangle1
END INTERFACE LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle1_(order, x, xij, ans, tsize, &
                                     refTriangle, coeff, firstCall, basisType)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !!
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Total size written in ans
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
  END SUBROUTINE LagrangeEvalAll_Triangle1_
END INTERFACE LagrangeEvalAll_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-04
! summary: Evaluate all Lagrange polynomials of order n at several points

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle2(order, x, xij, refTriangle, &
                 coeff, firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
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
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Triangle2
END INTERFACE LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!                             LagrangeEvalAll_Triangle_@LagrnageBasisMethods
!----------------------------------------------------------------------------

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle2_(order, x, xij, ans, nrow, &
          ncol, refTriangle, coeff, firstCall, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of rows and columns written to ans
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
  END SUBROUTINE LagrangeEvalAll_Triangle2_
END INTERFACE LagrangeEvalAll_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION QuadratureNumber_Triangle(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    INTEGER(I4B) :: ans
    !! Quadrature points
  END FUNCTION QuadratureNumber_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 QuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  based quadrature points

INTERFACE QuadraturePoint_Triangle
  MODULE FUNCTION QuadraturePoint_Triangle1(order, quadType, refTriangle, &
                                            xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit ! Unit
    !! If xij is present,then this parameter is not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION QuadraturePoint_Triangle1
END INTERFACE QuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE QuadraturePoint_Triangle_
  MODULE SUBROUTINE QuadraturePoint_Triangle1_(order, quadType, refTriangle, &
                                               xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit ! Unit
    !! If xij is present,then this parameter is not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE QuadraturePoint_Triangle1_
END INTERFACE QuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  based quadrature points

INTERFACE QuadraturePoint_Triangle
  MODULE FUNCTION QuadraturePoint_Triangle2(nips, quadType, refTriangle, &
                                            xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! nips(1) .LE. 79, then we call
    !! economical quadrature rules.
    !! Otherwise, this routine will retport
    !! error
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type,
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit ! Unit
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION QuadraturePoint_Triangle2
END INTERFACE QuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE QuadraturePoint_Triangle_
  MODULE SUBROUTINE QuadraturePoint_Triangle2_(nips, quadType, refTriangle, &
                                               xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! nips(1) .LE. 79, then we call
    !! economical quadrature rules.
    !! Otherwise, this routine will retport
    !! error
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type,
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit ! Unit
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE QuadraturePoint_Triangle2_
END INTERFACE QuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor based quadrature points

INTERFACE TensorQuadraturePoint_Triangle
  MODULE FUNCTION TensorQuadraturePoint_Triangle1(order, quadType, &
    & refTriangle, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Triangle1
END INTERFACE TensorQuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TensorQuadraturePoint_Triangle_
  MODULE SUBROUTINE TensorQuadraturePoint_Triangle1_(order, quadType, &
                                            refTriangle, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle ! Biunit ! Unit
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
   !! number of rows and columns written in ans
  END SUBROUTINE TensorQuadraturePoint_Triangle1_
END INTERFACE TensorQuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor based quadrature points

INTERFACE TensorQuadraturePoint_Triangle
  MODULE FUNCTION TensorQuadraturePoint_Triangle2(nipsx, nipsy, quadType, &
    & refTriangle, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Triangle2
END INTERFACE TensorQuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TensorQuadraturePoint_Triangle_
  MODULE SUBROUTINE TensorQuadraturePoint_Triangle2_(nipsx, nipsy, quadType, &
                                            refTriangle, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 2 or 3.
    !! The number of columns in xij should be 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
  END SUBROUTINE TensorQuadraturePoint_Triangle2_
END INTERFACE TensorQuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeGradientEvalAll_Triangle
  MODULE FUNCTION LagrangeGradientEvalAll_Triangle1( &
    & order, &
    & x, &
    & xij, &
    & refTriangle, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 2)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
  END FUNCTION LagrangeGradientEvalAll_Triangle1
END INTERFACE LagrangeGradientEvalAll_Triangle

!----------------------------------------------------------------------------
!                                          LagrangeGradientEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials of n at several points

INTERFACE LagrangeGradientEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Triangle1_(order, x, xij, ans, &
          dim1, dim2, dim3, refTriangle, coeff, firstCall, basisType, alpha, &
                                                       beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2), 2)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! SIZE(x, 2), SIZE(xij, 2), 2
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Legendre ! Chebyshev ! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeGradientEvalAll_Triangle1_
END INTERFACE LagrangeGradientEvalAll_Triangle_

!----------------------------------------------------------------------------
!                                        HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma
! date:   2024-04-21
! summary:  Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasisGradient_Triangle
  MODULE FUNCTION HeirarchicalBasisGradient_Triangle1(order, pe1, pe2, pe3, &
                                                 xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! Order of interpolation on edge e1
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe2
    !! Order of interpolation on edge e2
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe3
    !! Order of interpolation on edge e3
    !! It should be greater than 1 for edge bubble to exists
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2), 2)
  END FUNCTION HeirarchicalBasisGradient_Triangle1
END INTERFACE HeirarchicalBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                       HeirarchicalBasisGradient_Triangle_
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma
! date:   2024-04-21
! summary:  Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasisGradient_Triangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Triangle1_(order, pe1, pe2, &
                           pe3, xij, refTriangle, ans, tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! Order of interpolation on edge e1
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe2
    !! Order of interpolation on edge e2
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe3
    !! Order of interpolation on edge e3
    !! It should be greater than 1 for edge bubble to exists
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
     !! tsize1 = SIZE(xij, 2)
     !! tsize2 = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
     !! tsize3 = 2
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
  END SUBROUTINE HeirarchicalBasisGradient_Triangle1_
END INTERFACE HeirarchicalBasisGradient_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasisGradient_Triangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Triangle2_(order, pe1, pe2, &
                pe3, xij, edgeOrient1, edgeOrient2, edgeOrient3, faceOrient, &
                                     refTriangle, ans, tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of approximation inside the triangle (i.e., cell)
    !! it should be greater than 2 for cell bubble to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! Order of interpolation on edge e1
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe2
    !! Order of interpolation on edge e2
    !! It should be greater than 1 for edge bubble to exists
    INTEGER(I4B), INTENT(IN) :: pe3
    !! Order of interpolation on edge e3
    !! It should be greater than 1 for edge bubble to exists
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: edgeOrient1
    !! edge orientation, 1 or -1
    INTEGER(I4B), INTENT(IN) :: edgeOrient2
    !! edge orientation, 1 or -1
    INTEGER(I4B), INTENT(IN) :: edgeOrient3
    !! edge orientation, 1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! orientation of face
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! This parameter denotes the type of reference triangle.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Triangle.
    !! BIUNIT: in this case xij is in biunit triangle.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
     !! tsize1 = SIZE(xij, 2)
     !! tsize2 = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
     !! tsize3 = 2
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
  END SUBROUTINE HeirarchicalBasisGradient_Triangle2_
END INTERFACE HeirarchicalBasisGradient_Triangle_

!----------------------------------------------------------------------------
!                                         OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit.
!
! The shape of `ans` is (M,N), where M=SIZE(xij,2) (number of points)
! N = 0.5*(order+1)*(order+2).
!
! In this way, ans(j,:) denotes the values of all polynomial at jth point
!
! Polynomials are returned in following way:
!
!$$
! P_{0,0}, P_{0,1}, \cdots , P_{0,order} \\
! P_{1,0}, P_{1,1}, \cdots , P_{1,order-1} \\
! P_{2,0}, P_{2,1}, \cdots , P_{2,order-2} \\
! \cdots
! P_{order,0}
!$$
!
! For example for order=3, the polynomials are arranged as:
!
!$$
! P_{0,0}, P_{0,1}, P_{0,2}, P_{0,3} \\
! P_{1,0}, P_{1,1}, P_{1,2} \\
! P_{2,0}, P_{2,1} \\
! P_{3,0}
!$$

INTERFACE OrthogonalBasisGradient_Triangle
  MODULE FUNCTION OrthogonalBasisGradient_Triangle1(order, xij, refTriangle) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in reference triangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2, 2)
    !! Derivative of shape functions
    !! ans(:, j, 1), derivative wrt x of jth shape functions at all points
    !! ans(j, :, 1), derivative wrt x of all shape functions at jth point
  END FUNCTION OrthogonalBasisGradient_Triangle1
END INTERFACE OrthogonalBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                         OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit.
!
! The shape of `ans` is (M,N), where M=SIZE(xij,2) (number of points)
! N = 0.5*(order+1)*(order+2).
!
! In this way, ans(j,:) denotes the values of all polynomial at jth point
!
! Polynomials are returned in following way:
!
!$$
! P_{0,0}, P_{0,1}, \cdots , P_{0,order} \\
! P_{1,0}, P_{1,1}, \cdots , P_{1,order-1} \\
! P_{2,0}, P_{2,1}, \cdots , P_{2,order-2} \\
! \cdots
! P_{order,0}
!$$
!
! For example for order=3, the polynomials are arranged as:
!
!$$
! P_{0,0}, P_{0,1}, P_{0,2}, P_{0,3} \\
! P_{1,0}, P_{1,1}, P_{1,2} \\
! P_{2,0}, P_{2,1} \\
! P_{3,0}
!$$

INTERFACE OrthogonalBasisGradient_Triangle_
  MODULE SUBROUTINE OrthogonalBasisGradient_Triangle1_(order, xij, &
                                     refTriangle, ans, tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in reference triangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    ! ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2, 2)
    !! Derivative of shape functions
    !! ans(:, j, 1), derivative wrt x of jth shape functions at all points
    !! ans(j, :, 1), derivative wrt x of all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
  END SUBROUTINE OrthogonalBasisGradient_Triangle1_
END INTERFACE OrthogonalBasisGradient_Triangle_

!----------------------------------------------------------------------------
!                                                                 Triangle
!----------------------------------------------------------------------------

END MODULE TriangleInterpolationUtility
