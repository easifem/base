
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
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

MODULE QuadrangleInterpolationUtility
USE GlobalData
USE String_Class, ONLY: String

IMPLICIT NONE

PRIVATE

PUBLIC :: LagrangeDegree_Quadrangle
PUBLIC :: LagrangeDOF_Quadrangle
PUBLIC :: LagrangeInDOF_Quadrangle

PUBLIC :: EquidistancePoint_Quadrangle
PUBLIC :: EquidistancePoint_Quadrangle_

PUBLIC :: EquidistanceInPoint_Quadrangle

PUBLIC :: InterpolationPoint_Quadrangle
PUBLIC :: InterpolationPoint_Quadrangle_

PUBLIC :: LagrangeCoeff_Quadrangle
PUBLIC :: LagrangeCoeff_Quadrangle_

PUBLIC :: Dubiner_Quadrangle
PUBLIC :: Dubiner_Quadrangle_

PUBLIC :: DubinerGradient_Quadrangle
PUBLIC :: DubinerGradient_Quadrangle_

PUBLIC :: TensorProdBasis_Quadrangle

PUBLIC :: OrthogonalBasis_Quadrangle

PUBLIC :: VertexBasis_Quadrangle

PUBLIC :: VerticalEdgeBasis_Quadrangle

PUBLIC :: HorizontalEdgeBasis_Quadrangle

PUBLIC :: CellBasis_Quadrangle

PUBLIC :: HeirarchicalBasis_Quadrangle
PUBLIC :: HeirarchicalBasis_Quadrangle_

PUBLIC :: IJ2VEFC_Quadrangle_Clockwise
PUBLIC :: IJ2VEFC_Quadrangle_AntiClockwise

PUBLIC :: LagrangeEvalAll_Quadrangle
PUBLIC :: LagrangeEvalAll_Quadrangle_

PUBLIC :: QuadraturePoint_Quadrangle
PUBLIC :: QuadratureNumber_Quadrangle

PUBLIC :: FacetConnectivity_Quadrangle
PUBLIC :: RefElemDomain_Quadrangle

PUBLIC :: LagrangeGradientEvalAll_Quadrangle
PUBLIC :: LagrangeGradientEvalAll_Quadrangle_

PUBLIC :: HeirarchicalBasisGradient_Quadrangle
PUBLIC :: HeirarchicalBasisGradient_Quadrangle_

PUBLIC :: TensorProdBasisGradient_Quadrangle

PUBLIC :: OrthogonalBasisGradient_Quadrangle

PUBLIC :: GetTotalDOF_Quadrangle
PUBLIC :: GetTotalInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                     GetTotalDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on Quadrangle

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Quadrangle(order, baseContinuity, &
                                              baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalDOF_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Quadrangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Quadrangle
!- These dof are strictly inside the Quadrangle

INTERFACE GetTotalInDOF_Quadrangle
  MODULE PURE FUNCTION GetTotalInDOF_Quadrangle1(order, baseContinuity, &
                                                baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalInDOF_Quadrangle1
END INTERFACE GetTotalInDOF_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE GetTotalInDOF_Quadrangle
  MODULE PURE FUNCTION GetTotalInDOF_Quadrangle2(p, q, baseContinuity, &
                                                baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalInDOF_Quadrangle2
END INTERFACE GetTotalInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE FUNCTION RefElemDomain_Quadrangle(baseContinuity, baseInterpol) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-10
! summary:  This function returns the edge connectivity of Quadrangle

INTERFACE
  MODULE FUNCTION FacetConnectivity_Quadrangle(baseInterpol, baseContinuity) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    CHARACTER(*), INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2, 4)
    !! rows represents the end points of an edges
    !! columns denote the edge (facet)
  END FUNCTION FacetConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 QuadratureNumber_Quadrangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Quadrangle(p, q, quadType1, &
                                                   quadType2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    INTEGER(I4B) :: ans(2)
  END FUNCTION QuadratureNumber_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Quadrangle
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Quadrangle1
END INTERFACE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Quadrangle_
  MODULE PURE SUBROUTINE LagrangeDegree_Quadrangle1_(order, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeDegree_Quadrangle1_
END INTERFACE LagrangeDegree_Quadrangle_

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE LagrangeDegree_Quadrangle
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Quadrangle2
END INTERFACE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeDegree_Quadrangle_
  MODULE PURE SUBROUTINE LagrangeDegree_Quadrangle2_(p, q, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeDegree_Quadrangle2_
END INTERFACE LagrangeDegree_Quadrangle_

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Quadrangle

INTERFACE LagrangeDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Quadrangle1
END INTERFACE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Quadrangle

INTERFACE LagrangeDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Quadrangle2
END INTERFACE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                  LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of Quadrangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of Quadrangle
!- These dof are strictly inside the Quadrangle

INTERFACE LagrangeInDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Quadrangle1
END INTERFACE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                  LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of Quadrangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of Quadrangle
!- These dof are strictly inside the Quadrangle

INTERFACE LagrangeInDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Quadrangle2
END INTERFACE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Quadrangle element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Quadrangle element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE EquidistancePoint_Quadrangle
  MODULE RECURSIVE FUNCTION EquidistancePoint_Quadrangle1(order, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of quadrangle
    !! number of rows = 2
    !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates of interpolation points in $x_{iJ}$ format.
    !! Number of rows in ans is equal to the 2
    !! Number of columns in ans is equal to the number of points
  END FUNCTION EquidistancePoint_Quadrangle1
END INTERFACE EquidistancePoint_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE EquidistancePoint_Quadrangle_
  MODULE RECURSIVE SUBROUTINE EquidistancePoint_Quadrangle1_(order, &
                                                         ans, nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! returned coordinates of interpolation points in $x_{iJ}$ format.
    !! Number of rows in ans is equal to the 2
    !! Number of columns in ans is equal to the number of points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of quadrangle
    !! number of rows = 2
    !! number of cols = 4
  END SUBROUTINE EquidistancePoint_Quadrangle1_
END INTERFACE EquidistancePoint_Quadrangle_

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Quadrangle element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Quadrangle element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE EquidistancePoint_Quadrangle
  MODULE RECURSIVE FUNCTION EquidistancePoint_Quadrangle2(p, q, &
                                                          xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of quadrangle
    !! number of rows = 2 or 3
    !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates of interpolation points in $x_{iJ}$ format.
    !! Number of rows in ans is equal to the 2
    !! Number of columns in ans is equal to the number of points
  END FUNCTION EquidistancePoint_Quadrangle2
END INTERFACE EquidistancePoint_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE EquidistancePoint_Quadrangle_
  MODULE RECURSIVE SUBROUTINE EquidistancePoint_Quadrangle2_(p, q, ans, &
                                                             nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of quadrangle
    !! number of rows = 2 or 3
    !! number of cols = 4
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! returned coordinates of interpolation points in $x_{iJ}$ format.
    !! Number of rows in ans is equal to the 2
    !! Number of columns in ans is equal to the number of points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE EquidistancePoint_Quadrangle2_
END INTERFACE EquidistancePoint_Quadrangle_

!----------------------------------------------------------------------------
!                                             EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Quadrangle
!
!# Introduction
!
!- This function returns the equidistance points in Quadrangle
!- All points are inside the Quadrangle

INTERFACE EquidistanceInPoint_Quadrangle
  MODULE FUNCTION EquidistanceInPoint_Quadrangle1(order, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of quadrangle
    !! number of rows = 2 or 3
    !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates of interpolation points in $x_{iJ}$ format.
    !! Number of rows in ans is equal to the 2
    !! Number of columns in ans is equal to the number of points
  END FUNCTION EquidistanceInPoint_Quadrangle1
END INTERFACE EquidistanceInPoint_Quadrangle

!----------------------------------------------------------------------------
!                                             EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Quadrangle
!
!# Introduction
!
!- This function returns the equidistance points in Quadrangle
!- All points are inside the Quadrangle

INTERFACE EquidistanceInPoint_Quadrangle
  MODULE FUNCTION EquidistanceInPoint_Quadrangle2(p, q, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order in y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of quadrangle
    !! number of rows = 2 or 3
    !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! returned coordinates of interpolation points in $x_{iJ}$ format.
    !! Number of rows in ans is equal to the 2
    !! Number of columns in ans is equal to the number of points
  END FUNCTION EquidistanceInPoint_Quadrangle2
END INTERFACE EquidistanceInPoint_Quadrangle

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:  Interpolation point
!
!# Introduction
!
! In this case order is same in both x1 and x2 direction. Therefore,
! (N+1)**2 grid points are returned.
!
! Also in both x1 and x2 same type of grid family will be used.
!
!- This routine returns the interplation points on quad
!- `xij` contains nodal coordinates of quad in xij format.
!- SIZE(xij,1) = nsd, and SIZE(xij,2)=4
!- If xij is absent then biunit quad is used
!- `ipType` is interpolation point type, it can take following values
!- `Equidistance`, uniformly/evenly distributed points
!- `GaussLegendreLobatto
!- `GaussChebyshevLobatto
!
!- `layout` specifies the arrangement of points. The nodes are always
! returned in VEFC format (vertex, edge, face, cell). 1:3 are are
! vertex points, then edge, and then internal nodes. The internal nodes
! also follow the same convention. Please read Gmsh manual  on this topic.

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle1(order, ipType, layout, &
                                         xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
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
    CHARACTER(*), INTENT(IN) :: layout
    !! VEFC, INCREASING
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
  END FUNCTION InterpolationPoint_Quadrangle1
END INTERFACE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!                                           InterpolationPoint_Quadrangle_
!----------------------------------------------------------------------------

INTERFACE InterpolationPoint_Quadrangle_
  MODULE SUBROUTINE InterpolationPoint_Quadrangle1_(order, ipType, ans, &
                                 nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    CHARACTER(*), INTENT(IN) :: layout
    !! VEFC, INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE InterpolationPoint_Quadrangle1_
END INTERFACE InterpolationPoint_Quadrangle_

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:  Interpolation point
!
!# Introduction
!
! In this case order is same in both x1 and x2 direction. Therefore,
! (N+1)**2 grid points are returned.
!
! Also in both x1 and x2 same type of grid family will be used.
!
!- This routine returns the interplation points on quad
!- `xij` contains nodal coordinates of quad in xij format.
!- SIZE(xij,1) = nsd, and SIZE(xij,2)=4
!- If xij is absent then biunit quad is used
!- `ipType` is interpolation point type, it can take following values
!- `Equidistance`, uniformly/evenly distributed points
!- `GaussLegendreLobatto
!- `GaussChebyshevLobatto
!
!- `layout` specifies the arrangement of points. The nodes are always
! returned in VEFC format (vertex, edge, face, cell). 1:3 are are
! vertex points, then edge, and then internal nodes. The internal nodes
! also follow the same convention. Please read Gmsh manual  on this topic.

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle2(p, q, ipType1, ipType2, &
      layout, xij, alpha1, beta1, lambda1, alpha2, beta2, lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of element in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of element in y direction
    INTEGER(I4B), INTENT(IN) :: ipType1
    !! interpolation point type in x direction
    INTEGER(I4B), INTENT(IN) :: ipType2
    !! interpolation point type in y direction
    CHARACTER(*), INTENT(IN) :: layout
    !! VEFC, INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
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
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION InterpolationPoint_Quadrangle2
END INTERFACE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Quadrangle_
!----------------------------------------------------------------------------

INTERFACE InterpolationPoint_Quadrangle_
  MODULE SUBROUTINE InterpolationPoint_Quadrangle2_(p, q, ipType1, ipType2, &
                       ans, nrow, ncol, layout, xij, alpha1, beta1, lambda1, &
                                                    alpha2, beta2, lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of element in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of element in y direction
    INTEGER(I4B), INTENT(IN) :: ipType1
    !! interpolation point type in x direction
    INTEGER(I4B), INTENT(IN) :: ipType2
    !! interpolation point type in y direction
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !!
    CHARACTER(*), INTENT(IN) :: layout
    !! VEFC, INCREASING
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
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
  END SUBROUTINE InterpolationPoint_Quadrangle2_
END INTERFACE InterpolationPoint_Quadrangle_

!----------------------------------------------------------------------------
!                                                                  IJ2VEFC
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Convert format from IJ to VEFC

INTERFACE
  MODULE SUBROUTINE IJ2VEFC_Quadrangle(xi, eta, temp, p, q)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    REAL(DFP), INTENT(IN) :: eta(:, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
  END SUBROUTINE IJ2VEFC_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Convert format from IJ to VEFC

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE IJ2VEFC_Quadrangle_Clockwise(xi, eta, &
                                                        temp, p, q, startNode)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    REAL(DFP), INTENT(IN) :: eta(:, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: startNode
  END SUBROUTINE IJ2VEFC_Quadrangle_Clockwise
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Convert format from IJ to VEFC

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE IJ2VEFC_Quadrangle_AntiClockwise(xi, eta, &
                                                        temp, p, q, startNode)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    REAL(DFP), INTENT(IN) :: eta(:, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: startNode
  END SUBROUTINE IJ2VEFC_Quadrangle_AntiClockwise
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle1
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Quadrangle_
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle1_(order, i, xij, ans, tsize)
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
  END SUBROUTINE LagrangeCoeff_Quadrangle1_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle2(order, i, v, isVandermonde) &
    RESULT(ans)
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
  END FUNCTION LagrangeCoeff_Quadrangle2
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                                 LagrangeCoeff_Quadrangle_
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle2_(order, i, v, isVandermonde, &
                                               ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Quadrangle2_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle3(order, i, v, ipiv) RESULT(ans)
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
  END FUNCTION LagrangeCoeff_Quadrangle3
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle3_(order, i, v, ipiv, ans, tsize)
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
  END SUBROUTINE LagrangeCoeff_Quadrangle3_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle4(order, xij, basisType, alpha, &
                                            beta, lambda) RESULT(ans)
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
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle4
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle4_(order, xij, basisType, &
                                         alpha, beta, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Quadrangle4_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle5(p, q, xij, basisType1, &
       basisType2, alpha1, beta1, lambda1, alpha2, beta2, lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of polynomial in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of polynomial in y direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basisType in x direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basisType in y direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle5
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle5_(p, q, xij, basisType1, &
                 basisType2, alpha1, beta1, lambda1, alpha2, beta2, lambda2, &
                                               ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of polynomial in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of polynomial in y direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basisType in x direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basisType in y direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! This parameter is needed when basisType is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! This parameter is needed when basisType is Ultraspherical
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Quadrangle5_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is called while forming dubiner basis on triangle domain
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

INTERFACE Dubiner_Quadrangle
  MODULE PURE FUNCTION Dubiner_Quadrangle1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in biunit quadrangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Quadrangle1
END INTERFACE Dubiner_Quadrangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is called while forming dubiner basis on triangle domain
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

INTERFACE Dubiner_Quadrangle_
  MODULE PURE SUBROUTINE Dubiner_Quadrangle1_(order, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in biunit quadrangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Dubiner_Quadrangle1_
END INTERFACE Dubiner_Quadrangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is same as Dubiner_Quadrangle1
! The only difference is that xij are given by outerproduct of x and y.
! This function calls `Dubiner_Quadrangle1`.

INTERFACE Dubiner_Quadrangle
  MODULE PURE FUNCTION Dubiner_Quadrangle2(order, x, y) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinate on line
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinate on line
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Quadrangle2
END INTERFACE Dubiner_Quadrangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is same as Dubiner_Quadrangle1
! The only difference is that xij are given by outerproduct of x and y.
! This function calls `Dubiner_Quadrangle1`.

INTERFACE Dubiner_Quadrangle_
  MODULE PURE SUBROUTINE Dubiner_Quadrangle2_(order, x, y, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinate on line
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinate on line
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Dubiner_Quadrangle2_
END INTERFACE Dubiner_Quadrangle_

!----------------------------------------------------------------------------
!                                                       DubinerGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is called while forming dubiner basis on triangle domain
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

INTERFACE DubinerGradient_Quadrangle
  MODULE PURE FUNCTION DubinerGradient_Quadrangle1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in biunit quadrangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    REAL(DFP) :: ans(SIZE(xij, 2), &
                     (order + 1_I4B) * (order + 2_I4B) / 2_I4B, &
                     2_I4B)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION DubinerGradient_Quadrangle1
END INTERFACE DubinerGradient_Quadrangle

!----------------------------------------------------------------------------
!                                                       DubinerGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is called while forming dubiner basis on triangle domain
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

INTERFACE DubinerGradient_Quadrangle_
  MODULE PURE SUBROUTINE DubinerGradient_Quadrangle1_(order, xij, ans, &
                                                      tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in biunit quadrangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    ! ans( &
    ! SIZE(xij, 2), &
    ! & (order + 1_I4B) * (order + 2_I4B) / 2_I4B, &
    ! & 2_I4B)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
  END SUBROUTINE DubinerGradient_Quadrangle1_
END INTERFACE DubinerGradient_Quadrangle_

!----------------------------------------------------------------------------
!                                            TensorProdBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all tensor product orthogoanl polynomial on quadrangle
!
!# Introduction
!
! This function returns the tensor product expansion of orthogonal
! polynomial on biunit quadrangle.

INTERFACE TensorProdBasis_Quadrangle
  MODULE FUNCTION TensorProdBasis_Quadrangle1(p, q, xij, basisType1, &
       basisType2, alpha1, beta1, lambda1, alpha2, beta2, lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis type in x1 direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basis type in x2 direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
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
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1))
    !!
  END FUNCTION TensorProdBasis_Quadrangle1
END INTERFACE TensorProdBasis_Quadrangle

INTERFACE OrthogonalBasis_Quadrangle
  MODULE PROCEDURE TensorProdBasis_Quadrangle1
END INTERFACE OrthogonalBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TensorProdBasis_Quadrangle_
  MODULE SUBROUTINE TensorProdBasis_Quadrangle1_(p, q, xij, ans, nrow, &
               ncol, basisType1, basisType2, alpha1, beta1, lambda1, alpha2, &
                                                 beta2, lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(xij, 2)
    !! ncol = (p + 1) * (q + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis type in x1 direction
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basis type in x2 direction
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
  END SUBROUTINE TensorProdBasis_Quadrangle1_
END INTERFACE TensorProdBasis_Quadrangle_

INTERFACE OrthogonalBasis_Quadrangle_
  MODULE PROCEDURE TensorProdBasis_Quadrangle1_
END INTERFACE OrthogonalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                            TensorProdBasis_Quadrangle
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

INTERFACE TensorProdBasis_Quadrangle
  MODULE FUNCTION TensorProdBasis_Quadrangle2(p, q, x, y, basisType1, &
       basisType2, alpha1, beta1, lambda1, alpha2, beta2, lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! orthogonal polynomial family in x1 direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! orthogonal poly family in x2 direction
    !! Monomials
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! alpha1 needed when basisType1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! beta1 is needed when basisType1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! alpha2 needed when basisType2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! beta2 needed when basisType2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! lambda1 is needed when basisType1 is "Ultraspherical"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! lambda2 is needed when basisType2 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (p + 1) * (q + 1))
    !! Tensor basis
    !! The number of rows corresponds to the
    !! total number of points
  END FUNCTION TensorProdBasis_Quadrangle2
END INTERFACE TensorProdBasis_Quadrangle

INTERFACE OrthogonalBasis_Quadrangle
  MODULE PROCEDURE TensorProdBasis_Quadrangle2
END INTERFACE OrthogonalBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TensorProdBasis_Quadrangle_
  MODULE SUBROUTINE TensorProdBasis_Quadrangle2_(p, q, x, y, ans, nrow, &
        ncol, basisType1, basisType2, alpha1, beta1, lambda1, alpha2, beta2, &
                                                 lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (p + 1) * (q + 1))
    !! nrow = SIZE(x) * SIZE(y)
    !! ncol = (p + 1) * (q + 1)
    !! Tensor basis
    !! The number of rows corresponds to the
    !! total number of points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! Orthogonal polynomial family in x1 direction
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! Orthogonal poly family in x2 direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! alpha1 needed when basisType1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! beta1 is needed when basisType1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! alpha2 needed when basisType2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! beta2 needed when basisType2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! lambda1 is needed when basisType1 is "Ultraspherical"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! lambda2 is needed when basisType2 is "Ultraspherical"
  END SUBROUTINE TensorProdBasis_Quadrangle2_
END INTERFACE TensorProdBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

INTERFACE VertexBasis_Quadrangle
  MODULE PURE FUNCTION VertexBasis_Quadrangle1(x, y) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Quadrangle1
END INTERFACE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE VertexBasis_Quadrangle_
  MODULE PURE SUBROUTINE VertexBasis_Quadrangle1_(x, y, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE VertexBasis_Quadrangle1_
END INTERFACE VertexBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

INTERFACE VertexBasis_Quadrangle
  MODULE PURE FUNCTION VertexBasis_Quadrangle2(xij) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(xij, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Quadrangle2
END INTERFACE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!                                                   VertexBasis_Quadrangle
!----------------------------------------------------------------------------

INTERFACE VertexBasis_Quadrangle_
  MODULE PURE SUBROUTINE VertexBasis_Quadrangle2_(xij, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE VertexBasis_Quadrangle2_
END INTERFACE VertexBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                               VerticalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on left, right edge of biunit quadrangle
!
!# Introduction
!
! Evaluate basis functions on left and right edge of biunit quadrangle
!
! qe1 and qe2 should be greater than or equal to 2
!
! Note that both edge are aligned in positive y direction.

INTERFACE
  MODULE PURE FUNCTION VerticalEdgeBasis_Quadrangle(qe1, qe2, x, y) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order on left vertical edge (e1), it should be greater than 1
    !! It should be greater than 2
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order on right vertical edge(e2), it should be greater than 1
    !! It should be greater than 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP) :: ans(SIZE(x), qe1 + qe2 - 2)
  END FUNCTION VerticalEdgeBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE VerticalEdgeBasis_Quadrangle_(qe1, qe2, x, y, &
                                                       ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order on left vertical edge (e1), it should be greater than 1
    !! It should be greater than 2
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order on right vertical edge(e2), it should be greater than 1
    !! It should be greater than 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    !! these points should be between [-1, 1].
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(x), qe1 + qe2 - 2)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE VerticalEdgeBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on bottom and top edge of biunit quadrangle
!
!# Introduction
!
! Evaluate basis functions on bottom and top edge of biunit quadrangle
!
! pe3 and pe4 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION HorizontalEdgeBasis_Quadrangle(pe3, pe4, x, y)  RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on bottom vertical edge (e3), it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on top vertical edge(e4), it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), pe3 + pe4 - 2)
  END FUNCTION HorizontalEdgeBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE HorizontalEdgeBasis_Quadrangle_(pe3, pe4, x, y, &
                                                         ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on bottom vertical edge (e3), it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on top vertical edge(e4), it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), pe3 + pe4 - 2)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HorizontalEdgeBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of biunit quadrangle
!
!# Introduction
!
! Evaluate basis functions in the cell of biunit quadrangle

INTERFACE
  MODULE PURE FUNCTION CellBasis_Quadrangle(pb, qb, x, y) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on bottom vertical edge (e3), it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: qb
    !! order on top vertical edge(e4), it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), (pb - 1) * (qb - 1))
  END FUNCTION CellBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE CellBasis_Quadrangle_(pb, qb, x, y, ans, nrow, &
                                               ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on bottom vertical edge (e3), it should be greater than 1
    INTEGER(I4B), INTENT(IN) :: qb
    !! order on top vertical edge(e4), it should be greater than 1
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), (pb - 1) * (qb - 1))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE CellBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on quadrangle
!
!# Introduction
!
! This function returns the modal basis on orthogonal polynomial
! The modal function in 1D is given by scaled Lobatto polynomial.
! These modal functions are orthogonal with respect to H1 seminorm.
! However, these modal function are not orthogonal withrespect to L2 norm.
!
! Bubble function in 1D is proportional to Jacobi polynomial with
! alpha=beta=1. Equivalently, these bubble functions are proportional to
! Ultraspherical polynomials with lambda = 3/2.
!

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle1(pb, qb, pe3, pe4, &
                                                    qe1, qe2, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), &
      & pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)
    !!
  END FUNCTION HeirarchicalBasis_Quadrangle1
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle1_(pb, qb, pe3, pe4, &
                                               qe1, qe2, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(xij, 2), &
    !! ncol =  pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Quadrangle1_
END INTERFACE HeirarchicalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on quadrangle
!
!# Introduction
!
! This function is identical to `HeirarchicalBasis_Quadrangle1`
! with qe1=qe2=qb=q, and pe3=pe4=pb=p.
!

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle2(p, q, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of interpolation inside the quadrangle in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1))
  END FUNCTION HeirarchicalBasis_Quadrangle2
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle2_(p, q, xij, ans, &
                                                        nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of interpolation inside the quadrangle in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(xij, 2)
    !! ncol = (p + 1) * (q + 1))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Quadrangle2_
END INTERFACE HeirarchicalBasis_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle3(pb, qb, pe3, pe4, &
                  qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, qe2Orient, &
                                                     faceOrient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! orientation of edge 1
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! orientation of edge 2
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! orientation of edge 3
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! orientation of edge 4
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! orientation of face
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! nrow = SIZE(xij, 2)
    !! ncol =  pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1
  END FUNCTION HeirarchicalBasis_Quadrangle3
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle3_(pb, qb, pe3, pe4, &
                  qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, qe2Orient, &
                                                  faceOrient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! orientation of edge 1
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! orientation of edge 2
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! orientation of edge 3
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! orientation of edge 4
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! orientation of face
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(xij, 2), &
    !! ncol =  pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Quadrangle3_
END INTERFACE HeirarchicalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                                 LagrangeEvalAll_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-04
! summary: Evaluate all Lagrange polynomial of order n at single points

INTERFACE LagrangeEvalAll_Quadrangle
  MODULE FUNCTION LagrangeEvalAll_Quadrangle1(order, x, xij, coeff, &
                        firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    !! The number of rows in xij can be 2 or 3
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
    !! Legendre ! Lobatto ! Chebyshev ! Jacobi ! Ultraspherical
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Quadrangle1
END INTERFACE LagrangeEvalAll_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle1_(order, x, xij, ans, tsize, &
                             coeff, firstCall, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    !! The number of rows in xij can be 2 or 3
    !! The number of columns in xij should be equal to total
    !! degree of freedom
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Total size written in ans
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coeff(SIZE(xij, 2), SIZE(xij, 2))
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
  END SUBROUTINE LagrangeEvalAll_Quadrangle1_
END INTERFACE LagrangeEvalAll_Quadrangle_

!----------------------------------------------------------------------------
!                                               LagrangeEvalAll_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-04
! summary: Evaluate all Lagrange polynomials of order n at several points

INTERFACE LagrangeEvalAll_Quadrangle
  MODULE FUNCTION LagrangeEvalAll_Quadrangle2(order, x, xij, coeff, &
                        firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
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
  END FUNCTION LagrangeEvalAll_Quadrangle2
END INTERFACE LagrangeEvalAll_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle2_(order, x, xij, ans, &
                 nrow, ncol, coeff, firstCall, basisType, alpha, beta, lambda)
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
    !! number of rows and columns written in ans
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
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Quadrangle2_
END INTERFACE LagrangeEvalAll_Quadrangle_

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-19
! summary:  Returns quadrature points on reference quadrangle

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle1(order, quadType, &
                          refQuadrangle, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of integrand in x and y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
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
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle
    !! UNIT
    !! BIUNIT
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
  END FUNCTION QuadraturePoint_Quadrangle1
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                                QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle2(p, q, quadType1, quadType2, &
         refQuadrangle, xij, alpha1, beta1, lambda1, alpha2, beta2, lambda2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of  integrand in y direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
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
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
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
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION QuadraturePoint_Quadrangle2
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                                 QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-19
! summary:  Returns quadrature points on reference quadrangle

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle3(nips, quadType, &
                          refQuadrangle, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of integration points in x and y direction
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
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle
    !! UNIT
    !! BIUNIT
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
  END FUNCTION QuadraturePoint_Quadrangle3
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                                QuadraturePoint_Quadrangle
!----------------------------------------------------------------------------

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle4(nipsx, nipsy, quadType1, &
       quadType2, refQuadrangle, xij, alpha1, beta1, lambda1, alpha2, beta2, &
                                              lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! order of integrand in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! order of  integrand in y direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! interpolation point type in x direction
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
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
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
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION QuadraturePoint_Quadrangle4
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                          LagrangeGradientEvalAll_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials of n at several points

INTERFACE LagrangeGradientEvalAll_Quadrangle
  MODULE FUNCTION LagrangeGradientEvalAll_Quadrangle1(order, x, xij, coeff, &
                        firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
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
  END FUNCTION LagrangeGradientEvalAll_Quadrangle1
END INTERFACE LagrangeGradientEvalAll_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LagrangeGradientEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Quadrangle1_(order, x, xij, &
            ans, dim1, dim2, dim3, coeff, firstCall, basisType, alpha, beta, &
                                                         lambda)
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
    !! dim1 = SIZE(x, 2)
    !! dim2 = SIZE(xij, 2)
    !! dim3 = 2
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !!
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
  END SUBROUTINE LagrangeGradientEvalAll_Quadrangle1_
END INTERFACE LagrangeGradientEvalAll_Quadrangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on quadrangle
!
!# Introduction
!
! This function returns the modal basis on orthogonal polynomial
! The modal function in 1D is given by scaled Lobatto polynomial.
! These modal functions are orthogonal with respect to H1 seminorm.
! However, these modal function are not orthogonal withrespect to L2 norm.
!
! Bubble function in 1D is proportional to Jacobi polynomial with
! alpha=beta=1. Equivalently, these bubble functions are proportional to
! Ultraspherical polynomials with lambda = 3/2.

INTERFACE HeirarchicalBasisGradient_Quadrangle
  MODULE FUNCTION HeirarchicalBasisGradient_Quadrangle1(pb, qb, pe3, pe4, &
                                                    qe1, qe2, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), &
      & pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1, 2)
  END FUNCTION HeirarchicalBasisGradient_Quadrangle1
END INTERFACE HeirarchicalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasisGradient_Quadrangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Quadrangle1_(pb, qb, pe3, &
                                    pe4, qe1, qe2, xij, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! dim1 =  SIZE(xij, 2)
    !! dim2 = pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1
    !! dim3 = 2
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE HeirarchicalBasisGradient_Quadrangle1_
END INTERFACE HeirarchicalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on quadrangle

INTERFACE HeirarchicalBasisGradient_Quadrangle
  MODULE FUNCTION HeirarchicalBasisGradient_Quadrangle2(p, q, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of interpolation inside the quadrangle in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1), 2)
  END FUNCTION HeirarchicalBasisGradient_Quadrangle2
END INTERFACE HeirarchicalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasisGradient_Quadrangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Quadrangle2_(p, q, xij, &
                                                        ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of interpolation inside the quadrangle in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = SIZE(xij, 2)
    !! dim2 = (p+1)*(q+1)
    !! dim3 = 2
  END SUBROUTINE HeirarchicalBasisGradient_Quadrangle2_
END INTERFACE HeirarchicalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-06
! summary:  Basis gradient

INTERFACE HeirarchicalBasisGradient_Quadrangle
  MODULE FUNCTION HeirarchicalBasisGradient_Quadrangle3(pb, qb, pe3, pe4, &
      qe1, qe2, xij, qe1Orient, qe2Orient, pe3Orient, pe4Orient, faceOrient) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! left vertical edge orientation
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! right vertical edge orientation
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! orientation of bottom horizontal edge
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! orientation of top horizontal edge
    INTEGER(I4B), INTENT(IN) :: faceOrient(3)
    !! orientation of faces
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION HeirarchicalBasisGradient_Quadrangle3
END INTERFACE HeirarchicalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!                                       HeirarchicalBasisGradient_Quadrangle
!----------------------------------------------------------------------------

INTERFACE HeirarchicalBasisGradient_Quadrangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Quadrangle3_(pb, qb, pe3, pe4, &
      qe1, qe2, xij, qe1Orient, qe2Orient, pe3Orient, pe4Orient, faceOrient, &
                                                        ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! left vertical edge orientation
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! right vertical edge orientation
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! orientation of bottom horizontal edge
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! orientation of top horizontal edge
    INTEGER(I4B), INTENT(IN) :: faceOrient(3)
    !! orientation of faces
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! dim1 =  SIZE(xij, 2)
    !! dim2 = pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1
    !! dim3 = 2
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE HeirarchicalBasisGradient_Quadrangle3_
END INTERFACE HeirarchicalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                                        TensorProdBasisGradient_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all tensor product orthogoanl polynomial on quadrangle

INTERFACE TensorProdBasisGradient_Quadrangle
  MODULE FUNCTION TensorProdBasisGradient_Quadrangle1(p, q, xij, &
              basisType1, basisType2, alpha1, beta1, lambda1, alpha2, beta2, &
                                                      lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis type in x1 direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basis type in x2 direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
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
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1), 2)
    !!
  END FUNCTION TensorProdBasisGradient_Quadrangle1
END INTERFACE TensorProdBasisGradient_Quadrangle

INTERFACE OrthogonalBasisGradient_Quadrangle
  MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1
END INTERFACE OrthogonalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TensorProdBasisGradient_Quadrangle_
  MODULE SUBROUTINE TensorProdBasisGradient_Quadrangle1_(p, q, xij, ans, &
   dim1, dim2, dim3, basisType1, basisType2, alpha1, beta1, lambda1, alpha2, &
                                                         beta2, lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! dim1 = SIZE(xij, 2)
    !! dim2 = (p + 1) * (q + 1)
    !! dim3 = 2
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dimension of data written in ans
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis type in x1 direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! basis type in x2 direction
    !! Monomials ! Jacobi ! Legendre ! Chebyshev ! Ultraspherical
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
  END SUBROUTINE TensorProdBasisGradient_Quadrangle1_
END INTERFACE TensorProdBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE OrthogonalBasisGradient_Quadrangle_
  MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1_
END INTERFACE OrthogonalBasisGradient_Quadrangle_

END MODULE QuadrangleInterpolationUtility
