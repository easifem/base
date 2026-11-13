! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https://www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: TriangleInterpolationUtility geometry and interpolation utilities.
!
! UNIT vertices are (0,0),(1,0),(0,1), and BIUNIT vertices are
! (-1,-1),(1,-1),(-1,1). Basis outputs have point rows and basis columns;
! gradients add a coordinate axis. Node layouts and edge orientation must
! match the element connectivity. Dubiner and OrthogonalBasis names are
! aliases. The orthogonal gradient implementation uses UNIT chain factors;
! BIUNIT scaling requires review.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$u(x,y)=\sum_{i+j\le p} c_{ij}\phi_{ij}(x,y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

MODULE TriangleInterpolationUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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

PUBLIC :: GetHierarchicalDOF_Triangle

!----------------------------------------------------------------------------
!                                               GetHierarchicalDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count vertex, edge, cell or total hierarchical triangle modes.
!
! opt selects V, E, C, or total (H and other values). Use edge orders >=1 and
! cell order >=1.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   opt : CHARACTER(1), INTENT(IN). Connectivity or entity selector; see the
! routine description.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=3+\sum_{e=1}^3(p_e-1)+(p-1)(p-2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: GetHierarchicalDOF_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! n = GetHierarchicalDOF_Triangle(3_I4B,2_I4B,3_I4B,4_I4B,"H")
! CALL OK(n == 10, "Three vertices, six edge modes and one cell mode")
! CALL OK(GetHierarchicalDOF_Triangle(3_I4B,2_I4B,3_I4B,4_I4B,"C") &
!   == 1, "One cubic bubble mode")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetHierarchicalDOF_Triangle( &
    order, pe1, pe2, pe3, opt) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! cell order
    INTEGER(I4B), INTENT(IN) :: pe1, pe2, pe3
    !! face order
    CHARACTER(1), INTENT(IN) :: opt
    !! 'V' - vertex
    !! 'E' - edge
    !! 'C' - cell
    !! 'H' - total hierarchical dof
    INTEGER(I4B) :: ans
  END FUNCTION GetHierarchicalDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query interpolation dimensions, exponent tables and reference
! domains.
!
! Degree tables have monomial rows and coordinate columns. Use positive
! interpolation orders. Current total-count implementations do not branch on
! the supplied continuity or interpolation strings; they return the scalar
! polynomial counts. Triangle H1 Lagrange uses UNIT, while H1 hierarchical and
! orthogonal families use BIUNIT.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   baseContinuity : CHARACTER(*), INTENT(IN). Use the declaration and
! description for this overload.
!   baseInterpolation : CHARACTER(*), INTENT(IN). Use the declaration and
! description for this overload.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

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
! LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query interpolation dimensions, exponent tables and reference
! domains.
!
! Degree tables have monomial rows and coordinate columns. Use positive
! interpolation orders. Current total-count implementations do not branch on
! the supplied continuity or interpolation strings; they return the scalar
! polynomial counts. Triangle H1 Lagrange uses UNIT, while H1 hierarchical and
! orthogonal families use BIUNIT.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   baseContinuity : CHARACTER(*), INTENT(IN). Use the declaration and
! description for this overload.
!   baseInterpolation : CHARACTER(*), INTENT(IN). Use the declaration and
! description for this overload.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query interpolation dimensions, exponent tables and reference
! domains.
!
! Degree tables have monomial rows and coordinate columns. Use positive
! interpolation orders. Current total-count implementations do not branch on
! the supplied continuity or interpolation strings; they return the scalar
! polynomial counts. Triangle H1 Lagrange uses UNIT, while H1 hierarchical and
! orthogonal families use BIUNIT.
!
! Arguments:
!   baseContinuity : CHARACTER(*), INTENT(IN). Use the declaration and
! description for this overload.
!   baseInterpol : CHARACTER(*), INTENT(IN). Use the declaration and
! description for this overload.
!
! Returns:
!   ans : TYPE(String).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION RefElemDomain_Triangle(baseContinuity, baseInterpol) &
    RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return vertex connectivity for triangular boundary edges.
!
! The current implementation ignores the optional basis family strings and
! always returns this connectivity.
!
! Arguments:
!   baseInterpol : CHARACTER(*), OPTIONAL, INTENT(IN). Use the declaration and
! description for this overload.
!   baseContinuity : CHARACTER(*), OPTIONAL, INTENT(IN). Use the declaration
! and description for this overload.
!
! Returns:
!   ans : INTEGER(I4B)(2, 3).
!
! Formula:
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: FacetConnectivity_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: edges(2,3)
!
! edges = FacetConnectivity_Triangle()
! CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
!   ALL(edges(:,2) == [2_I4B,3_I4B]) .AND. &
!   ALL(edges(:,3) == [3_I4B,1_I4B]), "Counterclockwise boundary edges")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION FacetConnectivity_Triangle(baseInterpol, &
                                             baseContinuity) RESULT(ans)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpol
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2, 3)
    !! rows represents the end points of an edges
    !! columns denote the edge (facet)
  END FUNCTION FacetConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         IJ2VEFC_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorder triangular grid coordinates into entity ordering.
!
! Provide xi and eta on an (order+1) square grid and temp with two coordinate
! rows and N columns. Only the triangular subset is visited.
!
! Arguments:
!   xi : REAL(DFP), INTENT(IN)(:, :). Grid of first coordinates.
!   eta : REAL(DFP), INTENT(IN)(:, :). Grid of second coordinates.
!   temp : REAL(DFP), INTENT(INOUT)(:, :). Output point coordinates in VEFC
! ordering.
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   N : INTEGER(I4B), INTENT(IN). Number of samples, coordinate dimension or
! grid count as specified by the routine.
!
! Returns:
!   Updates temp(:, :).
!
! Formula:
! $$N=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: IJ2VEFC_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: xi(2,2), eta(2,2), nodes(2,3)
!
! xi(:,1) = [0.0_DFP,1.0_DFP]
! xi(:,2) = xi(:,1)
! eta(1,:) = [0.0_DFP,1.0_DFP]
! eta(2,:) = eta(1,:)
! CALL IJ2VEFC_Triangle(xi,eta,nodes,order=1_I4B,N=3_I4B)
! CALL OK(MAXVAL(ABS(nodes-RESHAPE( &
!   [0.0_DFP,0.0_DFP,1.0_DFP,0.0_DFP,0.0_DFP,1.0_DFP],[2,3]))) &
!   < 1.0E-12_DFP, "Grid vertices become VEFC coordinates")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query interpolation dimensions, exponent tables and reference
! domains.
!
! Degree tables have monomial rows and coordinate columns. Use positive
! interpolation orders. Current total-count implementations do not branch on
! the supplied continuity or interpolation strings; they return the scalar
! polynomial counts. Triangle H1 Lagrange uses UNIT, while H1 hierarchical and
! orthogonal families use BIUNIT.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE(:, :).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query interpolation dimensions, exponent tables and reference
! domains.
!
! Degree tables have monomial rows and coordinate columns. Use positive
! interpolation orders. Current total-count implementations do not branch on
! the supplied continuity or interpolation strings; they return the scalar
! polynomial counts. Triangle H1 Lagrange uses UNIT, while H1 hierarchical and
! orthogonal families use BIUNIT.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query interpolation dimensions, exponent tables and reference
! domains.
!
! Degree tables have monomial rows and coordinate columns. Use positive
! interpolation orders. Current total-count implementations do not branch on
! the supplied continuity or interpolation strings; they return the scalar
! polynomial counts. Triangle H1 Lagrange uses UNIT, while H1 hierarchical and
! orthogonal families use BIUNIT.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Triangle(3_I4B)
! CALL OK(SIZE(degree,1) == 10 .AND. &
!   SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Triangle("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Generate interior triangle interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: EquidistanceInPoint_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = EquidistanceInPoint_Triangle(order=3_I4B)
! CALL OK(ALL(SHAPE(y) == [2, 1]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y-(1.0_DFP/3.0_DFP))) < 1.0E-11_DFP, &
!   "Single interior point is the centroid")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Generate triangle interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = EquidistancePoint_Triangle(order=2_I4B)
! CALL OK(ALL(SHAPE(y) == [2, 6]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE RECURSIVE PURE FUNCTION EquidistancePoint_Triangle(order, &
    xij) RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Generate triangle interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(2, 6)
! INTEGER(I4B) :: nr, nc
!
! CALL EquidistancePoint_Triangle_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Generate triangle interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node family selector.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = InterpolationPoint_Triangle(order=2_I4B, ipType=Equidistance, &
!   layout="VEFC")
! CALL OK(ALL(SHAPE(y) == [2, 6]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION InterpolationPoint_Triangle(order, ipType, &
                                              layout, xij, alpha, &
                                              beta, lambda) RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Generate triangle interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node family selector.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(2, 6)
! INTEGER(I4B) :: nr, nc
!
! CALL InterpolationPoint_Triangle_(order=2_I4B, ipType=Equidistance, &
!   layout="VEFC", ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE InterpolationPoint_Triangle_( &
    order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   i : INTEGER(I4B), INTENT(IN). One based cardinal basis function index.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2)).
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! c = LagrangeCoeff_Triangle(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   i : INTEGER(I4B), INTENT(IN). One based cardinal basis function index.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle_
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! CALL LagrangeCoeff_Triangle_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   i : INTEGER(I4B), INTENT(IN). One based cardinal basis function index.
!   v : REAL(DFP), INTENT(IN)(:, :). Vandermonde matrix or LU factorization,
! according to overload.
!   isVandermonde : LOGICAL(LGT), INTENT(IN). Marks v as a Vandermonde matrix.
!
! Returns:
!   ans : REAL(DFP)(SIZE(v, 1)).
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! c = LagrangeCoeff_Triangle(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   i : INTEGER(I4B), INTENT(IN). One based cardinal basis function index.
!   v : REAL(DFP), INTENT(IN)(:, :). Vandermonde matrix or LU factorization,
! according to overload.
!   isVandermonde : LOGICAL(LGT), INTENT(IN). Marks v as a Vandermonde matrix.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle_
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! CALL LagrangeCoeff_Triangle_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

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
    ! ans(SIZE(v, 1)) ! coefficients of ith Lagrange polynomial
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Triangle2_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   i : INTEGER(I4B), INTENT(IN). One based cardinal basis function index.
!   v : REAL(DFP), INTENT(INOUT)(:, :). Vandermonde matrix or LU
! factorization, according to overload.
!   ipiv : INTEGER(I4B), INTENT(IN)(:). Pivot indices for the supplied LU
! factorization.
!
! Returns:
!   ans : REAL(DFP)(SIZE(v, 1)).
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! c = LagrangeCoeff_Triangle(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   i : INTEGER(I4B), INTENT(IN). One based cardinal basis function index.
!   v : REAL(DFP), INTENT(INOUT)(:, :). Vandermonde matrix or LU
! factorization, according to overload.
!   ipiv : INTEGER(I4B), INTENT(IN)(:). Pivot indices for the supplied LU
! factorization.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!
! Returns:
!   Updates v(:, :), ans(:), tsize.
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle_
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! CALL LagrangeCoeff_Triangle_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

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
    !! ans(SIZE(v, 1)) ! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Triangle3_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   refTriangle : CHARACTER(*), OPTIONAL, INTENT(IN). Reference triangle name,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), SIZE(xij, 2)).
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! c = LagrangeCoeff_Triangle(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle4(order, xij, basisType, &
                                          refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials ! Jacobi (Dubiner) ! Heirarchical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTriangle
    !! UNIT ! BIUNIT
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Triangle4
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle_
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! CALL LagrangeCoeff_Triangle_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle4_(order, xij, basisType, &
                                             refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomials ! Jacobi (Dubiner) ! Heirarchical
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! UNIT ! BIUNIT
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Triangle4_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct coefficients of the nodal interpolation basis.
!
! The i overload returns one coefficient column. The v/isVandermonde overload
! takes a Vandermonde matrix; the v/ipiv overload takes its LU factors and
! pivots. Columns are basis functions. The example uses a monomial basis and
! verifies its interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   degree : INTEGER(I4B), INTENT(IN)(:, :). Monomial exponent table, with
! monomial rows and coordinate columns.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$V C=I,\quad\ell_j(x)=\sum_k C_{kj}m_k(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeCoeff_Triangle_
! USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), c(3,3), v(3,3)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! CALL LagrangeCoeff_Triangle_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Triangle(1_I4B)
! DO j = 1, 3
!   DO i = 1, 3
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 3
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle5_( &
    order, xij, basisType, refTriangle, degree, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomials, Jacobi (Dubiner), Hierarchical
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! UNIT, BIUNIT
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
    !! degree of monomials, used when basisType is Monomial
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Triangle5_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                              LagrangeVandermonde_Triangle
!----------------------------------------------------------------------------

INTERFACE LagrangeVandermonde_Triangle_
  MODULE PURE SUBROUTINE LagrangeVandermonde_Triangle1_(xij, degree, ans, &
                                                        nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    !! nrow = number of spatial dimensions
    !! ncol = number of points of evaluation
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
    !! degree of monomials
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xij, 2)
    !! ncol = SIZE(degree, 1)
  END SUBROUTINE LagrangeVandermonde_Triangle1_
END INTERFACE LagrangeVandermonde_Triangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Dubiner on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (order + 1) * (order + 2) / 2).
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = Dubiner_Triangle(order=1_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = OrthogonalBasis_Triangle(order=1_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle
  MODULE PROCEDURE Dubiner_Triangle1
END INTERFACE OrthogonalBasis_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Dubiner on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL Dubiner_Triangle_(order=1_I4B, xij=points, refTriangle="UNIT", &
!   ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL OrthogonalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle_
  MODULE PROCEDURE Dubiner_Triangle1_
END INTERFACE OrthogonalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Dubiner on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Use the declaration and description for this
! overload.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2).
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = Dubiner_Triangle(order=1_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = OrthogonalBasis_Triangle(order=1_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle
  MODULE PROCEDURE Dubiner_Triangle2
END INTERFACE OrthogonalBasis_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Dubiner on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Use the declaration and description for this
! overload.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL Dubiner_Triangle_(order=1_I4B, xij=points, refTriangle="UNIT", &
!   ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL OrthogonalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle_
  MODULE PROCEDURE Dubiner_Triangle2_
END INTERFACE OrthogonalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                    VertexBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate VertexBasis triangle modes.
!
! Vertex modes have three columns; edge modes have pe1+pe2+pe3-3; cell modes
! have (order-1)(order-2)/2. Vertex modes are barycentric coordinates. Use
! edge orders >=1 and cell order >=3 when requesting cell modes.
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), 3).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,
! \quad\phi_{e_{ij}}\propto\lambda_i\lambda_j,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: VertexBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,3), y(3,3)
!
! points = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! y = VertexBasis_Triangle(xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Barycentric vertex partition")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate EdgeBasis triangle modes.
!
! Vertex modes have three columns; edge modes have pe1+pe2+pe3-3; cell modes
! have (order-1)(order-2)/2. Vertex modes are barycentric coordinates. Use
! edge orders >=1 and cell order >=3 when requesting cell modes.
!
! Arguments:
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), pe1 + pe2 + pe3 - 3).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,
! \quad\phi_{e_{ij}}\propto\lambda_i\lambda_j,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: EdgeBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,3), y(3,3)
!
! points = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! y = EdgeBasis_Triangle(pe1=2_I4B,pe2=2_I4B,pe3=2_I4B, xij=points, &
!   refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y)) < 1.0E-11_DFP, &
!   "Interior entity modes vanish at the vertices")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate CellBasis triangle modes.
!
! Vertex modes have three columns; edge modes have pe1+pe2+pe3-3; cell modes
! have (order-1)(order-2)/2. Vertex modes are barycentric coordinates. Use
! edge orders >=1 and cell order >=3 when requesting cell modes.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), INT((order - 1) * (order - 2) / 2)).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,
! \quad\phi_{e_{ij}}\propto\lambda_i\lambda_j,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: CellBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,3), y(3,1)
!
! points = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! y = CellBasis_Triangle(order=3_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y)) < 1.0E-11_DFP, &
!   "Interior entity modes vanish at the vertices")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   pe1 + pe2 + pe3 + INT((order - 1) *
! (order - 2) / 2)).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = HeirarchicalBasis_Triangle(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2),  3 * order + INT((order - 1) * (order - 2) /
! 2)).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = HeirarchicalBasis_Triangle(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL HeirarchicalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B, ans=y, nrow=nr, &
!   ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle1_( &
    order, pe1, pe2, pe3, xij, refTriangle, ans, nrow, ncol)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL HeirarchicalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B, ans=y, nrow=nr, &
!   ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle2_( &
    order, xij, refTriangle, ans, nrow, ncol)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   edgeOrient1 : INTEGER(I4B), INTENT(IN). Orientation sign for edge one.
!   edgeOrient2 : INTEGER(I4B), INTENT(IN). Orientation sign for edge two.
!   edgeOrient3 : INTEGER(I4B), INTENT(IN). Orientation sign for edge three.
!   faceOrient : INTEGER(I4B), INTENT(IN)(:). Two entry zero based rotation
! and sign orientation code.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL HeirarchicalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B, ans=y, nrow=nr, &
!   ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle3_( &
    order, pe1, pe2, pe3, xij, refTriangle, edgeOrient1, edgeOrient2, &
    edgeOrient3, faceOrient, ans, nrow, ncol)
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
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Triangle3_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(2). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL. Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2)).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad\sum_j\ell_j=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3)
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! y = LagrangeEvalAll_Triangle(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle1( &
    order, x, xij, refTriangle, coeff, firstCall, basisType) RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(2). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(:, :). Optional coefficient
! cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL. Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!
! Returns:
!   Updates xij(:, :), ans(:), tsize, coeff(:, :).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad\sum_j\ell_j=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3)
! INTEGER(I4B) :: nr,nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! CALL LagrangeEvalAll_Triangle_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle1_( &
    order, x, xij, ans, tsize, refTriangle, coeff, firstCall, basisType)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL. Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x, 2), SIZE(xij, 2)).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad\sum_j\ell_j=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3)
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! y = LagrangeEvalAll_Triangle(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle2( &
    order, x, xij, refTriangle, coeff, firstCall, basisType, alpha, beta, &
    lambda) RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(:, :). Optional coefficient
! cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL. Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates xij(:, :), ans(:, :), nrow, ncol, coeff(:, :).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad\sum_j\ell_j=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3)
! INTEGER(I4B) :: nr,nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! CALL LagrangeEvalAll_Triangle_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle2_( &
    order, x, xij, ans, nrow, ncol, refTriangle, coeff, firstCall, &
    basisType, alpha, beta, lambda)
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
!                                   LagrangeEvalAll_Triangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   coeff : REAL(DFP), INTENT(INOUT)(:, :). Optional coefficient cache; must
! match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT). Use .TRUE. to construct the coefficient cache,
! then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   xx : REAL(DFP), INTENT(INOUT)(:, :). Reusable point by basis workspace;
! shape must match the selected overload.
!   degree : INTEGER(I4B)(:, :). Monomial exponent table, with monomial rows
! and coordinate columns.
!
! Returns:
!   Updates xij(:, :), ans(:, :), nrow, ncol, coeff(:, :), xx(:, :).
!
! Formula:
! $$\ell_j(x_i)=\delta_{ij},\quad\sum_j\ell_j=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3)
! INTEGER(I4B) :: nr,nc
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! CALL LagrangeEvalAll_Triangle_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT", ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle3_( &
    order, x, xij, ans, nrow, ncol, refTriangle, coeff, firstCall, &
    basisType, xx, degree)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation; x(1, :) is x coord; x(2, :) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Number of rows and columns written to ans
    !! nrow = size(x, 2), points of evaluation
    !! ncol = size(xij, 2), number of interpolation points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle ! Biunit ! Unit
    REAL(DFP), INTENT(INOUT) :: coeff(:, :)
    !! coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomials *Default ! Jacobi=Dubiner ! Heirarchical
    REAL(DFP), INTENT(INOUT) :: xx(:, :)
  !! xx(SIZE(x, 2), SIZE(xij, 2))
    INTEGER(I4B) :: degree(:, :)
    ! degree(SIZE(xij, 2), 2)
  END SUBROUTINE LagrangeEvalAll_Triangle3_
END INTERFACE LagrangeEvalAll_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! order requests polynomial integration degree; nips requests an explicit
! supported point count. Output columns are points, coordinate rows come
! first, and the final row contains weights. The default Line domain is
! BIUNIT; Triangle requires refTriangle. Supply vertex xij to map coordinates
! and scale weights by the geometric Jacobian. Triangle economical rules are
! selected from Solin tables; the count selector currently ignores quadType.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Triangle(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Triangle(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT")
! CALL OK(ABS(SUM(q(3,:))-0.5_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(3,n))
! CALL QuadraturePoint_Triangle_(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! order requests polynomial integration degree; nips requests an explicit
! supported point count. Output columns are points, coordinate rows come
! first, and the final row contains weights. The default Line domain is
! BIUNIT; Triangle requires refTriangle. Supply vertex xij to map coordinates
! and scale weights by the geometric Jacobian. Triangle economical rules are
! selected from Solin tables; the count selector currently ignores quadType.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Triangle(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Triangle(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT")
! CALL OK(ABS(SUM(q(3,:))-0.5_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(3,n))
! CALL QuadraturePoint_Triangle_(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! order requests polynomial integration degree; nips requests an explicit
! supported point count. Output columns are points, coordinate rows come
! first, and the final row contains weights. The default Line domain is
! BIUNIT; Triangle requires refTriangle. Supply vertex xij to map coordinates
! and scale weights by the geometric Jacobian. Triangle economical rules are
! selected from Solin tables; the count selector currently ignores quadType.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Triangle(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Triangle(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT")
! CALL OK(ABS(SUM(q(3,:))-0.5_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(3,n))
! CALL QuadraturePoint_Triangle_(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! order requests polynomial integration degree; nips requests an explicit
! supported point count. Output columns are points, coordinate rows come
! first, and the final row contains weights. The default Line domain is
! BIUNIT; Triangle requires refTriangle. Supply vertex xij to map coordinates
! and scale weights by the geometric Jacobian. Triangle economical rules are
! selected from Solin tables; the count selector currently ignores quadType.
!
! Arguments:
!   nips : INTEGER(I4B), INTENT(IN)(1). Explicit quadrature point count; must
! be supported by the selected rule.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Triangle(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Triangle(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT")
! CALL OK(ABS(SUM(q(3,:))-0.5_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(3,n))
! CALL QuadraturePoint_Triangle_(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! order requests polynomial integration degree; nips requests an explicit
! supported point count. Output columns are points, coordinate rows come
! first, and the final row contains weights. The default Line domain is
! BIUNIT; Triangle requires refTriangle. Supply vertex xij to map coordinates
! and scale weights by the geometric Jacobian. Triangle economical rules are
! selected from Solin tables; the count selector currently ignores quadType.
!
! Arguments:
!   nips : INTEGER(I4B), INTENT(IN)(1). Explicit quadrature point count; must
! be supported by the selected rule.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
! USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Triangle(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Triangle(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT")
! CALL OK(ABS(SUM(q(3,:))-0.5_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(3,n))
! CALL QuadraturePoint_Triangle_(order=2_I4B, quadType=GaussLegendreQP, &
!   refTriangle="UNIT", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL. Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x, 2), SIZE(xij, 2), 2).
!
! Formula:
! $$\sum_j\nabla\ell_j=0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeGradientEvalAll_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3,2)
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! y = LagrangeGradientEvalAll_Triangle(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the nodal triangle basis.
!
! Keep evaluation points separate from the INOUT node array xij. coeff can
! cache basis coefficients: pass firstCall=.TRUE. on the first call and
! .FALSE. on later calls with the same nodes and basis. Line firstCall is an
! input flag, so set it explicitly rather than assuming it is changed
! internally.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   dim2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   dim3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(:, :). Optional coefficient
! cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL. Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates xij(:, :), ans(:, :, :), dim1, dim2, dim3, coeff(:, :).
!
! Formula:
! $$\sum_j\nabla\ell_j=0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE TriangleInterpolationUtility, ONLY: LagrangeGradientEvalAll_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,3), points(2,3)
! REAL(DFP) :: y(3,3,2)
! INTEGER(I4B) :: nr,nc,nd
!
! nodes = RESHAPE([0.0_DFP,0.0_DFP,1.0_DFP, &
!   0.0_DFP,0.0_DFP,1.0_DFP],[2,3])
! points = nodes
! CALL LagrangeGradientEvalAll_Triangle_(order=1_I4B, x=points, &
!   xij=nodes, basisType=poly%Monomial, refTriangle="UNIT", ans=y, &
!   dim1=nr, dim2=nc, dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Triangle1_( &
    order, x, xij, ans, dim1, dim2, dim3, refTriangle, coeff, firstCall, &
    basisType, alpha, beta, lambda)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasisGradient on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   pe1 + pe2 + pe3 + INT((order - 1) *
! (order - 2) / 2), 2).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3,2)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = HeirarchicalBasisGradient_Triangle(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Triangle
  MODULE FUNCTION HeirarchicalBasisGradient_Triangle1( &
    order, pe1, pe2, pe3, xij, refTriangle) RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasisGradient on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   tsize1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   tsize2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   tsize3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!
! Returns:
!   Updates ans(:, :, :), tsize1, tsize2, tsize3.
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3,2)
! INTEGER(I4B) :: nr,nc,nd
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL HeirarchicalBasisGradient_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B, ans=y, &
!   tsize1=nr,tsize2=nc,tsize3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Triangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Triangle1_( &
    order, pe1, pe2, pe3, xij, refTriangle, ans, tsize1, tsize2, tsize3)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HeirarchicalBasisGradient on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. The public spelling
! Heirarchical is retained. Vertex modes precede boundary edge modes, followed
! by cell modes. Oriented subroutine overloads take edge signs and a two entry
! rotation/sign faceOrient array.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge one.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge two.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on edge three.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   edgeOrient1 : INTEGER(I4B), INTENT(IN). Orientation sign for edge one.
!   edgeOrient2 : INTEGER(I4B), INTENT(IN). Orientation sign for edge two.
!   edgeOrient3 : INTEGER(I4B), INTENT(IN). Orientation sign for edge three.
!   faceOrient : INTEGER(I4B), INTENT(IN)(:). Two entry zero based rotation
! and sign orientation code.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   tsize1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   tsize2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   tsize3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!
! Returns:
!   Updates ans(:, :, :), tsize1, tsize2, tsize3.
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad \sum_i\phi_{v_i}=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3,2)
! INTEGER(I4B) :: nr,nc,nd
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL HeirarchicalBasisGradient_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", pe1=1_I4B, pe2=1_I4B, pe3=1_I4B, ans=y, &
!   tsize1=nr,tsize2=nc,tsize3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Triangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Triangle2_( &
    order, pe1, pe2, pe3, xij, edgeOrient1, edgeOrient2, edgeOrient3, &
    faceOrient, refTriangle, ans, tsize1, tsize2, tsize3)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasisGradient on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (order + 1) * (order + 2) / 2, 2).
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3,2)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = OrthogonalBasisGradient_Triangle(order=1_I4B, xij=points, &
!   refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1,:))) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! CALL OK(MAXVAL(ABS(y(:,2,2)-3.0_DFP)) < 1.0E-10_DFP, &
!   "Derivative of the first nonconstant UNIT mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasisGradient on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. Modes are ordered
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   tsize1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   tsize2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   tsize3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!
! Returns:
!   Updates ans(:, :, :), tsize1, tsize2, tsize3.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3,2)
! INTEGER(I4B) :: nr,nc,nd
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL OrthogonalBasisGradient_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", ans=y, tsize1=nr,tsize2=nc,tsize3=nd)
! CALL OK(MAXVAL(ABS(y(:,1,:))) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! CALL OK(MAXVAL(ABS(y(:,2,2)-3.0_DFP)) < 1.0E-10_DFP, &
!   "Derivative of the first nonconstant UNIT mode")
! END PROGRAM main
!```

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
