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
! summary: TetrahedronInterpolationUtility geometry and interpolation
! utilities.
!
! Scalar spaces have (p+1)(p+2)(p+3)/6 modes. Cartesian coordinates have three
! rows and barycentric coordinates four, with points in columns. The
! uniform-order hierarchical gradient subroutine specific has no
! implementation; use its explicit entity-order overload. Orthogonal analytic
! gradient assembly currently overwrites a component; its example demonstrates
! a finite-difference workaround instead.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$u(x,y,z)=\sum_{i+j+k\le p}c_{ijk}\phi_{ijk}(x,y,z)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

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
PUBLIC :: EquidistancePoint_Tetrahedron_
PUBLIC :: LagrangeCoeff_Tetrahedron
PUBLIC :: LagrangeCoeff_Tetrahedron_
PUBLIC :: InterpolationPoint_Tetrahedron
PUBLIC :: InterpolationPoint_Tetrahedron_

PUBLIC :: OrthogonalBasis_Tetrahedron
PUBLIC :: OrthogonalBasis_Tetrahedron_

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
PUBLIC :: HeirarchicalBasis_Tetrahedron_

PUBLIC :: FacetConnectivity_Tetrahedron
PUBLIC :: EdgeConnectivity_Tetrahedron
PUBLIC :: GetVertexDOF_Tetrahedron
PUBLIC :: GetEdgeDOF_Tetrahedron
PUBLIC :: GetFacetDOF_Tetrahedron
PUBLIC :: GetCellDOF_Tetrahedron

PUBLIC :: LagrangeEvalAll_Tetrahedron
PUBLIC :: LagrangeEvalAll_Tetrahedron_

PUBLIC :: QuadraturePoint_Tetrahedron
PUBLIC :: QuadraturePoint_Tetrahedron_
PUBLIC :: QuadratureNumber_Tetrahedron

PUBLIC :: RefElemDomain_Tetrahedron
PUBLIC :: LagrangeGradientEvalAll_Tetrahedron
PUBLIC :: LagrangeGradientEvalAll_Tetrahedron_

PUBLIC :: HeirarchicalBasisGradient_Tetrahedron
PUBLIC :: HeirarchicalBasisGradient_Tetrahedron_

PUBLIC :: OrthogonalBasisGradient_Tetrahedron
PUBLIC :: OrthogonalBasisGradient_Tetrahedron_

PUBLIC :: GetTotalDOF_Tetrahedron
PUBLIC :: GetTotalInDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query nodal dimensions, exponent tables and reference domains.
!
! Exponent tables have monomial rows and coordinate columns. Use positive
! degrees; interior count formulas are polynomial expressions, not guarded
! combinatorial functions at invalid low orders. Current total-count
! dispatchers ignore continuity/family strings and return scalar polynomial
! counts.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   baseContinuity : CHARACTER(*), INTENT(IN). Basis continuity string, for
! example H1.
!   baseInterpolation : CHARACTER(*), INTENT(IN). Interpolation family string,
! for example Lagrange.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)(q+1),\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Tetrahedron(order, baseContinuity, &
                                               baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeInDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query nodal dimensions, exponent tables and reference domains.
!
! Exponent tables have monomial rows and coordinate columns. Use positive
! degrees; interior count formulas are polynomial expressions, not guarded
! combinatorial functions at invalid low orders. Current total-count
! dispatchers ignore continuity/family strings and return scalar polynomial
! counts.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   baseContinuity : CHARACTER(*), INTENT(IN). Basis continuity string, for
! example H1.
!   baseInterpolation : CHARACTER(*), INTENT(IN). Interpolation family string,
! for example Lagrange.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)(q+1),\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Tetrahedron(order, baseContinuity, &
                                                baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalInDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 RefElemDomain_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query nodal dimensions, exponent tables and reference domains.
!
! Exponent tables have monomial rows and coordinate columns. Use positive
! degrees; interior count formulas are polynomial expressions, not guarded
! combinatorial functions at invalid low orders. Current total-count
! dispatchers ignore continuity/family strings and return scalar polynomial
! counts.
!
! Arguments:
!   baseContinuity : CHARACTER(*), INTENT(IN). Basis continuity string, for
! example H1.
!   baseInterpol : CHARACTER(*), INTENT(IN). Interpolation family string, for
! example Lagrange or Hierarchy.
!
! Returns:
!   ans : TYPE(String).
!
! Formula:
! $$N_Q=(p+1)(q+1),\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count tetrahedral hierarchical entity modes.
!
! Edge and facet overloads accept either a uniform p or separate orders for
! all entities. Use positive orders; edge modes begin at degree 2, facet modes
! at degree 3 and cell modes at degree 4.
!
! Arguments:
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+6(p-1)+4(p-1)(p-2)/2+(p-1)(p-2)(p-3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
! CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
! CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
! CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
! n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
!   +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
! CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetVertexDOF_Tetrahedron() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION GetVertexDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count tetrahedral hierarchical entity modes.
!
! Edge and facet overloads accept either a uniform p or separate orders for
! all entities. Use positive orders; edge modes begin at degree 2, facet modes
! at degree 3 and cell modes at degree 4.
!
! Arguments:
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+6(p-1)+4(p-1)(p-2)/2+(p-1)(p-2)(p-3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
! CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
! CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
! CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
! n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
!   +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
! CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count tetrahedral hierarchical entity modes.
!
! Edge and facet overloads accept either a uniform p or separate orders for
! all entities. Use positive orders; edge modes begin at degree 2, facet modes
! at degree 3 and cell modes at degree 4.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+6(p-1)+4(p-1)(p-2)/2+(p-1)(p-2)(p-3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
! CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
! CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
! CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
! n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
!   +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
! CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count tetrahedral hierarchical entity modes.
!
! Edge and facet overloads accept either a uniform p or separate orders for
! all entities. Use positive orders; edge modes begin at degree 2, facet modes
! at degree 3 and cell modes at degree 4.
!
! Arguments:
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+6(p-1)+4(p-1)(p-2)/2+(p-1)(p-2)(p-3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
! CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
! CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
! CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
! n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
!   +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
! CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count tetrahedral hierarchical entity modes.
!
! Edge and facet overloads accept either a uniform p or separate orders for
! all entities. Use positive orders; edge modes begin at degree 2, facet modes
! at degree 3 and cell modes at degree 4.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+6(p-1)+4(p-1)(p-2)/2+(p-1)(p-2)(p-3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
! CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
! CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
! CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
! n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
!   +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
! CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count tetrahedral hierarchical entity modes.
!
! Edge and facet overloads accept either a uniform p or separate orders for
! all entities. Use positive orders; edge modes begin at degree 2, facet modes
! at degree 3 and cell modes at degree 4.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+6(p-1)+4(p-1)(p-2)/2+(p-1)(p-2)(p-3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
! CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
! CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
! CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
! n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
!   +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
! CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return tetrahedral basis edge and facet vertex connectivity.
!
! Both family strings are required. Edge connectivity currently ignores them.
! Facet connectivity distinguishes hierarchy/orthogonal family names from the
! default nodal order; the continuity string currently has no effect. Do not
! silently interchange nodal and modal facet orientations.
!
! Arguments:
!   baseInterpol : CHARACTER(*), INTENT(IN). Interpolation family string, for
! example Lagrange or Hierarchy.
!   baseContinuity : CHARACTER(*), INTENT(IN). Basis continuity string, for
! example H1.
!
! Returns:
!   ans : INTEGER(I4B)(3, 4).
!
! Formula:
! $$\partial T=f_{132}\cup f_{124}\cup f_{143}\cup f_{234}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: EdgeConnectivity_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: FacetConnectivity_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: edges(2,6),nodal(3,4),modal(3,4)
!
! edges = EdgeConnectivity_Tetrahedron("Lagrange","H1")
! nodal = FacetConnectivity_Tetrahedron("Lagrange","H1")
! modal = FacetConnectivity_Tetrahedron("Hierarchy","H1")
! CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
!   ALL(edges(:,6) == [3_I4B,4_I4B]), "Six pairwise vertex edges")
! CALL OK(ALL(nodal(:,1) == [1_I4B,3_I4B,2_I4B]), "Nodal facet orientation")
! CALL OK(ALL(modal(:,1) == [1_I4B,2_I4B,3_I4B]), "Modal facet orientation")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return tetrahedral basis edge and facet vertex connectivity.
!
! Both family strings are required. Edge connectivity currently ignores them.
! Facet connectivity distinguishes hierarchy/orthogonal family names from the
! default nodal order; the continuity string currently has no effect. Do not
! silently interchange nodal and modal facet orientations.
!
! Arguments:
!   baseInterpol : CHARACTER(*), INTENT(IN). Interpolation family string, for
! example Lagrange or Hierarchy.
!   baseContinuity : CHARACTER(*), INTENT(IN). Basis continuity string, for
! example H1.
!
! Returns:
!   ans : INTEGER(I4B)(2, 6).
!
! Formula:
! $$\partial T=f_{132}\cup f_{124}\cup f_{143}\cup f_{234}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: EdgeConnectivity_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: FacetConnectivity_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: edges(2,6),nodal(3,4),modal(3,4)
!
! edges = EdgeConnectivity_Tetrahedron("Lagrange","H1")
! nodal = FacetConnectivity_Tetrahedron("Lagrange","H1")
! modal = FacetConnectivity_Tetrahedron("Hierarchy","H1")
! CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
!   ALL(edges(:,6) == [3_I4B,4_I4B]), "Six pairwise vertex edges")
! CALL OK(ALL(nodal(:,1) == [1_I4B,3_I4B,2_I4B]), "Nodal facet orientation")
! CALL OK(ALL(modal(:,1) == [1_I4B,2_I4B,3_I4B]), "Modal facet orientation")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query nodal dimensions, exponent tables and reference domains.
!
! Exponent tables have monomial rows and coordinate columns. Use positive
! degrees; interior count formulas are polynomial expressions, not guarded
! combinatorial functions at invalid low orders. Current total-count
! dispatchers ignore continuity/family strings and return scalar polynomial
! counts.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE(:, :).
!
! Formula:
! $$N_Q=(p+1)(q+1),\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE LagrangeDegree_Tetrahedron_(order, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeDegree_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query nodal dimensions, exponent tables and reference domains.
!
! Exponent tables have monomial rows and coordinate columns. Use positive
! degrees; interior count formulas are polynomial expressions, not guarded
! combinatorial functions at invalid low orders. Current total-count
! dispatchers ignore continuity/family strings and return scalar polynomial
! counts.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)(q+1),\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 LagrangeInDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query nodal dimensions, exponent tables and reference domains.
!
! Exponent tables have monomial rows and coordinate columns. Use positive
! degrees; interior count formulas are polynomial expressions, not guarded
! combinatorial functions at invalid low orders. Current total-count
! dispatchers ignore continuity/family strings and return scalar polynomial
! counts.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)(q+1),\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Tetrahedron(4_I4B)
! CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
! domain = RefElemDomain_Tetrahedron("H1","Lagrange")
! CALL OK(domain%chars() == "UNIT", "Reference domain")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct interior tetrahedron interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
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
! $$x_i=i/p,\ y_j=j/p,\ z_k=k/p,\quad i+j+k\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: EquidistanceInPoint_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = EquidistanceInPoint_Tetrahedron(order=4_I4B)
! CALL OK(ALL(SHAPE(nodes) == [3,1]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes-(0.25_DFP))) &
!   < 1.0E-11_DFP, "Single interior point is the centroid")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct tetrahedron interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
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
! $$x_i=i/p,\ y_j=j/p,\ z_k=k/p,\quad i+j+k\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: EquidistancePoint_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = EquidistancePoint_Tetrahedron(order=2_I4B)
! CALL OK(ALL(SHAPE(nodes) == [3,10]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct tetrahedron interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
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
! $$x_i=i/p,\ y_j=j/p,\ z_k=k/p,\quad i+j+k\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: EquidistancePoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,10)
! INTEGER(I4B) :: nr,nc
!
! CALL EquidistancePoint_Tetrahedron_(order=2_I4B,ans=nodes,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 10, &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE EquidistancePoint_Tetrahedron_(order, xij, ans, nrow, &
                                                   ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! returned coordinates in $x_{iJ}$ format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE EquidistancePoint_Tetrahedron_
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct tetrahedron interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node family selector.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(3, 4). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_i=i/p,\ y_j=j/p,\ z_k=k/p,\quad i+j+k\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE TetrahedronInterpolationUtility, ONLY: InterpolationPoint_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = InterpolationPoint_Tetrahedron(order=2_I4B, &
!   ipType=Equidistance,layout="VEFC")
! CALL OK(ALL(SHAPE(nodes) == [3,10]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION InterpolationPoint_Tetrahedron(order, ipType, layout, &
                                         xij, alpha, beta, lambda) RESULT(ans)
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
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct tetrahedron interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node family selector.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(3, 4). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_i=i/p,\ y_j=j/p,\ z_k=k/p,\quad i+j+k\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE TetrahedronInterpolationUtility, ONLY: InterpolationPoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,10)
! INTEGER(I4B) :: nr,nc
!
! CALL InterpolationPoint_Tetrahedron_(order=2_I4B,ipType=Equidistance, &
!   layout="VEFC",ans=nodes,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 10, &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE InterpolationPoint_Tetrahedron_(order, ipType, ans, &
                                 nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation type
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !!
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
  END SUBROUTINE InterpolationPoint_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! c = LagrangeCoeff_Tetrahedron(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT")
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! c = LagrangeCoeff_Tetrahedron(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT")
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! c = LagrangeCoeff_Tetrahedron(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT")
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! c = LagrangeCoeff_Tetrahedron(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT")
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! CALL LagrangeCoeff_Tetrahedron_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT",ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron1_(order, i, xij, ans, tsize)
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
  END SUBROUTINE LagrangeCoeff_Tetrahedron1_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! CALL LagrangeCoeff_Tetrahedron_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT",ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron2_(order, i, v, isVandermonde, &
                                                ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP), INTENT(INOUT) :: ans(:)
    ! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Tetrahedron2_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! CALL LagrangeCoeff_Tetrahedron_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT",ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron3_(order, i, v, ipiv, ans, tsize)
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
  END SUBROUTINE LagrangeCoeff_Tetrahedron3_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute nodal polynomial coefficient columns.
!
! The i overload returns one coefficient column. v/isVandermonde uses a
! Vandermonde matrix, while v/ipiv uses its LU factors and pivot indices.
! Quadrangle directional overloads allow anisotropic degrees. The example uses
! monomials and verifies the cardinal interpolation identity.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeCoeff_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! CALL LagrangeCoeff_Tetrahedron_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT",ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Tetrahedron(1_I4B)
! DO j = 1,4
!   DO i = 1,4
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1,4
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "V times coefficients is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron4_(order, xij, basisType, &
                         refTetrahedron, alpha, beta, lambda, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Tetrahedron4_
END INTERFACE LagrangeCoeff_Tetrahedron_

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
  MODULE SUBROUTINE Isaac_Tetrahedron(order, ipType, ans, nrow, ncol, &
                                      layout, xij, alpha, beta, lambda)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! xij coordinates
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns
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
  END SUBROUTINE Isaac_Tetrahedron
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
  MODULE RECURSIVE SUBROUTINE IJK2VEFC_Tetrahedron(xi, eta, zeta, temp, &
                                                   order, N)
    REAL(DFP), INTENT(IN) :: xi(:, :, :)
    REAL(DFP), INTENT(IN) :: eta(:, :, :)
    REAL(DFP), INTENT(IN) :: zeta(:, :, :)
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: N
  END SUBROUTINE IJK2VEFC_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate tetrahedral orthogonal basis.
!
! There are (order+1)(order+2)(order+3)/6 total-degree modes, ordered with r
! varying fastest, then q, then p. UNIT and BIUNIT are supported exact domain
! strings. Use interior points to avoid singular collapsed-coordinate
! denominators. Separate x,y,z overloads evaluate tensor grids in collapsed
! cube coordinates.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   (order + 1) * (order + 2) * (order + 3)
! / 6).
!
! Formula:
! $$\phi_{pqr}=P_p(a)((1-b)/2)^p
! P_q^{(2p+1,0)}(b)((1-c)/2)^{p+q}P_r^{(2p+2q+2,0)}(c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),v(2,4)
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! v = OrthogonalBasis_Tetrahedron(order=1_I4B,xij=x,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-11_DFP, "Constant mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(4.0_DFP*x(3,:)-1.0_DFP))) &
!   < 1.0E-11_DFP, "First nonconstant UNIT mode")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate tetrahedral orthogonal basis.
!
! There are (order+1)(order+2)(order+3)/6 total-degree modes, ordered with r
! varying fastest, then q, then p. UNIT and BIUNIT are supported exact domain
! strings. Use interior points to avoid singular collapsed-coordinate
! denominators. Separate x,y,z overloads evaluate tensor grids in collapsed
! cube coordinates.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{pqr}=P_p(a)((1-b)/2)^p
! P_q^{(2p+1,0)}(b)((1-c)/2)^{p+q}P_r^{(2p+2q+2,0)}(c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! CALL OrthogonalBasis_Tetrahedron_(order=1_I4B,xij=x, &
!   refTetrahedron="UNIT",ans=v,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-11_DFP, "Constant mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(4.0_DFP*x(3,:)-1.0_DFP))) &
!   < 1.0E-11_DFP, "First nonconstant UNIT mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Tetrahedron_
  MODULE SUBROUTINE OrthogonalBasis_Tetrahedron1_(order, xij, &
                                              refTetrahedron, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns
   !! nrow = SIZE(xij, 2)
   !! ncol = (order + 1) * (order + 2) * (order + 3) / 6
  END SUBROUTINE OrthogonalBasis_Tetrahedron1_
END INTERFACE OrthogonalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                                OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate tetrahedral orthogonal basis.
!
! There are (order+1)(order+2)(order+3)/6 total-degree modes, ordered with r
! varying fastest, then q, then p. UNIT and BIUNIT are supported exact domain
! strings. Use interior points to avoid singular collapsed-coordinate
! denominators. Separate x,y,z overloads evaluate tensor grids in collapsed
! cube coordinates.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!   z : REAL(DFP), INTENT(IN)(:). Third collapsed coordinate vector for a
! tensor point grid.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(x) * SIZE(y) * SIZE(z),   (order + 1) * (order +
! 2) * (order + 3) / 6).
!
! Formula:
! $$\phi_{pqr}=P_p(a)((1-b)/2)^p
! P_q^{(2p+1,0)}(b)((1-c)/2)^{p+q}P_r^{(2p+2q+2,0)}(c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),v(2,4)
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! v = OrthogonalBasis_Tetrahedron(order=1_I4B,xij=x,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-11_DFP, "Constant mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(4.0_DFP*x(3,:)-1.0_DFP))) &
!   < 1.0E-11_DFP, "First nonconstant UNIT mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Tetrahedron
  MODULE FUNCTION OrthogonalBasis_Tetrahedron2(order, x, y, z, &
                                               refTetrahedron) RESULT(ans)
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate tetrahedral orthogonal basis.
!
! There are (order+1)(order+2)(order+3)/6 total-degree modes, ordered with r
! varying fastest, then q, then p. UNIT and BIUNIT are supported exact domain
! strings. Use interior points to avoid singular collapsed-coordinate
! denominators. Separate x,y,z overloads evaluate tensor grids in collapsed
! cube coordinates.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!   z : REAL(DFP), INTENT(IN)(:). Third collapsed coordinate vector for a
! tensor point grid.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{pqr}=P_p(a)((1-b)/2)^p
! P_q^{(2p+1,0)}(b)((1-c)/2)^{p+q}P_r^{(2p+2q+2,0)}(c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! CALL OrthogonalBasis_Tetrahedron_(order=1_I4B,xij=x, &
!   refTetrahedron="UNIT",ans=v,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-11_DFP, "Constant mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(4.0_DFP*x(3,:)-1.0_DFP))) &
!   < 1.0E-11_DFP, "First nonconstant UNIT mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Tetrahedron_
  MODULE SUBROUTINE OrthogonalBasis_Tetrahedron2_(order, x, y, z, &
                                              refTetrahedron, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x) * SIZE(y) * SIZE(z)
    !! ncol = (order + 1) * (order + 2) * (order + 3) / 6
  END SUBROUTINE OrthogonalBasis_Tetrahedron2_
END INTERFACE OrthogonalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate barycentric vertex tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   lambda : REAL(DFP), INTENT(IN)(:, :). Barycentric coordinates with four
! rows and point columns; each column sums to one.
!
! Returns:
!   ans : REAL(DFP)(SIZE(lambda, 2), 4).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: BarycentricVertexBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(4,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = 0.0_DFP
! DO i = 1,4
!   points(i,i) = 1.0_DFP
! END DO
! v = BarycentricVertexBasis_Tetrahedron(lambda=points)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear vertex partition of unity")
! CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricVertexBasis_Tetrahedron_(lambda, ans, &
                                                             nrow, ncol)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentrix coords
    !! number of rows = 4
    !! number of columns = number of points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(lambda, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricVertexBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Gradient of vertex basis in terms of barycentric coord

INTERFACE
  MODULE PURE FUNCTION BarycentricVertexBasisGradient_Tetrahedron(lambda) &
    RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate barycentric edge tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained. Precomputed phi overloads expect
! Lobatto kernel values with second index starting at zero; row blocks encode
! the required barycentric differences.
!
! Arguments:
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   lambda : REAL(DFP), INTENT(IN)(:, :). Barycentric coordinates with four
! rows and point columns; each column sums to one.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(lambda, 2),   pe1 + pe2 + pe3 + pe4 + pe5 + pe6 -
! 6).
!
! Formula:
! $$\phi_e\propto\lambda_i\lambda_j,
! \quad\phi_f\propto\lambda_i\lambda_j\lambda_k,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3\lambda_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: BarycentricEdgeBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(4,4),v(4,6)
! INTEGER(I4B) :: i
!
! points = 0.0_DFP
! DO i = 1,4
!   points(i,i) = 1.0_DFP
! END DO
! v = BarycentricEdgeBasis_Tetrahedron(pe1=2_I4B,pe2=2_I4B,pe3=2_I4B, &
!   pe4=2_I4B,pe5=2_I4B,pe6=2_I4B,lambda=points)
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
!   "Higher entity modes vanish at every vertex")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Tetrahedron(pe1, pe2, pe3, pe4, &
                                                 pe5, pe6, lambda) RESULT(ans)
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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricEdgeBasis_Tetrahedron_(pe1, pe2, pe3, &
                                       pe4, pe5, pe6, lambda, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(lambda, 2)
    !! ncol = pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricEdgeBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                           BarycentricEdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Oct 2022
! summary: Evaluate the edge basis on Tetrahedron in terms of barycentric

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Tetrahedron2(pe1, pe2, pe3, &
                                       pe4, pe5, pe6, lambda, phi) RESULT(ans)
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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricEdgeBasis_Tetrahedron2_(pe1, pe2, pe3, &
                                  pe4, pe5, pe6, lambda, phi, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(lambda, 2)
    !! ncol = pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricEdgeBasis_Tetrahedron2_
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate barycentric facet tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained. Precomputed phi overloads expect
! Lobatto kernel values with second index starting at zero; row blocks encode
! the required barycentric differences.
!
! Arguments:
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   lambda : REAL(DFP), INTENT(IN)(:, :). Barycentric coordinates with four
! rows and point columns; each column sums to one.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(lambda, 2),     (ps1 - 1) * (ps1 - 2) / 2    +
! (ps2 - 1) * (ps2 - 2) / 2    + (ps3 - 1) * (ps3 - 2) / 2    + (ps4 - 1) *
! (ps4 - 2) / 2).
!
! Formula:
! $$\phi_e\propto\lambda_i\lambda_j,
! \quad\phi_f\propto\lambda_i\lambda_j\lambda_k,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3\lambda_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: BarycentricFacetBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(4,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = 0.0_DFP
! DO i = 1,4
!   points(i,i) = 1.0_DFP
! END DO
! v = BarycentricFacetBasis_Tetrahedron(ps1=3_I4B,ps2=3_I4B,ps3=3_I4B, &
!   ps4=3_I4B,lambda=points)
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
!   "Higher entity modes vanish at every vertex")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricFacetBasis_Tetrahedron_(ps1, ps2, ps3, &
                                                 ps4, lambda, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(lambda, 2)
    !! ncol = (ps1 - 1) * (ps1 - 2) / 2  + (ps2 - 1) * (ps2 - 2) / 2  &
    !! + (ps3 - 1) * (ps3 - 2) / 2   + (ps4 - 1) * (ps4 - 2) / 2
  END SUBROUTINE BarycentricFacetBasis_Tetrahedron_
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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricFacetBasis_Tetrahedron2_(ps1, ps2, ps3, &
                                            ps4, lambda, phi, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(lambda, 2)
    !! ncol = (ps1 - 1) * (ps1 - 2) / 2  + (ps2 - 1) * (ps2 - 2) / 2  &
    !! + (ps3 - 1) * (ps3 - 2) / 2  + (ps4 - 1) * (ps4 - 2) / 2
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricFacetBasis_Tetrahedron2_
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate barycentric cell tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained. Precomputed phi overloads expect
! Lobatto kernel values with second index starting at zero; row blocks encode
! the required barycentric differences.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   lambda : REAL(DFP), INTENT(IN)(:, :). Barycentric coordinates with four
! rows and point columns; each column sums to one.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(lambda, 2),   (pb - 1) * (pb - 2) * (pb - 3) /
! 6_I4B).
!
! Formula:
! $$\phi_e\propto\lambda_i\lambda_j,
! \quad\phi_f\propto\lambda_i\lambda_j\lambda_k,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3\lambda_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: BarycentricCellBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(4,4),v(4,1)
! INTEGER(I4B) :: i
!
! points = 0.0_DFP
! DO i = 1,4
!   points(i,i) = 1.0_DFP
! END DO
! v = BarycentricCellBasis_Tetrahedron(pb=4_I4B,lambda=points)
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
!   "Higher entity modes vanish at every vertex")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCellBasis_Tetrahedron_(pb, lambda, ans, &
                                                           nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on  facet parallel to xy
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to 4
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(lambda, 2)
    !! ncol = (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B
  END SUBROUTINE BarycentricCellBasis_Tetrahedron_
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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCellBasis_Tetrahedron2_(pb, lambda, phi, &
                                                            ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(lambda, 2)
    !! ncol = (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B
  END SUBROUTINE BarycentricCellBasis_Tetrahedron2_
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate barycentric heirarchical tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   lambda : REAL(DFP), INTENT(IN)(:, :). Barycentric coordinates with four
! rows and point columns; each column sums to one.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(lambda, 2),   4   + pe1 + pe2 + pe3 + pe4 + pe5 +
! pe6 - 6   + (ps1 - 1) * (ps1 - 2) / 2    + (ps2 - 1) * (ps2 - 2) / 2    +
! (ps3 - 1) * (ps3 - 2) / 2    + (ps4 - 1) * (ps4 - 2) / 2   + (order - 1) *
! (order - 2) * (order - 3) / 6_I4B).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: BarycentricHeirarchicalBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(4,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = 0.0_DFP
! DO i = 1,4
!   points(i,i) = 1.0_DFP
! END DO
! v = BarycentricHeirarchicalBasis_Tetrahedron(order=1_I4B,lambda=points)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear vertex partition of unity")
! CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
! END PROGRAM main
!```

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasis_Tetrahedron1(order, &
         pe1, pe2, pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, lambda) RESULT(ans)
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
!
!----------------------------------------------------------------------------

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron1_(order, &
    pe1, pe2, pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, lambda, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(lambda, 2)
    !! ncol = 4  + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
    !!  + (ps1 - 1) * (ps1 - 2) / 2  + (ps2 - 1) * (ps2 - 2) / 2   &
    !! + (ps3 - 1) * (ps3 - 2) / 2   + (ps4 - 1) * (ps4 - 2) / 2 &
    !! + (order - 1) * (order - 2) * (order - 3) / 6_I4B
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron1_
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate barycentric heirarchical tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   lambda : REAL(DFP), INTENT(IN)(:, :). Barycentric coordinates with four
! rows and point columns; each column sums to one.
!
! Returns:
!   ans : REAL(DFP)(SIZE(lambda, 2),  (order + 1) * (order + 2) * (order + 3)
! / 6_I4B).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: BarycentricHeirarchicalBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(4,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = 0.0_DFP
! DO i = 1,4
!   points(i,i) = 1.0_DFP
! END DO
! v = BarycentricHeirarchicalBasis_Tetrahedron(order=1_I4B,lambda=points)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear vertex partition of unity")
! CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
! END PROGRAM main
!```

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION &
    BarycentricHeirarchicalBasis_Tetrahedron2(order, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 4
    !! number of cols = number of points
    REAL(DFP) :: ans(SIZE(lambda, 2), &
                     (order + 1) * (order + 2) * (order + 3) / 6_I4B)
  END FUNCTION BarycentricHeirarchicalBasis_Tetrahedron2
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron2_( &
    order, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 4
    !! number of cols = number of points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow =  SIZE(lambda, 2)
    !! ncol = (order + 1) * (order + 2) * (order + 3) / 6_I4B
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron2_
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                         BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Gradient of heirarchical basis in terms of barycentric coord

INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron1( &
  order, pe1, pe2, pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, lambda) RESULT(ans)
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate vertex tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), 4).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: VertexBasis_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(3,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = RefCoord_Tetrahedron("UNIT")
! v = VertexBasis_Tetrahedron(xij=points,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear vertex partition of unity")
! CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE VertexBasis_Tetrahedron_(xij, refTetrahedron, &
                                                  ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! Unit or biunit
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! ans(SIZE(xij, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END SUBROUTINE VertexBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     EdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate edge tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6).
!
! Formula:
! $$\phi_e\propto\lambda_i\lambda_j,
! \quad\phi_f\propto\lambda_i\lambda_j\lambda_k,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3\lambda_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: EdgeBasis_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(3,4),v(4,6)
! INTEGER(I4B) :: i
!
! points = RefCoord_Tetrahedron("UNIT")
! v = EdgeBasis_Tetrahedron(pe1=2_I4B,pe2=2_I4B,pe3=2_I4B,pe4=2_I4B, &
!   pe5=2_I4B,pe6=2_I4B,xij=points,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
!   "Higher entity modes vanish at every vertex")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE EdgeBasis_Tetrahedron_(pe1, pe2, pe3, pe4, pe5, &
                                    pe6, xij, refTetrahedron, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xij, 2)
    !! ncol = pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6
  END SUBROUTINE EdgeBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     FacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate facet tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),     (ps1 - 1) * (ps1 - 2) / 2    + (ps2 -
! 1) * (ps2 - 2) / 2    + (ps3 - 1) * (ps3 - 2) / 2    + (ps4 - 1) * (ps4 - 2)
! / 2).
!
! Formula:
! $$\phi_e\propto\lambda_i\lambda_j,
! \quad\phi_f\propto\lambda_i\lambda_j\lambda_k,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3\lambda_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: FacetBasis_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(3,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = RefCoord_Tetrahedron("UNIT")
! v = FacetBasis_Tetrahedron(ps1=3_I4B,ps2=3_I4B,ps3=3_I4B,ps4=3_I4B, &
!   xij=points,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
!   "Higher entity modes vanish at every vertex")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE FacetBasis_Tetrahedron_(ps1, ps2, ps3, ps4, xij, &
                                              refTetrahedron, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xij, 2)
    !! ncol = (ps1 - 1) * (ps1 - 2) / 2  + (ps2 - 1) * (ps2 - 2) / 2  &
    !! + (ps3 - 1) * (ps3 - 2) / 2  + (ps4 - 1) * (ps4 - 2) / 2
  END SUBROUTINE FacetBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     CellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate cell tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   (pb - 1) * (pb - 2) * (pb - 3) /
! 6_I4B).
!
! Formula:
! $$\phi_e\propto\lambda_i\lambda_j,
! \quad\phi_f\propto\lambda_i\lambda_j\lambda_k,
! \quad\phi_c\propto\lambda_1\lambda_2\lambda_3\lambda_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: CellBasis_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(3,4),v(4,1)
! INTEGER(I4B) :: i
!
! points = RefCoord_Tetrahedron("UNIT")
! v = CellBasis_Tetrahedron(pb=4_I4B,xij=points,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
!   "Higher entity modes vanish at every vertex")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE CellBasis_Tetrahedron_(pb, xij, refTetrahedron, &
                                                ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order in cell
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! number of rows and columns written to ans
  !! nrow = SIZE(xij, 2)
  !! ncol = (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B
  END SUBROUTINE CellBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate heirarchical tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   4   + pe1 + pe2 + pe3 + pe4 + pe5 + pe6
! - 6   + (ps1 - 1) * (ps1 - 2) / 2    + (ps2 - 1) * (ps2 - 2) / 2    + (ps3 -
! 1) * (ps3 - 2) / 2    + (ps4 - 1) * (ps4 - 2) / 2   + (order - 1) * (order -
! 2) * (order - 3) / 6_I4B).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(3,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = RefCoord_Tetrahedron("UNIT")
! v = HeirarchicalBasis_Tetrahedron(order=1_I4B,xij=points, &
!   refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear vertex partition of unity")
! CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Tetrahedron1(order, pe1, pe2, &
                pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron) &
    RESULT(ans)
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate hierarchical tetrahedron values into caller supplied
! storage.
!
! Uniform order and separate entity order value overloads are implemented.
! Results use point rows and mode columns; allocate enough storage before
! calling.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! CALL HeirarchicalBasis_Tetrahedron_(order=1_I4B,xij=x, &
!   refTetrahedron="UNIT",ans=v,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Active dimensions")
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear hierarchical partition")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Tetrahedron1_(order, pe1, pe2, &
           pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron, ans, &
                                                         nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(xij, 2),
    !! ncol = 4 + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 + (ps1 - 1) * (ps1 -
    !! 2) / 2
    !! + (ps2 - 1) * (ps2 - 2) / 2  + (ps3 - 1) * (ps3 - 2) / 2  &
    !! + (ps4 - 1) * (ps4 - 2) / 2 + (order - 1) * (order - 2) * (order - 3) /
    !! 6_I4B)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Tetrahedron1_
END INTERFACE HeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate heirarchical tetrahedral modes.
!
! Coordinates are columns. Barycentric input has four rows and must sum to one
! per point; Cartesian input has three rows. Results use point rows and mode
! columns. Entity blocks are vertices, edges, facets and cell modes. The
! public spelling Heirarchical is retained.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   (order + 1) * (order + 2) * (order + 3)
! / 6_I4B).
!
! Formula:
! $$\phi_{v_i}=\lambda_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(3,4),v(4,4)
! INTEGER(I4B) :: i
!
! points = RefCoord_Tetrahedron("UNIT")
! v = HeirarchicalBasis_Tetrahedron(order=1_I4B,xij=points, &
!   refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear vertex partition of unity")
! CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Tetrahedron2(order, xij, &
                                                   refTetrahedron) RESULT(ans)
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate hierarchical tetrahedron values into caller supplied
! storage.
!
! Uniform order and separate entity order value overloads are implemented.
! Results use point rows and mode columns; allocate enough storage before
! calling.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! CALL HeirarchicalBasis_Tetrahedron_(order=1_I4B,xij=x, &
!   refTetrahedron="UNIT",ans=v,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Active dimensions")
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
!   "Linear hierarchical partition")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Tetrahedron2_(order, xij, &
                                              refTetrahedron, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow =  SIZE(xij, 2)
    !! ncol = (order + 1) * (order + 2) * (order + 3) / 6_I4B)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE HeirarchicalBasis_Tetrahedron2_
END INTERFACE HeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the cardinal nodal basis.
!
! Keep evaluation points separate from the INOUT interpolation node array.
! Reuse coeff only for the same nodes, degree, reference domain and basis;
! initialize firstCall=.TRUE. then explicitly pass .FALSE. to reuse it.
! Gradient output uses point, basis and coordinate axes. The example uses the
! working monomial backend; other selected backends retain their own
! limitations.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(3). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeEvalAll_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),points(3,4),y(4,4)
!
! nodes = RefCoord_Tetrahedron("UNIT")
! points = nodes
! y = LagrangeEvalAll_Tetrahedron(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeEvalAll_Tetrahedron1(order, x, xij, &
           refTetrahedron, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the cardinal nodal basis.
!
! Keep evaluation points separate from the INOUT interpolation node array.
! Reuse coeff only for the same nodes, degree, reference domain and basis;
! initialize firstCall=.TRUE. then explicitly pass .FALSE. to reuse it.
! Gradient output uses point, basis and coordinate axes. The example uses the
! working monomial backend; other selected backends retain their own
! limitations.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(3). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeEvalAll_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),points(3,4),y(4,4)
! INTEGER(I4B) :: nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! points = nodes
! CALL LagrangeEvalAll_Tetrahedron_(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT",ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Tetrahedron_
  MODULE SUBROUTINE LagrangeEvalAll_Tetrahedron1_(order, x, xij, ans, &
            tsize, refTetrahedron, coeff, firstCall, basisType, alpha, beta, &
                                                  lambda)
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
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Value of n+1 Lagrange polynomials at point x
    !! size(xij, 2)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total data written in ans
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT *default
    !! BIUNIT
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
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Tetrahedron1_
END INTERFACE LagrangeEvalAll_Tetrahedron_

!----------------------------------------------------------------------------
!                                                LagrangeEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the cardinal nodal basis.
!
! Keep evaluation points separate from the INOUT interpolation node array.
! Reuse coeff only for the same nodes, degree, reference domain and basis;
! initialize firstCall=.TRUE. then explicitly pass .FALSE. to reuse it.
! Gradient output uses point, basis and coordinate axes. The example uses the
! working monomial backend; other selected backends retain their own
! limitations.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeEvalAll_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),points(3,4),y(4,4)
!
! nodes = RefCoord_Tetrahedron("UNIT")
! points = nodes
! y = LagrangeEvalAll_Tetrahedron(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeEvalAll_Tetrahedron2(order, x, xij, &
           refTetrahedron, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the cardinal nodal basis.
!
! Keep evaluation points separate from the INOUT interpolation node array.
! Reuse coeff only for the same nodes, degree, reference domain and basis;
! initialize firstCall=.TRUE. then explicitly pass .FALSE. to reuse it.
! Gradient output uses point, basis and coordinate axes. The example uses the
! working monomial backend; other selected backends retain their own
! limitations.
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
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
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
! USE TetrahedronInterpolationUtility, ONLY: LagrangeEvalAll_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),points(3,4),y(4,4)
! INTEGER(I4B) :: nr,nc
!
! nodes = RefCoord_Tetrahedron("UNIT")
! points = nodes
! CALL LagrangeEvalAll_Tetrahedron_(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,refTetrahedron="UNIT",ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Tetrahedron_
  MODULE SUBROUTINE LagrangeEvalAll_Tetrahedron2_(order, x, xij, ans, &
       nrow, ncol, refTetrahedron, coeff, firstCall, basisType, alpha, beta, &
                                                  lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns writen in ans
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT *default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    ! coeff(SIZE(xij, 2), SIZE(xij, 2))
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
  END SUBROUTINE LagrangeEvalAll_Tetrahedron2_
END INTERFACE LagrangeEvalAll_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select and construct quadrature coordinates and weights.
!
! order means polynomial integration degree; nips requests a supported
! explicit count. Columns are points, followed by a final weight row after
! coordinate rows. Quadrangle uses tensor products and two directional counts.
! Tetrahedron economical rules use Solin tables and currently ignore quadType
! in their count selector. Weights include geometric Jacobian scaling when
! physical vertices are supplied.
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
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Tetrahedron(2_I4B,GaussLegendreQP)
! ALLOCATE(buffer(4,n))
! q = QuadraturePoint_Tetrahedron(order=2_I4B,quadType=GaussLegendreQP, &
!   refTetrahedron="UNIT")
! CALL QuadraturePoint_Tetrahedron_(order=2_I4B, &
!   quadType=GaussLegendreQP,refTetrahedron="UNIT",ans=buffer,nrow=nr,ncol=nc)
! CALL OK(nr == 4 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(4,:))-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(4,:)*q(1,:))-(1.0_DFP/24.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION QuadratureNumber_Tetrahedron(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    INTEGER(I4B) :: ans
    !! Quadrature points
  END FUNCTION QuadratureNumber_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select and construct quadrature coordinates and weights.
!
! order means polynomial integration degree; nips requests a supported
! explicit count. Columns are points, followed by a final weight row after
! coordinate rows. Quadrangle uses tensor products and two directional counts.
! Tetrahedron economical rules use Solin tables and currently ignore quadType
! in their count selector. Weights include geometric Jacobian scaling when
! physical vertices are supplied.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Tetrahedron(2_I4B,GaussLegendreQP)
! ALLOCATE(buffer(4,n))
! q = QuadraturePoint_Tetrahedron(order=2_I4B,quadType=GaussLegendreQP, &
!   refTetrahedron="UNIT")
! CALL QuadraturePoint_Tetrahedron_(order=2_I4B, &
!   quadType=GaussLegendreQP,refTetrahedron="UNIT",ans=buffer,nrow=nr,ncol=nc)
! CALL OK(nr == 4 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(4,:))-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(4,:)*q(1,:))-(1.0_DFP/24.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select and construct quadrature coordinates and weights.
!
! order means polynomial integration degree; nips requests a supported
! explicit count. Columns are points, followed by a final weight row after
! coordinate rows. Quadrangle uses tensor products and two directional counts.
! Tetrahedron economical rules use Solin tables and currently ignore quadType
! in their count selector. Weights include geometric Jacobian scaling when
! physical vertices are supplied.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
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
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Tetrahedron(2_I4B,GaussLegendreQP)
! ALLOCATE(buffer(4,n))
! q = QuadraturePoint_Tetrahedron(order=2_I4B,quadType=GaussLegendreQP, &
!   refTetrahedron="UNIT")
! CALL QuadraturePoint_Tetrahedron_(order=2_I4B, &
!   quadType=GaussLegendreQP,refTetrahedron="UNIT",ans=buffer,nrow=nr,ncol=nc)
! CALL OK(nr == 4 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(4,:))-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(4,:)*q(1,:))-(1.0_DFP/24.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE QuadraturePoint_Tetrahedron1_(order, quadType, &
                                         refTetrahedron, xij, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE QuadraturePoint_Tetrahedron1_
END INTERFACE QuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select and construct quadrature coordinates and weights.
!
! order means polynomial integration degree; nips requests a supported
! explicit count. Columns are points, followed by a final weight row after
! coordinate rows. Quadrangle uses tensor products and two directional counts.
! Tetrahedron economical rules use Solin tables and currently ignore quadType
! in their count selector. Weights include geometric Jacobian scaling when
! physical vertices are supplied.
!
! Arguments:
!   nips : INTEGER(I4B), INTENT(IN)(1). Explicit quadrature point count; must
! be supported by the selected rule.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Tetrahedron(2_I4B,GaussLegendreQP)
! ALLOCATE(buffer(4,n))
! q = QuadraturePoint_Tetrahedron(order=2_I4B,quadType=GaussLegendreQP, &
!   refTetrahedron="UNIT")
! CALL QuadraturePoint_Tetrahedron_(order=2_I4B, &
!   quadType=GaussLegendreQP,refTetrahedron="UNIT",ans=buffer,nrow=nr,ncol=nc)
! CALL OK(nr == 4 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(4,:))-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(4,:)*q(1,:))-(1.0_DFP/24.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select and construct quadrature coordinates and weights.
!
! order means polynomial integration degree; nips requests a supported
! explicit count. Columns are points, followed by a final weight row after
! coordinate rows. Quadrangle uses tensor products and two directional counts.
! Tetrahedron economical rules use Solin tables and currently ignore quadType
! in their count selector. Weights include geometric Jacobian scaling when
! physical vertices are supplied.
!
! Arguments:
!   nips : INTEGER(I4B), INTENT(IN)(1). Explicit quadrature point count; must
! be supported by the selected rule.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature family selector from
! GlobalData or TypeQuadratureOpt.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
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
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron
! USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Tetrahedron(2_I4B,GaussLegendreQP)
! ALLOCATE(buffer(4,n))
! q = QuadraturePoint_Tetrahedron(order=2_I4B,quadType=GaussLegendreQP, &
!   refTetrahedron="UNIT")
! CALL QuadraturePoint_Tetrahedron_(order=2_I4B, &
!   quadType=GaussLegendreQP,refTetrahedron="UNIT",ans=buffer,nrow=nr,ncol=nc)
! CALL OK(nr == 4 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(4,:))-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(4,:)*q(1,:))-(1.0_DFP/24.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE QuadraturePoint_Tetrahedron2_(nips, quadType, &
                                         refTetrahedron, xij, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows ans columns
  END SUBROUTINE QuadraturePoint_Tetrahedron2_
END INTERFACE QuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
! TensorQuadraturePoints_Tetrahedron
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
!
!----------------------------------------------------------------------------

INTERFACE TensorQuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE TensorQuadraturePoint_Tetrahedron1_(order, quadType, &
                                         refTetrahedron, xij, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE TensorQuadraturePoint_Tetrahedron1_
END INTERFACE TensorQuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                        TensorQuadraturePoints_Tetrahedron
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
    !! Reference triangle ! BIUNIT ! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be 3
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Tetrahedron2
END INTERFACE TensorQuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE TensorQuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE TensorQuadraturePoint_Tetrahedron2_(nipsx, nipsy, &
                        nipsz, quadType, refTetrahedron, xij, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE TensorQuadraturePoint_Tetrahedron2_
END INTERFACE TensorQuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the cardinal nodal basis.
!
! Keep evaluation points separate from the INOUT interpolation node array.
! Reuse coeff only for the same nodes, degree, reference domain and basis;
! initialize firstCall=.TRUE. then explicitly pass .FALSE. to reuse it.
! Gradient output uses point, basis and coordinate axes. The example uses the
! working monomial backend; other selected backends retain their own
! limitations.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
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
!   ans : REAL(DFP)(SIZE(x, 2), SIZE(xij, 2), 3).
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
! USE TetrahedronInterpolationUtility, &
!   ONLY: LagrangeGradientEvalAll_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),points(3,4),y(4,4,3)
!
! nodes = RefCoord_Tetrahedron("UNIT")
! points = nodes
! y = LagrangeGradientEvalAll_Tetrahedron(order=1_I4B,x=points, &
!   xij=nodes,basisType=poly%Monomial,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
!   < 1.0E-11_DFP, "Gradient of partition of unity")
! END PROGRAM main
!```

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
    !! UNIT *default ! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre ! Lobatto ! Chebyshev ! Jacobi ! Ultraspherical ! Heirarchical
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the cardinal nodal basis.
!
! Keep evaluation points separate from the INOUT interpolation node array.
! Reuse coeff only for the same nodes, degree, reference domain and basis;
! initialize firstCall=.TRUE. then explicitly pass .FALSE. to reuse it.
! Gradient output uses point, basis and coordinate axes. The example uses the
! working monomial backend; other selected backends retain their own
! limitations.
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
!   refTetrahedron : CHARACTER(*), OPTIONAL, INTENT(IN). Reference tetrahedron
! domain, UNIT or BIUNIT.
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
!   Updates xij(:, :), ans(:, :, :), dim1, dim2, dim3, coeff(SIZE(xij, 2),
! SIZE(xij, 2)).
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
! USE TetrahedronInterpolationUtility, &
!   ONLY: LagrangeGradientEvalAll_Tetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(3,4),points(3,4),y(4,4,3)
! INTEGER(I4B) :: nr,nc,nd
!
! nodes = RefCoord_Tetrahedron("UNIT")
! points = nodes
! CALL LagrangeGradientEvalAll_Tetrahedron_(order=1_I4B,x=points, &
!   xij=nodes,basisType=poly%Monomial,refTetrahedron="UNIT",ans=y, &
!   dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
!   < 1.0E-11_DFP, "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Tetrahedron_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Tetrahedron1_(order, x, xij, &
         ans, dim1, dim2, dim3, refTetrahedron, coeff, firstCall, basisType, &
                                                          alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord ! x(2, :) is y coord ! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1, dim2, dim3 = SIZE(x, 2), SIZE(xij, 2), 3
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! UNIT *default ! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default ! Orthogonal
    !! Legendre ! Lobatto ! Chebyshev ! Jacobi ! Ultraspherical ! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeGradientEvalAll_Tetrahedron1_
END INTERFACE LagrangeGradientEvalAll_Tetrahedron_

!----------------------------------------------------------------------------
!                                       OrthogonalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Orthogonal gradient interface with an incorrect current backend.
!
! Do not rely on the current analytic gradients: both function and subroutine
! implementations assign ans0(:,:,2) twice and never assign ans0(:,:,3) in the
! modal derivative loop. The example deliberately differentiates the working
! value API instead; it does not validate the affected analytic gradient
! routines.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2),  (order + 1) * (order + 2) * (order + 3) /
! 6, 3).
!
! Formula:
! $$\partial_k\phi(x)\simeq[\phi(x+h e_k)-\phi(x-h e_k)]/(2h)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Tetrahedron
! USE TetrahedronInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Tetrahedron_
! USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),xp(3,2),xm(3,2),vp(2,4),vm(2,4),g(2,4,3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
! INTEGER(I4B) :: k
!
! ! Current analytic gradient assembly overwrites derivative component two
! ! and never fills component three. Differentiate working values instead.
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! DO k = 1,3
!   xp = x
!   xm = x
!   xp(k,:) = xp(k,:)+h
!   xm(k,:) = xm(k,:)-h
!   vp = OrthogonalBasis_Tetrahedron(1_I4B,xp,"UNIT")
!   vm = OrthogonalBasis_Tetrahedron(1_I4B,xm,"UNIT")
!   g(:,:,k) = (vp-vm)/(2.0_DFP*h)
! END DO
! CALL OK(MAXVAL(ABS(g(:,1,:))) < 1.0E-8_DFP, "Constant mode gradient")
! CALL OK(MAXVAL(ABS(g(:,2,3)-4.0_DFP)) < 1.0E-8_DFP, &
!   "Numerical derivative of the first nonconstant mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisGradient_Tetrahedron
  MODULE FUNCTION OrthogonalBasisGradient_Tetrahedron1(order, xij, &
                                                   refTetrahedron) RESULT(ans)
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
    REAL(DFP) :: ans(SIZE(xij, 2), &
                     (order + 1) * (order + 2) * (order + 3) / 6, 3)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION OrthogonalBasisGradient_Tetrahedron1
END INTERFACE OrthogonalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                       OrthogonalBasisGradient_Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Orthogonal gradient interface with an incorrect current backend.
!
! Do not rely on the current analytic gradients: both function and subroutine
! implementations assign ans0(:,:,2) twice and never assign ans0(:,:,3) in the
! modal derivative loop. The example deliberately differentiates the working
! value API instead; it does not validate the affected analytic gradient
! routines.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   dim2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   dim3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!
! Returns:
!   Updates ans(:, :, :), dim1, dim2, dim3.
!
! Formula:
! $$\partial_k\phi(x)\simeq[\phi(x+h e_k)-\phi(x-h e_k)]/(2h)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Tetrahedron
! USE TetrahedronInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Tetrahedron_
! USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),xp(3,2),xm(3,2),vp(2,4),vm(2,4),g(2,4,3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
! INTEGER(I4B) :: k
!
! ! Current analytic gradient assembly overwrites derivative component two
! ! and never fills component three. Differentiate working values instead.
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! DO k = 1,3
!   xp = x
!   xm = x
!   xp(k,:) = xp(k,:)+h
!   xm(k,:) = xm(k,:)-h
!   vp = OrthogonalBasis_Tetrahedron(1_I4B,xp,"UNIT")
!   vm = OrthogonalBasis_Tetrahedron(1_I4B,xm,"UNIT")
!   g(:,:,k) = (vp-vm)/(2.0_DFP*h)
! END DO
! CALL OK(MAXVAL(ABS(g(:,1,:))) < 1.0E-8_DFP, "Constant mode gradient")
! CALL OK(MAXVAL(ABS(g(:,2,3)-4.0_DFP)) < 1.0E-8_DFP, &
!   "Numerical derivative of the first nonconstant mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisGradient_Tetrahedron_
  MODULE SUBROUTINE OrthogonalBasisGradient_Tetrahedron1_(order, xij, &
                                        refTetrahedron, ans, dim1, dim2, dim3)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of  shape functions
    !! first dimension = evaluation point
    !! second dimension = shape function number
    !! third dimension = spatial dimension
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = size(xij, 2)
    !! dim2 = (order + 1) * (order + 2) * (order + 3) / 6
    !! dim3 = 3
  END SUBROUTINE OrthogonalBasisGradient_Tetrahedron1_
END INTERFACE OrthogonalBasisGradient_Tetrahedron_

!----------------------------------------------------------------------------
!                                    HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Cartesian hierarchical tetrahedron gradients.
!
! The uniform-order subroutine specific
! HeirarchicalBasisGradient_Tetrahedron2_ is declared but has no
! implementation in the current source tree. Explicitly pass all six edge
! orders and four facet orders as in this example to select the implemented
! subroutine. The uniform-order function wrapper is implemented. BIUNIT
! derivatives apply an additional factor of one half.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   4   + pe1 + pe2 + pe3 + pe4 + pe5 + pe6
! - 6   + (ps1 - 1) * (ps1 - 2) / 2    + (ps2 - 1) * (ps2 - 2) / 2    + (ps3 -
! 1) * (ps3 - 2) / 2    + (ps4 - 1) * (ps4 - 2) / 2   + (order - 1) * (order -
! 2) * (order - 3) / 6_I4B, 3).
!
! Formula:
! $$\partial_x\phi=\partial_{\lambda_2}\phi-\partial_{\lambda_1}\phi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),g(2,4,3)
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! g = HeirarchicalBasisGradient_Tetrahedron(order=1_I4B,pe1=1_I4B, &
!   pe2=1_I4B,pe3=1_I4B,pe4=1_I4B,pe5=1_I4B,pe6=1_I4B,ps1=1_I4B, &
!   ps2=1_I4B,ps3=1_I4B,ps4=1_I4B,xij=x,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(g,DIM=2))) < 1.0E-11_DFP, &
!   "Vertex gradient partition")
! CALL OK(MAXVAL(ABS(g(:,2,1)-1.0_DFP)) < 1.0E-11_DFP, &
!   "UNIT barycentric coordinate derivative")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Tetrahedron
  MODULE FUNCTION HeirarchicalBasisGradient_Tetrahedron1(order, pe1, pe2, &
                pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron) &
    RESULT(ans)
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
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Cartesian hierarchical tetrahedron gradients.
!
! The uniform-order subroutine specific
! HeirarchicalBasisGradient_Tetrahedron2_ is declared but has no
! implementation in the current source tree. Explicitly pass all six edge
! orders and four facet orders as in this example to select the implemented
! subroutine. The uniform-order function wrapper is implemented. BIUNIT
! derivatives apply an additional factor of one half.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   pe1 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 1.
!   pe2 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 2.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe5 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 5.
!   pe6 : INTEGER(I4B), INTENT(IN). Polynomial order on edge 6.
!   ps1 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 1.
!   ps2 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 2.
!   ps3 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 3.
!   ps4 : INTEGER(I4B), INTENT(IN). Polynomial order on triangular facet 4.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   dim2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   dim3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!
! Returns:
!   Updates ans(:, :, :), dim1, dim2, dim3.
!
! Formula:
! $$\partial_x\phi=\partial_{\lambda_2}\phi-\partial_{\lambda_1}\phi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),g(2,4,3)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! CALL HeirarchicalBasisGradient_Tetrahedron_(order=1_I4B,pe1=1_I4B, &
!   pe2=1_I4B,pe3=1_I4B,pe4=1_I4B,pe5=1_I4B,pe6=1_I4B,ps1=1_I4B, &
!   ps2=1_I4B,ps3=1_I4B,ps4=1_I4B,xij=x,refTetrahedron="UNIT",ans=g, &
!   dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(g,DIM=2))) < 1.0E-11_DFP, &
!   "Vertex gradient partition")
! CALL OK(MAXVAL(ABS(g(:,2,1)-1.0_DFP)) < 1.0E-11_DFP, &
!   "UNIT barycentric coordinate derivative")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Tetrahedron_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Tetrahedron1_(order, pe1, pe2, &
                pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron, &
                                                        ans, dim1, dim2, dim3)

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
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = SIZE(xij, 2)
    !! dim2 = 4  + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
    !! + (ps1 - 1) * (ps1 - 2) / 2  &
    !! + (ps2 - 1) * (ps2 - 2) / 2  &
    !! + (ps3 - 1) * (ps3 - 2) / 2  &
    !! + (ps4 - 1) * (ps4 - 2) / 2 &
    !! + (order - 1) * (order - 2) * (order - 3) / 6_I4B
    !! dim3 = 3
  END SUBROUTINE HeirarchicalBasisGradient_Tetrahedron1_
END INTERFACE HeirarchicalBasisGradient_Tetrahedron_

!----------------------------------------------------------------------------
!                                     HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Cartesian hierarchical tetrahedron gradients.
!
! The uniform-order subroutine specific
! HeirarchicalBasisGradient_Tetrahedron2_ is declared but has no
! implementation in the current source tree. Explicitly pass all six edge
! orders and four facet orders as in this example to select the implemented
! subroutine. The uniform-order function wrapper is implemented. BIUNIT
! derivatives apply an additional factor of one half.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result buffer; allocate the full
! active shape before the call.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active point dimension.
!   dim2 : INTEGER(I4B), INTENT(OUT). Active basis dimension.
!   dim3 : INTEGER(I4B), INTENT(OUT). Active coordinate dimension.
!
! Returns:
!   Updates ans(:, :, :), dim1, dim2, dim3.
!
! Formula:
! $$\partial_x\phi=\partial_{\lambda_2}\phi-\partial_{\lambda_1}\phi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),g(2,4,3)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! CALL HeirarchicalBasisGradient_Tetrahedron_(order=1_I4B,pe1=1_I4B, &
!   pe2=1_I4B,pe3=1_I4B,pe4=1_I4B,pe5=1_I4B,pe6=1_I4B,ps1=1_I4B, &
!   ps2=1_I4B,ps3=1_I4B,ps4=1_I4B,xij=x,refTetrahedron="UNIT",ans=g, &
!   dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(g,DIM=2))) < 1.0E-11_DFP, &
!   "Vertex gradient partition")
! CALL OK(MAXVAL(ABS(g(:,2,1)-1.0_DFP)) < 1.0E-11_DFP, &
!   "UNIT barycentric coordinate derivative")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Tetrahedron_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Tetrahedron2_(order, xij, &
                                        refTetrahedron, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! order on xij
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! UNIT or BIUNIT
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = SIZE(xij, 2)
    !! dim2 = (order + 1) * (order + 2) * (order + 3) / 6_I4B
    !! dim3 = 3
  END SUBROUTINE HeirarchicalBasisGradient_Tetrahedron2_
END INTERFACE HeirarchicalBasisGradient_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Cartesian hierarchical tetrahedron gradients.
!
! The uniform-order subroutine specific
! HeirarchicalBasisGradient_Tetrahedron2_ is declared but has no
! implementation in the current source tree. Explicitly pass all six edge
! orders and four facet orders as in this example to select the implemented
! subroutine. The uniform-order function wrapper is implemented. BIUNIT
! derivatives apply an additional factor of one half.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(   SIZE(xij, 2),   (order + 1) * (order + 2) * (order + 3)
! / 6_I4B, 3).
!
! Formula:
! $$\partial_x\phi=\partial_{\lambda_2}\phi-\partial_{\lambda_1}\phi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TetrahedronInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,2),g(2,4,3)
!
! x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
! x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
! g = HeirarchicalBasisGradient_Tetrahedron(order=1_I4B,pe1=1_I4B, &
!   pe2=1_I4B,pe3=1_I4B,pe4=1_I4B,pe5=1_I4B,pe6=1_I4B,ps1=1_I4B, &
!   ps2=1_I4B,ps3=1_I4B,ps4=1_I4B,xij=x,refTetrahedron="UNIT")
! CALL OK(MAXVAL(ABS(SUM(g,DIM=2))) < 1.0E-11_DFP, &
!   "Vertex gradient partition")
! CALL OK(MAXVAL(ABS(g(:,2,1)-1.0_DFP)) < 1.0E-11_DFP, &
!   "UNIT barycentric coordinate derivative")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Tetrahedron
  MODULE FUNCTION HeirarchicalBasisGradient_Tetrahedron2(order, xij, &
    refTetrahedron) RESULT(ans)
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
