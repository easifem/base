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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: LineInterpolationUtility geometry and interpolation utilities.
!
! Points are columns of coordinate arrays; basis values have point rows and
! basis columns. Gradient element wrappers add a final coordinate axis. The
! public spelling Heirarchical is retained. Named polynomial Laplacians are
! unimplemented in the Polynomial backends; monomial Laplacians work on
! BIUNIT.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$u(x)=\sum_j c_j\phi_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

MODULE LineInterpolationUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

PUBLIC :: LagrangeDegree_Line
PUBLIC :: LagrangeDOF_Point
PUBLIC :: LagrangeDOF_Line
PUBLIC :: LagrangeInDOF_Line
PUBLIC :: GetTotalDOF_Line
PUBLIC :: GetTotalInDOF_Line
PUBLIC :: EquidistanceInPoint_Line
PUBLIC :: EquidistanceInPoint_Line_
PUBLIC :: EquidistancePoint_Line
PUBLIC :: EquidistancePoint_Line_
PUBLIC :: InterpolationPoint_Line
PUBLIC :: InterpolationPoint_Line_
PUBLIC :: LagrangeCoeff_Line
PUBLIC :: LagrangeCoeff_Line_
PUBLIC :: LagrangeEvalAll_Line
PUBLIC :: LagrangeEvalAll_Line_

PUBLIC :: LagrangeGradientEvalAll_Line
PUBLIC :: LagrangeGradientEvalAll_Line_

PUBLIC :: LagrangeLaplacianEvalAll_Line
PUBLIC :: LagrangeLaplacianEvalAll_Line_

PUBLIC :: BasisEvalAll_Line
PUBLIC :: BasisEvalAll_Line_

PUBLIC :: BasisGradientEvalAll_Line
PUBLIC :: BasisGradientEvalAll_Line_

PUBLIC :: BasisLaplacianEvalAll_Line
PUBLIC :: BasisLaplacianEvalAll_Line_

PUBLIC :: QuadraturePoint_Line
PUBLIC :: QuadraturePoint_Line_

PUBLIC :: ToVEFC_Line
PUBLIC :: QuadratureNumber_Line
PUBLIC :: RefElemDomain_Line

PUBLIC :: HeirarchicalBasis_Line
PUBLIC :: HeirarchicalBasis_Line_

PUBLIC :: HeirarchicalBasisGradient_Line
PUBLIC :: HeirarchicalBasisGradient_Line_

PUBLIC :: HeirarchicalBasisLaplacian_Line
PUBLIC :: HeirarchicalBasisLaplacian_Line_

PUBLIC :: OrthogonalBasis_Line
PUBLIC :: OrthogonalBasis_Line_

PUBLIC :: OrthogonalBasisGradient_Line
PUBLIC :: OrthogonalBasisGradient_Line_

PUBLIC :: OrthogonalBasisLaplacian_Line
PUBLIC :: OrthogonalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!                                                       RefElemDomain_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION RefElemDomain_Line(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      QuadratureNumber_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
!   layout="INCREASING")
! CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(2,n))
! CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
!   layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Line(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: quadType
    INTEGER(I4B) :: ans
  END FUNCTION QuadratureNumber_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                            ToVEFC_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reorder an increasing line point list into endpoint first layout.
!
!
! Arguments:
!   pt : REAL(DFP), INTENT(INOUT)(:). Use the declaration and description for
! this overload.
!
! Returns:
!   Updates pt(:).
!
! Formula:
! $$[x_1,x_2,\ldots,x_N]\mapsto[x_1,x_N,x_2,\ldots,x_{N-1}]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: ToVEFC_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p(3)
!
! p = [-1.0_DFP,0.0_DFP,1.0_DFP]
! CALL ToVEFC_Line(p)
! CALL OK(MAXVAL(ABS(p-[-1.0_DFP,1.0_DFP,0.0_DFP])) &
!   < 1.0E-12_DFP, "Endpoints precede interior points")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE ToVEFC_Line(pt)
    REAL(DFP), INTENT(INOUT) :: pt(:)
  END SUBROUTINE ToVEFC_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Point(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Point
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetDOF_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetTotalDOF_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Line(order, baseContinuity, &
                                        baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! not used
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
! USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE LineInterpolationUtility, ONLY: RefElemDomain_Line
! USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:, :)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
! CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
! CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
! CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
!   LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
! degree = LagrangeDegree_Line(3_I4B)
! CALL OK(SIZE(degree,1) == 4 .AND. &
!   SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
! domain = RefElemDomain_Line("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Line(order, baseContinuity, &
                                          baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalInDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate interior line interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:).
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = EquidistanceInPoint_Line(order=2_I4B)
! CALL OK(ALL(SHAPE(y) == [1, 1]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y-(0.0_DFP))) < 1.0E-11_DFP, &
!   "Single interior point is the centroid")
! END PROGRAM main
!```

INTERFACE EquidistanceInPoint_Line
  MODULE PURE FUNCTION EquidistanceInPoint_Line1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of point 1 and point 2
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION EquidistanceInPoint_Line1
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate interior line interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(1, 1)
! INTEGER(I4B) :: nr, nc
!
! CALL EquidistanceInPoint_Line_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 1, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y-(0.0_DFP))) < 1.0E-11_DFP, &
!   "Single interior point is the centroid")
! END PROGRAM main
!```

INTERFACE EquidistanceInPoint_Line_
  MODULE PURE SUBROUTINE EquidistanceInPoint_Line1_(order, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of point 1 and point 2
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE EquidistanceInPoint_Line1_
END INTERFACE EquidistanceInPoint_Line_

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate interior line interpolation coordinates.
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
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = EquidistanceInPoint_Line(order=2_I4B)
! CALL OK(ALL(SHAPE(y) == [1, 1]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y-(0.0_DFP))) < 1.0E-11_DFP, &
!   "Single interior point is the centroid")
! END PROGRAM main
!```

INTERFACE EquidistanceInPoint_Line
  MODULE PURE FUNCTION EquidistanceInPoint_Line2(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Equidistnace points in $x_{iJ}$ format
    !! The number of rows is equal to the number of rows in xij
    !! (if xij present), otherwise, it is 1.
  END FUNCTION EquidistanceInPoint_Line2
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate interior line interpolation coordinates.
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
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistanceInPoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(1, 1)
! INTEGER(I4B) :: nr, nc
!
! CALL EquidistanceInPoint_Line_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 1, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y-(0.0_DFP))) < 1.0E-11_DFP, &
!   "Single interior point is the centroid")
! END PROGRAM main
!```

INTERFACE EquidistanceInPoint_Line_
  MODULE PURE SUBROUTINE EquidistanceInPoint_Line2_(order, xij, ans, &
                                                    nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 2
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Equidistnace points in $x_{iJ}$ format
    !! The number of rows is equal to the number of rows in xij
    !! (if xij present), otherwise, it is 1.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE EquidistanceInPoint_Line2_
END INTERFACE EquidistanceInPoint_Line_

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:).
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistancePoint_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = EquidistancePoint_Line(order=2_I4B)
! CALL OK(ALL(SHAPE(y) == [1, 3]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE EquidistancePoint_Line
  MODULE PURE FUNCTION EquidistancePoint_Line1(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coorindates of point 1 and point 2
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! equidistance points
  END FUNCTION EquidistancePoint_Line1
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistancePoint_Line_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
!
! Default Line endpoints are -1 and +1; default Triangle vertices are (0,0),
! (1,0), (0,1). Points are columns and vertices precede edge and interior
! points. InterpolationPoint requires a layout; VEFC requests entity ordering.
! Supply vertex coordinates xij to map to a different element.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistancePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(1, 3)
! INTEGER(I4B) :: nr, nc
!
! CALL EquidistancePoint_Line_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 3, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE EquidistancePoint_Line_
  MODULE PURE SUBROUTINE EquidistancePoint_Line1_(order, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coorindates of point 1 and point 2
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! equidistance points
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE EquidistancePoint_Line1_
END INTERFACE EquidistancePoint_Line_

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
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
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistancePoint_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = EquidistancePoint_Line(order=2_I4B)
! CALL OK(ALL(SHAPE(y) == [1, 3]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE EquidistancePoint_Line
  MODULE PURE FUNCTION EquidistancePoint_Line2(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! equidistance points in $x_{iJ}$ format
    !! If xij is not present, then number of rows in ans
    !! is 1. If `xij` is present then the number of rows in
    !! ans is  same as xij.
  END FUNCTION EquidistancePoint_Line2
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistancePoint_Line_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
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
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: EquidistancePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(1, 3)
! INTEGER(I4B) :: nr, nc
!
! CALL EquidistancePoint_Line_(order=2_I4B, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 3, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE EquidistancePoint_Line_
  MODULE PURE SUBROUTINE EquidistancePoint_Line2_(order, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 2
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! equidistance points in $x_{iJ}$ format
    !! If xij is not present, then number of rows in ans
    !! is 1. If `xij` is present then the number of rows in
    !! ans is  same as xij.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE EquidistancePoint_Line2_
END INTERFACE EquidistancePoint_Line_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
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
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = InterpolationPoint_Line(order=2_I4B, ipType=Equidistance, layout="VEFC")
! CALL OK(ALL(SHAPE(y) == [1, 3]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Line
  MODULE FUNCTION InterpolationPoint_Line1( &
    order, ipType, layout, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! size(ans,1) = 1
    !! size(ans,2) = order+1
  END FUNCTION InterpolationPoint_Line1
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
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
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(1, 3)
! INTEGER(I4B) :: nr, nc
!
! CALL InterpolationPoint_Line_(order=2_I4B, ipType=Equidistance, &
!   layout="VEFC", ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 3, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Line_
  MODULE SUBROUTINE InterpolationPoint_Line1_( &
    order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation points in xij format
    !! size(ans,1) = 1, size(ans,2) = order+1
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to ans
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" or "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE InterpolationPoint_Line1_
END INTERFACE InterpolationPoint_Line_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
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
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:).
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! y = InterpolationPoint_Line(order=2_I4B, ipType=Equidistance, layout="VEFC")
! CALL OK(ALL(SHAPE(y) == [1, 3]), "Coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Line
  MODULE FUNCTION InterpolationPoint_Line2( &
    order, ipType, xij, layout, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC", "INCREASING", "DECREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! one dimensional interpolation point
  END FUNCTION InterpolationPoint_Line2
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Generate line interpolation coordinates.
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
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$x_j=a+j(b-a)/p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(1, 3)
! INTEGER(I4B) :: nr, nc
!
! CALL InterpolationPoint_Line_(order=2_I4B, ipType=Equidistance, &
!   layout="VEFC", ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 3, "Active coordinate dimensions")
! CALL OK(MAXVAL(ABS(y)) <= 1.0_DFP, "Nodes lie in reference domain")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Line_
  MODULE SUBROUTINE InterpolationPoint_Line2_( &
    order, ipType, ans, tsize, xij, layout, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, see TypeInterpolationOpt
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! one dimensional interpolation point
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC", "INCREASING", "DECREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE InterpolationPoint_Line2_
END INTERFACE InterpolationPoint_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   ans : REAL(DFP)(order + 1).
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! c = LagrangeCoeff_Line(order=1_I4B, xij=nodes, basisType=poly%Monomial)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line1
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! CALL LagrangeCoeff_Line_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line1_(order, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(order + 1)
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Line1_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   ans : REAL(DFP)(order + 1).
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! c = LagrangeCoeff_Line(order=1_I4B, xij=nodes, basisType=poly%Monomial)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line2(order, i, v, isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line2
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! CALL LagrangeCoeff_Line_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line2_(order, i, v, isVandermonde, ans, &
                                         tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(order + 1)
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Line2_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   ans : REAL(DFP)(order + 1).
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! c = LagrangeCoeff_Line(order=1_I4B, xij=nodes, basisType=poly%Monomial)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line3
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! CALL LagrangeCoeff_Line_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line3_(order, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(order + 1)
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff_Line3_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!
! Returns:
!   ans : REAL(DFP)(order + 1, order + 1).
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! c = LagrangeCoeff_Line(order=1_I4B, xij=nodes, basisType=poly%Monomial)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line4(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    REAL(DFP) :: ans(order + 1, order + 1)
    !! coefficients
    !! jth column of ans corresponds to the coeff of lagrange polynomial
    !! at the jth point
  END FUNCTION LagrangeCoeff_Line4
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! CALL LagrangeCoeff_Line_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line4_(order, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(order + 1, order + 1)
    !! coefficients
    !! jth column of ans corresponds to the coeff of lagrange polynomial
    !! at the jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Line4_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                       LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! c = LagrangeCoeff_Line(order=1_I4B, xij=nodes, basisType=poly%Monomial)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line5(order, xij, basisType, alpha, &
                                      beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto, UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    !! jth column of ans corresponds to the coeff of lagrange polynomial
    !! at the jth point
  END FUNCTION LagrangeCoeff_Line5
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                       LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
! USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), c(2,2), v(2,2)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i, j, nr, nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! CALL LagrangeCoeff_Line_(order=1_I4B, xij=nodes, &
!   basisType=poly%Monomial, ans=c, nrow=nr, ncol=nc)
! degree = LagrangeDegree_Line(1_I4B)
! DO j = 1, 2
!   DO i = 1, 2
!     v(i,j) = PRODUCT(nodes(:,i)**degree(j,:))
!   END DO
! END DO
! v = MATMUL(v,c)
! DO j = 1, 2
!   v(j,j) = v(j,j)-1.0_DFP
! END DO
! CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, "Vandermonde times C is identity")
! END PROGRAM main
!```

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line5_(order, xij, basisType, alpha, &
                                         beta, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto, UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    !! jth column of ans corresponds to the coeff of lagrange polynomial
    !! at the jth point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff_Line5_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the nodal line basis.
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
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
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
! USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2)
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! y = LagrangeEvalAll_Line(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Line
  MODULE FUNCTION LagrangeEvalAll_Line1(order, x, xij, coeff, firstCall, &
                                   basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial  Jacobi  Legendre  Chebyshev  Lobatto  UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Line1
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                               LagrangeEvalAll_Line_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the nodal line basis.
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
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(:, :). Optional coefficient
! cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!
! Returns:
!   Updates xij(:, :), coeff(:, :), ans(:), tsize.
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
! USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2)
! INTEGER(I4B) :: nr,nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeEvalAll_Line_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Line_
  MODULE SUBROUTINE LagrangeEvalAll_Line1_( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda, ans, &
    tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial  Jacobi  Legendre  Chebyshev  Lobatto  UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeEvalAll_Line1_
END INTERFACE LagrangeEvalAll_Line_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the nodal line basis.
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
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
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
! USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2)
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! y = LagrangeEvalAll_Line(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Line
  MODULE FUNCTION LagrangeEvalAll_Line2( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    !! size(xij, 1) = nsd
    !! size(xij, 2) = number of points
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
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
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION LagrangeEvalAll_Line2
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                       LagrangeEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the nodal line basis.
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
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(:, :). Optional coefficient
! cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
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
! USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2)
! INTEGER(I4B) :: nr,nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeEvalAll_Line_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Line_
  MODULE SUBROUTINE LagrangeEvalAll_Line2_( &
    order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    !! size(xij, 1) = nsd
    !! size(xij, 2) = number of points
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nubmer of rows and cols writte in ans
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto, UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Line2_
END INTERFACE LagrangeEvalAll_Line_

!----------------------------------------------------------------------------
!                                       LagrangeEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the nodal line basis.
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
!   coeff : REAL(DFP), INTENT(INOUT)(:, :). Optional coefficient cache; must
! match the same nodes, order and basis.
!   xx : REAL(DFP), INTENT(INOUT)(:, :). Reusable point by basis workspace;
! shape must match the selected overload.
!   firstCall : LOGICAL(LGT), INTENT(IN). Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
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
! USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2)
! INTEGER(I4B) :: nr,nc
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeEvalAll_Line_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Line_
  MODULE SUBROUTINE LagrangeEvalAll_Line3_( &
    order, x, xij, ans, nrow, ncol, coeff, xx, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    !! size(xij, 1) = nsd
    !! size(xij, 2) = number of points, ncol
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    !! rows of xij = nsd
    !! cols of xij = ncol
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nubmer of rows and cols writte in ans
    !! nrow = size(x, 2), number of points of evaluation
    !! ncol = size(xij, 2), number of interpolation points
    REAL(DFP), INTENT(INOUT) :: coeff(:, :), xx(:, :)
    !! coefficient of Lagrange polynomials
    !! The size should be at least ncol by ncol
    !! The size of xx should be at least nrow by ncol
    !! It contains the evaluation of basis functions on x
    !! Size of xx is nrow by ncol
    LOGICAL(LGT), INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial, Jacobi, Legendre, Chebyshev, Lobatto, UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Line3_
END INTERFACE LagrangeEvalAll_Line_

!----------------------------------------------------------------------------
!                                               LagrangeGradientEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate gradients of the nodal line basis.
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
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x, 2), SIZE(xij, 2), 1).
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
! USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2,1)
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! y = LagrangeGradientEvalAll_Line(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Line
  MODULE FUNCTION LagrangeGradientEvalAll_Line1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
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
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 1)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
  END FUNCTION LagrangeGradientEvalAll_Line1
END INTERFACE LagrangeGradientEvalAll_Line

!----------------------------------------------------------------------------
!                               LagrangeGradientEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate gradients of the nodal line basis.
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
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
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
! USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2,1)
! INTEGER(I4B) :: nr,nc,nd
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeGradientEvalAll_Line_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, ans=y, dim1=nr, dim2=nc, dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Line_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Line1_( &
    order, x, xij, ans, dim1, dim2, dim3, coeff, firstCall, basisType, &
    alpha, beta, lambda)
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
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! ans(SIZE(x, 2), SIZE(xij, 2), 1)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeGradientEvalAll_Line1_
END INTERFACE LagrangeGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                               LagrangeGradientEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate gradients of the nodal line basis.
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
!   coeff : REAL(DFP), INTENT(INOUT)(:, :). Optional coefficient cache; must
! match the same nodes, order and basis.
!   xx : REAL(DFP), INTENT(INOUT)(:, :). Reusable point by basis workspace;
! shape must match the selected overload.
!   firstCall : LOGICAL(LGT), INTENT(IN). Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates xij(:, :), ans(:, :, :), dim1, dim2, dim3, coeff(:, :), xx(:, :).
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
! USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,2), points(1,2)
! REAL(DFP) :: y(2,2,1)
! INTEGER(I4B) :: nr,nc,nd
!
! nodes(1,:) = [-1.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeGradientEvalAll_Line_(order=1_I4B, x=points, xij=nodes, &
!   basisType=poly%Monomial, ans=y, dim1=nr, dim2=nc, dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) < 1.0E-10_DFP, &
!   "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Line_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Line2_( &
    order, x, xij, ans, dim1, dim2, dim3, coeff, xx, firstCall, basisType, &
    alpha, beta, lambda)
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
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! ans(SIZE(x, 2), SIZE(xij, 2), 1)
    REAL(DFP), INTENT(INOUT) :: coeff(:, :)
    !! coefficient of Lagrange polynomials
    !! shape nrow = size(xij, 2), ncol = size(xij, 2)
    REAL(DFP), INTENT(INOUT) :: xx(:, :)
    !! working array needed for internal used
    !! number of rows in xx should be : size(x, 2)
    !! number of cols in xx should be : order + 1
    LOGICAL(LGT), INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeGradientEvalAll_Line2_
END INTERFACE LagrangeGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                                              LagrangeLaplacianEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate second derivatives of nodal line basis functions.
!
! Uses the implemented monomial second derivative backend. Other selected
! polynomial families can reach empty Laplacian implementations. Cache and
! point layout conventions match LagrangeEvalAll_Line.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:, :). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
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
! $$\ell_j''(x)=\sum_k C_{kj}m_k''(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: LagrangeLaplacianEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,3), points(1,3), y(3,3)
!
! nodes(1,:) = [-1.0_DFP,0.0_DFP,1.0_DFP]
! points = nodes
! y = LagrangeLaplacianEvalAll_Line(order=2_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,2)+2.0_DFP)) < 1.0E-10_DFP, &
!   "Central cardinal quadratic has second derivative -2")
! END PROGRAM main
!```

INTERFACE LagrangeLaplacianEvalAll_Line
  MODULE FUNCTION LagrangeLaplacianEvalAll_Line1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
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
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of Laplacian of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation (ips)
    !! the second index denotes Lagrange polynomial number (I)
  END FUNCTION LagrangeLaplacianEvalAll_Line1
END INTERFACE LagrangeLaplacianEvalAll_Line

!----------------------------------------------------------------------------
!                             LagrangeLaplacianEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate second derivatives of nodal line basis functions.
!
! Uses the implemented monomial second derivative backend. Other selected
! polynomial families can reach empty Laplacian implementations. Cache and
! point layout conventions match LagrangeEvalAll_Line.
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
!   coeff : REAL(DFP), OPTIONAL, INTENT(INOUT)(SIZE(xij, 2), SIZE(xij, 2)).
! Optional coefficient cache; must match the same nodes, order and basis.
!   firstCall : LOGICAL(LGT), OPTIONAL, INTENT(IN). Use .TRUE. to construct
! the coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates xij(:, :), ans(:, :), nrow, ncol, coeff(SIZE(xij, 2), SIZE(xij,
! 2)).
!
! Formula:
! $$\ell_j''(x)=\sum_k C_{kj}m_k''(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: LagrangeLaplacianEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,3), points(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! nodes(1,:) = [-1.0_DFP,0.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeLaplacianEvalAll_Line_(order=2_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,2)+2.0_DFP)) < 1.0E-10_DFP, &
!   "Central cardinal quadratic has second derivative -2")
! END PROGRAM main
!```

INTERFACE LagrangeLaplacianEvalAll_Line_
  MODULE SUBROUTINE LagrangeLaplacianEvalAll_Line1_( &
    order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of Laplacian of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which Laplacian is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! ans(SIZE(x, 2), SIZE(xij, 2), 1)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeLaplacianEvalAll_Line1_
END INTERFACE LagrangeLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!                             LagrangeLaplacianEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate second derivatives of nodal line basis functions.
!
! Uses the implemented monomial second derivative backend. Other selected
! polynomial families can reach empty Laplacian implementations. Cache and
! point layout conventions match LagrangeEvalAll_Line.
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
!   coeff : REAL(DFP), INTENT(INOUT)(:, :). Optional coefficient cache; must
! match the same nodes, order and basis.
!   xx : REAL(DFP), INTENT(INOUT)(:, :). Reusable point by basis workspace;
! shape must match the selected overload.
!   firstCall : LOGICAL(LGT), INTENT(IN). Use .TRUE. to construct the
! coefficient cache, then explicitly pass .FALSE. to reuse it.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates xij(:, :), ans(:, :), nrow, ncol, coeff(:, :), xx(:, :).
!
! Formula:
! $$\ell_j''(x)=\sum_k C_{kj}m_k''(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: LagrangeLaplacianEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(1,3), points(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! nodes(1,:) = [-1.0_DFP,0.0_DFP,1.0_DFP]
! points = nodes
! CALL LagrangeLaplacianEvalAll_Line_(order=2_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,2)+2.0_DFP)) < 1.0E-10_DFP, &
!   "Central cardinal quadratic has second derivative -2")
! END PROGRAM main
!```

INTERFACE LagrangeLaplacianEvalAll_Line_
  MODULE SUBROUTINE LagrangeLaplacianEvalAll_Line2_( &
    order, x, xij, ans, nrow, ncol, coeff, xx, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of Laplacian of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which Laplacian is
    !! computed
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! ans(SIZE(x, 2), SIZE(xij, 2), 1)
    REAL(DFP), INTENT(INOUT) :: coeff(:, :)
    !! coefficient of Lagrange polynomials
    !! shape nrow = size(xij, 2), ncol = size(xij, 2)
    REAL(DFP), INTENT(INOUT) :: xx(:, :)
    !! working array needed for internal used
    !! number of rows in xx should be : size(x, 2)
    !! number of cols in xx should be : order + 1
    LOGICAL(LGT), INTENT(IN) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeLaplacianEvalAll_Line2_
END INTERFACE LagrangeLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!                                              OrthogonalBasisLaplacian_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line OrthogonalBasis laplacian.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. Only Monomial has a working
! Laplacian here; named polynomial family branches can reach unimplemented
! routines. UNIT second derivative scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), order + 1).
!
! Formula:
! $$L_{ij}=\phi_j''(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: OrthogonalBasisLaplacian_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = OrthogonalBasisLaplacian_Line(order=2_I4B, xij=x, &
!   refLine="BIUNIT", basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisLaplacian_Line
  MODULE FUNCTION OrthogonalBasisLaplacian_Line1( &
    order, xij, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    ! basisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION OrthogonalBasisLaplacian_Line1
END INTERFACE OrthogonalBasisLaplacian_Line

!----------------------------------------------------------------------------
!                                               OrthgonalBasisLaplacian_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line OrthogonalBasis laplacian.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. Only Monomial has a working
! Laplacian here; named polynomial family branches can reach unimplemented
! routines. UNIT second derivative scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
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
! $$L_{ij}=\phi_j''(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: OrthogonalBasisLaplacian_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL OrthogonalBasisLaplacian_Line_(order=2_I4B, xij=x, &
!   refLine="BIUNIT", basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisLaplacian_Line_
  MODULE SUBROUTINE OrthogonalBasisLaplacian_Line1_( &
    order, xij, refLine, basisType, ans, nrow, ncol, alpha, beta, &
    lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference line element: UNIT, BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), order + 1, 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = size(xij,2) ! ncol = order+1
  END SUBROUTINE OrthogonalBasisLaplacian_Line1_
END INTERFACE OrthogonalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!                                                          BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(order + 1).
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = BasisEvalAll_Line(order=2_I4B, x=x(1,:), refLine="BIUNIT", &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (x(1,:)**2))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisEvalAll_Line
  MODULE FUNCTION BasisEvalAll_Line1(order, x, refLine, basisType, alpha, &
                                     beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! BasisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(order + 1)
    !! Value of n+1  polynomials at point x
  END FUNCTION BasisEvalAll_Line1
END INTERFACE BasisEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL BasisEvalAll_Line_(order=2_I4B, x=x(1,:), refLine="BIUNIT", &
!   basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (x(1,:)**2))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisEvalAll_Line_
  MODULE SUBROUTINE BasisEvalAll_Line1_(order, x, ans, tsize, refLine, &
                                        basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(order + 1)
    !! Value of n+1  polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! BasisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE BasisEvalAll_Line1_
END INTERFACE BasisEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), order + 1).
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = BasisEvalAll_Line(order=2_I4B, x=x(1,:), refLine="BIUNIT", &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (x(1,:)**2))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisEvalAll_Line
  MODULE FUNCTION BasisEvalAll_Line2(order, x, refLine, basisType, &
                                     alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT, BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! BasisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION BasisEvalAll_Line2
END INTERFACE BasisEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL BasisEvalAll_Line_(order=2_I4B, x=x(1,:), refLine="BIUNIT", &
!   basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (x(1,:)**2))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisEvalAll_Line_
  MODULE SUBROUTINE BasisEvalAll_Line2_(order, x, ans, nrow, ncol, &
                                      refLine, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to ans
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT, BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis type
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE BasisEvalAll_Line2_
END INTERFACE BasisEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line OrthogonalBasis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), order + 1).
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = OrthogonalBasis_Line(order=2_I4B, xij=x, refLine="BIUNIT", &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (x(1,:)**2))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Line
  MODULE FUNCTION OrthogonalBasis_Line1(order, xij, refLine, basisType, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Jacobi ! Ultraspherical ! Legendre ! Chebyshev ! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION OrthogonalBasis_Line1
END INTERFACE OrthogonalBasis_Line

!----------------------------------------------------------------------------
!                                                     OrthogonalBasis_Line_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line OrthogonalBasis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
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
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL OrthogonalBasis_Line_(order=2_I4B, xij=x, refLine="BIUNIT", &
!   basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (x(1,:)**2))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Line_
  MODULE SUBROUTINE OrthogonalBasis_Line1_( &
    order, xij, refLine, basisType, ans, nrow, ncol, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Jacobi ! Ultraspherical ! Legendre ! Chebyshev ! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of n+1  polynomials at point x
    !  ans(SIZE(xij, 2), order + 1)
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = size(xij, 2)
    !! ncol = order+1
  END SUBROUTINE OrthogonalBasis_Line1_
END INTERFACE OrthogonalBasis_Line_

!----------------------------------------------------------------------------
!                                               OrthogonalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line OrthogonalBasis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), order + 1, 1).
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: OrthogonalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3,1)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = OrthogonalBasisGradient_Line(order=2_I4B, xij=x, refLine="BIUNIT", &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3,1)- &
!   (2.0_DFP*x(1,:)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisGradient_Line
  MODULE FUNCTION OrthogonalBasisGradient_Line1( &
    order, xij, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    ! basisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1, 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION OrthogonalBasisGradient_Line1
END INTERFACE OrthogonalBasisGradient_Line

!----------------------------------------------------------------------------
!                                               OrthgonalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line OrthogonalBasis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector, for
! example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
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
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: OrthogonalBasisGradient_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3,1)
! INTEGER(I4B) :: nr,nc,nd
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL OrthogonalBasisGradient_Line_(order=2_I4B, xij=x, &
!   refLine="BIUNIT", basisType=poly%Monomial, ans=y, dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(y(:,3,1)- &
!   (2.0_DFP*x(1,:)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisGradient_Line_
  MODULE SUBROUTINE OrthogonalBasisGradient_Line1_( &
    order, xij, refLine, basisType, ans, dim1, dim2, dim3, alpha, beta, &
    lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference line element: UNIT, BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basisType
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! ans(SIZE(xij, 2), order + 1, 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = size(xij,2) ! dim2 = order+1 ! dim3 = 1
  END SUBROUTINE OrthogonalBasisGradient_Line1_
END INTERFACE OrthogonalBasisGradient_Line_

!----------------------------------------------------------------------------
!                                                     HeirarchicalBasis_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), order + 1).
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = HeirarchicalBasis_Line(order=2_I4B, xij=x, refLine="BIUNIT")
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (1.5_DFP*(x(1,:)**2-1.0_DFP)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, &
!   "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Line
  MODULE FUNCTION HeirarchicalBasis_Line1(order, xij, refLine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! Hierarchical basis
  END FUNCTION HeirarchicalBasis_Line1
END INTERFACE HeirarchicalBasis_Line

!----------------------------------------------------------------------------
!                                                     HeirarchicalBasis_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
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
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL HeirarchicalBasis_Line_(order=2_I4B, xij=x, refLine="BIUNIT", &
!   ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (1.5_DFP*(x(1,:)**2-1.0_DFP)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, &
!   "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Line_
  MODULE SUBROUTINE HeirarchicalBasis_Line1_( &
    order, xij, refLine, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Hierarchical basis
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! SIZE(xij, 2), order + 1
  END SUBROUTINE HeirarchicalBasis_Line1_
END INTERFACE HeirarchicalBasis_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis values.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   orient : INTEGER(I4B), INTENT(IN). Interior mode orientation sign, +1 or
! -1.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$Y_{ij}=\phi_j(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL HeirarchicalBasis_Line_(order=2_I4B, xij=x, refLine="BIUNIT", &
!   ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (1.5_DFP*(x(1,:)**2-1.0_DFP)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, &
!   "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Line_
  MODULE SUBROUTINE HeirarchicalBasis_Line2_( &
    order, xij, refLine, orient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of line: 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Hierarchical basis
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! SIZE(xij, 2), order + 1
  END SUBROUTINE HeirarchicalBasis_Line2_
END INTERFACE HeirarchicalBasis_Line_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), order + 1, 1).
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3,1)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = HeirarchicalBasisGradient_Line(order=2_I4B, xij=x, refLine="BIUNIT")
! CALL OK(MAXVAL(ABS(y(:,3,1)- &
!   (3.0_DFP*x(1,:)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Line
  MODULE FUNCTION HeirarchicalGradientBasis_Line1(order, xij, refLine) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1, 1)
    !! Gradient of Hierarchical basis
  END FUNCTION HeirarchicalGradientBasis_Line1
END INTERFACE HeirarchicalBasisGradient_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
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
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3,1)
! INTEGER(I4B) :: nr,nc,nd
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL HeirarchicalBasisGradient_Line_(order=2_I4B, xij=x, &
!   refLine="BIUNIT", ans=y, dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(y(:,3,1)- &
!   (3.0_DFP*x(1,:)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Line_
  MODULE SUBROUTINE HeirarchicalGradientBasis_Line1_( &
    order, xij, refLine, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Gradient of Hierarchical basis
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! SIZE(xij, 2), order + 1, 1
  END SUBROUTINE HeirarchicalGradientBasis_Line1_
END INTERFACE HeirarchicalBasisGradient_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   orient : INTEGER(I4B), INTENT(IN). Interior mode orientation sign, +1 or
! -1.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :, :).
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3,1)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = HeirarchicalBasisGradient_Line(order=2_I4B, xij=x, refLine="BIUNIT")
! CALL OK(MAXVAL(ABS(y(:,3,1)- &
!   (3.0_DFP*x(1,:)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Line
  MODULE FUNCTION HeirarchicalGradientBasis_Line2( &
    order, xij, refLine, orient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of line: 1 or -1
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! Gradient of Hierarchical basis
    !! SIZE(xij, 2), order + 1, 1
  END FUNCTION HeirarchicalGradientBasis_Line2
END INTERFACE HeirarchicalBasisGradient_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line HeirarchicalBasis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. The API spelling Heirarchical is
! intentional. Endpoint modes come first, then scaled Lobatto interior modes.
! Explicit orient overloads change interior mode signs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   orient : INTEGER(I4B), INTENT(IN). Interior mode orientation sign, +1 or
! -1.
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
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3,1)
! INTEGER(I4B) :: nr,nc,nd
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL HeirarchicalBasisGradient_Line_(order=2_I4B, xij=x, &
!   refLine="BIUNIT", ans=y, dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(y(:,3,1)- &
!   (3.0_DFP*x(1,:)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisGradient_Line_
  MODULE SUBROUTINE HeirarchicalGradientBasis_Line2_( &
    order, xij, refLine, orient, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of line: 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Gradient of Hierarchical basis
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! SIZE(xij, 2), order + 1, 1
  END SUBROUTINE HeirarchicalGradientBasis_Line2_
END INTERFACE HeirarchicalBasisGradient_Line_

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(order + 1).
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisGradientEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = BasisGradientEvalAll_Line(order=2_I4B, x=x(1,:), refLine="BIUNIT", &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP*x(1,:)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisGradientEvalAll_Line
  MODULE FUNCTION BasisGradientEvalAll_Line1( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev ! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(order + 1)
    !! Value of n+1  polynomials at point x
  END FUNCTION BasisGradientEvalAll_Line1
END INTERFACE BasisGradientEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisGradientEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL BasisGradientEvalAll_Line_(order=2_I4B, x=x(1,:), &
!   refLine="BIUNIT", basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP*x(1,:)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisGradientEvalAll_Line_
  MODULE SUBROUTINE BasisGradientEvalAll_Line1_( &
    order, x, refLine, basisType, alpha, beta, lambda, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(order + 1)
    !! Value of n+1  polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! order + 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev
    !! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE BasisGradientEvalAll_Line1_
END INTERFACE BasisGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), order + 1).
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisGradientEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = BasisGradientEvalAll_Line(order=2_I4B, x=x(1,:), refLine="BIUNIT", &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP*x(1,:)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisGradientEvalAll_Line
  MODULE FUNCTION BasisGradientEvalAll_Line2( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev
    !! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION BasisGradientEvalAll_Line2
END INTERFACE BasisGradientEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis gradient.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$G_{ij}=\phi_j'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisGradientEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL BasisGradientEvalAll_Line_(order=2_I4B, x=x(1,:), &
!   refLine="BIUNIT", basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP*x(1,:)))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisGradientEvalAll_Line_
  MODULE SUBROUTINE BasisGradientEvalAll_Line2_( &
    order, x, ans, nrow, ncol, refLine, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to ans
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev
    !! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE BasisGradientEvalAll_Line2_
END INTERFACE BasisGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                                                      QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
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
! USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
!   layout="INCREASING")
! CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(2,n))
! CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
!   layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line1(order, quadType, layout, xij, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance, ! GaussLegendre, ! GaussLegendreLobatto,
    !! GaussChebyshev, ! GaussChebyshevLobatto, ! GaussJacobi,
    !! GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" ! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points
    !! If xij is present then the number of rows in ans
    !! is same as size(xij,1) + 1.
    !! If xij is not present then the number of rows in
    !! ans is 2
    !! The last row of ans contains the weights
    !! The first few rows contains the quadrature points
  END FUNCTION QuadraturePoint_Line1
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
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
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
!   layout="INCREASING")
! CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(2,n))
! CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
!   layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line2(order, quadType, xij, layout, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance ! GaussLegendre ! GaussLegendreLobatto ! GaussChebyshev,
    !! GaussChebyshevLobatto ! GaussJacobi ! GaussJacobiLobatto
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! one dimensional interpolation point
  END FUNCTION QuadraturePoint_Line2
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   xij : REAL(DFP), INTENT(IN)(2). Point columns; vertices or interpolation
! nodes according to the interface.
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
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
!   layout="INCREASING")
! CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(2,n))
! CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
!   layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line4(nips, quadType, xij, layout, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance ! GaussLegendre ! GaussLegendreLobatto ! GaussChebyshev,
    !! GaussChebyshevLobatto ! GaussJacobi ! GaussJacobiLobatto
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! one dimensional interpolation point
  END FUNCTION QuadraturePoint_Line4
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                      QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
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
! USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
!   layout="INCREASING")
! CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(2,n))
! CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
!   layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line3(nips, quadType, layout, xij, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance,
    !! GaussLegendre,
    !! GaussLegendreLobatto,
    !! GaussChebyshev,
    !! GaussChebyshevLobatto,
    !! GaussJacobi,
    !! GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points
    !! If xij is present then the number of rows in ans
    !! is same as size(xij,1) + 1.
    !! If xij is not present then the number of rows in
    !! ans is 2
    !! The last row of ans contains the weights
    !! The first few rows contains the quadrature points
  END FUNCTION QuadraturePoint_Line3
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
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
! $$\int_K f(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
! USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:), buffer(:,:)
! INTEGER(I4B) :: n,nr,nc
!
! n = QuadratureNumber_Line(2_I4B,GaussLegendreQP)
! q = QuadraturePoint_Line(order=2_I4B, quadType=GaussLegendreQP, &
!   layout="INCREASING")
! CALL OK(ABS(SUM(q(2,:))-2.0_DFP) < 1.0E-10_DFP, &
!   "Weights integrate reference measure")
! ALLOCATE(buffer(2,n))
! CALL QuadraturePoint_Line_(nips=[n], quadType=GaussLegendreQP, &
!   layout="INCREASING", ans=buffer, nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == n, "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-10_DFP, &
!   "Function and subroutine quadrature agree")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Line_
  MODULE SUBROUTINE QuadraturePoint_Line1_(nips, quadType, layout, xij, &
                                           alpha, beta, lambda, ans, nrow, &
                                           ncol)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance, ! GaussLegendre, ! GaussLegendreLobatto,
    !! GaussChebyshev, GaussChebyshevLobatto, GaussJacobi,
    !! GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" ! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points
    !! If xij is present then the number of rows in ans
    !! is same as size(xij,1) + 1.
    !! If xij is not present then the number of rows in
    !! ans is 2
    !! The last row of ans contains the weights
    !! The first few rows contains the quadrature points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE QuadraturePoint_Line1_
END INTERFACE QuadraturePoint_Line_

!----------------------------------------------------------------------------
!                                            HeirarchicalBasisLaplacian_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Declared hierarchical Laplacian interface (unimplemented backend).
!
! Do not call this API currently: it delegates to empty Lobatto Laplacian
! bodies and can leave values and active dimensions undefined. The example
! deliberately uses a gradient based workaround. It is not validation of this
! Laplacian interface.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!
! Returns:
!   No reliable result from the affected backend; see the workaround example.
!
! Formula:
! $$\phi''(x)\simeq[\phi'(x+h)-\phi'(x-h)]/(2h)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), xp(1,3), xm(1,3)
! REAL(DFP) :: gp(3,3,1), gm(3,3,1), second(3,3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The hierarchical Laplacian forwards to empty Lobatto implementations.
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! xp = x+h
! xm = x-h
! gp = HeirarchicalBasisGradient_Line(2_I4B,xp,"BIUNIT")
! gm = HeirarchicalBasisGradient_Line(2_I4B,xm,"BIUNIT")
! second = (gp(:,:,1)-gm(:,:,1))/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(second(:,3)-3.0_DFP/SQRT(6.0_DFP))) &
!   < 1.0E-8_DFP, "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisLaplacian_Line
  MODULE FUNCTION HeirarchicalLaplacianBasis_Line1(order, xij, refLine) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! Laplacian of Hierarchical basis
  END FUNCTION HeirarchicalLaplacianBasis_Line1
END INTERFACE HeirarchicalBasisLaplacian_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Declared hierarchical Laplacian interface (unimplemented backend).
!
! Do not call this API currently: it delegates to empty Lobatto Laplacian
! bodies and can leave values and active dimensions undefined. The example
! deliberately uses a gradient based workaround. It is not validation of this
! Laplacian interface.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   No reliable result from the affected backend; see the workaround example.
!
! Formula:
! $$\phi''(x)\simeq[\phi'(x+h)-\phi'(x-h)]/(2h)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line_
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), xp(1,3), xm(1,3)
! REAL(DFP) :: gp(3,3,1), gm(3,3,1), second(3,3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The hierarchical Laplacian forwards to empty Lobatto implementations.
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! xp = x+h
! xm = x-h
! gp = HeirarchicalBasisGradient_Line(2_I4B,xp,"BIUNIT")
! gm = HeirarchicalBasisGradient_Line(2_I4B,xm,"BIUNIT")
! second = (gp(:,:,1)-gm(:,:,1))/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(second(:,3)-3.0_DFP/SQRT(6.0_DFP))) &
!   < 1.0E-8_DFP, "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisLaplacian_Line_
  MODULE SUBROUTINE HeirarchicalLaplacianBasis_Line1_( &
    order, xij, refLine, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Laplacian of Hierarchical basis
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! SIZE(xij, 2), order + 1, 1
  END SUBROUTINE HeirarchicalLaplacianBasis_Line1_
END INTERFACE HeirarchicalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Declared hierarchical Laplacian interface (unimplemented backend).
!
! Do not call this API currently: it delegates to empty Lobatto Laplacian
! bodies and can leave values and active dimensions undefined. The example
! deliberately uses a gradient based workaround. It is not validation of this
! Laplacian interface.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   orient : INTEGER(I4B), INTENT(IN). Interior mode orientation sign, +1 or
! -1.
!
! Returns:
!   No reliable result from the affected backend; see the workaround example.
!
! Formula:
! $$\phi''(x)\simeq[\phi'(x+h)-\phi'(x-h)]/(2h)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), xp(1,3), xm(1,3)
! REAL(DFP) :: gp(3,3,1), gm(3,3,1), second(3,3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The hierarchical Laplacian forwards to empty Lobatto implementations.
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! xp = x+h
! xm = x-h
! gp = HeirarchicalBasisGradient_Line(2_I4B,xp,"BIUNIT")
! gm = HeirarchicalBasisGradient_Line(2_I4B,xm,"BIUNIT")
! second = (gp(:,:,1)-gm(:,:,1))/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(second(:,3)-3.0_DFP/SQRT(6.0_DFP))) &
!   < 1.0E-8_DFP, "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisLaplacian_Line
  MODULE FUNCTION HeirarchicalLaplacianBasis_Line2( &
    order, xij, refLine, orient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of line: 1 or -1
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Laplacian of Hierarchical basis
    !! SIZE(xij, 2), order + 1, 1
  END FUNCTION HeirarchicalLaplacianBasis_Line2
END INTERFACE HeirarchicalBasisLaplacian_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Declared hierarchical Laplacian interface (unimplemented backend).
!
! Do not call this API currently: it delegates to empty Lobatto Laplacian
! bodies and can leave values and active dimensions undefined. The example
! deliberately uses a gradient based workaround. It is not validation of this
! Laplacian interface.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   orient : INTEGER(I4B), INTENT(IN). Interior mode orientation sign, +1 or
! -1.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   No reliable result from the affected backend; see the workaround example.
!
! Formula:
! $$\phi''(x)\simeq[\phi'(x+h)-\phi'(x-h)]/(2h)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line_
! USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), xp(1,3), xm(1,3)
! REAL(DFP) :: gp(3,3,1), gm(3,3,1), second(3,3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The hierarchical Laplacian forwards to empty Lobatto implementations.
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! xp = x+h
! xm = x-h
! gp = HeirarchicalBasisGradient_Line(2_I4B,xp,"BIUNIT")
! gm = HeirarchicalBasisGradient_Line(2_I4B,xm,"BIUNIT")
! second = (gp(:,:,1)-gm(:,:,1))/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(second(:,3)-3.0_DFP/SQRT(6.0_DFP))) &
!   < 1.0E-8_DFP, "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasisLaplacian_Line_
  MODULE SUBROUTINE HeirarchicalLaplacianBasis_Line2_( &
    order, xij, refLine, orient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of line: 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Laplacian of Hierarchical basis
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! SIZE(xij, 2), order + 1, 1
  END SUBROUTINE HeirarchicalLaplacianBasis_Line2_
END INTERFACE HeirarchicalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!                                                 BasisLaplacianEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis laplacian.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. Only Monomial has a working
! Laplacian here; named polynomial family branches can reach unimplemented
! routines. UNIT second derivative scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(order + 1).
!
! Formula:
! $$L_{ij}=\phi_j''(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = BasisLaplacianEvalAll_Line(order=2_I4B, x=x(1,:), &
!   refLine="BIUNIT", basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisLaplacianEvalAll_Line
  MODULE FUNCTION BasisLaplacianEvalAll_Line1( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev ! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(order + 1)
    !! Value of n+1  polynomials at point x
  END FUNCTION BasisLaplacianEvalAll_Line1
END INTERFACE BasisLaplacianEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis laplacian.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. Only Monomial has a working
! Laplacian here; named polynomial family branches can reach unimplemented
! routines. UNIT second derivative scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN). Evaluation point(s); columns for rank two
! arrays.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:), tsize.
!
! Formula:
! $$L_{ij}=\phi_j''(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL BasisLaplacianEvalAll_Line_(order=2_I4B, x=x(1,:), &
!   refLine="BIUNIT", basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisLaplacianEvalAll_Line_
  MODULE SUBROUTINE BasisLaplacianEvalAll_Line1_( &
    order, x, refLine, basisType, alpha, beta, lambda, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(order + 1)
    !! Value of n+1  polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! order + 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev
    !! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE BasisLaplacianEvalAll_Line1_
END INTERFACE BasisLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis laplacian.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. Only Monomial has a working
! Laplacian here; named polynomial family branches can reach unimplemented
! routines. UNIT second derivative scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), order + 1).
!
! Formula:
! $$L_{ij}=\phi_j''(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! y = BasisLaplacianEvalAll_Line(order=2_I4B, x=x(1,:), &
!   refLine="BIUNIT", basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisLaplacianEvalAll_Line
  MODULE FUNCTION BasisLaplacianEvalAll_Line2( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev
    !! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION BasisLaplacianEvalAll_Line2
END INTERFACE BasisLaplacianEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate line Basis laplacian.
!
! Results use point rows and degree columns. Gradient element wrappers add a
! coordinate axis. BIUNIT is used explicitly. Only Monomial has a working
! Laplacian here; named polynomial family branches can reach unimplemented
! routines. UNIT second derivative scaling requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!   basisType : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial family
! selector, for example poly%Monomial.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter alpha.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight parameter beta.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Ultraspherical family parameter.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$L_{ij}=\phi_j''(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1,3), y(3,3)
! INTEGER(I4B) :: nr,nc
!
! x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
! CALL BasisLaplacianEvalAll_Line_(order=2_I4B, x=x(1,:), &
!   refLine="BIUNIT", basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,3)- &
!   (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
! END PROGRAM main
!```

INTERFACE BasisLaplacianEvalAll_Line_
  MODULE SUBROUTINE BasisLaplacianEvalAll_Line2_( &
    order, x, ans, nrow, ncol, refLine, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to ans
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial ! Jacobi ! Ultraspherical ! Legendre ! Chebyshev
    !! Lobatto ! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE BasisLaplacianEvalAll_Line2_
END INTERFACE BasisLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LineInterpolationUtility
