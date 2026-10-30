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
! summary: QuadrangleInterpolationUtility geometry and interpolation
! utilities.
!
! Tensor product spaces have (p+1)(q+1) modes; Dubiner uses (p+1)(p+2)/2
! total-degree modes on collapsed square coordinates. Values use point rows
! and basis columns; gradients add a coordinate axis. Tensor routines
! currently use basisType1 for both directions, ignoring basisType2. The
! documented mixed-order examples use the same family in each direction.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$u(x,y)=\sum_{i=0}^{p}\sum_{j=0}^{q}c_{ij}P_i(x)Q_j(y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

MODULE QuadrangleInterpolationUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: stderr
USE String_Class, ONLY: String
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypeQuadratureOpt
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypePolynomialOpt

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
PUBLIC :: OrthogonalBasis_Quadrangle_

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
PUBLIC :: QuadraturePoint_Quadrangle_
PUBLIC :: QuadratureNumber_Quadrangle

PUBLIC :: FacetConnectivity_Quadrangle
PUBLIC :: RefElemDomain_Quadrangle

PUBLIC :: LagrangeGradientEvalAll_Quadrangle
PUBLIC :: LagrangeGradientEvalAll_Quadrangle_

PUBLIC :: HeirarchicalBasisGradient_Quadrangle
PUBLIC :: HeirarchicalBasisGradient_Quadrangle_

PUBLIC :: TensorProdBasisGradient_Quadrangle

PUBLIC :: OrthogonalBasisGradient_Quadrangle
PUBLIC :: OrthogonalBasisGradient_Quadrangle_

PUBLIC :: GetTotalDOF_Quadrangle
PUBLIC :: GetTotalInDOF_Quadrangle

PUBLIC :: GetHierarchicalDOF_Quadrangle

!----------------------------------------------------------------------------
!                                               GetHierarchicalDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count quadrilateral hierarchical vertex, edge, cell or total modes.
!
! opt selects V, E or C; other values return the total. Use positive entity
! orders. pe3 and pe4 are horizontal orders; qe1 and qe2 are vertical orders.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   opt : CHARACTER(1), INTENT(IN). Connectivity or entity selector; see the
! routine description.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: GetHierarchicalDOF_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: n
!
! n = GetHierarchicalDOF_Quadrangle(2_I4B,3_I4B, &
!   2_I4B,3_I4B,2_I4B,3_I4B,"H")
! CALL OK(n == 12, "Four vertices, six edge modes and two cell modes")
! CALL OK(GetHierarchicalDOF_Quadrangle(2_I4B,3_I4B, &
!   2_I4B,3_I4B,2_I4B,3_I4B,"C") == 2, "Anisotropic cell count")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GetHierarchicalDOF_Quadrangle( &
    pb, qb, pe3, pe4, qe1, qe2, opt) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb, qb
    !! cell order
    INTEGER(I4B), INTENT(IN) :: qe1, qe2, pe3, pe4
    !! face order
    CHARACTER(1), INTENT(IN) :: opt
    !! 'V' - vertex
    !! 'E' - edge
    !! 'C' - cell
    !! 'H' - total hierarchical dof
    INTEGER(I4B) :: ans
  END FUNCTION GetHierarchicalDOF_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalDOF_Quadrangle@DOFMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

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
!                                         LagrangeInDOF_Quadrangle@DOFMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

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
!                                         GetTotalInDOF_Quadrangle@DOFMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

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
!                                      LagrangeDOF_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

INTERFACE LagrangeDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Quadrangle1
END INTERFACE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                      LagrangeDOF_Quadrangle@LagrangeMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

INTERFACE LagrangeDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Quadrangle2
END INTERFACE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeInDOF_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

INTERFACE LagrangeInDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Quadrangle1
END INTERFACE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                    LagrangeInDOF_Quadrangle@LagrangeMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

INTERFACE LagrangeInDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Quadrangle2
END INTERFACE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                  LagrangeDegree_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

INTERFACE LagrangeDegree_Quadrangle
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Quadrangle1
END INTERFACE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeDegree_Quadrangle@LagrangeMethods
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
!                                   LagrangeDegree_Quadrangle@LagrangeMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

INTERFACE LagrangeDegree_Quadrangle
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Quadrangle2
END INTERFACE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                 LagrangeDegree_Quadrangle_@LagrangeMethods
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
!                                    MonomialBasis_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE MonomialBasis_Quadrangle_( &
    p, q, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of interpolation inside the quadrangle in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nrow = SIZE(xij, 2) -> Number of points of evaluation
    !! ncol = (p + 1) * (q + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE MonomialBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! c = LagrangeCoeff_Quadrangle(order=1_I4B,xij=nodes,basisType=poly%Monomial)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! CALL LagrangeCoeff_Quadrangle_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! c = LagrangeCoeff_Quadrangle(order=1_I4B,xij=nodes,basisType=poly%Monomial)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! CALL LagrangeCoeff_Quadrangle_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! c = LagrangeCoeff_Quadrangle(order=1_I4B,xij=nodes,basisType=poly%Monomial)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! CALL LagrangeCoeff_Quadrangle_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! c = LagrangeCoeff_Quadrangle(order=1_I4B,xij=nodes,basisType=poly%Monomial)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! CALL LagrangeCoeff_Quadrangle_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType1 : INTEGER(I4B), INTENT(IN). First directional family selector;
! currently also used in the second direction.
!   basisType2 : INTEGER(I4B), INTENT(IN). Intended second directional family;
! currently ignored by Quadrangle tensor evaluation.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! c = LagrangeCoeff_Quadrangle(order=1_I4B,xij=nodes,basisType=poly%Monomial)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType1 : INTEGER(I4B), INTENT(IN). First directional family selector;
! currently also used in the second direction.
!   basisType2 : INTEGER(I4B), INTENT(IN). Intended second directional family;
! currently ignored by Quadrangle tensor evaluation.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),c(4,4),v(4,4)
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! INTEGER(I4B) :: i,j,nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! CALL LagrangeCoeff_Quadrangle_(order=1_I4B,xij=nodes, &
!   basisType=poly%Monomial,ans=c,nrow=nr,ncol=nc)
! degree = LagrangeDegree_Quadrangle(1_I4B)
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
!                                  LagrangeEvalAll_Quadrangle@LagrangeMethods
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
!   x : REAL(DFP), INTENT(IN)(2). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4)
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! y = LagrangeEvalAll_Quadrangle(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Quadrangle
  MODULE FUNCTION LagrangeEvalAll_Quadrangle1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
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
!                                  LagrangeEvalAll_Quadrangle@LagrangeMethods
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
!   x : REAL(DFP), INTENT(IN)(2). Evaluation point(s); columns for rank two
! arrays.
!   xij : REAL(DFP), INTENT(INOUT)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:). Result buffer; allocate the full active
! shape before the call.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active output entries.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4)
! INTEGER(I4B) :: nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! CALL LagrangeEvalAll_Quadrangle_(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle1_( &
    order, x, xij, ans, tsize, coeff, firstCall, basisType, alpha, beta, &
    lambda)
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
    !! Legendre ! Lobatto ! Chebyshev ! Jacobi ! Ultraspherical ! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Quadrangle1_
END INTERFACE LagrangeEvalAll_Quadrangle_

!----------------------------------------------------------------------------
!                                  LagrangeEvalAll_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4)
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! y = LagrangeEvalAll_Quadrangle(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Quadrangle
  MODULE FUNCTION LagrangeEvalAll_Quadrangle2( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
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
!                                 LagrangeEvalAll_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4)
! INTEGER(I4B) :: nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! CALL LagrangeEvalAll_Quadrangle_(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle2_( &
    order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, alpha, &
    beta, lambda)
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
!                                 LagrangeEvalAll_Quadrangle@LagrangeMethods
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
!   coeff : REAL(DFP), INTENT(INOUT)(:, :). Optional coefficient cache; must
! match the same nodes, order and basis.
!   xx : REAL(DFP), INTENT(INOUT)(:, :). Reusable point by basis workspace;
! shape must match the selected overload.
!   firstCall : LOGICAL(LGT). Use .TRUE. to construct the coefficient cache,
! then explicitly pass .FALSE. to reuse it.
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4)
! INTEGER(I4B) :: nr,nc
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! CALL LagrangeEvalAll_Quadrangle_(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial,ans=y,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Partition of unity")
! CALL OK(ABS(y(1,1)-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(y(1,2)) < 1.0E-11_DFP, "Nodal delta property")
! END PROGRAM main
!```

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle3_( &
    order, x, xij, ans, nrow, ncol, coeff, xx, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation, x(1, :) is x coord, x(2, :) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    !! nrow = number of points of evaluation
    !! ncol = number of degrees of freedom
    REAL(DFP), INTENT(INOUT) :: coeff(:, :), xx(:, :)
    !! Coefficient of Lagrange polynomials, The size is ncol by ncol
    !! The size of xx is nrow by ncol (it is used internally)
    !! nrow is number of points of evaluation
    !! ncol is number of degrees of freedom
    LOGICAL(LGT) :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default, Jacobi=Dubiner, Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Quadrangle3_
END INTERFACE LagrangeEvalAll_Quadrangle_

!----------------------------------------------------------------------------
!                          LagrangeGradientEvalAll_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeGradientEvalAll_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4,2)
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! y = LagrangeGradientEvalAll_Quadrangle(order=1_I4B,x=points,xij=nodes, &
!   basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
!   < 1.0E-11_DFP, "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Quadrangle
  MODULE FUNCTION LagrangeGradientEvalAll_Quadrangle1( &
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
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 2)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
  END FUNCTION LagrangeGradientEvalAll_Quadrangle1
END INTERFACE LagrangeGradientEvalAll_Quadrangle

!----------------------------------------------------------------------------
!                         LagrangeGradientEvalAll_Quadrangle@LagrangeMethods
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
! USE QuadrangleInterpolationUtility, &
!   ONLY: LagrangeGradientEvalAll_Quadrangle_
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,4),points(2,4),y(4,4,2)
! INTEGER(I4B) :: nr,nc,nd
!
! nodes = RefCoord_Quadrangle("BIUNIT")
! points = nodes
! CALL LagrangeGradientEvalAll_Quadrangle_(order=1_I4B,x=points, &
!   xij=nodes,basisType=poly%Monomial,ans=y,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(y,DIM=2))) &
!   < 1.0E-11_DFP, "Gradient of partition of unity")
! END PROGRAM main
!```

INTERFACE LagrangeGradientEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Quadrangle1_( &
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
!                                            RefElemDomain_Quadrangle@Methods
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
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), ALLOCATABLE :: degree(:,:)
! TYPE(String) :: domain
!
! CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
! CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
! CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
!   "Total count dispatcher")
! CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
!   "Interior count dispatcher")
! degree = LagrangeDegree_Quadrangle(2_I4B)
! CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
! domain = RefElemDomain_Quadrangle("H1","Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Reference domain")
! CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
!   "Anisotropic total count")
! END PROGRAM main
!```

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
!                                        FacetConnectivity_Quadrangle@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return quadrilateral vertex connectivity.
!
! The optional family strings are currently ignored. ReferenceQuadrangle edge
! option 1 uses a different edge ordering; keep connectivity and basis
! conventions consistent.
!
! Arguments:
!   baseInterpol : CHARACTER(*), OPTIONAL, INTENT(IN). Interpolation family
! string, for example Lagrange or Hierarchy.
!   baseContinuity : CHARACTER(*), OPTIONAL, INTENT(IN). Basis continuity
! string, for example H1.
!
! Returns:
!   ans : INTEGER(I4B)(2, 4).
!
! Formula:
! $$\partial Q=e_{12}\cup e_{23}\cup e_{34}\cup e_{41}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: FacetConnectivity_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: edges(2,4)
!
! edges = FacetConnectivity_Quadrangle()
! CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
!   ALL(edges(:,4) == [4_I4B,1_I4B]), "Counterclockwise boundary edges")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION FacetConnectivity_Quadrangle(baseInterpol, baseContinuity) &
    RESULT(ans)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpol
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2, 4)
    !! rows represents the end points of an edges
    !! columns denote the edge (facet)
  END FUNCTION FacetConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                      EquidistancePoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
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
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = EquidistancePoint_Quadrangle(order=2_I4B)
! CALL OK(ALL(SHAPE(nodes) == [2,9]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

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
!                     EquidistancePoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
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
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,9)
! INTEGER(I4B) :: nr,nc
!
! CALL EquidistancePoint_Quadrangle_(order=2_I4B,ans=nodes,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 9, &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE EquidistancePoint_Quadrangle_
  MODULE RECURSIVE SUBROUTINE EquidistancePoint_Quadrangle1_(order, ans, &
                                                             nrow, ncol, xij)
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
!                      EquidistancePoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = EquidistancePoint_Quadrangle(order=2_I4B)
! CALL OK(ALL(SHAPE(nodes) == [2,9]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

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
!                     EquidistancePoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
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
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,9)
! INTEGER(I4B) :: nr,nc
!
! CALL EquidistancePoint_Quadrangle_(order=2_I4B,ans=nodes,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 9, &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

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
!                    EquidistanceInPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct interior quadrangle interpolation points.
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
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: EquidistanceInPoint_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = EquidistanceInPoint_Quadrangle(order=2_I4B)
! CALL OK(ALL(SHAPE(nodes) == [2,1]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes-(0.0_DFP))) &
!   < 1.0E-11_DFP, "Single interior point is the centroid")
! END PROGRAM main
!```

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
!                    EquidistanceInPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct interior quadrangle interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: EquidistanceInPoint_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = EquidistanceInPoint_Quadrangle(order=2_I4B)
! CALL OK(ALL(SHAPE(nodes) == [2,1]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes-(0.0_DFP))) &
!   < 1.0E-11_DFP, "Single interior point is the centroid")
! END PROGRAM main
!```

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
!                    InterpolationPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
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
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = InterpolationPoint_Quadrangle(order=2_I4B,ipType=Equidistance, &
!   layout="VEFC")
! CALL OK(ALL(SHAPE(nodes) == [2,9]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle1( &
    order, ipType, layout, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
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
!                   InterpolationPoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
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
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,9)
! INTEGER(I4B) :: nr,nc
!
! CALL InterpolationPoint_Quadrangle_(order=2_I4B,ipType=Equidistance, &
!   layout="VEFC",ans=nodes,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 9, &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Quadrangle_
  MODULE SUBROUTINE InterpolationPoint_Quadrangle1_( &
    order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
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
!                    InterpolationPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   ipType1 : INTEGER(I4B), INTENT(IN). First directional interpolation node
! selector.
!   ipType2 : INTEGER(I4B), INTENT(IN). Second directional interpolation node
! selector.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:,:)
!
! nodes = InterpolationPoint_Quadrangle(order=2_I4B,ipType=Equidistance, &
!   layout="VEFC")
! CALL OK(ALL(SHAPE(nodes) == [2,9]), &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle2( &
    p, q, ipType1, ipType2, layout, xij, alpha1, beta1, lambda1, alpha2, &
    beta2, lambda2) RESULT(ans)
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
!                    InterpolationPoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct quadrangle interpolation points.
!
! Coordinates have point columns. Defaults are BIUNIT for Quadrangle and UNIT
! for Tetrahedron. VEFC puts vertices, edges, faces, then cell nodes first.
! Supply vertex xij to map to another element. Tetrahedron layout comparison
! is case sensitive and must be exactly VEFC to reorder; its recursive node
! work arrays limit supported orders.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   ipType1 : INTEGER(I4B), INTENT(IN). First directional interpolation node
! selector.
!   ipType2 : INTEGER(I4B), INTENT(IN). Second directional interpolation node
! selector.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   layout : CHARACTER(*), INTENT(IN). Point ordering; VEFC puts vertices
! before edge/interior points.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$x_i=-1+2i/p,\quad y_j=-1+2j/q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: nodes(2,9)
! INTEGER(I4B) :: nr,nc
!
! CALL InterpolationPoint_Quadrangle_(order=2_I4B,ipType=Equidistance, &
!   layout="VEFC",ans=nodes,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 9, &
!   "Interpolation coordinate dimensions")
! CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
! END PROGRAM main
!```

INTERFACE InterpolationPoint_Quadrangle_
  MODULE SUBROUTINE InterpolationPoint_Quadrangle2_( &
    p, q, ipType1, ipType2, ans, nrow, ncol, layout, xij, alpha1, beta1, &
    lambda1, alpha2, beta2, lambda2)
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
!                                IJ2VEFC_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-17
! summary:  Convert interpolation point format from IJ to VEFC

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
!                                IJ2VEFC_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorder a tensor grid into clockwise entity layout.
!
! xi and eta have shape (p+1,q+1); output has two coordinate rows and
! (p+1)(q+1) point columns. startNode selects one of the four corners;
! direction sets the boundary traversal. Interior rings are processed
! recursively.
!
! Arguments:
!   xi : REAL(DFP), INTENT(IN)(:, :). Grid of first coordinates.
!   eta : REAL(DFP), INTENT(IN)(:, :). Grid of second coordinates.
!   temp : REAL(DFP), INTENT(OUT)(:, :). Output point coordinates in VEFC
! ordering.
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   startNode : INTEGER(I4B), INTENT(IN). Starting corner identifier, from one
! to four.
!
! Returns:
!   Updates temp(:, :).
!
! Formula:
! $$N=(p+1)(q+1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: IJ2VEFC_Quadrangle_Clockwise
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: xi(2,2),eta(2,2),nodes(2,4)
!
! xi(:,1) = [-1.0_DFP,1.0_DFP]
! xi(:,2) = xi(:,1)
! eta(1,:) = [-1.0_DFP,1.0_DFP]
! eta(2,:) = eta(1,:)
! CALL IJ2VEFC_Quadrangle_Clockwise(xi,eta,nodes, &
!   p=1_I4B,q=1_I4B,startNode=1_I4B)
! CALL OK(MAXVAL(ABS(nodes(:,1)+1.0_DFP)) < 1.0E-12_DFP, &
!   "Requested first corner")
! CALL OK(ALL(ABS(nodes) == 1.0_DFP), "All four corners are retained")
! CALL OK(ABS(SUM(nodes(1,:))) < 1.0E-12_DFP .AND. &
!   ABS(SUM(nodes(2,:))) < 1.0E-12_DFP, "Each coordinate is balanced")
! END PROGRAM main
!```

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
!                                IJ2VEFC_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorder a tensor grid into anticlockwise entity layout.
!
! xi and eta have shape (p+1,q+1); output has two coordinate rows and
! (p+1)(q+1) point columns. startNode selects one of the four corners;
! direction sets the boundary traversal. Interior rings are processed
! recursively.
!
! Arguments:
!   xi : REAL(DFP), INTENT(IN)(:, :). Grid of first coordinates.
!   eta : REAL(DFP), INTENT(IN)(:, :). Grid of second coordinates.
!   temp : REAL(DFP), INTENT(OUT)(:, :). Output point coordinates in VEFC
! ordering.
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   startNode : INTEGER(I4B), INTENT(IN). Starting corner identifier, from one
! to four.
!
! Returns:
!   Updates temp(:, :).
!
! Formula:
! $$N=(p+1)(q+1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: IJ2VEFC_Quadrangle_AntiClockwise
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: xi(2,2),eta(2,2),nodes(2,4)
!
! xi(:,1) = [-1.0_DFP,1.0_DFP]
! xi(:,2) = xi(:,1)
! eta(1,:) = [-1.0_DFP,1.0_DFP]
! eta(2,:) = eta(1,:)
! CALL IJ2VEFC_Quadrangle_AntiClockwise(xi,eta,nodes, &
!   p=1_I4B,q=1_I4B,startNode=1_I4B)
! CALL OK(MAXVAL(ABS(nodes(:,1)+1.0_DFP)) < 1.0E-12_DFP, &
!   "Requested first corner")
! CALL OK(ALL(ABS(nodes) == 1.0_DFP), "All four corners are retained")
! CALL OK(ABS(SUM(nodes(1,:))) < 1.0E-12_DFP .AND. &
!   ABS(SUM(nodes(2,:))) < 1.0E-12_DFP, "Each coordinate is balanced")
! END PROGRAM main
!```

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
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate Dubiner basis.
!
! This has (p+1)(p+2)/2 total-degree modes, not the (p+1)^2 tensor-product
! space. Coordinates are on the collapsed BIUNIT square. The separate x,y
! value overload forms a tensor grid of points. Modes are ordered
! (0,0),(0,1),...,(1,0),... .
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (order + 1) * (order + 2) / 2).
!
! Formula:
! $$\phi_{ij}=P_i(x)((1-y)/2)^iP_j^{(2i+1,0)}(y),\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,3)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = Dubiner_Quadrangle(order=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-12_DFP, &
!   "Constant total degree mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(0.5_DFP+1.5_DFP*x(2,:)))) &
!   < 1.0E-12_DFP, "First nonconstant Jacobi mode")
! END PROGRAM main
!```

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
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate Dubiner basis.
!
! This has (p+1)(p+2)/2 total-degree modes, not the (p+1)^2 tensor-product
! space. Coordinates are on the collapsed BIUNIT square. The separate x,y
! value overload forms a tensor grid of points. Modes are ordered
! (0,0),(0,1),...,(1,0),... .
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
! $$\phi_{ij}=P_i(x)((1-y)/2)^iP_j^{(2i+1,0)}(y),\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,3)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL Dubiner_Quadrangle_(order=1_I4B,xij=x,ans=v,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-12_DFP, &
!   "Constant total degree mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(0.5_DFP+1.5_DFP*x(2,:)))) &
!   < 1.0E-12_DFP, "First nonconstant Jacobi mode")
! END PROGRAM main
!```

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
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate Dubiner basis.
!
! This has (p+1)(p+2)/2 total-degree modes, not the (p+1)^2 tensor-product
! space. Coordinates are on the collapsed BIUNIT square. The separate x,y
! value overload forms a tensor grid of points. Modes are ordered
! (0,0),(0,1),...,(1,0),... .
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2).
!
! Formula:
! $$\phi_{ij}=P_i(x)((1-y)/2)^iP_j^{(2i+1,0)}(y),\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,3)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = Dubiner_Quadrangle(order=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-12_DFP, &
!   "Constant total degree mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(0.5_DFP+1.5_DFP*x(2,:)))) &
!   < 1.0E-12_DFP, "First nonconstant Jacobi mode")
! END PROGRAM main
!```

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
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the collapsed-coordinate Dubiner basis.
!
! This has (p+1)(p+2)/2 total-degree modes, not the (p+1)^2 tensor-product
! space. Coordinates are on the collapsed BIUNIT square. The separate x,y
! value overload forms a tensor grid of points. Modes are ordered
! (0,0),(0,1),...,(1,0),... .
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$\phi_{ij}=P_i(x)((1-y)/2)^iP_j^{(2i+1,0)}(y),\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,3)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL Dubiner_Quadrangle_(order=1_I4B,xij=x,ans=v,nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-12_DFP, &
!   "Constant total degree mode")
! CALL OK(MAXVAL(ABS(v(:,2)-(0.5_DFP+1.5_DFP*x(2,:)))) &
!   < 1.0E-12_DFP, "First nonconstant Jacobi mode")
! END PROGRAM main
!```

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
!                                              DubinerGradient@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the collapsed-coordinate Dubiner basis.
!
! This has (p+1)(p+2)/2 total-degree modes, not the (p+1)^2 tensor-product
! space. Coordinates are on the collapsed BIUNIT square. The separate x,y
! value overload forms a tensor grid of points. Modes are ordered
! (0,0),(0,1),...,(1,0),... .
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2),  (order + 1_I4B) * (order + 2_I4B) / 2_I4B,
! 2_I4B).
!
! Formula:
! $$\phi_{ij}=P_i(x)((1-y)/2)^iP_j^{(2i+1,0)}(y),\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: DubinerGradient_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,3,2)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = DubinerGradient_Quadrangle(order=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(v(:,1,:))) < 1.0E-12_DFP, "Constant gradient")
! CALL OK(MAXVAL(ABS(v(:,2,2)-1.5_DFP)) < 1.0E-12_DFP, &
!   "Jacobi linear mode derivative")
! END PROGRAM main
!```

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
!                                              DubinerGradient@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the collapsed-coordinate Dubiner basis.
!
! This has (p+1)(p+2)/2 total-degree modes, not the (p+1)^2 tensor-product
! space. Coordinates are on the collapsed BIUNIT square. The separate x,y
! value overload forms a tensor grid of points. Modes are ordered
! (0,0),(0,1),...,(1,0),... .
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
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
! $$\phi_{ij}=P_i(x)((1-y)/2)^iP_j^{(2i+1,0)}(y),\quad i+j\le p$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: DubinerGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,3,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL DubinerGradient_Quadrangle_(order=1_I4B,xij=x,ans=v,tsize1=nr, &
!   tsize2=nc,tsize3=nd)
! CALL OK(MAXVAL(ABS(v(:,1,:))) < 1.0E-12_DFP, "Constant gradient")
! CALL OK(MAXVAL(ABS(v(:,2,2)-1.5_DFP)) < 1.0E-12_DFP, &
!   "Jacobi linear mode derivative")
! END PROGRAM main
!```

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
!                                TensorProdBasis_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType1 : INTEGER(I4B), INTENT(IN). First directional family selector;
! currently also used in the second direction.
!   basisType2 : INTEGER(I4B), INTENT(IN). Intended second directional family;
! currently ignored by Quadrangle tensor evaluation.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (p + 1) * (q + 1)).
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Quadrangle
  MODULE PROCEDURE TensorProdBasis_Quadrangle1
END INTERFACE OrthogonalBasis_Quadrangle

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Quadrangle_
  MODULE PROCEDURE TensorProdBasis_Quadrangle1_
END INTERFACE OrthogonalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!   basisType1 : INTEGER(I4B), INTENT(IN). First directional family selector;
! currently also used in the second direction.
!   basisType2 : INTEGER(I4B), INTENT(IN). Intended second directional family;
! currently ignored by Quadrangle tensor evaluation.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x) * SIZE(y), (p + 1) * (q + 1)).
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Quadrangle
  MODULE PROCEDURE TensorProdBasis_Quadrangle2
END INTERFACE OrthogonalBasis_Quadrangle

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Quadrangle_
  MODULE PROCEDURE TensorProdBasis_Quadrangle2_
END INTERFACE OrthogonalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate VertexBasis quadrilateral hierarchical modes.
!
! Uses BIUNIT coordinates. Unlike tensor-grid basis overloads, separate x,y
! entity basis inputs are paired point vectors and must have equal size.
! Vertical orders are qe1/qe2; horizontal orders are pe3/pe4. Cell orders
! pb/qb determine (pb-1)(qb-1) interior columns.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), 4).
!
! Formula:
! $$\phi_{v_1}=(1-x)(1-y)/4,\quad\phi_c=L_i(x)L_j(y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: VertexBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,4),v(4,4)
!
! x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
!   1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
! v = VertexBasis_Quadrangle(xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Vertex partition of unity")
! END PROGRAM main
!```

INTERFACE VertexBasis_Quadrangle
  MODULE PURE FUNCTION VertexBasis_Quadrangle1(x, y) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Quadrangle1
END INTERFACE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
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
!                                  VertexBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate VertexBasis quadrilateral hierarchical modes.
!
! Uses BIUNIT coordinates. Unlike tensor-grid basis overloads, separate x,y
! entity basis inputs are paired point vectors and must have equal size.
! Vertical orders are qe1/qe2; horizontal orders are pe3/pe4. Cell orders
! pb/qb determine (pb-1)(qb-1) interior columns.
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), 4).
!
! Formula:
! $$\phi_{v_1}=(1-x)(1-y)/4,\quad\phi_c=L_i(x)L_j(y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: VertexBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,4),v(4,4)
!
! x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
!   1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
! v = VertexBasis_Quadrangle(xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Vertex partition of unity")
! END PROGRAM main
!```

INTERFACE VertexBasis_Quadrangle
  MODULE PURE FUNCTION VertexBasis_Quadrangle2(xij) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(xij, 2), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Quadrangle2
END INTERFACE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
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
!                            VerticalEdgeBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate VerticalEdgeBasis quadrilateral hierarchical modes.
!
! Uses BIUNIT coordinates. Unlike tensor-grid basis overloads, separate x,y
! entity basis inputs are paired point vectors and must have equal size.
! Vertical orders are qe1/qe2; horizontal orders are pe3/pe4. Cell orders
! pb/qb determine (pb-1)(qb-1) interior columns.
!
! Arguments:
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), qe1 + qe2 - 2).
!
! Formula:
! $$\phi_{v_1}=(1-x)(1-y)/4,\quad\phi_c=L_i(x)L_j(y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: VerticalEdgeBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,4),v(4,2)
!
! x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
!   1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
! v = VerticalEdgeBasis_Quadrangle(qe1=2_I4B,qe2=2_I4B,x=x(1,:),y=x(2,:))
! CALL OK(MAXVAL(ABS(v)) &
!   < 1.0E-12_DFP, "Interior entity modes vanish at vertices")
! END PROGRAM main
!```

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
!                           VerticalEdgeBasis_Quadrangle@HierarchicalMethods
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
!                          HorizontalEdgeBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate HorizontalEdgeBasis quadrilateral hierarchical modes.
!
! Uses BIUNIT coordinates. Unlike tensor-grid basis overloads, separate x,y
! entity basis inputs are paired point vectors and must have equal size.
! Vertical orders are qe1/qe2; horizontal orders are pe3/pe4. Cell orders
! pb/qb determine (pb-1)(qb-1) interior columns.
!
! Arguments:
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), pe3 + pe4 - 2).
!
! Formula:
! $$\phi_{v_1}=(1-x)(1-y)/4,\quad\phi_c=L_i(x)L_j(y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HorizontalEdgeBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,4),v(4,2)
!
! x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
!   1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
! v = HorizontalEdgeBasis_Quadrangle(pe3=2_I4B,pe4=2_I4B,x=x(1,:),y=x(2,:))
! CALL OK(MAXVAL(ABS(v)) &
!   < 1.0E-12_DFP, "Interior entity modes vanish at vertices")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION HorizontalEdgeBasis_Quadrangle(pe3, pe4, x, y) &
    RESULT(ans)
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
!                                   CellBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate CellBasis quadrilateral hierarchical modes.
!
! Uses BIUNIT coordinates. Unlike tensor-grid basis overloads, separate x,y
! entity basis inputs are paired point vectors and must have equal size.
! Vertical orders are qe1/qe2; horizontal orders are pe3/pe4. Cell orders
! pb/qb determine (pb-1)(qb-1) interior columns.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   y : REAL(DFP), INTENT(IN)(:). Second coordinate vector; paired or tensor
! grid as stated above.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), (pb - 1) * (qb - 1)).
!
! Formula:
! $$\phi_{v_1}=(1-x)(1-y)/4,\quad\phi_c=L_i(x)L_j(y)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: CellBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,4),v(4,1)
!
! x = RESHAPE([-1.0_DFP,-1.0_DFP,1.0_DFP,-1.0_DFP, &
!   1.0_DFP,1.0_DFP,-1.0_DFP,1.0_DFP],[2,4])
! v = CellBasis_Quadrangle(pb=2_I4B,qb=2_I4B,x=x(1,:),y=x(2,:))
! CALL OK(MAXVAL(ABS(v)) &
!   < 1.0E-12_DFP, "Interior entity modes vanish at vertices")
! END PROGRAM main
!```

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
!                                   CellBasis_Quadrangle@HierarchicalMethods
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
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2),   pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2
! + 1).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = HeirarchicalBasis_Quadrangle(p=1_I4B,q=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
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
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL HeirarchicalBasis_Quadrangle_(p=1_I4B,q=1_I4B,xij=x,ans=v, &
!   nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle1_( &
    pb, qb, pe3, pe4, qe1, qe2, xij, ans, nrow, ncol)
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
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (p + 1) * (q + 1)).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = HeirarchicalBasis_Quadrangle(p=1_I4B,q=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                           HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
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
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL HeirarchicalBasis_Quadrangle_(p=1_I4B,q=1_I4B,xij=x,ans=v, &
!   nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                           HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   pe3Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   pe4Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   qe1Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   qe2Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   faceOrient : INTEGER(I4B), INTENT(IN)(:). Quadrilateral orientation code
! with three signs.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = HeirarchicalBasis_Quadrangle(p=1_I4B,q=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle3( &
    pb, qb, pe3, pe4, qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, &
    qe2Orient, faceOrient) RESULT(ans)
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
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   pe3Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   pe4Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   qe1Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   qe2Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   faceOrient : INTEGER(I4B), INTENT(IN)(:). Quadrilateral orientation code
! with three signs.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result buffer; allocate the full
! active shape before the call.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Updates ans(:, :), nrow, ncol.
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4)
! INTEGER(I4B) :: nr,nc
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL HeirarchicalBasis_Quadrangle_(p=1_I4B,q=1_I4B,xij=x,ans=v, &
!   nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle3_( &
    pb, qb, pe3, pe4, qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, &
    qe2Orient, faceOrient, ans, nrow, ncol)
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
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2),   pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2
! + 1, 2).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4,2)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = HeirarchicalBasisGradient_Quadrangle(p=1_I4B,q=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
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
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL HeirarchicalBasisGradient_Quadrangle_(p=1_I4B,q=1_I4B,xij=x, &
!   ans=v,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (p + 1) * (q + 1), 2).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4,2)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = HeirarchicalBasisGradient_Quadrangle(p=1_I4B,q=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
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
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL HeirarchicalBasisGradient_Quadrangle_(p=1_I4B,q=1_I4B,xij=x, &
!   ans=v,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   qe1Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   qe2Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   pe3Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   pe4Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   faceOrient : INTEGER(I4B), INTENT(IN)(3). Quadrilateral orientation code
! with three signs.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :, :).
!
! Formula:
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4,2)
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = HeirarchicalBasisGradient_Quadrangle(p=1_I4B,q=1_I4B,xij=x)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate gradients of the hierarchical quadrilateral basis.
!
! Uses BIUNIT coordinates. The public spelling Heirarchical is retained.
! Uniform directional p,q overloads or separate cell and edge orders are
! available. Entity columns are vertices, bottom/right/top/left edges, then
! interior modes. Oriented overloads accept four edge signs and a three entry
! face sign code from ReferenceQuadrangle; it differs from triangular
! rotation/sign metadata.
!
! Arguments:
!   pb : INTEGER(I4B), INTENT(IN). First cell interior order.
!   qb : INTEGER(I4B), INTENT(IN). Second cell interior order.
!   pe3 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   pe4 : INTEGER(I4B), INTENT(IN). Polynomial order on the named edge
! (horizontal on Quadrangle).
!   qe1 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   qe2 : INTEGER(I4B), INTENT(IN). Polynomial order on the named vertical
! Quadrangle edge.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   qe1Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   qe2Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   pe3Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   pe4Orient : INTEGER(I4B), INTENT(IN). Orientation sign on the named
! boundary edge.
!   faceOrient : INTEGER(I4B), INTENT(IN)(3). Quadrilateral orientation code
! with three signs.
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
! $$N=4+\sum_e(p_e-1)+(p_b-1)(q_b-1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE QuadrangleInterpolationUtility, &
!   ONLY: HeirarchicalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,4,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! CALL HeirarchicalBasisGradient_Quadrangle_(p=1_I4B,q=1_I4B,xij=x, &
!   ans=v,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
!   < 1.0E-12_DFP, "Linear vertex partition identity")
! END PROGRAM main
!```

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
!                       TensorProdBasisGradient_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   basisType1 : INTEGER(I4B), INTENT(IN). First directional family selector;
! currently also used in the second direction.
!   basisType2 : INTEGER(I4B), INTENT(IN). Intended second directional family;
! currently ignored by Quadrangle tensor evaluation.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), (p + 1) * (q + 1), 2).
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisGradient_Quadrangle
  MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1
END INTERFACE OrthogonalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!                       TensorProdBasisGradient_Quadrangle@TensorProdMethods
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate anisotropic tensor product values and gradients.
!
! Coordinates are BIUNIT coordinates. basisType2 is currently ignored: both
! directional evaluations use basisType1, including gradient assembly. Use the
! same family in both directions until repaired. Directional alpha/beta/lambda
! parameters remain separate. Columns enumerate the first coordinate degree
! fastest. Separate x,y overloads form a tensor point grid, not paired
! coordinates.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{ij}(x,y)=P_i(x)Q_j(y),\quad0\le i\le p,\ 0\le j\le q$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
! USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
! USE QuadrangleInterpolationUtility, &
!   ONLY: OrthogonalBasisGradient_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
! INTEGER(I4B) :: nr,nc,nd
!
! x(:,1) = [0.2_DFP,-0.4_DFP]
! x(:,2) = [-0.3_DFP,0.5_DFP]
! v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
! CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=alias,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
! CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
!   < 1.0E-12_DFP, "First coordinate degree varies fastest")
! g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial)
! CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
! CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
!   basisType1=poly%Monomial,basisType2=poly%Monomial, &
!   ans=ga,dim1=nr,dim2=nc,dim3=nd)
! CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
!   < 1.0E-12_DFP, "Derivative in first direction")
! CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
!   "Derivative in second direction")
! END PROGRAM main
!```

INTERFACE OrthogonalBasisGradient_Quadrangle_
  MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1_
END INTERFACE OrthogonalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                               QuadratureNumber_Quadrangle@QuadratureMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   quadType1 : INTEGER(I4B), INTENT(IN). First directional quadrature family
! selector.
!   quadType2 : INTEGER(I4B), INTENT(IN). Second directional quadrature family
! selector.
!
! Returns:
!   ans : INTEGER(I4B)(2).
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
! USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n(2),nr,nc
!
! n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
!   GaussLegendreQP,GaussLegendreQP)
! ALLOCATE(buffer(3,PRODUCT(n)))
! q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
!   refQuadrangle="BIUNIT")
! CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
!   quadType1=GaussLegendreQP, &
!   quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
!   nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Quadrangle(p, q, quadType1, &
                                                   quadType2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    INTEGER(I4B) :: ans(2)
  END FUNCTION QuadratureNumber_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                QuadraturePoint_Quadrangle@QuadratureMethods
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
!   refQuadrangle : CHARACTER(*), INTENT(IN). Reference square domain, UNIT or
! BIUNIT, when this overload supports mapping.
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
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n(2),nr,nc
!
! n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
!   GaussLegendreQP,GaussLegendreQP)
! ALLOCATE(buffer(3,PRODUCT(n)))
! q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
!   refQuadrangle="BIUNIT")
! CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
!   quadType1=GaussLegendreQP, &
!   quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
!   nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle1(order, quadType, &
                          refQuadrangle, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of integrand in x and y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle ! UNIT ! BIUNIT
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
!                                QuadraturePoint_Quadrangle@QuadratureMethods
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
!   p : INTEGER(I4B), INTENT(IN). First directional polynomial order, or
! uniform entity order.
!   q : INTEGER(I4B), INTENT(IN). Second directional polynomial order.
!   quadType1 : INTEGER(I4B), INTENT(IN). First directional quadrature family
! selector.
!   quadType2 : INTEGER(I4B), INTENT(IN). Second directional quadrature family
! selector.
!   refQuadrangle : CHARACTER(*), INTENT(IN). Reference square domain, UNIT or
! BIUNIT, when this overload supports mapping.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
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
! USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n(2),nr,nc
!
! n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
!   GaussLegendreQP,GaussLegendreQP)
! ALLOCATE(buffer(3,PRODUCT(n)))
! q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
!   refQuadrangle="BIUNIT")
! CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
!   quadType1=GaussLegendreQP, &
!   quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
!   nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle2(p, q, quadType1, quadType2, &
         refQuadrangle, xij, alpha1, beta1, lambda1, alpha2, beta2, lambda2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of integrand in x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of  integrand in y direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! quadrature point type in x direction, see above
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle ! UNIT ! BIUNIT
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
!                                QuadraturePoint_Quadrangle@QuadratureMethods
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
!   refQuadrangle : CHARACTER(*), INTENT(IN). Reference square domain, UNIT or
! BIUNIT, when this overload supports mapping.
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
! $$\int_K f\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLegendreQP
! USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n(2),nr,nc
!
! n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
!   GaussLegendreQP,GaussLegendreQP)
! ALLOCATE(buffer(3,PRODUCT(n)))
! q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
!   refQuadrangle="BIUNIT")
! CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
!   quadType1=GaussLegendreQP, &
!   quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
!   nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle3(nips, quadType, &
                          refQuadrangle, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of integration points in x and y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! interpolation point type, see above
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle ! UNIT ! BIUNIT
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
!                                QuadraturePoint_Quadrangle@QuadratureMethods
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
!   nipsx : INTEGER(I4B), INTENT(IN)(1). Point count in the first quadrature
! direction (length one array).
!   nipsy : INTEGER(I4B), INTENT(IN)(1). Point count in the second quadrature
! direction (length one array).
!   quadType1 : INTEGER(I4B), INTENT(IN). First directional quadrature family
! selector.
!   quadType2 : INTEGER(I4B), INTENT(IN). Second directional quadrature family
! selector.
!   refQuadrangle : CHARACTER(*), INTENT(IN). Reference square domain, UNIT or
! BIUNIT, when this overload supports mapping.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
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
! USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n(2),nr,nc
!
! n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
!   GaussLegendreQP,GaussLegendreQP)
! ALLOCATE(buffer(3,PRODUCT(n)))
! q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
!   refQuadrangle="BIUNIT")
! CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
!   quadType1=GaussLegendreQP, &
!   quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
!   nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

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
    !! Equidistance ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadauLeft ! GaussLegendreRadauRight ! GaussChebyshev1
    !! GaussChebyshev1Lobatto ! GaussChebyshev1RadauLeft
    !! GaussChebyshev1RadauRight ! GaussUltraspherical
    !! GaussUltrasphericalLobatto ! GaussUltrasphericalRadauLeft
    !! GaussUltrasphericalRadauRight ! GaussJacobi
    !! GaussJacobiLobatto ! GaussJacobiRadauLeft ! GaussJacobiRadauRight
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle ! UNIT ! BIUNIT
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
!                               QuadraturePoint_Quadrangle@QuadratureMethods
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
!   nipsx : INTEGER(I4B), INTENT(IN)(1). Point count in the first quadrature
! direction (length one array).
!   nipsy : INTEGER(I4B), INTENT(IN)(1). Point count in the second quadrature
! direction (length one array).
!   quadType1 : INTEGER(I4B), INTENT(IN). First directional quadrature family
! selector.
!   quadType2 : INTEGER(I4B), INTENT(IN). Second directional quadrature family
! selector.
!   refQuadrangle : CHARACTER(*), INTENT(IN). Reference square domain, UNIT or
! BIUNIT, when this overload supports mapping.
!   xij : REAL(DFP), OPTIONAL, INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   alpha1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 1.
!   beta1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 1.
!   lambda1 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 1.
!   alpha2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter alpha for
! coordinate direction 2.
!   beta2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter beta for
! coordinate direction 2.
!   lambda2 : REAL(DFP), OPTIONAL, INTENT(IN). Family parameter lambda for
! coordinate direction 2.
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
! USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle
! USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: q(:,:),buffer(:,:)
! INTEGER(I4B) :: n(2),nr,nc
!
! n = QuadratureNumber_Quadrangle(2_I4B,2_I4B, &
!   GaussLegendreQP,GaussLegendreQP)
! ALLOCATE(buffer(3,PRODUCT(n)))
! q = QuadraturePoint_Quadrangle(order=2_I4B,quadType=GaussLegendreQP, &
!   refQuadrangle="BIUNIT")
! CALL QuadraturePoint_Quadrangle_(nipsx=[n(1)],nipsy=[n(2)], &
!   quadType1=GaussLegendreQP, &
!   quadType2=GaussLegendreQP,refQuadrangle="BIUNIT",ans=buffer, &
!   nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == PRODUCT(n), "Active quadrature dimensions")
! CALL OK(MAXVAL(ABS(q-buffer)) < 1.0E-11_DFP, "Quadrature forms agree")
! CALL OK(ABS(SUM(q(3,:))-(4.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate reference measure")
! CALL OK(ABS(SUM(q(3,:)*q(1,:)**2)-(4.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Weights integrate an analytic polynomial moment")
! END PROGRAM main
!```

INTERFACE QuadraturePoint_Quadrangle_
  MODULE SUBROUTINE QuadraturePoint_Quadrangle1_( &
    nipsx, nipsy, quadType1, quadType2, refQuadrangle, xij, alpha1, beta1, &
    lambda1, alpha2, beta2, lambda2, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! order of integrand in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! order of  integrand in y direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! interpolation point type in x direction
    !! Equidistance ! GaussLegendre ! GaussLegendreLobatto
    !! GaussLegendreRadauLeft ! GaussLegendreRadauRight ! GaussChebyshev1
    !! GaussChebyshev1Lobatto ! GaussChebyshev1RadauLeft
    !! GaussChebyshev1RadauRight ! GaussUltraspherical
    !! GaussUltrasphericalLobatto ! GaussUltrasphericalRadauLeft
    !! GaussUltrasphericalRadauRight ! GaussJacobi
    !! GaussJacobiLobatto ! GaussJacobiRadauLeft ! GaussJacobiRadauRight
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! Reference quadrangle ! UNIT ! BIUNIT
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation points in xij format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
  END SUBROUTINE QuadraturePoint_Quadrangle1_
END INTERFACE QuadraturePoint_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE QuadrangleInterpolationUtility
