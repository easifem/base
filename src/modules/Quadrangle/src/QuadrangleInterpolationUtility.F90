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
! summary: Interpolation and basis functions on the reference quadrangle.
!
! This module collects the routines that build polynomial spaces on a
! quadrangle.
! It covers four groups.
!
! 1. DOF counts, for Lagrange and hierarchical bases.
! 2. Lagrange nodes, polynomial degrees, coefficients and basis values.
! 3. Orthogonal, tensor product and hierarchical basis values and
!    gradients.
! 4. Quadrature points.
!
! A tensor product space of orders `p` and `q` has `(p + 1) * (q + 1)`
! modes.
! Basis values use one row per point and one column per basis function.
! Gradients add a third axis for the coordinate direction.
!
! @note
! The tensor product routines use `basisType1` for both directions.
! They ignore `basisType2`.
! @endnote
!
! Formula:
!   $$ u(x, y) = \sum_{i=0}^{p} \sum_{j=0}^{q} c_{ij} P_i(x) Q_j(y) $$

MODULE QuadrangleInterpolationUtility
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypeQuadratureOpt
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypePolynomialOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: stderr
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
! summary: Returns the number of hierarchical DOFs of a quadrangle.
!
! This function counts the vertex, edge or cell DOFs of a hierarchical
! quadrangle, or their total.
! The argument `opt` selects the kind of DOF.
!
! 1. `"V"` counts the vertex DOFs. The result is 4.
! 2. `"E"` counts the edge DOFs, without the vertices.
! 3. `"C"` counts the cell (interior) DOFs.
! 4. Any other value counts all DOFs.
!
! The edge orders `pe3` and `pe4` belong to the horizontal edges.
! The edge orders `qe1` and `qe2` belong to the vertical edges.
! The letters in `opt` can be upper or lower case.
!
! Returns:
!   ans : INTEGER(I4B) - the number of DOFs
!
! Formula:
!   $$ N_V = 4 $$
!   $$ N_E = q_{e1} + q_{e2} + p_{e3} + p_{e4} - 4 $$
!   $$ N_C = (p_b - 1)(q_b - 1) $$
!   $$ N = N_V + N_E + N_C $$

INTERFACE
  MODULE PURE FUNCTION GetHierarchicalDOF_Quadrangle( &
    pb, qb, pe3, pe4, qe1, qe2, opt) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb, qb
    !! cell order in the first (`pb`) and second (`qb`) direction
    INTEGER(I4B), INTENT(IN) :: qe1, qe2, pe3, pe4
    !! order of each edge: `qe1`, `qe2` vertical; `pe3`, `pe4` horizontal
    CHARACTER(1), INTENT(IN) :: opt
    !! what to count: `"V"` vertex, `"E"` edge, `"C"` cell;
    !! any other value gives the total
    INTEGER(I4B) :: ans
    !! number of DOFs
  END FUNCTION GetHierarchicalDOF_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalDOF_Quadrangle@DOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the total number of DOFs of a quadrangle.
!
! This function returns the number of degrees of freedom of a quadrangle
! with the same order in both directions.
! The count includes vertex, edge and interior DOFs.
!
! @note
! `baseContinuity` and `baseInterpolation` are not used.
! The result is the same for every value.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - the number of DOFs, `(order + 1)**2`
!
! Formula:
!   $$ N = (p + 1)^2 $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/GetTotalInDOF_Quadrangle_test_1.F90)
!
! See also: [[GetTotalInDOF_Quadrangle]]

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Quadrangle(order, baseContinuity, &
                                              baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, for example `"H1"`; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation family, for example `"Lagrange"`; not used
    INTEGER(I4B) :: ans
    !! total number of DOFs
  END FUNCTION GetTotalDOF_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                         LagrangeInDOF_Quadrangle@DOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior DOFs of a quadrangle.
!
! This function returns the number of interior degrees of freedom of a
! quadrangle with the same order in both directions.
! Vertex and edge DOFs are not counted.
!
! @note
! `baseContinuity` and `baseInterpolation` are not used.
! The result is the same for every value.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - the number of interior DOFs, `(order - 1)**2`
!
! Formula:
!   $$ N_{in} = (p - 1)^2 $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/GetTotalInDOF_Quadrangle_test_1.F90)
!
! See also: [[GetTotalDOF_Quadrangle]]

INTERFACE GetTotalInDOF_Quadrangle
  MODULE PURE FUNCTION GetTotalInDOF_Quadrangle1(order, baseContinuity, &
                                                 baseInterpolation) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, for example `"H1"`; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation family, for example `"Lagrange"`; not used
    INTEGER(I4B) :: ans
    !! number of interior DOFs
  END FUNCTION GetTotalInDOF_Quadrangle1
END INTERFACE GetTotalInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                         GetTotalInDOF_Quadrangle@DOFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior DOFs of a quadrangle.
!
! This function returns the number of interior degrees of freedom.
! The quadrangle has order `p` in the first direction and `q` in the
! second.
! Vertex and edge DOFs are not counted.
!
! @note
! `baseContinuity` and `baseInterpolation` are not used.
! The result is the same for every value.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - the number of interior DOFs, `(p - 1) * (q - 1)`
!
! Formula:
!   $$ N_{in} = (p - 1)(q - 1) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/GetTotalInDOF_Quadrangle_test_1.F90)
!
! See also: [[GetTotalDOF_Quadrangle]]

INTERFACE GetTotalInDOF_Quadrangle
  MODULE PURE FUNCTION GetTotalInDOF_Quadrangle2(p, q, baseContinuity, &
                                                 baseInterpolation) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    !! polynomial order in the first (`p`) and second (`q`) direction
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, for example `"H1"`; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! interpolation family, for example `"Lagrange"`; not used
    INTEGER(I4B) :: ans
    !! number of interior DOFs
  END FUNCTION GetTotalInDOF_Quadrangle2
END INTERFACE GetTotalInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                      LagrangeDOF_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the total number of Lagrange nodes of a quadrangle.
!
! This function returns the number of nodes of a quadrangle whose
! polynomial order is `order` in both directions.
! The count includes the vertices, the edge nodes and the interior nodes.
!
! Returns:
!   ans : INTEGER(I4B) - the number of nodes, `(order + 1)**2`
!
! Formula:
!   $$ N = (p + 1)^2 $$
!
! See also: [[GetTotalDOF_Quadrangle]]

INTERFACE LagrangeDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B) :: ans
    !! total number of nodes
  END FUNCTION LagrangeDOF_Quadrangle1
END INTERFACE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                      LagrangeDOF_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the total number of Lagrange nodes of a quadrangle,
!   with different orders in the two directions.
!
! This function returns the number of nodes of a quadrangle.
! The polynomial order is `p` in the `x` direction.
! It is `q` in the `y` direction.
! The count includes the vertices, the edge nodes and the interior nodes.
!
! Returns:
!   ans : INTEGER(I4B) - the number of nodes, `(p + 1) * (q + 1)`
!
! Formula:
!   $$ N = (p + 1)(q + 1) $$
!
! See also: [[LagrangeDOF_Quadrangle1]], [[LagrangeInDOF_Quadrangle2]]

INTERFACE LagrangeDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the `x` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the `y` direction
    INTEGER(I4B) :: ans
    !! total number of nodes
  END FUNCTION LagrangeDOF_Quadrangle2
END INTERFACE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeInDOF_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior Lagrange nodes of a quadrangle.
!
! This function returns the number of nodes inside the quadrangle, not
! on its edges or vertices.
! The polynomial order is `order` in both directions.
!
! @note
! The function evaluates the formula without checks. For `order` below
! 1, the result is not a valid node count.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - the number of interior nodes, `(order - 1)**2`
!
! Formula:
!   $$ N = (order - 1)^2 $$
!
! See also: [[LagrangeDOF_Quadrangle1]], [[GetTotalInDOF_Quadrangle]]

INTERFACE LagrangeInDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B) :: ans
    !! number of interior nodes
  END FUNCTION LagrangeInDOF_Quadrangle1
END INTERFACE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                    LagrangeInDOF_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior Lagrange nodes of a quadrangle,
!   with different orders in the two directions.
!
! This function returns the number of nodes inside the quadrangle, not
! on its edges or vertices.
! The polynomial order is `p` in the `x` direction and `q` in the `y`
! direction.
!
! @note
! The function evaluates the formula without checks. For `p` or `q`
! below 1, the result is not a valid node count.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - the number of interior nodes, `(p - 1) * (q - 1)`
!
! Formula:
!   $$ N = (p - 1)(q - 1) $$
!
! See also: [[LagrangeDOF_Quadrangle2]]

INTERFACE LagrangeInDOF_Quadrangle
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    !! `p` is the polynomial order in the `x` direction.
    !! `q` is the polynomial order in the `y` direction.
    INTEGER(I4B) :: ans
    !! number of interior nodes
  END FUNCTION LagrangeInDOF_Quadrangle2
END INTERFACE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                  LagrangeDegree_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial exponent table of a quadrangle.
!
! This function returns the exponents of the monomials `x**a * y**b`,
! with `a` and `b` from 0 to `order`.
! It allocates `ans` with `(order + 1)**2` rows and 2 columns.
!
! Row `(order + 1) * b + a + 1` holds the pair `[a, b]`.
! The `x` exponent changes fastest.
! Column 1 holds the `x` exponent. Column 2 holds the `y` exponent.
!
! Returns:
!   ans : INTEGER(I4B) - exponent table, allocated with shape
!   `[(order + 1)**2, 2]`
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeDegree_Quadrangle_test_1.F90)
!
! See also: [[LagrangeDegree_Quadrangle1_]]

INTERFACE LagrangeDegree_Quadrangle
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! exponent table, shape `[(order + 1)**2, 2]`
  END FUNCTION LagrangeDegree_Quadrangle1
END INTERFACE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeDegree_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-18
! summary: Fills the monomial exponent table of a quadrangle without
!   allocating.
!
! This routine writes the exponents of the monomials `x**a * y**b` into
! `ans`, with `a` and `b` from 0 to `order`.
! Row `(order + 1) * b + a + 1` holds the pair `[a, b]`.
! The `x` exponent changes fastest.
!
! Column 1 holds the `x` exponent. Column 2 holds the `y` exponent.
!
! @note
! `ans` must have at least `(order + 1)**2` rows and 2 columns.
! The routine does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeDegree_Quadrangle_test_1.F90)
!
! See also: [[LagrangeDegree_Quadrangle1]]

INTERFACE LagrangeDegree_Quadrangle_
  MODULE PURE SUBROUTINE LagrangeDegree_Quadrangle1_(order, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! exponent table, the routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of monomials, `(order + 1)**2`.
    !! `ncol` is always 2.
  END SUBROUTINE LagrangeDegree_Quadrangle1_
END INTERFACE LagrangeDegree_Quadrangle_

!----------------------------------------------------------------------------
!                                   LagrangeDegree_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial exponent table of a quadrangle, with
!   different orders in the two directions.
!
! This function returns the exponents of the monomials `x**a * y**b`,
! with `a` from 0 to `p` and `b` from 0 to `q`.
! It allocates `ans` with `(p + 1) * (q + 1)` rows and 2 columns.
!
! Row `(p + 1) * b + a + 1` holds the pair `[a, b]`.
! The `x` exponent changes fastest.
! Column 1 holds the `x` exponent. Column 2 holds the `y` exponent.
!
! Returns:
!   ans : INTEGER(I4B) - exponent table, allocated with shape
!   `[(p + 1) * (q + 1), 2]`
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeDegree_Quadrangle_test_1.F90)
!
! See also: [[LagrangeDegree_Quadrangle2_]]

INTERFACE LagrangeDegree_Quadrangle
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle2(p, q) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the `x` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the `y` direction
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! exponent table, shape `[(p + 1) * (q + 1), 2]`
  END FUNCTION LagrangeDegree_Quadrangle2
END INTERFACE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                 LagrangeDegree_Quadrangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills the monomial exponent table of a quadrangle, with
!   different orders in the two directions, without allocating.
!
! This routine writes the exponents of the monomials `x**a * y**b` into
! `ans`, with `a` from 0 to `p` and `b` from 0 to `q`.
! Row `(p + 1) * b + a + 1` holds the pair `[a, b]`.
! The `x` exponent changes fastest.
!
! Column 1 holds the `x` exponent. Column 2 holds the `y` exponent.
!
! @note
! `ans` must have at least `(p + 1) * (q + 1)` rows and 2 columns.
! The routine does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeDegree_Quadrangle_test_1.F90)
!
! See also: [[LagrangeDegree_Quadrangle2]]

INTERFACE LagrangeDegree_Quadrangle_
  MODULE PURE SUBROUTINE LagrangeDegree_Quadrangle2_(p, q, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the `x` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the `y` direction
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! exponent table, the routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of monomials, `(p + 1) * (q + 1)`.
    !! `ncol` is always 2.
  END SUBROUTINE LagrangeDegree_Quadrangle2_
END INTERFACE LagrangeDegree_Quadrangle_

!----------------------------------------------------------------------------
!                                    MonomialBasis_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the monomials of a quadrangle at a set of points.
!
! This routine evaluates the monomials `x**a * y**b` at the points `xij`,
! with `a` from 0 to `p` and `b` from 0 to `q`.
! It writes the values into `ans`, one row per point and one column per
! monomial.
!
! The columns follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
! Column `(p + 1) * b + a + 1` holds `x**a * y**b`.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and `(p + 1) * (q + 1)`
! columns. The routine does not check this.
! @endnote
!
! Formula:
!   $$ ans_{k,\, (p+1) b + a + 1} = x_k^{a}\, y_k^{b} $$

INTERFACE
  MODULE PURE SUBROUTINE MonomialBasis_Quadrangle_( &
    p, q, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the `x` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the `y` direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one point per column, `SIZE(xij, 1) = 2`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the monomials, the routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows, equal to `SIZE(xij, 2)`.
    !! `ncol` is the number of columns, equal to `(p + 1) * (q + 1)`.
  END SUBROUTINE MonomialBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial coefficients of one Lagrange polynomial
!   of a quadrangle.
!
! This function returns the coefficients of the `i`-th Lagrange
! polynomial of degree `order` in both directions.
!
! The polynomial uses the monomials `x**a * y**b`.
! The monomials follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
!
! The coefficients make the polynomial equal to 1 at point `i` and
! equal to 0 at the other points of `xij`.
! `xij` must hold the `(order + 1)**2` interpolation points, one per
! column, in the node order of the element.
!
! @note
! `i` must lie between 1 and `SIZE(xij, 2)`. The function does not
! check this.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - coefficients, of size `SIZE(xij, 2)`
!
! Formula:
!   $$ V c_i = e_i, \quad
!   \ell_i(x, y) = \sum_k c_{k,i}\, x^{a_k} y^{b_k} $$
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Quadrangle1_]]

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(xij, 2)`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, one point per column, `SIZE(xij, 1) = 2`
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle1
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the monomial coefficients of one Lagrange polynomial
!   of a quadrangle, without allocating the result.
!
! This routine computes the coefficients of the `i`-th Lagrange
! polynomial of degree `order` in both directions.
! It writes them into the first `tsize` entries of `ans`.
!
! The polynomial uses the monomials `x**a * y**b`.
! The monomials follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
!
! The routine does the following:
!
! 1. It builds the Vandermonde matrix of the monomials at the points `xij`.
! 2. It solves the linear system for the right-hand side that is 1 at
!    position `i` and 0 elsewhere.
!
! `xij` must hold the `(order + 1)**2` interpolation points, one per
! column, in the node order of the element.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` entries. `i` must lie between
! 1 and `SIZE(xij, 2)`. The routine does not check either condition.
! @endnote
!
! Formula:
!   $$ V c_i = e_i, \quad
!   \ell_i(x, y) = \sum_k c_{k,i}\, x^{a_k} y^{b_k} $$
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Quadrangle1]]

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle1_(order, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(xij, 2)`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, one point per column, `SIZE(xij, 1) = 2`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine fills `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coefficients, equal to `SIZE(xij, 2)`
  END SUBROUTINE LagrangeCoeff_Quadrangle1_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial coefficients of one Lagrange polynomial
!   of a quadrangle, from a Vandermonde matrix.
!
! This function returns the coefficients of the `i`-th Lagrange
! polynomial of a quadrangle.
! You pass the Vandermonde matrix `v` of the monomials at the
! interpolation points.
!
! The Vandermonde matrix `V` has the value of the `k`-th monomial at
! the `j`-th point in entry `(j, k)`.
! The monomials follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
!
! The coefficients make the polynomial equal to 1 at point `i` and
! equal to 0 at the other points.
! The function solves the linear system `V c = e_i` and leaves `v`
! unchanged.
!
! @note
! `isVandermonde` only selects this overload. The function ignores its
! value and always treats `v` as a Vandermonde matrix.
! It ignores `order` too.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - coefficients, of size `SIZE(v, 1)`
!
! Formula:
!   $$ V c_i = e_i $$
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Quadrangle2_]], [[LagrangeCoeff_Quadrangle3]]

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle2(order, i, v, isVandermonde) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix, square, of size `(order + 1)**2`
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! only selects this overload, its value is not used
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle2
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the monomial coefficients of one Lagrange polynomial
!   of a quadrangle from a Vandermonde matrix, without allocating.
!
! This routine computes the coefficients of the `i`-th Lagrange
! polynomial of a quadrangle.
! You pass the Vandermonde matrix `v` of the monomials at the
! interpolation points.
! The routine writes the coefficients into the first `tsize` entries
! of `ans`.
!
! The Vandermonde matrix `V` has the value of the `k`-th monomial at
! the `j`-th point in entry `(j, k)`.
! The monomials follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
!
! The routine solves the linear system `V c = e_i` and leaves `v`
! unchanged.
!
! @note
! `ans` must have at least `SIZE(v, 1)` entries. The routine does not
! check this.
! @endnote
!
! @note
! `isVandermonde` only selects this overload. The routine ignores its
! value and always treats `v` as a Vandermonde matrix.
! It ignores `order` too.
! @endnote
!
! Formula:
!   $$ V c_i = e_i $$
!
! See also: [[LagrangeCoeff_Quadrangle2]], [[LagrangeCoeff_Quadrangle3_]]

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle2_(order, i, v, isVandermonde, &
                                               ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix, square, of size `(order + 1)**2`
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! only selects this overload, its value is not used
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine fills `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coefficients, equal to `SIZE(v, 1)`
  END SUBROUTINE LagrangeCoeff_Quadrangle2_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial coefficients of one Lagrange polynomial
!   of a quadrangle, from LU factors.
!
! This function returns the coefficients of the `i`-th Lagrange
! polynomial of a quadrangle.
! You pass the LU factors `v` of the Vandermonde matrix of the
! monomials at the interpolation points.
! The pivot indices `ipiv` come from the same LU factorization.
!
! The monomials follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
!
! The coefficients make the polynomial equal to 1 at point `i` and
! equal to 0 at the other points.
! The function solves the linear system `V c = e_i` with the factors.
!
! @note
! The function ignores `order`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - coefficients, of size `SIZE(v, 1)`
!
! Formula:
!   $$ V c_i = e_i $$
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Quadrangle2]], [[LagrangeCoeff_Quadrangle3_]]

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices of the LU factorization
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle3
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the monomial coefficients of one Lagrange polynomial
!   of a quadrangle from LU factors, without allocating.
!
! This routine computes the coefficients of the `i`-th Lagrange
! polynomial of a quadrangle.
! You pass the LU factors `v` of the Vandermonde matrix of the
! monomials at the interpolation points.
! The pivot indices `ipiv` come from the same LU factorization.
! The routine writes the coefficients into the first `tsize` entries
! of `ans`.
!
! The monomials follow the order of [[LagrangeDegree_Quadrangle]]:
! the `x` exponent changes fastest.
!
! The routine solves the linear system `V c = e_i` with the factors.
!
! @note
! `ans` must have at least `SIZE(v, 1)` entries. The routine does not
! check this. It ignores `order`.
! @endnote
!
! Formula:
!   $$ V c_i = e_i $$
!
! See also: [[LagrangeCoeff_Quadrangle3]], [[LagrangeCoeff_Quadrangle2_]]

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle3_(order, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices of the LU factorization
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine fills `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coefficients, equal to `SIZE(v, 1)`
  END SUBROUTINE LagrangeCoeff_Quadrangle3_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of all Lagrange polynomials of a
!   quadrangle, in a chosen tensor product basis.
!
! This function returns the coefficients of all Lagrange polynomials
! of a quadrangle.
! Column `j` of the result holds the coefficients of the `j`-th
! Lagrange polynomial.
! The coefficients refer to the tensor product basis selected by
! `basisType`. The default is the monomial basis.
!
! The polynomial order is `order` in both directions.
! `xij` must hold the `(order + 1)**2` interpolation points, one per
! column.
! The basis matrix `V` holds the value of each basis function at each
! point. The coefficients are its inverse.
!
! Returns:
!   ans : REAL(DFP) - coefficient matrix, of shape
!   `[SIZE(xij, 2), SIZE(xij, 2)]`
!
! Formula:
!   $$ C = V^{-1}, \quad \ell_j(x) = \sum_k C_{kj}\, m_k(x) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle_test_1.F90)
!
! See also: [[LagrangeCoeff_Quadrangle4_]], [[LagrangeCoeff_Quadrangle5]]

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle4(order, xij, basisType, alpha, &
                                            beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, one point per column
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, a constant of `TypePolynomialOpt`.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Jacobi, Legendre, Chebyshev, Ultraspherical
    !! and hierarchical.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed when `basisType` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed when `basisType` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter, needed when `basisType` is Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients, column `j` belongs to the `j`-th Lagrange polynomial
  END FUNCTION LagrangeCoeff_Quadrangle4
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the coefficients of all Lagrange polynomials of a
!   quadrangle, in a chosen tensor product basis, without allocating.
!
! This routine computes the coefficients of all Lagrange polynomials
! of a quadrangle.
! It writes them into `ans(1:nrow, 1:ncol)`.
! Column `j` holds the coefficients of the `j`-th Lagrange polynomial.
! The coefficients refer to the tensor product basis selected by
! `basisType`. The default is the monomial basis.
!
! The polynomial order is `order` in both directions.
! `xij` must hold the `(order + 1)**2` interpolation points, one per
! column.
! The basis matrix `V` holds the value of each basis function at each
! point. The coefficients are its inverse.
!
! @note
! `ans` must be at least as large as the coefficient matrix.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ C = V^{-1}, \quad \ell_j(x) = \sum_k C_{kj}\, m_k(x) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle__test_1.F90)
!
! See also: [[LagrangeCoeff_Quadrangle4]], [[LagrangeCoeff_Quadrangle5_]]

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle4_(order, xij, basisType, &
                                               alpha, beta, lambda, ans, &
                                               nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, one point per column
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, a constant of `TypePolynomialOpt`.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Jacobi, Legendre, Chebyshev, Ultraspherical
    !! and hierarchical.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed when `basisType` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed when `basisType` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter, needed when `basisType` is Ultraspherical
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, the routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! size of the coefficient matrix, both equal `(order + 1)**2`
  END SUBROUTINE LagrangeCoeff_Quadrangle4_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                    LagrangeCoeff_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of all Lagrange polynomials of a
!   quadrangle, with a different order and family in each direction.
!
! This function returns the coefficients of all Lagrange polynomials
! of a quadrangle.
! Column `j` of the result holds the coefficients of the `j`-th
! Lagrange polynomial.
! The polynomial order is `p` in the `x` direction and `q` in the `y`
! direction.
! `xij` must hold the `(p + 1) * (q + 1)` interpolation points, one per
! column.
!
! `basisType1` selects the polynomial family in the `x` direction.
! `basisType2` selects the family in the `y` direction.
! If both are hierarchical, the function uses the hierarchical basis.
! Otherwise it uses the tensor product of the two families.
!
! The basis matrix `V` holds the value of each basis function at each
! point. The coefficients are its inverse.
!
! Returns:
!   ans : REAL(DFP) - coefficient matrix, of shape
!   `[SIZE(xij, 2), SIZE(xij, 2)]`
!
! Formula:
!   $$ C = V^{-1}, \quad \ell_j(x) = \sum_k C_{kj}\, m_k(x) $$
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/LagrangeCoeff_Quadrangle_test_2.F90)
!   with `p = 2` and `q = 1`
!
! See also: [[LagrangeCoeff_Quadrangle4]], [[LagrangeCoeff_Quadrangle5_]]

INTERFACE LagrangeCoeff_Quadrangle
  MODULE FUNCTION LagrangeCoeff_Quadrangle5(p, q, xij, basisType1, &
                                            basisType2, alpha1, beta1, &
                                            lambda1, alpha2, beta2, lambda2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the `x` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the `y` direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, one point per column
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! polynomial family in the `x` direction, a constant of
    !! `TypePolynomialOpt`, such as `TypePolynomialOpt%monomial`
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! polynomial family in the `y` direction, a constant of
    !! `TypePolynomialOpt`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter in the `x` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter in the `x` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! Ultraspherical parameter in the `x` direction,
    !! needed for Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter in the `y` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter in the `y` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! Ultraspherical parameter in the `y` direction,
    !! needed for Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients, column `j` belongs to the `j`-th Lagrange polynomial
  END FUNCTION LagrangeCoeff_Quadrangle5
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                   LagrangeCoeff_Quadrangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the coefficients of all Lagrange polynomials of a
!   quadrangle, with a different order and family in each direction,
!   without allocating.
!
! This routine computes the coefficients of all Lagrange polynomials
! of a quadrangle.
! It writes them into `ans(1:nrow, 1:ncol)`.
! Column `j` holds the coefficients of the `j`-th Lagrange polynomial.
! The polynomial order is `p` in the `x` direction and `q` in the `y`
! direction.
! `xij` must hold the `(p + 1) * (q + 1)` interpolation points, one per
! column.
!
! `basisType1` selects the polynomial family in the `x` direction.
! `basisType2` selects the family in the `y` direction.
! If both are hierarchical, the routine uses the hierarchical basis.
! Otherwise it uses the tensor product of the two families.
!
! The basis matrix `V` holds the value of each basis function at each
! point. The coefficients are its inverse.
!
! @note
! `ans` must be at least as large as the coefficient matrix.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ C = V^{-1}, \quad \ell_j(x) = \sum_k C_{kj}\, m_k(x) $$
!
! See also: [[LagrangeCoeff_Quadrangle5]], [[LagrangeCoeff_Quadrangle4_]]

INTERFACE LagrangeCoeff_Quadrangle_
  MODULE SUBROUTINE LagrangeCoeff_Quadrangle5_(p, q, xij, basisType1, &
                                               basisType2, alpha1, beta1, &
                                               lambda1, alpha2, &
                                               beta2, lambda2, &
                                               ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the `x` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the `y` direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, one point per column
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! polynomial family in the `x` direction, a constant of
    !! `TypePolynomialOpt`, such as `TypePolynomialOpt%monomial`
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! polynomial family in the `y` direction, a constant of
    !! `TypePolynomialOpt`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter in the `x` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter in the `x` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! Ultraspherical parameter in the `x` direction,
    !! needed for Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter in the `y` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter in the `y` direction, needed for Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! Ultraspherical parameter in the `y` direction,
    !! needed for Ultraspherical
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, the routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! size of the coefficient matrix, both equal `(p + 1) * (q + 1)`
  END SUBROUTINE LagrangeCoeff_Quadrangle5_
END INTERFACE LagrangeCoeff_Quadrangle_

!----------------------------------------------------------------------------
!                                  LagrangeEvalAll_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates all Lagrange polynomials of a quadrangle at one point.
!
! This function evaluates all Lagrange polynomials of a quadrangle
! at the point `x`.
! The result holds the value of each polynomial at `x`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
! The `j`-th polynomial is 1 at the `j`-th point and 0 at the others.
!
! The function needs the coefficients of the polynomials.
! If `coeff` is absent, it computes them on every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them
! and stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reuses `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! The values do not depend on `basisType`. The family only changes how
! the function builds the coefficients.
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The function does not check this.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - values of the polynomials at `x`, of size
!   `SIZE(xij, 2)`
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/LagrangeEvalAll_Quadrangle_test_2.F90)
!
! See also: [[LagrangeEvalAll_Quadrangle1_]], [[LagrangeCoeff_Quadrangle4]]

INTERFACE LagrangeEvalAll_Quadrangle
  MODULE FUNCTION LagrangeEvalAll_Quadrangle1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(2)
    !! evaluation point, `x(1)` is the `x` coordinate and `x(2)` is
    !! the `y` coordinate
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one point per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! optional coefficients of the Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! used only when `coeff` is present.
    !! If `.TRUE.`, the function computes `coeff` and returns it.
    !! If `.FALSE.`, the function reuses the `coeff` you pass.
    !! Default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! values of the Lagrange polynomials at `x`
  END FUNCTION LagrangeEvalAll_Quadrangle1
END INTERFACE LagrangeEvalAll_Quadrangle

!----------------------------------------------------------------------------
!                                  LagrangeEvalAll_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates all Lagrange polynomials of a quadrangle at one point,
!   without allocating.
!
! This routine evaluates all Lagrange polynomials of a quadrangle
! at the point `x`.
! It writes the values into the first `tsize` entries of `ans`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
! The `j`-th polynomial is 1 at the `j`-th point and 0 at the others.
!
! The routine needs the coefficients of the polynomials.
! If `coeff` is absent, it computes them on every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them
! and stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reuses `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! The values do not depend on `basisType`. The family only changes how
! the routine builds the coefficients.
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The routine does not check this.
! @endnote
!
! @note
! `ans` must have at least `SIZE(xij, 2)` entries.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! See also: [[LagrangeEvalAll_Quadrangle1]], [[LagrangeCoeff_Quadrangle4_]]

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle1_( &
    order, x, xij, ans, tsize, coeff, firstCall, basisType, alpha, beta, &
    lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(2)
    !! evaluation point, `x(1)` is the `x` coordinate and `x(2)` is
    !! the `y` coordinate
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one point per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! values of the Lagrange polynomials at `x`,
    !! the routine fills `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values, equal to `SIZE(xij, 2)`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! optional coefficients of the Lagrange polynomials,
    !! shape `[SIZE(xij, 2), SIZE(xij, 2)]`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! used only when `coeff` is present.
    !! If `.TRUE.`, the routine computes `coeff` and returns it.
    !! If `.FALSE.`, the routine reuses the `coeff` you pass.
    !! Default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
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
! summary: Evaluates all Lagrange polynomials of a quadrangle at many
!   points.
!
! This function evaluates all Lagrange polynomials of a quadrangle at
! each column of `x`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
! The `j`-th polynomial is 1 at the `j`-th point and 0 at the others.
!
! The function needs the coefficients of the polynomials.
! If `coeff` is absent, it computes them on every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them
! and stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reuses `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! The values do not depend on `basisType`. The family only changes how
! the function builds the coefficients.
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The function does not check this.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - values of the polynomials, of shape
!   `[SIZE(x, 2), SIZE(xij, 2)]`. Entry `(i, j)` is the `j`-th
!   polynomial at the `i`-th point of `x`.
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeEvalAll_Quadrangle_test_1.F90)
!
! See also: [[LagrangeEvalAll_Quadrangle1]],
! [[LagrangeEvalAll_Quadrangle2_]]

INTERFACE LagrangeEvalAll_Quadrangle
  MODULE FUNCTION LagrangeEvalAll_Quadrangle2( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one per column.
    !! `x(1, :)` holds the x coordinates and `x(2, :)` the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! optional coefficients of the Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! used only when `coeff` is present.
    !! If `.TRUE.`, the function computes `coeff` and returns it.
    !! If `.FALSE.`, the function reuses the `coeff` you pass.
    !! Default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! values of the Lagrange polynomials at the points `x`
  END FUNCTION LagrangeEvalAll_Quadrangle2
END INTERFACE LagrangeEvalAll_Quadrangle

!----------------------------------------------------------------------------
!                                 LagrangeEvalAll_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates all Lagrange polynomials of a quadrangle at many
!   points, without allocating.
!
! This routine evaluates all Lagrange polynomials of a quadrangle at
! each column of `x`.
! It writes the values into `ans(1:nrow, 1:ncol)`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
! The `j`-th polynomial is 1 at the `j`-th point and 0 at the others.
!
! The routine needs the coefficients of the polynomials.
! If `coeff` is absent, it computes them on every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them
! and stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reuses `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! The values do not depend on `basisType`. The family only changes how
! the routine builds the coefficients.
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The routine does not check this.
! @endnote
!
! @note
! `ans` must have at least `SIZE(x, 2)` rows and `SIZE(xij, 2)` columns.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeEvalAll_Quadrangle__test_1.F90)
!
! See also: [[LagrangeEvalAll_Quadrangle2]],
! [[LagrangeEvalAll_Quadrangle3_]]

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle2_( &
    order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one per column.
    !! `x(1, :)` holds the x coordinates and `x(2, :)` the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the Lagrange polynomials.
    !! The routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`.
    !! `nrow = SIZE(x, 2)` and `ncol = SIZE(xij, 2)`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! optional coefficients of the Lagrange polynomials,
    !! shape `[SIZE(xij, 2), SIZE(xij, 2)]`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! used only when `coeff` is present.
    !! If `.TRUE.`, the routine computes `coeff` and returns it.
    !! If `.FALSE.`, the routine reuses the `coeff` you pass.
    !! Default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
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
! summary: Evaluates all Lagrange polynomials of a quadrangle at many
!   points, with caller-provided work arrays.
!
! This routine evaluates all Lagrange polynomials of a quadrangle at
! each column of `x`.
! It writes the values into `ans(1:nrow, 1:ncol)`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
! The `j`-th polynomial is 1 at the `j`-th point and 0 at the others.
!
! The caller supplies the arrays `coeff` and `xx`.
! `coeff` holds the coefficients of the polynomials.
! `xx` is a work array for the basis values at `x`.
! If `firstCall` is `.TRUE.`, the routine computes `coeff` from `xij`.
! If `firstCall` is `.FALSE.`, it reuses the `coeff` you pass.
!
! The values do not depend on `basisType`. The family only changes how
! the routine builds the coefficients.
!
! @note
! `firstCall` has no default. You must always pass it.
! @endnote
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! See also: [[LagrangeEvalAll_Quadrangle2_]]

INTERFACE LagrangeEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeEvalAll_Quadrangle3_( &
    order, x, xij, ans, nrow, ncol, coeff, xx, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one per column.
    !! `x(1, :)` holds the x coordinates and `x(2, :)` the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the Lagrange polynomials.
    !! The routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`.
    !! `nrow = SIZE(x, 2)` is the number of evaluation points.
    !! `ncol = SIZE(xij, 2)` is the number of polynomials
    REAL(DFP), INTENT(INOUT) :: coeff(:, :), xx(:, :)
    !! `coeff` holds the coefficients of the polynomials,
    !! shape `[ncol, ncol]`.
    !! `xx` is a work array of shape `[nrow, ncol]`
    LOGICAL(LGT) :: firstCall
    !! If `.TRUE.`, the routine computes `coeff` and returns it.
    !! If `.FALSE.`, the routine reuses the `coeff` you pass.
    !! You must always pass it.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
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
! summary: Evaluates the gradients of all Lagrange polynomials of a
!   quadrangle at many points.
!
! This function evaluates the gradient of each Lagrange polynomial of a
! quadrangle at each column of `x`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
!
! The function needs the coefficients of the polynomials.
! If `coeff` is absent, it computes them on every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them
! and stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reuses `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The function does not check this.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - gradients, of shape
!   `[SIZE(x, 2), SIZE(xij, 2), 2]`. Entry `(i, j, k)` is the derivative
!   of the `j`-th polynomial with respect to coordinate `k`, at the
!   `i`-th point of `x`.
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeGradientEvalAll_Quadrangle_test_1.F90)
!
! See also: [[LagrangeGradientEvalAll_Quadrangle1_]]

INTERFACE LagrangeGradientEvalAll_Quadrangle
  MODULE FUNCTION LagrangeGradientEvalAll_Quadrangle1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one per column.
    !! `x(1, :)` holds the x coordinates and `x(2, :)` the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! optional coefficients of the Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! used only when `coeff` is present.
    !! If `.TRUE.`, the function computes `coeff` and returns it.
    !! If `.FALSE.`, the function reuses the `coeff` you pass.
    !! Default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 2)
    !! gradients of the Lagrange polynomials.
    !! The first index is the evaluation point.
    !! The second index is the polynomial.
    !! The third index is the coordinate of the derivative
  END FUNCTION LagrangeGradientEvalAll_Quadrangle1
END INTERFACE LagrangeGradientEvalAll_Quadrangle

!----------------------------------------------------------------------------
!                         LagrangeGradientEvalAll_Quadrangle@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of all Lagrange polynomials of a
!   quadrangle at many points, without allocating.
!
! This routine evaluates the gradient of each Lagrange polynomial of a
! quadrangle at each column of `x`.
! It writes the values into `ans(1:dim1, 1:dim2, 1:dim3)`.
! The polynomial order is `order` in both directions.
! `xij` holds the `(order + 1)**2` interpolation points, one per column.
!
! The routine needs the coefficients of the polynomials.
! If `coeff` is absent, it computes them on every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them
! and stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reuses `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! @note
! Reuse `coeff` only with the same `xij`, `order` and `basisType`.
! The routine does not check this.
! @endnote
!
! @note
! `ans` must be large enough for `dim1 = SIZE(x, 2)`,
! `dim2 = SIZE(xij, 2)` and `dim3 = 2`.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/LagrangeGradientEvalAll_Quadrangle__test_1.F90)
!
! See also: [[LagrangeGradientEvalAll_Quadrangle1]]

INTERFACE LagrangeGradientEvalAll_Quadrangle_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Quadrangle1_( &
    order, x, xij, ans, dim1, dim2, dim3, coeff, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one per column.
    !! `x(1, :)` holds the x coordinates and `x(2, :)` the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points, one per column,
    !! `SIZE(xij, 2) = (order + 1)**2`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the Lagrange polynomials.
    !! The routine fills `ans(1:dim1, 1:dim2, 1:dim3)`.
    !! The first index is the evaluation point.
    !! The second index is the polynomial.
    !! The third index is the coordinate of the derivative
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! sizes written in `ans`.
    !! `dim1 = SIZE(x, 2)`, `dim2 = SIZE(xij, 2)` and `dim3 = 2`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! optional coefficients of the Lagrange polynomials,
    !! shape `[SIZE(xij, 2), SIZE(xij, 2)]`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! used only when `coeff` is present.
    !! If `.TRUE.`, the routine computes `coeff` and returns it.
    !! If `.FALSE.`, the routine reuses the `coeff` you pass.
    !! Default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family used to build the coefficients.
    !! Default is `TypePolynomialOpt%monomial`.
    !! Other families are Legendre, Lobatto, Chebyshev, Jacobi,
    !! Ultraspherical and hierarchical.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeGradientEvalAll_Quadrangle1_
END INTERFACE LagrangeGradientEvalAll_Quadrangle_

!----------------------------------------------------------------------------
!                                            RefElemDomain_Quadrangle@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the name of the reference quadrangle domain.
!
! This function returns the name of the reference domain of the basis
! functions.
! For a quadrangle, the answer is always `"BIUNIT"`.
! This is the square $[-1, 1]^2$.
! The function ignores both arguments.
!
! Returns:
!   ans : TYPE(String) - the string `"BIUNIT"`

INTERFACE
  MODULE FUNCTION RefElemDomain_Quadrangle(baseContinuity, baseInterpol) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis functions, such as `"H1"`.
    !! The function ignores it
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! family of the basis functions, such as `"Lagrange"`.
    !! The function ignores it
    TYPE(String) :: ans
    !! name of the reference domain, always `"BIUNIT"`
  END FUNCTION RefElemDomain_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                        FacetConnectivity_Quadrangle@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the vertex pairs of the four edges of a quadrangle.
!
! This function returns the local vertex numbers of the end points of
! each edge.
! The edges run in counterclockwise order.
! Edge 1 is `[1, 2]`, edge 2 is `[2, 3]`, edge 3 is `[3, 4]` and
! edge 4 is `[4, 1]`.
! The function ignores both optional arguments.
!
! @note
! The edge order of the reference quadrangle with edge option 1 is
! different. Use one edge order consistently.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - array of shape `[2, 4]`. Column `i` holds the
!   two vertices of edge `i`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/FacetConnectivity_Quadrangle_test_1.F90)

INTERFACE
  MODULE FUNCTION FacetConnectivity_Quadrangle(baseInterpol, baseContinuity) &
    RESULT(ans)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpol
    !! family of the basis functions. The function ignores it
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! continuity of the basis functions. The function ignores it
    INTEGER(I4B) :: ans(2, 4)
    !! the two end vertices of each edge.
    !! Column `i` belongs to edge `i`
  END FUNCTION FacetConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                      EquidistancePoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns equally spaced points of a quadrangle of order `order`.
!
! This function returns the equally spaced interpolation points of a
! quadrangle.
! The order is `order` in both directions.
! The result has `(order + 1)**2` columns, one per point.
!
! The default domain is the square $[-1, 1]^2$.
! The points are in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! If `xij` is present, the function maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then the result has `SIZE(xij, 1)` rows.
! Otherwise the result has 2 rows.
!
! Returns:
!   ans : REAL(DFP) - allocatable array of shape `[nrow, ncol]`, with
!   `ncol = (order + 1)**2`
!
! Formula:
!   $$ x_i = -1 + 2i/p, \quad y_j = -1 + 2j/q $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/EquidistancePoint_Quadrangle_test_1.F90)
!
! See also: [[EquidistancePoint_Quadrangle2]],
! [[EquidistancePoint_Quadrangle1_]]

INTERFACE EquidistancePoint_Quadrangle
  MODULE RECURSIVE FUNCTION EquidistancePoint_Quadrangle1(order, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The function allocates it
  END FUNCTION EquidistancePoint_Quadrangle1
END INTERFACE EquidistancePoint_Quadrangle

!----------------------------------------------------------------------------
!                     EquidistancePoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes equally spaced points of a quadrangle of order
!   `order`, without allocating.
!
! This routine computes the equally spaced interpolation points of a
! quadrangle.
! The order is `order` in both directions.
! It writes the points into `ans(1:nrow, 1:ncol)`.
!
! The default domain is the square $[-1, 1]^2$.
! The points are in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! If `xij` is present, the routine maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then `nrow` equals `SIZE(xij, 1)`.
! Otherwise `nrow` is 2.
! `ncol` is `(order + 1)**2`.
!
! @note
! `ans` must have at least `(order + 1)**2` columns.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ x_i = -1 + 2i/p, \quad y_j = -1 + 2j/q $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/EquidistancePoint_Quadrangle__test_1.F90)
!
! See also: [[EquidistancePoint_Quadrangle1]],
! [[EquidistancePoint_Quadrangle2_]]

INTERFACE EquidistancePoint_Quadrangle_
  MODULE RECURSIVE SUBROUTINE EquidistancePoint_Quadrangle1_(order, ans, &
                                                             nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns.
    !! Default is the square $[-1, 1]^2$
  END SUBROUTINE EquidistancePoint_Quadrangle1_
END INTERFACE EquidistancePoint_Quadrangle_

!----------------------------------------------------------------------------
!                      EquidistancePoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns equally spaced points of a quadrangle with order `p`
!   in x and `q` in y.
!
! This function returns the equally spaced interpolation points of a
! quadrangle.
! The order is `p` in the x direction and `q` in the y direction.
! The result has `(p + 1) * (q + 1)` columns, one per point.
!
! The default domain is the square $[-1, 1]^2$.
! The points are in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! If `xij` is present, the function maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then the result has `SIZE(xij, 1)` rows.
! Otherwise the result has 2 rows.
!
! Returns:
!   ans : REAL(DFP) - allocatable array of shape `[nrow, ncol]`, with
!   `ncol = (p + 1) * (q + 1)`
!
! Formula:
!   $$ x_i = -1 + 2i/p, \quad y_j = -1 + 2j/q $$
!
! See also: [[EquidistancePoint_Quadrangle1]],
! [[EquidistancePoint_Quadrangle2_]]

INTERFACE EquidistancePoint_Quadrangle
  MODULE RECURSIVE FUNCTION EquidistancePoint_Quadrangle2(p, q, &
                                                          xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns and 2 or 3 rows.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The function allocates it
  END FUNCTION EquidistancePoint_Quadrangle2
END INTERFACE EquidistancePoint_Quadrangle

!----------------------------------------------------------------------------
!                     EquidistancePoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes equally spaced points of a quadrangle with order `p`
!   in x and `q` in y, without allocating.
!
! This routine computes the equally spaced interpolation points of a
! quadrangle.
! The order is `p` in the x direction and `q` in the y direction.
! It writes the points into `ans(1:nrow, 1:ncol)`.
!
! The default domain is the square $[-1, 1]^2$.
! The points are in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! If `xij` is present, the routine maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then `nrow` equals `SIZE(xij, 1)`.
! Otherwise `nrow` is 2.
! `ncol` is `(p + 1) * (q + 1)`.
!
! @note
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endnote
!
! Formula:
!   $$ x_i = -1 + 2i/p, \quad y_j = -1 + 2j/q $$
!
! See also: [[EquidistancePoint_Quadrangle2]],
! [[EquidistancePoint_Quadrangle1_]]

INTERFACE EquidistancePoint_Quadrangle_
  MODULE RECURSIVE SUBROUTINE EquidistancePoint_Quadrangle2_(p, q, ans, &
                                                             nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns and 2 or 3 rows.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
  END SUBROUTINE EquidistancePoint_Quadrangle2_
END INTERFACE EquidistancePoint_Quadrangle_

!----------------------------------------------------------------------------
!                    EquidistanceInPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the interior equally spaced points of a quadrangle of
!   order `order`.
!
! This function returns the equally spaced points that lie inside the
! quadrangle.
! It leaves out the vertices and the edge points.
! The order is `order` in both directions.
!
! The default domain is the square $[-1, 1]^2$.
! If `xij` is present, the function maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then the result has `SIZE(xij, 1)` rows.
! Otherwise the result has 2 rows.
!
! If the quadrangle has no interior point, the result has shape `[0, 0]`.
! This happens for `order` below 2.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/EquidistanceInPoint_Quadrangle_test_1.F90)
!
! See also: [[EquidistanceInPoint_Quadrangle2]],
! [[EquidistancePoint_Quadrangle1]]

INTERFACE EquidistanceInPoint_Quadrangle
  MODULE FUNCTION EquidistanceInPoint_Quadrangle1(order, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns and 2 or 3 rows.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! coordinates of the interior points, one point per column.
    !! The function allocates it
  END FUNCTION EquidistanceInPoint_Quadrangle1
END INTERFACE EquidistanceInPoint_Quadrangle

!----------------------------------------------------------------------------
!                    EquidistanceInPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the interior equally spaced points of a quadrangle with
!   order `p` in x and `q` in y.
!
! This function returns the equally spaced points that lie inside the
! quadrangle.
! It leaves out the vertices and the edge points.
! The order is `p` in the x direction and `q` in the y direction.
!
! The default domain is the square $[-1, 1]^2$.
! If `xij` is present, the function maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then the result has `SIZE(xij, 1)` rows.
! Otherwise the result has 2 rows.
!
! If the quadrangle has no interior point, the result has shape `[0, 0]`.
!
! See also: [[EquidistanceInPoint_Quadrangle1]],
! [[EquidistancePoint_Quadrangle2]]

INTERFACE EquidistanceInPoint_Quadrangle
  MODULE FUNCTION EquidistanceInPoint_Quadrangle2(p, q, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns and 2 or 3 rows.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! coordinates of the interior points, one point per column.
    !! The function allocates it
  END FUNCTION EquidistanceInPoint_Quadrangle2
END INTERFACE EquidistanceInPoint_Quadrangle

!----------------------------------------------------------------------------
!                    InterpolationPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the interpolation points of a quadrangle of order
!   `order`.
!
! This function returns the interpolation points of a quadrangle.
! The order is `order` in both directions.
! The result has `(order + 1)**2` columns, one per point.
!
! The point family `ipType` is the same in both directions.
! The points are the tensor product of the one-dimensional points.
! The default domain is the square $[-1, 1]^2$.
! If `xij` is present, the function maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then the result has `SIZE(xij, 1)` rows.
! Otherwise the result has 2 rows.
!
! If `layout` starts with `"VEFC"`, the function orders the points as
! vertices, then edge points, then interior points.
! For any other layout, such as `"INCREASING"`, the y index runs fastest.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/InterpolationPoint_Quadrangle_test_1.F90)
!
! See also: [[InterpolationPoint_Quadrangle2]],
! [[InterpolationPoint_Quadrangle1_]]

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle1( &
    order, ipType, layout, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point family, for example `Equidistance`
    CHARACTER(*), INTENT(IN) :: layout
    !! point ordering, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, used by the Jacobi-type point families
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, used by the Jacobi-type point families
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, used by the ultraspherical families
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The function allocates it
  END FUNCTION InterpolationPoint_Quadrangle1
END INTERFACE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!                   InterpolationPoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the interpolation points of a quadrangle of order
!   `order`, without allocating.
!
! This routine computes the interpolation points of a quadrangle.
! The order is `order` in both directions.
! It writes the points into `ans(1:nrow, 1:ncol)`.
!
! The point family `ipType` is the same in both directions.
! The default domain is the square $[-1, 1]^2$.
! If `xij` is present, the routine maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then `nrow` equals `SIZE(xij, 1)`.
! Otherwise `nrow` is 2.
! `ncol` is `(order + 1)**2`.
!
! If `layout` starts with `"VEFC"`, the routine orders the points as
! vertices, then edge points, then interior points.
! For any other layout, such as `"INCREASING"`, the y index runs fastest.
!
! @note
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/InterpolationPoint_Quadrangle__test_1.F90)
!
! See also: [[InterpolationPoint_Quadrangle2_]],
! [[InterpolationPoint_Quadrangle1]]

INTERFACE InterpolationPoint_Quadrangle_
  MODULE SUBROUTINE InterpolationPoint_Quadrangle1_( &
    order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order in both directions
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point family, for example `Equidistance`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    CHARACTER(*), INTENT(IN) :: layout
    !! point ordering, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, used by the Jacobi-type point families
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, used by the Jacobi-type point families
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, used by the ultraspherical families
  END SUBROUTINE InterpolationPoint_Quadrangle1_
END INTERFACE InterpolationPoint_Quadrangle_

!----------------------------------------------------------------------------
!                    InterpolationPoint_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the interpolation points of a quadrangle with order `p`
!   in x and `q` in y.
!
! This function returns the interpolation points of a quadrangle.
! The order is `p` in the x direction and `q` in the y direction.
! The result has `(p + 1) * (q + 1)` columns, one per point.
!
! Each direction has its own point family, `ipType1` and `ipType2`.
! It also has its own parameters, such as `alpha1` and `alpha2`.
! The points are the tensor product of the one-dimensional points.
! The default domain is the square $[-1, 1]^2$.
! If `xij` is present, the function maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then the result has `SIZE(xij, 1)` rows.
! Otherwise the result has 2 rows.
!
! If `layout` starts with `"VEFC"`, the function orders the points as
! vertices, then edge points, then interior points.
! For any other layout, such as `"INCREASING"`, the y index runs fastest.
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/InterpolationPoint_Quadrangle_test_2.F90)
!
! See also: [[InterpolationPoint_Quadrangle1]],
! [[InterpolationPoint_Quadrangle2_]]

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle2( &
    p, q, ipType1, ipType2, layout, xij, alpha1, beta1, lambda1, alpha2, &
    beta2, lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    INTEGER(I4B), INTENT(IN) :: ipType1
    !! interpolation point family in the x direction
    INTEGER(I4B), INTENT(IN) :: ipType2
    !! interpolation point family in the y direction
    CHARACTER(*), INTENT(IN) :: layout
    !! point ordering, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter in the x direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter in the x direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter in the x direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter in the y direction
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The function allocates it
  END FUNCTION InterpolationPoint_Quadrangle2
END INTERFACE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!                    InterpolationPoint_Quadrangle_@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the interpolation points of a quadrangle with order
!   `p` in x and `q` in y, without allocating.
!
! This routine computes the interpolation points of a quadrangle.
! The order is `p` in the x direction and `q` in the y direction.
! It writes the points into `ans(1:nrow, 1:ncol)`.
!
! Each direction has its own point family, `ipType1` and `ipType2`.
! It also has its own parameters, such as `alpha1` and `alpha2`.
! The points are the tensor product of the one-dimensional points.
! The default domain is the square $[-1, 1]^2$.
! If `xij` is present, the routine maps the points to the quadrangle
! with the vertices `xij(:, 1:4)`.
! Then `nrow` equals `SIZE(xij, 1)`.
! Otherwise `nrow` is 2.
! `ncol` is `(p + 1) * (q + 1)`.
!
! If `layout` starts with `"VEFC"`, the routine orders the points as
! vertices, then edge points, then interior points.
! For any other layout, such as `"INCREASING"`, the y index runs fastest.
!
! @note
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endnote
!
! See also: [[InterpolationPoint_Quadrangle1_]],
! [[InterpolationPoint_Quadrangle2]]

INTERFACE InterpolationPoint_Quadrangle_
  MODULE SUBROUTINE InterpolationPoint_Quadrangle2_( &
    p, q, ipType1, ipType2, ans, nrow, ncol, layout, xij, alpha1, beta1, &
    lambda1, alpha2, beta2, lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    INTEGER(I4B), INTENT(IN) :: ipType1
    !! interpolation point family in the x direction
    INTEGER(I4B), INTENT(IN) :: ipType2
    !! interpolation point family in the y direction
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coordinates of the points, one point per column.
    !! The routine fills `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    CHARACTER(*), INTENT(IN) :: layout
    !! point ordering, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the quadrangle, one vertex per column.
    !! It has 4 columns.
    !! Default is the square $[-1, 1]^2$
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter in the x direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter in the x direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter in the x direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter in the y direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter in the y direction
  END SUBROUTINE InterpolationPoint_Quadrangle2_
END INTERFACE InterpolationPoint_Quadrangle_

!----------------------------------------------------------------------------
!                                IJ2VEFC_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-17
! summary: Reorders a tensor grid of points into VEFC layout.
!
! This routine reorders the points of a tensor grid.
! The grids `xi` and `eta` hold the first and second coordinates.
! The routine writes the points into `temp` in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! The vertices run anticlockwise, starting at corner 1, the point
! `(1, 1)` of the grid.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/IJ2VEFC_Quadrangle_AntiClockwise_test_1.F90)
!   uses the anticlockwise routine that this one calls
!
! See also: [[IJ2VEFC_Quadrangle_AntiClockwise]],
! [[IJ2VEFC_Quadrangle_Clockwise]]

INTERFACE
  MODULE SUBROUTINE IJ2VEFC_Quadrangle(xi, eta, temp, p, q)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    !! first coordinates of the grid points, shape `[p + 1, q + 1]`
    REAL(DFP), INTENT(IN) :: eta(:, :)
    !! second coordinates of the grid points, shape `[p + 1, q + 1]`
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    !! reordered points, one point per column.
    !! It has 2 rows and `(p + 1) * (q + 1)` columns
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
  END SUBROUTINE IJ2VEFC_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                IJ2VEFC_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorders a tensor grid of points into VEFC layout, with the
!   vertices running clockwise.
!
! This routine reorders the points of a tensor grid.
! The grids `xi` and `eta` hold the first and second coordinates.
! The routine writes the points into `temp` in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! The vertices run clockwise.
! Number the corners of the grid anticlockwise from 1 to 4.
! Corner 1 is `(1, 1)`, corner 2 is `(p + 1, 1)`, corner 3 is
! `(p + 1, q + 1)` and corner 4 is `(1, q + 1)`.
! The first vertex written is the corner `startNode`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/IJ2VEFC_Quadrangle_Clockwise_test_1.F90)
!
! See also: [[IJ2VEFC_Quadrangle_AntiClockwise]],
! [[IJ2VEFC_Quadrangle]]

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE IJ2VEFC_Quadrangle_Clockwise(xi, eta, &
                                                                temp, p, &
                                                                q, &
                                                                startNode)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    !! first coordinates of the grid points, shape `[p + 1, q + 1]`
    REAL(DFP), INTENT(IN) :: eta(:, :)
    !! second coordinates of the grid points, shape `[p + 1, q + 1]`
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    !! reordered points, one point per column.
    !! It has 2 rows and `(p + 1) * (q + 1)` columns
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    INTEGER(I4B), INTENT(IN) :: startNode
    !! first vertex to write, a corner number from 1 to 4
  END SUBROUTINE IJ2VEFC_Quadrangle_Clockwise
END INTERFACE

!----------------------------------------------------------------------------
!                                IJ2VEFC_Quadrangle@InterpolationPointMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorders a tensor grid of points into VEFC layout, with the
!   vertices running anticlockwise.
!
! This routine reorders the points of a tensor grid.
! The grids `xi` and `eta` hold the first and second coordinates.
! The routine writes the points into `temp` in VEFC layout.
! This means the four vertices come first, then the edge points, then
! the interior points.
!
! The vertices run anticlockwise.
! Number the corners of the grid anticlockwise from 1 to 4.
! Corner 1 is `(1, 1)`, corner 2 is `(p + 1, 1)`, corner 3 is
! `(p + 1, q + 1)` and corner 4 is `(1, q + 1)`.
! The first vertex written is the corner `startNode`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/IJ2VEFC_Quadrangle_AntiClockwise_test_1.F90)
!
! See also: [[IJ2VEFC_Quadrangle_Clockwise]],
! [[IJ2VEFC_Quadrangle]]

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE IJ2VEFC_Quadrangle_AntiClockwise(xi, eta, &
                                                                    temp, p, &
                                                                    q, &
                                                                    startNode)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    !! first coordinates of the grid points, shape `[p + 1, q + 1]`
    REAL(DFP), INTENT(IN) :: eta(:, :)
    !! second coordinates of the grid points, shape `[p + 1, q + 1]`
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    !! reordered points, one point per column.
    !! It has 2 rows and `(p + 1) * (q + 1)` columns
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order in the x direction
    INTEGER(I4B), INTENT(IN) :: q
    !! polynomial order in the y direction
    INTEGER(I4B), INTENT(IN) :: startNode
    !! first vertex to write, a corner number from 1 to 4
  END SUBROUTINE IJ2VEFC_Quadrangle_AntiClockwise
END INTERFACE

!----------------------------------------------------------------------------
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the Dubiner basis at a set of points.
!
! Evaluates all Dubiner basis functions up to total degree `order`
! at the points `xij`.
!
! The basis has `(order + 1) * (order + 2) / 2` functions.
! It holds all products of total degree up to `order`.
! The tensor-product space is larger, with `(order + 1)**2` functions.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Function `(i, j)` has Legendre degree `i` in x.
! It has total degree `i + j`, which must not exceed `order`.
! The columns follow `i` first, then `j`.
! This gives `(0, 0)`, `(0, 1)`, ..., `(1, 0)`, `(1, 1)`, ...
!
! Returns:
!   ans : REAL(DFP) - basis values.
!   `ans(ii, k)` is function `k` at point `ii`.
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) \left(\frac{1-y}{2}\right)^i
!   P_j^{(2i+1, 0)}(y), \quad i + j \le order $$
!
! Here `P_i` is the Legendre polynomial of degree `i`.
! The polynomial `P_j^{(a, b)}` is the Jacobi polynomial of degree `j`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/Dubiner_Quadrangle_test_1.F90)
! - [test 2](|base|Quadrangle/examples/Dubiner_Quadrangle_test_2.F90)
!
! See also: [[Dubiner_Quadrangle1_]], [[Dubiner_Quadrangle2]],
! [[DubinerGradient_Quadrangle1]]

INTERFACE Dubiner_Quadrangle
  MODULE PURE FUNCTION Dubiner_Quadrangle1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest total degree of the basis, `order >= 0`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the biunit square, shape `[2, npts]`.
    !! `xij(1, :)` holds x and `xij(2, :)` holds y
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! basis values.
    !! `ans(:, k)` is function `k` at all points.
    !! `ans(ii, :)` is all functions at point `ii`
  END FUNCTION Dubiner_Quadrangle1
END INTERFACE Dubiner_Quadrangle

!----------------------------------------------------------------------------
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the Dubiner basis at a set of points, into a buffer.
!
! Evaluates all Dubiner basis functions up to total degree `order`
! at the points `xij`.
! The routine writes into the buffer `ans` and does not allocate.
! It returns the size of the written block in `nrow` and `ncol`.
!
! The basis has `(order + 1) * (order + 2) / 2` functions.
! It holds all products of total degree up to `order`.
! The tensor-product space is larger, with `(order + 1)**2` functions.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Function `(i, j)` has Legendre degree `i` in x.
! It has total degree `i + j`, which must not exceed `order`.
! The columns follow `i` first, then `j`.
! This gives `(0, 0)`, `(0, 1)`, ..., `(1, 0)`, `(1, 1)`, ...
!
! @note
! `ans` must be at least `SIZE(xij, 2)` by
! `(order + 1) * (order + 2) / 2`.
! @endnote
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) \left(\frac{1-y}{2}\right)^i
!   P_j^{(2i+1, 0)}(y), \quad i + j \le order $$
!
! Here `P_i` is the Legendre polynomial of degree `i`.
! The polynomial `P_j^{(a, b)}` is the Jacobi polynomial of degree `j`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/Dubiner_Quadrangle__test_1.F90)
!
! See also: [[Dubiner_Quadrangle1]], [[Dubiner_Quadrangle2_]]

INTERFACE Dubiner_Quadrangle_
  MODULE PURE SUBROUTINE Dubiner_Quadrangle1_(order, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest total degree of the basis, `order >= 0`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the biunit square, shape `[2, npts]`.
    !! `xij(1, :)` holds x and `xij(2, :)` holds y
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values.
    !! `ans(:, k)` is function `k` at all points.
    !! `ans(ii, :)` is all functions at point `ii`.
    !! The routine fills only the block `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of points, `SIZE(xij, 2)`.
    !! `ncol` is the number of functions, `(order + 1) * (order + 2) / 2`
  END SUBROUTINE Dubiner_Quadrangle1_
END INTERFACE Dubiner_Quadrangle_

!----------------------------------------------------------------------------
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the Dubiner basis on a grid of points.
!
! Evaluates all Dubiner basis functions up to total degree `order`
! on the grid formed by `x` and `y`.
! Grid point `(i - 1) * SIZE(y) + j` is `(x(i), y(j))`.
! Along the rows of `ans`, `y` varies fastest.
!
! The basis has `(order + 1) * (order + 2) / 2` functions.
! It holds all products of total degree up to `order`.
! The tensor-product space is larger, with `(order + 1)**2` functions.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Function `(i, j)` has Legendre degree `i` in x.
! It has total degree `i + j`, which must not exceed `order`.
! The columns follow `i` first, then `j`.
! This gives `(0, 0)`, `(0, 1)`, ..., `(1, 0)`, `(1, 1)`, ...
!
! Returns:
!   ans : REAL(DFP) - basis values.
!   `ans(ii, k)` is function `k` at grid point `ii`.
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) \left(\frac{1-y}{2}\right)^i
!   P_j^{(2i+1, 0)}(y), \quad i + j \le order $$
!
! Here `P_i` is the Legendre polynomial of degree `i`.
! The polynomial `P_j^{(a, b)}` is the Jacobi polynomial of degree `j`.
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/Dubiner_Quadrangle_test_2.F90)
!
! See also: [[Dubiner_Quadrangle1]], [[Dubiner_Quadrangle2_]]

INTERFACE Dubiner_Quadrangle
  MODULE PURE FUNCTION Dubiner_Quadrangle2(order, x, y) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest total degree of the basis, `order >= 0`
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates of the grid, in `[-1, 1]`
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinates of the grid, in `[-1, 1]`
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! basis values.
    !! `ans(:, k)` is function `k` at all grid points.
    !! `ans(ii, :)` is all functions at grid point `ii`
  END FUNCTION Dubiner_Quadrangle2
END INTERFACE Dubiner_Quadrangle

!----------------------------------------------------------------------------
!                                            DubinerPolynomial@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the Dubiner basis on a grid of points, into a buffer.
!
! Evaluates all Dubiner basis functions up to total degree `order`
! on the grid formed by `x` and `y`.
! The routine writes into the buffer `ans` and does not allocate.
! It returns the size of the written block in `nrow` and `ncol`.
! Grid point `(i - 1) * SIZE(y) + j` is `(x(i), y(j))`.
! Along the rows of `ans`, `y` varies fastest.
!
! The basis has `(order + 1) * (order + 2) / 2` functions.
! It holds all products of total degree up to `order`.
! The tensor-product space is larger, with `(order + 1)**2` functions.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Function `(i, j)` has Legendre degree `i` in x.
! It has total degree `i + j`, which must not exceed `order`.
! The columns follow `i` first, then `j`.
! This gives `(0, 0)`, `(0, 1)`, ..., `(1, 0)`, `(1, 1)`, ...
!
! @note
! `ans` must be at least `SIZE(x) * SIZE(y)` by
! `(order + 1) * (order + 2) / 2`.
! @endnote
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) \left(\frac{1-y}{2}\right)^i
!   P_j^{(2i+1, 0)}(y), \quad i + j \le order $$
!
! Here `P_i` is the Legendre polynomial of degree `i`.
! The polynomial `P_j^{(a, b)}` is the Jacobi polynomial of degree `j`.
!
! See also: [[Dubiner_Quadrangle2]], [[Dubiner_Quadrangle1_]]

INTERFACE Dubiner_Quadrangle_
  MODULE PURE SUBROUTINE Dubiner_Quadrangle2_(order, x, y, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest total degree of the basis, `order >= 0`
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates of the grid, in `[-1, 1]`
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinates of the grid, in `[-1, 1]`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values.
    !! `ans(:, k)` is function `k` at all grid points.
    !! `ans(ii, :)` is all functions at grid point `ii`.
    !! The routine fills only the block `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of grid points, `SIZE(x) * SIZE(y)`.
    !! `ncol` is the number of functions, `(order + 1) * (order + 2) / 2`
  END SUBROUTINE Dubiner_Quadrangle2_
END INTERFACE Dubiner_Quadrangle_

!----------------------------------------------------------------------------
!                                              DubinerGradient@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the Dubiner basis at a set of points.
!
! Evaluates the gradients of all Dubiner basis functions up to total
! degree `order` at the points `xij`.
!
! The basis has `(order + 1) * (order + 2) / 2` functions.
! The functions and their order are those of [[Dubiner_Quadrangle1]].
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The last index of `ans` selects the derivative.
! Index 1 is the derivative in `x` and index 2 is the derivative in `y`.
!
! Returns:
!   ans : REAL(DFP) - gradient values.
!   `ans(ii, k, d)` is derivative `d` of function `k` at point `ii`.
!
! Formula:
!   $$ \partial_x \phi_{ij} = P_i'(x) \left(\frac{1-y}{2}\right)^i
!   P_j^{(2i+1, 0)}(y) $$
!   $$ \partial_y \phi_{ij} = P_i(x) \left(\frac{1-y}{2}\right)^{i-1}
!   \left(\frac{1-y}{2} P_j'(y) - \frac{i}{2} P_j(y)\right) $$
!
! Here `P_i` is the Legendre polynomial of degree `i`.
! In the second formula, `P_j` is the Jacobi polynomial with
! parameters `(2i+1, 0)`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/DubinerGradient_Quadrangle_test_1.F90)
! - [test 2](|base|Quadrangle/examples/DubinerGradient_Quadrangle_test_2.F90)
!
! See also: [[DubinerGradient_Quadrangle1_]], [[Dubiner_Quadrangle1]]

INTERFACE DubinerGradient_Quadrangle
  MODULE PURE FUNCTION DubinerGradient_Quadrangle1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest total degree of the basis, `order >= 0`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the biunit square, shape `[2, npts]`.
    !! `xij(1, :)` holds x and `xij(2, :)` holds y
    REAL(DFP) :: ans(SIZE(xij, 2), &
                     (order + 1_I4B) * (order + 2_I4B) / 2_I4B, &
                     2_I4B)
    !! gradient values, shape
    !! `[npts, (order + 1) * (order + 2) / 2, 2]`.
    !! `ans(:, k, 1)` is the x derivative of function `k`.
    !! `ans(:, k, 2)` is the y derivative of function `k`
  END FUNCTION DubinerGradient_Quadrangle1
END INTERFACE DubinerGradient_Quadrangle

!----------------------------------------------------------------------------
!                                              DubinerGradient@DubinerMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the Dubiner basis, into a buffer.
!
! Evaluates the gradients of all Dubiner basis functions up to total
! degree `order` at the points `xij`.
! The routine writes into the buffer `ans` and does not allocate.
! It returns the size of the written block in `tsize1`, `tsize2`
! and `tsize3`.
!
! The basis has `(order + 1) * (order + 2) / 2` functions.
! The functions and their order are those of [[Dubiner_Quadrangle1]].
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The last index of `ans` selects the derivative.
! Index 1 is the derivative in `x` and index 2 is the derivative in `y`.
!
! @note
! `ans` must be at least `SIZE(xij, 2)` by
! `(order + 1) * (order + 2) / 2` by 2.
! @endnote
!
! Formula:
!   $$ \partial_x \phi_{ij} = P_i'(x) \left(\frac{1-y}{2}\right)^i
!   P_j^{(2i+1, 0)}(y) $$
!   $$ \partial_y \phi_{ij} = P_i(x) \left(\frac{1-y}{2}\right)^{i-1}
!   \left(\frac{1-y}{2} P_j'(y) - \frac{i}{2} P_j(y)\right) $$
!
! Here `P_i` is the Legendre polynomial of degree `i`.
! In the second formula, `P_j` is the Jacobi polynomial with
! parameters `(2i+1, 0)`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/DubinerGradient_Quadrangle__test_1.F90)
!
! See also: [[DubinerGradient_Quadrangle1]], [[Dubiner_Quadrangle1_]]

INTERFACE DubinerGradient_Quadrangle_
  MODULE PURE SUBROUTINE DubinerGradient_Quadrangle1_(order, xij, ans, &
                                                      tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest total degree of the basis, `order >= 0`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the biunit square, shape `[2, npts]`.
    !! `xij(1, :)` holds x and `xij(2, :)` holds y
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient values.
    !! `ans(:, k, 1)` is the x derivative of function `k`.
    !! `ans(:, k, 2)` is the y derivative of function `k`.
    !! The routine fills only the block `ans(1:tsize1, 1:tsize2, 1:tsize3)`
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
    !! `tsize1` is the number of points, `SIZE(xij, 2)`.
    !! `tsize2` is the number of functions, `(order + 1) * (order + 2) / 2`.
    !! `tsize3` is 2, the number of derivatives
  END SUBROUTINE DubinerGradient_Quadrangle1_
END INTERFACE DubinerGradient_Quadrangle_

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis at a set of
!   points.
!
! Evaluates the tensor-product basis of degree `p` in x and `q` in y
! at the points `xij`.
!
! The basis has `(p + 1) * (q + 1)` functions.
! Function `(i, j)` is a product of a degree `i` polynomial in x
! and a degree `j` polynomial in y.
! The x degree varies fastest, so column `j * (p + 1) + i + 1` holds
! function `(i, j)`.
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The arguments `basisType1` and `basisType2` select the polynomial
! family, for example `poly%Legendre` or `poly%Monomial`.
! The families `poly%Jacobi` and `poly%Ultraspherical` need
! `alpha` and `beta`, or `lambda`.
! Arguments with suffix 1 apply to x, and those with suffix 2 to y.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - basis values.
!   `ans(ii, k)` is function `k` at point `ii`.
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) Q_j(y), \quad 0 \le i \le p,
!   \; 0 \le j \le q $$
!
! Here `P_i` and `Q_j` are the one-dimensional basis functions.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/TensorProdBasis_Quadrangle_test_1.F90)
!
! See also: [[TensorProdBasis_Quadrangle1_]], [[TensorProdBasis_Quadrangle2]]

INTERFACE TensorProdBasis_Quadrangle
  MODULE FUNCTION TensorProdBasis_Quadrangle1(p, q, xij, basisType1, &
                                              basisType2, alpha1, beta1, &
                                              lambda1, alpha2, beta2, &
                                              lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest degree in the x direction, `p >= 0`
    INTEGER(I4B), INTENT(IN) :: q
    !! highest degree in the y direction, `q >= 0`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the biunit square, shape `[2, npts]`.
    !! `xij(1, :)` holds x and `xij(2, :)` holds y
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! polynomial family in the x direction, for example `poly%Legendre`.
    !! The routine also uses it in the y direction
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! polynomial family in the y direction.
    !! The routine ignores it
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter alpha in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter beta in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter lambda in the x direction.
    !! Needed when `basisType1` is `poly%Ultraspherical`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter alpha in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter beta in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter lambda in the y direction.
    !! Needed when the family is `poly%Ultraspherical`
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1))
    !! basis values.
    !! `ans(:, k)` is function `k` at all points.
    !! `ans(ii, :)` is all functions at point `ii`
  END FUNCTION TensorProdBasis_Quadrangle1
END INTERFACE TensorProdBasis_Quadrangle

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis at a set of
!   points. Alias of [[TensorProdBasis_Quadrangle1]].
!
! This generic name calls [[TensorProdBasis_Quadrangle1]] with the same
! arguments.
! The routine returns the basis values of degree `p` in x and `q` in y
! at the points in `xij`.
! Column `j * (p + 1) + i + 1` of the result holds the function that
! has degree `i` in x and degree `j` in y.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) Q_j(y), \quad 0 \le i \le p,
!   \; 0 \le j \le q $$
!
! See also: [[TensorProdBasis_Quadrangle1]],
! [[OrthogonalBasis_Quadrangle_]]

INTERFACE OrthogonalBasis_Quadrangle
  MODULE PROCEDURE TensorProdBasis_Quadrangle1
END INTERFACE OrthogonalBasis_Quadrangle

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis at a set of
!   points, into a buffer.
!
! Evaluates the tensor-product basis of degree `p` in x and `q` in y
! at the points `xij`.
! The routine writes into the buffer `ans` and does not allocate.
! It returns the size of the written block in `nrow` and `ncol`.
!
! The basis has `(p + 1) * (q + 1)` functions.
! Function `(i, j)` is a product of a degree `i` polynomial in x
! and a degree `j` polynomial in y.
! The x degree varies fastest, so column `j * (p + 1) + i + 1` holds
! function `(i, j)`.
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The arguments `basisType1` and `basisType2` select the polynomial
! family, for example `poly%Legendre` or `poly%Monomial`.
! The families `poly%Jacobi` and `poly%Ultraspherical` need
! `alpha` and `beta`, or `lambda`.
! Arguments with suffix 1 apply to x, and those with suffix 2 to y.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! @note
! `ans` must be at least `SIZE(xij, 2)` by `(p + 1) * (q + 1)`.
! @endnote
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) Q_j(y), \quad 0 \le i \le p,
!   \; 0 \le j \le q $$
!
! Here `P_i` and `Q_j` are the one-dimensional basis functions.
!
! See also: [[TensorProdBasis_Quadrangle1]], [[TensorProdBasis_Quadrangle2_]]

INTERFACE TensorProdBasis_Quadrangle_
  MODULE SUBROUTINE TensorProdBasis_Quadrangle1_(p, q, xij, ans, nrow, &
                                                 ncol, basisType1, &
                                                 basisType2, alpha1, beta1, &
                                                 lambda1, alpha2, &
                                                 beta2, lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest degree in the x direction, `p >= 0`
    INTEGER(I4B), INTENT(IN) :: q
    !! highest degree in the y direction, `q >= 0`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the biunit square, shape `[2, npts]`.
    !! `xij(1, :)` holds x and `xij(2, :)` holds y
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values.
    !! `ans(:, k)` is function `k` at all points.
    !! The routine fills only the block `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of points, `SIZE(xij, 2)`.
    !! `ncol` is the number of functions, `(p + 1) * (q + 1)`
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! polynomial family in the x direction, for example `poly%Legendre`.
    !! The routine also uses it in the y direction
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! polynomial family in the y direction.
    !! The routine ignores it
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter alpha in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter beta in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter lambda in the x direction.
    !! Needed when `basisType1` is `poly%Ultraspherical`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter alpha in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter beta in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter lambda in the y direction.
    !! Needed when the family is `poly%Ultraspherical`
  END SUBROUTINE TensorProdBasis_Quadrangle1_
END INTERFACE TensorProdBasis_Quadrangle_

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis at a set of
!   points, into a buffer. Alias of [[TensorProdBasis_Quadrangle1_]].
!
! This generic name calls [[TensorProdBasis_Quadrangle1_]] with the same
! arguments.
! The routine writes the basis values of degree `p` in x and `q` in y
! into the buffer `ans` and does not allocate.
! It returns the size of the written block in `nrow` and `ncol`.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! See also: [[TensorProdBasis_Quadrangle1_]], [[OrthogonalBasis_Quadrangle]]

INTERFACE OrthogonalBasis_Quadrangle_
  MODULE PROCEDURE TensorProdBasis_Quadrangle1_
END INTERFACE OrthogonalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis on a grid of
!   points.
!
! Evaluates the tensor-product basis of degree `p` in x and `q` in y
! on the grid formed by `x` and `y`.
! Grid point `(i - 1) * SIZE(y) + j` is `(x(i), y(j))`.
! Along the rows of `ans`, `y` varies fastest.
!
! The basis has `(p + 1) * (q + 1)` functions.
! Function `(i, j)` is a product of a degree `i` polynomial in x
! and a degree `j` polynomial in y.
! The x degree varies fastest, so column `j * (p + 1) + i + 1` holds
! function `(i, j)`.
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The arguments `basisType1` and `basisType2` select the polynomial
! family, for example `poly%Legendre` or `poly%Monomial`.
! The families `poly%Jacobi` and `poly%Ultraspherical` need
! `alpha` and `beta`, or `lambda`.
! Arguments with suffix 1 apply to x, and those with suffix 2 to y.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - basis values.
!   `ans(ii, k)` is function `k` at grid point `ii`.
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) Q_j(y), \quad 0 \le i \le p,
!   \; 0 \le j \le q $$
!
! Here `P_i` and `Q_j` are the one-dimensional basis functions.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/TensorProdBasis_Quadrangle_test_1.F90)
!
! See also: [[TensorProdBasis_Quadrangle1]], [[TensorProdBasis_Quadrangle2_]]

INTERFACE TensorProdBasis_Quadrangle
  MODULE FUNCTION TensorProdBasis_Quadrangle2(p, q, x, y, basisType1, &
                                              basisType2, alpha1, beta1, &
                                              lambda1, alpha2, beta2, &
                                              lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest degree in the x direction, `p >= 0`
    INTEGER(I4B), INTENT(IN) :: q
    !! highest degree in the y direction, `q >= 0`
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! x and y coordinates of the grid, in `[-1, 1]`
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! polynomial family in the x direction, for example `poly%Legendre`.
    !! The routine also uses it in the y direction
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! polynomial family in the y direction.
    !! The routine ignores it
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter alpha in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter beta in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter alpha in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter beta in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter lambda in the x direction.
    !! Needed when `basisType1` is `poly%Ultraspherical`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter lambda in the y direction.
    !! Needed when the family is `poly%Ultraspherical`
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (p + 1) * (q + 1))
    !! basis values.
    !! `ans(:, k)` is function `k` at all grid points.
    !! `ans(ii, :)` is all functions at grid point `ii`
  END FUNCTION TensorProdBasis_Quadrangle2
END INTERFACE TensorProdBasis_Quadrangle

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis on a grid of
!   points. Alias of [[TensorProdBasis_Quadrangle2]].
!
! This generic name calls [[TensorProdBasis_Quadrangle2]] with the same
! arguments.
! The routine returns the basis values of degree `p` in x and `q` in y
! on the grid formed by `x` and `y`.
! Grid point `(i - 1) * SIZE(y) + j` is `(x(i), y(j))`.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! See also: [[TensorProdBasis_Quadrangle2]], [[OrthogonalBasis_Quadrangle_]]

INTERFACE OrthogonalBasis_Quadrangle
  MODULE PROCEDURE TensorProdBasis_Quadrangle2
END INTERFACE OrthogonalBasis_Quadrangle

!----------------------------------------------------------------------------
!                                TensorProdBasis_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis on a grid of
!   points, into a buffer.
!
! Evaluates the tensor-product basis of degree `p` in x and `q` in y
! on the grid formed by `x` and `y`.
! The routine writes into the buffer `ans` and does not allocate.
! It returns the size of the written block in `nrow` and `ncol`.
! Grid point `(i - 1) * SIZE(y) + j` is `(x(i), y(j))`.
! Along the rows of `ans`, `y` varies fastest.
!
! The basis has `(p + 1) * (q + 1)` functions.
! Function `(i, j)` is a product of a degree `i` polynomial in x
! and a degree `j` polynomial in y.
! The x degree varies fastest, so column `j * (p + 1) + i + 1` holds
! function `(i, j)`.
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The arguments `basisType1` and `basisType2` select the polynomial
! family, for example `poly%Legendre` or `poly%Monomial`.
! The families `poly%Jacobi` and `poly%Ultraspherical` need
! `alpha` and `beta`, or `lambda`.
! Arguments with suffix 1 apply to x, and those with suffix 2 to y.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! @note
! `ans` has the fixed shape `SIZE(x) * SIZE(y)` by `(p + 1) * (q + 1)`.
! @endnote
!
! Formula:
!   $$ \phi_{ij}(x, y) = P_i(x) Q_j(y), \quad 0 \le i \le p,
!   \; 0 \le j \le q $$
!
! Here `P_i` and `Q_j` are the one-dimensional basis functions.
!
! See also: [[TensorProdBasis_Quadrangle2]], [[TensorProdBasis_Quadrangle1_]]

INTERFACE TensorProdBasis_Quadrangle_
  MODULE SUBROUTINE TensorProdBasis_Quadrangle2_(p, q, x, y, ans, nrow, &
                                                 ncol, basisType1, &
                                                 basisType2, alpha1, beta1, &
                                                 lambda1, alpha2, beta2, &
                                                 lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest degree in the x direction, `p >= 0`
    INTEGER(I4B), INTENT(IN) :: q
    !! highest degree in the y direction, `q >= 0`
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! x and y coordinates of the grid, in `[-1, 1]`
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (p + 1) * (q + 1))
    !! basis values.
    !! `ans(:, k)` is function `k` at all grid points.
    !! `ans(ii, :)` is all functions at grid point `ii`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of grid points, `SIZE(x) * SIZE(y)`.
    !! `ncol` is the number of functions, `(p + 1) * (q + 1)`
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! polynomial family in the x direction, for example `poly%Legendre`.
    !! The routine also uses it in the y direction
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! polynomial family in the y direction.
    !! The routine ignores it
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter alpha in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter beta in the x direction.
    !! Needed when `basisType1` is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter alpha in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter beta in the y direction.
    !! Needed when the family is `poly%Jacobi`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter lambda in the x direction.
    !! Needed when `basisType1` is `poly%Ultraspherical`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter lambda in the y direction.
    !! Needed when the family is `poly%Ultraspherical`
  END SUBROUTINE TensorProdBasis_Quadrangle2_
END INTERFACE TensorProdBasis_Quadrangle_

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the anisotropic tensor-product basis on a grid of
!   points, into a buffer. Alias of [[TensorProdBasis_Quadrangle2_]].
!
! This generic name calls [[TensorProdBasis_Quadrangle2_]] with the same
! arguments.
! The routine writes the basis values of degree `p` in x and `q` in y
! on the grid formed by `x` and `y`.
! It returns the size of the written block in `nrow` and `ncol`.
! Grid point `(i - 1) * SIZE(y) + j` is `(x(i), y(j))`.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! See also: [[TensorProdBasis_Quadrangle2_]], [[OrthogonalBasis_Quadrangle]]

INTERFACE OrthogonalBasis_Quadrangle_
  MODULE PROCEDURE TensorProdBasis_Quadrangle2_
END INTERFACE OrthogonalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the vertex basis of a quadrangle at points `(x, y)`
!
! Evaluates the four bilinear vertex functions at the points
! `(x(i), y(i))`.
! The arrays `x` and `y` must have the same size.
! The coordinates must lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! Column `v` of the result is the function of vertex `v`.
! The vertices run counterclockwise, starting at `(-1, -1)`.
!
! Returns:
!   ans : REAL(DFP) - the vertex functions, size `(SIZE(x), 4)`
!
! Formula:
!   $$ \phi_1 = \frac{(1 - x)(1 - y)}{4}, \quad
!   \phi_2 = \frac{(1 + x)(1 - y)}{4} $$
!   $$ \phi_3 = \frac{(1 + x)(1 + y)}{4}, \quad
!   \phi_4 = \frac{(1 - x)(1 + y)}{4} $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/VertexBasis_Quadrangle_test_1.F90)
!
! See also: [[VertexBasis_Quadrangle_]]

INTERFACE VertexBasis_Quadrangle
  MODULE PURE FUNCTION VertexBasis_Quadrangle1(x, y) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP) :: ans(SIZE(x), 4)
    !! `ans(:, v)` is the function of vertex `v` at all points
  END FUNCTION VertexBasis_Quadrangle1
END INTERFACE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the vertex basis of a quadrangle at points `(x, y)`
!
! This routine does the same as [[VertexBasis_Quadrangle]] with `x`, `y`.
! It writes into the array `ans` that you provide.
!
! It fills the block `ans(1:nrow, 1:ncol)`, with `ncol = 4`.
! The rest of `ans` stays unchanged.
!
! @note
! `ans` needs at least `SIZE(x)` rows and 4 columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/VertexBasis_Quadrangle_test_1.F90)
!
! See also: [[VertexBasis_Quadrangle]]

INTERFACE VertexBasis_Quadrangle_
  MODULE PURE SUBROUTINE VertexBasis_Quadrangle1_(x, y, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! vertex functions, needs `SIZE(x)` rows and 4 columns
    !! `ans(:, v)` is the function of vertex `v` at all points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
  END SUBROUTINE VertexBasis_Quadrangle1_
END INTERFACE VertexBasis_Quadrangle_

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the vertex basis of a quadrangle at points in `xij`
!
! Evaluates the four bilinear vertex functions at the points in `xij`.
! Column `j` of `xij` holds the `x` and `y` coordinates of point `j`.
! The coordinates must lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! Column `v` of the result is the function of vertex `v`.
! The vertices run counterclockwise, starting at `(-1, -1)`.
!
! Returns:
!   ans : REAL(DFP) - the vertex functions, size `(SIZE(xij, 2), 4)`
!
! Formula:
!   $$ \phi_1 = \frac{(1 - x)(1 - y)}{4}, \quad
!   \phi_2 = \frac{(1 + x)(1 - y)}{4} $$
!   $$ \phi_3 = \frac{(1 + x)(1 + y)}{4}, \quad
!   \phi_4 = \frac{(1 - x)(1 + y)}{4} $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/VertexBasis_Quadrangle_test_1.F90)
!
! See also: [[VertexBasis_Quadrangle_]]

INTERFACE VertexBasis_Quadrangle
  MODULE PURE FUNCTION VertexBasis_Quadrangle2(xij) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, size `(2, N)`, coordinates in `[-1, 1]`
    REAL(DFP) :: ans(SIZE(xij, 2), 4)
    !! `ans(:, v)` is the function of vertex `v` at all points
  END FUNCTION VertexBasis_Quadrangle2
END INTERFACE VertexBasis_Quadrangle

!----------------------------------------------------------------------------
!                                  VertexBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the vertex basis of a quadrangle at points in `xij`
!
! This routine does the same as [[VertexBasis_Quadrangle]] with `xij`.
! It writes into the array `ans` that you provide.
!
! It fills the block `ans(1:nrow, 1:ncol)`, with `ncol = 4`.
! The rest of `ans` stays unchanged.
!
! @note
! `ans` needs at least `SIZE(xij, 2)` rows and 4 columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/VertexBasis_Quadrangle_test_1.F90)
!
! See also: [[VertexBasis_Quadrangle]]

INTERFACE VertexBasis_Quadrangle_
  MODULE PURE SUBROUTINE VertexBasis_Quadrangle2_(xij, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, size `(2, N)`, coordinates in `[-1, 1]`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! vertex functions, needs `SIZE(xij, 2)` rows and 4 columns
    !! `ans(:, v)` is the function of vertex `v` at all points
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
  END SUBROUTINE VertexBasis_Quadrangle2_
END INTERFACE VertexBasis_Quadrangle_

!----------------------------------------------------------------------------
!                            VerticalEdgeBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the vertical edge hierarchical basis on a quadrangle
!
! Evaluates the modes of the two vertical edges at the points
! `(x(i), y(i))`.
! The arrays `x` and `y` must have the same size.
! The coordinates must lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The result has `qe1 - 1` modes of the left edge first.
! The `qe2 - 1` modes of the right edge follow.
! The edge modes are Lobatto polynomials in `y`, times a linear factor in
! `x`.
!
! @note
! `qe1` and `qe2` must be at least 2.
! An edge of order 1 has no modes.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - the modes, size `(SIZE(x), qe1 + qe2 - 2)`
!
! Formula:
!   $$ \phi^{left}_{k}(x, y) = (-1)^{k} \frac{1 - x}{2} L_{k}(y),
!   \quad \phi^{right}_{k}(x, y) = \frac{1 + x}{2} L_{k}(y) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/VerticalEdgeBasis_Quadrangle_test_1.F90)
!
! See also: [[VerticalEdgeBasis_Quadrangle_]]

INTERFACE
  MODULE PURE FUNCTION VerticalEdgeBasis_Quadrangle(qe1, qe2, x, y) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order on the left edge (e1), at least 2
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order on the right edge (e2), at least 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP) :: ans(SIZE(x), qe1 + qe2 - 2)
    !! edge modes, one row per point and one column per mode
  END FUNCTION VerticalEdgeBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                           VerticalEdgeBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the vertical edge hierarchical basis on a quadrangle
!
! This routine does the same as [[VerticalEdgeBasis_Quadrangle]].
! It writes into the array `ans` that you provide.
! It does not allocate memory for the result.
!
! It fills the block `ans(1:nrow, 1:ncol)`.
! The rest of `ans` stays unchanged.
!
! @note
! `ans` needs at least `SIZE(x)` rows and `qe1 + qe2 - 2` columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/VerticalEdgeBasis_Quadrangle_test_1.F90)
!
! See also: [[VerticalEdgeBasis_Quadrangle]]

INTERFACE
  MODULE PURE SUBROUTINE VerticalEdgeBasis_Quadrangle_(qe1, qe2, x, y, &
                                                       ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order on the left edge (e1), at least 2
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order on the right edge (e2), at least 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! edge modes, needs `SIZE(x)` rows and `qe1 + qe2 - 2` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
  END SUBROUTINE VerticalEdgeBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                          HorizontalEdgeBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the horizontal edge hierarchical basis on a quadrangle
!
! Evaluates the modes of the two horizontal edges at the points
! `(x(i), y(i))`.
! The arrays `x` and `y` must have the same size.
! The coordinates must lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The result has `pe3 - 1` modes of the bottom edge first.
! The `pe4 - 1` modes of the top edge follow.
! The edge modes are Lobatto polynomials in `x`, times a linear factor in
! `y`.
!
! @note
! `pe3` and `pe4` must be at least 2.
! An edge of order 1 has no modes.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - the modes, size `(SIZE(x), pe3 + pe4 - 2)`
!
! Formula:
!   $$ \phi^{bottom}_{k}(x, y) = L_{k}(x) \frac{1 - y}{2},
!   \quad \phi^{top}_{k}(x, y) = (-1)^{k} L_{k}(x) \frac{1 + y}{2} $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HorizontalEdgeBasis_Quadrangle_test_1.F90)
!
! See also: [[HorizontalEdgeBasis_Quadrangle_]]

INTERFACE
  MODULE PURE FUNCTION HorizontalEdgeBasis_Quadrangle(pe3, pe4, x, y) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the bottom edge (e3), at least 2
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the top edge (e4), at least 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP) :: ans(SIZE(x), pe3 + pe4 - 2)
    !! edge modes, one row per point and one column per mode
  END FUNCTION HorizontalEdgeBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                          HorizontalEdgeBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the horizontal edge hierarchical basis on a quadrangle
!
! This routine does the same as [[HorizontalEdgeBasis_Quadrangle]].
! It writes into the array `ans` that you provide.
! It does not allocate memory for the result.
!
! It fills the block `ans(1:nrow, 1:ncol)`.
! The rest of `ans` stays unchanged.
!
! @note
! `ans` needs at least `SIZE(x)` rows and `pe3 + pe4 - 2` columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HorizontalEdgeBasis_Quadrangle_test_1.F90)
!
! See also: [[HorizontalEdgeBasis_Quadrangle]]

INTERFACE
  MODULE PURE SUBROUTINE HorizontalEdgeBasis_Quadrangle_(pe3, pe4, x, y, &
                                                         ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the bottom edge (e3), at least 2
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the top edge (e4), at least 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! edge modes, needs `SIZE(x)` rows and `pe3 + pe4 - 2` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
  END SUBROUTINE HorizontalEdgeBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                   CellBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the cell (interior) hierarchical basis on a quadrangle
!
! Evaluates the interior modes of the quadrangle at the points
! `(x(i), y(i))`.
! The arrays `x` and `y` must have the same size.
! The coordinates must lie in the biunit square `[-1, 1] x [-1, 1]`.
!
! The result has `(pb - 1) * (qb - 1)` modes.
! Each mode is a product of Lobatto polynomials in `x` and `y`.
! The modes with the same `x` order sit next to each other.
! Inside that group, the `y` order grows from 2 to `qb`.
!
! @note
! `pb` and `qb` must be at least 2.
! Otherwise the quadrangle has no interior modes.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - the modes, size `(SIZE(x), (pb - 1) * (qb - 1))`
!
! Formula:
!   $$ \phi_{k_1 k_2}(x, y) = L_{k_1}(x) L_{k_2}(y),
!   \quad 2 \le k_1 \le p_b, \; 2 \le k_2 \le q_b $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/CellBasis_Quadrangle_test_1.F90)
!
! See also: [[CellBasis_Quadrangle_]]

INTERFACE
  MODULE PURE FUNCTION CellBasis_Quadrangle(pb, qb, x, y) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order in the `x` direction, at least 2
    INTEGER(I4B), INTENT(IN) :: qb
    !! order in the `y` direction, at least 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP) :: ans(SIZE(x), (pb - 1) * (qb - 1))
    !! cell modes, one row per point and one column per mode
  END FUNCTION CellBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                   CellBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the cell (interior) hierarchical basis on a quadrangle
!
! This routine does the same as [[CellBasis_Quadrangle]].
! It writes into the array `ans` that you provide.
! It does not allocate memory for the result.
!
! It fills the block `ans(1:nrow, 1:ncol)`.
! The rest of `ans` stays unchanged.
!
! @note
! `ans` needs at least `SIZE(x)` rows and `(pb - 1) * (qb - 1)` columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/CellBasis_Quadrangle_test_1.F90)
!
! See also: [[CellBasis_Quadrangle]]

INTERFACE
  MODULE PURE SUBROUTINE CellBasis_Quadrangle_(pb, qb, x, y, ans, nrow, &
                                               ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order in the `x` direction, at least 2
    INTEGER(I4B), INTENT(IN) :: qb
    !! order in the `y` direction, at least 2
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! evaluation points, coordinates in `[-1, 1]`, `SIZE(y) = SIZE(x)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! cell modes, needs `SIZE(x)` rows and `(pb - 1) * (qb - 1)` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
  END SUBROUTINE CellBasis_Quadrangle_
END INTERFACE

!----------------------------------------------------------------------------
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the hierarchical quadrangle basis for separate cell
! and edge orders.
!
! Evaluates every basis function at the points in `xij`.
! Each edge and the cell has its own order.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
!
! The edge and face signs are all +1.
! Use [[HeirarchicalBasis_Quadrangle3]] to set other signs.
!
! Returns:
!   ans : REAL(DFP) - basis values, size
!   `(SIZE(xij, 2), pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)`
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/HeirarchicalBasis_Quadrangle_test_2.F90) with different cell and edge orders
!
! See also: [[HeirarchicalBasis_Quadrangle1_]],
! [[HeirarchicalBasis_Quadrangle3]]

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle1(pb, qb, pe3, pe4, &
                                                     qe1, qe2, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP) :: ans(SIZE(xij, 2), &
      & pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)
    !! basis values, one row per point and one column per function
  END FUNCTION HeirarchicalBasis_Quadrangle1
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the hierarchical quadrangle basis for separate cell
! and edge orders, into a buffer.
!
! Evaluates every basis function at the points in `xij`.
! Each edge and the cell has its own order.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
!
! The edge and face signs are all +1.
! Use [[HeirarchicalBasis_Quadrangle3_]] to set other signs.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and `ncol` columns.
! The routine writes only the block `ans(1:nrow, 1:ncol)`.
! @endnote
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! See also: [[HeirarchicalBasis_Quadrangle3_]],
! [[HeirarchicalBasis_Quadrangle2_]]

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle1_( &
    pb, qb, pe3, pe4, qe1, qe2, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, written to `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1`
  END SUBROUTINE HeirarchicalBasis_Quadrangle1_
END INTERFACE HeirarchicalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the hierarchical quadrangle basis for orders `p` and
! `q`.
!
! Evaluates every basis function at the points in `xij`.
! The orders are `p` in the `x1` direction and `q` in the `x2` direction.
! The same order applies to the cell and to the edges of that direction.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! Returns:
!   ans : REAL(DFP) - basis values, size `(SIZE(xij, 2), (p + 1) * (q + 1))`
!
! Formula:
!   $$ N = (p + 1)(q + 1) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HeirarchicalBasis_Quadrangle_test_1.F90)
! - [test 2](|base|Quadrangle/examples/HeirarchicalBasis_Quadrangle_test_2.F90)
!
! See also: [[HeirarchicalBasis_Quadrangle2_]],
! [[HeirarchicalBasis_Quadrangle1]]

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle2(p, q, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in the `x1` direction, for the cell and the edges
    INTEGER(I4B), INTENT(IN) :: q
    !! order in the `x2` direction, for the cell and the edges
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1))
    !! basis values, one row per point and one column per function
  END FUNCTION HeirarchicalBasis_Quadrangle2
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!                           HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the hierarchical quadrangle basis for orders `p` and
! `q`, into a buffer.
!
! Evaluates every basis function at the points in `xij`.
! The orders are `p` in the `x1` direction and `q` in the `x2` direction.
! The same order applies to the cell and to the edges of that direction.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! @note
! `ans` must be at least `SIZE(xij, 2)` by `(p + 1) * (q + 1)`.
! The routine writes only that part.
! @endnote
!
! Formula:
!   $$ N = (p + 1)(q + 1) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HeirarchicalBasis_Quadrangle__test_1.F90)
!
! See also: [[HeirarchicalBasis_Quadrangle1_]],
! [[HeirarchicalBasis_Quadrangle2]]

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle2_(p, q, xij, ans, &
                                                        nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in the `x1` direction, for the cell and the edges
    INTEGER(I4B), INTENT(IN) :: q
    !! order in the `x2` direction, for the cell and the edges
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, written to `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `(p + 1) * (q + 1)`
  END SUBROUTINE HeirarchicalBasis_Quadrangle2_
END INTERFACE HeirarchicalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                           HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the hierarchical quadrangle basis with edge and face
! signs.
!
! Evaluates every basis function at the points in `xij`.
! Each edge and the cell has its own order.
! The signs set the orientation of each edge and of the face.
! The routine allocates the result.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
!
! Each edge sign is +1 or -1.
! The value -1 reverses the edge and flips the sign of its odd modes.
! `faceOrient` has three entries.
! Entry 1 is the sign for `x1` and entry 2 is the sign for `x2`.
! If entry 3 is negative, the cell orders `pb` and `qb` swap roles.
!
! Returns:
!   ans : REAL(DFP) - basis values, allocated with size
!   `(SIZE(xij, 2), pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)`
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/HeirarchicalBasis_Quadrangle_test_2.F90) with edge and face signs
!
! See also: [[HeirarchicalBasis_Quadrangle3_]],
! [[HeirarchicalBasis_Quadrangle1]]

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle3( &
    pb, qb, pe3, pe4, qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, &
    qe2Orient, faceOrient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! sign of the bottom edge `e3`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! sign of the top edge `e4`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! sign of the left edge `e1`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! sign of the right edge `e2`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! face signs, size 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! basis values, one row per point and one column per function
  END FUNCTION HeirarchicalBasis_Quadrangle3
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!                            HeirarchicalBasis_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the hierarchical quadrangle basis with edge and face
! signs, into a buffer.
!
! Evaluates every basis function at the points in `xij`.
! Each edge and the cell has its own order.
! The signs set the orientation of each edge and of the face.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
!
! Each edge sign is +1 or -1.
! The value -1 reverses the edge and flips the sign of its odd modes.
! `faceOrient` has three entries.
! Entry 1 is the sign for `x1` and entry 2 is the sign for `x2`.
! If entry 3 is negative, the cell orders `pb` and `qb` swap roles.
!
! @note
! `ans` must be at least `nrow` by `ncol`, with the sizes given below.
! The routine writes only that part.
! @endnote
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! See also: [[HeirarchicalBasis_Quadrangle3]],
! [[HeirarchicalBasis_Quadrangle1_]]

INTERFACE HeirarchicalBasis_Quadrangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Quadrangle3_( &
    pb, qb, pe3, pe4, qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, &
    qe2Orient, faceOrient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! sign of the bottom edge `e3`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! sign of the top edge `e4`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! sign of the left edge `e1`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! sign of the right edge `e2`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! face signs, size 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, written to `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1`
  END SUBROUTINE HeirarchicalBasis_Quadrangle3_
END INTERFACE HeirarchicalBasis_Quadrangle_

!----------------------------------------------------------------------------
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the hierarchical quadrangle basis
! for separate cell and edge orders.
!
! Evaluates the `x1` and `x2` derivatives of every basis function at the
! points in `xij`.
! Each edge and the cell has its own order.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! The edge and face signs are all +1.
! Use [[HeirarchicalBasisGradient_Quadrangle3]] to set other signs.
!
! Returns:
!   ans : REAL(DFP) - gradients, size
!   `(SIZE(xij, 2), pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1, 2)`
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/HeirarchicalBasisGradient_Quadrangle_test_2.F90) against finite differences
!
! See also: [[HeirarchicalBasisGradient_Quadrangle1_]],
! [[HeirarchicalBasisGradient_Quadrangle3]]

INTERFACE HeirarchicalBasisGradient_Quadrangle
  MODULE FUNCTION HeirarchicalBasisGradient_Quadrangle1(pb, qb, pe3, pe4, &
                                                        qe1, qe2, xij) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP) :: ans(SIZE(xij, 2), &
      & pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1, 2)
    !! gradients, `ans(:, :, 1)` is d/dx1 and `ans(:, :, 2)` is d/dx2
  END FUNCTION HeirarchicalBasisGradient_Quadrangle1
END INTERFACE HeirarchicalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the hierarchical quadrangle basis
! for separate cell and edge orders, into a buffer.
!
! Evaluates the `x1` and `x2` derivatives of every basis function at the
! points in `xij`.
! Each edge and the cell has its own order.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! The edge and face signs are all +1.
! Use [[HeirarchicalBasisGradient_Quadrangle3_]] to set other signs.
!
! @note
! `ans` must be at least `dim1` by `dim2` by 2, with the sizes given below.
! The routine writes only that part.
! @endnote
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HeirarchicalBasisGradient_Quadrangle__test_1.F90) through the `p`, `q` form
!
! See also: [[HeirarchicalBasisGradient_Quadrangle3_]],
! [[HeirarchicalBasisGradient_Quadrangle2_]]

INTERFACE HeirarchicalBasisGradient_Quadrangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Quadrangle1_(pb, qb, pe3, &
                                                           pe4, qe1, qe2, &
                                                           xij, ans, dim1, &
                                                           dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients, written to `ans(1:dim1, 1:dim2, 1:dim3)`
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! `dim1` is `SIZE(xij, 2)`
    !! `dim2` is `pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1`
    !! `dim3` is 2
  END SUBROUTINE HeirarchicalBasisGradient_Quadrangle1_
END INTERFACE HeirarchicalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the hierarchical quadrangle basis
! for orders `p` and `q`.
!
! Evaluates the `x1` and `x2` derivatives of every basis function at the
! points in `xij`.
! The orders are `p` in the `x1` direction and `q` in the `x2` direction.
! The same order applies to the cell and to the edges of that direction.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! Returns:
!   ans : REAL(DFP) - gradients, size `(SIZE(xij, 2), (p + 1) * (q + 1), 2)`
!
! Formula:
!   $$ N = (p + 1)(q + 1) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HeirarchicalBasisGradient_Quadrangle_test_1.F90)
!
! See also: [[HeirarchicalBasisGradient_Quadrangle2_]],
! [[HeirarchicalBasisGradient_Quadrangle1]]

INTERFACE HeirarchicalBasisGradient_Quadrangle
  MODULE FUNCTION HeirarchicalBasisGradient_Quadrangle2(p, q, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in the `x1` direction, for the cell and the edges
    INTEGER(I4B), INTENT(IN) :: q
    !! order in the `x2` direction, for the cell and the edges
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1), 2)
    !! gradients, `ans(:, :, 1)` is d/dx1 and `ans(:, :, 2)` is d/dx2
  END FUNCTION HeirarchicalBasisGradient_Quadrangle2
END INTERFACE HeirarchicalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the hierarchical quadrangle basis
! for orders `p` and `q`, into a buffer.
!
! Evaluates the `x1` and `x2` derivatives of every basis function at the
! points in `xij`.
! The orders are `p` in the `x1` direction and `q` in the `x2` direction.
! The same order applies to the cell and to the edges of that direction.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! @note
! `ans` must be at least `SIZE(xij, 2)` by `(p + 1) * (q + 1)` by 2.
! The routine writes only that part.
! @endnote
!
! Formula:
!   $$ N = (p + 1)(q + 1) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HeirarchicalBasisGradient_Quadrangle__test_1.F90)
!
! See also: [[HeirarchicalBasisGradient_Quadrangle1_]],
! [[HeirarchicalBasis_Quadrangle2_]]

INTERFACE HeirarchicalBasisGradient_Quadrangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Quadrangle2_(p, q, xij, &
                                                           ans, dim1, &
                                                           dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: p
    !! order in the `x1` direction, for the cell and the edges
    INTEGER(I4B), INTENT(IN) :: q
    !! order in the `x2` direction, for the cell and the edges
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients, written to `ans(1:dim1, 1:dim2, 1:dim3)`
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! `dim1` is `SIZE(xij, 2)`
    !! `dim2` is `(p + 1) * (q + 1)`
    !! `dim3` is 2
  END SUBROUTINE HeirarchicalBasisGradient_Quadrangle2_
END INTERFACE HeirarchicalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the hierarchical quadrangle basis
! with edge and face signs.
!
! Evaluates the `x1` and `x2` derivatives of every basis function at the
! points in `xij`.
! Each edge and the cell has its own order.
! The signs set the orientation of each edge and of the face.
! The routine allocates the result.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
! The cell has functions only if `pb` and `qb` are both at least 2.
!
! Each edge sign is +1 or -1.
! The value -1 reverses the edge and flips the sign of its odd modes.
! `faceOrient` has three entries.
! Entry 1 is the sign for `x1` and entry 2 is the sign for `x2`.
! If entry 3 is negative, the cell orders `pb` and `qb` swap roles.
!
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! Returns:
!   ans : REAL(DFP) - gradients, allocated with size
!   `(SIZE(xij, 2), pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1, 2)`
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
!
! Examples
!
! - [test 2](|base|Quadrangle/examples/HeirarchicalBasisGradient_Quadrangle_test_2.F90) with edge and face signs
!
! See also: [[HeirarchicalBasisGradient_Quadrangle3_]],
! [[HeirarchicalBasisGradient_Quadrangle1]]

INTERFACE HeirarchicalBasisGradient_Quadrangle
  MODULE FUNCTION HeirarchicalBasisGradient_Quadrangle3(pb, qb, pe3, pe4, &
                                                        qe1, qe2, xij, &
                                                        qe1Orient, &
    qe2Orient, pe3Orient, pe4Orient, faceOrient) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! sign of the left edge `e1`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! sign of the right edge `e2`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! sign of the bottom edge `e3`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! sign of the top edge `e4`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(3)
    !! face signs, size 3
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! gradients, `ans(:, :, 1)` is d/dx1 and `ans(:, :, 2)` is d/dx2
  END FUNCTION HeirarchicalBasisGradient_Quadrangle3
END INTERFACE HeirarchicalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!                    HeirarchicalBasisGradient_Quadrangle@HierarchicalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the hierarchical quadrangle basis
! with edge and face signs, into a buffer.
!
! Evaluates the `x1` and `x2` derivatives of every basis function at the
! points in `xij`.
! Each edge and the cell has its own order.
! The signs set the orientation of each edge and of the face.
! The routine writes into the buffer `ans` and does not allocate it.
! It returns the size of the written block in `dim1`, `dim2` and `dim3`.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The basis functions come in this order.
!
! 1. The four vertex functions.
! 2. The bottom, right, top and left edge functions.
! 3. The interior (cell) functions.
!
! An edge with order below 2 has no edge functions.
!
! Each edge sign is +1 or -1.
! The value -1 reverses the edge and flips the sign of its odd modes.
! `faceOrient` has three entries.
! Entry 1 is the sign for `x1` and entry 2 is the sign for `x2`.
! If entry 3 is negative, the cell orders `pb` and `qb` swap roles.
!
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! @note
! `ans` must be at least `dim1` by `dim2` by 2.
! The routine writes only that part.
! @endnote
!
! Formula:
!   $$ N = 4 + \sum_e (p_e - 1) + (p_b - 1)(q_b - 1) $$
!
! Here the sum runs over the four edge orders, each at least 1.
! `dim1` is `SIZE(xij, 2)`, `dim2` is `N` and `dim3` is 2.
!
! See also: [[HeirarchicalBasisGradient_Quadrangle3]],
! [[HeirarchicalBasisGradient_Quadrangle1_]]

INTERFACE HeirarchicalBasisGradient_Quadrangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Quadrangle3_(pb, qb, pe3, pe4, &
                                                           qe1, qe2, xij, &
                                                           qe1Orient, &
                                                           qe2Orient, &
                                                           pe3Orient, &
                                                           pe4Orient, &
                                                           faceOrient, &
                                                           ans, dim1, &
                                                           dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: pb
    !! cell order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! cell order in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of the bottom edge `e3`
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of the top edge `e4`
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of the left edge `e1`
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of the right edge `e2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    INTEGER(I4B), INTENT(IN) :: qe1Orient
    !! sign of the left edge `e1`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: qe2Orient
    !! sign of the right edge `e2`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: pe3Orient
    !! sign of the bottom edge `e3`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: pe4Orient
    !! sign of the top edge `e4`, +1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(3)
    !! face signs, size 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients, written to `ans(1:dim1, 1:dim2, 1:dim3)`.
    !! `ans(:, :, 1)` is d/dx1 and `ans(:, :, 2)` is d/dx2
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! `dim1` is `SIZE(xij, 2)`.
    !! `dim2` is `pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1`.
    !! `dim3` is 2
  END SUBROUTINE HeirarchicalBasisGradient_Quadrangle3_
END INTERFACE HeirarchicalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                       TensorProdBasisGradient_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the anisotropic tensor-product basis
! at a set of points.
!
! Evaluates the `x1` and `x2` derivatives of every tensor-product basis
! function at the points in `xij`.
! The orders are `p` in the `x1` direction and `q` in the `x2` direction.
! The basis is a product of two one-dimensional bases.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The columns of `ans` run over the `x1` degree first.
! Then they run over the `x2` degree.
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! @note
! `basisType2` is not used.
! Both directions use `basisType1`.
! Pass the same family in both directions.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - gradients, size `(SIZE(xij, 2), (p + 1) * (q + 1), 2)`
!
! Formula:
!   $$ \phi_{ij}(x_1, x_2) = P_i(x_1) Q_j(x_2) $$
!
! Here `i` runs from 0 to `p` and `j` runs from 0 to `q`.
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/TensorProdBasisGradient_Quadrangle_test_1.F90)
!
! See also: [[TensorProdBasisGradient_Quadrangle1_]]

INTERFACE TensorProdBasisGradient_Quadrangle
  MODULE FUNCTION TensorProdBasisGradient_Quadrangle1(p, q, xij, &
                                                      basisType1, &
                                                      basisType2, alpha1, &
                                                      beta1, lambda1, &
                                                      alpha2, beta2, &
                                                      lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in the `x2` direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis family used in both directions
    !! Monomial, Jacobi, Legendre, Chebyshev, Ultraspherical or
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! not used, see the note above
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter `alpha` for the `x1` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter `beta` for the `x1` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter for the `x1` direction
    !! needed when `basisType1` is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter `alpha` for the `x2` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter `beta` for the `x2` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter for the `x2` direction
    !! needed when `basisType1` is Ultraspherical
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1), 2)
    !! gradients, `ans(:, :, 1)` is d/dx1 and `ans(:, :, 2)` is d/dx2
  END FUNCTION TensorProdBasisGradient_Quadrangle1
END INTERFACE TensorProdBasisGradient_Quadrangle

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the anisotropic tensor-product basis
!   at a set of points. Alias of [[TensorProdBasisGradient_Quadrangle1]].
!
! This generic name calls [[TensorProdBasisGradient_Quadrangle1]] with the
! same arguments.
! The routine returns the `x1` and `x2` derivatives of every basis
! function at the points in `xij`.
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! See also: [[TensorProdBasisGradient_Quadrangle1]],
! [[OrthogonalBasisGradient_Quadrangle_]]

INTERFACE OrthogonalBasisGradient_Quadrangle
  MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1
END INTERFACE OrthogonalBasisGradient_Quadrangle

!----------------------------------------------------------------------------
!                       TensorProdBasisGradient_Quadrangle@TensorProdMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the anisotropic tensor-product basis
! at a set of points, into a buffer.
!
! Evaluates the `x1` and `x2` derivatives of every tensor-product basis
! function at the points in `xij`.
! The orders are `p` in the `x1` direction and `q` in the `x2` direction.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The points lie in the biunit square `[-1, 1] x [-1, 1]`.
! Each column of `xij` is one point, with `x1` in row 1 and `x2` in row 2.
!
! The columns of `ans` run over the `x1` degree first.
! Then they run over the `x2` degree.
! The derivative with respect to `x1` is `ans(:, :, 1)`.
! The derivative with respect to `x2` is `ans(:, :, 2)`.
!
! @note
! `ans` must be at least `dim1` by `dim2` by 2.
! The routine writes only that part.
! @endnote
!
! @note
! `basisType2` is not used.
! Both directions use `basisType1`.
! Pass the same family in both directions.
! @endnote
!
! Formula:
!   $$ \phi_{ij}(x_1, x_2) = P_i(x_1) Q_j(x_2) $$
!
! Here `i` runs from 0 to `p` and `j` runs from 0 to `q`.
!
! See also: [[TensorProdBasisGradient_Quadrangle1]]

INTERFACE TensorProdBasisGradient_Quadrangle_
  MODULE SUBROUTINE TensorProdBasisGradient_Quadrangle1_(p, q, xij, ans, &
                                                         dim1, dim2, dim3, &
                                                         basisType1, &
                                                         basisType2, alpha1, &
                                                         beta1, &
                                                         lambda1, alpha2, &
                                                         beta2, lambda2)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in the `x2` direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one per column, size `(2, npoints)`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients, written to `ans(1:dim1, 1:dim2, 1:dim3)`
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! `dim1` is `SIZE(xij, 2)`
    !! `dim2` is `(p + 1) * (q + 1)`
    !! `dim3` is 2
    INTEGER(I4B), INTENT(IN) :: basisType1
    !! basis family used in both directions
    !! Monomial, Jacobi, Legendre, Chebyshev, Ultraspherical or
    !! Heirarchical
    INTEGER(I4B), INTENT(IN) :: basisType2
    !! not used, see the note above
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter `alpha` for the `x1` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter `beta` for the `x1` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter for the `x1` direction
    !! needed when `basisType1` is Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter `alpha` for the `x2` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter `beta` for the `x2` direction
    !! needed when `basisType1` is Jacobi
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter for the `x2` direction
    !! needed when `basisType1` is Ultraspherical
  END SUBROUTINE TensorProdBasisGradient_Quadrangle1_
END INTERFACE TensorProdBasisGradient_Quadrangle_

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluates the gradients of the anisotropic tensor-product basis
!   at a set of points, into a buffer. Alias of
!   [[TensorProdBasisGradient_Quadrangle1_]].
!
! This generic name calls [[TensorProdBasisGradient_Quadrangle1_]] with the
! same arguments.
! The routine writes the `x1` and `x2` derivatives of every basis
! function into the buffer `ans` and does not allocate.
! It returns the size of the written block in `dim1`, `dim2` and `dim3`.
!
! @note
! The routine ignores `basisType2`.
! It uses `basisType1` for both directions.
! Pass the same family in both arguments.
! @endnote
!
! See also: [[TensorProdBasisGradient_Quadrangle1_]],
! [[OrthogonalBasisGradient_Quadrangle]]

INTERFACE OrthogonalBasisGradient_Quadrangle_
  MODULE PROCEDURE TensorProdBasisGradient_Quadrangle1_
END INTERFACE OrthogonalBasisGradient_Quadrangle_

!----------------------------------------------------------------------------
!                               QuadratureNumber_Quadrangle@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of quadrature points in each direction of a
! quadrangle.
!
! Returns the point counts in the `x1` and `x2` directions.
! The counts come from the orders `p` and `q` and from the quadrature
! families `quadType1` and `quadType2`.
! The total number of points is the product of the two counts.
!
! Returns:
!   ans : INTEGER(I4B) - `ans(1)` is the count in `x1`, `ans(2)` in `x2`
!
! See also: [[QuadraturePoint_Quadrangle4]]

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Quadrangle(p, q, quadType1, &
                                                   quadType2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q
    !! order of the integrand in the `x1` direction (`p`) and in the `x2`
    !! direction (`q`)
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! quadrature family in the `x1` direction (`quadType1`) and in the
    !! `x2` direction (`quadType2`)
    INTEGER(I4B) :: ans(2)
    !! number of points in the `x1` and `x2` directions
  END FUNCTION QuadratureNumber_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                QuadraturePoint_Quadrangle@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the quadrature points and weights of a quadrangle for
! one order.
!
! Returns the quadrature points and weights for an integrand of order
! `order` in both directions.
! Both directions use the same quadrature family `quadType`.
!
! The result has one column per point.
! The first rows hold the coordinates of the point.
! The last row holds the weight.
! The `x2` index runs fastest along the columns.
!
! If `xij` is present, the points lie in the quadrangle with the four
! vertices in `xij`, and `refQuadrangle` is not used.
! Otherwise `refQuadrangle` selects the reference square.
! The value `"UNIT"` gives `[0, 1] x [0, 1]`.
! Any other value gives the biunit square `[-1, 1] x [-1, 1]`.
! The weights include the scaling of the map from the biunit square.
!
! Returns:
!   ans : REAL(DFP) - allocated, one column per point
!
! Formula:
!   $$ \int_K f \, dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/QuadraturePoint_Quadrangle_test_1.F90)
!
! See also: [[QuadraturePoint_Quadrangle2]], [[QuadraturePoint_Quadrangle3]],
! [[QuadraturePoint_Quadrangle4]]

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle1(order, quadType, &
                                              refQuadrangle, xij, alpha, &
                                              beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the integrand in each direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature family, used in both directions
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! reference quadrangle, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! the four vertices of the quadrangle, one per column
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter `alpha`, used in both directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter `beta`, used in both directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, used in both directions
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, one point per column
  END FUNCTION QuadraturePoint_Quadrangle1
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                QuadraturePoint_Quadrangle@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the quadrature points and weights of a quadrangle for
! two orders.
!
! Returns the quadrature points and weights for an integrand of order `p`
! in the `x1` direction and order `q` in the `x2` direction.
! Each direction has its own quadrature family.
!
! The result has one column per point.
! The first rows hold the coordinates of the point.
! The last row holds the weight.
! The `x2` index runs fastest along the columns.
!
! If `xij` is present, the points lie in the quadrangle with the four
! vertices in `xij`, and `refQuadrangle` is not used.
! Otherwise `refQuadrangle` selects the reference square.
! The value `"UNIT"` gives `[0, 1] x [0, 1]`.
! Any other value gives the biunit square `[-1, 1] x [-1, 1]`.
! The weights include the scaling of the map from the biunit square.
!
! Returns:
!   ans : REAL(DFP) - allocated, one column per point
!
! Formula:
!   $$ \int_K f \, dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/QuadraturePoint_Quadrangle_test_1.F90)
!
! See also: [[QuadraturePoint_Quadrangle1]], [[QuadraturePoint_Quadrangle3]],
! [[QuadraturePoint_Quadrangle4]]

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle2(p, q, quadType1, quadType2, &
                                              refQuadrangle, xij, alpha1, &
                                              beta1, lambda1, alpha2, &
    beta2, lambda2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of the integrand in the `x1` direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of the integrand in the `x2` direction
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! quadrature family in the `x1` direction (`quadType1`) and in the
    !! `x2` direction (`quadType2`)
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! reference quadrangle, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! the four vertices of the quadrangle, one per column
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter `alpha` in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter `beta` in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter `alpha` in the `x2` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter `beta` in the `x2` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter in the `x2` direction
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, one point per column
  END FUNCTION QuadraturePoint_Quadrangle2
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                QuadraturePoint_Quadrangle@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the quadrature points and weights of a quadrangle for
! one point count.
!
! Returns the quadrature points and weights for `nips(1)` points in each
! direction.
! Both directions use the same quadrature family `quadType`.
!
! The result has one column per point.
! The first rows hold the coordinates of the point.
! The last row holds the weight.
! The `x2` index runs fastest along the columns.
!
! If `xij` is present, the points lie in the quadrangle with the four
! vertices in `xij`, and `refQuadrangle` is not used.
! Otherwise `refQuadrangle` selects the reference square.
! The value `"UNIT"` gives `[0, 1] x [0, 1]`.
! Any other value gives the biunit square `[-1, 1] x [-1, 1]`.
! The weights include the scaling of the map from the biunit square.
!
! Returns:
!   ans : REAL(DFP) - allocated with `nips(1) * nips(1)` columns
!
! Formula:
!   $$ \int_K f \, dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/QuadraturePoint_Quadrangle_test_1.F90)
!
! See also: [[QuadraturePoint_Quadrangle1]], [[QuadraturePoint_Quadrangle2]],
! [[QuadraturePoint_Quadrangle4]]

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle3(nips, quadType, &
                                              refQuadrangle, xij, alpha, &
                                              beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of points in each direction, an array of size 1
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature family, used in both directions
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! reference quadrangle, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! the four vertices of the quadrangle, one per column
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter `alpha`, used in both directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter `beta`, used in both directions
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, used in both directions
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, one point per column
  END FUNCTION QuadraturePoint_Quadrangle3
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                                QuadraturePoint_Quadrangle@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the quadrature points and weights of a quadrangle for
! two point counts.
!
! Returns the quadrature points and weights for `nipsx(1)` points in the
! `x1` direction and `nipsy(1)` points in the `x2` direction.
! Each direction has its own quadrature family.
!
! The result has one column per point.
! The first rows hold the coordinates of the point.
! The last row holds the weight.
! The `x2` index runs fastest along the columns.
!
! If `xij` is present, the points lie in the quadrangle with the four
! vertices in `xij`, and `refQuadrangle` is not used.
! Otherwise `refQuadrangle` selects the reference square.
! The value `"UNIT"` gives `[0, 1] x [0, 1]`.
! Any other value gives the biunit square `[-1, 1] x [-1, 1]`.
! The weights include the scaling of the map from the biunit square.
!
! Returns:
!   ans : REAL(DFP) - allocated with `nipsx(1) * nipsy(1)` columns
!
! Formula:
!   $$ \int_K f \, dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/QuadraturePoint_Quadrangle_test_1.F90)
!
! See also: [[QuadraturePoint_Quadrangle1]], [[QuadraturePoint_Quadrangle2]],
! [[QuadraturePoint_Quadrangle3]]

INTERFACE QuadraturePoint_Quadrangle
  MODULE FUNCTION QuadraturePoint_Quadrangle4(nipsx, nipsy, quadType1, &
                                              quadType2, refQuadrangle, xij, &
                                              alpha1, beta1, lambda1, &
                                              alpha2, beta2, &
                                              lambda2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of points in the `x1` direction, an array of size 1
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of points in the `x2` direction, an array of size 1
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! quadrature family in the `x1` direction (`quadType1`) and in the
    !! `x2` direction (`quadType2`).
    !! Examples are `Equidistance`, `GaussLegendre`, `GaussLegendreLobatto`,
    !! `GaussChebyshev1`, `GaussUltraspherical` and `GaussJacobi`.
    !! The Lobatto and Radau variants of each Gauss family also work
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! reference quadrangle, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! the four vertices of the quadrangle, one per column
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter `alpha` in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter `beta` in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter `alpha` in the `x2` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter `beta` in the `x2` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter in the `x2` direction
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, one point per column
  END FUNCTION QuadraturePoint_Quadrangle4
END INTERFACE QuadraturePoint_Quadrangle

!----------------------------------------------------------------------------
!                               QuadraturePoint_Quadrangle@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the quadrature points and weights of a quadrangle,
! into a buffer.
!
! Computes the quadrature points and weights for `nipsx(1)` points in the
! `x1` direction and `nipsy(1)` points in the `x2` direction.
! Each direction has its own quadrature family.
! The result goes into the preallocated array `ans`.
! The routine does not allocate.
!
! The result has one column per point.
! The first rows hold the coordinates of the point.
! The last row holds the weight.
! The `x2` index runs fastest along the columns.
!
! If `xij` is present, the points lie in the quadrangle with the four
! vertices in `xij`, and `refQuadrangle` is not used.
! Otherwise `refQuadrangle` selects the reference square.
! The value `"UNIT"` gives `[0, 1] x [0, 1]`.
! Any other value gives the biunit square `[-1, 1] x [-1, 1]`.
! The weights include the scaling of the map from the biunit square.
!
! @note
! `ans` must have at least `nsd + 1` rows and `nipsx(1) * nipsy(1)`
! columns.
! Here `nsd` is `MAX(SIZE(xij, 1), 2)` if `xij` is present, else 2.
! @endnote
!
! Formula:
!   $$ \int_K f \, dx \simeq \sum_i w_i f(x_i) $$
!
! See also: [[QuadraturePoint_Quadrangle4]]

INTERFACE QuadraturePoint_Quadrangle_
  MODULE SUBROUTINE QuadraturePoint_Quadrangle1_( &
    nipsx, nipsy, quadType1, quadType2, refQuadrangle, xij, alpha1, beta1, &
    lambda1, alpha2, beta2, lambda2, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of points in the `x1` direction, an array of size 1
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of points in the `x2` direction, an array of size 1
    INTEGER(I4B), INTENT(IN) :: quadType1, quadType2
    !! quadrature family in the `x1` direction (`quadType1`) and in the
    !! `x2` direction (`quadType2`).
    !! Examples are `Equidistance`, `GaussLegendre`, `GaussLegendreLobatto`,
    !! `GaussChebyshev1`, `GaussUltraspherical` and `GaussJacobi`.
    !! The Lobatto and Radau variants of each Gauss family also work
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! reference quadrangle, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! the four vertices of the quadrangle, one per column
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! Jacobi parameter `alpha` in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! Jacobi parameter `beta` in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! ultraspherical parameter in the `x1` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! Jacobi parameter `alpha` in the `x2` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! Jacobi parameter `beta` in the `x2` direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! ultraspherical parameter in the `x2` direction
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! points and weights, written to `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
  END SUBROUTINE QuadraturePoint_Quadrangle1_
END INTERFACE QuadraturePoint_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE QuadrangleInterpolationUtility
