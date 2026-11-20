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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the name of the reference domain of a line element.
!
! The reference line is the interval from -1 to 1. Its name is
! `"BIUNIT"`. The result is the same for every basis family.
!
! @note
! `baseContinuity` and `baseInterpol` are not used.
! @endnote
!
! Returns:
!   ans : TYPE(String) - always `"BIUNIT"`
!
! Examples
!
! - [test 1](|base|Line/examples/RefElemDomain_Line_test_1.F90)

INTERFACE
  MODULE FUNCTION RefElemDomain_Line(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, such as `"H1"`, `"HDiv"`, `"HCurl"`
    !! or `"DG"`; not used
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! basis family, such as `"Lagrange"` or `"Hierarchy"`; not used
    TYPE(String) :: ans
    !! name of the reference domain
  END FUNCTION RefElemDomain_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      QuadratureNumber_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of quadrature points needed on a line.
!
! The result is the smallest point count that integrates a polynomial of
! degree `order` exactly. The count depends on `quadType`.
!
! 1. For the Gauss rules (Legendre, Chebyshev, Jacobi and
!    Ultraspherical), the result is `1 + order / 2`.
! 2. For the Gauss-Radau rules, left or right, the result is
!    `2 + (order - 1) / 2`.
! 3. For any other `quadType`, such as a Lobatto rule, the result is
!    `2 + order / 2`.
!
! The divisions are integer divisions.
!
! Formula:
!   $$ n = 1 + \lfloor p/2 \rfloor $$
!
! for the first case, where `p` is `order`.
!
! Examples
!
! - [test 1](|base|Line/examples/QuadratureNumber_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:QuadraturePoint_Line]]

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Line(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree to integrate exactly
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature type, a value of `TypeQuadratureOpt`
    INTEGER(I4B) :: ans
    !! number of quadrature points
  END FUNCTION QuadratureNumber_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                            ToVEFC_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorders line points so the end points come first.
!
! The input holds points in increasing order along the line.
! This routine moves the last point to the second place.
! The interior points shift by one place. The first point stays.
! The result lists the two end points, then the interior points.
! This is the vertex, edge, face, cell (VEFC) order.
!
! If `pt` has two points or fewer, it does not change.
!
! Formula:
!   $$ [x_1, x_2, \ldots, x_N] \mapsto [x_1, x_N, x_2, \ldots, x_{N-1}] $$
!
! Examples
!
! - [test 1](|base|Line/examples/ToVEFC_Line_test_1.F90)

INTERFACE
  MODULE PURE SUBROUTINE ToVEFC_Line(pt)
    REAL(DFP), INTENT(INOUT) :: pt(:)
    !! points in increasing order on input, in VEFC order on output
  END SUBROUTINE ToVEFC_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial exponents of a Lagrange line basis.
!
! The monomial basis on a line is `1, x, x^2, ..., x^p`, where `p` is
! `order`. Row `i` of the result holds the exponent of the `i`-th
! monomial. The result has `order + 1` rows and one column.
!
! Formula:
!   $$ ans(i, 1) = i - 1, \quad i = 1, \ldots, p + 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeDegree_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:LagrangeDOF_Line]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! exponents, size is `(order + 1, 1)`
  END FUNCTION LagrangeDegree_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of Lagrange DOFs of a point.
!
! A point always has one DOF. The result is 1 for every `order`.
!
! Returns:
!   ans : INTEGER(I4B) - always 1
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeDOF_Point_test_1.F90)
!
! See also: [[LineInterpolationUtility:LagrangeDOF_Line]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Point(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis, not used
    INTEGER(I4B) :: ans
    !! number of DOFs, always 1
  END FUNCTION LagrangeDOF_Point
END INTERFACE

!----------------------------------------------------------------------------
!                                                            LagrangeDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the total number of Lagrange DOFs of a line element.
!
! The total counts the DOFs at the two end points and in the interior.
! The result is `order + 1`.
!
! Returns:
!   ans : INTEGER(I4B) - total number of DOFs
!
! Formula:
!   $$ N = p + 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeDOF_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:LagrangeInDOF_Line]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    INTEGER(I4B) :: ans
    !! total number of DOFs, equal to `order + 1`
  END FUNCTION LagrangeDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior Lagrange DOFs of a line element.
!
! The interior DOFs of a line are those not attached to its two end
! points. The result is `order - 1`.
!
! @warning
! For `order` below 1 the result is zero or negative. It is not clipped.
! Use [[LineInterpolationUtility:GetTotalInDOF_Line]] for a result that
! is never negative.
! @endwarning
!
! Returns:
!   ans : INTEGER(I4B) - number of interior DOFs
!
! Formula:
!   $$ N_{in} = p - 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeInDOF_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:LagrangeDOF_Line]]

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    INTEGER(I4B) :: ans
    !! number of interior DOFs, equal to `order - 1`
  END FUNCTION LagrangeInDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetTotalDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the total number of DOFs of a line element.
!
! The total counts the DOFs at the two end points and in the interior.
! The result is `order + 1`.
!
! @note
! `baseContinuity` and `baseInterpolation` are not used. Every basis
! family gets the same count.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - total number of DOFs
!
! Formula:
!   $$ N = p + 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/GetTotalDOF_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:LagrangeDOF_Line]],
! [[LineInterpolationUtility:GetTotalInDOF_Line]]

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Line(order, baseContinuity, &
                                        baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis family, not used
    INTEGER(I4B) :: ans
    !! total number of DOFs
  END FUNCTION GetTotalDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetTotalInDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior DOFs of a line element.
!
! The interior DOFs of a line are those not attached to its two end
! points. The result is `order - 1`, and it is never negative.
! If `order` is 1 or less, the result is 0.
!
! @note
! `baseContinuity` and `baseInterpolation` are not used. Every basis
! family gets the same count.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - number of interior DOFs
!
! Formula:
!   $$ N_{in} = \max(p - 1, 0) $$
!
! Examples
!
! - [test 1](|base|Line/examples/GetTotalInDOF_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:LagrangeInDOF_Line]],
! [[LineInterpolationUtility:GetTotalDOF_Line]]

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Line(order, baseContinuity, &
                                          baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis family, not used
    INTEGER(I4B) :: ans
    !! number of interior DOFs
  END FUNCTION GetTotalInDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns equally spaced interior points of a line.
!
! This function returns the interior points of a line segment, in
! increasing distance from the first end point.
! The end points are not included.
! The points divide the segment into `order` equal parts.
!
! If `order` is 1 or less, the result is an empty array.
! Otherwise it has `order - 1` entries.
!
! Formula:
!   $$ x_j = x_1 + j\,\frac{x_2 - x_1}{p}, \quad j = 1, \ldots, p-1 $$
!
! where `p` is `order`, `x_1` is `xij(1)` and `x_2` is `xij(2)`.
!
! Examples
!
! - [test 2](|base|Line/examples/EquidistanceInPoint_Line_test_2.F90)
!
! See also: [[LineInterpolationUtility:EquidistanceInPoint_Line1_]]

INTERFACE EquidistanceInPoint_Line
  MODULE PURE FUNCTION EquidistanceInPoint_Line1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of the first and the second end point
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! interior points, size is `MAX(order - 1, 0)`
  END FUNCTION EquidistanceInPoint_Line1
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                  EquidistanceInPoint_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes equally spaced interior points of a line into a buffer.
!
! This subroutine writes the interior points of a line segment into `ans`.
! The points run from the first end point towards the second.
! The end points are not included.
! The points divide the segment into `order` equal parts.
!
! If `order` is 1 or less, it writes nothing and sets `tsize` to 0.
! Otherwise it writes `order - 1` entries and sets `tsize` to `order - 1`.
!
! @note
! `ans` must hold at least `order - 1` entries.
! The routine does not allocate it.
! @endnote
!
! Formula:
!   $$ x_j = x_1 + j\,\frac{x_2 - x_1}{p}, \quad j = 1, \ldots, p-1 $$
!
! where `p` is `order`, `x_1` is `xij(1)` and `x_2` is `xij(2)`.
!
! Examples
!
! - [test 2](|base|Line/examples/EquidistanceInPoint_Line_test_2.F90)
!
! See also: [[LineInterpolationUtility:EquidistanceInPoint_Line1]]

INTERFACE EquidistanceInPoint_Line_
  MODULE PURE SUBROUTINE EquidistanceInPoint_Line1_(order, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of the first and the second end point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interior points, the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of interior points written to `ans`, `MAX(order - 1, 0)`
  END SUBROUTINE EquidistanceInPoint_Line1_
END INTERFACE EquidistanceInPoint_Line_

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns equally spaced interior points of a line in `xij` format.
!
! This function returns the interior points of a line segment.
! Each column of `ans` is one point.
! The points run from the first end point towards the second.
! The end points are not included.
! The points divide the segment into `order` equal parts.
!
! If `xij` is absent, the segment runs from -1 to 1 and `ans` has one row.
! Otherwise `ans` has as many rows as `xij`.
!
! If `order` is 1 or less, `ans` has no rows and no columns.
! Otherwise `ans` has `order - 1` columns.
!
! @note
! `xij` can have at most 3 rows.
! @endnote
!
! Formula:
!   $$ x_j = x_1 + j\,\frac{x_2 - x_1}{p}, \quad j = 1, \ldots, p-1 $$
!
! where `p` is `order`, and `x_1` and `x_2` are the two columns of `xij`.
! The formula applies to each row.
!
! Examples
!
! - [test 1](|base|Line/examples/EquidistanceInPoint_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:EquidistanceInPoint_Line2_]]

INTERFACE EquidistanceInPoint_Line
  MODULE PURE FUNCTION EquidistanceInPoint_Line2(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of the two end points, one point per column.
    !! The number of rows is the number of space dimensions.
    !! The number of columns is 2.
    !! The default is a single row with the values -1 and 1.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interior points, one point per column.
    !! The number of rows is the number of rows of `xij`.
    !! It is 1 if `xij` is absent.
    !! The number of columns is `MAX(order - 1, 0)`.
  END FUNCTION EquidistanceInPoint_Line2
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                  EquidistanceInPoint_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes equally spaced interior points of a line into a matrix.
!
! This subroutine writes the interior points of a line segment into `ans`.
! Each column of `ans` is one point.
! The points run from the first end point towards the second.
! The end points are not included.
! The points divide the segment into `order` equal parts.
!
! If `xij` is absent, the segment runs from -1 to 1 and `nrow` is 1.
! Otherwise `nrow` is the number of rows of `xij`.
!
! If `order` is 1 or less, it writes nothing and sets `nrow` and `ncol`
! to 0. Otherwise it sets `ncol` to `order - 1`.
!
! @note
! `ans` must have at least `nrow` rows and `order - 1` columns.
! The routine does not allocate it.
! @endnote
!
! @note
! `xij` can have at most 3 rows.
! @endnote
!
! Formula:
!   $$ x_j = x_1 + j\,\frac{x_2 - x_1}{p}, \quad j = 1, \ldots, p-1 $$
!
! where `p` is `order`, and `x_1` and `x_2` are the two columns of `xij`.
! The formula applies to each row.
!
! Examples
!
! - [test 1](|base|Line/examples/EquidistanceInPoint_Line__test_1.F90)
!
! See also: [[LineInterpolationUtility:EquidistanceInPoint_Line2]]

INTERFACE EquidistanceInPoint_Line_
  MODULE PURE SUBROUTINE EquidistanceInPoint_Line2_(order, xij, ans, &
                                                    nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of the two end points, one point per column.
    !! The number of rows is the number of space dimensions.
    !! The number of columns is 2.
    !! The default is a single row with the values -1 and 1.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interior points, one point per column
    !! the routine writes the first `nrow` rows and `ncol` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `ncol` is `MAX(order - 1, 0)`
  END SUBROUTINE EquidistanceInPoint_Line2_
END INTERFACE EquidistanceInPoint_Line_

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns equally spaced points on a line, including the end points.
!
! This function returns `order + 1` equally spaced points on a line
! segment. The end points come first, then the interior points.
!
! 1. `ans(1)` is `xij(1)`.
! 2. `ans(2)` is `xij(2)`.
! 3. The remaining entries are the interior points. They run from the
!    first end point towards the second.
!
! If `order` is 0, `ans` has one entry. It is the midpoint of the segment.
!
! Examples
!
! - [test 2](|base|Line/examples/EquidistancePoint_Line_test_2.F90)
!
! See also: [[LineInterpolationUtility:EquidistanceInPoint_Line1_]]

INTERFACE EquidistancePoint_Line
  MODULE PURE FUNCTION EquidistancePoint_Line1(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of the first and the second end point
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! the points, size is `order + 1`
  END FUNCTION EquidistancePoint_Line1
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistancePoint_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes equally spaced points of a line into a buffer.
!
! This subroutine writes `order + 1` equally spaced points of a line
! segment into `ans`. The end points come first, then the interior points.
!
! 1. `ans(1)` is `xij(1)`.
! 2. `ans(2)` is `xij(2)`.
! 3. The remaining entries are the interior points. They run from the
!    first end point towards the second.
!
! If `order` is 0, it writes one entry. It is the midpoint of the segment.
!
! @note
! `ans` must hold at least `order + 1` entries.
! The routine does not allocate it.
! @endnote
!
! Examples
!
! - [test 2](|base|Line/examples/EquidistancePoint_Line_test_2.F90)
!
! See also: [[LineInterpolationUtility:EquidistancePoint_Line1]]

INTERFACE EquidistancePoint_Line_
  MODULE PURE SUBROUTINE EquidistancePoint_Line1_(order, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of the first and the second end point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! the points, the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of points written to `ans`, equal to `order + 1`
  END SUBROUTINE EquidistancePoint_Line1_
END INTERFACE EquidistancePoint_Line_

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns equally spaced points of a line in `xij` format.
!
! This function returns `order + 1` equally spaced points on a line
! segment. Each column of `ans` is one point.
! The end points come first, then the interior points.
!
! 1. The first column is the first end point.
! 2. The second column is the second end point.
! 3. The remaining columns are the interior points. They run from the
!    first end point towards the second.
!
! If `xij` is absent, the segment runs from -1 to 1 and `ans` has one row.
! Otherwise `ans` has as many rows as `xij`.
!
! If `order` is 0, `ans` has one column. It is the midpoint of the segment.
! The midpoint is 0 if `xij` is absent.
!
! @note
! `xij` can have at most 3 rows.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/EquidistancePoint_Line_test_1.F90)
!
! See also: [[LineInterpolationUtility:EquidistancePoint_Line2_]]

INTERFACE EquidistancePoint_Line
  MODULE PURE FUNCTION EquidistancePoint_Line2(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of the two end points, one point per column.
    !! The number of rows is the number of space dimensions.
    !! The number of columns is 2.
    !! The default is a single row with the values -1 and 1.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! the points, one point per column.
    !! The number of rows is the number of rows of `xij`.
    !! It is 1 if `xij` is absent.
    !! The number of columns is `order + 1`.
  END FUNCTION EquidistancePoint_Line2
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistancePoint_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes equally spaced points of a line into a matrix.
!
! This subroutine writes `order + 1` equally spaced points of a line
! segment into `ans`. Each column of `ans` is one point.
! The end points come first, then the interior points.
!
! 1. The first column is the first end point.
! 2. The second column is the second end point.
! 3. The remaining columns are the interior points. They run from the
!    first end point towards the second.
!
! If `xij` is absent, the segment runs from -1 to 1 and `nrow` is 1.
! Otherwise `nrow` is the number of rows of `xij`.
!
! If `order` is 0, it writes one column. It is the midpoint of the segment.
! The midpoint is 0 if `xij` is absent.
!
! @note
! `ans` must have at least `nrow` rows and `order + 1` columns.
! The routine does not allocate it.
! @endnote
!
! @note
! `xij` can have at most 3 rows.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/EquidistancePoint_Line__test_1.F90)
!
! See also: [[LineInterpolationUtility:EquidistancePoint_Line2]]

INTERFACE EquidistancePoint_Line_
  MODULE PURE SUBROUTINE EquidistancePoint_Line2_(order, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! interpolation order, the number of segments between the end points
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of the two end points, one point per column.
    !! The number of rows is the number of space dimensions.
    !! The number of columns is 2.
    !! The default is a single row with the values -1 and 1.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! the points, one point per column
    !! the routine writes the first `nrow` rows and `ncol` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `ncol` is `order + 1`
  END SUBROUTINE EquidistancePoint_Line2_
END INTERFACE EquidistancePoint_Line_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns interpolation points of a line in `xij` format.
!
! This function returns `order + 1` interpolation points on a line segment.
! Each column of `ans` is one point.
! The argument `ipType` selects the family of points.
! The argument `layout` selects the order of the points.
!
! The families are `Equidistance`, `GaussLegendre`, `GaussChebyshev`,
! `GaussJacobi`, `GaussUltraspherical` and the Lobatto form of the last
! four, such as `GaussLegendreLobatto`.
! The Lobatto families include the end points. The Gauss families do not.
!
! The layout is `"VEFC"` or `"INCREASING"`.
! The routine reads only the first letter.
!
! - `"VEFC"` puts the two end points first, then the interior points in
!   increasing order. It applies to `Equidistance` and the Lobatto families.
! - `"INCREASING"` sorts the points in increasing order.
!   It applies to `Equidistance` and the Lobatto families.
!
! The Gauss families ignore `layout`.
!
! If `xij` is absent, the segment runs from -1 to 1 and `ans` has one row.
! Otherwise `ans` has as many rows as `xij`.
! If `order` is 0, `ans` is the midpoint of the segment.
! In that case `ipType` and `layout` have no effect.
!
! @note
! `GaussJacobi` and `GaussJacobiLobatto` need `alpha` and `beta`.
! The ultraspherical families need `lambda`.
! Debug builds check this. Release builds do not.
! @endnote
!
! @warning
! The Gauss and Lobatto families support at most 64 points.
! @endwarning
!
! Examples
!
! - [test 1](|base|Line/examples/InterpolationPoint_Line_test_1.F90)
! - [test 2](|base|Line/examples/InterpolationPoint_Line_test_2.F90) with
!   a two-row `xij`
!
! See also: [[LineInterpolationUtility:InterpolationPoint_Line1_]]

INTERFACE InterpolationPoint_Line
  MODULE FUNCTION InterpolationPoint_Line1( &
    order, ipType, layout, xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation, the number of points is `order + 1`
    INTEGER(I4B), INTENT(IN) :: ipType
    !! type of interpolation points
    !! `Equidistance`, `GaussLegendre`, `GaussLegendreLobatto`,
    !! `GaussChebyshev`, `GaussChebyshevLobatto`, `GaussJacobi`,
    !! `GaussJacobiLobatto`, `GaussUltraspherical`,
    !! `GaussUltrasphericalLobatto`
    CHARACTER(*), INTENT(IN) :: layout
    !! order of the points, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of the two end points of the segment, one per column.
    !! The number of rows is the number of space dimensions.
    !! The number of columns is 2.
    !! The default is a single row with the values -1 and 1.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed by `GaussJacobi` and `GaussJacobiLobatto`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed by `GaussJacobi` and `GaussJacobiLobatto`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed by the ultraspherical families
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points, one point per column.
    !! The number of rows is the number of rows of `xij`.
    !! It is 1 if `xij` is absent.
    !! The number of columns is `order + 1`.
  END FUNCTION InterpolationPoint_Line1
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes interpolation points of a line into a matrix.
!
! This subroutine writes `order + 1` interpolation points of a line segment
! into `ans`. Each column of `ans` is one point.
! The argument `ipType` selects the family of points.
! The argument `layout` selects the order of the points.
!
! The families are `Equidistance`, `GaussLegendre`, `GaussChebyshev`,
! `GaussJacobi`, `GaussUltraspherical` and the Lobatto form of the last
! four, such as `GaussLegendreLobatto`.
! The Lobatto families include the end points. The Gauss families do not.
!
! The layout is `"VEFC"` or `"INCREASING"`.
! The routine reads only the first letter.
!
! - `"VEFC"` puts the two end points first, then the interior points in
!   increasing order. It applies to `Equidistance` and the Lobatto families.
! - `"INCREASING"` sorts the points in increasing order.
!   It applies to `Equidistance` and the Lobatto families.
!
! The Gauss families ignore `layout`.
!
! If `xij` is absent, the segment runs from -1 to 1 and `nrow` is 1.
! Otherwise `nrow` is the number of rows of `xij`.
! If `order` is 0, it writes the midpoint of the segment.
! In that case `ipType` and `layout` have no effect.
!
! @note
! `ans` must have at least `nrow` rows and `order + 1` columns.
! The routine does not allocate it.
! @endnote
!
! @note
! `GaussJacobi` and `GaussJacobiLobatto` need `alpha` and `beta`.
! The ultraspherical families need `lambda`.
! Debug builds check this. Release builds do not.
! @endnote
!
! @warning
! The Gauss and Lobatto families support at most 64 points.
! @endwarning
!
! Examples
!
! - [test 1](|base|Line/examples/InterpolationPoint_Line__test_1.F90)
!
! See also: [[LineInterpolationUtility:InterpolationPoint_Line1]]

INTERFACE InterpolationPoint_Line_
  MODULE SUBROUTINE InterpolationPoint_Line1_( &
    order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation, the number of points is `order + 1`
    INTEGER(I4B), INTENT(IN) :: ipType
    !! type of interpolation points
    !! `Equidistance`, `GaussLegendre`, `GaussLegendreLobatto`,
    !! `GaussChebyshev`, `GaussChebyshevLobatto`, `GaussJacobi`,
    !! `GaussJacobiLobatto`, `GaussUltraspherical`,
    !! `GaussUltrasphericalLobatto`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation points, one point per column
    !! the routine writes the first `nrow` rows and `ncol` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `ncol` is `order + 1`
    CHARACTER(*), INTENT(IN) :: layout
    !! order of the points, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of the two end points of the segment, one per column.
    !! The number of rows is the number of space dimensions.
    !! The number of columns is 2.
    !! The default is a single row with the values -1 and 1.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed by `GaussJacobi` and `GaussJacobiLobatto`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed by `GaussJacobi` and `GaussJacobiLobatto`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed by the ultraspherical families
  END SUBROUTINE InterpolationPoint_Line1_
END INTERFACE InterpolationPoint_Line_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns interpolation points of a line as a vector.
!
! This function returns `order + 1` interpolation points on a line segment
! from `xij(1)` to `xij(2)`.
! The argument `ipType` selects the family of points.
! The argument `layout` selects the order of the points.
!
! The families are `Equidistance`, `GaussLegendre`, `GaussChebyshev`,
! `GaussJacobi`, `GaussUltraspherical` and the Lobatto form of the last
! four, such as `GaussLegendreLobatto`.
! The Lobatto families include the end points. The Gauss families do not.
!
! The layout is `"VEFC"` or `"INCREASING"`.
! The routine reads only the first two letters.
!
! - `"VEFC"` puts the two end points first, then the interior points in
!   increasing order. It applies to `Equidistance` and the Lobatto families.
! - `"INCREASING"` sorts the points in increasing order.
!   It applies to `Equidistance` and the Lobatto families.
!
! The Gauss families ignore `layout`.
! If `order` is 0, `ans` is the midpoint of the segment.
! In that case `ipType` and `layout` have no effect.
!
! @note
! `layout` must have at least two characters.
! @endnote
!
! @note
! `GaussJacobi` and `GaussJacobiLobatto` need `alpha` and `beta`.
! The ultraspherical families need `lambda`.
! Debug builds check this. Release builds do not.
! @endnote
!
! Examples
!
! - [test 2](|base|Line/examples/InterpolationPoint_Line_test_2.F90)
!
! See also: [[LineInterpolationUtility:InterpolationPoint_Line2_]]

INTERFACE InterpolationPoint_Line
  MODULE FUNCTION InterpolationPoint_Line2( &
    order, ipType, xij, layout, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation, the number of points is `order + 1`
    INTEGER(I4B), INTENT(IN) :: ipType
    !! type of interpolation points
    !! `Equidistance`, `GaussLegendre`, `GaussLegendreLobatto`,
    !! `GaussChebyshev`, `GaussChebyshevLobatto`, `GaussJacobi`,
    !! `GaussJacobiLobatto`, `GaussUltraspherical`,
    !! `GaussUltrasphericalLobatto`
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of the first and the second end point
    CHARACTER(*), INTENT(IN) :: layout
    !! order of the points, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed by `GaussJacobi` and `GaussJacobiLobatto`
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed by `GaussJacobi` and `GaussJacobiLobatto`
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed by the ultraspherical families
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! interpolation points, size is `order + 1`
  END FUNCTION InterpolationPoint_Line2
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes one dimensional interpolation points of a line.
!
! Writes `order + 1` interpolation points into the vector `ans`.
! The points lie on the segment between the two end points in `xij`.
!
! 1. If `order` is 0, it writes the midpoint of the segment.
! 2. Otherwise it picks the points of the family given by `ipType`.
! 3. It maps the points to the segment.
!
! `layout` sets the order of the points.
! The routine reads only its first two letters.
!
! - With `Equidistance`, `"INCREASING"` sorts the points in ascending
!   order. Any other layout puts the two end points first.
! - With a Lobatto family, `"VEFC"` puts the two end points first.
!   Any other layout puts the end points first and last.
! - With the other Gauss families, `layout` has no effect.
!
! @note
! `ans` must hold at least `order + 1` entries.
! The routine does not allocate it.
! @endnote
!
! @warning
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
! Only a debug build checks that they are present.
! @endwarning
!
! Examples
!
! - [test 2](|base|Line/examples/InterpolationPoint_Line_test_2.F90)
!
! See also: [[InterpolationPoint_Line2]]

INTERFACE InterpolationPoint_Line_
  MODULE SUBROUTINE InterpolationPoint_Line2_( &
    order, ipType, ans, tsize, xij, layout, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation, the number of points is `order + 1`
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type, see `TypeInterpolationOpt`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! points, the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of points written to `ans`, equal to `order + 1`
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of the first and the second end point
    CHARACTER(*), INTENT(IN) :: layout
    !! point order: `"VEFC"`, `"INCREASING"` or `"DECREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi families
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi families
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical families
  END SUBROUTINE InterpolationPoint_Line2_
END INTERFACE InterpolationPoint_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of one Lagrange polynomial of a line.
!
! Returns the coefficients of one Lagrange polynomial of a line.
! The polynomial uses the monomial basis `1, x, x**2, ...`.
! It is 1 at the `i`th point of `xij` and 0 at the other points.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - coefficients, size `order + 1`
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V c = e_i $$
!   $$ \ell_i(x) = \sum_k c_k x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line1_]]

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(xij, 2) - 1`
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `order + 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, `SIZE(xij, 2) = order + 1`
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line1
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the coefficients of one Lagrange polynomial of a line.
!
! Computes the coefficients of one Lagrange polynomial of a line.
! It writes them into the vector `ans`.
! The polynomial uses the monomial basis `1, x, x**2, ...`.
! It is 1 at the `i`th point of `xij` and 0 at the other points.
!
! @note
! `ans` must hold at least `order + 1` entries.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! @endnote
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V c = e_i $$
!   $$ \ell_i(x) = \sum_k c_k x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line1]]

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line1_(order, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(xij, 2) - 1`
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `order + 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, `SIZE(xij, 2) = order + 1`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to `ans`, equal to `order + 1`
  END SUBROUTINE LagrangeCoeff_Line1_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns one Lagrange polynomial of a line, from a Vandermonde
!   matrix.
!
! Returns the coefficients of one Lagrange polynomial of a line.
! The polynomial uses the monomial basis `1, x, x**2, ...`.
! It is 1 at the `i`th point and 0 at the other points.
!
! The caller gives the Vandermonde matrix `v` of the points.
! The routine does not change `v`.
!
! @note
! The routine never reads `isVandermonde`.
! The argument only tells this overload from the others.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - coefficients, size `order + 1`
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V c = e_i $$
!   $$ \ell_i(x) = \sum_k c_k x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line2_]]

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line2(order, i, v, isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(v, 2) - 1`
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `order + 1`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix, size `(order + 1, order + 1)`
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! not used, it only resolves the generic interface
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line2
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes one Lagrange polynomial of a line, from a Vandermonde
!   matrix.
!
! Computes the coefficients of one Lagrange polynomial of a line.
! It writes them into the vector `ans`.
! The polynomial uses the monomial basis `1, x, x**2, ...`.
! It is 1 at the `i`th point and 0 at the other points.
!
! The caller gives the Vandermonde matrix `v` of the points.
! The routine does not change `v`.
!
! @note
! `ans` must hold at least `order + 1` entries.
! The routine does not allocate it.
! @endnote
!
! @note
! The routine never reads `isVandermonde`.
! The argument only tells this overload from the others.
! @endnote
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V c = e_i $$
!   $$ \ell_i(x) = \sum_k c_k x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line2]]

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line2_(order, i, v, isVandermonde, ans, &
                                         tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(v, 2) - 1`
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `order + 1`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix, size `(order + 1, order + 1)`
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! not used, it only resolves the generic interface
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to `ans`, equal to `order + 1`
  END SUBROUTINE LagrangeCoeff_Line2_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns one Lagrange polynomial of a line from LU factors.
!
! Returns the coefficients of one Lagrange polynomial of a line.
! The polynomial uses the monomial basis `1, x, x**2, ...`.
! It is 1 at the `i`th point and 0 at the other points.
!
! The caller gives the LU factors of the Vandermonde matrix, with the
! pivots, as returned by `GetLU`.
! This saves the factorization when you need many polynomials.
!
! Returns:
!   ans : REAL(DFP) - coefficients, size `order + 1`
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V c = e_i $$
!   $$ \ell_i(x) = \sum_k c_k x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line3_]]

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(v, 2) - 1`
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `order + 1`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix, size `(order + 1, order + 1)`
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices of the factorization, as returned by `GetLU`
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line3
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes one Lagrange polynomial of a line from LU factors.
!
! Computes the coefficients of one Lagrange polynomial of a line.
! It writes them into the vector `ans`.
! The polynomial uses the monomial basis `1, x, x**2, ...`.
! It is 1 at the `i`th point and 0 at the other points.
!
! The caller gives the LU factors of the Vandermonde matrix, with the
! pivots, as returned by `GetLU`.
! This saves the factorization when you need many polynomials.
!
! @note
! `ans` must hold at least `order + 1` entries.
! The routine does not allocate it.
! @endnote
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V c = e_i $$
!   $$ \ell_i(x) = \sum_k c_k x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line3]]

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line3_(order, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(v, 2) - 1`
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `order + 1`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix, size `(order + 1, order + 1)`
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices of the factorization, as returned by `GetLU`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to `ans`, equal to `order + 1`
  END SUBROUTINE LagrangeCoeff_Line3_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of all Lagrange polynomials of a line.
!
! Returns the coefficients of all Lagrange polynomials of a line.
! The polynomials use the monomial basis `1, x, x**2, ...`.
! Column `j` of `ans` holds the coefficients of the `j`th polynomial.
! This polynomial is 1 at the `j`th point of `xij` and 0 at the others.
!
! The result is the inverse of the Vandermonde matrix of the points.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - square matrix of size `order + 1`
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V C = I $$
!   $$ \ell_j(x) = \sum_k C_{kj} x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line4_]], [[LagrangeCoeff_Line5]]

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line4(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, `SIZE(xij, 2) = order + 1`
    REAL(DFP) :: ans(order + 1, order + 1)
    !! coefficients, column `j` belongs to the Lagrange polynomial
    !! of the `j`th point
  END FUNCTION LagrangeCoeff_Line4
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the coefficients of all Lagrange polynomials of a line.
!
! Computes the coefficients of all Lagrange polynomials of a line.
! It writes them into the matrix `ans`.
! The polynomials use the monomial basis `1, x, x**2, ...`.
! Column `j` of `ans` holds the coefficients of the `j`th polynomial.
! This polynomial is 1 at the `j`th point of `xij` and 0 at the others.
!
! The result is the inverse of the Vandermonde matrix of the points.
!
! @note
! `ans` must hold at least `order + 1` rows and `order + 1` columns.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! @endnote
!
! Formula:
!   $$ V_{ik} = x_i^{k-1}, \quad V C = I $$
!   $$ \ell_j(x) = \sum_k C_{kj} x^{k-1} $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeCoeff_Line_test_2.F90)
!
! See also: [[LagrangeCoeff_Line4]]

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line4_(order, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, `SIZE(xij, 2) = order + 1`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, the routine writes `ans(1:nrow, 1:ncol)`
    !! column `j` belongs to the Lagrange polynomial of the `j`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`, both `order + 1`
  END SUBROUTINE LagrangeCoeff_Line4_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                       LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the Lagrange coefficients of a line in a chosen basis.
!
! Returns the coefficients of all Lagrange polynomials of a line.
! The polynomials use the basis chosen by `basisType`.
! Column `j` of `ans` holds the coefficients of the `j`th polynomial.
! This polynomial is 1 at the `j`th point of `xij` and 0 at the others.
!
! If `basisType` is `poly%Monomial`, the basis is `1, x, x**2, ...`.
! Then the result equals that of [[LagrangeCoeff_Line4]].
!
! Any other `basisType` selects an orthogonal polynomial family.
! The result is the inverse of the matrix of basis values at the points.
! `alpha` and `beta` are the Jacobi parameters.
! `lambda` is the ultraspherical parameter.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! The orthogonal families use only the first row of `xij`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - square matrix of size `SIZE(xij, 2)`
!
! Formula:
!   $$ V_{ik} = \phi_k(x_i), \quad V C = I $$
!   $$ \ell_j(x) = \sum_k C_{kj} \phi_k(x) $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeCoeff_Line_test_1.F90)
!
! See also: [[LagrangeCoeff_Line4]], [[LagrangeCoeff_Line5_]]

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line5(order, xij, basisType, alpha, &
                                      beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, `SIZE(xij, 2) = order + 1`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients, column `j` belongs to the Lagrange polynomial
    !! of the `j`th point
  END FUNCTION LagrangeCoeff_Line5
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                       LagrangeCoeff_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the Lagrange coefficients of a line in a chosen basis.
!
! Computes the coefficients of all Lagrange polynomials of a line.
! It writes them into the matrix `ans`.
! The polynomials use the basis chosen by `basisType`.
! Column `j` of `ans` holds the coefficients of the `j`th polynomial.
! This polynomial is 1 at the `j`th point of `xij` and 0 at the others.
!
! If `basisType` is `poly%Monomial`, the basis is `1, x, x**2, ...`.
! Then the result equals that of [[LagrangeCoeff_Line4_]].
!
! Any other `basisType` selects an orthogonal polynomial family.
! The result is the inverse of the matrix of basis values at the points.
! `alpha` and `beta` are the Jacobi parameters.
! `lambda` is the ultraspherical parameter.
!
! @note
! `ans` must hold at least `order + 1` rows and `order + 1` columns.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! The orthogonal families use only the first row of `xij`.
! @endnote
!
! Formula:
!   $$ V_{ik} = \phi_k(x_i), \quad V C = I $$
!   $$ \ell_j(x) = \sum_k C_{kj} \phi_k(x) $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeCoeff_Line__test_1.F90)
!
! See also: [[LagrangeCoeff_Line5]]

INTERFACE LagrangeCoeff_Line_
  MODULE SUBROUTINE LagrangeCoeff_Line5_(order, xij, basisType, alpha, &
                                         beta, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, `SIZE(xij, 2) = order + 1`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, the routine writes `ans(1:nrow, 1:ncol)`
    !! column `j` belongs to the Lagrange polynomial of the `j`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`, both `order + 1`
  END SUBROUTINE LagrangeCoeff_Line5_
END INTERFACE LagrangeCoeff_Line_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns all Lagrange polynomials of a line at one point.
!
! Returns the values of all Lagrange polynomials of a line at the point
! `x`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! If `coeff` is absent, the routine computes the coefficients on every
! call.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! `xij` is not changed.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - values of the polynomials, size `SIZE(xij, 2)`
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeEvalAll_Line1_]], [[LagrangeEvalAll_Line2]]

INTERFACE LagrangeEvalAll_Line
  MODULE FUNCTION LagrangeEvalAll_Line1(order, x, xij, coeff, firstCall, &
                                   basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! the routine writes it when `firstCall` is true
    !! the routine reads it when `firstCall` is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! values of the `order + 1` Lagrange polynomials at `x`
  END FUNCTION LagrangeEvalAll_Line1
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                               LagrangeEvalAll_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes all Lagrange polynomials of a line at one point.
!
! Computes the values of all Lagrange polynomials of a line at the point
! `x`.
! It writes them into the vector `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! If `coeff` is absent, the routine computes the coefficients on every
! call.
!
! @note
! `ans` must hold at least `SIZE(xij, 2)` entries.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! `xij` is not changed.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeEvalAll_Line1]], [[LagrangeEvalAll_Line2_]]

INTERFACE LagrangeEvalAll_Line_
  MODULE SUBROUTINE LagrangeEvalAll_Line1_( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda, ans, &
    tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials.
    !! Its size is at least `(SIZE(xij, 2), SIZE(xij, 2))`.
    !! The routine writes it when `firstCall` is true.
    !! The routine reads it when `firstCall` is false.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! values of the Lagrange polynomials at `x`
    !! the routine writes the first `tsize` entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to `ans`, equal to `SIZE(xij, 2)`
  END SUBROUTINE LagrangeEvalAll_Line1_
END INTERFACE LagrangeEvalAll_Line_

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns all Lagrange polynomials of a line at many points.
!
! Returns the values of all Lagrange polynomials of a line at the
! points `x`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! @warning
! If `coeff` is absent, do not set `firstCall` to `.FALSE.`.
! The routine then has no coefficients to read.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - values of the polynomials, size
!     `(SIZE(x, 2), SIZE(xij, 2))`
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeEvalAll_Line_test_1.F90)
!
! See also: [[LagrangeEvalAll_Line1]], [[LagrangeEvalAll_Line2_]]

INTERFACE LagrangeEvalAll_Line
  MODULE FUNCTION LagrangeEvalAll_Line2( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! the routine writes it when `firstCall` is true
    !! the routine reads it when `firstCall` is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! values of the Lagrange polynomials
    !! `ans(i, j)` is the value of the `j`th polynomial at the `i`th point
  END FUNCTION LagrangeEvalAll_Line2
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                       LagrangeEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes all Lagrange polynomials of a line at many points.
!
! Computes the values of all Lagrange polynomials of a line at the
! points `x`.
! It writes them into the matrix `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `ans` must hold at least `SIZE(x, 2)` rows and `SIZE(xij, 2)` columns.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! @warning
! If `coeff` is absent, do not set `firstCall` to `.FALSE.`.
! The routine then has no coefficients to read.
! @endwarning
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeEvalAll_Line__test_1.F90)
! - [test 2](|base|Line/examples/LagrangeEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeEvalAll_Line2]], [[LagrangeEvalAll_Line3_]]

INTERFACE LagrangeEvalAll_Line_
  MODULE SUBROUTINE LagrangeEvalAll_Line2_( &
    order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the Lagrange polynomials
    !! the routine writes `ans(1:nrow, 1:ncol)`
    !! `ans(i, j)` is the value of the `j`th polynomial at the `i`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `nrow = SIZE(x, 2)` and `ncol = SIZE(xij, 2)`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials.
    !! Its size is at least `(SIZE(xij, 2), SIZE(xij, 2))`.
    !! The routine writes it when `firstCall` is true.
    !! The routine reads it when `firstCall` is false.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE LagrangeEvalAll_Line2_
END INTERFACE LagrangeEvalAll_Line_

!----------------------------------------------------------------------------
!                                       LagrangeEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes all Lagrange polynomials of a line, with work arrays.
!
! Computes the values of all Lagrange polynomials of a line at the
! points `x`.
! It writes them into the matrix `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! The caller gives the work arrays `coeff` and `xx`.
! The routine does not allocate memory.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` stores the polynomial coefficients.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `ans` must hold at least `SIZE(x, 2)` rows and `SIZE(xij, 2)` columns.
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j(x) = 1 $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeEvalAll_Line2_]]

INTERFACE LagrangeEvalAll_Line_
  MODULE SUBROUTINE LagrangeEvalAll_Line3_( &
    order, x, xij, ans, nrow, ncol, coeff, xx, firstCall, basisType, alpha, &
    beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine reads it only when `firstCall` is true
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the Lagrange polynomials
    !! the routine writes `ans(1:nrow, 1:ncol)`
    !! `ans(i, j)` is the value of the `j`th polynomial at the `i`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `nrow = SIZE(x, 2)` and `ncol = SIZE(xij, 2)`
    REAL(DFP), INTENT(INOUT) :: coeff(:, :), xx(:, :)
    !! `coeff` holds the coefficients of the Lagrange polynomials.
    !! Its size is at least `ncol` by `ncol`.
    !! The routine writes it when `firstCall` is true.
    !! The routine reads it when `firstCall` is false.
    !! `xx` holds the basis functions at the points `x`.
    !! Its size is at least `nrow` by `ncol`.
    LOGICAL(LGT), INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE LagrangeEvalAll_Line3_
END INTERFACE LagrangeEvalAll_Line_

!----------------------------------------------------------------------------
!                                               LagrangeGradientEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradients of all Lagrange polynomials of a line.
!
! Returns the gradients of all Lagrange polynomials of a line at the
! points `x`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
! A line has one space dimension, so the third index of `ans` has size 1.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! @warning
! If `coeff` is absent, do not set `firstCall` to `.FALSE.`.
! The routine then has no coefficients to read.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - gradients, size `(SIZE(x, 2), SIZE(xij, 2), 1)`
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeGradientEvalAll_Line_test_1.F90)
! - [test 2](|base|Line/examples/LagrangeGradientEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeGradientEvalAll_Line1_]]

INTERFACE LagrangeGradientEvalAll_Line
  MODULE FUNCTION LagrangeGradientEvalAll_Line1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! the routine writes it when `firstCall` is true
    !! the routine reads it when `firstCall` is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 1)
    !! gradients of the Lagrange polynomials
    !! `ans(i, j, 1)` is the derivative of the `j`th polynomial at the
    !! `i`th point
  END FUNCTION LagrangeGradientEvalAll_Line1
END INTERFACE LagrangeGradientEvalAll_Line

!----------------------------------------------------------------------------
!                               LagrangeGradientEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the gradients of all Lagrange polynomials of a line.
!
! Computes the gradients of all Lagrange polynomials of a line at the
! points `x`.
! It writes them into the array `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
! A line has one space dimension, so `dim3` is 1.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `ans` must hold at least `SIZE(x, 2)` by `SIZE(xij, 2)` by 1 entries.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! @warning
! If `coeff` is absent, do not set `firstCall` to `.FALSE.`.
! The routine then has no coefficients to read.
! @endwarning
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeGradientEvalAll_Line__test_1.F90)
!
! See also: [[LagrangeGradientEvalAll_Line1]],
! [[LagrangeGradientEvalAll_Line2_]]

INTERFACE LagrangeGradientEvalAll_Line_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Line1_( &
    order, x, xij, ans, dim1, dim2, dim3, coeff, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the Lagrange polynomials
    !! the routine writes `ans(1:dim1, 1:dim2, 1:dim3)`
    !! `ans(i, j, 1)` is the derivative of the `j`th polynomial at the
    !! `i`th point
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! number of entries written to `ans` in each index
    !! `dim1 = SIZE(x, 2)`, `dim2 = SIZE(xij, 2)` and `dim3 = 1`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! the routine writes it when `firstCall` is true
    !! the routine reads it when `firstCall` is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE LagrangeGradientEvalAll_Line1_
END INTERFACE LagrangeGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                               LagrangeGradientEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the gradients of all Lagrange polynomials, with work
!   arrays.
!
! Computes the gradients of all Lagrange polynomials of a line at the
! points `x`.
! It writes them into the array `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
! A line has one space dimension, so `dim3` is 1.
!
! The caller gives the work arrays `coeff` and `xx`.
! The routine does not allocate memory.
!
! `basisType` selects the polynomial family used to build them.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` stores the polynomial coefficients.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `ans` must hold at least `SIZE(x, 2)` by `SIZE(xij, 2)` by 1 entries.
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeGradientEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeGradientEvalAll_Line1_]]

INTERFACE LagrangeGradientEvalAll_Line_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Line2_( &
    order, x, xij, ans, dim1, dim2, dim3, coeff, xx, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine reads it only when `firstCall` is true
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the Lagrange polynomials
    !! the routine writes `ans(1:dim1, 1:dim2, 1)`
    !! `ans(i, j, 1)` is the derivative of the `j`th polynomial at the
    !! `i`th point
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! number of entries written to `ans` in each index
    !! `dim1 = SIZE(x, 2)`, `dim2 = SIZE(xij, 2)` and `dim3 = 1`
    REAL(DFP), INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials.
    !! Its size is at least `(SIZE(xij, 2), SIZE(xij, 2))`.
    !! The routine writes it when `firstCall` is true.
    !! The routine reads it when `firstCall` is false.
    REAL(DFP), INTENT(INOUT) :: xx(:, :)
    !! work array for the basis derivatives at the points `x`.
    !! Its size is at least `(SIZE(x, 2), order + 1)`.
    LOGICAL(LGT), INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE LagrangeGradientEvalAll_Line2_
END INTERFACE LagrangeGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                                              LagrangeLaplacianEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the second derivatives of all Lagrange polynomials.
!
! Returns the second derivatives of all Lagrange polynomials of a line at
! the points `x`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! The tests cover only `poly%Monomial`.
! @endnote
!
! @warning
! If `coeff` is absent, do not set `firstCall` to `.FALSE.`.
! The routine then has no coefficients to read.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - second derivatives, size
!     `(SIZE(x, 2), SIZE(xij, 2))`
!
! Formula:
!   $$ \ell_j''(x) = \sum_k C_{kj} \phi_k''(x) $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeLaplacianEvalAll_Line_test_1.F90)
!
! See also: [[LagrangeLaplacianEvalAll_Line1_]]

INTERFACE LagrangeLaplacianEvalAll_Line
  MODULE FUNCTION LagrangeLaplacianEvalAll_Line1( &
    order, x, xij, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! the routine writes it when `firstCall` is true
    !! the routine reads it when `firstCall` is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! second derivatives of the Lagrange polynomials
    !! `ans(i, j)` is the value for the `j`th polynomial at the `i`th point
  END FUNCTION LagrangeLaplacianEvalAll_Line1
END INTERFACE LagrangeLaplacianEvalAll_Line

!----------------------------------------------------------------------------
!                             LagrangeLaplacianEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of all Lagrange polynomials.
!
! Computes the second derivatives of all Lagrange polynomials of a line at
! the points `x`.
! It writes them into the matrix `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! `basisType` selects the polynomial family used to build them.
! The default is `poly%Monomial`.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` can store the polynomial coefficients for later calls.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `ans` must hold at least `SIZE(x, 2)` rows and `SIZE(xij, 2)` columns.
! The routine does not allocate it.
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! @warning
! Only `poly%Monomial` works.
! The other families call empty routines and leave `ans` undefined.
! @endwarning
!
! @warning
! If `coeff` is absent, do not set `firstCall` to `.FALSE.`.
! The routine then has no coefficients to read.
! @endwarning
!
! Formula:
!   $$ \ell_j''(x) = \sum_k C_{kj} \phi_k''(x) $$
!
! Examples
!
! - [test 1](|base|Line/examples/LagrangeLaplacianEvalAll_Line__test_1.F90)
!
! See also: [[LagrangeLaplacianEvalAll_Line1]],
! [[LagrangeLaplacianEvalAll_Line2_]]

INTERFACE LagrangeLaplacianEvalAll_Line_
  MODULE SUBROUTINE LagrangeLaplacianEvalAll_Line1_( &
    order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! the routine does not change it
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! second derivatives of the Lagrange polynomials
    !! the routine writes `ans(1:nrow, 1:ncol)`
    !! `ans(i, j)` is the value for the `j`th polynomial at the `i`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `nrow = SIZE(x, 2)` and `ncol = SIZE(xij, 2)`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials.
    !! The routine writes it when `firstCall` is true.
    !! The routine reads it when `firstCall` is false.
    !! If absent, the routine uses a local array.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`, default `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE LagrangeLaplacianEvalAll_Line1_
END INTERFACE LagrangeLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!                             LagrangeLaplacianEvalAll_Line_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of all Lagrange polynomials,
! with work arrays.
!
! Computes the second derivatives of all Lagrange polynomials of a line at
! the points `x`.
! It writes them into the matrix `ans`.
! Polynomial `j` is 1 at the `j`th point of `xij` and 0 at the others.
!
! The caller gives the work arrays `coeff` and `xx`.
! The routine does not allocate memory.
!
! `basisType` selects the polynomial family used to build them.
! The Jacobi families require `alpha` and `beta`.
! The ultraspherical families require `lambda`.
!
! `coeff` stores the polynomial coefficients.
! 1. In the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and writes them to `coeff`.
! 2. In later calls with the same `xij` and `basisType`, set `firstCall`
!    to `.FALSE.`.
!    The routine reads the coefficients from `coeff`.
!
! @note
! `ans` must hold at least `SIZE(x, 2)` rows and `SIZE(xij, 2)` columns.
! `order` must equal `SIZE(xij, 2) - 1`.
! The routine reads only the first row of `x`.
! @endnote
!
! @warning
! Only `poly%Monomial` works.
! The other families call empty routines and leave `xx`, and so `ans`,
! undefined.
! @endwarning
!
! Formula:
!   $$ \ell_j''(x) = \sum_k C_{kj} \phi_k''(x) $$
!
! Examples
!
! - [test 2](|base|Line/examples/LagrangeLaplacianEvalAll_Line_test_2.F90)
!
! See also: [[LagrangeLaplacianEvalAll_Line1_]]

INTERFACE LagrangeLaplacianEvalAll_Line_
  MODULE SUBROUTINE LagrangeLaplacianEvalAll_Line2_( &
    order, x, xij, ans, nrow, ncol, coeff, xx, firstCall, basisType, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials, it should be `SIZE(xij, 2) - 1`
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation in xij format
    !! `SIZE(x, 2)` is the number of points
    !! the routine reads only `x(1, :)`
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points in xij format, `SIZE(xij, 2) = order + 1`
    !! its size sets `ncol`
    !! the routine reads its values only when `firstCall` is true
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! second derivatives of the Lagrange polynomials
    !! the routine writes `ans(1:nrow, 1:ncol)`
    !! `ans(i, j)` is the value for the `j`th polynomial at the `i`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `nrow = SIZE(x, 2)` and `ncol = SIZE(xij, 2)`
    REAL(DFP), INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials.
    !! Its size is at least `ncol` by `ncol`.
    !! The routine writes it when `firstCall` is true.
    !! The routine reads it when `firstCall` is false.
    REAL(DFP), INTENT(INOUT) :: xx(:, :)
    !! second derivatives of the basis functions at the points `x`
    !! its size is at least `nrow` by `ncol`
    LOGICAL(LGT), INTENT(IN) :: firstCall
    !! if true, compute `coeff`, if false, use `coeff`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`
    !! there is no default
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE LagrangeLaplacianEvalAll_Line2_
END INTERFACE LagrangeLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!                                              OrthogonalBasisLaplacian_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the second derivatives of the orthogonal basis of a line.
!
! Returns the second derivatives of the first `order + 1` basis
! polynomials of a line at the points `xij`.
! Column `j` holds the polynomial of degree `j - 1`.
! `basisType` selects the polynomial family.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain second derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine returns the second derivatives scaled by 2.
!
! @note
! `xij` has one row, and the routine reads only that row.
! The routine accepts `alpha`, `beta` and `lambda` but does not use them.
! @endnote
!
! @warning
! Only `poly%Monomial` works.
! The other families call empty routines and leave `ans` undefined.
! @endwarning
!
! @warning
! The chain rule for `"UNIT"` needs a factor 4, not 2.
! The result for `"UNIT"` is wrong by a factor 2.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - second derivatives, size `(SIZE(xij, 2), order + 1)`
!
! Formula:
!   $$ L_{ij} = \phi_{j-1}''(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/OrthogonalBasisLaplacian_Line_test_1.F90)
! - [test 2](|base|Line/examples/OrthogonalBasisLaplacian_Line_test_2.F90)
!   with `"BIUNIT"` and `"UNIT"`
!
! See also: [[OrthogonalBasisLaplacian_Line1_]]

INTERFACE OrthogonalBasisLaplacian_Line
  MODULE FUNCTION OrthogonalBasisLaplacian_Line1( &
    order, xij, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 1
    !! `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the points, `"BIUNIT"` or `"UNIT"`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`
    !! there is no default
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, not used
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! second derivatives of the `order + 1` polynomials
    !! `ans(:, j)` is the `j`th polynomial at all points
    !! `ans(i, :)` is all polynomials at the `i`th point
  END FUNCTION OrthogonalBasisLaplacian_Line1
END INTERFACE OrthogonalBasisLaplacian_Line

!----------------------------------------------------------------------------
!                                               OrthgonalBasisLaplacian_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of the orthogonal basis of a line.
!
! Computes the second derivatives of the first `order + 1` basis
! polynomials of a line at the points `xij`.
! It writes them into the matrix `ans`.
! Column `j` holds the polynomial of degree `j - 1`.
! `basisType` selects the polynomial family.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine writes the plain second derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine writes the second derivatives scaled by 2.
!
! @note
! `ans` must hold at least `SIZE(xij, 2)` rows and `order + 1` columns.
! `xij` has one row, and the routine reads only that row.
! The routine accepts `alpha`, `beta` and `lambda` but does not use them.
! @endnote
!
! @warning
! Only `poly%Monomial` works.
! The other families call empty routines and leave `ans` undefined.
! @endwarning
!
! @warning
! The chain rule for `"UNIT"` needs a factor 4, not 2.
! The result for `"UNIT"` is wrong by a factor 2.
! @endwarning
!
! Formula:
!   $$ L_{ij} = \phi_{j-1}''(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/OrthogonalBasisLaplacian_Line__test_1.F90)
!
! See also: [[OrthogonalBasisLaplacian_Line1]]

INTERFACE OrthogonalBasisLaplacian_Line_
  MODULE SUBROUTINE OrthogonalBasisLaplacian_Line1_( &
    order, xij, refLine, basisType, ans, nrow, ncol, alpha, beta, &
    lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 1
    !! `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the points, `"BIUNIT"` or `"UNIT"`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`
    !! there is no default
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, not used
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! second derivatives of the `order + 1` polynomials.
    !! The routine writes `ans(1:nrow, 1:ncol)`.
    !! `ans(:, j)` is the `j`th polynomial at all points
    !! `ans(i, :)` is all polynomials at the `i`th point
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `nrow = SIZE(xij, 2)` and `ncol = order + 1`
  END SUBROUTINE OrthogonalBasisLaplacian_Line1_
END INTERFACE OrthogonalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!                                                          BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the basis polynomials of a line at one point.
!
! Returns the values of the first `order + 1` basis polynomials at the
! point `x`.
! `ans(j)` is the value of the polynomial of degree `j - 1`.
!
! `basisType` selects the polynomial family.
! The default is `poly%Monomial`, which gives `ans(j) = x**(j - 1)`.
! The Jacobi family requires `alpha` and `beta`.
! The ultraspherical family requires `lambda`.
!
! @note
! `x` must lie in the interval [-1, 1], so `refLine` must be `"BIUNIT"`.
! A release build does not check `refLine` and ignores it.
! @endnote
!
! @warning
! For a family other than `poly%Monomial`, `order` must be at most 99.
! The routine keeps the values in a work array of 100 entries.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - basis values, size `order + 1`
!
! Formula:
!   $$ ans_j = \phi_{j-1}(x) $$
!
! Examples
!
! - [test 2](|base|Line/examples/BasisEvalAll_Line_test_2.F90)
!
! See also: [[BasisEvalAll_Line1_]], [[BasisEvalAll_Line2]]

INTERFACE BasisEvalAll_Line
  MODULE FUNCTION BasisEvalAll_Line1(order, x, refLine, basisType, alpha, &
                                     beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the point, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(order + 1)
    !! values of the `order + 1` polynomials at the point `x`
  END FUNCTION BasisEvalAll_Line1
END INTERFACE BasisEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the basis polynomials of a line at one point.
!
! Computes the values of the first `order + 1` basis polynomials at the
! point `x`.
! It writes them into the vector `ans`.
! `ans(j)` is the value of the polynomial of degree `j - 1`.
!
! `basisType` selects the polynomial family.
! The default is `poly%Monomial`, which gives `ans(j) = x**(j - 1)`.
! The Jacobi family requires `alpha` and `beta`.
! The ultraspherical family requires `lambda`.
!
! @note
! `ans` must hold at least `order + 1` entries.
! The routine does not allocate it.
! `x` must lie in the interval [-1, 1], so `refLine` must be `"BIUNIT"`.
! A release build does not check `refLine` and ignores it.
! @endnote
!
! @warning
! For a family other than `poly%Monomial`, `order` must be at most 99.
! The routine keeps the values in a work array of 100 entries.
! @endwarning
!
! Formula:
!   $$ ans_j = \phi_{j-1}(x) $$
!
! Examples
!
! - [test 2](|base|Line/examples/BasisEvalAll_Line_test_2.F90)
!
! See also: [[BasisEvalAll_Line1]], [[BasisEvalAll_Line2_]]

INTERFACE BasisEvalAll_Line_
  MODULE SUBROUTINE BasisEvalAll_Line1_(order, x, ans, tsize, refLine, &
                                        basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! values of the `order + 1` polynomials at the point `x`
    !! the routine writes `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written to `ans`, it is `order + 1`
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the point, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE BasisEvalAll_Line1_
END INTERFACE BasisEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the basis polynomials of a line at many points.
!
! Returns the values of the first `order + 1` basis polynomials at the
! points `x`.
! `ans(i, j)` is the polynomial of degree `j - 1` at the point `x(i)`.
!
! `basisType` selects the polynomial family.
! The default is `poly%Monomial`, which gives `ans(i, j) = x(i)**(j - 1)`.
! The Jacobi family requires `alpha` and `beta`.
! The ultraspherical family requires `lambda`.
!
! @note
! The routine evaluates at `x` as given.
! It does not map points from the `"UNIT"` interval to [-1, 1].
! Pass points of the interval [-1, 1] and `refLine` equal to `"BIUNIT"`.
! A release build does not check `refLine` and ignores it.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - basis values, size `(SIZE(x), order + 1)`
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisEvalAll_Line_test_1.F90)
!
! See also: [[BasisEvalAll_Line2_]], [[BasisEvalAll_Line1]]

INTERFACE BasisEvalAll_Line
  MODULE FUNCTION BasisEvalAll_Line2(order, x, refLine, basisType, &
                                     alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the points, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! values of the `order + 1` polynomials at the points `x`
    !! `ans(:, j)` is the `j`th polynomial at all points
    !! `ans(i, :)` is all polynomials at the point `x(i)`
  END FUNCTION BasisEvalAll_Line2
END INTERFACE BasisEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the basis polynomials of a line at many points.
!
! Computes the values of the first `order + 1` basis polynomials at the
! points `x`.
! It writes them into the matrix `ans`.
! `ans(i, j)` is the polynomial of degree `j - 1` at the point `x(i)`.
!
! `basisType` selects the polynomial family.
! The default is `poly%Monomial`, which gives `ans(i, j) = x(i)**(j - 1)`.
! The Jacobi family requires `alpha` and `beta`.
! The ultraspherical family requires `lambda`.
!
! @note
! `ans` must hold at least `SIZE(x)` rows and `order + 1` columns.
! The routine does not allocate it.
! @endnote
!
! @note
! The routine evaluates at `x` as given.
! It does not map points from the `"UNIT"` interval to [-1, 1].
! Pass points of the interval [-1, 1] and `refLine` equal to `"BIUNIT"`.
! A release build does not check `refLine` and ignores it.
! @endnote
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisEvalAll_Line__test_1.F90)
!
! See also: [[BasisEvalAll_Line2]], [[BasisEvalAll_Line1_]]

INTERFACE BasisEvalAll_Line_
  MODULE SUBROUTINE BasisEvalAll_Line2_(order, x, ans, nrow, ncol, &
                                      refLine, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the `order + 1` polynomials at the points `x`.
    !! The routine writes `ans(1:nrow, 1:ncol)`.
    !! `ans(:, j)` is the `j`th polynomial at all points
    !! `ans(i, :)` is all polynomials at the point `x(i)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`
    !! `nrow = SIZE(x)` and `ncol = order + 1`
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the points, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`, `poly%Legendre`
    !! default `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE BasisEvalAll_Line2_
END INTERFACE BasisEvalAll_Line_

!----------------------------------------------------------------------------
!                                                        OrthogonalBasis_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the orthogonal basis polynomials of a line.
!
! Returns the values of the first `order + 1` basis polynomials of a line
! at the points `xij`.
! `ans(i, j)` is the polynomial of degree `j - 1` at the `i`th point.
! `basisType` selects the polynomial family.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine evaluates the polynomials at the points as given.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!
! @note
! `xij` has one row, and the routine reads only that row.
! The Jacobi family requires `alpha` and `beta`.
! The ultraspherical family requires `lambda`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - basis values, size `(SIZE(xij, 2), order + 1)`
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/OrthogonalBasis_Line_test_1.F90)
! - [test 2](|base|Line/examples/OrthogonalBasis_Line_test_2.F90)
!   with `"BIUNIT"` and `"UNIT"`
!
! See also: [[OrthogonalBasis_Line1_]]

INTERFACE OrthogonalBasis_Line
  MODULE FUNCTION OrthogonalBasis_Line1(order, xij, refLine, basisType, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 1
    !! `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the points, `"BIUNIT"` or `"UNIT"`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Legendre`, `poly%Jacobi`
    !! there is no default
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! values of the `order + 1` polynomials at the points `xij`.
    !! `ans(:, j)` is the `j`th polynomial at all points.
    !! `ans(i, :)` is all polynomials at the `i`th point.
  END FUNCTION OrthogonalBasis_Line1
END INTERFACE OrthogonalBasis_Line

!----------------------------------------------------------------------------
!                                                     OrthogonalBasis_Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the orthogonal basis polynomials of a line.
!
! Computes the values of the first `order + 1` basis polynomials of a line
! at the points `xij`.
! It writes them into the matrix `ans`.
! `ans(i, j)` is the polynomial of degree `j - 1` at the `i`th point.
! `basisType` selects the polynomial family.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine evaluates the polynomials at the points as given.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!
! @note
! `ans` must hold at least `SIZE(xij, 2)` rows and `order + 1` columns.
! `xij` has one row, and the routine reads only that row.
! The Jacobi family requires `alpha` and `beta`.
! The ultraspherical family requires `lambda`.
! @endnote
!
! @warning
! For any other `refLine`, the routine only fills `ans` with zeros.
! A debug build also reports an error.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/OrthogonalBasis_Line__test_1.F90)
!
! See also: [[OrthogonalBasis_Line1]]

INTERFACE OrthogonalBasis_Line_
  MODULE SUBROUTINE OrthogonalBasis_Line1_( &
    order, xij, refLine, basisType, ans, nrow, ncol, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomials, the highest degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 1
    !! `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refLine
    !! interval of the points, `"BIUNIT"` or `"UNIT"`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Legendre`, `poly%Jacobi`
    !! there is no default
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the `order + 1` polynomials at the points `xij`.
    !! The routine writes `ans(1:nrow, 1:ncol)`.
    !! `ans(:, j)` is the `j`th polynomial at all points.
    !! `ans(i, :)` is all polynomials at the `i`th point.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! `nrow = SIZE(xij, 2)` and `ncol = order + 1`.
  END SUBROUTINE OrthogonalBasis_Line1_
END INTERFACE OrthogonalBasis_Line_

!----------------------------------------------------------------------------
!                                               OrthogonalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradient of the orthogonal basis of a line.
!
! Returns the first derivatives of the first `order + 1` basis
! polynomials of a line at the points `xij`.
! The result is a rank-3 array with shape `SIZE(xij, 2)` by `order + 1`
! by 1.
! Entry `ans(i, j, 1)` is the derivative of the polynomial of degree
! `j - 1` at the `i`th point.
! `basisType` selects the polynomial family.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!    It then multiplies the derivatives by 2.
!
! If `refLine` is not recognised, `ans` is not set.
! A debug build stops with an error in this case.
!
! @note
! The routine does not pass `alpha`, `beta` and `lambda` on, so they have
! no effect.
! @endnote
!
! Formula:
!   $$ ans_{i,j,1} = \phi_j'(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/OrthogonalBasisGradient_Line_test_1.F90)
! - [test 2](|base|Line/examples/OrthogonalBasisGradient_Line_test_2.F90)
! - [test 3](|base|Line/examples/OrthogonalBasisGradient_Line_test_3.F90)
!
! See also: [[OrthogonalBasisGradient_Line1_]]

INTERFACE OrthogonalBasisGradient_Line
  MODULE FUNCTION OrthogonalBasisGradient_Line1( &
    order, xij, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial` or `poly%Legendre`.
    !! It has no default.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, not used
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1, 1)
    !! derivatives of the `order + 1` polynomials.
    !! `ans(:, j, 1)` is the `j`th polynomial at all points.
    !! `ans(i, :, 1)` is all polynomials at the `i`th point.
  END FUNCTION OrthogonalBasisGradient_Line1
END INTERFACE OrthogonalBasisGradient_Line

!----------------------------------------------------------------------------
!                                               OrthgonalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the gradient of the orthogonal basis of a line, into
!   a buffer.
!
! Computes the first derivatives of the first `order + 1` basis
! polynomials of a line at the points `xij`.
! It writes them into the rank-3 array `ans`.
! Entry `ans(i, j, 1)` is the derivative of the polynomial of degree
! `j - 1` at the `i`th point.
! `basisType` selects the polynomial family.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!    It then multiplies the derivatives by 2.
!
! If `refLine` is not recognised, `ans` stays untouched.
! A debug build stops with an error in this case.
!
! @note
! The routine does not pass `alpha`, `beta` and `lambda` on, so they have
! no effect.
! @endnote
!
! Formula:
!   $$ ans_{i,j,1} = \phi_j'(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/OrthogonalBasisGradient_Line__test_1.F90)
!
! See also: [[OrthogonalBasisGradient_Line1]]

INTERFACE OrthogonalBasisGradient_Line_
  MODULE SUBROUTINE OrthogonalBasisGradient_Line1_( &
    order, xij, refLine, basisType, ans, dim1, dim2, dim3, alpha, beta, &
    lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial` or `poly%Legendre`.
    !! It has no default.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, not used
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! derivatives of the `order + 1` polynomials.
    !! The routine writes `ans(1:dim1, 1:dim2, 1)`.
    !! The shape of `ans` must be at least `SIZE(xij, 2)` by `order + 1`
    !! by 1.
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! number of entries written to `ans` along each dimension.
    !! `dim1 = SIZE(xij, 2)`, `dim2 = order + 1` and `dim3 = 1`.
  END SUBROUTINE OrthogonalBasisGradient_Line1_
END INTERFACE OrthogonalBasisGradient_Line_

!----------------------------------------------------------------------------
!                                                     HeirarchicalBasis_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the hierarchical basis of a line.
!
! Returns the values of the first `order + 1` hierarchical basis
! functions of a line at the points `xij`.
! The result is a matrix with shape `SIZE(xij, 2)` by `order + 1`.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine evaluates the functions at the points as given.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!
! This routine uses the orientation 1.
! Use [[HeirarchicalBasis_Line2_]] to choose the orientation.
!
! Formula:
!   $$ ans_{i,j} = \phi_j(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/HeirarchicalBasis_Line_test_1.F90)
! - [test 2](|base|Line/examples/HeirarchicalBasis_Line_test_2.F90)
!
! See also: [[HeirarchicalBasis_Line1_]]

INTERFACE HeirarchicalBasis_Line
  MODULE FUNCTION HeirarchicalBasis_Line1(order, xij, refLine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! hierarchical basis
  END FUNCTION HeirarchicalBasis_Line1
END INTERFACE HeirarchicalBasis_Line

!----------------------------------------------------------------------------
!                                                     HeirarchicalBasis_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the hierarchical basis of a line, into a buffer.
!
! Computes the values of the first `order + 1` hierarchical basis
! functions of a line at the points `xij`.
! It writes them into the matrix `ans`.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine evaluates the functions at the points as given.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!
! If `refLine` is not recognised, the routine sets `nrow` and `ncol`
! to 0 and leaves `ans` untouched.
!
! This routine uses the orientation 1.
! Use [[HeirarchicalBasis_Line2_]] to choose the orientation.
!
! Formula:
!   $$ ans_{i,j} = \phi_j(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/HeirarchicalBasis_Line__test_1.F90)
! - [test 2](|base|Line/examples/HeirarchicalBasis_Line_test_2.F90)
!
! See also: [[HeirarchicalBasis_Line1]]

INTERFACE HeirarchicalBasis_Line_
  MODULE SUBROUTINE HeirarchicalBasis_Line1_( &
    order, xij, refLine, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! hierarchical basis.
    !! The routine writes `ans(1:nrow, 1:ncol)`.
    !! The shape of `ans` must be at least `SIZE(xij, 2)` by `order + 1`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! `nrow = SIZE(xij, 2)` and `ncol = order + 1`.
  END SUBROUTINE HeirarchicalBasis_Line1_
END INTERFACE HeirarchicalBasis_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the hierarchical basis of a line with a given
!   orientation, into a buffer.
!
! Computes the values of the first `order + 1` hierarchical basis
! functions of a line at the points `xij`.
! It writes them into the matrix `ans`.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine evaluates the functions at the points as given.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!
! `orient` sets the orientation of the line.
! The routine multiplies the function of degree `k`, with `k >= 2`, by
! `orient**k`.
! The end-point functions do not change.
!
! If `refLine` is not recognised, the routine sets `nrow` and `ncol`
! to 0 and leaves `ans` untouched.
!
! @note
! The routine does not check `orient`. Use 1 or -1.
! @endnote
!
! Formula:
!   $$ ans_{i,j} = \phi_j(x_i) $$
!
! Examples
!
! - [test 2](|base|Line/examples/HeirarchicalBasis_Line_test_2.F90)
!
! See also: [[HeirarchicalBasis_Line_]]

INTERFACE HeirarchicalBasis_Line_
  MODULE SUBROUTINE HeirarchicalBasis_Line2_( &
    order, xij, refLine, orient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of the line, 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! hierarchical basis.
    !! The routine writes `ans(1:nrow, 1:ncol)`.
    !! The shape of `ans` must be at least `SIZE(xij, 2)` by `order + 1`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! `nrow = SIZE(xij, 2)` and `ncol = order + 1`.
  END SUBROUTINE HeirarchicalBasis_Line2_
END INTERFACE HeirarchicalBasis_Line_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradient of the hierarchical basis of a line.
!
! Returns the first derivatives of the first `order + 1` hierarchical
! basis functions of a line at the points `xij`.
! The result is a rank-3 array with shape `SIZE(xij, 2)` by `order + 1`
! by 1.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!    It then multiplies the derivatives by 2.
!
! This routine uses the orientation 1.
! Use [[HeirarchicalGradientBasis_Line2]] to choose the orientation.
!
! Formula:
!   $$ ans_{i,j,1} = \phi_j'(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/HeirarchicalBasisGradient_Line_test_1.F90)
! - [test 2](|base|Line/examples/HeirarchicalBasisGradient_Line_test_2.F90)
!
! See also: [[HeirarchicalGradientBasis_Line1_]]

INTERFACE HeirarchicalBasisGradient_Line
  MODULE FUNCTION HeirarchicalGradientBasis_Line1(order, xij, refLine) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1, 1)
    !! gradient of the hierarchical basis
  END FUNCTION HeirarchicalGradientBasis_Line1
END INTERFACE HeirarchicalBasisGradient_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the gradient of the hierarchical basis of a line, into
!   a buffer.
!
! Computes the first derivatives of the first `order + 1` hierarchical
! basis functions of a line at the points `xij`.
! It writes them into the rank-3 array `ans`.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!    It then multiplies the derivatives by 2.
!
! If `refLine` is not recognised, the routine sets `dim1`, `dim2` and
! `dim3` to 0 and leaves `ans` untouched.
!
! This routine uses the orientation 1.
! Use [[HeirarchicalGradientBasis_Line2_]] to choose the orientation.
!
! Formula:
!   $$ ans_{i,j,1} = \phi_j'(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/HeirarchicalBasisGradient_Line__test_1.F90)
! - [test 2](|base|Line/examples/HeirarchicalBasisGradient_Line_test_2.F90)
!
! See also: [[HeirarchicalGradientBasis_Line1]]

INTERFACE HeirarchicalBasisGradient_Line_
  MODULE SUBROUTINE HeirarchicalGradientBasis_Line1_( &
    order, xij, refLine, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of the hierarchical basis.
    !! The routine writes `ans(1:dim1, 1:dim2, 1)`.
    !! The shape of `ans` must be at least `SIZE(xij, 2)` by `order + 1`
    !! by 1.
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! number of entries written to `ans` along each dimension.
    !! `dim1 = SIZE(xij, 2)`, `dim2 = order + 1` and `dim3 = 1`.
  END SUBROUTINE HeirarchicalGradientBasis_Line1_
END INTERFACE HeirarchicalBasisGradient_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradient of the hierarchical basis of a line with
!   a given orientation.
!
! Returns the first derivatives of the first `order + 1` hierarchical
! basis functions of a line at the points `xij`.
! The result is a rank-3 array with shape `SIZE(xij, 2)` by `order + 1`
! by 1.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!    It then multiplies the derivatives by 2.
!
! `orient` sets the orientation of the line.
! The routine multiplies the derivative of the function of degree `k`,
! with `k >= 2`, by `orient**(k - 1)`.
! The end-point functions do not change.
!
! @note
! The routine does not check `orient`. Use 1 or -1.
! @endnote
!
! Formula:
!   $$ ans_{i,j,1} = \phi_j'(x_i) $$
!
! Examples
!
! - [test 2](|base|Line/examples/HeirarchicalBasisGradient_Line_test_2.F90)
!
! See also: [[HeirarchicalGradientBasis_Line2_]]

INTERFACE HeirarchicalBasisGradient_Line
  MODULE FUNCTION HeirarchicalGradientBasis_Line2( &
    order, xij, refLine, orient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of the line, 1 or -1
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! gradient of the hierarchical basis.
    !! Its shape is `SIZE(xij, 2)` by `order + 1` by 1.
  END FUNCTION HeirarchicalGradientBasis_Line2
END INTERFACE HeirarchicalBasisGradient_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the gradient of the hierarchical basis of a line with
!   a given orientation, into a buffer.
!
! Computes the first derivatives of the first `order + 1` hierarchical
! basis functions of a line at the points `xij`.
! It writes them into the rank-3 array `ans`.
! The first two functions belong to the end points.
! The others use scaled Lobatto polynomials of degree 2 and higher.
! The spelling `Heirarchical` is part of the API.
!
! `refLine` names the interval of the points.
! The routine checks only its first letter, in any case.
! 1. `"BIUNIT"` means the interval [-1, 1].
!    The routine returns the plain derivatives.
! 2. `"UNIT"` means the interval [0, 1].
!    The routine first maps the points to [-1, 1].
!    It then multiplies the derivatives by 2.
!
! `orient` sets the orientation of the line.
! The routine multiplies the derivative of the function of degree `k`,
! with `k >= 2`, by `orient**(k - 1)`.
! The end-point functions do not change.
!
! If `refLine` is not recognised, the routine sets `dim1`, `dim2` and
! `dim3` to 0 and leaves `ans` untouched.
!
! @note
! The routine does not check `orient`. Use 1 or -1.
! @endnote
!
! Formula:
!   $$ ans_{i,j,1} = \phi_j'(x_i) $$
!
! Examples
!
! - [test 2](|base|Line/examples/HeirarchicalBasisGradient_Line_test_2.F90)
!
! See also: [[HeirarchicalBasisGradient_Line_]]

INTERFACE HeirarchicalBasisGradient_Line_
  MODULE SUBROUTINE HeirarchicalGradientBasis_Line2_( &
    order, xij, refLine, orient, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in `xij` format.
    !! The routine reads only the first row, so `SIZE(xij, 1)` should be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of the line, 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of the hierarchical basis.
    !! The routine writes `ans(1:dim1, 1:dim2, 1)`.
    !! The shape of `ans` must be at least `SIZE(xij, 2)` by `order + 1`
    !! by 1.
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! number of entries written to `ans` along each dimension.
    !! `dim1 = SIZE(xij, 2)`, `dim2 = order + 1` and `dim3 = 1`.
  END SUBROUTINE HeirarchicalGradientBasis_Line2_
END INTERFACE HeirarchicalBasisGradient_Line_

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the first derivatives of all basis polynomials of a line
!   at one point.
!
! Returns the first derivatives of the first `order + 1` basis polynomials
! of a line at the point `x`.
! Entry `j` of the result holds the derivative of the polynomial of degree
! `j - 1`.
! `basisType` selects the polynomial family.
! The default is the monomial family.
!
! The point `x` must lie on the biunit interval [-1, 1].
! The routine does not map points from the unit interval.
! It checks `refLine` only in a debug build, where `refLine` must start
! with `"B"`.
!
! The Jacobi family needs `alpha` and `beta`.
! The ultraspherical family needs `lambda`.
! A debug build checks this.
!
! @note
! For the monomial family, the derivative of the polynomial of degree 0
! is 0.
! @endnote
!
! Formula:
!   $$ ans_j = \phi_{j-1}'(x), \quad j = 1, \ldots, order + 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisGradientEvalAll_Line_test_1.F90)
! - [test 2](|base|Line/examples/BasisGradientEvalAll_Line_test_2.F90)
!
! See also: [[BasisGradientEvalAll_Line_]]

INTERFACE BasisGradientEvalAll_Line
  MODULE FUNCTION BasisGradientEvalAll_Line1( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation, in [-1, 1]
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial` or `poly%Legendre`.
    !! The default is `poly%Monomial`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(order + 1)
    !! first derivatives of the `order + 1` polynomials at `x`
  END FUNCTION BasisGradientEvalAll_Line1
END INTERFACE BasisGradientEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the first derivatives of all basis polynomials of a
!   line at one point, into a buffer.
!
! Writes the first derivatives of the first `order + 1` basis polynomials
! at the point `x` into `ans`.
! Entry `j` holds the derivative of the polynomial of degree `j - 1`.
! `basisType` selects the polynomial family.
! The default is the monomial family.
! The routine sets `tsize` to the number of entries it wrote.
!
! The point `x` must lie on the biunit interval [-1, 1].
! The routine does not map points from the unit interval.
! It checks `refLine` only in a debug build, where `refLine` must start
! with `"B"`.
!
! The Jacobi family needs `alpha` and `beta`.
! The ultraspherical family needs `lambda`.
! A debug build checks this.
!
! @note
! For the monomial family, the derivative of the polynomial of degree 0
! is 0.
! @endnote
!
! Formula:
!   $$ ans_j = \phi_{j-1}'(x), \quad j = 1, \ldots, order + 1 $$
!
! Examples
!
! - [test 2](|base|Line/examples/BasisGradientEvalAll_Line_test_2.F90)
!
! See also: [[BasisGradientEvalAll_Line1]]

INTERFACE BasisGradientEvalAll_Line_
  MODULE SUBROUTINE BasisGradientEvalAll_Line1_( &
    order, x, refLine, basisType, alpha, beta, lambda, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation, in [-1, 1]
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! first derivatives, at least `order + 1` entries.
    !! `ans(j)` is the derivative of the polynomial of degree `j - 1`.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to `ans`, `order + 1`
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial` or `poly%Legendre`.
    !! The default is `poly%Monomial`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE BasisGradientEvalAll_Line1_
END INTERFACE BasisGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the first derivatives of all basis polynomials of a line
!   at many points.
!
! Returns the first derivatives of the first `order + 1` basis polynomials
! at the points `x`.
! Row `i` belongs to the point `x(i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! `basisType` selects the polynomial family.
! The default is the monomial family.
!
! The points must lie on the biunit interval [-1, 1].
! The routine does not map points from the unit interval.
! It checks `refLine` only in a debug build, where `refLine` must start
! with `"B"`.
!
! The Jacobi family needs `alpha` and `beta`.
! The ultraspherical family needs `lambda`.
! A debug build checks this.
!
! @note
! For the monomial family, the derivative of the polynomial of degree 0
! is 0.
! @endnote
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}'(x_i), \quad j = 1, \ldots, order + 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisGradientEvalAll_Line_test_1.F90)
!
! See also: [[BasisGradientEvalAll_Line2_]]

INTERFACE BasisGradientEvalAll_Line
  MODULE FUNCTION BasisGradientEvalAll_Line2( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation, in [-1, 1]
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial` or `poly%Legendre`.
    !! The default is `poly%Monomial`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! first derivatives, `ans(i, j)` is the derivative of the polynomial
    !! of degree `j - 1` at `x(i)`
  END FUNCTION BasisGradientEvalAll_Line2
END INTERFACE BasisGradientEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the first derivatives of all basis polynomials of a
!   line at many points, into a matrix.
!
! Writes the first derivatives of the first `order + 1` basis polynomials
! at the points `x` into `ans`.
! Row `i` belongs to the point `x(i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! `basisType` selects the polynomial family.
! The default is the monomial family.
! The routine sets `nrow` and `ncol` to the size it wrote.
!
! The points must lie on the biunit interval [-1, 1].
! The routine does not map points from the unit interval.
! It checks `refLine` only in a debug build, where `refLine` must start
! with `"B"`.
!
! The Jacobi family needs `alpha` and `beta`.
! The ultraspherical family needs `lambda`.
! A debug build checks this.
!
! @note
! For the monomial family, the derivative of the polynomial of degree 0
! is 0.
! @endnote
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}'(x_i), \quad j = 1, \ldots, order + 1 $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisGradientEvalAll_Line__test_1.F90)
!
! See also: [[BasisGradientEvalAll_Line2]]

INTERFACE BasisGradientEvalAll_Line_
  MODULE SUBROUTINE BasisGradientEvalAll_Line2_( &
    order, x, ans, nrow, ncol, refLine, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation, in [-1, 1]
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! first derivatives, at least `SIZE(x)` rows and `order + 1` columns.
    !! `ans(i, j)` is the derivative of the polynomial of degree `j - 1`
    !! at `x(i)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! They are `SIZE(x)` and `order + 1`.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, it should be `"BIUNIT"`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial` or `poly%Legendre`.
    !! The default is `poly%Monomial`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi family
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical family
  END SUBROUTINE BasisGradientEvalAll_Line2_
END INTERFACE BasisGradientEvalAll_Line_

!----------------------------------------------------------------------------
!                                                      QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! Returns the points and weights of a quadrature rule on a line.
! `order` is the polynomial degree the rule should integrate.
! The routine picks the number of points from `order` and `quadType`.
!
! Each column of the result is one quadrature point.
! The last row holds the weights.
! The rows before it hold the coordinates of the point.
!
! Without `xij`, the points lie on the biunit interval [-1, 1].
! The result then has two rows.
! With `xij`, the routine maps the points to a segment.
! Column 1 of `xij` is the first end point and column 2 is the second.
! The result then has `SIZE(xij, 1) + 1` rows.
! The routine multiplies the weights by half the segment length.
!
! The rules, the `layout` values and the checks of `alpha`, `beta` and
! `lambda` are the same as in [[QuadraturePoint_Line1_]].
!
! Formula:
!   $$ \int_{-1}^{1} f(x)\,dx \approx \sum_{i=1}^{n} w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/QuadraturePoint_Line_test_1.F90) with
!   a Lobatto rule on the biunit interval
!
! See also: [[QuadraturePoint_Line2]], [[QuadratureNumber_Line]]

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line1(order, quadType, layout, xij, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree the rule should integrate
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature rule, a constant of `TypeQuadratureOpt`,
    !! for example `ipopt%GaussLegendre`
    CHARACTER(*), INTENT(IN) :: layout
    !! `"VEFC"` puts the end points first.
    !! `"INCREASING"` gives increasing order.
    !! Only the first letter matters.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! end points of the segment, `xij(:, 1)` and `xij(:, 2)`.
    !! The default is the biunit interval [-1, 1].
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical rules
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, shape `(nsd + 1, n)`.
    !! `nsd` is `SIZE(xij, 1)`, or 1 when `xij` is absent.
    !! `n` is [[QuadratureNumber_Line]] for `order` and `quadType`.
    !! The last row holds the weights.
  END FUNCTION QuadraturePoint_Line1
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! Returns the points and weights of a quadrature rule on a segment of a
! one dimensional line.
! `order` is the polynomial degree the rule should integrate.
! The routine picks the number of points from `order` and `quadType`.
!
! The segment runs from `xij(1)` to `xij(2)`.
! The result has two rows.
! Row 1 holds the points.
! Row 2 holds the weights, scaled by half the length of the segment.
!
! The rules, the `layout` values and the checks of `alpha`, `beta` and
! `lambda` are the same as in [[QuadraturePoint_Line1_]].
!
! Formula:
!   $$ \int_{x_1}^{x_2} f(x)\,dx \approx \sum_{i=1}^{n} w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/QuadraturePoint_Line_test_1.F90) with
!   order 3 on the segment [0, 2]
!
! See also: [[QuadraturePoint_Line1]], [[QuadraturePoint_Line4]],
!   [[QuadratureNumber_Line]]

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line2(order, quadType, xij, layout, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree the rule should integrate
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature rule, a constant of `TypeQuadratureOpt`,
    !! for example `ipopt%GaussLegendre`
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points of the segment
    CHARACTER(*), INTENT(IN) :: layout
    !! `"VEFC"` puts the end points first.
    !! `"INCREASING"` gives increasing order.
    !! Only the first letter matters.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical rules
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, shape `(2, n)`.
    !! `n` is [[QuadratureNumber_Line]] for `order` and `quadType`.
    !! Row 1 holds the points and row 2 holds the weights.
  END FUNCTION QuadraturePoint_Line2
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! Returns the points and weights of a quadrature rule on a segment of a
! one dimensional line.
! The caller gives the number of points `nips(1)`.
! The rule must support that number of points.
!
! The segment runs from `xij(1)` to `xij(2)`.
! The result has two rows and `nips(1)` columns.
! Row 1 holds the points.
! Row 2 holds the weights, scaled by half the length of the segment.
!
! The rules, the `layout` values and the checks of `alpha`, `beta` and
! `lambda` are the same as in [[QuadraturePoint_Line1_]].
!
! Formula:
!   $$ \int_{x_1}^{x_2} f(x)\,dx \approx \sum_{i=1}^{n} w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/QuadraturePoint_Line_test_1.F90) with
!   three points on the segment [0, 2]
!
! See also: [[QuadraturePoint_Line2]], [[QuadraturePoint_Line3]]

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line4(nips, quadType, xij, layout, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points, `nips(1)`
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature rule, a constant of `TypeQuadratureOpt`,
    !! for example `ipopt%GaussLegendre`
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points of the segment
    CHARACTER(*), INTENT(IN) :: layout
    !! `"VEFC"` puts the end points first.
    !! `"INCREASING"` gives increasing order.
    !! Only the first letter matters.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical rules
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, shape `(2, nips(1))`.
    !! Row 1 holds the points and row 2 holds the weights.
  END FUNCTION QuadraturePoint_Line4
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                      QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! Returns the points and weights of a quadrature rule on a line.
! The caller gives the number of points `nips(1)`.
! The rule must support that number of points.
!
! Each column of the result is one quadrature point.
! The last row holds the weights.
! The rows before it hold the coordinates of the point.
!
! Without `xij`, the points lie on the biunit interval [-1, 1].
! The result then has two rows.
! With `xij`, the routine maps the points to a segment.
! Column 1 of `xij` is the first end point and column 2 is the second.
! The result then has `SIZE(xij, 1) + 1` rows.
! The routine multiplies the weights by half the segment length.
!
! The rules, the `layout` values and the checks of `alpha`, `beta` and
! `lambda` are the same as in [[QuadraturePoint_Line1_]].
!
! Formula:
!   $$ \int_{-1}^{1} f(x)\,dx \approx \sum_{i=1}^{n} w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/QuadraturePoint_Line_test_1.F90) with a
!   segment in two dimensions
!
! See also: [[QuadraturePoint_Line1]], [[QuadraturePoint_Line4]]

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line3(nips, quadType, layout, xij, &
                                        alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points, `nips(1)`
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature rule, a constant of `TypeQuadratureOpt`,
    !! for example `ipopt%GaussLegendre`
    CHARACTER(*), INTENT(IN) :: layout
    !! `"VEFC"` puts the end points first.
    !! `"INCREASING"` gives increasing order.
    !! Only the first letter matters.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! end points of the segment, `xij(:, 1)` and `xij(:, 2)`.
    !! The default is the biunit interval [-1, 1].
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical rules
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points and weights, shape `(nsd + 1, nips(1))`.
    !! `nsd` is `SIZE(xij, 1)`, or 1 when `xij` is absent.
    !! The last row holds the weights.
  END FUNCTION QuadraturePoint_Line3
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Select a rule and construct quadrature coordinates and weights.
!
! Computes the points and weights of a quadrature rule on a line.
! The routine writes them into `ans`, which the caller allocates.
! It never allocates `ans`.
!
! Each column of `ans` is one quadrature point.
! The last row holds the weights.
! The rows before it hold the coordinates of the point.
!
! Without `xij`, the points lie on the biunit interval [-1, 1].
! `ans` then has two rows, and the weights sum to 2.
!
! With `xij`, the routine maps the points to a segment.
! Column 1 of `xij` is the first end point of the segment.
! Column 2 is the second end point.
! `SIZE(xij, 1)` is the number of space dimensions `nsd`.
! `ans` then has `nsd + 1` rows.
! The routine multiplies the weights by half the segment length.
!
! `layout` sets the order of the points.
! If its first letter is `"V"`, the two end points come first.
! The interior points follow.
! Any other value gives increasing order.
!
! `quadType` selects the rule.
! Use a constant of `TypeQuadratureOpt`, for example `ipopt%GaussLegendre`.
! The Legendre, Chebyshev, Jacobi and ultraspherical families are
! available.
! Each family has a Gauss, a left Radau, a right Radau and a Lobatto rule.
!
! The Jacobi rules need `alpha` and `beta`.
! The ultraspherical rules need `lambda`.
! A debug build checks this.
!
! @note
! The routine does not check `quadType` in a release build.
! For an unknown type, the routine does not fill `ans`.
! A debug build stops with an error.
! @endnote
!
! Formula:
!   $$ \int_{-1}^{1} f(x)\,dx \approx \sum_{i=1}^{n} w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/QuadraturePoint_Line__test_1.F90)
!
! See also: [[QuadraturePoint_Line3]], [[QuadratureNumber_Line]]

INTERFACE QuadraturePoint_Line_
  MODULE SUBROUTINE QuadraturePoint_Line1_(nips, quadType, layout, xij, &
                                           alpha, beta, lambda, ans, nrow, &
                                           ncol)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points, `nips(1)`
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature rule, a constant of `TypeQuadratureOpt`,
    !! for example `ipopt%GaussLegendre`
    CHARACTER(*), INTENT(IN) :: layout
    !! `"VEFC"` puts the end points first.
    !! `"INCREASING"` gives increasing order.
    !! Only the first letter matters.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! end points of the segment, `xij(:, 1)` and `xij(:, 2)`.
    !! The default is the biunit interval [-1, 1].
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi rules
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical rules
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! points and weights.
    !! It needs at least `nsd + 1` rows and `nips(1)` columns.
    !! The last active row holds the weights.
    !! `nsd` is `SIZE(xij, 1)`, or 1 when `xij` is absent.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! They are `nsd + 1` and `nips(1)`.
  END SUBROUTINE QuadraturePoint_Line1_
END INTERFACE QuadraturePoint_Line_

!----------------------------------------------------------------------------
!                                            HeirarchicalBasisLaplacian_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the second derivatives of the hierarchical basis of a
!   line.
!
! Intended to return the second derivatives of the first `order + 1`
! hierarchical basis polynomials at the points `xij`.
! Row `i` of the result belongs to point `xij(1, i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! The routine uses the orientation `+1`.
!
! @warning
! Do not call this function yet.
! The Lobatto Laplacian routine behind it has an empty body.
! It leaves the result unset.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}''(x_i), \quad j = 1, \ldots, order + 1 $$
!
! See also: [[HeirarchicalLaplacianBasis_Line1_]]

INTERFACE HeirarchicalBasisLaplacian_Line
  MODULE FUNCTION HeirarchicalLaplacianBasis_Line1(order, xij, refLine) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one point per column.
    !! `SIZE(xij, 1)` must be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`.
    !! Only the first letter matters.
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! second derivatives at the points `xij`
  END FUNCTION HeirarchicalLaplacianBasis_Line1
END INTERFACE HeirarchicalBasisLaplacian_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of the hierarchical basis of a
!   line, into a buffer.
!
! Intended to write the second derivatives of the first `order + 1`
! hierarchical basis polynomials at the points `xij` into `ans`.
! Row `i` of `ans` belongs to point `xij(1, i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! The routine uses the orientation `+1`.
! It sets `nrow` and `ncol` to the active size of `ans`.
!
! @warning
! Do not call this routine yet.
! The Lobatto Laplacian routine behind it has an empty body.
! It leaves `ans`, `nrow` and `ncol` unset.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}''(x_i), \quad j = 1, \ldots, order + 1 $$
!
! See also: [[HeirarchicalLaplacianBasis_Line1]]

INTERFACE HeirarchicalBasisLaplacian_Line_
  MODULE SUBROUTINE HeirarchicalLaplacianBasis_Line1_( &
    order, xij, refLine, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one point per column.
    !! `SIZE(xij, 1)` must be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`.
    !! Only the first letter matters.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! second derivatives, at least `SIZE(xij, 2)` rows and
    !! `order + 1` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! They should be `SIZE(xij, 2)` and `order + 1`.
  END SUBROUTINE HeirarchicalLaplacianBasis_Line1_
END INTERFACE HeirarchicalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the second derivatives of the hierarchical basis of a
!   line with an orientation.
!
! Intended to return the second derivatives of the first `order + 1`
! hierarchical basis polynomials at the points `xij`.
! Row `i` of the result belongs to the point `xij(1, i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! The routine allocates the result with `SIZE(xij, 2)` rows and
! `order + 1` columns.
!
! The routine reads the first letter of `refLine`.
!
! 1. For `"B"`, `xij` lies on the biunit interval `[-1, 1]`.
! 2. For `"U"`, `xij` lies on the unit interval `[0, 1]`.
!    The routine maps the points to the biunit interval and multiplies
!    the result by 2.
! 3. For any other letter, the result is not filled.
!
! Then it multiplies column `k + 1` by `orient**(k - 1)` for each degree
! `k >= 2`. The two vertex columns stay unchanged.
!
! @warning
! Do not call this function yet.
! The Lobatto Laplacian routine behind it has an empty body.
! It leaves the result unset.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}''(x_i), \quad j = 1, \ldots, order + 1 $$
!
! See also: [[HeirarchicalLaplacianBasis_Line1]],
!   [[HeirarchicalLaplacianBasis_Line2_]]

INTERFACE HeirarchicalBasisLaplacian_Line
  MODULE FUNCTION HeirarchicalLaplacianBasis_Line2( &
    order, xij, refLine, orient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one point per column.
    !! `SIZE(xij, 1)` must be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`.
    !! Only the first letter matters.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of the line, 1 or -1
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! second derivatives at the points `xij`, shape
    !! `(SIZE(xij, 2), order + 1)`
  END FUNCTION HeirarchicalLaplacianBasis_Line2
END INTERFACE HeirarchicalBasisLaplacian_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of the hierarchical basis of a
!   line with an orientation, into a buffer.
!
! Intended to write the second derivatives of the first `order + 1`
! hierarchical basis polynomials at the points `xij` into `ans`.
! Row `i` of `ans` belongs to the point `xij(1, i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! The routine sets `nrow` and `ncol` to the active size of `ans`.
! It never allocates `ans`.
!
! The routine reads the first letter of `refLine`.
!
! 1. For `"B"`, `xij` lies on the biunit interval `[-1, 1]`.
! 2. For `"U"`, `xij` lies on the unit interval `[0, 1]`.
!    The routine maps the points to the biunit interval and multiplies
!    the result by 2.
! 3. For any other letter, it sets `nrow` and `ncol` to 0.
!
! Then it multiplies column `k + 1` by `orient**(k - 1)` for each degree
! `k >= 2`. The two vertex columns stay unchanged.
!
! @warning
! Do not call this routine yet.
! The Lobatto Laplacian routine behind it has an empty body.
! It leaves `ans`, `nrow` and `ncol` unset.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}''(x_i), \quad j = 1, \ldots, order + 1 $$
!
! See also: [[HeirarchicalLaplacianBasis_Line1_]],
!   [[HeirarchicalLaplacianBasis_Line2]]

INTERFACE HeirarchicalBasisLaplacian_Line_
  MODULE SUBROUTINE HeirarchicalLaplacianBasis_Line2_( &
    order, xij, refLine, orient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one point per column.
    !! `SIZE(xij, 1)` must be 1.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, `"UNIT"` or `"BIUNIT"`.
    !! Only the first letter matters.
    INTEGER(I4B), INTENT(IN) :: orient
    !! orientation of the line, 1 or -1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! second derivatives, at least `SIZE(xij, 2)` rows and
    !! `order + 1` columns
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! They should be `SIZE(xij, 2)` and `order + 1`.
  END SUBROUTINE HeirarchicalLaplacianBasis_Line2_
END INTERFACE HeirarchicalBasisLaplacian_Line_

!----------------------------------------------------------------------------
!                                                 BasisLaplacianEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the second derivatives of the basis of a line at one
!   point.
!
! Returns the second derivatives of the first `order + 1` basis
! polynomials at the point `x`.
! Entry `j` belongs to the polynomial of degree `j - 1`.
!
! The default basis is the monomial basis `1, x, x**2, ...`.
! Entries 1 and 2 are zero.
! For `j >= 3`, entry `j` is `(j - 1) * (j - 2) * x**(j - 3)`.
!
! The routine works on the biunit interval `[-1, 1]` only.
!
! @note
! A debug build stops with an error if `refLine` does not start with `B`.
! A release build does not check `refLine`.
! It never rescales the result for `"UNIT"`.
! @endnote
!
! @warning
! Only the monomial basis works.
! For any other `basisType`, the polynomial backend is an empty stub.
! It leaves the result unset.
! @endwarning
!
! Formula:
!   $$ ans_j = \phi_{j-1}''(x) $$
!
! Examples
!
! - [test 2](|base|Line/examples/BasisLaplacianEvalAll_Line_test_2.F90)
!
! See also: [[BasisLaplacianEvalAll_Line1_]], [[BasisLaplacianEvalAll_Line2]]

INTERFACE BasisLaplacianEvalAll_Line
  MODULE FUNCTION BasisLaplacianEvalAll_Line1( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, must be `"BIUNIT"`.
    !! Only a debug build checks the first letter.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, a constant of `TypePolynomialOpt`.
    !! The default is `poly%Monomial`.
    !! Only the monomial basis works.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical basis
    REAL(DFP) :: ans(order + 1)
    !! second derivatives at the point `x`.
    !! `ans(j)` is the second derivative of the polynomial of degree
    !! `j - 1` at `x`.
  END FUNCTION BasisLaplacianEvalAll_Line1
END INTERFACE BasisLaplacianEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of the basis of a line at one
!   point, into a buffer.
!
! Writes the second derivatives of the first `order + 1` basis
! polynomials at the point `x` into `ans`.
! Entry `j` belongs to the polynomial of degree `j - 1`.
! The routine sets `tsize` to `order + 1`.
! It never allocates `ans`.
!
! The default basis is the monomial basis `1, x, x**2, ...`.
! Entries 1 and 2 are zero.
! For `j >= 3`, entry `j` is `(j - 1) * (j - 2) * x**(j - 3)`.
!
! The routine works on the biunit interval `[-1, 1]` only.
!
! @note
! A debug build stops with an error if `refLine` does not start with `B`.
! A release build does not check `refLine`.
! It never rescales the result for `"UNIT"`.
! @endnote
!
! @warning
! Only the monomial basis works.
! For any other `basisType`, the polynomial backend is an empty stub.
! It leaves `ans` unset.
! @endwarning
!
! Formula:
!   $$ ans_j = \phi_{j-1}''(x) $$
!
! Examples
!
! - [test 2](|base|Line/examples/BasisLaplacianEvalAll_Line_test_2.F90)
!   calls this routine in its second block
!
! See also: [[BasisLaplacianEvalAll_Line1]], [[BasisLaplacianEvalAll_Line2_]]

INTERFACE BasisLaplacianEvalAll_Line_
  MODULE SUBROUTINE BasisLaplacianEvalAll_Line1_( &
    order, x, refLine, basisType, alpha, beta, lambda, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! second derivatives, at least `order + 1` entries.
    !! `ans(j)` is the second derivative of the polynomial of degree
    !! `j - 1` at `x`.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to `ans`, which is `order + 1`
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, must be `"BIUNIT"`.
    !! Only a debug build checks the first letter.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, a constant of `TypePolynomialOpt`.
    !! The default is `poly%Monomial`.
    !! Only the monomial basis works.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical basis
  END SUBROUTINE BasisLaplacianEvalAll_Line1_
END INTERFACE BasisLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the second derivatives of the basis of a line at many
!   points.
!
! Returns the second derivatives of the first `order + 1` basis
! polynomials at the points `x`.
! Row `i` of the result belongs to the point `x(i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
!
! The default basis is the monomial basis `1, x, x**2, ...`.
! Columns 1 and 2 are zero.
! For `j >= 3`, column `j` is `(j - 1) * (j - 2) * x**(j - 3)`.
!
! The routine works on the biunit interval `[-1, 1]` only.
!
! @note
! A debug build stops with an error if `refLine` does not start with `B`.
! A release build does not check `refLine`.
! It never rescales the result for `"UNIT"`.
! @endnote
!
! @warning
! Only the monomial basis works.
! For any other `basisType`, the polynomial backend is an empty stub.
! It leaves the result unset.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}''(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisLaplacianEvalAll_Line_test_1.F90)
!
! See also: [[BasisLaplacianEvalAll_Line2_]], [[BasisLaplacianEvalAll_Line1]]

INTERFACE BasisLaplacianEvalAll_Line
  MODULE FUNCTION BasisLaplacianEvalAll_Line2( &
    order, x, refLine, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, must be `"BIUNIT"`.
    !! Only a debug build checks the first letter.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, a constant of `TypePolynomialOpt`.
    !! The default is `poly%Monomial`.
    !! Only the monomial basis works.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical basis
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! second derivatives at the points `x`.
    !! `ans(i, j)` is the second derivative of the polynomial of
    !! degree `j - 1` at `x(i)`.
  END FUNCTION BasisLaplacianEvalAll_Line2
END INTERFACE BasisLaplacianEvalAll_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the second derivatives of the basis of a line at many
!   points, into a buffer.
!
! Writes the second derivatives of the first `order + 1` basis
! polynomials at the points `x` into `ans`.
! Row `i` of `ans` belongs to the point `x(i)`.
! Column `j` belongs to the polynomial of degree `j - 1`.
! The routine sets `nrow` and `ncol` to the active size of `ans`.
! It never allocates `ans`.
!
! The default basis is the monomial basis `1, x, x**2, ...`.
! Columns 1 and 2 are zero.
! For `j >= 3`, column `j` is `(j - 1) * (j - 2) * x**(j - 3)`.
!
! The routine works on the biunit interval `[-1, 1]` only.
!
! @note
! A debug build stops with an error if `refLine` does not start with `B`.
! A release build does not check `refLine`.
! It never rescales the result for `"UNIT"`.
! @endnote
!
! @warning
! Only the monomial basis works.
! For any other `basisType`, the polynomial backend is an empty stub.
! It leaves `ans`, `nrow` and `ncol` unset.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \phi_{j-1}''(x_i) $$
!
! Examples
!
! - [test 1](|base|Line/examples/BasisLaplacianEvalAll_Line__test_1.F90)
!
! See also: [[BasisLaplacianEvalAll_Line2]]

INTERFACE BasisLaplacianEvalAll_Line_
  MODULE SUBROUTINE BasisLaplacianEvalAll_Line2_( &
    order, x, ans, nrow, ncol, refLine, basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! highest polynomial degree
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! second derivatives, at least `SIZE(x)` rows and `order + 1`
    !! columns.
    !! `ans(i, j)` is the second derivative of the polynomial of
    !! degree `j - 1` at `x(i)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to `ans`.
    !! They are `SIZE(x)` and `order + 1`.
    CHARACTER(*), INTENT(IN) :: refLine
    !! reference interval, must be `"BIUNIT"`.
    !! Only a debug build checks the first letter.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, a constant of `TypePolynomialOpt`.
    !! The default is `poly%Monomial`.
    !! Only the monomial basis works.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter, needed for the Jacobi basis
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, needed for the ultraspherical basis
  END SUBROUTINE BasisLaplacianEvalAll_Line2_
END INTERFACE BasisLaplacianEvalAll_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LineInterpolationUtility
