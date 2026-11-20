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

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Divergence with respect to the physical coordinates
!
! Compute the divergence of a vector or matrix at the quadrature points
! from its nodal values and `dNdXt` (or `dNTdXt` for space-time). The
! divergence of a vector is a scalar; the divergence of a matrix is the
! vector of row divergences, `div(M)_i = SUM_j dM_ij/dx_j`. `dNdXt` (and
! `dNTdXt`) must be set first, for example by [[Set]].

MODULE ElemshapeData_DivergenceMethods
USE BaseType, ONLY: ElemShapeData_, FEVariable_, STElemshapeData_
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: GetDivergence
PUBLIC :: Divergence

!----------------------------------------------------------------------------
!                                                      GetDivergence@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Divergence of vector nodal values
!
! Returns the divergence of a vector at the quadrature points.
! `val(nsd, nns)` holds the nodal values of the vector.
! The result is a scalar at each quadrature point.
!
! The routine does not allocate `ans`.
! It returns the number of quadrature points in `tsize`.
!
! @note
! Set `obj%dNdXt` before the call.
! @endnote
!
! Formula:
!   $$ ans(ips) = \sum_I \sum_i val(i, I) \, dNdXt(I, i, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
!
! See also: [[elemsd_GetDivergence_3]], [[elemsd_Divergence_1]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_1(obj, val, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values, val(nsd, nns)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! divergence, ans(nips)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of ans
  END SUBROUTINE elemsd_GetDivergence_1
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                          GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Divergence of vector space-time nodal values
!
! Returns the divergence of a space-time vector at the quadrature points.
! `val(nsd, nns, nnt)` holds the nodal values in space and time.
! The result is for the one time point that `obj` represents.
!
! The routine does not allocate `ans`.
! It returns the number of quadrature points in `tsize`.
!
! @note
! `obj` must be a `STElemShapeData_` with `dNTdXt` set. Otherwise,
! the routine leaves `ans` unchanged.
! @endnote
!
! Changes:
!   - 2026-11-14: removed a duplicate declaration
!
! Formula:
!   $$ ans(ips) = \sum_a \sum_I \sum_i val(i, I, a) \,
!   dNTdXt(I, a, i, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
!
! See also: [[elemsd_GetDivergence_1]], [[elemsd_GetDivergence_3]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_2(obj, val, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values, val(nsd, nns, nnt)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! divergence, ans(nips)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans, nips
  END SUBROUTINE elemsd_GetDivergence_2
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                      GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Divergence of a vector FEVariable
!
! Returns the divergence of the vector `FEVariable_` `val` in `ans`.
! The routine does not allocate `ans`.
! It returns the number of quadrature points in `tsize`.
! The variable type of `val` decides the result.
!
! - A constant `val` gives zero.
! - A space `val` works like [[elemsd_GetDivergence_1]].
! - A space-time `val` works like [[elemsd_GetDivergence_2]].
!   Then `obj` must be a `STElemShapeData_`.
!
! For other variable types, the routine leaves `ans` unchanged.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
! - [test 2](|base|ElemshapeData/examples/GetDivergence_test_2.F90) with a
!   space-time `val`
!
! See also: [[elemsd_GetDivergence_1]], [[elemsd_GetDivergence_2]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_3(obj, val, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector nodal FEVariable
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! divergence, ans(nips)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
  END SUBROUTINE elemsd_GetDivergence_3
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                            GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Row divergence of matrix nodal values
!
! Returns the divergence of each row of a matrix at the quadrature points.
! `val(nrow, nsd, nns)` holds the nodal values of the matrix.
! The result is a vector with `nrow` entries at each quadrature point.
!
! The routine does not allocate `ans`.
! It returns `nrow = SIZE(val, 1)` and `ncol = nips`.
!
! @note
! Set `obj%dNdXt` before the call.
! @endnote
!
! Changes:
!   - 2026-11-14: row divergence, `SUM_j dM_ij/dx_j`. Before, it summed
!     over the first index.
!
! Formula:
!   $$ ans(i, ips) = \sum_I \sum_j val(i, j, I) \, dNdXt(I, j, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
!
! See also: [[elemsd_GetDivergence_5]], [[elemsd_GetDivergence_6]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_4(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal values, val(nrow, nsd, nns)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! row divergence, ans(nrow, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans, nips
  END SUBROUTINE elemsd_GetDivergence_4
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                       GetDivergence@DivergenceMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Row divergence of matrix space-time nodal values
!
! Returns the divergence of each row of a space-time matrix.
! `val(nrow, nsd, nns, nnt)` holds the nodal values in space and time.
! The result is for the one time point that `obj` represents.
!
! The routine does not allocate `ans`.
! It returns `nrow = SIZE(val, 1)` and `ncol = nips`.
!
! @note
! `obj` must be a `STElemShapeData_` with `dNTdXt` set. Otherwise,
! the routine leaves `ans` unchanged.
! @endnote
!
! Changes:
!   - 2026-11-14: row divergence, `SUM_j dM_ij/dx_j`. Before, it summed
!     over the first index.
!
! Formula:
!   $$ ans(i, ips) = \sum_a \sum_I \sum_j val(i, j, I, a) \,
!   dNTdXt(I, a, j, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
!
! See also: [[elemsd_GetDivergence_4]], [[elemsd_GetDivergence_6]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_5(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values, val(nrow, nsd, nns, nnt)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! row divergence, ans(nrow, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans, nips
  END SUBROUTINE elemsd_GetDivergence_5
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                                      GetDivergence@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Row divergence of a matrix FEVariable
!
! Returns the row divergence of the matrix `FEVariable_` `val` in `ans`.
! The routine does not allocate `ans`.
! It returns the shape of the result in `nrow` and `ncol`.
! The variable type of `val` decides the result.
!
! - A constant `val` gives zero.
! - A space `val` works like [[elemsd_GetDivergence_4]].
! - A space-time `val` works like [[elemsd_GetDivergence_5]].
!   Then `obj` must be a `STElemShapeData_`.
!
! For other variable types, the routine leaves `ans` unchanged.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
! - [test 2](|base|ElemshapeData/examples/GetDivergence_test_2.F90) with a
!   constant and a space-time `val`
!
! See also: [[elemsd_GetDivergence_4]], [[elemsd_GetDivergence_5]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_6(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix nodal FEVariable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! row divergence, ans(nrow, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans, nips
  END SUBROUTINE elemsd_GetDivergence_6
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                                     GetDivergence@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Divergence of a FEVariable as a FEVariable
!
! Returns the divergence of `val` as a quadrature, space `FEVariable_`.
! A vector `val` gives a scalar.
! A matrix `val` gives a vector of row divergences.
! For any other rank of `val`, the routine leaves `ans` unchanged.
!
! @note
! `val` must hold nodal values. A quadrature variable is not supported.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
! - [test 2](|base|ElemshapeData/examples/GetDivergence_test_2.F90) with a
!   constant `val`
!
! See also: [[elemsd_GetDivergence_3]], [[elemsd_GetDivergence_6]],
! [[elemsd_Divergence_1]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_7(obj, val, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! divergence, quadrature space variable
  END SUBROUTINE elemsd_GetDivergence_7
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                                      GetDivergence@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: Divergence of a FEVariable at all time points
!
! Returns the divergence of `val` at all time points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! The result is a quadrature, space-time `FEVariable_`.
! In the shapes below, `nips` is the largest number of quadrature
! points in `obj`.
!
! - A vector `val` gives a scalar with shape `[nips, nipt]`.
! - A matrix `val` gives a vector with shape `[nrow, nips, nipt]`.
!
! For any other rank of `val`, the routine leaves `ans` unchanged.
!
! Changes:
!   - 2026-11-14: for a vector `val`, the result used `obj(nipt + 1)`
!     after the loop. It now uses the largest `nips`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetDivergence_test_1.F90)
! - [test 2](|base|ElemshapeData/examples/GetDivergence_test_2.F90) with a
!   matrix `val`
!
! See also: [[elemsd_GetDivergence_7]], [[elemsd_Divergence_2]]

INTERFACE GetDivergence
  MODULE PURE SUBROUTINE elemsd_GetDivergence_8(obj, val, ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! divergence, quadrature space-time variable
  END SUBROUTINE elemsd_GetDivergence_8
END INTERFACE GetDivergence

!----------------------------------------------------------------------------
!                                                          Divergence@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the divergence of a FEVariable
!
! Returns the divergence of `val` as a quadrature `FEVariable_`.
! A vector `val` gives a scalar.
! A matrix `val` gives a vector of row divergences.
! The result is the same as that of [[elemsd_GetDivergence_7]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Divergence_test_1.F90)
!
! See also: [[elemsd_GetDivergence_7]]

INTERFACE Divergence
  MODULE PURE FUNCTION elemsd_Divergence_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
    TYPE(FEVariable_) :: ans
    !! divergence, quadrature space variable
  END FUNCTION elemsd_Divergence_1
END INTERFACE Divergence

!----------------------------------------------------------------------------
!                                                          Divergence@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Function form of the divergence at all time points
!
! Returns the divergence of `val` at all time points.
! `obj(ipt)` is the shape data at time quadrature point `ipt`.
! The result is the same as that of [[elemsd_GetDivergence_8]].
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Divergence_test_1.F90)
! - [test 2](|base|ElemshapeData/examples/Divergence_test_2.F90) with vector
!   and matrix `val`
!
! See also: [[elemsd_GetDivergence_8]]

INTERFACE Divergence
  MODULE PURE FUNCTION elemsd_Divergence_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector or matrix nodal FEVariable
    TYPE(FEVariable_) :: ans
    !! divergence, quadrature space-time variable
  END FUNCTION elemsd_Divergence_2
END INTERFACE Divergence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElemshapeData_DivergenceMethods
