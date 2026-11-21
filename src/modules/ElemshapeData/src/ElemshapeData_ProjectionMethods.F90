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
! summary: Projection of shape function gradients on a vector
!
! Compute `c . grad(N_I)` (or `c . grad(N_I T_a)` for space-time) at the
! quadrature points, for a vector `c`. This is the convective term of
! advection operators. Set `dNdXt` (or `dNTdXt`) first.

MODULE ElemshapeData_ProjectionMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: GetProjectionOfdNdXt
PUBLIC :: GetProjectionOfdNdXt_
PUBLIC :: GetProjectionOfdNTdXt
PUBLIC :: GetProjectionOfdNTdXt_

!----------------------------------------------------------------------------
!                                                GetProjectionOfdNdXt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-21
! summary: Projection of dN/dx on a constant vector
!
! Returns `c . grad(N_I)` at each quadrature point.
! `c` is a constant vector. It reallocates `ans(nns, nips)`.
!
! For nodal values `u`, `SUM_I u(I) * ans(I, ips)` gives `c . grad(u)`.
! Set `dNdXt` first.
!
! Formula:
!   $$ ans(I, ips) = \sum_k dNdXt(I, k, ips) \, c(k) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_1.F90) first block
!
! See also: [[GetProjectionOfdNdXt1_]], [[GetProjectionOfdNdXt4_]]

INTERFACE GetProjectionOfdNdXt
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt_1(obj, c, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant vector, c(nsd)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nips)
  END SUBROUTINE GetProjectionOfdNdXt_1
END INTERFACE GetProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-05
! summary: Projection of dN/dx on a constant vector, no allocation
!
! Returns `c . grad(N_I)` like [[GetProjectionOfdNdXt_1]].
! It writes into the existing `ans` and does not allocate it.
! It returns `nrow = nns` and `ncol = nips`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_1.F90) first block
!
! See also: [[GetProjectionOfdNdXt_1]]

INTERFACE GetProjectionOfdNdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt1_(obj, c, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant vector, c(nsd)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE GetProjectionOfdNdXt1_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                                GetProjectionOfdNdXt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-21
! summary: Projection of dN/dx on a vector FEVariable
!
! Returns `c(ips) . grad(N_I)` at each quadrature point.
! It interpolates `c` at each quadrature point with `N`.
! It reallocates `ans(nns, nips)`.
!
! @warning
! `c` must have exactly `obj%nsd` components, with `obj%nsd <= 3`.
! This routine uses a three-component interpolation buffer.
! Debug builds reject a mismatched count before interpolation.
! They also check the returned interpolation size.
! Both checks use `ERROR STOP`.
! Release builds do not check component counts.
! More than three components can overwrite memory in release builds.
! Too few components silently substitute zeros for missing directions.
! Extra components do not contribute to the projection.
! @endwarning
!
! @note
! `c` must be a constant or space vector variable.
! Time and space-time variables are not supported.
! Debug builds reject them with `ERROR STOP`.
! Release builds return zero projection values for these types.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject time-dependent velocities in debug builds.
!   - 2026-11-21: Document component-count and buffer constraints.
!   - 2026-11-21: Check component counts in debug builds.
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_3.F90)
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_1.F90) second block, constant `c`
!
! - [test 4](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_4.F90)
!
! See also: [[GetProjectionOfdNdXt2_]]

INTERFACE GetProjectionOfdNdXt
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt_2(obj, c, crank, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    CLASS(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! rank of c should be vector
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nips)
  END SUBROUTINE GetProjectionOfdNdXt_2
END INTERFACE GetProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Projection of dN/dx on a vector FEVariable, no allocation
!
! Returns `c(ips) . grad(N_I)` like [[GetProjectionOfdNdXt_2]].
! It writes into the existing `ans` and does not allocate it.
! It returns `nrow = nns` and `ncol = nips`.
! The component-count precondition and buffer risks of
! [[GetProjectionOfdNdXt_2]] also apply here.
!
! @note
! `c` must be a constant or space vector variable.
! Time and space-time variables are not supported.
! Debug builds reject them with `ERROR STOP`.
! Release builds return zero projection values for these types.
! @endnote
!
! Changes:
!   - 2026-11-21: Reject time-dependent velocities in debug builds.
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_3.F90)
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_1.F90) second block, nodal space `c`
!
! See also: [[GetProjectionOfdNdXt_2]]

INTERFACE GetProjectionOfdNdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt2_(obj, c, crank, ans, nrow, &
                                                ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    CLASS(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE GetProjectionOfdNdXt2_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                                GetProjectionOfdNdXt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-21
! summary: Projection of dN/dx on a vector given at quadrature points
!
! Returns `c(:, ips) . grad(N_I)` at each quadrature point.
! `c(nsd, nips)` holds the vector at the quadrature points.
! It reallocates `ans(nns, nips)`.
!
! Formula:
!   $$ ans(I, ips) = \sum_k dNdXt(I, k, ips) \, c(k, ips) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_1.F90) second block, `c` at quadrature points
!
! See also: [[GetProjectionOfdNdXt3_]]

INTERFACE GetProjectionOfdNdXt
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt_3(obj, c, ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! ElemshapeData object
    REAL(DFP), INTENT(IN) :: c(:, :)
    !! vector at quadrature points, c(nsd, nips)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nips)
  END SUBROUTINE GetProjectionOfdNdXt_3
END INTERFACE GetProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-03-05
! summary: Projection of dN/dx on a vector at quadrature points, no allocation
!
! Returns `c(:, ips) . grad(N_I)` like [[GetProjectionOfdNdXt_3]].
! It writes into the existing `ans` and does not allocate it.
! It returns `nrow = nns` and `ncol = nips`.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_2.F90)
!
! See also: [[GetProjectionOfdNdXt_3]]

INTERFACE GetProjectionOfdNdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt3_(obj, c, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    REAL(DFP), INTENT(IN) :: c(:, :)
    !! vector at quadrature points, c(nsd, nips)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nips)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE GetProjectionOfdNdXt3_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-01
! summary: Projection of dN/dx on a constant vector at one point
!
! Returns `c . grad(N_I)` at the quadrature point `ips`.
! It sets `ans(1:nns)` and returns `tsize = nns`.
!
! Formula:
!   $$ ans(I) = \sum_k dNdXt(I, k, ips) \, c(k) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNdXt_test_1.F90) first block, at `ips = 3`
!
! See also: [[GetProjectionOfdNdXt_1]]

INTERFACE GetProjectionOfdNdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNdXt4_(obj, ips, c, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    INTEGER(I4B), INTENT(IN) :: ips
    !! integration points in space
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant vector, c(nsd)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! projection, ans(nns)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! the size of data written in ans
  END SUBROUTINE GetProjectionOfdNdXt4_
END INTERFACE GetProjectionOfdNdXt_

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNTdXt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-23
! summary: Projection of dNT/dx on a constant vector
!
! Returns `c . grad(N_I T_a)` at each quadrature point.
! `c` is a constant vector. It reallocates `ans(nns, nnt, nips)`.
!
! For space-time nodal values `u`, the sum
! `SUM_I SUM_a u(I, a) * ans(I, a, ips)` gives `c . grad(u)`.
!
! Formula:
!   $$ ans(I, a, ips) = \sum_k dNTdXt(I, a, k, ips) \, c(k) $$
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_1.F90) first block
!
! See also: [[GetProjectionOfdNTdXt1_]]

INTERFACE GetProjectionOfdNTdXt
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt_1(obj, c, ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant vector, c(nsd)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! projection, ans(nns, nnt, nips)
  END SUBROUTINE GetProjectionOfdNTdXt_1
END INTERFACE GetProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                              GetProjectionOfdNTdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-23
! summary: Projection of dNT/dx on a constant vector, no allocation
!
! Returns `c . grad(N_I T_a)` like [[GetProjectionOfdNTdXt_1]].
! It writes into the existing `ans` and does not allocate it.
! It returns `dim1 = nns`, `dim2 = nnt` and `dim3 = nips`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_1.F90) first block
!
! See also: [[GetProjectionOfdNTdXt_1]]

INTERFACE GetProjectionOfdNTdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt1_(obj, c, ans, dim1, dim2, &
                                                 dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    REAL(DFP), INTENT(IN) :: c(:)
    !! constant vector, c(nsd)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! projection, ans(nns, nnt, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE GetProjectionOfdNTdXt1_
END INTERFACE GetProjectionOfdNTdXt_

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNTdXt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-07
! summary: Projection of dNT/dx on a vector FEVariable
!
! Returns `c(ips) . grad(N_I T_a)` at each quadrature point.
! It interpolates `c` at each quadrature point with `N` and `T`.
! The interpolation uses time index 1.
! It reallocates `ans(nns, nnt, nips)`.
!
! @warning
! `c` must have exactly `obj%nsd` components, with `obj%nsd <= 3`.
! This routine uses a three-component interpolation buffer.
! Debug builds reject a mismatched count before interpolation.
! They also check the returned interpolation size.
! Both checks use `ERROR STOP`.
! Release builds do not check component counts.
! More than three components can overwrite memory in release builds.
! Too few components silently substitute zeros for missing directions.
! Extra components do not contribute to the projection.
! @endwarning
!
! @note
! A time-only `c` is unsupported. Debug builds reject it with `ERROR STOP`.
! Release builds return zero projection values for this type.
! @endnote
!
! Changes:
!   - 2026-11-21: Document component-count and buffer constraints.
!   - 2026-11-21: Check component counts in debug builds.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_1.F90) first block, constant `c`
!
! - [test 4](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_4.F90)
!
! See also: [[GetProjectionOfdNTdXt2_]], [[GetProjectionOfdNTdXt_3]]

INTERFACE GetProjectionOfdNTdXt
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt_2(obj, c, crank, ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! TypeFEVariableVector
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! projection, ans(nns, nnt, nips)
  END SUBROUTINE GetProjectionOfdNTdXt_2
END INTERFACE GetProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                              GetProjectionOfdNTdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Projection of dNT/dx on a vector FEVariable, no allocation
!
! Returns `c(ips) . grad(N_I T_a)` like [[GetProjectionOfdNTdXt_2]].
! It writes into the existing `ans` and does not allocate it.
! It returns `dim1 = nns`, `dim2 = nnt` and `dim3 = nips`.
! The component-count precondition and buffer risks of
! [[GetProjectionOfdNTdXt_2]] also apply here.
! A time-only `c` triggers `ERROR STOP` in debug builds.
! Release builds return zero projection values for this type.
!
! Changes:
!   - 2026-11-21: Reject time-only velocities in debug builds.
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_3.F90)
!
! - [test 2](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_2.F90) first block
!
! See also: [[GetProjectionOfdNTdXt_2]]

INTERFACE GetProjectionOfdNTdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt2_(obj, c, crank, ans, dim1, &
                                                 dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    !! space-time shape data at one time point
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! projection, ans(nns, nnt, nips)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
  END SUBROUTINE GetProjectionOfdNTdXt2_
END INTERFACE GetProjectionOfdNTdXt_

!----------------------------------------------------------------------------
!                                               GetProjectionOfdNTdXt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-07
! summary: Projection of dNT/dx on a vector FEVariable at all time points
!
! Returns `c . grad(N_I T_a)` at all space and time points.
! Here `obj(ipt)` is the shape data at time quadrature point `ipt`.
! It reallocates `ans(nns, nnt, nips, nipt)`.
!
! It fills time point `ipt` like [[GetProjectionOfdNTdXt_2]].
! It interpolates `c` at each space point `ips` and time point `ipt`.
! It follows [[GetProjectionOfdNTdXt_2]] for unsupported time-only `c`.
! Its component-count precondition and buffer risks also apply here.
! Each entry of `obj` must have the same spatial dimension.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_1.F90) second block
!
! See also: [[GetProjectionOfdNTdXt3_]], [[GetProjectionOfdNTdXt4_]]

INTERFACE GetProjectionOfdNTdXt
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt_3(obj, c, crank, ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! TypeFEVariableVector
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! projection, ans(nns, nnt, nips, nipt)
  END SUBROUTINE GetProjectionOfdNTdXt_3
END INTERFACE GetProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                              GetProjectionOfdNTdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Projection of dNT/dx at all time points, no allocation
!
! Returns `c . grad(N_I T_a)` like [[GetProjectionOfdNTdXt_3]].
! It writes into the existing `ans` and does not allocate it.
! It returns `dim1 = nns`, `dim2 = nnt`, `dim3 = nips` and
! `dim4 = nipt`.
! The component-count precondition and buffer risks of
! [[GetProjectionOfdNTdXt_2]] also apply here.
! Each entry of `obj` must have the same spatial dimension.
! A time-only `c` triggers `ERROR STOP` in debug builds.
! Release builds return zero projection values for this type.
!
! Changes:
!   - 2026-11-21: Reject time-only velocities in debug builds.
!
! Examples
!
! - [test 3](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_3.F90)
!
! - [test 2](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_2.F90) second block
!
! See also: [[GetProjectionOfdNTdXt_3]]

INTERFACE GetProjectionOfdNTdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt3_(obj, c, crank, ans, dim1, &
                                                 dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! projection, ans(nns, nnt, nips, nipt)
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! size of dim1 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! size of dim2 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! size of dim3 written in ans
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! size of dim4 written in ans
  END SUBROUTINE GetProjectionOfdNTdXt3_
END INTERFACE GetProjectionOfdNTdXt_

!----------------------------------------------------------------------------
!                                              GetProjectionOfdNTdXt_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Projection of dNT/dx at one space and time point
!
! Returns `c . grad(N_I T_a)` at space point `ips` and time point `ipt`.
! It uses the shape data `obj(ipt)`.
! It sets `ans(1:nns, 1:nnt)` and returns `nrow = nns` and
! `ncol = nnt`.
! The component-count precondition and buffer risks of
! [[GetProjectionOfdNTdXt_2]] also apply here, using `obj(ipt)%nsd`.
! A time-only `c` triggers `ERROR STOP` in debug builds.
! Release builds return zero projection values for this type.
!
! Changes:
!   - 2026-11-14: it reads nns, nnt and nsd from obj(ipt); before, it
!     read them from obj(ips)
!   - 2026-11-21: Reject time-only velocities in debug builds.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_1.F90) second block, at `ips = 1` and `ipt = 2`
!
! - [test 3](|base|ElemshapeData/examples/GetProjectionOfdNTdXt_test_3.F90)
!
! See also: [[GetProjectionOfdNTdXt_3]]

INTERFACE GetProjectionOfdNTdXt_
  MODULE PURE SUBROUTINE GetProjectionOfdNTdXt4_(obj, c, crank, ans, nrow, &
                                                 ncol, ips, ipt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! projection, ans(nns, nnt)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    INTEGER(I4B), INTENT(IN) :: ips
    !! space quadrature point index
    INTEGER(I4B), INTENT(IN) :: ipt
    !! time quadrature point index
  END SUBROUTINE GetProjectionOfdNTdXt4_
END INTERFACE GetProjectionOfdNTdXt_

END MODULE ElemshapeData_ProjectionMethods
