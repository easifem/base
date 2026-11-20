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

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector FEVariable to quadrature points
!
! `GetInterpolation_` evaluates a vector `FEVariable_` at quadrature
! points without allocation.
! For a nodal variable it uses the space shape functions `N`.
! For space-time it also uses the time shape functions `T`.
! For a quadrature variable it copies the values.
! Each variable type (constant, space, space-time) has three forms:
!
! - `_1`: values at all `nips` points in an array
! - `_2`: values at all points written in a `FEVariable_` at `timeIndx`
! - `_3`: value at the single point `spaceIndx`
!
! `VectorGetInterpolation_3` picks the variable type from `obj`.
! Time variables are not supported.

MODULE FEVariable_VectorInterpolationMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableVector_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, constant, at all quadrature points
!
! Interpolates a constant vector at all quadrature points.
! Every column of `ans` receives `scale * obj`, because a constant vector
! does not depend on the point.
! `N` and `nns` are not used.
!
! If `addContribution` is true, the result is added to `ans`.
! Otherwise `ans(1:nrow, 1:nips)` is set to zero first.
! `nrow` is the number of components and `ncol` equals `nips`.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorConstantGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! not used
    INTEGER(I4B), INTENT(IN) :: nns
    !! not used
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points to fill
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated values, SIZE(ans, 1) >= ncomp, SIZE(ans, 2) >= nips
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE VectorConstantGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, constant, into a FEVariable
!
! Interpolates a constant vector into one time slice of `ans`.
! Every quadrature point of the slice `timeIndx` receives `scale * obj`.
! `N` and `nns` are not used.
!
! `ans` must be a quadrature `FEVariable_` that already has its shape.
! Each time slice holds `ans%s(1) * ans%s(2)` values.
! The slice `timeIndx` starts after the first `timeIndx - 1` slices.
! Only the values of that slice in `ans%val` change.
!
! If `addContribution` is true, the result is added to the slice.
! Otherwise the first `nips` points of the slice are set to zero first.
!
! Changes:
!   - 2026-11-14: the constant value goes to every point. Before, the
!     routine read past the values of `obj` when `nips > 1`.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorConstantGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! not used
    INTEGER(I4B), INTENT(IN) :: nns
    !! not used
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points to fill
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at `timeIndx`
  END SUBROUTINE VectorConstantGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, constant, at one quadrature point
!
! Interpolates a constant vector at one quadrature point.
! The result is `ans(1:tsize) = scale * obj`.
! The point does not matter, so `N`, `nns`, `spaceIndx` and `timeIndx`
! are not used.
!
! If `addContribution` is true, the result is added to `ans`.
! Otherwise `ans(1:tsize)` is set to zero first.
! `tsize` is the number of components.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorConstantGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! not used
    INTEGER(I4B), INTENT(IN) :: nns
    !! not used
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! not used
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! not used
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value, SIZE(ans) >= ncomp
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE VectorConstantGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, space, at all quadrature points
!
! Interpolates a space vector at all quadrature points.
! The result goes to `ans(1:nrow, 1:nips)`.
! The formula depends on how `obj` is defined.
!
! 1. Nodal: `ans(:, ips) = scale * SUM_I N(I, ips) * obj(:, I)`.
! 2. Quadrature: `ans(:, ips) = scale * obj(:, ips)`.
!    Here `N` and `nns` are not used.
!
! If `addContribution` is true, the result is added to `ans`.
! Otherwise `ans(1:nrow, 1:nips)` is set to zero first.
! `nrow` is the number of components and `ncol` equals `nips`.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, ans, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated values, SIZE(ans, 1) >= ncomp, SIZE(ans, 2) >= nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
  END SUBROUTINE VectorSpaceGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, space, into a FEVariable
!
! Interpolates a space vector into one time slice of `ans`.
! The formula depends on how `obj` is defined.
!
! 1. Nodal: `ans(:, ips) = scale * SUM_I N(I, ips) * obj(:, I)`.
! 2. Quadrature: `ans(:, ips) = scale * obj(:, ips)`.
!    Here `N` and `nns` are not used.
!
! `ans` must be a quadrature `FEVariable_` that already has its shape.
! Each time slice holds `ans%s(1) * ans%s(2)` values.
! The slice `timeIndx` starts after the first `timeIndx - 1` slices.
! Only the values of that slice in `ans%val` change.
!
! If `addContribution` is true, the result is added to the slice.
! Otherwise the whole slice is set to zero first.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, scale, addContribution, timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at `timeIndx`
  END SUBROUTINE VectorSpaceGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, space, at one quadrature point
!
! Interpolates a space vector at one quadrature point.
! The formula depends on how `obj` is defined.
!
! 1. Nodal: `ans(:) = scale * SUM_I N(I, spaceIndx) * obj(:, I)`.
! 2. Quadrature: `ans(:) = scale * obj(:, spaceIndx)`.
!    Here `N` and `nns` are not used.
!
! `timeIndx` is not used.
!
! If `addContribution` is true, the result is added to `ans`.
! Otherwise `ans(1:tsize)` is set to zero first.
! `tsize` is the number of components.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, scale, addContribution, &
    ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time quadrature point index, not used
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value, SIZE(ans) >= ncomp
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE VectorSpaceGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, space-time, at all quadrature points
!
! Interpolates a space-time vector at all quadrature points.
! The result goes to `ans(1:nrow, 1:nips)`.
! The formula depends on how `obj` is defined.
!
! 1. Nodal:
!    `ans(:, ips) = scale * SUM_a T(a) SUM_I N(I, ips) * obj(:, I, a)`.
! 2. Quadrature: `ans(:, ips) = scale * obj(:, ips, timeIndx)`.
!    Here `N`, `nns`, `T` and `nnt` are not used.
!
! The nodal formula sums over the time nodes `a = 1, ..., nnt`.
! It does not use `timeIndx`. The quadrature formula uses it to pick
! the time point.
!
! If `addContribution` is true, the result is added to `ans`.
! Otherwise `ans(1:nrow, 1:nips)` is set to zero first.
! `nrow` is the number of components and `ncol` equals `nips`.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_1( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, ans, &
    nrow, ncol, timeIndx)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolated values, SIZE(ans, 1) >= ncomp, SIZE(ans, 2) >= nips
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time point index, used only if obj is on quadrature points
  END SUBROUTINE VectorSpaceTimeGetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, space-time, into a FEVariable
!
! Interpolates a space-time vector into one time slice of `ans`.
! The formula depends on how `obj` is defined.
!
! 1. Nodal:
!    `ans(:, ips) = scale * SUM_a T(a) SUM_I N(I, ips) * obj(:, I, a)`.
! 2. Quadrature: `ans(:, ips) = scale * obj(:, ips, timeIndx)`.
!    Here `N`, `nns`, `T` and `nnt` are not used.
!
! The nodal formula sums over the time nodes `a = 1, ..., nnt`.
! It does not use `timeIndx` to read `obj`. The quadrature formula
! uses it to pick the time point.
!
! `ans` must be a quadrature `FEVariable_` that already has its shape.
! Each time slice holds `ans%s(1) * ans%s(2)` values.
! The slice `timeIndx` starts after the first `timeIndx - 1` slices.
! Only the values of that slice in `ans%val` change.
!
! If `addContribution` is true, the result is added to the slice.
! Otherwise the whole slice is set to zero first.
!
! Changes:
!   - 2026-11-14: the routine clears `ans` when `addContribution` is
!     false
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_2( &
    obj, rank, vartype, N, nns, nips, T, nnt, scale, addContribution, &
    timeIndx, ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: nips
    !! number of quadrature points, bound for dim2 in N
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time point index, used only if obj is on quadrature points
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    !! quadrature variable, values written at `timeIndx`
  END SUBROUTINE VectorSpaceTimeGetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector, space-time, at one quadrature point
!
! Interpolates a space-time vector at one quadrature point.
! The formula depends on how `obj` is defined.
!
! 1. Nodal:
!    `ans(:) = scale * SUM_a T(a) SUM_I N(I, spaceIndx) * obj(:, I, a)`.
! 2. Quadrature: `ans(:) = scale * obj(:, spaceIndx, timeIndx)`.
!    Here `N`, `nns`, `T` and `nnt` are not used.
!
! The nodal formula sums over the time nodes `a = 1, ..., nnt`.
! It does not use `timeIndx`.
!
! If `addContribution` is true, the result is added to `ans`.
! Otherwise `ans(1:tsize)` is set to zero first.
! `tsize` is the number of components.
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorSpaceTimeGetInterpolation_3( &
    obj, rank, vartype, N, nns, spaceIndx, timeIndx, T, nnt, scale, &
    addContribution, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time point index, used only if obj is on quadrature points
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value, SIZE(ans) >= ncomp
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE VectorSpaceTimeGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                      GetInterpolation_@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Interpolation of vector at one point, any variable type
!
! Interpolates a vector at one quadrature point.
! It reads the variable type of `obj` and picks the matching routine:
!
! 1. Constant variable: `VectorConstantGetInterpolation_3`.
! 2. Space variable: `VectorSpaceGetInterpolation_3`.
! 3. Space-time variable: `VectorSpaceTimeGetInterpolation_3`.
!
! @note
! A time variable is not supported. For it, `ans` and `tsize` are not
! set.
! @endnote
!
! Examples
!
! - [test 2](|base|FEVariable/examples/GetInterpolation__test_2.F90)
! - [test 8](|base|FEVariable/examples/GetInterpolation__test_8.F90)
!
! See also: [[VectorConstantGetInterpolation_3]],
! [[VectorSpaceGetInterpolation_3]],
! [[VectorSpaceTimeGetInterpolation_3]]

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE VectorGetInterpolation_3( &
    obj, rank, N, nns, spaceIndx, timeIndx, T, nnt, scale, addContribution, &
    ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable to interpolate
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    REAL(DFP), INTENT(IN) :: N(:, :)
    !! space shape functions, N(I, ips): I is node, ips is quadrature point
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes, bound for dim1 in N
    INTEGER(I4B), INTENT(IN) :: spaceIndx
    !! space quadrature point index, column of N
    INTEGER(I4B), INTENT(IN) :: timeIndx
    !! time point index, used only for a space-time variable
    REAL(DFP), INTENT(IN) :: T(:)
    !! time shape functions at the time quadrature point, T(a)
    INTEGER(I4B), INTENT(IN) :: nnt
    !! number of time nodes, bound for T
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! interpolated value, SIZE(ans) >= ncomp
    REAL(DFP), INTENT(IN) :: scale
    !! scale factor applied to the interpolated value
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, the interpolated value is added to ans
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in ans
  END SUBROUTINE VectorGetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_VectorInterpolationMethod
